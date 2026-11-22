;;;; runtime/outputs.lisp --- Monitors: arriving, moving, and being unplugged.
;;;;
;;;; PLAN.org's fiat rules multi-monitor as "one model, one viewport per
;;;; output", and this file is the half of that which talks to river.  The
;;;; other half — which part of the model each output shows — is
;;;; POLICY:OUTPUT-CONTENT, and it is a policy decision on purpose.
;;;;
;;;; TWO FACTS RIVER DOES NOT CARRY, and both matter.  river_output_v1 has a
;;;; position, a size, and the numeric id of a wl_output.  It has no *name* and
;;;; no *scale*.  The name is what per-output workspace memory is keyed on; the
;;;; scale is what everything the window manager draws for itself has to be
;;;; drawn at.  Both come from wl_output, both arrive in a race with the
;;;; river_output_v1 event that says which wl_output this is, and the join
;;;; happens here rather than in either handler so that it does not matter
;;;; which arrives first.
;;;;
;;;; HOTPLUG IS THE PART THAT WAS MISSING.  Forgetting an unplugged output was
;;;; the easy third of it; the other two thirds are releasing the surfaces,
;;;; buffers and file descriptors that belonged to it, and making sure the
;;;; workspace it was showing is still on a screen somewhere.  Without the
;;;; second, docking a laptop twice a day leaks a set of overlays each time.
;;;; Without the third, undocking leaves you looking at a monitor showing a
;;;; workspace you are not on, with a status line confidently reporting where
;;;; you are.

(in-package #:latticewm/runtime)

(defun name-outputs ()
  "Give every output the name of the wl_output it named, once both are known.

Called from both sides because the two events race: river_output_v1.wl_output
tells us *which* wl_output an output is, and wl_output.name tells us what that
one is called, and neither is guaranteed to arrive first.  Doing the join here
rather than in either handler means it does not matter."
  (when *server*
    (dolist (output (c:world-outputs *world*))
      (let ((id (c:prop output :wl-output-id)))
        (when id
          (let ((name (gethash id (server-output-names *server*))))
            (when (and name (not (equal name (c:output-name output))))
              (setf (c:output-name output) name)
              ;; A MONITOR THAT HAS JUST LEARNED ITS OWN NAME IS NEWS.  The
              ;; :OUTPUT-ADDED hook waits for it, and this join is not driven
              ;; by anything that asks for a manage sequence — so without this
              ;; line an output whose name lost the race would sit unannounced
              ;; until something else happened to want one, which on an idle
              ;; desktop is a keystroke away and might be minutes.
              (when (assoc output *unannounced-outputs*) (request-manage))
              (logmsg :info "output ~a is ~a" id name))))))))

(defun wl-output-named (name)
  "The bound wl_output proxy for the output called NAME, or NIL.

Needed because a tablet or a touchscreen is confined to a monitor by handing
river the *wl_output*, and river_output_v1 is not one — so this is the join
run backwards, from the human-readable name a configuration file writes to the
object the protocol wants.  Accepts an OUTPUT as well as a name, since half the
callers already have one."
  (when (and *server* *world* name)
    (let ((output (if (typep name 'c:output)
                      name
                      (find (string name) (c:world-outputs *world*)
                            :key #'c:output-name :test #'equal))))
      (when output
        (let ((id (c:prop output :wl-output-id)))
          (and id (gethash id (server-wl-outputs *server*))))))))

(defun attach-output (proxy)
  "Register an output and follow its position, size and layer-shell area."
  (let ((output (make-instance 'c:output :proxy proxy)))
    (setf (gethash proxy (server-outputs *server*)) output)
    (setf (c:world-outputs *world*)
          (append (c:world-outputs *world*) (list output)))
    (on-events (proxy "river_output_v1")
                ;; NOT :name.  river_output_v1 has no such event -- the case
                ;; that used to be here could never fire, so every output was
                ;; anonymous, and the per-output workspace memory is keyed on
                ;; the name.  It silently did nothing on every machine.
                (:wl-output
                 (setf (c:prop output :wl-output-id) (first arguments))
                 (name-outputs)
                 (adopt-wl-output output))
                (:position
                 (setf (c:rect-x (c:output-rect output)) (first arguments)
                       (c:rect-y (c:output-rect output)) (second arguments))
                 (mark-dirty))
                (:dimensions
                 (setf (c:rect-w (c:output-rect output)) (first arguments)
                       (c:rect-h (c:output-rect output)) (second arguments))
                 (mark-dirty))
                ;; The whole monitor is being recorded, which is what an
                ;; ordinary screen share is; the per-window count in
                ;; runtime/windows.lisp is the rarer half of the same event.
                (:capture-sessions
                 (note-capture-sessions output (first arguments)))
                (:removed (detach-output output proxy))
                (t nil))
    (attach-layer-shell-output output)
    ;; A new monitor brings its own workspace, or it mirrors the first one and
    ;; looks broken.
    (guarded "workspaces for outputs" (p:ensure-workspaces-for-outputs *world*))
    (push (cons output 0) *unannounced-outputs*)
    (mark-dirty)
    (request-manage)
    (logmsg :info "output appeared: ~s (workspace ~a)"
            output (c:prop output :workspace))
    output))

(defparameter +announce-attempts+ 10
  "Manage sequences to wait for a monitor to say what it is before giving up.

A CAP, NOT A TIMEOUT, and it exists so that the wait cannot become a silence.
Waiting for the name is right — the name is what per-output workspace memory
is keyed on, and it arrives on the same wl_output join as the scale, so an
output that has one has both.  But `wait until it is known' with nothing on
the end of it means a compositor that never sends one has a hook that never
fires, and a hook that never fires is the bug this whole mechanism was in.
Ten sequences is far more than the two it actually takes, and reaching it logs
what was missing rather than passing over it.")

(defun output-knowable-p (output)
  "Has OUTPUT said enough about itself for a hook to act on it?

Its name, because a process per screen is keyed on it and it carries the same
wl_output join the scale does; and a real size, because a surface per screen
needs one.  Those are the two facts the hook's documented uses require and the
two river does not put in the event that announces the monitor."
  (let ((rect (c:output-rect output)))
    (and (c:output-name output)
         (plusp (length (c:output-name output)))
         (plusp (c:rect-w rect))
         (plusp (c:rect-h rect)))))

(defun announce-new-outputs ()
  "Run :OUTPUT-ADDED for monitors that have arrived and said what they are.

Called from the manage sequence, which is the same answer PLACE-UNPLACED-
WINDOWS gives to the same question: at the moment `output' arrives we know
nothing about the output.  One round trip brings the position and the size;
the name comes from a *different global* — wl_output, joined by id — and lost
the race about half the time, which is why this waits for it rather than for a
fixed number of sequences.  The integration run asserts on what the hook was
handed at the moment it fired, so `it turned up eventually' cannot pass for it."
  (let ((pending (nreverse *unannounced-outputs*))
        (again '()))
    (setf *unannounced-outputs* '())
    (dolist (entry pending)
      (destructuring-bind (output . attempts) entry
        ;; Still ours, and still plugged in.  DETACH-OUTPUT drops it from the
        ;; list as well, so this is the belt to that braces.
        (when (member output (c:world-outputs *world*))
          (cond
            ((output-knowable-p output) (run-hooks :output-added output))
            ((>= attempts +announce-attempts+)
             (logmsg :warn "output ~s never said ~:[what it is called~;how big ~
                            it is~]; announcing it anyway"
                     output (c:output-name output))
             (run-hooks :output-added output))
            (t (push (cons output (1+ attempts)) again))))))
    (setf *unannounced-outputs* again)))

(defun detach-output (output proxy)
  "A monitor went away.  Take it out of everything and put its work somewhere.

THREE THINGS HAVE TO HAPPEN AND ONLY ONE OF THEM USED TO.  Forgetting the
output was the easy part; the other two are what docking a laptop actually
needs:

  * the overlays that belonged to it are surfaces, river nodes, mmaps and file
    descriptors, and nothing was releasing them — so every undock leaked a set;
  * the workspace it was showing is now on no screen at all, which is a black
    display with a status line confidently reporting where you are.  The same
    failure RESTORE-OUTPUT-WORKSPACES was written for, arriving by a different
    road.

The protocol also asks us to destroy the object after `removed', which frees
the compositor's side."
  (let ((was-showing (c:prop output :workspace))
        ;; PAIRED, OR NEITHER.  A monitor plugged and unplugged inside one
        ;; round trip is never announced, and a hook keeping a table per screen
        ;; must not be told to remove a screen it was never told about — which
        ;; is the shape of the :STARTUP-without-:SHUTDOWN bug one file over,
        ;; and cheaper to not write than to find.
        (announced (not (assoc output *unannounced-outputs*))))
    (setf *unannounced-outputs* (remove output *unannounced-outputs* :key #'car))
    (forget-overlays-for-output output)
    (let ((layer (c:prop output :layer-shell)))
      (when layer
        (best-effort "layer shell output destroy"
          (w:layer-output-destroy layer))
        (setf (c:prop output :layer-shell) nil)))
    (remhash proxy (server-outputs *server*))
    (setf (c:world-outputs *world*) (remove output (c:world-outputs *world*)))
    (best-effort "output destroy" (w:output-destroy proxy))
    (setf (c:output-proxy output) nil)
    (rehome-orphaned-workspace was-showing)
    (when announced (run-hooks :output-removed output))
    (mark-dirty)
    (request-manage)
    (logmsg :info "output removed: ~s" output))
  nil)

(defun set-plane (leader index)
  "Make LEADER's group -- or LEADER alone -- display workspace INDEX."
  (let ((group (or (c:output-share-group leader) (list leader))))
    (dolist (member group)
      (setf (c:prop member :workspace) index)))
  index)

(defun show-workspace-on (output index)
  "Make OUTPUT display workspace INDEX, and trade with whoever had it.

A SHARED GROUP IS ONE PLANE, SO IT MOVES AS ONE (#126).  Switching workspace on
either screen of a shared pair used to leave the other screen blank: the leader
went on presenting the union of both rectangles -- laid out for its *own* plane
-- while the follower, which presents nothing by design, showed a workspace
nobody could see.  The windows on it were in the world and on no screen.  Asking
a follower is asking the group, so the switch is handed to the leader and every
member takes the new index; asking the leader takes the followers with it, and
whatever screen was already showing the target trades as before.

THE ONE WRITER OF :WORKSPACE, which is what was missing.  Six places set that
property directly and each of them was individually right; between them they
could put two monitors on one workspace, and two outputs showing one workspace
is the layout collapsing onto the last of them with the second screen blank.
Switching workspaces on a two-monitor desktop reached it without any restore
being involved: show workspace 2 while the other monitor is already showing 2
and both monitors were showing 2.

*They trade.* The other screen takes the workspace this one was showing, which
is the answer that leaves both monitors displaying something and leaves you
looking at what you asked for.  The alternative — moving the keyboard to the
monitor that already has it — is what i3 does and is a defensible reading of
the same request; it is not this one, because here the cursor is a place in a
model rather than a consequence of which screen is active, and a command that
moved it to another monitor would be answering a question nobody asked.

If the other output had nothing to trade back it is left with no workspace, and
ENSURE-WORKSPACES-FOR-OUTPUTS gives it one before anything is drawn."
  (when (and output (integerp index))
    ;; A FOLLOWER IS NOT A SCREEN YOU SWITCH.  It draws nothing, so a switch
    ;; asked of it is a switch of the group; the leader is the member that
    ;; keeps the index and lays the plane out, so it is the one asked.
    (let ((follower-group (c:output-share-group output)))
      (when (and follower-group (not (eq output (first follower-group))))
        (return-from show-workspace-on
          (show-workspace-on (first follower-group) index))))
    (let* ((group (c:output-share-group output))
           (leader (if group (first group) output))
           (showing (p:output-showing *world* index))
           ;; The output already showing INDEX may itself be one screen of a
           ;; group, in which case the trade is with the group, not the screen:
           ;; giving one member a different plane is the divergence #126 was.
           (other-group (and showing (c:output-share-group showing)))
           (other (if other-group (first other-group) showing))
           (was (c:prop leader :workspace)))
      (cond
        ((eq other leader) nil)          ; already showing it; nothing to trade
        (t
         (when (and other (not (eq other output)))
           (set-plane other was)
           (logmsg :debug "~a takes workspace ~@[~d~] from ~a"
                   (or (c:output-name other) "an output")
                   (and was (1+ was))
                   (or (c:output-name output) "an output")))
         (set-plane leader index))))))

(defun rehome-orphaned-workspace (index)
  "Make sure the workspace INDEX was showing is still on a screen somewhere.

Called when the output showing it is unplugged.  If another output is already
showing it there is nothing to do; otherwise the cursor's own output adopts it,
which is the answer that keeps you looking at what you were looking at.  With
no outputs left at all there is nothing to be done and nothing to see."
  (let ((outputs (all-outputs)))
    (when (and (integerp index) outputs
               (notany (lambda (output) (eql index (c:prop output :workspace)))
                       outputs))
      (let ((adopter (or (p:output-showing *world* (first (current-path)))
                         (first outputs))))
        (when (and adopter (c:world-focused-float *world*))
          (setf (c:world-focused-float *world*) nil))
        (when adopter
          (show-workspace-on adopter index)
          (let ((stack (c:world-workspaces *world*)))
            (when stack (setf (c:container-selection stack) index)))
          (p:repair-cursor (p:current-policy) *world*)
          (logmsg :info "workspace ~d had no screen; ~a adopted it"
                  (1+ index) (or (c:output-name adopter) "an output")))))))

(defun adopt-wl-output (output)
  "Bind the wl_output OUTPUT names, so its scale and name can be read.

river_output_v1 carries neither: it has a `wl_output' event with the numeric id
of the global and nothing else, so both the human-readable name and the scale
factor have to be fetched from wl_output itself.  BIND-GLOBALS already listens
for them; this is the join, and it is called from both sides because the two
events race."
  (when *server*
    (let ((id (c:prop output :wl-output-id)))
      (when id
        (let ((scale (gethash id (server-output-scales *server*))))
          (when (and scale (/= scale (c:output-scale output)))
            (setf (c:output-scale output) scale)
            (logmsg :info "output ~a has scale ~d"
                    (or (c:output-name output) id) scale)
            (mark-dirty)))))))
;;; ========================================================== shared workspaces
;;; #96: ONE PLANE ACROSS SEVERAL OUTPUTS.
;;;
;;; "Share a workspace" cannot mean what the phrase first suggests, and saying
;;; why is the whole of the design.  A Wayland surface is one window with one
;;; geometry, so two outputs cannot display the *same* window twice: that would
;;; be two surfaces for one client, which no protocol in the tree offers.  What
;;; this file does instead is present one workspace across the union of the
;;; outputs in a group, so a cell, a split or a window can straddle the seam.
;;; That is a real reading of "share a plane across screens" and it is the one
;;; the lattice — a plane of cells with a cursor that is a place in a model —
;;; was always going to need.  The other half of #96, MIRROR (the second screen
;;; showing a copy), is not expressible here for the same reason and is filed
;;; separately; see the issue.

(defun cursor-output ()
  "The output the cursor's workspace is showing on."
  (and *world* (p:output-showing *world* (first (current-path)))))

(defun output-in-direction-not-sharing (from direction)
  "The nearest output DIRECTION of FROM that is not already in its group.

P:OUTPUT-IN-DIRECTION answers `the next screen that way', which is the right
answer for moving the cursor and the wrong one for growing a group: from the
leader it returns the first follower, which is already sharing, so a group
could never reach three screens.  Same geometry, one exclusion — nearest first
by the gap along the axis of travel."
  (when from
    (let ((horizontal (c:direction-horizontal-p direction))
          (sign (c:direction-sign direction))
          (best nil)
          (best-gap nil))
      (multiple-value-bind (from-x from-y) (c:rect-center (c:output-rect from))
        (dolist (output (all-outputs) best)
          (unless (or (eq output from) (c:share-siblings-p from output))
            (multiple-value-bind (x y) (c:rect-center (c:output-rect output))
              (let ((gap (* sign (if horizontal (- x from-x) (- y from-y)))))
                (when (and (plusp gap) (or (null best-gap) (< gap best-gap)))
                  (setf best output best-gap gap))))))))))

(defun install-share (group)
  "Record GROUP as sharing one workspace: the first member's.

The first member is the leader and the only one that lays the plane out; it
also owns the group's workspace index, which is the plane being shared.  A
group of one is dissolved rather than recorded, so a lone output never carries
a :SHARE-GROUP it does not use."
  (let ((group (remove-duplicates group :test #'eq)))
    (if (= 1 (length group))
        (setf (c:output-share-group (first group)) nil)
        (let ((index (c:prop (first group) :workspace)))
          (dolist (member group)
            (setf (c:output-share-group member) group
                  (c:prop member :workspace) index))))
    (c:refresh-share-rects *world*)
    group))

(defun unshare-output (output)
  "Make OUTPUT leave whatever group it is in, alone or with the others."

  (let ((group (c:output-share-group output)))
    (when group
      (let ((rest (remove output group :test #'eq)))
        (dolist (member group)
          (setf (c:output-share-group member) nil
                (c:output-share-rect member) nil))
        ;; The rest of the group keeps sharing among themselves, under the same
        ;; leader and the same plane; a group of one is nothing at all.
        (install-share rest)
        (c:refresh-share-rects *world*)))
    (p:ensure-workspaces-for-outputs *world*)
    (mark-dirty)
    (relayout)
    (request-manage)))

(defcommand share-workspace (direction)
  "Show the workspace you are on across the output DIRECTION too.

Both screens become one plane: the layout spans the union of their rectangles,
so a cell, a split or a window can straddle the seam.  It is the one honest
form of \"sharing\" a Wayland compositor allows — a surface has one geometry,
so two outputs cannot display the *same* window twice; see #96.  The cursor's
output is the leader and keeps the workspace; the other screen joins it.  Give
it again to span three or four; the output joins the group already there."
  (:interactive :direction)
  (let* ((here (cursor-output))
         (target (and here (output-in-direction-not-sharing here direction))))
    (cond
      ((null here) (notify "no output to share from"))
      ((null target) (notify "no screen further ~(~a~) to share with" direction))
      (t
       (let* ((gh (or (c:output-share-group here) (list here)))
              (gt (or (c:output-share-group target) (list target)))
              (merged (append gh gt)))
         ;; TARGET's old group, if any, loses it but keeps sharing.
         (when (cdr gt)
           (install-share (remove target gt :test #'eq)))
         (install-share (remove-duplicates merged :test #'eq))
         (mark-dirty) (relayout) (request-manage)
         (notify "sharing with ~a" (or (c:output-name target)
                                       "that output")))))))

(defcommand unshare-workspace ()
  "Stop sharing the workspace you are on; this screen shows its own again.

The rest of the group keeps sharing among themselves.  The plane that was
shared stays on the leader; the screen you are on is given the next free
workspace, because the one it was looking at is still the group's."
  (let ((output (cursor-output)))
    (if (null (and output (c:output-share-group output)))
        (notify "this workspace is not shared")
        (progn
          (unshare-output output)
          (notify "no longer sharing")))))

(defcommand list-shared ()
  "List every shared workspace and the screens showing it."
  (let ((groups '()))
    (dolist (output (all-outputs))
      (let ((group (c:output-share-group output)))
        (when (and group (not (member group groups :test #'eq)))
          (push group groups))))
    (if (null groups)
        (notify "no workspace is shared")
        (show-help-page
         (format nil "shared -- ~d" (length groups))
         (loop for group in (sort groups (lambda (a b) (< (length a) (length b))))
               for index from 1
               collect (cons (format nil "~d" index)
                             (format nil "workspace ~d across ~{~(~a~)~^, ~}"
                                     (1+ (or (c:prop (first group) :workspace) 0))
                                     (mapcar (lambda (member)
                                               (or (c:output-name member) "?"))
                                             group))))))))

;;; --------------------------------------------- a workspace is not a screen (#8)
;;;
;;; THE MODEL IS ONE STACK OF WORKSPACES AND ONE VIEWPORT PER OUTPUT, which is
;;; the fiat in PLAN.org and the reason most of #8 was already true.  Numbers
;;; already mean the same thing everywhere, because there is one stack: `M-x
;;; workspace 5' shows workspace five on the screen the cursor is on and leaves
;;; the other screens alone.  And mirroring in the sense people mean it -- one
;;; picture on two monitors -- is impossible for a Wayland client and was
;;; answered by #96 and #125 rather than by a verb.
;;;
;;; What was missing was the verb for `this workspace belongs over there', and
;;; the report that says which screen is showing what.  Both are here, and
;;; neither needed a new field: SHOW-WORKSPACE-ON already trades, which is
;;; exactly what moving a workspace between screens means.

(defcommand move-workspace-to-output (which &key (follow nil))
  "Show the workspace you are on, on another screen -- a direction or a number.

The whole workspace moves, windows and all, because a workspace is one node of
one tree and there is nothing to take apart.  THIS SCREEN TRADES: it comes back
showing the workspace the other screen was showing, which is SHOW-WORKSPACE-ON's
answer and the one that leaves both monitors displaying something rather than
one of them blank.

FOLLOW takes the cursor with the workspace, for the keyboard to arrive where the
work you just moved now is.  Without it you stay and look at what you traded
for, which is right when you are shipping a distraction away and wrong when you
are following your work."
  (:interactive :direction)
  (let* ((here (cursor-output))
         (target (and here (ignore-errors (resolve-output which))))
         (index (and here (c:prop here :workspace))))
    (cond
      ;; NIL, and not the message NOTIFY returns: the answer to `move this
      ;; workspace' when there is nowhere to move it is a moved workspace or
      ;; nothing, and a command whose failure value is a string is a command
      ;; every caller has to special-case.
      ((null here) (notify "no output to move from") nil)
      ((null target) (notify "no screen that way") nil)
      ((eq target here) (notify "that is the screen you are on") nil)
      ((null index) (notify "this screen is not showing a workspace") nil)
      (t
       (let ((traded (c:prop target :workspace)))
         (show-workspace-on target index)
         (setf (c:container-selection (c:world-workspaces *world*)) index)
         (relayout)
         (request-manage)
         ;; THE CURSOR IS A PLACE IN THE MODEL, NOT A SIDE EFFECT OF WHICH
         ;; SCREEN IS ACTIVE -- the same ruling SHOW-WORKSPACE-ON makes.  So
         ;; without FOLLOW it is repaired onto the workspace this screen has
         ;; just traded for, which is where the person looking at that screen
         ;; now is; with FOLLOW it crosses the seam with the work.
         (if follow
             (focus-output which)
             (p:jump-cursor (p:current-policy) *world*
                            (p:remembered-place *world* (c:prop here :workspace))))
         (notify "workspace ~d is on ~a~@[; this one now shows ~d~]"
                 (1+ index)
                 (or (c:output-name target) "that screen")
                 (and (integerp traded) (1+ traded)))
         index)))))

(defcommand list-outputs ()
  "Show every screen: what it is called, how big it is, and what it is showing."
  ()
  (let ((outputs (all-outputs))
        (here (cursor-output)))
    (if (null outputs)
        (progn (notify "no outputs") nil)
        (progn
          (show-help-page
           (format nil "outputs -- ~d" (length outputs))
           (loop for output in outputs
                 for index from 1
                 collect (cons (format nil "~d ~a~:[~; *~]"
                                       index (or (c:output-name output) "?")
                                       (eq output here))
                               (format nil "~dx~d at ~d,~d  workspace ~d~@[  sharing with ~a~]"
                                       (c:rect-w (c:output-rect output))
                                       (c:rect-h (c:output-rect output))
                                       (c:rect-x (c:output-rect output))
                                       (c:rect-y (c:output-rect output))
                                       (1+ (or (c:prop output :workspace) 0))
                                       (let ((group (c:output-share-group output)))
                                         (and group
                                              (format nil "~{~a~^, ~}"
                                                      (mapcar (lambda (member)
                                                                (or (c:output-name member) "?"))
                                                              (remove output group)))))))))
          (notify "~d output~:p" (length outputs))
          outputs))))
