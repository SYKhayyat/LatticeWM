;;;; runtime/windows.lisp --- Window lifecycle, driven by river's events.
;;;;
;;;; Every window we will ever manage arrives as a `new_id' inside a `window'
;;;; event on river_window_manager_v1, which is why server-created new_id
;;;; handling was the first thing verified in the binding library.
;;;;
;;;; The order of the events at creation is the thing to hold on to: river
;;;; sends `window', then a burst of property events (app_id, title,
;;;; identifier, parent, dimensions_hint), then `manage_start'.  So we must not
;;;; decide where a window goes the moment it appears — at that instant we know
;;;; nothing about it, and SHOULD-FLOAT-P would be answering about a blank.
;;;; Placement is deferred to the first manage sequence, by which time
;;;; everything has arrived.

(in-package #:latticewm/runtime)

;;; *UNPLACED* is declared in runtime/server.lisp, with the rest of the state
;;; that loads before the things that consult it.

(defun attach-window (proxy)
  "Register a newly announced river_window_v1 and start listening to it."
  (let ((window (make-instance 'c:window :proxy proxy)))
    (setf (gethash proxy (server-windows *server*)) window)
    (push (evlambda-for-window window) (wl:wl-proxy-hooks proxy))
    (push window *unplaced*)
    (logmsg :debug "window appeared: ~s" window)
    window))

(defun evlambda-for-window (window)
  "The event handler for one window.

Everything here is bookkeeping: record what river tells us and mark the layout
dirty.  No placement decisions are made in an event handler, because an event
handler runs at an arbitrary point in the protocol and has no idea which
sequence it is in."
  ;; Declared rather than wrapped in ON-EVENTS: this one *returns* the handler
  ;; instead of pushing it, so the macro's shape does not fit.  The declaration
  ;; is what gate 8 checks, and it has to be kept beside the CASE by hand --
  ;; which is exactly the drift the macro exists to prevent, so it is the one
  ;; place in the system where that risk is real.
  (load-time-value
   (declare-handled-events "river_window_v1"
    '(:app-id :title :identifier :parent :dimensions :dimensions-hint :decoration-hint :closed :fullscreen-requested :exit-fullscreen-requested :minimize-requested :maximize-requested :unmaximize-requested :pointer-move-requested :pointer-resize-requested :show-window-menu-requested :unreliable-pid :presentation-hint :capture-sessions))
   t)
  (lambda (event &rest arguments)
    (with-abandon
      (case event
        (:app-id (setf (c:window-app-id window) (first arguments)))
        (:title (setf (c:window-title window) (first arguments)))
        (:identifier (setf (c:window-identifier window) (first arguments)))
        (:parent
         (setf (c:window-parent-window window)
               (and (first arguments) (window-of-proxy (first arguments)))))
        (:dimensions
         (setf (c:window-width window) (first arguments)
               (c:window-height window) (second arguments))
         ;; The size we asked for is advisory and this is the answer.  Redo the
         ;; placement so gravity can centre a window that came back small.
         (mark-dirty))
        (:dimensions-hint
         (destructuring-bind (min-w min-h max-w max-h) arguments
           (setf (c:window-min-width window) min-w
                 (c:window-min-height window) min-h
                 (c:window-max-width window) max-w
                 (c:window-max-height window) max-h)))
        (:decoration-hint (setf (c:window-decoration-hint window)
                                (first arguments)))
        (:closed (detach-window window))
        (:fullscreen-requested (request-fullscreen window t))
        (:exit-fullscreen-requested (request-fullscreen window nil))
        (:minimize-requested (minimize-window window))
        (:maximize-requested (best-effort "inform_maximized"
                               (w:window-inform-maximized (c:window-proxy window))))
        (:unmaximize-requested
         (best-effort "inform_unmaximized"
           (w:window-inform-unmaximized (c:window-proxy window))))
        ;; Somebody grabbed the window's own titlebar or corner.  Clients that
        ;; draw their own decorations ask for this through xdg-shell and river
        ;; forwards it; ignoring it makes a GTK titlebar inert, which reads as
        ;; the window manager being broken rather than as a policy.
        (:pointer-move-requested
         (on-client-move-request window (seat-of-proxy (first arguments))))
        (:pointer-resize-requested
         (on-client-resize-request window (seat-of-proxy (first arguments))
                                   (second arguments)))
        ;; A right-click on a client-side titlebar.  There is no window menu to
        ;; put up -- every operation it would offer is a command with a key --
        ;; so the honest answer is to say what the keys are, in the one place
        ;; the program can talk.
        (:show-window-menu-requested
         (notify "window menu: ~a float  ~a fullscreen  ~a close"
                 (or (p:keys-running (p:current-policy) "toggle-float") "?")
                 (or (p:keys-running (p:current-policy) "toggle-fullscreen") "?")
                 (or (p:keys-running (p:current-policy) "close") "?")))
        ;; Advisory and unreliable by its own name, and useful anyway: it is
        ;; the only way to tell two windows of the same application apart when
        ;; writing a rule.
        (:unreliable-pid (setf (c:prop window :pid) (first arguments)))
        (:presentation-hint (setf (c:prop window :presentation-hint)
                                  (first arguments)))
        ;; Something is recording this window: a screen share, a recorder, a
        ;; thumbnailer.  Bookkeeping like everything else here -- what is done
        ;; with it is runtime/capture.lisp's, and nothing about the window's
        ;; placement changes because of it.
        (:capture-sessions (note-capture-sessions window (first arguments)))
        (t (logmsg :debug "window event ~s ~s" event arguments))))))

(defun detach-window (window)
  "River says the window is gone.  Take it out of everything.

AND GIVE THE COMPOSITOR ITS OBJECTS BACK, WHICH THIS DID NOT DO.  Both
`river_window_v1.destroy' and `river_node_v1.destroy' existed in wire/wrappers,
exported, wrapped and documented, and were called by nothing at all — so every
window that ever closed leaked a compositor object and a wayflan proxy for the
life of the session, on the teardown path that runs more often than any other.
DETACH-OUTPUT, DETACH-SEAT and FORGET-INPUT-DEVICE all destroy theirs; the one
that forgot is the one that runs every time you close a window.

The protocol is explicit: `The client should destroy this object with the
river_window_v1.destroy request to free up resources', after `closed', which is
the event that brought us here.  The node goes first because it was made from
the window."
  (setf (c:window-live-p window) nil)
  (let ((node (gethash window (server-nodes *server*))))
    (when node (best-effort "node destroy" (w:node-destroy node))))
  (let ((proxy (c:window-proxy window)))
    (when proxy
      (remhash proxy (server-windows *server*))
      ;; BEST-EFFORT and not GUARDED: a proxy river has already torn down its
      ;; side of is an ordinary outcome on this path, not a bug of ours.
      (best-effort "window destroy" (w:window-destroy proxy))))
  (remhash window (server-nodes *server*))
  (forget-window-state window)
  (setf *unplaced* (remove window *unplaced*))
  ;; Out of the float list and the scratchpad, wherever it was.
  (setf (c:world-floats *world*)
        (remove window (c:world-floats *world*) :key #'c:float-window)
        (c:world-scratchpad *world*)
        (remove window (c:world-scratchpad *world*)))
  ;; And out of the tree, through the policy, so that focus repair and the
  ;; collapse rules are the ones the user configured.
  (let* ((root (c:world-root *world*))
         (leaf (c:leaf-holding root window))
         (path (and leaf (c:node-path-to root leaf))))
    (when path
      (guarded "on-window-close"
        (p:on-window-close (p:current-policy) *world* window path))))
  (setf (c:window-proxy window) nil)
  (run-hooks :window-closed window)
  (mark-dirty)
  (logmsg :debug "window closed: ~s" window))

(defun window-placed-p (window)
  "True when WINDOW already has a place: tiled, floating, or put away.

The question PLACE-UNPLACED-WINDOWS has to ask before it places anything, and it
is asked of the world rather than of the compositor -- a window the compositor
announced and a snapshot already mentions is placed, whatever the arrival list
says (#144)."
  (or (not (null (c:leaf-holding (c:world-root *world*) window)))
      (c:window-floating-p window)
      (c:window-minimized-p window)
      (not (null (member window (c:world-scratchpad *world*))))))

(defun place-unplaced-windows ()
  "Place every window that has appeared since the last manage sequence.

Called from inside a manage sequence, which is where the window-management
requests this makes — set_capabilities, use_ssd, propose_dimensions — are
legal."
  (let ((pending (nreverse *unplaced*))
        (policy (p:current-policy)))
    (setf *unplaced* '())
    (dolist (window pending)
      ;; A WINDOW THAT ALREADY HAS A PLACE IS NOT PLACED AGAIN.  A restore can
      ;; put a window the compositor has announced but this program has not yet
      ;; placed into the tree from a snapshot (#144), and then the manage pass
      ;; would place it a second time -- the third member of the family that also
      ;; holds a float after an undo (#141) and a minimized window after an undo
      ;; (#142).  One window, one place.
      (when (and (c:window-live-p window) (not (window-placed-p window)))
        (declare-window-capabilities policy window)
        (let ((path (guarded "on-window-open"
                      (p:on-window-open policy *world* window))))
          (when (and (null path) (c:window-floating-p window))
            (float-window-now policy window
                              :focus (and (c:prop window :focus-on-open t) t)))
          (run-hooks :window-opened window)
          (logmsg :info "placed ~s at ~s" window path))))
    (when pending (mark-dirty))))

(defun declare-window-capabilities (policy window)
  "Tell river which window-menu buttons to draw, and who decorates.

River draws client-side-decoration titlebar buttons based on what we declare,
so getting this wrong shows up as a titlebar with a maximize button that does
nothing."
  (let ((proxy (c:window-proxy window)))
    (when proxy
      (best-effort "set_capabilities"
        (w:window-set-capabilities proxy (p:window-capabilities policy window)))
      (guarded "decoration"
        (if (eq (p:decoration-mode policy window) :ssd)
            (w:window-use-ssd proxy)
            (w:window-use-csd proxy)))
      ;; set_tiled is reconciled by EMIT-WINDOW-MANAGEMENT-STATE, so that there
      ;; is exactly one place that decides it.
      nil)))

;;; ------------------------------------------------------------- floating

(defun float-window-now (policy window &key (focus t))
  "Give WINDOW a floating rectangle and put it on the float list.

FOCUS NIL leaves the keyboard where it is, which is what FOCUS STEALING (#19)
needs for a window that appears without being asked for: a popup is exactly the
window that should not take the cursor, and a popup is exactly the window that
floats.  The floating commands never pass it -- you floated this one on purpose.

What ties the two halves together is the :FOCUS-ON-OPEN prop the policy writes
in ON-WINDOW-OPEN; PLACE-UNPLACED-WINDOWS hands its value to this keyword."
  (let* ((output (or (current-output) (first (all-outputs))))
         ;; A WINDOW RULED TO A PLACE OPENS THERE (#14), which is the declarative
         ;; half of snapping: `(:app-id "mpv") :snap :right' in *window-rules*
         ;; needs no command and no key, and it asks SNAP-RECT the same question
         ;; the command does.
         (place (and output
                     (let ((place (c:prop window :snap-place)))
                       (and (p:snap-place-p place) place))))
         (rect (cond
                 (place (guarded "snap-rect" (p:snap-rect policy output place)))
                 (output (guarded "default-float-rect"
                           (p:default-float-rect policy window output)))
                 (t (c:make-default-float-rect 100 100)))))
    (setf (c:window-floating-p window) t)
    (let ((float (make-instance 'c:floating-window
                                :window window
                                :rect (or rect (c:make-default-float-rect 100 100)))))
      (push float (c:world-floats *world*))
      ;; Focus follows the window you just floated.  Without this, floating
      ;; something takes the keyboard away from it — which is exactly backwards,
      ;; since you floated it in order to use it.
      (when focus
        (setf (c:world-focused-float *world* p:*seat*) float)))
    ;; set_tiled is window-management state and is reconciled by the emitter in
    ;; the next manage sequence.  Sending it here would work from a key binding
    ;; and fail from a REPL.
    (mark-dirty)
    window))

(defun reconcile-tiled-windows (world)
  "Every window WORLD's tree mentions is tiled, so it is in no other place.

A MINIMIZED WINDOW IS IN THE SCRATCHPAD AND NOT IN THE TREE -- that is what
minimize means here, and it is why minimizing is recorded as an undo step.  An
undo that put the window back in the tree while leaving the flag set and the
window in the scratchpad left it in two places: the emitter skipped it (minimized
wins), and RESTORE-WINDOW then found its old slot *occupied by itself*, took the
split branch, and put a second copy in the tree (#142).

Derived from the tree rather than recorded, because the tree is the authority
after a restore.  Returns WORLD."
  (let ((scratchpad (c:world-scratchpad world)))
    (dolist (window (c:node-windows (c:world-root world)))
      (when (c:window-minimized-p window)
        (setf (c:window-minimized-p window) nil
              scratchpad (remove window scratchpad))))
    (setf (c:world-scratchpad world) scratchpad)
    world))

(defun forget-unplaced-float-focus (world)
  "Clear the keyboard of any seat whose focused float is no longer floating.

A FOCUS FLAG IS ABOUT A PLACE.  UNFLOAT-WINDOW clears it for the seat that
unfloated, because that is the seat that asked; a restore reconciles the float
list wholesale and cannot know which seat noticed, so it asks the only question
that matters -- is the place still a place.  Left set, the tiled window under the
cursor is drawn as unfocused, since LEAF-FOCUS-STATE answers :CURSOR rather than
T while a float has the keyboard (#143).  Returns WORLD."
  (let ((places (c:world-floats world)))
    (dolist (seat (world-seats-with-a-place world))
      (let ((record (c:world-focused-float world seat)))
        (when (and record (not (member record places :test #'eq)))
          (setf (c:world-focused-float world seat) nil))))
    world))

(defun world-seats-with-a-place (world)
  "Every seat WORLD knows about, plus the seatless one.

The seatless entry is not a mistake: the accessor defaults SEAT to NIL, and a
program with no seat table at all -- a unit test, a replay -- writes there.  A
sweep that walked only the table would leave that one stale."
  (let ((seats (list nil)))
    (maphash (lambda (seat cursor)
               (declare (ignore cursor))
               (pushnew seat seats))
             (c:world-cursors world))
    seats))

(defun float-anchor-paths (root floats)
  "Where each anchored float's anchor sits in ROOT, as (WINDOW PATH) entries.

A NODE OBJECT DOES NOT SURVIVE A COPY, and that is the whole of #149.  The tree
is copied into a snapshot (`copy-node`), so the anchor a float record points at
-- a node *object* -- is a node of the tree the snapshot was taken FROM.  Undo
installs the snapshot's copy, and the anchor then names a node in no tree: the
render index holds no rectangle for it, so the renderer skips the float and an
anchored window that survived the undo is drawn nowhere at all.

A PATH is how the rest of the model says \"the same node\" across a copy -- the
cursor's repair already uses one -- and a path is available for every node in a
tree, whereas a name is only there for the ones somebody named.  The entry is a
LIST rather than the bare path so that the root (whose path is NIL) can be told
apart from a node that was not in the tree at all."
  (loop for record in floats
        for anchor = (c:float-anchor record)
        for window = (c:float-window record)
        for path = (and anchor (c:node-path-to root anchor))
        ;; NODE-PATH-TO answers NIL for the root and for a node it never found,
        ;; so the root is named separately: an entry for a node the tree does
        ;; not hold would send a restore to the ROOT, which is an anchor, and
        ;; a wrong one.
        when (and anchor window (or path (eq anchor root)))
          collect (list window path)))

(defun reconcile-floats (world records &optional anchor-paths)
  "The float records WORLD should hold, given the tree it now has and RECORDS.

FOR RESTORING A SNAPSHOT, and the rule is that a window is in one place (#141).
A window the tree now mentions stops floating: an undone TOGGLE-FLOAT put it back
in the tree, so its record has to go, or the emitter places it twice.  A window
that was floating in the snapshot comes back floating, WITH THE SNAPSHOT'S
RECTANGLE.  A dead window's record is dropped, the same rule PRUNE-DEAD-WINDOWS
applies to a leaf.

IT USED TO KEEP THE RECTANGLE THE WINDOW HAD FROM THE LAST DRAG.  The argument
was written here: a float move was not recorded, so undoing an unrelated tiling
change must not snap a float the user had since dragged back to an old
rectangle.  That argument was right and its premise was the bug (#147) -- a
float move was not recorded because the signature was the tree, and a float is
not in the tree.  Now that a move is its own step, the snapshot's rectangle is
exactly what the snapshot is *of*, and keeping the current one turned undo into
a no-op for the only change it was invoked to undo.

A RECORD ON ITS WAY IN IS COPIED, because the ring and the live world must not
share one: an undo, a redo and another undo would otherwise leave the ring
holding whatever the emitter last wrote into the live rectangle.

EXCEPT WHEN THE WINDOW IS ALREADY FLOATING, and there the record the world has is
the one that stays -- a seat's keyboard focus IS a float record, and
FORGET-UNPLACED-FLOAT-FOCUS asks whether that exact object is still on the list
(#143).  Swapping in a copy made every seat's focus on a float die on every undo,
which showed up as `undo, then toggle-float' floating the *tiled* window next to
it instead of putting the float back.  So the copy is of the rectangle, which is
the part the snapshot owns, and the identity is left where the world put it.

THE ANCHOR COMES FROM ANCHOR-PATHS, NOT FROM THE RECORD (#149).  The record's
anchor names a node of the tree the snapshot was taken from, and the tree
installed here is a copy of it, so that node is in no tree and would place
nothing.  ANCHOR-FLOAT and UNANCHOR-FLOAT are undoable steps of their own, so the
snapshot's answer -- anchored to this path, or not anchored at all -- is the one
that has to come back.  An entry with no path means the anchor was not in the
tree when the snapshot was taken, which is not something a restore can improve
on, so that float keeps whatever anchor it has."
  (let ((tiled (c:node-windows (c:world-root world)))
        (out '()))
    (dolist (record records)
      (let ((window (c:float-window record)))
        (when (and window
                   (not (member window tiled))
                   (c:window-live-p window))
          (let* ((live (c:float-of-window world window))
                 (entry (assoc window anchor-paths))
                 ;; A path of NIL is the root, which is why the entry is a list:
                 ;; its presence, not its contents, says the anchor was placed.
                 (anchor (and entry
                              (c:resolve-path (c:world-root world)
                                              (second entry))))
                 ;; Anchored in the snapshot, or known to be unanchored: either
                 ;; way the answer is known.  Otherwise say nothing.
                 (anchor-known (or anchor (null (c:float-anchor record)))))
            (cond
              (live
               (let ((rect (c:float-rect record)))
                 (setf (c:float-rect live) (and rect (c:copy-rect rect)))
                 (when anchor-known (setf (c:float-anchor live) anchor)))
               (push live out))
              (t (let ((copy (c:copy-float-record record)))
                   (when anchor-known (setf (c:float-anchor copy) anchor))
                   (push copy out))))))))
    (nreverse out)))

(defun float-away-from-tree (policy window)
  "Take WINDOW out of the tree and make it a float, remembering the slot it left.

The surgery, extracted from TOGGLE-FLOAT so that #25's TRANSITION-WINDOW can
float a window without a second copy of it.  NIL when the window holds no leaf
to leave -- it is already outside the tree, which is a no-op rather than an
error.

The remembered slot is :FLOAT-HOME-PATH ON THE WINDOW'S PROPS and not
C:WINDOW-HOME-PATH, which is where ON-MINIMIZE records where a window came from
and what ON-RESTORE reads.  The old code wrote the latter, so minimizing a
window, then floating and unfloating anything at all, then restoring, put the
restored window somewhere it had never been: one verb quietly overwriting
another's memory."
  (let* ((root (c:world-root *world*))
         (leaf (c:leaf-holding root window))
         (path (and leaf (c:node-path-to root leaf))))
    (when path
      (setf (c:prop window :float-home-path) path)
      (multiple-value-bind (removed new-root landed)
          (c:tree-remove-at root path :focus-path (current-path))
        (declare (ignore removed))
        (setf (c:world-root *world*) new-root
              (c:world-cursor *world*) (c:repair-path new-root landed)))
      (float-window-now policy window))))

(p:define-option *unfloat-returns-home* t
  "Put an unfloated window back where it was floated from, if that place survives.

T is the symmetry with minimize and restore, and the argument ON-RESTORE's
docstring makes applies verbatim: \"landing back where you were is the
difference between minimize being useful and being a way to lose a window\".
Floating something to look at it and then putting it back is the same gesture,
and it had the opposite behaviour — unfloat dropped the window at the cursor,
wherever that happened to be by then.

NIL restores the old behaviour: an unfloated window lands at the cursor, which
is what you want if you use floating as a way of *moving* windows around.")

(defun unfloat-window (window)
  "Put a floating window back into the tree — where it came from, or at the cursor."
  (let ((float (c:float-of-window *world* window)))
    (when (eq float (c:world-focused-float *world* p:*seat*))
      (setf (c:world-focused-float *world* p:*seat*) nil)))
  (setf (c:world-floats *world*)
        (remove window (c:world-floats *world*) :key #'c:float-window)
        (c:window-floating-p window) nil)
  (let* ((policy (p:current-policy))
         (leaf (c:make-leaf window))
         (home (and *unfloat-returns-home* (c:prop window :float-home-path)))
         (target (and home (c:resolve-path (c:world-root *world*) home)))
         (landed
           (cond
             ;; The pane it was floated out of is still there and still empty:
             ;; it is still yours.
             ((c:empty-pane-p target)
              (setf (c:world-root *world*)
                    (c:tree-replace-at (c:world-root *world*) home leaf))
              home)
             (t
              (let ((here (current-leaf)))
                (p:place-node policy *world* leaf (current-path)
                              (if (and here (c:leaf-empty-p here))
                                  :fill :split)))))))
    (setf (c:prop window :float-home-path) nil)
    (mark-dirty)
    (p:jump-cursor policy *world* landed)
    landed))

;;; --------------------------------------------------- fullscreen, minimize

(defun request-fullscreen (window on)
  "Honour a fullscreen request.  Cheap, in both directions.

River makes a fullscreen window the only thing rendered on its output; clip
boxes are ignored and borders are not drawn.  So entering and leaving cost no
relayout at all, which is why we honour it unconditionally rather than making
it a policy question."
  ;; Model only.  EMIT-WINDOW-MANAGEMENT-STATE reconciles it in the next manage
  ;; sequence, which is the only place the requests are legal.
  (setf (c:window-fullscreen-p window) on)
  (mark-dirty)
  (request-manage)
  window)

(defun minimize-window (window)
  "Take WINDOW out of the tree and onto the scratchpad."
  (guarded "on-minimize" (p:on-minimize (p:current-policy) *world* window))
  ;; No explicit hide: the window is out of the tree, so the next layout does
  ;; not place it, and HIDE-UNPLACED-WINDOWS hides everything the layout did
  ;; not place.  Hiding here as well would be a rendering request outside a
  ;; render sequence.
  ;;
  ;; That second sentence was written before it was true.  The emitter walked
  ;; placements only, so a window taken *out* of the tree was never mentioned
  ;; again and river went on drawing it where it was — minimize left the window
  ;; on screen, and the model was correct throughout.
  (let ((float (c:float-of-window *world* window)))
    (when (eq float (c:world-focused-float *world* p:*seat*))
      (setf (c:world-focused-float *world* p:*seat*) nil)))
  (mark-dirty)
  (request-manage)
  window)

(defun restore-window (window)
  "Bring WINDOW back from the scratchpad.

A WINDOW THAT IS NOT PUT AWAY IS NOT RESTORED, and the guard is not politeness.
The policy's ON-RESTORE puts the window back in its remembered slot when that
slot is *empty* and splits the cursor's pane otherwise -- so restoring a window
that the tree already mentions fills that remembered slot with the window itself,
and then puts a second copy of the same surface in the tree.  The emitter is
asked to tile one window in two places, which is the sort of thing that looks
like a rendering bug and is not (#142).

PUT AWAY MEANS MINIMIZED, and a minimized window may be a float -- that is a
decision the state machine already took, and a float that was put away has to
come back a float.  So the test is *put away and not in the tree*, not *not
floating*: a window that is tiled, or floating without being put away, is already
placed and there is nothing to bring back."
  (cond
    ((and (not (c:window-minimized-p window))
          (not (member window (c:world-scratchpad *world*))))
     window)
    ((member window (c:node-windows (c:world-root *world*))) window)
    (t
     (guarded "on-restore" (p:on-restore (p:current-policy) *world* window))
     (mark-dirty)
     (request-manage)
     window)))

;;; ------------------------------------------------------------ keyboard focus

(defun apply-keyboard-focus ()
  "Derive every seat's Wayland keyboard focus from that seat's cursor.  DESIGN D18.

EVERY SEAT, NOT THE FIRST ONE.  This derived focus once, for `(primary-seat)',
and on a one-seat machine that is the whole job.  With a second seat it was
worse than nothing: the second keyboard's focus was never sent at all, so its
typing went nowhere — every keystroke that was not a bound chord was dropped —
and nobody could tell why.  Each seat has its own place now, so each one is
asked separately and told separately.

The derivation is per seat because P:*SEAT* is what the policy reads; see
APPLY-SEAT-KEYBOARD-FOCUS."
  (let ((policy (p:current-policy)))
    (dolist (seat (server-seats *server*) (values))
      (apply-seat-keyboard-focus policy seat))))

(defun apply-seat-keyboard-focus (policy seat)
  "Derive and send keyboard focus for SEAT, and say so if it changed.

Focus is a *place*.  When the place holds a window that window gets focus;
when it is an empty pane there is nothing for Wayland focus to be, and
clear_focus is the honest answer rather than leaving the last window focused
while the cursor is somewhere else — which would mean your keystrokes go
somewhere other than where the highlight is.

WHAT COUNTS AS THE PLACE IS P:FOCUS-TARGET, and that is the point of the one
line that changed here.  The rule itself was a C:WORLD-FOCUS-WINDOW call in the
middle of this function, so the single idea the README leads with was the single
decision no policy could reach: click-to-focus and sloppy-focus were both
unwritable, in a program whose thesis is that they are methods.  This function
keeps what is genuinely the runtime's: the lock-screen case, the diff, and the
choice of which protocol request says `nothing'."
  (let* ((p:*seat* (seat-key seat))
         ;; Wrapped in a list, deliberately.  A policy answering NIL means `an
         ;; empty pane, clear the keyboard' and is D18's whole point; GUARDED
         ;; also answers NIL when a method signalled.  Clearing the keyboard
         ;; because somebody's FOCUS-TARGET has a typo in it is the wrong
         ;; failure — the right one is to leave focus exactly where it was.
         (answer (guarded "focus-target"
                   (list (p:focus-target policy *world*))))
         (window (if answer (first answer) (and seat (seat-focused seat)))))
    (when seat
      (let ((was (seat-focused seat)))
        (cond
          ;; A layer surface has taken exclusive focus — a screen locker.  The
          ;; protocol says every focus request we make is ignored until it
          ;; clears, so sending them anyway is a window manager fighting a
          ;; locker for the keyboard on every manage sequence and losing,
          ;; quietly, forever.  Forgetting what we last focused is deliberate:
          ;; when the lock clears we want to re-send focus rather than believe
          ;; a cached value the compositor never honoured.
          ((layer-shell-holds-keyboard-p seat)
           (setf (seat-focused seat) nil))
          ((eq window (seat-focused seat)) nil)
          (t
           (setf (seat-focused seat) window)
           (guarded "focus"
             (if (and window (c:window-proxy window) (c:window-live-p window))
                 (w:seat-focus-window (seat-proxy seat) (c:window-proxy window))
                 (w:seat-clear-focus (seat-proxy seat))))))
        ;; THE EVENT THAT SAYS WHAT HAS THE KEYBOARD, off the diff that was
        ;; already here.  :FOCUS-CHANGED reports the *cursor*, and the cursor
        ;; is a place: focusing a float, cycling floats, calling up a named
        ;; scratchpad and a locker taking the keyboard all change who receives
        ;; the next keystroke without moving it.  A status bar attached to the
        ;; only focus hook there was would go on naming the tiled window
        ;; underneath the dialog you were typing into.
        ;;
        ;; Here rather than at the call sites because this is the one place the
        ;; question is answered — D18 makes keyboard focus derived, so deriving
        ;; it is the only moment at which the answer is known to have changed.
        ;; TWO HOOKS, ONE OLD AND ONE NEW.  :KEYBOARD-FOCUS-CHANGED keeps the
        ;; shape and the meaning it had with one seat — a status bar attached
        ;; to it keeps working — and now describes the *default* seat, which is
        ;; the machine's own person.  :SEAT-FOCUS-CHANGED names the seat, so a
        ;; listener can tell two people's focus apart instead of seeing one
        ;; pair of windows after another with no way to know whose they were.
        (unless (eq was (seat-focused seat))
          (run-hooks :seat-focus-changed seat was (seat-focused seat))
          (when (eq seat (default-seat))
            (run-hooks :keyboard-focus-changed was (seat-focused seat))))))))

;;; ------------------------------------------------------- the state machine
;;;
;;; #25.  Five functions above each change one thing about a window, and each is
;;; right; what was missing is a *name* for "move this window to that state",
;;; so that a command, an extension and an IPC call all mean the same thing by
;;; "float this".  This is that name, and it is deliberately thin: the
;;; invariants are the ones the five already keep -- floating remembers where
;;; it came from, unfloating goes back there, minimizing records the slot and
;;; routing it through REPAIR-CURSOR so the focus history stays true -- and this
;;; function's job is to not re-derive them.  A second copy of those rules is
;;; what the issue was filed about.
;;;
;;; Every transition is IDEMPOTENT.  Asking for the state a window is already in
;;; is not an error and does not bump the undo history: a toggle computed from a
;;; stale view, two key bindings pressed at once, or an IPC caller replaying a
;;; journal all end up here, and the answer to "float it again" must be
;;; "it is floating".

(defparameter *window-transitions* '(:tiled :floating :fullscreen :normal
                                     :minimized :visible)
  "The states TRANSITION-WINDOW understands, for the inspector and the error.")

(defun transition-window (window to &key (policy (p:current-policy)))
  "Move WINDOW to state TO.  The one way a window changes state.

TO is one of :TILED, :FLOATING, :FULLSCREEN, :NORMAL, :MINIMIZED, :VISIBLE.
Returns WINDOW when something changed and NIL when it was already there, so a
caller can tell `done' from `nothing to do' without asking first -- which is
what makes it safe to call twice.

:MINIMIZED and :VISIBLE are each other's inverse; :FULLSCREEN and :NORMAL are.
:TILED and :FLOATING are the disposition, and they are a separate axis on
purpose: a fullscreen window is still one or the other underneath, which is what
C:WINDOW-STATE reports and what makes leaving fullscreen land somewhere sensible."
  (unless (member to *window-transitions*)
    (error "not a window state: ~s (one of ~{~s~^, ~})"
           to *window-transitions*))
  (let ((changed
          (case to
            (:minimized
             (unless (c:window-minimized-p window)
               (minimize-window window)))
            (:visible
             (when (c:window-minimized-p window)
               (restore-window window)))
            (:fullscreen
             (unless (c:window-fullscreen-p window)
               (request-fullscreen window t)))
            (:normal
             (when (c:window-fullscreen-p window)
               (request-fullscreen window nil)))
            (:floating
             (unless (or (c:window-floating-p window)
                         (c:window-minimized-p window))
               (float-away-from-tree policy window)))
            (:tiled
             (when (c:window-floating-p window)
               (unfloat-window window))))))
    (if changed window nil)))
