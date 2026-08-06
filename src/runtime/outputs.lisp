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
        (guarded "layer shell output destroy"
          (river:river-layer-shell-output-v1.destroy layer))
        (setf (c:prop output :layer-shell) nil)))
    (remhash proxy (server-outputs *server*))
    (setf (c:world-outputs *world*) (remove output (c:world-outputs *world*)))
    (guarded "output destroy" (river:river-output-v1.destroy proxy))
    (setf (c:output-proxy output) nil)
    (rehome-orphaned-workspace was-showing)
    (when announced (run-hooks :output-removed output))
    (mark-dirty)
    (request-manage)
    (logmsg :info "output removed: ~s" output))
  nil)

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
      (let ((adopter (or (output-showing-workspace (first (current-path)))
                         (first outputs))))
        (when (and adopter (c:world-focused-float *world*))
          (setf (c:world-focused-float *world*) nil))
        (when adopter
          (setf (c:prop adopter :workspace) index)
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