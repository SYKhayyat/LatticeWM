;;;; runtime/session.lisp --- Connect, bind, and run the manage/render loop.
;;;;
;;;; THE LOOP, from river's own specification:
;;;;
;;;;   1. Server sends all state changes since the last manage sequence, then
;;;;      manage_start.
;;;;   2. Client modifies window-management state and rendering state, then
;;;;      manage_finish.
;;;;   3. Server sends the new state to the windows and waits for responses.
;;;;   4. Server sends new window dimensions to the client, then render_start.
;;;;   5. Client modifies rendering state, then render_finish.
;;;;   6. If dimensions changed, go to 4.  If manage-requiring state changed,
;;;;      or the client sends manage_dirty, go to 1.
;;;;
;;;; A manage sequence is *always* followed by at least one render sequence.
;;;; Several render sequences may happen consecutively — a window changing its
;;;; own size causes one — so a render handler must be cheap and idempotent.
;;;;
;;;; PROTOCOL VERSION IS CHECKED AT BIND TIME AND WE REFUSE TO START ON A
;;;; MISMATCH.  river-window-management-v1 is young and a pre-release protocol
;;;; can break within a version number rather than politely bumping to v2.
;;;; PLAN.org calls that the largest single threat to the "survives without AI"
;;;; requirement — after the budget expires, a river upgrade could break the
;;;; window manager with nobody available to repair it.  A clear refusal naming
;;;; both version numbers is repairable by a non-programmer.  Silent
;;;; misbehaviour is not.

(in-package #:latticewm/runtime)

(defparameter +window-management-version+ 4
  "The river_window_manager_v1 version this build was generated against.

Bumping this means regenerating from a new XML and re-running the codegen
count check.  It is deliberately not `whatever the compositor offers'.")

(defparameter +xkb-bindings-version+ 3
  "The river_xkb_bindings_v1 version this build was generated against.")

(define-condition protocol-version-mismatch (error)
  ((interface :initarg :interface :reader mismatch-interface)
   (wanted :initarg :wanted :reader mismatch-wanted)
   (offered :initarg :offered :reader mismatch-offered))
  (:report
   (lambda (condition stream)
     (format stream
             "~&This build of LatticeWM was generated against ~a version ~d,~%~
              but the running compositor offers version ~d.~%~%~
              Refusing to start rather than misbehave: the two versions may~%~
              disagree about what a request means.~%~%~
              To fix this, re-vendor the protocol XML from the river you are~%~
              running, then rebuild:~%~%~
              ~2tcp $RIVER/share/river-protocols/stable/*.xml src/protocol/~%~
              ~2t# set +WINDOW-MANAGEMENT-VERSION+ in src/runtime/session.lisp~%~
              ~2tmake~%"
             (mismatch-interface condition) (mismatch-wanted condition)
             (mismatch-offered condition))))
  (:documentation
   "Signalled at startup when the compositor's protocol version is not the one
we generated bindings from."))

;;; ------------------------------------------------------------ connecting

(defun dispatch-one-event (display)
  "Dispatch a single event, surviving anything wrong with it.

Two things go wrong here in normal operation and neither should be fatal:

  * an event for an object we already destroyed — the race libwayland has
    zombie proxies for and wayflan does not;
  * an enum value we do not know.  wl_shm advertises every pixel format the
    GPU supports as DRM fourcc codes, and a binding generated from a protocol
    XML knows only the handful the XML names.  *This killed the window manager
    on startup the first time an shm global was bound*, from an event nobody
    reads, about formats nobody asked for.

The general rule they share: a compositor newer than our bindings must be able
to say things we do not understand without taking the session down.  Skipping
one event is always safe, because wayflan drains the whole message body into a
separate buffer before invoking any handler — so the stream is already at the
next message when this returns."
  (handler-case (wl:wl-display-dispatch-event display)
    (wl:wl-message-error (condition)
      (logmsg :debug "stale event ignored: ~a" condition))
    (wl:wl-server-error (condition) (error condition))
    (end-of-file (condition) (error condition))
    (error (condition)
      (logmsg :debug "undecodable event ignored: ~a" condition))))

(defun safe-roundtrip (display)
  "WL-DISPLAY-ROUNDTRIP, tolerating events we cannot decode.

The roundtrip in BIND-GLOBALS dispatches everything the compositor volunteers
about the globals we just bound — including wl_shm's list of every pixel
format the GPU supports, which a generated binding does not know.  Doing this
by hand rather than calling the library's roundtrip is the price of surviving
that."
  (let ((done nil))
    (let ((callback (wl:wl-display.sync display)))
      (push (lambda (event &rest arguments)
              (declare (ignore arguments))
              (when (eq event :done) (setf done t)))
            (wl:wl-proxy-hooks callback))
      (loop until done
            do (dispatch-one-event display)))))

(defun bind-globals (server)
  "Bind the river globals we need, refusing to start on a version mismatch."
  (let ((display (server-display server))
        (found '()))
    (let ((registry (wl:wl-display.get-registry display)))
      (setf (server-registry server) registry)
      (push (wl:evlambda
              (:global (name interface version)
               (push (list name interface version) found)))
            (wl:wl-proxy-hooks registry))
      (safe-roundtrip display)
      (dolist (entry (nreverse found))
        (destructuring-bind (name interface version) entry
          (cond
            ((string= interface "river_window_manager_v1")
             (unless (= version +window-management-version+)
               (error 'protocol-version-mismatch
                      :interface interface :wanted +window-management-version+
                      :offered version))
             (setf (server-version server) version
                   (server-manager server)
                   (wl:wl-registry.bind registry name
                                        'river:river-window-manager-v1 version)))
            ((string= interface "river_xkb_bindings_v1")
             (if (= version +xkb-bindings-version+)
                 (setf (server-bindings server)
                       (wl:wl-registry.bind registry name
                                            'river:river-xkb-bindings-v1 version))
                 (logmsg :warn "river_xkb_bindings_v1 is version ~d, we want ~d; ~
                                keybindings disabled"
                         version +xkb-bindings-version+)))
            ((string= interface "wl_compositor")
             (setf (server-compositor server)
                   (wl:wl-registry.bind registry name 'wl:wl-compositor
                                        (min version 4))))
            ((string= interface "wl_shm")
             (setf (server-shm server)
                   (wl:wl-registry.bind registry name 'wl:wl-shm 1)))
            ((string= interface "river_layer_shell_v1")
             (setf (server-layer-shell server)
                   (wl:wl-registry.bind registry name
                                        'river:river-layer-shell-v1 version))))))
      (unless (server-manager server)
        (error "This compositor does not offer river_window_manager_v1.~%~
                LatticeWM is a window manager *for river*, and needs river~%~
                0.4 or later.  Are you running it inside the right~%~
                compositor?  WAYLAND_DISPLAY is ~s."
               (uiop:getenv "WAYLAND_DISPLAY")))
      (logmsg :info "bound river_window_manager_v1 v~d" (server-version server))
      server)))

(defun attach-manager-hooks (server)
  "Listen to the window manager global: the whole protocol arrives here."
  (push
   (lambda (event &rest arguments)
     (with-abandon
       (case event
         (:window (attach-window (first arguments)))
         (:output (attach-output (first arguments)))
         (:seat (attach-seat (first arguments)))
         (:manage-start (run-manage-sequence))
         (:render-start (run-render-sequence))
         (:session-locked (setf (c:prop *world* :locked) t))
         (:session-unlocked (setf (c:prop *world* :locked) nil))
         (:finished (setf (server-running server) nil))
         (t (logmsg :debug "manager event ~s ~s" event arguments)))))
   (wl:wl-proxy-hooks (server-manager server)))
  server)

(defun attach-output (proxy)
  "Register an output and follow its position and size."
  (let ((output (make-instance 'c:output :proxy proxy)))
    (setf (gethash proxy (server-outputs *server*)) output)
    (setf (c:world-outputs *world*)
          (append (c:world-outputs *world*) (list output)))
    (push (lambda (event &rest arguments)
            (with-abandon
              (case event
                (:name (setf (c:output-name output) (first arguments)))
                (:position
                 (setf (c:rect-x (c:output-rect output)) (first arguments)
                       (c:rect-y (c:output-rect output)) (second arguments))
                 (mark-dirty))
                (:dimensions
                 (setf (c:rect-w (c:output-rect output)) (first arguments)
                       (c:rect-h (c:output-rect output)) (second arguments))
                 (mark-dirty))
                (:removed
                 (remhash proxy (server-outputs *server*))
                 (setf (c:world-outputs *world*)
                       (remove output (c:world-outputs *world*)))
                 (mark-dirty))
                (t nil))))
          (wl:wl-proxy-hooks proxy))
    ;; A new monitor brings its own workspace, or it mirrors the first one and
    ;; looks broken.
    (guarded "workspaces for outputs" (p:ensure-workspaces-for-outputs *world*))
    (mark-dirty)
    (logmsg :info "output appeared: ~s (workspace ~a)"
            output (c:prop output :workspace))
    output))

(defun attach-seat (proxy)
  "Register a seat, and give it its keyboard bindings."
  (let ((seat (make-instance 'seat :proxy proxy)))
    (push seat (server-seats *server*))
    (push (lambda (event &rest arguments)
            (with-abandon
              (case event
                (:pointer-position
                 (setf (seat-pointer-x seat) (first arguments)
                       (seat-pointer-y seat) (second arguments))
                 ;; The spec is explicit that a pointer move alone must not
                 ;; start a manage sequence, so focus-follows-mouse cannot be
                 ;; driven from here without asking for one.
                 (when p:*focus-follows-mouse* (pointer-moved seat)))
                (:modifiers (setf (seat-modifiers seat) (first arguments)))
                (t nil))))
          (wl:wl-proxy-hooks proxy))
    (when (server-bindings *server*)
      (setf (seat-bindings-seat seat)
            (w:bindings-get-seat (server-bindings *server*) proxy))
      (push (lambda (event &rest arguments)
              (with-abandon
                (case event
                  (:ate-unbound-key (handle-unbound-key (first arguments)))
                  (t nil))))
            (wl:wl-proxy-hooks (seat-bindings-seat seat)))
      ;; Registration itself is fine here, but ENABLE is window-management
      ;; state and is therefore manage-sequence-only.  A seat arrives during
      ;; the initial roundtrip, when no sequence is in progress, so the work is
      ;; deferred to the first manage sequence.
      ;;
      ;; This is the sequence discipline doing exactly what it was built for:
      ;; it caught the violation at the point of use, before any bytes reached
      ;; the wire, instead of letting river kill the connection with
      ;; sequence_order and leaving a hang to debug.
      (setf (server-bindings-dirty *server*) t))
    (logmsg :info "seat appeared")
    seat))

(defun pointer-moved (seat)
  "Focus follows the pointer, if that is turned on."
  (let* ((policy (p:current-policy))
         (path (guarded "pointer-focus"
                 (p:pointer-focus policy *world*
                                  (seat-pointer-x seat) (seat-pointer-y seat)))))
    (when (and path (not (c:path-equal path (current-path))))
      (p:jump-cursor policy *world* path)
      (mark-dirty))))

;;; --------------------------------------------------------- keybindings

(defun register-bindings (seat)
  "Ask river for every key the keymap binds, including chord prefixes.

Must be called inside a manage sequence: river_xkb_binding_v1.enable is
window-management state."
  (let ((bindings (server-bindings *server*)))
    (unless bindings (return-from register-bindings nil))
    (dolist (entry (all-bound-keys))
      (let ((key (car entry)))
        (unless (gethash key (seat-bound-keys seat))
          (let ((binding (guarded "get_xkb_binding"
                           (w:bindings-get-xkb-binding
                            bindings (seat-proxy seat) (car key) (cdr key)))))
            (when binding
              (setf (gethash key (seat-bound-keys seat)) binding)
              (push (let ((key key))
                      (lambda (event &rest arguments)
                        (declare (ignore arguments))
                        (with-abandon
                          (case event
                            (:pressed (when (handle-key key) (after-command)))
                            (t nil)))))
                    (wl:wl-proxy-hooks binding))
              (guarded "enable binding" (w:binding-enable binding)))))))
    (logmsg :info "~d key~:p bound" (hash-table-count (seat-bound-keys seat)))))

(defun rebind-keys ()
  "Re-register bindings after the keymap changed at a REPL.

This is what makes a keymap edit take effect without a restart, and it is why
DEFINE-KEY does not need to know about the compositor.  The work is deferred
to the next manage sequence, because that is the only place it is legal — so
this is safe to call from anywhere, including a SWANK thread."
  (when *server*
    (setf (server-bindings-dirty *server*) t)
    (request-manage)))

;;; ------------------------------------------------------------- the loop

(defun mark-dirty ()
  "Note that the layout needs recomputing, and make sure it happens.

Inside a protocol sequence this only sets the flag — the sequence we are
already in will do the work, and asking for another would be a wasted round
trip.  *Outside* one it also asks river for a manage sequence, because
otherwise nothing would ever collect the flag.

That second half is not an optimisation, it is the difference between a
command working and not.  Commands invoked from a key binding run inside a
manage sequence and were fine; the identical command invoked from a REPL or a
script set the flag and stopped, so the model changed and the screen did not.
Zoom looked completely broken while being completely correct — the viewport
said 3x2 and the display showed one cell — and no unit test could have seen it,
because the model was right."
  (when *server*
    (setf (server-dirty *server*) t)
    (when (null w:*sequence*) (request-manage))))

(defun in-wm-thread-p ()
  "True when we are running on the thread that owns the compositor socket."
  (or (null *wm-thread*) (eq sb-thread:*current-thread* *wm-thread*)))

(defun request-manage ()
  "Ask river to start a manage sequence, because we want to change something
only a manage sequence may change.

There is no frame or timer event in this protocol, so this is the only way to
drive anything of our own.  It costs a full manage round trip — it is not a
frame clock.

*Safe from any thread*, and that is not a nicety.  This writes to the
compositor socket, and the wayflan client is single-threaded, so calling it
from a SWANK REPL raced with whatever the window manager thread was
marshalling — which does not error, it *hangs*, holding the desktop.  Every
command that changes window-management state calls this, so without the guard
below almost the entire command set was a REPL hazard.  Off-thread callers
queue and wake the loop instead."
  (cond
    ((not (and *server* (server-manager *server*))) nil)
    ((in-wm-thread-p)
     (guarded "manage_dirty" (w:wm-manage-dirty (server-manager *server*))))
    (t (call-in-wm-thread #'request-manage))))

(defun after-command ()
  "Called after a key binding runs.  Push the consequences out.

MARK-DIRTY does the asking now, so this is a single call; it is kept as a
named step because it is the obvious place to hang anything that should happen
once per user action rather than once per relayout."
  (mark-dirty))

(defun run-manage-sequence ()
  "Everything that is only legal in a manage sequence.

Placement happens here rather than when a window appears, because at the
moment `window' arrives we know nothing about the window — its app_id, title
and parent are all still in flight — and SHOULD-FLOAT-P would be answering a
question about a blank."
  (w:with-manage-sequence ((server-manager *server*))
    (when (server-bindings-dirty *server*)
      (setf (server-bindings-dirty *server*) nil)
      (dolist (seat (server-seats *server*)) (register-bindings seat)))
    (place-unplaced-windows)
    (when (server-dirty *server*)
      (setf (server-dirty *server*) nil)
      (relayout))
    (emit-window-management-state)
    (emit-dimension-work)
    (apply-keyboard-focus)
    (arm-capture)))

(defun run-render-sequence ()
  "Everything that a render sequence may change.

Called after every manage sequence, and again whenever a window changes its
own size.  Cheap because the emitter diffs: in the common case where nothing
moved, this sends nothing at all."
  (w:with-render-sequence ((server-manager *server*))
    (when (server-dirty *server*)
      (setf (server-dirty *server*) nil)
      (relayout))))

(defun cursor-on-empty-pane-p ()
  "True when the cursor rests on a deliberately empty pane and no float has
the keyboard."
  (let ((leaf (current-leaf)))
    (and leaf (c:leaf-empty-p leaf) (null (c:world-focused-float *world*)))))

(defparameter +capture-keysyms+
  (append (loop for code from #x20 to #x7e collect code)   ; printable ASCII
          (list #xff08 #xff09 #xff0d #xff1b #xff8d))       ; bs tab ret esc kpret
  "Every key the window manager may want to read directly.

Bound once, enabled only while something is reading — see ARM-CAPTURE.  Ninety
-eight bindings sounds like a lot and is one round trip at startup; the
alternative is not being able to read text at all, because river delivers keys
to the focused *window* and gives us only what we asked for.")

(defun capture-wanted-p ()
  "Is anything currently waiting to read a key directly?

Two things ever are: a prompt in the echo area, and README D19's empty pane.
They share the machinery because they are the same question — *should the next
keypress belong to the window manager rather than to a window?* — and having
two answers to one question is how they end up disagreeing."
  (or (reading-p) (cursor-on-empty-pane-p)))

(defun ensure-capture-bindings (seat)
  "Create the capture bindings, once."
  (let ((bindings (server-bindings *server*)))
    (when (and bindings (null (c:prop seat :capture-bindings)))
      (setf (c:prop seat :capture-bindings)
            (loop for keysym in +capture-keysyms+
                  for binding = (guarded "get_xkb_binding"
                                  (w:bindings-get-xkb-binding
                                   bindings (seat-proxy seat) keysym '()))
                  when binding
                    collect (progn
                              (push (let ((keysym keysym))
                                      (lambda (event &rest arguments)
                                        (declare (ignore arguments))
                                        (with-abandon
                                          (when (eq event :pressed)
                                            (handle-captured-key keysym)))))
                                    (wl:wl-proxy-hooks binding))
                              (cons keysym binding))))))
  (c:prop seat :capture-bindings))

(defun handle-captured-key (keysym)
  "A key arrived because we had asked for it.  Decide what it meant."
  (let ((character (when (<= #x20 keysym #x7e) (code-char keysym))))
    (cond
      ((reading-p) (prompt-key keysym character))
      ((cursor-on-empty-pane-p) (spawn-for-empty-pane character))
      (t nil))))

(defun arm-capture ()
  "Enable or disable the capture bindings to match what is being read.

Manage sequence only: enable and disable are window-management state.  Diffed,
because this runs on every manage sequence and there are ninety-eight of them."
  (let* ((seat (primary-seat))
         (bindings-seat (and seat (seat-bindings-seat seat))))
    (when bindings-seat
      (when *pending-keymap*
        (guarded "ensure_next_key_eaten"
          (w:bindings-seat-ensure-next-key-eaten bindings-seat)))
      (let ((wanted (capture-wanted-p)))
        (ensure-capture-bindings seat)
        (unless (eq wanted (c:prop seat :capture-armed))
          (setf (c:prop seat :capture-armed) wanted)
          (loop for (nil . binding) in (c:prop seat :capture-bindings)
                do (guarded "capture binding"
                     (if wanted (w:binding-enable binding)
                         (w:binding-disable binding)))))))))

(defun spawn-for-empty-pane (character)
  "Run the command CHARACTER names, if the cursor is still on an empty pane.

README D19: while the cursor rests on an empty pane, an unbound printable key
is looked up in a table -- e opens an editor, t a terminal -- so the empty pane
is a spawn menu with no menu.

THE DESIGN GUESSED THE MECHANISM WRONG AND THE ANSWER IS BETTER THAN ITS
FALLBACK.  D19 rests on ate_unbound_key and asks whether it carries the keysym,
ruling that if it does not, only a single-default mode is possible and the table
dies.  Measured against the shipped protocol, ate_unbound_key has *no arguments
at all*.  But binding the keys ourselves and enabling them conditionally gets
the keysym, keeps the table, and never intercepts a key that would have gone to
an application -- because the binding is not enabled then."
  (when (and character (cursor-on-empty-pane-p))
    (let ((command (guarded "key-unbound"
                     (p:key-unbound (p:current-policy) *world* character))))
      (when command
        (logmsg :debug "empty pane: ~a -> ~a" character command)
        (run-command command)
        (after-command)))))

;;; ---------------------------------------------------------------- SWANK

(defvar *swank-port* 4005
  "The port the SWANK server listens on, or NIL to not start one.")

(defvar *swank-thread* nil)

(defun start-swank (&optional (port *swank-port*))
  "Start SWANK in a thread so a REPL can be attached to the running window
manager.

This is the entire reason the project is in Common Lisp, so it starts before
anything interesting exists rather than as a debugging afterthought.  Connect
from Emacs with M-x slime-connect, and redefine anything:

    (defmethod p:gaps ((policy p:conventional-policy) container) 8)
    (r:relayout :force t)

SWANK runs in another thread, and the wayflan client is single-threaded, so
anything you evaluate that touches the compositor should go through
CALL-IN-WM-THREAD.  Redefining a method is always safe — it touches no
socket."
  (when port
    (handler-case
        (progn
          (funcall (read-from-string "swank:create-server")
                   :port port :dont-close t :style :spawn)
          (logmsg :info "SWANK listening on port ~d" port)
          port)
      (error (condition)
        (logmsg :warn "could not start SWANK: ~a" condition)
        nil))))

(defvar *wm-thread-queue* '()
  "Thunks queued by other threads, to be run in the window manager thread.")

(defvar *wm-thread-lock* (bt:make-lock "latticewm-queue"))

(defun call-in-wm-thread (thunk)
  "Queue THUNK to run in the window manager thread, and wake the loop.

The wayflan client is single-threaded — this is StumpWM's pattern, and it is
the reason a REPL cannot simply call protocol functions directly."
  (bt:with-lock-held (*wm-thread-lock*)
    (push thunk *wm-thread-queue*))
  ;; Wake the loop rather than touching the socket from this thread: the
  ;; wayflan client is single-threaded, so writing a request from here would
  ;; race with whatever the window manager thread is marshalling.
  (wake-event-loop)
  t)

(defmacro in-wm (&body body)
  "Run BODY in the window manager thread.  For use from a SWANK REPL."
  `(call-in-wm-thread (lambda () ,@body)))

(defun drain-wm-queue ()
  "Run everything other threads have queued for us."
  (let ((queue (bt:with-lock-held (*wm-thread-lock*)
                 (prog1 (nreverse *wm-thread-queue*)
                   (setf *wm-thread-queue* '())))))
    (dolist (thunk queue)
      (guarded "queued from another thread" (funcall thunk)))
    (when queue (mark-dirty))))
