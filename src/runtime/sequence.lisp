;;;; runtime/sequence.lisp --- The manage/render loop, and the watchdog.
;;;;
;;;; THE MOST DELICATE CODE IN THE PROGRAM, and it used to sit in the middle of
;;;; a 690-line file it shared with a REPL server, a keyboard capture table and
;;;; the output registry.  It is here on its own because it deserves to be read
;;;; without scrolling past any of those.
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

(in-package #:latticewm/runtime)
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
    ;; A changed layout is a layout worth writing down.  Debounced by
    ;; SAVE-STATE-IF-NEEDED, which the event loop calls at idle, so this costs a
    ;; SETF here and at most one file write every few seconds there.
    (save-state-soon)
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

(p:define-option *manage-warn-seconds* 0.25
  "Log a warning when a manage sequence takes longer than this.  NIL to stop.

A number rather than a failure, in the shape of gate 6: the point is that a
slow relayout is *visible* rather than that it is fatal.  A quarter second is
about four frames, which is where a person starts to feel it.")

(p:define-option *manage-timeout-seconds* 5
  "Abandon a manage sequence that has run this long.  NIL to never abandon.

THE FAILURE THIS EXISTS FOR IS THE WORST ONE THIS PROGRAM HAS.  River waits
for manage_finish before processing any further input, so a policy method that
loops forever does not hang the window manager -- it hangs *the desktop*, with
no keyboard, no mouse and no way back except a tty.  GUARDED catches a method
that signals and WITH-ABANDON catches an event that breaks; neither catches a
method that simply never returns.

The cost is real and worth stating.  Abandoning unwinds through whatever was
mid-flight, and if that was a request being marshalled the connection can end
up out of step, which river answers by closing it.  So this trades `frozen
until you reboot' for `probably fine, possibly the session ends' -- a bad
trade in general and a good one here, because the first outcome has no
recovery and the second drops you back at a login screen.

WITH-MANAGE-SEQUENCE's UNWIND-PROTECT still sends manage_finish on the way
out, so the sequence itself is closed properly however the body ends.

Five seconds is deliberately far beyond any legitimate relayout.  Nothing that
finishes should ever meet it; it is a backstop, not a budget.")

(defun call-with-watchdog (label thunk)
  "Run THUNK, complaining if it is slow and abandoning it if it never ends."
  (let ((start (get-internal-real-time)))
    (flet ((elapsed ()
             (/ (float (- (get-internal-real-time) start))
                internal-time-units-per-second)))
      (prog1
          (if *manage-timeout-seconds*
              (handler-case
                  (sb-ext:with-timeout *manage-timeout-seconds* (funcall thunk))
                (sb-ext:timeout ()
                  (p:logmsg :error
                            "~a ran for ~,1f seconds and was abandoned. ~
                             Something in the policy is not returning -- the ~
                             desktop would have frozen here."
                            label *manage-timeout-seconds*)
                  nil))
              (funcall thunk))
        (let ((seconds (elapsed)))
          (when (and *manage-warn-seconds* (> seconds *manage-warn-seconds*))
            (p:logmsg :warn "~a took ~,3f seconds" label seconds)))))))

(defmacro with-watchdog ((label) &body body)
  `(call-with-watchdog ,label (lambda () ,@body)))

(defvar *shutdown-run* nil
  "True once the shutdown sequence has been done.  See RUN-SHUTDOWN-ONCE.")

(defun run-shutdown-once ()
  "Run the :SHUTDOWN hooks and save the layout, at most once per session.

QUIT and RESTART-WM each did both directly, and START's UNWIND-PROTECT does
them again on the way out -- so every exit ran every shutdown hook twice and
saved the layout twice.  Harmless for the shipped hooks, which is why nobody
noticed; not harmless for a hook that flushes a file, posts a notification or
closes a connection, which is exactly what a shutdown hook is for.

Found by a session recorder writing its ending block twice."
  (unless *shutdown-run*
    (setf *shutdown-run* t)
    (run-hooks :shutdown)
    (save-state)))

(defun run-manage-sequence ()
  "Everything that is only legal in a manage sequence.

Placement happens here rather than when a window appears, because at the
moment `window' arrives we know nothing about the window — its app_id, title
and parent are all still in flight — and SHOULD-FLOAT-P would be answering a
question about a blank."
  (w:with-manage-sequence ((server-manager *server*))
   (with-watchdog ("manage sequence")
    (when (server-bindings-dirty *server*)
      (setf (server-bindings-dirty *server*) nil)
      (dolist (seat (server-seats *server*))
        (register-bindings seat)
        (attach-pointer-bindings seat))
      ;; New layer surfaces with no opinion belong on the output the cursor is
      ;; on.  Without this the default output is undefined, so a panel that
      ;; does not name one lands wherever the compositor last decided.
      (set-default-layer-output (current-output)))
    (place-unplaced-windows)
    (when (server-dirty *server*)
      (setf (server-dirty *server*) nil)
      (relayout))
    (emit-window-management-state)
    (emit-pending-closes)
    (emit-dimension-work)
    (apply-keyboard-focus)
    (arm-capture))))

(defun run-render-sequence ()
  "Everything that a render sequence may change.

Called after every manage sequence, and again whenever a window changes its
own size.  Cheap because the emitter diffs: in the common case where nothing
moved, this sends nothing at all."
  (w:with-render-sequence ((server-manager *server*))
    (with-watchdog ("render sequence")
      (when (server-dirty *server*)
        (setf (server-dirty *server*) nil)
        (relayout)))))
