;;;; runtime/swank.lisp --- A REPL in the running window manager, and the
;;;; queue that makes talking to it from another thread safe.
;;;;
;;;; Live redefinition of a running window manager is the best property this
;;;; program has and the entire reason the project is in Common Lisp.  It is
;;;; also, as a *default*, a TCP server offering arbitrary code execution with
;;;; no authentication step — so it is off unless asked for, and bound to
;;;; loopback when it is.  See *SWANK-PORT*, which says what that costs and
;;;; what would buy it back, and see runtime/ipc.lisp for the control surface
;;;; that is on by default and is a Unix socket.
;;;;
;;;; THE QUEUE IS NOT OPTIONAL AND IS NOT ABOUT SWANK.  The wayflan client is
;;;; single-threaded, so any thread that is not the window manager's must not
;;;; touch the compositor socket.  Doing it anyway does not error — it *hangs*,
;;;; holding the desktop, because river waits for our manage sequence before
;;;; processing further input.  Every off-thread caller queues instead and
;;;; wakes the loop, which drains the queue at its own safe point.

(in-package #:latticewm/runtime)

;;; ---------------------------------------------------------------- SWANK

(p:define-option *swank-port* nil
  "The TCP port SWANK listens on, or NIL for no SWANK server.

*OFF BY DEFAULT, AND THAT IS A DELIBERATE CHANGE FROM 4005.*  SWANK offers
arbitrary code execution as the logged-in user with no authentication step, and
it used to start unconditionally, before the user had done anything, in a
program that ships as a session binary.  For a development tool that is normal
and excellent; for a default it is the kind of surprise that ends up in a CVE
database rather than in a bug tracker.

WHAT IS LOST BY HAVING IT OFF, SAID HONESTLY -- because this docstring used to
say `nothing is lost' and that was wrong about the audience the project names
two paragraphs earlier.  The control socket in runtime/ipc.lisp is on by
default, is reachable only by the user who owns the session, and covers
scripting completely; and `latticewm --eval' and Super+; are both live.  None
of that is `M-x slime-connect, C-c C-c on a DEFMETHOD, watch the windows move',
which is a Lisper's whole relationship with a running Lisp program and is the
one thing no other Wayland compositor can offer.  A socket that takes one form
per line is a scripting interface.  Nobody rallies around a socket.

So the default stays off and the *remedy is made loud* instead: it is one line,
and the starter configuration this program writes into every user's home
directory now says so, in the paragraph a Lisper reads first, rather than
telling them to connect to a port nothing is listening on.

    latticewm --swank-port 4005
    (setf *swank-port* 4005)   ; in init.lisp

THE STANDING QUESTION IS A UNIX SOCKET.  runtime/ipc.lisp solved this exact
trust problem -- $XDG_RUNTIME_DIR, mode 0600, owner-only, chmod before listen
-- and a SWANK on those terms could be on by default with none of the argument
above applying to it.  SWANK's CREATE-SERVER takes a port, so that is work in
SWANK's socket layer rather than a flag here, and it is not done.  It is the
right shape of answer and it is written down here rather than in a plan nobody
reads.

See also *SWANK-INTERFACE*, which decides who may reach it.")

(p:define-option *swank-interface* "127.0.0.1"
  "Which address SWANK binds to when it is on.

Loopback, explicitly, rather than whatever the default happens to be — because
`whatever the default happens to be' for a REPL that executes arbitrary code is
not a thing to leave to a library's changing mind.  Set it to \"0.0.0.0\" if you
genuinely want a REPL reachable from the network and understand that this hands
your session to anybody who can route to it.")

(defvar *swank-thread* nil)

(defun start-swank (&optional (port *swank-port*))
  "Start SWANK, if a port was asked for, so a REPL can be attached.

Live redefinition of a running window manager is the best property this program
has, and it is the entire reason the project is in Common Lisp — so when it is
on it starts before anything interesting exists rather than as a debugging
afterthought, and a failure in step 5 of START still leaves you a REPL inside
the running image to find out why.

    (defmethod p:gaps ((policy p:conventional-policy) container) 8)
    (r:relayout :force t)

SWANK runs in another thread, and the wayflan client is single-threaded, so
anything you evaluate that touches the compositor should go through
CALL-IN-WM-THREAD.  Redefining a method is always safe — it touches no socket."
  (when port
    (handler-case
        (let ((create (read-from-string "swank:create-server"))
              (interface (find-symbol "*LOOPBACK-INTERFACE*" "SWANK")))
          ;; Bound rather than assumed.  SWANK's own default has been both
          ;; loopback and every interface across its history, and a REPL with
          ;; arbitrary code execution is not a thing to leave to that.
          (when (and interface *swank-interface*)
            (setf (symbol-value interface) *swank-interface*))
          (funcall create :port port :dont-close t :style :spawn)
          (logmsg :warn "SWANK listening on ~a:~d — anything that can reach ~
                         this port can run code as you"
                  (or *swank-interface* "*") port)
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
    ;; THE FOURTH DOOR, AND IT IS THE ONE THIS PROGRAM IS ADVERTISED ON.  Undo
    ;; used to be a wrapper on RUN-COMMAND, so anything a SLIME REPL did to the
    ;; tree was unrecoverable -- the mechanism that exists to make the tree
    ;; recoverable was blind on precisely the surface the project is about.
    ;; NOTE-LAYOUT-SETTLED costs one signature walk when nothing changed.
    (when queue
      (note-layout-settled "from a REPL")
      (mark-dirty))))
