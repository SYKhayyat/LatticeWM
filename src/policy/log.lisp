;;;; policy/log.lisp --- Logging, and the top-level error handler.
;;;;
;;;; THE HANDLER IS NOT OPTIONAL, and it is here rather than in main.lisp so
;;;; that it exists before anything can signal.
;;;;
;;;; DESIGN.org: "a daemon with no controlling terminal *must* install a
;;;; top-level handler / *debugger-hook*, or an unhandled condition will hang
;;;; waiting on a stdin that isn't there."  A window manager that hangs takes
;;;; the whole session with it, because river waits for the manage sequence to
;;;; complete before processing further input and has an `unresponsive'
;;;; protocol error it will eventually use.
;;;;
;;;; The behaviour we want is Common Lisp's actual advantage over a compiled
;;;; binary, and it is worth stating precisely.  An unhandled error in a
;;;; compiled window manager unwinds and the process dies, taking your session
;;;; layout with it.  Here it is logged, the offending operation is abandoned,
;;;; and *the window manager keeps running* — the socket is still open, the
;;;; tree is intact, and you can connect a REPL and fix the function that
;;;; signalled while the desktop you are looking at continues to work.  This is
;;;; why StumpWM users run one image for months.

(in-package #:latticewm/policy)

(defvar *log-level* :info
  "One of :DEBUG, :INFO, :WARN, :ERROR, or NIL for silence.")

(defvar *log-stream* *error-output*
  "Where log lines go.  River's own log captures our stderr, so the default
puts window manager messages and compositor messages in one file, in order,
which is what you want when a placement goes wrong.")

(defparameter +log-levels+ '(:debug 0 :info 1 :warn 2 :error 3))

(defun logmsg (level format &rest arguments)
  "Log at LEVEL if *LOG-LEVEL* admits it.  Never signals."
  (ignore-errors
   (when (and *log-level*
              (>= (getf +log-levels+ level 1) (getf +log-levels+ *log-level* 1)))
     (format *log-stream* "~&[~a] ~?~%" (string-downcase level) format arguments)
     (force-output *log-stream*)))
  nil)

(defmacro guarded (context &body body)
  "Run BODY; log and swallow any error, returning NIL.

Used at every boundary where a *policy method* is called — that is, at every
point where user code runs inside ours.  A bad DEFMETHOD written at a live REPL
must not be able to abort a manage sequence, because an abandoned manage
sequence is a frozen desktop.  It should produce a log line and a window in the
wrong place, which you then fix at the same REPL.

Deliberately *not* used around our own internal calls: swallowing our own bugs
would turn them into silent misbehaviour, which is much harder to find than a
backtrace."
  `(handler-case (progn ,@body)
     (error (condition)
       (logmsg :error "~a: ~a" ,context condition)
       nil)))

(defun install-debugger-hook ()
  "Make an unhandled condition log and abort the operation rather than hang.

Without this, SBCL tries to enter the debugger on a stream that does not exist
when running as a session daemon, and blocks there forever holding an
unfinished manage sequence."
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (logmsg :error "unhandled: ~a" condition)
          (ignore-errors
           (format *log-stream* "~&~a~%"
                   (with-output-to-string (out)
                     (sb-debug:print-backtrace :stream out :count 30))))
          (let ((restart (find-restart 'abandon)))
            (if restart (invoke-restart restart) (abort))))))

(defmacro with-abandon (&body body)
  "Run BODY; on any error, log it with a backtrace and carry on.

Wrapped around each event handler and each iteration of the event loop, so
that one bad event is abandoned and the loop goes round again.

It *handles* rather than only offering a restart, and that distinction was
learned the hard way: relying on the debugger hook to invoke the restart left
a path where a type error in an event handler printed a debugger banner to a
stderr nobody was reading and stopped the window manager dead.  A restart
nothing invokes is not a safety net.  The hook is still installed, as a second
line for anything that escapes this."
  `(restart-case
       (handler-case (progn ,@body)
         (error (condition)
           (logmsg :error "abandoned: ~a" condition)
           (ignore-errors
            (format *log-stream* "~&~a~%"
                    (with-output-to-string (out)
                      (sb-debug:print-backtrace :stream out :count 20))))
           nil))
     (abandon ()
       :report "Abandon this event and keep the window manager running."
       nil)))
