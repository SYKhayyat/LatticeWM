;;;; runtime/ipc.lisp --- A control socket that is not SWANK.
;;;;
;;;; THE PROBLEM WITH SWANK AS THE ONLY EXTERNAL CONTROL SURFACE is not that it
;;;; is bad — it is superb, and live redefinition of a running window manager is
;;;; the best property this program has.  It is that SWANK is a *TCP* server
;;;; offering arbitrary code execution with no authentication step, started
;;;; before the user has done anything, in a program that ships as a session
;;;; binary.  Anything that can reach the port is the logged-in user.  For a
;;;; development tool that is normal; for a default it is the kind of surprise
;;;; that ends up in a CVE database rather than a bug tracker.
;;;;
;;;; So SWANK is opt-in and bound to the loopback interface, and *this* is the
;;;; control surface that is on by default:
;;;;
;;;;   * a Unix domain socket in $XDG_RUNTIME_DIR, mode 0600.  The same trust
;;;;     boundary as the Wayland socket sitting beside it — which is exactly
;;;;     the right answer, because anything that can reach the Wayland socket
;;;;     can already do everything to your session that this can;
;;;;   * one form per line, one result per line, in a package a config file
;;;;     would recognise;
;;;;   * no listener on any network interface, ever.
;;;;
;;;; WHAT IT IS FOR.  Scripting, from a shell or from anything that can open a
;;;; socket, without an editor and without Lisp on the far end:
;;;;
;;;;     latticewm --eval '(workspace 3)'
;;;;     latticewm --eval '(length (all-windows))'
;;;;
;;;; and the same thing from any language, since the protocol is: write a form,
;;;; read a line.
;;;;
;;;; EVERYTHING RUNS IN THE WINDOW MANAGER'S OWN THREAD.  The wayflan client is
;;;; single-threaded, so a form evaluated on the accepting thread would race
;;;; whatever the loop was marshalling — which does not error, it *hangs*,
;;;; holding the desktop.  CALL-IN-WM-THREAD-SYNC is what makes that safe, and
;;;; it is the reason this file is worth more than a shell alias.

(in-package #:latticewm/runtime)

(p:define-option *ipc-socket* t
  "Listen on a Unix domain socket for forms to evaluate.

T uses the default path — $XDG_RUNTIME_DIR/latticewm-$WAYLAND_DISPLAY.sock — a
string or pathname names one, and NIL turns it off entirely.

On by default, and safe to be: a Unix socket in the runtime directory is
reachable only by the user who owns the session, which is the same boundary the
Wayland socket beside it draws.  It is what SWANK should have been and is not.")

(p:define-option *ipc-timeout-seconds* 10
  "How long a single form may run before the socket gives up waiting.

The client gets an answer either way, which is the point: a form that wedges
the window manager should produce a message on the terminal that asked, rather
than a shell that hangs forever with no indication of which side is stuck.")

(defvar *ipc-thread* nil "The accepting thread, or NIL.")
(defvar *ipc-listener* nil "The listening socket, or NIL.")
(defvar *ipc-path* nil "The path the socket is bound to, for unlinking.")

(defun default-ipc-path ()
  "$XDG_RUNTIME_DIR/latticewm-$WAYLAND_DISPLAY.sock.

Named after the display so that two nested sessions on one machine — which is
the development loop — do not fight over one socket."
  (merge-pathnames
   (format nil "latticewm-~a.sock"
           (or (uiop:getenv "WAYLAND_DISPLAY") "wayland-0"))
   (or (uiop:getenv-absolute-directory "XDG_RUNTIME_DIR")
       (uiop:getenv-absolute-directory "TMPDIR")
       #p"/tmp/")))

(defun ipc-socket-path ()
  "Where the control socket lives, or NIL when it is turned off."
  (case *ipc-socket*
    ((nil) nil)
    ((t) (default-ipc-path))
    (t (pathname *ipc-socket*))))

;;; ------------------------------------------- running things in the right thread

(defun call-in-wm-thread-sync (thunk &key (timeout *ipc-timeout-seconds*))
  "Run THUNK in the window manager thread and wait for its value.

Returns (values RESULT :OK), (values CONDITION :ERROR) or (values NIL :TIMEOUT).

The wait is what distinguishes this from CALL-IN-WM-THREAD, and it is why the
timeout is not optional: the window manager thread can be inside a manage
sequence that a bad policy method has wedged, and a control socket that waits
forever for that is a second thing wedged rather than a diagnostic.

Called on the window manager thread already — which happens when a form
evaluated over the socket itself uses this — it simply runs, because queueing
work for the thread you are on and then waiting for it is a deadlock with a
long name."
  (if (in-wm-thread-p)
      (handler-case (values (funcall thunk) :ok)
        (error (condition) (values condition :error)))
      (let ((semaphore (bt:make-semaphore))
            (result nil)
            (status :timeout))
        (call-in-wm-thread
         (lambda ()
           (unwind-protect
                (handler-case (setf result (funcall thunk) status :ok)
                  (error (condition) (setf result condition status :error)))
             (bt:signal-semaphore semaphore))))
        (if (bt:wait-on-semaphore semaphore :timeout timeout)
            (values result status)
            (values nil :timeout)))))

(defun explain-evaluation-error (condition)
  "CONDITION as a message, with a hint when the mistake is a known one.

THE MISTAKE WORTH CATCHING: a handful of commands are registered under a name
that is *not* their Lisp symbol, because the good name is a Common Lisp symbol
we may not redefine — `close' is the one that bites, and `split' resolves to
the CORE class of that name rather than to anything callable.  So the obvious
thing to type at a prompt fails with `the function SPLIT is undefined', which
is true and unhelpful.

Naming the command that exists turns a dead end into a correction."
  (let ((text (princ-to-string condition)))
    (if (typep condition 'undefined-function)
        (let* ((name (string-downcase (string (cell-error-name condition))))
               (command (find-command name)))
          (if (and command (command-symbol command)
                   (not (string-equal name (string (command-symbol command)))))
              (format nil "~a~%  -- there is a command called ~s, but its Lisp ~
                           name is ~(~a~).~%     Write (~(~a~) ...), or ~
                           (run-command ~s ...)."
                      text name (command-symbol command)
                      (command-symbol command) name)
              text))
        text)))

(defun one-line (text)
  "TEXT as a single line: backslash doubled, newline as backslash-n.

Applied to the *whole answer*, after it has been printed, rather than to the
text inside it.  Escaping the inner string and then PRIN1-ing it doubles every
backslash a second time, and the client then sees a stray backslash at the end
of each line — which is the shape of the bug this comment exists because of.

*THE FRAMING IS ONE FORM IN, ONE LINE OUT*, and that has to be true of every
answer or the protocol is a lie.  Two things produce embedded newlines without
being asked: a condition report written as a paragraph — which the good ones
here are — and the pretty printer wrapping a long list.  Either truncates the
answer at the client, which reads its reply with READ-LINE.

The first version of this shipped a multi-line diagnostic and the caller saw
its first line, which is the half that says what went wrong and not the half
that says what to do instead."
  (with-output-to-string (out)
    (loop for character across text
          do (case character
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return nil)
               (t (write-char character out))))))

(defun evaluate-for-ipc (text)
  "Read one form from TEXT, evaluate it in the window manager thread, answer.

Returns a single line.  Every failure mode produces one — an unreadable form, a
form that signals, a form that never returns — because a control socket whose
answer to a bad question is silence is a control socket nobody can debug."
  (let ((form (handler-case
                  (let ((*package* (find-package '#:latticewm/user))
                        (*read-eval* nil))
                    (read-from-string text))
                (error (condition)
                  (return-from evaluate-for-ipc
                    (one-line (format nil "(:error ~s)"
                                      (princ-to-string condition))))))))
    (multiple-value-bind (result status)
        (call-in-wm-thread-sync
         (lambda ()
           (let ((*package* (find-package '#:latticewm/user)))
             (prog1 (eval form) (after-command)))))
      (ecase status
        (:ok (let ((*package* (find-package '#:latticewm/user))
                   (*print-readably* nil)
                   ;; NIL, or the pretty printer wraps a long list across
                   ;; several lines and the client sees the first of them.
                   (*print-pretty* nil)
                   (*print-length* 200)
                   (*print-level* 8))
               (one-line (format nil "(:ok ~a)" (prin1-to-string result)))))
        (:error (one-line (format nil "(:error ~s)"
                                  (explain-evaluation-error result))))
        (:timeout
         (one-line
          (format nil "(:error ~s)"
                  (format nil "the window manager did not answer within ~a seconds"
                          *ipc-timeout-seconds*))))))))

;;; ------------------------------------------------------------ the server

(defun serve-ipc-connection (stream)
  "Answer forms on STREAM until it closes.  One form in, one line out."
  (loop for line = (handler-case (read-line stream nil nil) (error () nil))
        while line
        do (let ((answer (evaluate-for-ipc line)))
             (handler-case
                 (progn (write-line answer stream) (force-output stream))
               (error () (return))))))

(defun ipc-accept-loop (socket)
  "Accept connections until the socket is closed."
  (loop
    (let ((connection (handler-case (sb-bsd-sockets:socket-accept socket)
                        (error () (return)))))
      (unless connection (return))
      (bt:make-thread
       (lambda ()
         (with-abandon
           (let ((stream (sb-bsd-sockets:socket-make-stream
                          connection :input t :output t
                                     :element-type 'character
                                     :external-format :utf-8)))
             (unwind-protect (serve-ipc-connection stream)
               (ignore-errors (close stream))
               (ignore-errors (sb-bsd-sockets:socket-close connection))))))
       :name "latticewm-ipc-connection"))))

(defun start-ipc-server ()
  "Open the control socket and start accepting on it.  Never signals.

A stale socket from a window manager that did not shut down cleanly is unlinked
first, which is safe because the path is named after the Wayland display and
there can only be one of us on it."
  (let ((path (ignore-errors (ipc-socket-path))))
    (when path
      (handler-case
          (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                       :type :stream)))
            (ignore-errors (delete-file path))
            (ensure-directories-exist path)
            ;; Restrict the mode BEFORE the bind, not only after.  A chmod after
            ;; SOCKET-BIND leaves a window in which the node sits at whatever the
            ;; umask allowed, and under a loose umask -- or the /tmp fallback --
            ;; a local user can connect in that window and reach an eval socket.
            ;; Binding under a 0177 umask makes the node 0600 from the instant it
            ;; exists; the chmod below then stays as a second guarantee.
            (let ((old-umask (sb-posix:umask #o177)))
              (unwind-protect
                   (sb-bsd-sockets:socket-bind socket (namestring path))
                (sb-posix:umask old-umask)))
            (ignore-errors (sb-posix:chmod (namestring path) #o600))
            (sb-bsd-sockets:socket-listen socket 8)
            (setf *ipc-listener* socket
                  *ipc-path* path
                  *ipc-thread* (bt:make-thread (lambda () (ipc-accept-loop socket))
                                               :name "latticewm-ipc"))
            (logmsg :info "control socket at ~a" path)
            path)
        (error (condition)
          (logmsg :warn "could not open the control socket at ~a: ~a"
                  path condition)
          nil)))))

(defun stop-ipc-server ()
  "Close the control socket and unlink it."
  (let ((socket *ipc-listener*)
        (path *ipc-path*))
    (setf *ipc-listener* nil *ipc-path* nil)
    (when socket
      (ignore-errors (sb-bsd-sockets:socket-close socket)))
    (when path (ignore-errors (delete-file path)))
    (let ((thread *ipc-thread*))
      (setf *ipc-thread* nil)
      (when (and thread (bt:thread-alive-p thread))
        (ignore-errors (bt:destroy-thread thread)))))
  nil)

;;; ------------------------------------------------------------ the client

(defun restore-newlines (text)
  "The inverse of ONE-LINE: literal backslash-n becomes a newline again.

The wire carries one line per answer so that the framing is honest; a person
reading a paragraph of diagnosis on a terminal wants it as a paragraph.  Two
functions, four lines each, and the protocol stays a protocol."
  (with-output-to-string (out)
    (let ((i 0)
          (n (length text)))
      (loop while (< i n)
            do (cond ((and (char= #\\ (char text i)) (< (1+ i) n)
                           (char= #\n (char text (1+ i))))
                      (terpri out)
                      (incf i 2))
                     ((and (char= #\\ (char text i)) (< (1+ i) n)
                           (char= #\\ (char text (1+ i))))
                      (write-char #\\ out)
                      (incf i 2))
                     (t (write-char (char text i) out)
                        (incf i)))))))

(defun answer-ok-p (answer)
  "True when ANSWER is an (:OK …) reply from the control socket.

Matched on the prefix rather than READ, and that is deliberate.  The value in
an (:OK …) reply is whatever the form returned, printed — it can be a
structure with no readable representation, it can carry #<…>, and READing it
would fail on exactly the successful cases.  The status is the first token and
the first token is all this needs.

NIL for a NIL answer, which is the connection closing mid-exchange: EVALUATE-
FOR-IPC always writes a line, so silence is a failure whatever caused it."
  (and (stringp answer)
       (let ((text (string-left-trim '(#\Space #\Tab) answer)))
         (or (string= text "(:ok)")
             (and (> (length text) 4) (string= "(:ok " (subseq text 0 5)))))))

(defun ipc-evaluate (forms &key (path (ipc-socket-path)) (stream *standard-output*))
  "Send FORMS to a running LatticeWM and print what comes back.

This is the other half of `latticewm --eval', and it runs in a *second*
process: the binary is both the window manager and the thing that talks to it,
which means scripting it needs nothing installed that running it did not
already need.

Returns T only when every form was evaluated *successfully*.

IT USED TO RETURN T WHENEVER THE SOCKET WAS REACHABLE.  The wire protocol is
(:OK …) or (:ERROR …), this printed the distinction and then discarded it, and
MAIN turns the return value into the exit status — so `latticewm --eval
'(car 5)'` printed an error and exited 0.  That is against main.lisp's own
contract, which says every branch that is not `run the window manager' exits
with a status because these are things a script may check, and --eval is the
flag most likely to be in a script.

A form after a failing one is still sent.  The forms are independent lines on
one connection, `--eval A --eval B' is documented as one command rather than
two invocations racing each other, and stopping halfway would leave the
desktop in a state neither A nor A-then-B describes.  The status is about the
run, not about the first casualty."
  (unless path
    (format stream "~&no control socket: *ipc-socket* is nil~%")
    (return-from ipc-evaluate nil))
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
            (ok t))
        (sb-bsd-sockets:socket-connect socket (namestring path))
        (let ((connection (sb-bsd-sockets:socket-make-stream
                           socket :input t :output t
                                  :element-type 'character
                                  :external-format :utf-8)))
          (unwind-protect
               (dolist (form forms ok)
                 (write-line (if (stringp form) form (prin1-to-string form))
                             connection)
                 (force-output connection)
                 (let ((answer (read-line connection nil nil)))
                   (unless (answer-ok-p answer) (setf ok nil))
                   (format stream "~&~a~%"
                           (if answer
                               (restore-newlines answer)
                               "(:error \"no answer\")"))))
            (ignore-errors (close connection))
            (ignore-errors (sb-bsd-sockets:socket-close socket)))))
    (error (condition)
      (format stream "~&could not reach LatticeWM at ~a:~%  ~a~%~%~
                      Is it running?  The socket is named after WAYLAND_DISPLAY,~%~
                      which is ~s in this shell.~%"
              path condition (or (uiop:getenv "WAYLAND_DISPLAY") "unset"))
      nil)))
