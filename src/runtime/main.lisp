;;;; runtime/main.lisp --- Startup, the dispatch loop, and the command line.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------ the dispatch loop
;;;
;;; wayflan's %find-proxy! signals WL-MESSAGE-ERROR when a message arrives for
;;; an object id it does not know.  libwayland has zombie-proxy machinery to
;;; swallow events that were in flight when the client destroyed something;
;;; wayflan has the marker class but no guard.  In a window manager, windows
;;; are created and destroyed constantly, so this *will* be hit.
;;;
;;; The fix is a handler here rather than a fork, and it is safe for a specific
;;; mechanical reason: wayflan's %call-with-message reads the header and then
;;; drains the whole message body into a separate buffer *before* invoking the
;;; handler.  So when the handler throws, the socket stream is already
;;; positioned at the start of the next message and no desynchronisation is
;;; possible.  DESIGN calls this a five-line fix from a source read; it is
;;; five lines because of that property and not otherwise.

(defun dispatch-events ()
  "Read and dispatch whatever the compositor has sent.  Returns NIL on hangup."
  (handler-case
      (progn
        (loop while (wl:wl-display-listen (server-display *server*))
              do (dispatch-one-event (server-display *server*)))
        t)
    (end-of-file () (logmsg :info "compositor closed the connection") nil)
    (wl:wl-server-error (condition)
      (logmsg :error "protocol error: ~a" condition)
      nil)
    #+sbcl
    (sb-int:simple-stream-error (condition)
      (logmsg :info "connection lost: ~a" condition)
      nil)))

(defun display-fd (display)
  "The file descriptor behind DISPLAY's socket, or NIL.

Reaches into wayflan for it, which is the one place we do.  The alternative is
a busy loop, and the reason is worth stating: WL-DISPLAY-LISTEN is a
*non-blocking* poll — it answers 'is a message available' and returns
immediately either way — so a loop built on it alone spins a core at 100%.
That is not a performance nicety in a window manager: it is a laptop that runs
hot and flat while apparently sitting idle.

If a future wayflan renames these internals this returns NIL, and WAIT-FOR-WORK
falls back to sleeping, which is correct and merely less responsive."
  (ignore-errors
   (let ((socket (funcall (or (find-symbol "%WL-DISPLAY-SOCKET"
                                           "XYZ.SHUNTER.WAYFLAN.CLIENT")
                              (return-from display-fd nil))
                          display)))
     (slot-value socket (find-symbol "%FD" "XYZ.SHUNTER.WAYFLAN.WIRE")))))

(defvar *poll-interval* 30
  "Seconds to wait before waking up with nothing to do.

Should never actually elapse: the loop wakes on compositor input and on the
wakeup interrupt below.  It exists only so that a bug in either cannot wedge
the window manager permanently, and 30 seconds is long enough that the cost of
it existing is nil — two wakeups a minute.")

(defvar *wakeup-pending* nil
  "True between asking the loop to wake and it waking.

*This is not an optimisation.*  SB-THREAD:INTERRUPT-THREAD queues an
interruption, and SBCL refuses to nest more than eight — so a script that
called RELAYOUT thirty times in a row killed the window manager outright with
\"maximum interrupt nesting depth (8) exceeded\".  Any loop from a REPL would
have done it.

One pending wakeup is as good as thirty, because the loop drains its whole
queue when it wakes, so coalescing costs nothing and removes the failure.")

(defvar *wakeup-lock* (bt:make-lock "latticewm-wakeup"))

(defun wake-event-loop ()
  "Wake the event loop now.  Safe to call from any thread.

Interrupts the window manager thread, which is blocked in select; the
interruption sets a flag and returns, and the loop then drains its queue *at
its own safe point* rather than wherever it happened to be.  That distinction
matters — the wayflan client is single-threaded, so running real work inside an
interrupt could land in the middle of marshalling a request.

Two designs were tried before this one.  Polling every 100 ms cost about one
percent of a core forever, which on a laptop is measurable battery spent doing
nothing.  A self-pipe registered with SERVE-ALL-EVENTS spun a *whole* core,
because the compositor socket stays readable between the moment select reports
it and the moment wayflan drains its own buffer, so the multiplexer returned
instantly, over and over.  An interrupt has neither problem: it is delivered
once, exactly when there is something to deliver."
  (let ((thread *wm-thread*)
        (send nil))
    (bt:with-lock-held (*wakeup-lock*)
      (unless *wakeup-pending*
        (setf *wakeup-pending* t send t)))
    (when (and send thread (sb-thread:thread-alive-p thread))
      (ignore-errors
       (sb-thread:interrupt-thread
        thread
        (lambda ()
          ;; THROW, not just a flag.  SB-SYS:WAIT-UNTIL-FD-USABLE restarts
          ;; itself on EINTR, so an interruption that merely sets a variable
          ;; is delivered and then *the wait resumes for its full timeout*.
          ;; The queued work then sat there until the compositor happened to
          ;; say something — which in an idle session can be a very long time.
          ;;
          ;; The symptom was superb: every command worked, the model was
          ;; correct, and the screen was thirty seconds stale.  Zoom in
          ;; particular looked completely broken while being completely right.
          ;;
          ;; IGNORE-ERRORS because the interrupt can land while the loop is
          ;; outside the CATCH, in which case there is nothing to unwind to
          ;; and clearing the flag is the whole of what was needed.
          (ignore-errors (throw 'wake nil))))))
    (unless send
      ;; Somebody has already asked and the loop has not got there yet.  It
      ;; will drain the queue when it does, so this call is complete.
      nil)))

(defun wait-for-work (fd)
  "Block until the compositor speaks, or another thread wakes us.

Zero wakeups while nothing is happening, which is the whole point.  The
timeout is a backstop against a bug in the above, not a polling interval."
  (catch 'wake
    (handler-case
        (if fd
            (sb-sys:wait-until-fd-usable fd :input *poll-interval* nil)
            (sleep 0.05))
      (error () nil)
      (sb-sys:interactive-interrupt () nil)))
  ;; Whatever woke us, the queue is about to be drained, so the next caller
  ;; should send a fresh interrupt.
  (bt:with-lock-held (*wakeup-lock*) (setf *wakeup-pending* nil)))

(defun run-event-loop ()
  "The main loop.  Runs until the connection closes or QUIT is called."
  (setf *wm-thread* sb-thread:*current-thread*)
  (let ((fd (display-fd (server-display *server*))))
    (unless fd
      (logmsg :warn "could not find the display fd; falling back to polling"))
    (loop while (server-running *server*)
          do (with-abandon
               (drain-wm-queue)
               (unless (dispatch-events)
                 (setf (server-running *server*) nil))
               (save-state-if-needed)
               (when (server-running *server*)
                 (wait-for-work fd))))
    (setf *wm-thread* nil)))

;;; ---------------------------------------------------------------- startup

(define-condition cannot-start (error)
  ((summary :initarg :summary :reader cannot-start-summary)
   (detail :initarg :detail :initform nil :reader cannot-start-detail)
   (advice :initarg :advice :initform nil :reader cannot-start-advice))
  (:report
   (lambda (condition stream)
     (format stream "~&LatticeWM cannot start: ~a~@[~%~%~a~]~@[~%~%~a~]~%"
             (cannot-start-summary condition)
             (cannot-start-detail condition)
             (cannot-start-advice condition))))
  (:documentation
   "Signalled when startup fails for a reason the user can act on.

Distinct from every other error in the program because it is the one class of
failure that has an *audience*: somebody is looking at a login screen wondering
why their desktop did not appear.  It carries a one-line summary, the technical
detail, and the sentence that says what to do — and MAIN prints all three to
stderr as well as to the log, because at that moment the log may be the thing
they cannot find."))

(defun connect-to-compositor ()
  "Connect to the Wayland display, or signal CANNOT-START saying why.

WAYLAND_DISPLAY is checked *before* the connection is attempted rather than
after, because wayflan's failure for an unset one is a type error out of
MERGE-PATHNAMES — a condition that names neither Wayland nor the variable, and
that used to travel all the way to the debugger hook.  A window manager
launched from a session file and landing at a debugger prompt holds the session
open on a terminal nobody is reading, which is the worst failure this program
has; the check that makes it a sentence instead is three lines."
  (let ((display-name (uiop:getenv "WAYLAND_DISPLAY"))
        (runtime-dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (when (or (null display-name) (zerop (length display-name)))
      (error 'cannot-start
             :summary "WAYLAND_DISPLAY is not set, so there is no compositor to talk to."
             :detail "LatticeWM is a window manager *for* river: it is an ordinary
Wayland client that river runs, not a compositor of its own.  Running it
directly from a TTY or from a shell outside a session cannot work."
             :advice "Start it the way river expects:

  river -c latticewm

or log in through the LatticeWM session entry, which does that for you."))
    (when (or (null runtime-dir) (zerop (length runtime-dir)))
      (error 'cannot-start
             :summary "XDG_RUNTIME_DIR is not set, so the Wayland socket cannot be found."
             :detail (format nil "WAYLAND_DISPLAY is ~s, but a Wayland socket lives under
XDG_RUNTIME_DIR and that variable is empty." display-name)
             :advice "This is a session-manager misconfiguration.  On a systemd
system XDG_RUNTIME_DIR is normally /run/user/$UID and is set by pam_systemd."))
    (handler-case (wl:wl-display-connect display-name)
      (cannot-start (condition) (error condition))
      (error (condition)
        (error 'cannot-start
               :summary (format nil "could not connect to the Wayland display ~s."
                                display-name)
               :detail (format nil "~a~%~%The socket would be ~a/~a."
                               condition runtime-dir display-name)
               :advice "Is the compositor still running?  If LatticeWM was
launched by a session file it may simply have started before river was
ready.")))))

(defun report-cannot-start (condition)
  "Say why startup failed, to the log and to stderr, in that order.

Both, deliberately.  The log is where a session-launched failure can be read
afterwards; stderr is where somebody running it by hand is already looking.
Printing to only one of them is how a diagnostic written for exactly this
moment ends up invisible at exactly this moment."
  ;; The summary to the log and the whole thing to stderr, rather than both to
  ;; both: with no log file configured the log *is* stderr, and a paragraph
  ;; printed twice reads as the program having failed twice.
  (ignore-errors (logmsg :error "cannot start: ~a" (cannot-start-summary condition)))
  (ignore-errors
   (let ((detail (cannot-start-detail condition)))
     (when detail (logmsg :debug "~a" detail))))
  (ignore-errors
   (format *error-output* "~&~a" condition)
   (let ((path (ignore-errors (p:resolved-log-file))))
     (when path (format *error-output* "~&(this was also written to ~a)~%" path)))
   (finish-output *error-output*))
  nil)

(defun start (&key (swank-port *swank-port*) (config t) (restore t))
  "Connect to river and run.  Returns T on a clean exit and NIL on a failure
to start.  This is the whole program.

Order matters and each step is deliberate:

  1. The debugger hook goes in first, because a daemon with no controlling
     terminal that hits an unhandled condition hangs on a stdin that is not
     there — and a hung window manager freezes the desktop, since river waits
     for our manage sequence before processing input.
  2. The emergency save is registered next, so that a fatal condition from
     any point after this still writes the layout out.
  3. SWANK starts, if it was asked for, before anything interesting exists, so
     that if step 5 fails you still have a REPL inside the running image.
  4. Defaults, then the user's configuration, so the configuration wins.
  5. Connect and bind, refusing to start on a protocol version mismatch, and
     saying why in a sentence rather than a backtrace."
  (install-debugger-hook)
  (p:add-emergency-thunk 'emergency-shutdown)
  (register-data-registry)
  (start-swank swank-port)
  (start-ipc-server)
  (setf *world* (c:make-world)
        p:*policy* (or p:*policy* (make-instance 'p:conventional-policy)))
  (install-default-keymap)
  (when config (load-config))
  (run-hooks :startup)
  (let ((display (handler-case (connect-to-compositor)
                   (cannot-start (condition)
                     (report-cannot-start condition)
                     (return-from start nil)))))
    (setf *server* (make-instance 'server :display display)
          (server-running *server*) t)
    (unwind-protect
         (handler-case
             (progn
               (bind-globals *server*)
               (attach-manager-hooks *server*)
               (safe-roundtrip display)
               (apply-cursor-theme)
               (when restore (load-state))
               (mark-dirty)
               (request-manage)
               (logmsg :info "LatticeWM running on ~a"
                       (or (uiop:getenv "WAYLAND_DISPLAY") "?"))
               ;; After the outputs exist and before the loop starts, so the
               ;; very first frame carries it.  An empty screen with no hint on
               ;; it is the one moment a new user cannot recover from unaided.
               (maybe-show-welcome)
               (run-event-loop)
               t)
           (protocol-version-mismatch (condition)
             (report-cannot-start
              (make-condition 'cannot-start
                              :summary "the compositor speaks a different version of the protocol."
                              :detail (princ-to-string condition)))
             nil)
           (cannot-start (condition)
             (report-cannot-start condition)
             nil)
           (error (condition)
             (report-cannot-start
              (make-condition 'cannot-start
                              :summary "an error during startup."
                              :detail (princ-to-string condition)
                              :advice "Run with --log-level debug for the whole
sequence, and see the backtrace in the log."))
             (p:log-backtrace 40)
             nil))
      (guarded "shutdown" (run-shutdown-once))
      (guarded "ipc shutdown" (stop-ipc-server))
      (ignore-errors (wl:wl-display-disconnect display))
      (setf *server* nil)
      (logmsg :info "LatticeWM stopped")
      (ignore-errors (p:close-log-file)))))

(defun emergency-shutdown ()
  "Save whatever can be saved, from the path where the program has lost.

Registered with P:ADD-EMERGENCY-THUNK, so the debugger hook runs it just before
SB-EXT:EXIT.  Everything here has to survive being called at an arbitrary
moment with arbitrary state, which is why it is three guarded calls and not the
ordinary shutdown."
  (ignore-errors (save-state))
  (ignore-errors (run-hooks :shutdown))
  nil)

(defun apply-cursor-theme ()
  "Set the xcursor theme, if the configuration asked for one."
  (when *cursor-theme*
    (dolist (seat (server-seats *server*))
      (guarded "set_xcursor_theme"
        (w:seat-set-xcursor-theme (seat-proxy seat)
                                  *cursor-theme* *cursor-size*)))))

;;; ------------------------------------------------------------ the CLI

(defun print-options ()
  "Print every tier-0 option with its value, default and documentation."
  (format t "~&LatticeWM options.  Set any of these in ~a~2%" (config-file))
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore key))
      (format t "~a~%  value:   ~s~%  default: ~s~%~{  ~a~%~}~%"
              variable value default
              (split-lines documentation)))))

(defun print-commands ()
  "Print every command with its arguments and documentation."
  (format t "~&LatticeWM commands.~2%")
  (dolist (command (all-commands))
    (format t "(~a~{ ~(~a~)~})~%~{  ~a~%~}~%"
            (command-name command) (command-lambda-list command)
            (split-lines (or (command-documentation command)
                             "UNDOCUMENTED <-- flag me")))))

(defun print-keymap (&optional (keymap *keymap*) (prefix ""))
  "Print every binding."
  (when (string= prefix "") (format t "~&LatticeWM keymap.~2%"))
  (dolist (entry (keymap-keys keymap))
    (destructuring-bind (key . target) entry
      (if (typep target 'keymap)
          (print-keymap target (format nil "~a~a " prefix (key-to-string key)))
          (format t "~a~a~30t~s~%" prefix (key-to-string key) target)))))

(defparameter +usage+
  "usage: latticewm [options]

  --help                print this and exit
  --version             print the version and exit

  --list-options        every configuration value, with its default and why
  --list-commands       every command
  --list-keys           every key binding
  --extension-surface   every generic you can specialize
  --write-config        write a starter init.lisp
  --check-config        load the configuration, report problems, and exit

  --eval FORM           evaluate FORM in an already-running LatticeWM
                        (repeatable; talks to the control socket)

  --no-config           ignore the configuration file
  --no-restore          ignore the saved layout
  --no-ipc              do not open the control socket
  --swank-port N        start a SWANK REPL on this port (off by default)
  --log-level LEVEL     debug, info, warn, error, or none
  --log-file PATH       write the log there ('-' for stderr only)

LatticeWM is a window manager for the river Wayland compositor.  It is an
ordinary Wayland client that river runs, not a compositor of its own, so it
must be started inside river:

  river -c latticewm

or picked from the login screen, which does that for you.
"
  "The whole command line, in one string.

One string rather than a FORMAT with directives in it, because the help text is
the first thing a person reads and the second thing anybody edits, and a
paragraph broken across twenty ~% directives is a paragraph nobody edits
correctly.")

(defun check-config (&optional (path (config-file)))
  "Load the configuration file and report what is wrong with it.  Returns T if
it is clean.

*A CONFIGURATION ERROR USED TO MANIFEST AS A FAILED SESSION*, which is the
worst possible way to find out about one: you are at a login screen, the
desktop did not appear, and the only tool for diagnosing it is the desktop.
This is the answer — run it from a terminal in whatever session you have, and
find out before you log out.

It deliberately does *not* connect to a compositor.  Almost everything a
configuration file does — setting options, defining methods, binding keys,
defining commands — needs no compositor at all, so a check that required one
would be useless in exactly the situation it is for.

*It does everything else START does first*, though, and that is not
decoration.  A configuration file runs after the world exists and after the
default keymap is installed, so a check that skipped those would report
failures a real startup would never have — which is worse than not checking,
because it teaches people to ignore it.  The shipped starter configuration
found this immediately: (lattice:enable) needs a world, and there was none."
  (setf *world* (c:make-world)
        p:*policy* (or p:*policy* (make-instance 'p:conventional-policy)))
  (install-default-keymap)
  (format t "~&checking ~a~%~%" path)
  (cond
    ((not (probe-file path))
     (format t "There is no configuration file, which is fine: LatticeWM runs~%~
                on its defaults.  `latticewm --write-config' writes a~%~
                commented starter one.~%")
     t)
    (t
     (let ((problems '()))
       (handler-bind ((warning (lambda (condition)
                                 (push (format nil "warning: ~a" condition)
                                       problems)
                                 (muffle-warning condition))))
         (handler-case
             (let ((*package* (find-package '#:latticewm/user)))
               (register-data-registry)
               (load path))
           (error (condition)
             (push (format nil "error: ~a" condition) problems))))
       (cond
         ((null problems)
          (format t "No problems.~%~%~
                     ~d option~:p, ~d command~:p, ~d key binding~:p.~%"
                  (length (p:all-options)) (length (all-commands))
                  (length (all-bound-keys)))
          t)
         (t
          (format t "~{~a~%~}~%~d problem~:p.  ~
                     LatticeWM would still start — a bad configuration is~%~
                     logged and skipped rather than fatal — but everything~%~
                     after the failing form would not have run.~%"
                  (reverse problems) (length problems))
          nil))))))

(defun main ()
  "Entry point for the dumped image.

Every branch that is not `run the window manager' exits with a status, because
these are things a script may check: --check-config in particular is only
useful if a non-zero exit means what it says."
  (let ((arguments (rest sb-ext:*posix-argv*)))
    (flet ((flag (name) (member name arguments :test #'string=)))
      ;; Applied before anything else, so that a diagnostic from any branch
      ;; below lands where the user asked for it.
      (let ((level (argument-value arguments "--log-level"))
            (file (argument-value arguments "--log-file")))
        (when level
          (setf *log-level* (if (string= level "none")
                                nil
                                (intern (string-upcase level) :keyword))))
        (when file
          (setf p:*log-file* (if (string= file "-") nil (pathname file)))))
      (when (flag "--no-ipc") (setf *ipc-socket* nil))
      (cond
        ((flag "--help") (write-string +usage+) (sb-ext:exit :code 0))
        ((flag "--version")
         (format t "~&LatticeWM ~a (river_window_manager_v1 v~d, ~
                    river_xkb_bindings_v1 v~d)~%"
                 (or (ignore-errors
                      (asdf:component-version (asdf:find-system "latticewm")))
                     "0.1.0")
                 +window-management-version+ +xkb-bindings-version+)
         (sb-ext:exit :code 0))
        ((flag "--eval")
         (let ((forms (argument-values arguments "--eval")))
           (sb-ext:exit :code (if (ipc-evaluate forms) 0 1))))
        ((flag "--check-config")
         (sb-ext:exit :code (if (check-config) 0 1)))
        ((flag "--write-config")
         (install-default-keymap)
         (let ((path (write-sample-config)))
           (format t "~&~:[~a already exists; left alone~;wrote ~a~]~%"
                   path (config-file))
           (sb-ext:exit :code 0)))
        ((flag "--list-options") (print-options) (sb-ext:exit :code 0))
        ((flag "--list-commands") (print-commands) (sb-ext:exit :code 0))
        ((flag "--list-keys")
         (install-default-keymap) (print-keymap) (sb-ext:exit :code 0))
        ((flag "--extension-surface")
         (p:print-extension-surface) (sb-ext:exit :code 0))
        (t
         (let* ((port (argument-value arguments "--swank-port"))
                (started (start :swank-port (cond ((null port) *swank-port*)
                                                  ((string= port "0") nil)
                                                  (t (parse-integer
                                                      port :junk-allowed t)))
                                :config (not (flag "--no-config"))
                                :restore (not (flag "--no-restore")))))
           ;; A failure to start is a non-zero exit, so a session manager that
           ;; watches the status has something true to look at.
           (sb-ext:exit :code (if started 0 1))))))))

(defun argument-value (arguments name)
  "The value following NAME in ARGUMENTS, or NIL."
  (let ((position (position name arguments :test #'string=)))
    (when (and position (< (1+ position) (length arguments)))
      (nth (1+ position) arguments))))

(defun argument-values (arguments name)
  "Every value following an occurrence of NAME, in order.

So --eval can be given more than once and the forms run in the order written,
which is what makes `--eval (setf *gaps* 8) --eval (relayout :force t)' one
command rather than two invocations racing each other."
  (loop for (flag value) on arguments
        when (and (string= flag name) value) collect value))
