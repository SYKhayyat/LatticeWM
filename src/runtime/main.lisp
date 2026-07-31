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
;;; possible.  README calls this a five-line fix from a source read; it is
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

(defun start (&key (swank-port *swank-port*) (config t) (restore t))
  "Connect to river and run.  This is the whole program.

Order matters and each step is deliberate:

  1. The debugger hook goes in first, because a daemon with no controlling
     terminal that hits an unhandled condition hangs on a stdin that is not
     there — and a hung window manager freezes the desktop, since river waits
     for our manage sequence before processing input.
  2. SWANK starts before anything interesting exists, so that if step 4 fails
     you still have a REPL inside the running image to find out why.
  3. Defaults, then the user's configuration, so the configuration wins.
  4. Connect and bind, refusing to start on a protocol version mismatch."
  (install-debugger-hook)
  (start-swank swank-port)
  (setf *world* (c:make-world)
        p:*policy* (or p:*policy* (make-instance 'p:conventional-policy)))
  (install-default-keymap)
  (when config (load-config))
  (run-hooks :startup)
  (let ((display (wl:wl-display-connect)))
    (setf *server* (make-instance 'server :display display)
          (server-running *server*) t)
    (unwind-protect
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
           (run-event-loop))
      (guarded "shutdown"
        (run-hooks :shutdown)
        (save-state))
      (ignore-errors (wl:wl-display-disconnect display))
      (setf *server* nil)
      (logmsg :info "LatticeWM stopped"))))

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

(defun split-lines (string)
  "STRING split on newlines, for indenting a docstring."
  (loop with start = 0
        for position = (position #\Newline string :start start)
        collect (subseq string start position)
        while position
        do (setf start (1+ position))))

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

(defun main ()
  "Entry point for the dumped image."
  (let ((arguments (rest sb-ext:*posix-argv*)))
    (cond
      ((member "--help" arguments :test #'string=)
       (format t "~&usage: latticewm [options]~%~%~
                  ~2t--help                print this and exit~%~
                  ~2t--version             print the version and exit~%~
                  ~2t--list-options        every configuration value~%~
                  ~2t--list-commands       every command~%~
                  ~2t--list-keys           every key binding~%~
                  ~2t--extension-surface   every generic you can specialize~%~
                  ~2t--write-config        write a starter init.lisp~%~
                  ~2t--no-config           ignore the configuration file~%~
                  ~2t--no-restore          ignore the saved layout~%~
                  ~2t--swank-port N        REPL port (default ~d, 0 disables)~%~
                  ~2t--log-level LEVEL     debug, info, warn, error, or none~%~%~
                  LatticeWM is a window manager for the river Wayland~%~
                  compositor.  It must run inside river:~%~%~
                  ~2triver -c latticewm~%"
               *swank-port*))
      ((member "--version" arguments :test #'string=)
       (format t "~&LatticeWM ~a (river_window_manager_v1 v~d)~%"
               (asdf:component-version (asdf:find-system "latticewm"))
               +window-management-version+))
      ((member "--write-config" arguments :test #'string=)
       (install-default-keymap)
       (let ((path (write-sample-config)))
         (format t "~&~:[~a already exists; left alone~;wrote ~a~]~%"
                 path (config-file))))
      ((member "--list-options" arguments :test #'string=)
       (print-options))
      ((member "--list-commands" arguments :test #'string=)
       (print-commands))
      ((member "--list-keys" arguments :test #'string=)
       (install-default-keymap)
       (print-keymap))
      ((member "--extension-surface" arguments :test #'string=)
       (p:print-extension-surface))
      (t
       (let ((level (argument-value arguments "--log-level"))
             (port (argument-value arguments "--swank-port")))
         (when level
           (setf *log-level* (if (string= level "none")
                                 nil
                                 (intern (string-upcase level) :keyword))))
         (start :swank-port (cond ((null port) *swank-port*)
                                  ((string= port "0") nil)
                                  (t (parse-integer port :junk-allowed t)))
                :config (not (member "--no-config" arguments :test #'string=))
                :restore (not (member "--no-restore" arguments :test #'string=))))))))

(defun argument-value (arguments name)
  "The value following NAME in ARGUMENTS, or NIL."
  (let ((position (position name arguments :test #'string=)))
    (when (and position (< (1+ position) (length arguments)))
      (nth (1+ position) arguments))))
