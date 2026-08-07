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
  "Read and dispatch whatever the compositor has sent.  Returns NIL on hangup.

*THIS CANNOT SEE THE COMMONEST HANGUP THERE IS*, which is why
CONNECTION-HUNG-UP-P exists and is asked directly after every call to this.
The three conditions below are the ones where a hangup arrives in the middle of
something: a message half read, a protocol error, a broken stream.  A
compositor that simply goes away between messages produces none of them — see
the next function for what it produces instead, which is nothing at all."
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

;;; ------------------------------------------------- the hangup nothing reports
;;;
;;; KILL THE COMPOSITOR AND THIS PROGRAM USED TO SPIN A CORE FOREVER.  Six of
;;; them were found on one developer's machine at 95% CPU each, load average
;;; 7.7, their river long dead, and not one of them had written a line to its
;;; log since the day it was orphaned.
;;;
;;; The mechanism is exact and it is nobody's bug in particular.  When the peer
;;; closes a stream socket the fd becomes *readable* — a read would not block,
;;; it would return zero — so SB-SYS:WAIT-UNTIL-FD-USABLE returns immediately,
;;; every time, for ever.  wayflan's WL-DISPLAY-LISTEN then asks whether a
;;; message is available: its %READ-ONCE calls recvmsg, gets 0 for end of file
;;; and 0 for `nothing yet, try later', and cannot tell the caller which,
;;; because both are the same integer.  So DISPATCH-EVENTS above reads nothing,
;;; signals nothing, and answers T.  Wait, read nothing, answer T, wait: a busy
;;; loop with no error in it, which is the one shape that leaves no trace.
;;;
;;; That is the exact failure DISPLAY-FD's own docstring says it exists to
;;; prevent — "a laptop that runs hot and flat while apparently sitting idle" —
;;; reached by a road its fix did not cover, because it fixed the *idle* case
;;; and this is the *dead* one.
;;;
;;; poll(2) knows.  POLLHUP is set in the answer whether or not it was asked
;;; for, and it is the direct statement of the thing select cannot say: this
;;; descriptor is readable because the other end is gone.  One syscall per
;;; wakeup, and the alternative — inferring the hangup from "we woke up and
;;; there was nothing there" — is an inference where an answer is available.

(sb-alien:define-alien-type nil
  ;; struct pollfd, which is fixed by the kernel ABI: int, short, short.
  (sb-alien:struct pollfd
    (fd sb-alien:int)
    (events sb-alien:short)
    (revents sb-alien:short)))

(defconstant +pollin+ #x001 "There is something to read.")
(defconstant +pollerr+ #x008 "An error on the descriptor.")
(defconstant +pollhup+ #x010 "The other end hung up.")
(defconstant +pollnval+ #x020 "The descriptor is not open.")

(defun connection-hung-up-p (fd)
  "Has the other end of FD gone away?

Asked after the events have been dispatched, so that a socket carrying both a
last message and a hangup — which is what a compositor exiting cleanly looks
like — is drained before it is declared dead.

NIL when FD is NIL, because DISPLAY-FD could not find the descriptor and the
loop is in its degraded sleeping mode, where a spin is not possible anyway."
  (when fd
    (ignore-errors
     (sb-alien:with-alien ((fds (sb-alien:struct pollfd)))
       (setf (sb-alien:slot fds 'fd) fd
             (sb-alien:slot fds 'events) +pollin+
             (sb-alien:slot fds 'revents) 0)
       (let ((ready (sb-unix:unix-poll (sb-alien:addr fds) 1 0)))
         (and (integerp ready) (plusp ready)
              (logtest (sb-alien:slot fds 'revents)
                       (logior +pollhup+ +pollerr+ +pollnval+))
              t))))))

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

(defun consume-wakeup ()
  "Take the pending wakeup if there is one, and say whether there was.

Read *and* clear under one lock, because the two halves are the whole point:
finding the flag set means the interrupt that goes with it may never arrive,
and leaving it set means the next caller does not send one."
  (bt:with-lock-held (*wakeup-lock*)
    (prog1 *wakeup-pending* (setf *wakeup-pending* nil))))

(defun note-lost-wakeup (started)
  "Say so, once, if this wait ran its whole length with work already queued.

THE INSTRUMENT FOR A DEFECT THAT LEAVES NO TRACE.  A lost wakeup is not an
error: the loop waits its full interval, wakes on the timeout, drains the queue
and carries on, and what the user sees is a screen that was stale for thirty
seconds and is now correct.  Nothing signals, nothing logs, and by the time
anybody looks the evidence is a working window manager.

This is that evidence.  Waiting the whole interval is normal on an idle
desktop; waiting the whole interval with something already *in* the queue is
not, because the thing that put it there was supposed to have woken us.  The
two together are the signature, they cost one comparison per wakeup, and they
are the difference between the next report saying `it froze for about a minute'
and saying which mechanism froze.

FINDINGS.org records a 67-second stall that did not reproduce in a dozen
attempts and was written down as `not known to be the same thing'.  Two poll
intervals is 60 seconds.  If it happens again, this says so."
  (let ((elapsed (/ (float (- (get-internal-real-time) started))
                    internal-time-units-per-second)))
    (when (and (>= elapsed *poll-interval*)
               (bt:with-lock-held (*wm-thread-lock*) *wm-thread-queue*))
      (note-once :lost-wakeup
                 "the event loop waited ~,1f seconds with work already queued, ~
                  which means a wakeup was lost.  The screen was stale for that ~
                  long.  Please report this with the log."
                 elapsed))))

(defun wait-for-work (fd)
  "Block until the compositor speaks, or another thread wakes us.

Zero wakeups while nothing is happening, which is the whole point.  The
timeout is a backstop against a bug in the above, not a polling interval.

*A WAKEUP THAT IS ALREADY PENDING MEANS DO NOT WAIT AT ALL*, and that is the
half of this that was missing.  WAKE-EVENT-LOOP sets the flag and then throws
into the CATCH below; if the interrupt lands while the loop is *outside* the
catch — anywhere in the four calls above this one, which is most of the time
under load — the throw has no tag to reach, IGNORE-ERRORS swallows it, and the
flag is left set.  The loop then walked straight into this wait and blocked for
the full poll interval with the other thread's work sitting in the queue.

Thirty seconds of a correct model and a stale screen, exactly the symptom
WAKE-EVENT-LOOP's own docstring describes and was written to fix, reached by
the one path its fix did not cover.  It is rare — it needs the interrupt to
land inside a window a few microseconds wide — and it is not rare enough: the
integration suite hit it about one run in five, driving the window manager from
another thread the way a script or a REPL does.

So the flag is *consumed* here rather than merely cleared.  Finding it set costs
one extra turn of the loop, which drains the queue and comes back.

*AND IT IS CONSUMED TWICE, WHICH CLOSES THE LAST OF THE WINDOW.*  Consuming it
once above still leaves a gap: between that read and the CATCH being
established there are a few instructions, and an interrupt delivered in them
throws at a tag that does not exist yet, is swallowed, and leaves a flag the
first read has already gone past.  The second consume is *inside* the catch, so
every arrival is covered — before the catch, the flag is still set and this sees
it; after it, the throw has somewhere to land.  There is no third case.

That gap is microseconds wide and this program has already been bitten twice by
gaps that size, both times for thirty seconds a go.  See NOTE-LOST-WAKEUP for
what says so if there is ever a fourth."
  (unless (consume-wakeup)
    (let ((started (get-internal-real-time)))
      (catch 'wake
        ;; The second consume.  See above: this is the arrival that lands
        ;; before the catch exists, which the first one cannot see.
        (unless (consume-wakeup)
          (handler-case
              (if fd
                  (sb-sys:wait-until-fd-usable fd :input *poll-interval* nil)
                  (sleep 0.05))
            (error () nil)
            (sb-sys:interactive-interrupt () nil))))
      ;; Whatever woke us, the queue is about to be drained, so the next caller
      ;; should send a fresh interrupt.
      (consume-wakeup)
      (note-lost-wakeup started))))

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
               ;; The other way a session ends, and the one nothing signalled.
               ;; See CONNECTION-HUNG-UP-P: without this the loop spins a core
               ;; for as long as the machine is up.
               (when (and (server-running *server*)
                          (connection-hung-up-p fd))
                 (logmsg :info "compositor closed the connection")
                 (setf (server-running *server*) nil))
               ;; After the whole batch of events, not inside any of them: a
               ;; device announces itself with twenty-odd events and
               ;; configuring it after the first would mean deciding what a
               ;; touchpad supports before it had said.  Costs one boolean test
               ;; when nothing changed, which is every pass but a handful.
               (apply-input-if-needed)
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

(defun start (&key (swank-port :default) (ipc :default) (config t) (restore t))
  "Connect to river and run.  Returns T on a clean exit and NIL on a failure
to start.  This is the whole program.

Order matters and each step is deliberate:

  1. The debugger hook goes in first, because a daemon with no controlling
     terminal that hits an unhandled condition hangs on a stdin that is not
     there — and a hung window manager freezes the desktop, since river waits
     for our manage sequence before processing input.
  2. The emergency save is registered next, so that a fatal condition from
     any point after this still writes the layout out.
  3. Defaults, then the user's configuration, so the configuration wins.
  4. *Then* the two servers, because both are configurable and both used to
     start before the file that configures them.  See below.
  5. Connect and bind, refusing to start on a protocol version mismatch, and
     saying why in a sentence rather than a backtrace.

THE SERVERS COME AFTER THE CONFIGURATION AND THAT IS A FIX, not a preference.
*IPC-SOCKET*, *IPC-TIMEOUT-SECONDS* and *SWANK-PORT* are registered options,
printed by `--list-options', documented as things to set in init.lisp — and
they were read before init.lisp was loaded.  So the socket was already open at
its default path when the line that turned it off ran, and `(setf *swank-port*
4005)' in a configuration file did nothing at all, silently, for the life of
the program.  An option the configuration cannot change is not an option; it is
a hard-coded value with a docstring.

The reason SWANK used to be first was to have a REPL if startup failed.  That
still holds — LOAD-CONFIG catches and logs, so a bad configuration file cannot
prevent this line being reached, and everything that can fail without a REPL to
debug it (connecting, binding, the first manage sequence) is still after it."
  (install-debugger-hook)
  (p:add-emergency-thunk 'emergency-shutdown)
  (register-data-registry)
  (setf *world* (c:make-world)
        p:*policy* (or p:*policy* (make-instance 'p:conventional-policy)))
  (install-default-keymap)
  (when config (load-config))
  ;; :DEFAULT rather than the option's value as a default argument, because a
  ;; default argument is evaluated by the *caller* — so reading *SWANK-PORT* in
  ;; the lambda list would have read it in MAIN, before this function had
  ;; loaded the file that sets it, which is the same bug one level up.  A
  ;; command-line flag still wins, because it arrives as a real value.
  (start-swank (if (eq swank-port :default) *swank-port* swank-port))
  (unless (eq ipc :default) (setf *ipc-socket* ipc))
  (start-ipc-server)
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
               ;; HERE, AND NOT BEFORE THE CONNECT.  A startup hook exists to
               ;; do something with the session, and above the LET there is no
               ;; session: no display, no outputs, no seats, no windows.  The
               ;; hook's own docstring said "after the compositor connection is
               ;; up" and had said so, wrongly, since it was written — which is
               ;; what a hook nothing has ever attached to looks like.
               ;;
               ;; Inside the UNWIND-PROTECT is the other half.  Above it, a
               ;; compositor that refused us ran :STARTUP and then returned
               ;; without ever running :SHUTDOWN, so anything a hook acquired
               ;; leaked on precisely the path where nothing else was going to
               ;; clean up either.  The pairing is structural now.
               (run-hooks :startup)
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
  "Print every tier-0 option with its default, its value if that differs, and
its documentation.

TWO PER-IMAGE MEASUREMENTS USED TO GO INTO A COMMITTED FILE, and doc/OPTIONS.txt
is the file this corpus calls the un-driftable reference.

The first was the path: this printed (CONFIG-FILE), which is expanded because a
program about to open a file needs an absolute path — so line 1 of the shipped
document read \"Set any of these in /home/shaul/.config/latticewm/init.lisp\".
CONFIG-FILE-NAME is the saying-it form and carries the argument.

The second was `value:'.  It is exactly right for a live image, where the whole
question is what your configuration file did; it is meaningless in a file
generated by a build tool that loaded no configuration, where it can only ever
repeat `default:' — and it made the document unreproducible for anything whose
default is computed from the environment.  So it prints only when it differs,
which is never in the generating image and always exactly when it matters at a
REPL.  Same treatment as `attached:' in the hook surface, for the same reason."
  (format t "~&LatticeWM options.  Set any of these in ~a~%~
             ($XDG_CONFIG_HOME is honoured if it is set.)~2%"
          (config-file-name))
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore key))
      ;; Three FORMATs rather than one with a ~@[ in it, because ~@[ tests the
      ;; argument and an option whose value is NIL against a non-NIL default
      ;; both differs and is false -- which is the one case this exists for.
      (format t "~a~%  default: ~s~%" variable default)
      (unless (equal value default)
        (format t "  value:   ~s~%" value))
      (format t "~{  ~a~%~}~%" (split-lines documentation)))))

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
  --extension-surface   every policy generic you can specialize
  --container-surface   every generic a new container kind must answer
  --hooks               every hook, what it is called with, and when
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

(defparameter +flags+
  '(("--help" . 0) ("--version" . 0)
    ("--list-options" . 0) ("--list-commands" . 0) ("--list-keys" . 0)
    ("--extension-surface" . 0) ("--container-surface" . 0) ("--hooks" . 0)
    ("--write-config" . 0) ("--check-config" . 0)
    ("--no-config" . 0) ("--no-restore" . 0) ("--no-ipc" . 0)
    ("--eval" . 1) ("--swank-port" . 1) ("--log-level" . 1) ("--log-file" . 1))
  "Every accepted option, and how many values each takes.

Exists so that an unrecognised one can be *refused*.  It used to be ignored:
`latticewm --loglevel debug' started the window manager on its defaults and
said nothing, and so did `--swank-por 4005', and so did every typo anybody ever
made in a session file — where the consequence is a desktop that comes up
subtly not as configured, with no line anywhere saying why.  A program whose
command line silently accepts anything has no command line.")

(defun unknown-arguments (arguments)
  "Every argument in ARGUMENTS that is not a flag or a flag's value.

Positional arguments count as unknown, because this program has none: every
value belongs to the flag before it."
  (let ((out '())
        (skip 0))
    (dolist (argument arguments (nreverse out))
      (cond ((plusp skip) (decf skip))
            ((assoc argument +flags+ :test #'string=)
             (setf skip (cdr (assoc argument +flags+ :test #'string=))))
            (t (push argument out))))))

(defun suggest-flag (argument)
  "The accepted flag ARGUMENT was most likely meant to be, or NIL.

Three matches, which between them catch the three mistakes people actually
make: stopping typing early, mistyping a letter, and leaving out the hyphen in
a two-word flag.  That last one is the common one — `--loglevel' and
`--logfile' are what fingers produce — and it is the one a prefix or a distance
test both miss.  Anything cleverer would need a distance function nobody would
read."
  (let ((names (mapcar #'car +flags+)))
    (flet ((squashed (text) (remove #\- text)))
      (or (find-if (lambda (name) (and (> (length argument) 3)
                                       (eql 0 (search argument name))))
                   names)
          (find-if (lambda (name) (string-equal (squashed name) (squashed argument)))
                   names)
          (find-if (lambda (name)
                     (and (= (length name) (length argument))
                          (= 1 (loop for a across argument
                                     for b across name
                                     count (char/= a b)))))
                   names)))))

(defmacro printing (&body body)
  "Run BODY, which writes to standard output, and exit 0.

CLOSING THE PIPE IS NOT AN ERROR, and every listing flag used to treat it as
one.  `latticewm --extension-surface | head' printed five lines and then a
sixteen-frame SBCL backtrace, because SBCL turns EPIPE into a condition rather
than letting the default SIGPIPE disposition kill the process the way every
other program on the system does.  So the one command a person runs first to
see whether a flag is worth reading in full ended in what looks like a crash.
It applied to --list-options, --list-commands, --list-keys, --help and both
surfaces: five documents whose whole purpose is to be piped into a pager or a
grep.

Answering 0 rather than SIGPIPE's 141 is deliberate: `head' exiting first is
the reader's decision and not this program's failure, and a non-zero status
there breaks `set -o pipefail' in exactly the scripts most likely to use these
flags.  The output is flushed inside the handler's reach, so a broken pipe
during the *final* flush at exit is caught here too rather than escaping into
the debugger hook."
  `(progn
     (handler-case (progn ,@body (finish-output))
       (#+sbcl sb-int:broken-pipe #-sbcl stream-error () nil))
     (sb-ext:exit :code 0 :abort t)))

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
      (let ((unknown (unknown-arguments arguments)))
        (when unknown
          (format *error-output*
                  "~&latticewm: unrecognised argument~p ~{~s~^, ~}~
                   ~{~%~a~}~2%~a~%"
                  (length unknown) unknown
                  (loop for argument in unknown
                        for suggestion = (suggest-flag argument)
                        when suggestion
                          collect (format nil "  did you mean ~a ?" suggestion))
                  "Run `latticewm --help' for the whole command line.")
          (sb-ext:exit :code 2)))
      (cond
        ((flag "--help") (printing (write-string +usage+)))
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
        ((flag "--list-options") (printing (print-options)))
        ((flag "--list-commands") (printing (print-commands)))
        ((flag "--list-keys")
         (install-default-keymap)
         (printing (print-keymap)))
        ((flag "--extension-surface") (printing (p:print-extension-surface)))
        ;; The other surface.  A policy changes what the window manager
        ;; decides; a container kind changes what it can hold, and until this
        ;; existed the second one had no generated document at all.
        ((flag "--container-surface") (printing (c:print-container-surface)))
        ;; The third.  A generic decides, a hook notices, and this one was the
        ;; extension mechanism whose surface was a hand-typed list in a guide
        ;; -- four names short of the program, with no gate able to tell.
        ((flag "--hooks") (printing (p:print-hook-surface)))
        (t
         (let* ((port (argument-value arguments "--swank-port"))
                (started (start :swank-port (cond ((null port) :default)
                                                  ((string= port "0") nil)
                                                  (t (parse-integer
                                                      port :junk-allowed t)))
                                ;; :DEFAULT means `whatever the configuration
                                ;; says', so the flag wins over init.lisp and
                                ;; init.lisp wins over the shipped default --
                                ;; which is the order of specificity everybody
                                ;; expects and the one the old code had exactly
                                ;; backwards.
                                :ipc (if (flag "--no-ipc") nil :default)
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
