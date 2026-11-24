;;;; runtime/main.lisp --- Startup, the dispatch loop, and the command line.

(in-package #:latticewm/runtime)

(declaim (ftype (function (t) t) load-profile))

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

Should never actually elapse: the loop wakes on compositor input, on the
wakeup interrupt below, and on the nearest timer.  It exists only so that a bug
in any of them cannot wedge the window manager permanently, and 30 seconds is
long enough that the cost of it existing is nil — two wakeups a minute.")

;;; ------------------------------------------------------------------ timers
;;;
;;; THERE WAS NO PERIODIC ANYTHING, AND THE MECHANISM WAS ALREADY BUILT.  Every
;;; redraw in this program is caused by something: a key, a window, a monitor,
;;; a request from a client.  Nothing was caused by time passing.  So the clock
;;; in the shipped status-line example — whose own docstring says the time is
;;; "asked fresh every frame", which is true — showed the time of the last
;;; layout change, because there are no frames.  Sit still for twenty minutes
;;; and it read twenty minutes ago.
;;;
;;; WAIT-FOR-WORK has always taken a timeout and has always been handed the
;;; same constant.  A timer is that number becoming a question: how long until
;;; the next thing that wants to happen?  The loop was already correct; it was
;;; only ever asked one thing.
;;;
;;; THEY ARE NAMED, AND THAT IS THE HALF THAT IS NOT OBVIOUS.  Registering by
;;; name makes re-registering idempotent, so a configuration file that is
;;; loaded twice — which is what SHIFT+SUPER+C does, and what anybody editing
;;; their init file at a REPL does all evening — ends up with one clock rather
;;; than two.  ADD-HOOK's docstring records this project losing an afternoon to
;;; exactly that shape: "the help overlay drew itself twice, once through each
;;; generation of the same function."

(defstruct (timer (:constructor %make-timer (name interval function repeat)))
  "A thing to do later, and possibly again.

DEFSTRUCT rather than DEFCLASS: DESIGN's ruling is about core *state* that a
live image has to be able to grow a slot on — a node, a world — and this is a
runtime record with four fields that nothing outside this file reads."
  name interval function repeat (due 0))

(defvar *timers* (make-hash-table :test #'equal)
  "NAME -> TIMER.  See ADD-TIMER.")

(defun add-timer (name interval function &key (repeat t))
  "Call FUNCTION every INTERVAL seconds, or once after it when REPEAT is NIL.

NAME is any object comparable under EQUAL, and adding a timer under a name
that already has one *replaces* it — so loading your configuration a second
time does not give you two.

FUNCTION is called on the window manager's own thread, between compositor
events, so it may touch the model and call MARK-DIRTY directly and does not
need CALL-IN-WM-THREAD.  It is called inside GUARDED: a timer that signals
gets a log line with a backtrace and keeps its place, because a broken clock
must not take the desktop with it.

    (add-timer :my-clock 60 (lambda () (mark-dirty)))

An INTERVAL of 60 does not mean `on the minute'.  If you want a clock that
changes when the displayed minute does, ask for a shorter interval and let the
emit diff drop the redraws that would show the same thing — which is what the
shipped status line does."
  (check-type interval (real (0)))
  (setf (gethash name *timers*)
        (let ((timer (%make-timer name interval function repeat)))
          (setf (timer-due timer) (+ (get-internal-real-time)
                                     (* interval internal-time-units-per-second)))
          timer))
  (wake-event-loop)
  name)

(defun remove-timer (name)
  "Take the timer called NAME off, if there is one."
  (remhash name *timers*)
  nil)

(defun timer-wait ()
  "Seconds until the nearest timer is due, bounded by *POLL-INTERVAL*.

Never negative and never zero: a zero timeout would turn the wait into a spin,
and something already due is handled by RUN-DUE-TIMERS on the way past rather
than by waiting no time at all."
  (let ((soonest nil)
        (now (get-internal-real-time)))
    (maphash (lambda (name timer)
               (declare (ignore name))
               (let ((seconds (/ (float (- (timer-due timer) now))
                                 internal-time-units-per-second)))
                 (when (or (null soonest) (< seconds soonest))
                   (setf soonest seconds))))
             *timers*)
    (if soonest
        (max 0.001 (min *poll-interval* soonest))
        *poll-interval*)))

(defun run-due-timers ()
  "Run every timer whose moment has arrived.  Returns how many ran.

The due time of a repeating timer is computed from *now* rather than from when
it was last due, which is the choice that matters on a laptop: a machine that
was asleep for an hour comes back and runs each timer once, rather than
discovering it owes sixty ticks and running them all."
  (let ((now (get-internal-real-time))
        (ran 0)
        (expired '())
        (due '()))
    ;; Snapshot the due timers before running any of them.  A timer thunk may
    ;; ADD-TIMER or REMOVE-TIMER -- add-timer advertises re-registration as safe
    ;; "between compositor events," and a timer firing is one -- and adding a
    ;; key rehashes *TIMERS* mid-MAPHASH, which is undefined per CLHS.  Removal
    ;; was already deferred to after the loop; the traversal itself now is too.
    (maphash (lambda (name timer)
               (when (>= now (timer-due timer))
                 (push (cons name timer) due)))
             *timers*)
    (dolist (pair (nreverse due))
      (destructuring-bind (name . timer) pair
        ;; Still the registered timer for this name?  A thunk run earlier in
        ;; this same batch may have removed or replaced it.
        (when (eq timer (gethash name *timers*))
          (incf ran)
          (guarded (format nil "timer ~a" name)
            (funcall (timer-function timer)))
          (if (timer-repeat timer)
              (setf (timer-due timer)
                    (+ (get-internal-real-time)
                       (* (timer-interval timer)
                          internal-time-units-per-second)))
              (push name expired)))))
    (dolist (name expired) (remhash name *timers*))
    ran))

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
              ;; TIMER-WAIT, not *POLL-INTERVAL*: the constant was always the
              ;; answer to "how long until the next thing that wants to
              ;; happen", and until there were timers the answer was always
              ;; the same.  It still bounds the wait, so a timer cannot make
              ;; the backstop longer.
              (if fd
                  (sb-sys:wait-until-fd-usable fd :input (timer-wait) nil)
                  (sleep (min 0.05 (timer-wait))))
            (error () nil)
            (sb-sys:interactive-interrupt () nil))))
      ;; Whatever woke us, the queue is about to be drained, so the next caller
      ;; should send a fresh interrupt.
      (consume-wakeup)
      (note-lost-wakeup started))))

(defun run-event-loop ()
  "The main loop.  Runs until the connection closes or QUIT is called."
  ;; The thread first and the flag second: with the order the other way there
  ;; is an instant in which neither clause of IN-WM-THREAD-P holds and an
  ;; in-thread caller would queue for itself.
  (setf *wm-thread* sb-thread:*current-thread*
        *session-end* nil
        *session-starting* nil)
  (let ((fd (display-fd (server-display *server*))))
    (unless fd
      (logmsg :warn "could not find the display fd; falling back to polling"))
    (loop while (server-running *server*)
          do (with-abandon
               (drain-wm-queue)
               ;; Beside the queue, because they are the same species: work
               ;; the loop owes that did not come from the compositor.  Before
               ;; DISPATCH-EVENTS so that a timer which marks the layout dirty
               ;; has its request folded into this pass rather than the next.
               (run-due-timers)
               (unless (dispatch-events)
                 (setf (server-running *server*) nil
                       *session-end* :disconnected))
               ;; The other way a session ends, and the one nothing signalled.
               ;; See CONNECTION-HUNG-UP-P: without this the loop spins a core
               ;; for as long as the machine is up.
               (when (and (server-running *server*)
                          (connection-hung-up-p fd))
                 (logmsg :info "compositor closed the connection")
                 (setf (server-running *server*) nil
                       *session-end* :disconnected))
               ;; After the whole batch of events, not inside any of them: a
               ;; device announces itself with twenty-odd events and
               ;; configuring it after the first would mean deciding what a
               ;; touchpad supports before it had said.  Costs one boolean test
               ;; when nothing changed, which is every pass but a handful.
               (apply-input-if-needed)
               (save-state-if-needed)
               (when (server-running *server*)
                 (wait-for-work fd))))
    ;; Nothing claimed the end, so the flag went false without a reason: say
    ;; that rather than report it as a quit or a disconnect, either of which
    ;; would send somebody looking for a cause that is not there.
    (unless *session-end*
      (setf *session-end* :stopped))
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
  ;; Before anything connects: from here until the event loop takes the socket,
  ;; no thread owns it and every other thread must queue rather than marshal.
  ;; See IN-WM-THREAD-P (#150).
  (setf *session-starting* t)
  (p:add-emergency-thunk 'emergency-shutdown)
  (register-data-registry)
  (setf *world* (c:make-world)
        p:*policy* (or p:*policy* (make-instance 'p:conventional-policy)))
  (install-default-keymap)
  ;; #15: reads the scratchpad claims and connects them to newly opened
  ;; windows.  Before the configuration, because a config that claims a
  ;; scratchpad at load time is a claim like any other.
  (install-scratchpad-claims)
  (when config (load-config))
  ;; #12's declarative half: after the configuration, because a config file is
  ;; what names the profile, and guarded because a missing profile is a message
  ;; and not a session that does not start -- the same rule LOAD-CONFIG keeps.
  ;; LOAD-PROFILE is declared rather than defined here: profiles.lisp loads
  ;; after this file, and a forward *function* reference is a DECLAIM FTYPE
  ;; while a forward *variable* reference is a gate-1 failure -- which is why
  ;; the option itself lives in config.lisp, above this.
  (when *startup-profile*
    (guarded "startup profile"
      (load-profile *startup-profile*)))
  ;; #11's declarative half: the session that wants the keyboard given to its
  ;; one application from the first second.  After the configuration, because
  ;; the config file is what names it, and declared for the same load-order
  ;; reason LOAD-PROFILE is.
  (when *start-suspended*
    (p:give-keys-away))
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
           (protocol-version-too-old (condition)
             (report-cannot-start
              (make-condition 'cannot-start
                              :summary "this river is older than the protocol version LatticeWM needs."
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
      ;; OUR OWN CODE, GUARDED DELIBERATELY, AND THE REASON IS AT THE SITE
      ;; because GUARDED's docstring is otherwise a rule this breaks.  These
      ;; are UNWIND-PROTECT cleanup forms: a failure in the first must not
      ;; stop the second, and neither must stop the disconnect below, or the
      ;; session is left with an orphaned compositor object and an unlinked
      ;; socket.  Everything here has already been asked to save.
      (guarded "shutdown" (run-shutdown-once))
      (guarded "ipc shutdown" (stop-ipc-server))
      (ignore-errors (wl:wl-display-disconnect display))
      (setf *server* nil
            ;; A START that failed before the loop leaves this set, and every
            ;; later caller would then queue for a thread that never ran.
            *session-starting* nil)
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
      (best-effort "set_xcursor_theme"
        (w:seat-set-xcursor-theme (seat-proxy seat)
                                  *cursor-theme* *cursor-size*)))))
