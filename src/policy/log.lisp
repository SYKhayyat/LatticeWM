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
;;;;
;;;; THE HOOK MUST NOT BE ABLE TO SIGNAL.  It is the last line of defence for
;;;; *every* unhandled condition in the program, so as long as it can signal,
;;;; any bug anywhere reaches the debugger instead of the log — and a window
;;;; manager sitting at a `0]' prompt on a terminal nobody is reading, holding
;;;; the session open, is the single worst failure mode available to this class
;;;; of program.  The terminal branch used to be (ABORT), which signals
;;;; SIMPLE-CONTROL-ERROR when no ABORT restart is active, which is exactly the
;;;; situation at startup.  It is now SB-EXT:EXIT, which cannot.

(in-package #:latticewm/policy)

(define-option *log-level* :info
  "How much to say: :DEBUG, :INFO, :WARN, :ERROR, or NIL for silence.

:DEBUG is verbose enough to follow every placement decision and is what to
turn on before reporting anything.")

(defvar *log-stream* *error-output*
  "Where log lines go when no *LOG-FILE* is set.

River's own log captures our stderr, so the default puts window manager
messages and compositor messages in one file, in order, which is what you want
when a placement goes wrong.  It is a DEFVAR rather than an option because it
holds a *stream*: the value a user writes down is *LOG-FILE*, which holds a
path and therefore survives being put in a configuration file.")

(define-option *log-file* :default
  "Where to write the log, as a pathname — or :DEFAULT, or NIL.

  :DEFAULT   $XDG_STATE_HOME/latticewm/latticewm.log
  a pathname that file
  NIL        do not open a file; write to *LOG-STREAM*, which is stderr

THIS DEFAULTS TO A FILE ON PURPOSE, and the reason is the failure it was
written for.  Launched from a .desktop entry at a login screen, stderr goes
somewhere no user can find — so the diagnostics written for exactly that
situation were invisible at exactly the moment they were needed.  A window
manager that fails to start and says why into a void has not said why.

Everything still goes to stderr as well when stderr is a terminal, so running
it by hand from a shell behaves the way you expect.")

(define-option *log-max-bytes* (* 2 1024 1024)
  "Rotate the log file once it passes this many bytes.  NIL never rotates.

A window manager runs for weeks and logs a line per relayout at :DEBUG, so an
unrotated log is a slow disk leak.  Two megabytes is a few days of ordinary
use and a couple of hours of debugging.")

(define-option *log-keep* 3
  "How many rotated log files to keep beside the current one.

They are named latticewm.log.1 and so on, oldest discarded.  Three is enough
to cover the session before last, which is as far back as anybody has ever
looked.")

(defparameter +log-levels+ '(:debug 0 :info 1 :warn 2 :error 3))

;;; --------------------------------------------------------- the log file

(defvar *log-file-stream* nil
  "The open log file, or NIL.  Opened on first use, never by the user.")

(defvar *log-file-path* nil
  "The path *LOG-FILE-STREAM* is open on, so a changed *LOG-FILE* reopens.")

(defvar *log-bytes-written* 0
  "Bytes written to the current log file, for rotation.  Approximate on
purpose: FILE-LENGTH on every line would be a syscall per log line.")

(defvar *log-lock* (bt:make-recursive-lock "latticewm-log")
  "Serialises the whole write / rotate / open / close path.

The design assumes a REPL attached to a running event loop -- two threads --
and every one of those operations mutates shared state: the open stream, the
byte counter, and the files on disk.  Without this, two threads can interleave
a WRITE-STRING with its TERPRI (torn lines), race the INCF that drives
rotation, or have one thread CLOSE the stream another is mid-write on.  It is
recursive because LOG-LINE takes it and then calls ENSURE-LOG-FILE, which takes
it again.")

(defun default-log-file ()
  "$XDG_STATE_HOME/latticewm/latticewm.log — beside the saved layout.

State rather than configuration: it records what happened, not what the user
wants, so it belongs with the layout file and not with init.lisp."
  (merge-pathnames
   "latticewm/latticewm.log"
   (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
       (merge-pathnames ".local/state/" (user-homedir-pathname)))))

(defun resolved-log-file ()
  "The pathname *LOG-FILE* names, or NIL for none."
  (case *log-file*
    ((nil) nil)
    ((:default) (default-log-file))
    (t (pathname *log-file*))))

(defun rotate-log-file (path)
  "Shuffle latticewm.log to .1, .1 to .2, and drop the oldest.

Done by renaming rather than by copying, so a reader holding the old file keeps
reading the old file and nothing is ever half-written."
  (let ((keep (or *log-keep* 0)))
    (flet ((numbered (n)
             (if (zerop n)
                 path
                 (make-pathname :type (format nil "~a.~d"
                                              (or (pathname-type path) "log") n)
                                :defaults path))))
      (when (plusp keep)
        (ignore-errors (delete-file (numbered keep))))
      (loop for n from (1- keep) downto 0
            do (ignore-errors
                (when (probe-file (numbered n))
                  (rename-file (numbered n) (numbered (1+ n))))))
      (when (zerop keep)
        (ignore-errors (delete-file path))))))

(defun close-log-file ()
  "Close the log file, if one is open.  Safe to call at any time."
  (bt:with-recursive-lock-held (*log-lock*)
    (let ((stream *log-file-stream*))
      (setf *log-file-stream* nil *log-file-path* nil *log-bytes-written* 0)
      (when stream (ignore-errors (close stream)))))
  nil)

(defun ensure-log-file ()
  "The open log file stream, or NIL.  Opens and rotates as needed.

Never signals: a log file that cannot be opened must degrade to stderr rather
than take down the program that was trying to explain itself."
  (bt:with-recursive-lock-held (*log-lock*)
   (let ((path (ignore-errors (resolved-log-file))))
    (cond
      ((null path) (when *log-file-stream* (close-log-file)) nil)
      (t
       (unless (and *log-file-stream* (equal path *log-file-path*))
         (close-log-file)
         (ignore-errors
          (ensure-directories-exist path)
          (let* ((existing (ignore-errors
                            (with-open-file (in path :if-does-not-exist nil)
                              (and in (file-length in)))))
                 (rotated (and *log-max-bytes* existing
                               (> existing *log-max-bytes*))))
            (when rotated (rotate-log-file path))
            (setf *log-file-stream*
                  (open path :direction :output :if-exists :append
                             :if-does-not-exist :create
                             :external-format :utf-8)
                  *log-file-path* path
                  ;; After a rotate the file we just opened is empty, so the
                  ;; counter is 0 -- inheriting the old oversized EXISTING here
                  ;; made the second block below fire immediately and rotate a
                  ;; whole generation off the ring on every cold start.
                  *log-bytes-written* (if rotated 0 (or existing 0))))))
       (when (and *log-file-stream* *log-max-bytes*
                  (> *log-bytes-written* *log-max-bytes*))
         (close-log-file)
         (ignore-errors
          (rotate-log-file path)
          (setf *log-file-stream*
                (open path :direction :output :if-exists :append
                           :if-does-not-exist :create
                           :external-format :utf-8)
                *log-file-path* path
                *log-bytes-written* 0)))
       *log-file-stream*)))))

(defun log-timestamp ()
  "The current time as HH:MM:SS, which is the resolution a log of a window
manager is read at."
  (multiple-value-bind (second minute hour) (decode-universal-time
                                             (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defvar *log-to-stderr* :auto
  "Whether to echo the log to stderr as well as to the file.

:AUTO means yes when *LOG-STREAM* is a terminal — so running by hand from a
shell shows you the log and running from a session file does not fill a pipe
nobody is reading.  T and NIL force it.")

(defun log-line (text)
  "Write one already-formatted TEXT to wherever the log goes.  Never signals."
  (ignore-errors
   (bt:with-recursive-lock-held (*log-lock*)
   (let ((file (ensure-log-file)))
     (when file
       (write-string text file)
       (terpri file)
       (force-output file)
       ;; Count *bytes*, not code points: the stream is UTF-8, so a log full of
       ;; non-ASCII window titles would otherwise under-count and rotate late.
       (incf *log-bytes-written*
             (1+ (length (sb-ext:string-to-octets text :external-format :utf-8)))))
     (when (or (null file)
               (eq *log-to-stderr* t)
               (and (eq *log-to-stderr* :auto)
                    (ignore-errors (interactive-stream-p *log-stream*))))
       (format *log-stream* "~&~a~%" text)
       (force-output *log-stream*)))))
  nil)

(defun logmsg (level format &rest arguments)
  "Log at LEVEL if *LOG-LEVEL* admits it.  Never signals."
  (ignore-errors
   (when (and *log-level*
              (>= (getf +log-levels+ level 1) (getf +log-levels+ *log-level* 1)))
     (log-line (format nil "~a [~a] ~?" (log-timestamp)
                       (string-downcase level) format arguments))))
  nil)

(defvar *notes-said* (make-hash-table :test #'eq)
  "The KEYs NOTE-ONCE has already said something about.")

(defun note-once (key format &rest arguments)
  "Say something at :WARN the first time KEY comes up, and never again.

For the diagnostics that live on a path taken sixty times a second.  A relayout
notices things — two outputs showing one workspace, a policy answering
nonsense — that are worth exactly one line in the log and are worth *nothing*
repeated until the log is unreadable, which is the same as not saying it.

KEY is compared with EQ and is normally a keyword naming the condition rather
than the message, so that a message with a monitor's name in it does not
become a different note on each screen."
  (unless (gethash key *notes-said*)
    (setf (gethash key *notes-said*) t)
    (apply #'logmsg :warn format arguments))
  nil)

(defun log-backtrace (&optional (count 30))
  "Put a backtrace in the log.  Never signals."
  (ignore-errors
   (log-line (with-output-to-string (out)
               (sb-debug:print-backtrace :stream out :count count))))
  nil)

;;; ---------------------------------------------------- the three boundaries
;;;
;;; HANDLER-BIND, NOT HANDLER-CASE, AND THAT IS THE WHOLE FIX.
;;;
;;; HANDLER-CASE transfers control to the exit point *before* running its
;;; clause, so by the time anything logs, the stack the error happened on is
;;; gone.  All three of these used it.  The consequence was that the one class
;;; of error a live-editable window manager exists to let you fix -- your own
;;; DEFMETHOD, written at a REPL thirty seconds ago -- was reported as a single
;;; line of ~A: no frames, no restarts, no route to a debugger.
;;;
;;; WITH-ABANDON was worse, because it promised.  Its docstring said "log it
;;; with a backtrace and carry on" and it called LOG-BACKTRACE from a
;;; HANDLER-CASE clause -- post-unwind, so it printed the frames of the
;;; WITH-ABANDON site rather than of the error.  Meanwhile the *only* place in
;;; the program that captured a true backtrace was INSTALL-DEBUGGER-HOOK, which
;;; runs from *INVOKE-DEBUGGER-HOOK* before unwinding; and WITH-ABANDON handled
;;; the error first, so the working path was permanently shadowed by the broken
;;; one.
;;;
;;; A HANDLER-BIND handler runs on the signalling stack.  Log there, then
;;; transfer.  Two lines, and it is the difference between a bug report and a
;;; sentence.
;;;
;;; AND THE NAME WAS DOING TWO JOBS.  GUARDED's docstring said it was used at
;;; every boundary where a policy method is called and "deliberately *not* used
;;; around our own internal calls: swallowing our own bugs would turn them into
;;; silent misbehaviour."  There were 126 call sites.  Roughly half wrapped a
;;; w:/river: wire call, and a dozen wrapped our own functions -- (guarded
;;; "snapshot" ...), (guarded "save-state" ...), (guarded "deferred manage
;;; work" ...), (guarded "ipc shutdown" ...) -- each of them our own code
;;; swallowing our own bugs, producing exactly the silent misbehaviour the
;;; docstring forbids.  Once a boundary marker means anything it means nothing,
;;; and the log printed "window destroy: ..." beside "layout: ..." with nothing
;;; to say which one was somebody's bug.
;;;
;;; So there are two names for two boundaries:
;;;
;;;   GUARDED       user code runs inside ours.  Log it loudly, with frames,
;;;                 and keep going.  The per-decision granularity is the real
;;;                 justification and the docstring never gave it: emit.lisp
;;;                 has ~20 guarded policy calls in one manage sequence, so
;;;                 abandoning the sequence over one bad CLIP-RECT would cost
;;;                 the other nineteen windows their placement.
;;;
;;;   BEST-EFFORT   a wire call whose object river may already have destroyed.
;;;                 Expected, not a defect, and a backtrace is noise.
;;;
;;; Our own internal calls get neither.  Where one of those was load-bearing --
;;; a save that must not take the shutdown with it -- it says so at the site.

(define-option *debug-on-error* nil
  "Let errors reach the debugger instead of being logged and swallowed.

For a REPL that is attached to a running window manager, which is what this
program is for.  With it set, GUARDED, BEST-EFFORT and WITH-ABANDON decline to
handle: the condition keeps going, and if SLIME is connected you get the
debugger, the real stack, and the restarts -- including ABANDON, which is what
you almost always want to pick.

*WITH NOTHING ATTACHED, SETTING THIS WILL EXIT THE WINDOW MANAGER* on the first
error, because the debugger hook installed at startup has nowhere to put a
prompt and says so rather than blocking forever on a stream nobody is reading.
That is the honest behaviour and it is why this is off by default.

There was no such option.  The whole reported surface of an error in user code
was one line of text, in a program whose entire thesis is that you edit it
while it runs.")

(defmacro guarded (context &body body)
  "Run BODY; log any error with a backtrace and return NIL.

The boundary where a *policy method* is called — every point where user code
runs inside ours.  A bad DEFMETHOD written at a live REPL must not abort a
manage sequence, because an abandoned manage sequence is a frozen desktop.  It
should produce a log line, a backtrace naming the method, and a window in the
wrong place, which you then fix at the same REPL.

Per decision, not per sequence, and that is the point: one bad answer costs
that answer and nothing else.

For a wire call, use BEST-EFFORT.

For our own internal calls, use neither — swallowing our own bugs turns them
into silent misbehaviour, which is much harder to find than a backtrace.  The
exceptions are UNWIND-PROTECT cleanup and SAVE-STATE, where a failure must not
take the shutdown with it, and each of those carries the argument at the site
rather than relying on this paragraph.  If you are about to add another, write
the reason where the call is."
  (let ((condition (gensym "CONDITION")))
    `(block guarded
       (handler-bind
           ((error (lambda (,condition)
                     (unless *debug-on-error*
                       ;; On the signalling stack, so these are the frames of
                       ;; the error and not of this macro.
                       (logmsg :error "~a: ~a" ,context ,condition)
                       (log-backtrace 20)
                       (return-from guarded nil)))))
         ,@body))))

(defmacro best-effort (context &body body)
  "Run BODY; log any error in one line and return NIL.

The boundary where *we* call the compositor.  A request against a proxy river
has already torn down is an ordinary outcome — a window closes while a manage
sequence is in flight, an output is unplugged mid-frame — and it is not a
defect of ours, so it gets a line and no backtrace.

The difference from GUARDED is entirely about who is at fault, which is
entirely what a log is read for."
  (let ((condition (gensym "CONDITION")))
    `(block best-effort
       (handler-bind
           ((error (lambda (,condition)
                     (unless *debug-on-error*
                       (logmsg :warn "~a: ~a" ,context ,condition)
                       (return-from best-effort nil)))))
         ,@body))))

(defmacro with-abandon (&body body)
  "Run BODY; on any error, log it with a real backtrace and carry on.

Wrapped around each event handler and each iteration of the event loop, so
that one bad event is abandoned and the loop goes round again.

It *handles* rather than only offering a restart, and that distinction was
learned the hard way: relying on the debugger hook to invoke the restart left
a path where a type error in an event handler printed a debugger banner to a
stderr nobody was reading and stopped the window manager dead.  A restart
nothing invokes is not a safety net.  The hook is still installed, as a second
line for anything that escapes this.

THE BACKTRACE IS REAL NOW.  This called LOG-BACKTRACE from a HANDLER-CASE
clause, which runs after the unwind, so it printed this macro's own frames
under a docstring promising the error's.  A HANDLER-BIND handler runs on the
signalling stack; the ABANDON restart is invoked from inside it, so the
liveness guarantee is unchanged and the frames are the ones that matter."
  (let ((condition (gensym "CONDITION")))
    `(restart-case
         (handler-bind
             ((error (lambda (,condition)
                       (unless *debug-on-error*
                         (logmsg :error "abandoned: ~a" ,condition)
                         (log-backtrace 20)
                         (invoke-restart 'abandon)))))
           ,@body)
       (abandon ()
         :report "Abandon this event and keep the window manager running."
         nil))))

;;; ---------------------------------------------------- the debugger hook

(defvar *emergency-thunks* '()
  "Functions to run, each guarded, before a fatal exit.

The runtime puts its state-saving shutdown on here.  A plain list of thunks
rather than the hook mechanism in policy/hooks.lisp for one reason: this runs
on the path where the program has already lost, so it must depend on as little
as possible — and hooks.lisp is loaded after this file.")

(defun run-emergency-thunks ()
  "Run every emergency thunk, surviving all of them.  Never signals."
  (dolist (thunk (reverse *emergency-thunks*))
    (ignore-errors (funcall thunk)))
  nil)

(defun add-emergency-thunk (thunk)
  "Arrange for THUNK to run before a fatal exit.  Idempotent on symbols."
  (setf *emergency-thunks* (cons thunk (remove thunk *emergency-thunks*)))
  thunk)

(defun install-debugger-hook ()
  "Make an unhandled condition log and abandon the operation rather than hang.

Without this, SBCL tries to enter the debugger on a stream that does not exist
when running as a session daemon, and blocks there forever holding an
unfinished manage sequence.

Every step is wrapped so that the hook itself cannot signal, and the terminal
step is SB-EXT:EXIT rather than ABORT.  ABORT signals when no ABORT restart is
active — which is the case throughout startup — so the previous version of this
function reached the debugger *from inside the debugger hook*, which is the one
thing it existed to prevent."
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (ignore-errors (logmsg :error "unhandled: ~a" condition))
          (log-backtrace 40)
          ;; An ABANDON restart means somebody upstream is prepared to drop
          ;; this event and keep the window manager running, which is always
          ;; the better answer.  INVOKE-RESTART transfers control, so nothing
          ;; below runs when one is found.
          (ignore-errors
           (let ((restart (find-restart 'abandon)))
             (when restart (invoke-restart restart))))
          ;; Nothing to return to.  Save what can be saved, say so, and go.
          (ignore-errors
           (logmsg :error "no ABANDON restart is active; exiting"))
          (run-emergency-thunks)
          (ignore-errors (force-output *log-stream*))
          (ignore-errors (close-log-file))
          (sb-ext:exit :code 1 :abort t)))
  ;; SBCL's *DEBUGGER-HOOK* is consulted by INVOKE-DEBUGGER before the
  ;; implementation-specific one in some paths (a BREAK, notably), so both are
  ;; set.  Missing the second is how a stray BREAK in user configuration still
  ;; reached a prompt nobody could see.
  (setf *debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (funcall sb-ext:*invoke-debugger-hook* condition nil)))
  t)
