;;;; tools/integration.lisp --- Drive a real compositor.
;;;;
;;;; THIS IS THE TEST THAT CHANGES THE CATEGORY OF BUG THE PROJECT CAN FIND.
;;;;
;;;; The unit suite is fourteen hundred checks and every one of them
;;;; *constructs* state.  The codebase says so about itself, in
;;;; src/runtime/server.lisp, explaining how a handler for an event that does
;;;; not exist survived for the life of the project:
;;;;
;;;;     The tests pass because they construct state rather than receive it.
;;;;
;;;; That one sentence explains a startup path that reached the debugger, a
;;;; float drawn with the wrong border, a second monitor that was never on, a
;;;; dimension re-proposed on every keystroke and a layout saved only at a
;;;; clean exit.  Every one of them lives in code that can only be exercised by
;;;; assembling globals — so every one was tested, if at all, against an
;;;; arrangement somebody chose rather than one river produced.
;;;;
;;;; So: run river with its headless backend, connect to it as the ordinary
;;;; Wayland client this program is, open windows, drive the real commands, and
;;;; assert on what comes back.  No screen, no graphics card, no second
;;;; machine.
;;;;
;;;; WHY THIS FILE IS SHAPED THE WAY IT IS, which is the second thing it is for.
;;;;
;;;; It used to be one 250-line LET, and it stopped growing at eighteen checks
;;;; against the eight and a half thousand lines of src/runtime/ they were meant
;;;; to cover — because adding the nineteenth meant finding the right nesting
;;;; depth inside somebody else's binding form, and because a single error
;;;; anywhere in it abandoned every check after it.  A harness whose marginal
;;;; check is expensive gets exactly as many checks as its first sitting.
;;;;
;;;; So the marginal check is now cheap and the marginal *section* is nearly
;;;; free.  A section is a name and a body; it catches its own errors, so one
;;;; broken section costs one section; the fixtures — a compositor, a window
;;;; manager, a client, a settled round trip — are functions rather than scope.
;;;; And the file is read in a package that USEs the real ones, so a check is
;;;; written in the vocabulary the program is written in rather than through
;;;; READ-FROM-STRING.
;;;;
;;;; WHAT IT COVERS, and the rule for what belongs here.  If a fact can be
;;;; established by constructing state, it belongs in tests/ where it is
;;;; cheaper.  What belongs here is everything that is only true if a
;;;; compositor agreed: the sequence discipline, the wl_shm path, binding
;;;; objects river actually made, the placements river actually accepted, and
;;;; the difference between "the model changed" and "the screen changed".
;;;;
;;;;   make integration
;;;;   make check                 the same thing, with skips made fatal
;;;;
;;;; Absent river it says so and exits 0, because a machine without a
;;;; compositor is a legitimate place to run the unit suite.  Set
;;;; LATTICEWM_REQUIRE_INTEGRATION=1 — which `make check' does — to make a
;;;; missing dependency a failure instead.  There are two kinds of thing this
;;;; run can decline to check and they are not the same kind, so they are
;;;; counted separately: a *missing dependency* is a tool that should have been
;;;; installed, and it is fatal under that variable; a *structural skip* is
;;;; something a headless backend cannot have — libinput devices, a physical
;;;; scaled display — and it is reported every run and never fatal, because
;;;; making it fatal would only teach people to unset the variable.

(require :asdf)
(require :sb-posix)
(require :sb-bsd-sockets)

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm"))

;;; Read in the program's own vocabulary.  LOAD processes a file form by form,
;;; so the packages this one names exist by the time the reader reaches it.
;;; The alternative — and what this file used to do — is READ-FROM-STRING at
;;; every call site, which costs a quoted string per symbol and gives the
;;; compiler nothing to check.
(defpackage #:latticewm/integration
  (:use #:cl #:latticewm/core #:latticewm/policy #:latticewm/runtime)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:w #:latticewm/wire)))

(in-package #:latticewm/integration)

;;; ================================================================ the harness

(defvar *checks* 0)
(defvar *failures* '() "Failed check descriptions, newest first.")
(defvar *missing* '() "Dependencies that were not installed.  Fatal under strict.")
(defvar *skipped* '() "Things a headless backend cannot have.  Never fatal.")
(defvar *sections* '() "(TITLE . CHECK-COUNT), newest first.")
(defvar *section* "start-up" "The section a check belongs to.")

(defun strict-p ()
  "Is a missing dependency a failure rather than a skip?

An unset variable and an empty one mean the same thing, and so do `0', `no' and
`false' — because this is set from a Makefile, where the difference between
unset and empty is a source of exactly the confusion a test harness must not
have."
  (let ((value (sb-posix:getenv "LATTICEWM_REQUIRE_INTEGRATION")))
    (and value
         (not (member value '("" "0" "no" "false" "off") :test #'string-equal)))))

(defun check (ok format &rest arguments)
  "Record one assertion.  Returns OK, so a check can guard what follows it."
  (incf *checks*)
  (let ((entry (assoc *section* *sections* :test #'equal)))
    (when entry (incf (cdr entry))))
  (if ok
      (format t "  ok    ~?~%" format arguments)
      (progn (push (format nil "[~a] ~?" *section* format arguments) *failures*)
             (format t "  FAIL  ~?~%" format arguments)))
  (force-output)
  ok)

(defun skip (format &rest arguments)
  "Note something a headless backend cannot have.  Reported, never fatal."
  (let ((text (format nil "~?" format arguments)))
    (push (format nil "[~a] ~a" *section* text) *skipped*)
    (format t "  skip  ~a~%" text)
    (force-output)
    nil))

(defun missing (format &rest arguments)
  "Note a dependency that should have been installed.  Fatal under strict."
  (let ((text (format nil "~?" format arguments)))
    (push (format nil "[~a] ~a" *section* text) *missing*)
    (format t "  ~:[skip~;MISS~]  ~a~%" (strict-p) text)
    (force-output)
    nil))

(defun run-section (title thunk)
  "Run one section, and do not let it take the rest of the run with it.

A section that signals is a *failed* section rather than an absent one, and the
sections after it still run.  The old shape had neither property: an error
anywhere unwound the whole driver, and what you got was a backtrace and no
information about the twelve checks that never happened."
  (let ((*section* title))
    (push (cons title 0) *sections*)
    (format t "~&~%---- ~a~%" title)
    (force-output)
    (handler-case (funcall thunk)
      (error (condition)
        (check nil "the section itself signalled: ~a" condition)))))

(defmacro section (title &body body)
  `(run-section ,title (lambda () ,@body)))

(defun poll-until (predicate seconds &optional (step 0.02))
  "Poll PREDICATE until it answers or SECONDS elapse.  Returns its answer.

AN ERROR IN THE PREDICATE IS NOT AN ANSWER OF NO, and this used to treat the
two as the same thing.  Tolerating an error is right — half the predicates here
read state that is being built while they read it, and a transient type error
on the way to the answer is normal.  Swallowing it *silently* is not: a
predicate that is simply wrong then behaves exactly like a condition that never
became true, and what the report says is `waited ten seconds and it did not
happen'.

That cost a day.  The suite's last check asked whether QUIT had ended the
session by reading (SERVER-RUNNING *SERVER*) — and START sets *SERVER* to NIL
on its way out, so on every run where the shutdown got there first the
predicate signalled a type error for ten seconds and the suite reported that
the window manager had *not* exited.  The check failed precisely when the
shutdown was quickest.  Two runs in seven, and a whole section of FINDINGS.org
written about a window manager that always quit.

So the last condition is kept and printed when the wait times out having never
once answered.  One line, on the run where it happens, saying what the
predicate said instead of an answer."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (failure nil))
    (loop
      (multiple-value-bind (answer condition)
          (handler-case (values (funcall predicate) nil)
            (error (condition) (values nil condition)))
        (when condition (setf failure condition))
        (when answer (return answer)))
      (when (> (get-internal-real-time) deadline)
        (when failure
          (format t "  ....  the predicate never answered; it signalled: ~a~%"
                  failure)
          (force-output))
        (return nil))
      (sleep step))))

(defun find-program (&rest names)
  "The first of NAMES that is on PATH or named by an environment variable."
  (loop for name in names
        thereis (or (let ((from-env (sb-posix:getenv (string-upcase name))))
                      (when (and from-env (probe-file from-env))
                        ;; shell.nix exports RIVER as a *store path*, not a
                        ;; binary, so look inside it before believing it.
                        (or (probe-file (merge-pathnames
                                         (format nil "bin/~a" name)
                                         (uiop:ensure-directory-pathname from-env)))
                            (and (not (uiop:directory-pathname-p from-env))
                                 from-env))))
                    (ignore-errors
                     (let ((found (uiop:run-program (list "sh" "-c"
                                                          (format nil "command -v ~a" name))
                                                    :output '(:string :stripped t)
                                                    :ignore-error-status t)))
                       (when (plusp (length found)) (probe-file found)))))))

;;; ================================================================ the fixture

(defvar *river* nil)
(defvar *display* nil)
(defvar *runtime-dir* nil)
(defvar *display-file* nil
  "Where river's init command writes the display name it ended up with.")
(defvar *clients* '() "Launched client processes, for the teardown.")
(defvar *scratch* nil "A directory of our own, for state files and sockets.")

(defun start-river ()
  "Start river on a headless backend, and return its process.

WLR_BACKENDS=headless is what makes this possible without a screen: wlroots
creates a virtual output and never touches DRM or libinput.  The output it
makes is why the geometry checks can assert on a size this program did not
invent.

*THE DISPLAY NAME COMES BACK FROM RIVER, IT IS NOT GIVEN TO IT.*  Setting
WAYLAND_DISPLAY in a compositor's environment says nothing about the socket it
will create: wlroots calls wl_display_add_socket_auto and takes the first free
wayland-N.  So river's init command — the one thing guaranteed to run inside
the session with the right environment — writes the name out, and we read it.
Assuming the name instead is a test that fails on any machine where something
else already holds wayland-1, which is every developer's machine."
  (let ((program (find-program "river")))
    (unless program
      (return-from start-river nil))
    (setf *runtime-dir* (or (sb-posix:getenv "XDG_RUNTIME_DIR") "/tmp")
          *display-file* (format nil "/tmp/latticewm-integration-~d.display"
                                 (sb-posix:getpid)))
    (ignore-errors (delete-file *display-file*))
    (format t "~&river: ~a~%" program)
    (uiop:launch-program
     (list (namestring program)
           "-log-level" "error"
           ;; River runs one command as its init.  Ours reports the socket and
           ;; then does nothing: the window manager connects from *this*
           ;; process, so that the test can hold the world and ask it questions.
           "-c" (format nil "sh -c 'printf %s \"$WAYLAND_DISPLAY\" > ~a'"
                        *display-file*))
     :environment (list "WLR_BACKENDS=headless"
                        "WLR_LIBINPUT_NO_DEVICES=1"
                        "WLR_RENDERER=pixman"
                        ;; Nothing needs Xwayland here, and starting it costs a
                        ;; second and a screenful of xkbcomp warnings.
                        "XWAYLAND=0"
                        (format nil "XDG_RUNTIME_DIR=~a" *runtime-dir*)
                        (format nil "HOME=~a" (or (sb-posix:getenv "HOME") "/tmp"))
                        (format nil "PATH=~a" (or (sb-posix:getenv "PATH") "/usr/bin")))
     :output nil :error-output nil)))

(defun read-display-name ()
  "The WAYLAND_DISPLAY river settled on, once its init has written it."
  (and *display-file*
       (probe-file *display-file*)
       (let ((name (with-open-file (in *display-file*)
                     (read-line in nil ""))))
         (and (plusp (length name)) name))))

(defun socket-path ()
  (format nil "~a/~a" *runtime-dir* *display*))

(defvar *wm-under-test* nil
  "The thread START is running on.")

(defun start-window-manager ()
  "Run START on a thread, against the headless river.

On a thread because START owns the event loop and does not return until the
session ends, which is exactly the behaviour under test — a driver that called
the pieces by hand would be constructing state again."
  (sb-posix:setenv "WAYLAND_DISPLAY" *display* 1)
  (sb-posix:setenv "XDG_RUNTIME_DIR" *runtime-dir* 1)
  ;; A state directory of our own, so a developer's real session is neither
  ;; read nor written by the test.
  (setf *scratch* (format nil "/tmp/latticewm-integration-~d/" (sb-posix:getpid)))
  (ensure-directories-exist *scratch*)
  (sb-posix:setenv "XDG_STATE_HOME" *scratch* 1)
  (setf *log-level* :warn
        *log-file* nil
        *ipc-socket* nil)
  (setf *wm-under-test*
        (sb-thread:make-thread
         (lambda ()
           (ignore-errors (start :swank-port nil :config nil :restore nil)))
         :name "latticewm-under-test")))

(defun client-environment ()
  (list (format nil "WAYLAND_DISPLAY=~a" *display*)
        (format nil "XDG_RUNTIME_DIR=~a" *runtime-dir*)
        (format nil "HOME=~a" (or (sb-posix:getenv "HOME") "/tmp"))
        (format nil "PATH=~a" (or (sb-posix:getenv "PATH") "/usr/bin"))))

(defun client-command (program app-id)
  "How to ask PROGRAM for one window called APP-ID that then sits still.

A terminal running the user's shell is a terminal that exits when the shell
does, which on a machine with no $SHELL and no tty is immediately — so the
window appears, the checks start, and it vanishes underneath them.  Giving it a
command to run instead makes the window's lifetime ours."
  (let ((name (pathname-name program))
        (idle (list "sh" "-c" "exec sleep 3600")))
    (cond
      ((string= name "foot") (list* "--app-id" app-id idle))
      ((string= name "alacritty") (list* "--class" app-id "-e" idle))
      ((string= name "kitty") (list* "--class" app-id idle))
      ((string= name "xterm") (list* "-class" app-id "-e" idle))
      ;; Only terminals whose spelling for both is known are searched for, so
      ;; there is no fallback here that could hand a stranger a command line it
      ;; would refuse.
      (t idle))))

(defvar *client-program* nil)

(defun launch-client (app-id)
  "Start a client that will open one window, and return its process."
  (let ((program (or *client-program*
                     (setf *client-program*
                           (find-program "foot" "alacritty" "kitty" "xterm")))))
    (when program
      (let ((process (uiop:launch-program
                      (cons (namestring program) (client-command program app-id))
                      :environment (client-environment)
                      :output nil :error-output nil)))
        (push process *clients*)
        process))))

(defun window-named (app-id &optional (seconds 20))
  "Wait for the window APP-ID opened, and return it."
  (poll-until (lambda ()
                (find app-id (all-windows) :key #'c:window-app-id :test #'equal))
              seconds))

;;; -------------------------------------------------- reaching a later system
;;;
;;; THE LATTICE IS NOT LOADED IN THIS IMAGE UNTIL ONE SECTION LOADS IT, and it
;;; must not be.  Every section before that one is the *core* against a real
;;; compositor, and an extension that registers eighteen commands and twenty-eight
;;; methods before they run would have them asking their questions of a
;;; different program.  That is gate 4's ruling about a clean image, applied to
;;; the one place where it is a *sequence* rather than a separate process.
;;;
;;; Which means the names cannot be read yet: LOAD reads this whole file before
;;; running any of it, and LATTICE:ENABLE is not a readable symbol until the
;;; section has already loaded the system.  So the section resolves them when it
;;; calls them, which is the same thing tools/gates.lisp does for the same
;;; reason.

(defun late-call (name &rest arguments)
  "Call the function named by the string NAME, resolved now rather than at read
time."
  (apply (read-from-string name) arguments))

(defun late-value (name)
  "The value of the special variable named by the string NAME."
  (symbol-value (read-from-string name)))

(defun (setf late-value) (value name)
  (setf (symbol-value (read-from-string name)) value))

;;; ------------------------------------------------------- driving the program
;;;
;;; Every command below runs the way a command run from a REPL runs: queued onto
;;; the window manager thread, outside any protocol sequence, with the emitter
;;; reconciling afterwards.  That is deliberate.  It is the harder of the two
;;; paths — a command invoked from a key binding is already inside a manage
;;; sequence and can send whatever it likes — and it is the path the sequence
;;; discipline exists for.

(defun wm (thunk &key (timeout 10))
  "Run THUNK on the window manager thread and wait for its value.

Returns (values RESULT STATUS), STATUS being :OK, :ERROR or :TIMEOUT."
  (call-in-wm-thread-sync thunk :timeout timeout))

(defvar *relayouts* 0
  "How many times the emitter has finished a relayout.  See SETTLE.")

(defun note-relayout () (incf *relayouts*))

;;; ============================================================ the hook ledger
;;;
;;; FOURTEEN OF THE SEVENTEEN HOOKS HAD NEVER BEEN ATTACHED TO BY ANYTHING, so
;;; nothing had ever executed the two things a hook's contract actually is: the
;;; arguments its functions receive and the moment they receive them.  Both
;;; were guesses, and three were wrong -- including :STARTUP, which ran before
;;; the compositor connection existed while its docstring said it ran after.
;;;
;;; A recorder goes on every declared hook before the window manager starts,
;;; and stays there for the whole run.  The rest of this file then does what it
;;; was already doing -- opening windows, moving focus, switching workspaces,
;;; restoring a layout, quitting -- and the ledger section at the end asks what
;;; came out.  The marginal cost of covering a new hook is one row in a table,
;;; which is the same bargain the section shape made for checks.
;;;
;;; RECORDING RATHER THAN ASSERTING AT THE POINT OF FIRE, because a hook
;;; function runs on the window manager's thread inside a manage sequence, and
;;; a failed CHECK there would be a test harness signalling into the program
;;; under test.  RUN-HOOKS would guard it and the failure would vanish.

(defvar *hook-log* '()
  "(NAME . ARGUMENTS) for every hook that fired, most recent first.")

(defvar *hook-log-lock* (sb-thread:make-mutex :name "hook-log"))

(defun record-hook (name arguments)
  ;; Locked because these fire on the window manager's thread and are read
  ;; from the driver's.  A hook whose whole purpose is to be cheap should not
  ;; be the thing that makes a run flaky.
  (sb-thread:with-mutex (*hook-log-lock*)
    (push (cons name arguments) *hook-log*))
  nil)

(defparameter +cannot-fire-headless+
  '(:input-added :input-removed :keyboard-layout-changed :output-removed)
  "Hooks a headless backend structurally cannot produce.

Named one by one rather than counted.  river runs here with
WLR_LIBINPUT_NO_DEVICES=1, so no input device is ever announced and no xkb
layout ever toggles, and a virtual output cannot be unplugged.  Every one of
these is watched in tools/hardware-check.lisp instead, which is the only thing
in the project that runs on a machine.

:POINTER-OP is deliberately not here.  It looks like it belongs — an
interactive drag needs a pointer — but the floating section drives START- and
END-POINTER-OP directly, so it fires every run, and listing it would have been
an excuse for coverage that already existed.

A hook that is *not* on this list and did not fire is a failure, which is what
makes the list the interesting part of the check rather than an excuse in it.")

(defun watch-every-hook ()
  "Put a recorder on every declared hook.  Returns how many were attached.

By interned symbol rather than by closure, for the reason ADD-HOOK's docstring
gives: the list holds what it is given, and a fresh closure per call would
accumulate rather than replace."
  (let ((count 0))
    (dolist (row (all-hooks) count)
      (let* ((name (first row))
             (symbol (intern (format nil "WATCH~a" name) '#:latticewm/integration)))
        (unless (fboundp symbol)
          (setf (symbol-function symbol)
                (lambda (&rest arguments) (record-hook name arguments))))
        (add-hook name symbol)
        (incf count)))))

(defun fired (name)
  "Every argument list NAME was run with, oldest first."
  (sb-thread:with-mutex (*hook-log-lock*)
    (loop for (fired . arguments) in (reverse *hook-log*)
          when (eq fired name) collect arguments)))

;;; TWO HOOKS ARE ASSERTED ON WHAT THEY SAW RATHER THAN ON WHAT THEY WERE
;;; HANDED, because both bugs are about *when* and an object read afterwards
;;; has moved on.  An output's name and size arrive in events of their own, so
;;; a check that reads them at the end of the run cannot tell "the hook waited
;;; for them" from "they turned up later anyway" -- which is precisely the
;;; difference this commit is about.

(defvar *startup-saw* nil "The session, as :STARTUP found it.")

(defun note-startup-context ()
  (setf *startup-saw*
        (list :server (and *server* t)
              :manager (and *server* (server-manager *server*) t)
              :outputs (length (world-outputs *world*)))))

(defvar *outputs-as-announced* '()
  "(NAME WIDTH HEIGHT SCALE) for each output, read inside :OUTPUT-ADDED.")

(defun note-output-added (output)
  (let ((rect (output-rect output)))
    (push (list (output-name output) (rect-w rect) (rect-h rect)
                (output-scale output))
          *outputs-as-announced*)))

(defun settle (&key (seconds 10))
  "Ask for a manage sequence and wait for the relayout it causes.

COUNTED THROUGH THE :LAYOUT-CHANGED HOOK, which RELAYOUT runs as its last act.
Watching a piece of state instead is a race dressed as an assertion: the first
version of this compared the identity of :LAST-PLACEMENTS, which works right up
until a layout legitimately produces none — an empty pane on an empty
workspace — and then waits ten seconds for a change that already happened.  A
counter incremented by the program itself cannot have that problem, and it
exercises the hook mechanism on the way past."
  (let ((before *relayouts*))
    (mark-dirty)
    (poll-until (lambda () (> *relayouts* before)) seconds)))

(defun open-window (app-id)
  "Launch a client and wait for its window to be announced and placed."
  (when (launch-client app-id)
    (let ((window (window-named app-id)))
      (when window
        (poll-until (lambda () (c:leaf-holding (c:world-root *world*) window)) 10)
        (settle))
      window)))

(defun verb (name &rest arguments)
  "Run the command NAME with ARGUMENTS, then wait for the screen to catch up.

Returns (values COMMAND-RESULT SETTLED-P)."
  (multiple-value-bind (result status)
      (wm (lambda () (apply #'run-command name arguments)))
    (values result (and (eq status :ok) (settle)) status)))

(defun stand-on (window)
  "Put the cursor on WINDOW's pane, and say whether it got there.

Used wherever a section needs to be *on* a particular window rather than on
whatever the workspace's own DESCEND-TO-LEAF chose.  The sections above leave
empty panes lying about, which is correct behaviour and makes `the focused
pane' a different thing from run to run — so a check that means `send this
window' has to say so."
  (wm (lambda ()
        (let ((leaf (c:leaf-holding (c:world-root *world*) window)))
          (when leaf
            (jump-cursor (current-policy) *world*
                         (c:node-path-to (c:world-root *world*) leaf))))))
  (settle)
  (eq window (current-window)))

(defun placement-of (window)
  "WINDOW's placement in the last layout, as (NODE PATH RECT VISIBLE)."
  (find window (c:prop *world* :last-placements)
        :key (lambda (placement)
               (let ((node (first placement)))
                 (and (typep node 'c:leaf) (c:leaf-window node))))))

(defun rect-of (window)
  (third (placement-of window)))

(defun disjoint-p (a b)
  "Do A and B fail to overlap?  C:RECT-INTERSECT answers NIL for that case."
  (let ((overlap (and a b (c:rect-intersect a b))))
    (or (null overlap) (c:rect-empty-p overlap))))

(defun tree-window-count ()
  "How many windows are in the tree anywhere, on every workspace."
  (length (c:node-windows (c:world-root *world*))))

(defun on-screen-count ()
  "How many windows the last layout actually put on a screen.

Not TREE-WINDOW-COUNT: the root holds every workspace, so counting it answers
`how many windows exist' when the question is `how many are here'."
  (count-if (lambda (placement)
              (destructuring-bind (node path rect visible) placement
                (declare (ignore path rect))
                (and visible (typep node 'c:leaf) (c:leaf-window node))))
            (c:prop *world* :last-placements)))

(defun layout-shape ()
  "The tree's arrangement, in a form that survives a COPY-NODE.

NOT C:NODE-SIGNATURE, which is what the undo ring compares — that includes
NODE-ID, and a copied node deliberately gets a fresh one, so a signature can
say `this changed' about a tree that was restored perfectly.  Paths and windows
are what a person means by the arrangement, and both are copy-stable."
  (let ((root (c:world-root *world*)))
    (mapcar (lambda (path)
              (cons path (let ((node (c:resolve-path root path)))
                           (and (typep node 'c:leaf) (c:leaf-window node)))))
            (c:leaf-paths root))))

(defparameter +emitted-properties+
  '(:shown :position :borders :dimensions :clip :tiled :fullscreen
    :float-dimensions)
  "Every property the emitter diffs, so a snapshot of one window is total.")

(defun emitted-snapshot (window)
  "What the emitter believes it has already sent for WINDOW."
  (mapcar (lambda (property) (cons property (r::emitted window property)))
          +emitted-properties+))

;;; ==================================================================== the run

(setf *river* (start-river))

(unless *river*
  (format t "~&~%======== INTEGRATION ========~%~
             river was not found, so the compositor tests did not run.~%~%~
             This is the one test that receives state rather than~%~
             constructing it, and it is worth having: install river, or~%~
             run `nix-shell --run \"make integration\"', which pins one.~%~%")
  (sb-ext:quit :unix-status (if (strict-p) 1 0)))

(unwind-protect
     ;; A BLOCK rather than a PROGN, so that a river that never came up can stop
     ;; here instead of running eighty checks that can only say the same thing
     ;; eighty times.
     (block driving

       (section "the compositor"
         (setf *display* (poll-until #'read-display-name 20))
         (check *display* "river came up and told us its display: ~a"
                (or *display* "(nothing)"))
         (unless *display*
           (format t "~&  river did not start.  Try it by hand:~%~
                        ~4tWLR_BACKENDS=headless river -c true~%")
           (return-from driving))
         (check (poll-until (lambda () (probe-file (socket-path))) 10)
                "with a socket at ~a" (socket-path)))

       ;; BEFORE THE WINDOW MANAGER, not after.  :STARTUP fires once, from
       ;; inside START, so a recorder attached after the thread is launched is
       ;; a race that loses the one firing there will ever be.
       (add-hook :startup 'note-startup-context)
       (add-hook :output-added 'note-output-added)
       (watch-every-hook)

       (start-window-manager)
       ;; SETTLE counts relayouts through this, so it goes on before anything
       ;; asks for one.
       (add-hook :layout-changed 'note-relayout)

       ;; ------------------------------------------------------------------
       (section "the connection"
         ;; POLL FOR THE MANAGER, NOT JUST FOR THE SERVER.  *SERVER* is assigned
         ;; as soon as the object exists; the manager is bound a registry
         ;; roundtrip later.  Asserting on the manager the moment *SERVER*
         ;; appears is a race, and it lost about one run in three -- sometimes
         ;; here, sometimes on the version, sometimes not at all.  A test that
         ;; fails a third of the time for no reason is worse than no test,
         ;; because the next real failure reads as that one.
         (let* ((server (poll-until (lambda () *server*) 10))
                (manager (and server (poll-until (lambda () (server-manager server)) 10))))
           (check server "the window manager connected")
           (unless server (return-from driving))
           (check manager "river_window_manager_v1 is bound")
           (check (= (r::server-version server) r::+window-management-version+)
                  "at version ~d, which is the version we generated against"
                  (r::server-version server))
           ;; The globals the rest of this file depends on.  Every one of them
           ;; is a silent feature loss rather than an error when it is missing:
           ;; no wl_shm is a window manager that cannot draw its own echo area
           ;; and says nothing about why.
           ;;
           ;; AND EVERY ONE OF THEM IS POLLED, for the reason directly above,
           ;; which was fixed for the manager and left standing for its three
           ;; neighbours.  The manager being bound orders nothing else: globals
           ;; arrive when the compositor announces them, BIND-GLOBALS binds
           ;; whatever the first roundtrip caught and the registry hook binds
           ;; the rest as they appear, and river_xkb_bindings_v1 is on the far
           ;; side of that line often enough to matter.  It cost about one run
           ;; in two on a loaded machine -- reading as "keybindings are gone",
           ;; which is a catastrophe, in a run where they were merely late.
           (check (poll-until (lambda () (server-compositor server)) 10)
                  "wl_compositor is bound")
           (check (poll-until (lambda () (server-shm server)) 10)
                  "wl_shm is bound, so we can draw our own pixels")
           (check (poll-until (lambda () (r::server-bindings server)) 10)
                  "river_xkb_bindings_v1 is bound")
           (check (poll-until (lambda () (server-seats server)) 10)
                  "and river announced a seat")))

       ;; ------------------------------------------------------------------
       (section "the outputs"
         (let ((outputs (poll-until (lambda () (all-outputs)) 10)))
           (check outputs "river announced ~d output~:p" (length outputs))
           (when outputs
             (let* ((output (first outputs))
                    (rect (c:output-rect output)))
               (check (and (plusp (c:rect-w rect)) (plusp (c:rect-h rect)))
                      "with a size we received rather than assumed: ~dx~d"
                      (c:rect-w rect) (c:rect-h rect))
               ;; OUTPUTS HAD NO NAMES because the event we listened for does
               ;; not exist -- the name arrives on the wl_output, not on the
               ;; river_output_v1 -- and everything keyed on a name silently
               ;; keyed on NIL.  Per-output workspaces and the saved state file
               ;; are both keyed on it.
               ;; Polled, because the join is deliberately race-free rather than
               ;; ordered: river_output_v1.wl_output says *which* wl_output an
               ;; output is and wl_output.name says what that one is called, and
               ;; neither is guaranteed to arrive first.
               (check (poll-until (lambda ()
                                    (let ((name (c:output-name output)))
                                      (and name (plusp (length name)) name)))
                                  10)
                      "and a name from its wl_output: ~a"
                      (or (c:output-name output) "(none)"))
               (check (and (integerp (c:output-scale output))
                           (plusp (c:output-scale output)))
                      "and a scale of ~a" (c:output-scale output))
               ;; The bug this replaces: CURRENT-OUTPUT used to look the output
               ;; up through the cursor's *window*, so an empty pane -- which is
               ;; what a fresh session is -- matched nothing and fell through to
               ;; the first output.
               (check (eq output (current-output))
                      "and CURRENT-OUTPUT finds it from an empty pane")
               (check (equal (list (cons (c:output-name output) 0))
                             (r::output-workspaces))
                      "and the output is showing a workspace the state file can name")))))

       ;; ------------------------------------------------------------------
       (section "the first sequences"
         (check (poll-until (lambda () (c:prop *world* :last-placements)) 10)
                "a manage and a render sequence completed, and the emitter ~
                 produced placements river accepted")
         (check (hash-table-p (c:prop *world* :rect-index))
                "with a rectangle index for motion and pointer hit tests")
         (check (c:world-workspaces *world*)
                "and a workspace stack exists for the outputs river announced")
         (check (settle) "and asking for another round trip completes one"))

       ;; ------------------------------------------------------------------
       ;; THE wl_shm PATH.  DESIGN called it the least-proven part of wayflan,
       ;; and it is the one thing here that cannot be checked without a
       ;; compositor at all: a buffer the compositor refuses is a buffer whose
       ;; CREATE-POOL signalled, and nothing downstream of it exists.
       (section "the shared-memory surface"
         (let* ((overlays (poll-until (lambda () (all-overlays)) 10))
                (canvas (poll-until (lambda ()
                                      (some #'r::overlay-canvas (all-overlays)))
                                    10)))
           (check overlays "an overlay surface exists")
           (check canvas "with a shared-memory buffer river accepted")
           (when canvas
             (check (= (canvas-width canvas)
                       (* (r::canvas-scale canvas) (r::canvas-logical-width canvas)))
                    "whose device width is its logical width times its scale")
             (check (plusp (canvas-height canvas))
                    "and a height of ~d device pixels" (canvas-height canvas)))
           ;; HIDPI, WHICH THE README USED TO CLAIM WAS EXERCISED BY THE TEST
           ;; SUITE AND WHICH NOTHING TOUCHED.  A scaled canvas cannot be
           ;; constructed without a wl_shm, so this is the only place the
           ;; arithmetic that makes a 2x display legible can be run at all.
           ;; The headless backend's output is 1x, so the *canvas* is made at 2x
           ;; by hand; what a real 2x monitor would add is the scale arriving
           ;; from the compositor, which is a one-line assignment either way.
           (let ((shm (server-shm *server*)))
             (cond
               ((null shm) (skip "no wl_shm, so no scaled canvas"))
               (t
                (let ((scaled (wm (lambda () (make-canvas shm 100 40 2)))))
                  (cond
                    ((null scaled) (check nil "a 2x canvas could be created"))
                    (t
                     (check (= 200 (canvas-width scaled))
                            "a 2x canvas is allocated at ~d device pixels wide for 100 logical"
                            (canvas-width scaled))
                     (check (= 80 (canvas-height scaled))
                            "and ~d device pixels tall for 40 logical" (canvas-height scaled))
                     (check (= 100 (r::canvas-logical-width scaled))
                            "and still reports 100 logical pixels wide")
                     (check (= 800 (r::canvas-stride scaled))
                            "with a stride of ~d bytes, four per device pixel"
                            (r::canvas-stride scaled))
                     ;; The drawing routines take logical coordinates.  If they
                     ;; did not multiply, this would write off the end of a row
                     ;; and the run would end in a SIGSEGV rather than a check.
                     (check (progn (canvas-fill scaled (argb 0.0 0.0 0.0 1.0))
                                   (canvas-fill scaled (argb 1.0 1.0 1.0 1.0)
                                                (c:make-rect 10 5 80 30))
                                   t)
                            "and filling a logical rectangle inside it stays inside it")
                     (check (plusp (canvas-text scaled 2 2 "HiDPI" (argb 1.0 1.0 1.0)))
                            "and text drawn at 2x advances by whole device pixels")
                     (wm (lambda () (destroy-canvas scaled)))
                     (check t "and the buffer, pool, mapping and descriptor release")))))))
           (skip "a physically scaled output: wlroots' headless backend is 1x")))

       ;; ------------------------------------------------------------------
       ;; THE ECHO AREA IS ONE SURFACE PER OUTPUT, which it was not: it used to
       ;; be one global, so on a two-monitor desktop the one piece of permanent
       ;; orientation this window manager has existed on exactly one screen and
       ;; stayed there when the cursor left.
       (section "the echo area"
         (wm (lambda () (notify "integration: ~d" 42)))
         (check (settle) "a message settles a round trip")
         (let ((echo (first (all-overlays :echo))))
           (check echo "the echo area has an overlay of its own")
           (when echo
             (check (r::overlay-canvas echo) "with pixels behind it")
             (check (overlay-visible-p echo) "and river is showing it")
             (check (eq (current-output) (overlay-output echo))
                    "belonging to an output rather than to the program")
             (let ((rect (overlay-rect echo))
                   (screen (c:output-rect (current-output))))
               (check (= (c:rect-bottom rect) (c:rect-bottom screen))
                      "sitting on the bottom edge of it: y=~d, height ~d"
                      (c:rect-y rect) (c:rect-h rect))
               (check (= (c:rect-w rect) (c:rect-w screen))
                      "and as wide as the output"))
             ;; RESERVED SPACE.  The echo area takes a strip of the output away
             ;; from the layout through the :RESERVE-SPACE hook, and a layout
             ;; that ignored it would draw windows underneath the status line.
             (let ((outer (outer-rect (current-policy) (current-output)))
                   (screen (c:output-rect (current-output))))
               (check (< (c:rect-bottom outer) (c:rect-bottom screen))
                      "and the layout area stops above it: ~d against ~d"
                      (c:rect-bottom outer) (c:rect-bottom screen))))))

       ;; ------------------------------------------------------------------
       (section "the keyboard"
         (let ((seat (primary-seat)))
           (cond
             ((null seat) (skip "the headless backend announced no seat"))
             (t
              (let ((bound (hash-table-count (r::seat-bound-keys seat)))
                    (wanted (length (p::bindable-keys))))
                (check (= bound wanted)
                       "the keymap binds ~d key~:p and river made ~d binding object~:p"
                       wanted bound))
              ;; A KEYMAP EDIT HAS TO REACH THE COMPOSITOR, or live redefinition
              ;; is a story about a model nobody can see.  This is REBIND-KEYS'
              ;; whole job and nothing exercised it.
              (let ((before (hash-table-count (r::seat-bound-keys seat))))
                (multiple-value-bind (result status)
                    (wm (lambda ()
                          ;; DEFINE-KEY takes the *spec*, not a parsed key --
                          ;; it calls KBD itself, so handing it one twice is an
                          ;; error rather than a no-op.
                          (define-key *keymap* "Ctrl+Alt+Super+F12" "focus-next")
                          (rebind-keys)))
                  (declare (ignore result))
                  (check (eq :ok status) "a key can be bound at run time (~a)" status))
                (settle)
                (let ((after (hash-table-count (r::seat-bound-keys seat))))
                  (check (= after (1+ before))
                         "and binding one more key made one more binding ~
                          object (~d -> ~d)" before after)))
              ;; P:CAPTURE-KEYS is the whole of what a prompt, an empty pane or
              ;; the second key of a chord can ever read, because river delivers
              ;; keys to the focused *window* and hands us only what we asked
              ;; for.  Asking the seat how many binding objects exist is the
              ;; only way to know the answer was acted on: a list nobody turned
              ;; into river_xkb_binding_v1 objects is a keymap that silently
              ;; does nothing, which is precisely what a submap did before its
              ;; bug was found.
              (let ((wanted (length (capture-keys (current-policy))))
                    (made (poll-until (lambda ()
                                        (let ((b (c:prop seat :capture-bindings)))
                                          (and b (length b))))
                                      10)))
                (check (eql wanted made)
                       "the policy asked for ~d capture key~:p and river made ~a binding~:p"
                       wanted (or made 0)))
              ;; A fresh session's cursor is on an empty pane, which is one of
              ;; the three things that want the next keypress -- so the capture
              ;; bindings should be armed, not merely created.
              (check (c:prop seat :capture-armed)
                     "and they are armed, because the cursor is on an empty pane")))))

       ;; ------------------------------------------------------------------
       ;; THE BUG THIS FILE FOUND ON ITS FIRST RUN, and which nothing has
       ;; checked since: pointer bindings were enabled through the *keyboard*
       ;; binding interface, which is a type error inside the generated
       ;; marshaller -- caught, logged, and otherwise completely silent.
       ;; Super+drag did nothing at all.
       (section "the pointer bindings"
         (let ((seat (primary-seat)))
           (cond
             ((null seat) (skip "no seat, so no pointer bindings"))
             (t
              (let ((bindings (c:prop seat :pointer-bindings)))
                (check bindings "river made ~d pointer binding~:p"
                       (length bindings))
                (check (= (length bindings) (length p:*pointer-bindings*))
                       "one for each of the ~d the policy declares"
                       (length p:*pointer-bindings*))
                (check (every #'cdr bindings)
                       "and every one of them is a real river_pointer_binding_v1"))))))

       ;; ------------------------------------------------------------------
       (section "a client window"
         (let ((window (open-window "latticewm-a")))
           (cond
             ((null window)
              (missing "no terminal emulator on PATH, so no window was opened. ~
                        Install foot, alacritty, kitty or xterm — or run ~
                        `nix-shell --run \"make check\"', which pins one."))
             (t
              (check t "a client window was announced")
              (check (equal "latticewm-a" (c:window-app-id window))
                     "and river told us who it is: ~a" (c:window-app-id window))
              (check (c:window-live-p window) "and it is live")
              (check (c:leaf-holding (c:world-root *world*) window)
                     "and it was placed in the tree")
              (check (poll-until (lambda () (plusp (c:window-width window))) 10)
                     "and river told us the size it actually took: ~dx~d"
                     (c:window-width window) (c:window-height window))
              (check (window-river-node window)
                     "and it has a river_node_v1 to be positioned by")
              (let ((rect (rect-of window))
                    (screen (c:output-rect (current-output))))
                (check rect "and the emitter gave it a rectangle")
                (when (and rect screen)
                  (check (and (>= (c:rect-x rect) (c:rect-x screen))
                              (>= (c:rect-y rect) (c:rect-y screen))
                              (<= (c:rect-right rect) (c:rect-right screen))
                              (<= (c:rect-bottom rect) (c:rect-bottom screen)))
                         "inside the output river gave us: ~d,~d ~dx~d"
                         (c:rect-x rect) (c:rect-y rect)
                         (c:rect-w rect) (c:rect-h rect))))
              ;; D18 is the idea the README leads with and it was a COND in an
              ;; event handler until it was P:FOCUS-TARGET.  The window opened
              ;; onto the pane the cursor is on, so the derivation says: that
              ;; window has the keyboard.  Anything short of asking the *seat*
              ;; proves only that the model agrees with itself.
              (let ((seat (primary-seat)))
                (when seat
                  (check (poll-until (lambda () (eq window (r::seat-focused seat))) 10)
                         "and P:FOCUS-TARGET's answer is what river was told to focus")))))))

       ;; ------------------------------------------------------------------
       ;; THE PAIR OF EVENTS THAT WERE UNHANDLED.  river 0.4.6 added
       ;; capture_sessions to river_window_v1 and to river_output_v1, and
       ;; src/protocol/PINNED recorded that we did not listen for either.
       ;;
       ;; This is the half of handling them that only a compositor can settle.
       ;; The unit suite establishes everything about the count, the
       ;; announcement rule and the listing by constructing state; what it
       ;; cannot see is whether river *sends* the events, and a count of NIL is
       ;; precisely the shape of a handler that never ran.  That is why the
       ;; model keeps NIL and 0 apart -- so this check can tell "nobody is
       ;; recording" from "we are not being told", which is the failure that
       ;; would otherwise look identical from the outside.
       (section "screen capture"
         (let ((output (first (all-outputs)))
               (window (window-named "latticewm-a" 1)))
           (cond
             ((null output) (missing "no output, so no capture count to receive"))
             (t
              (check (poll-until (lambda ()
                                   (integerp (c:output-capture-sessions output)))
                                 10)
                     "river reported the output's capture sessions: ~a"
                     (c:output-capture-sessions output))
              (check (not (c:output-captured-p output))
                     "and nothing is recording this headless screen")))
           (cond
             ((null window) (missing "no window, so no per-window capture count"))
             (t
              (check (poll-until (lambda ()
                                   (integerp (c:window-capture-sessions window)))
                                 10)
                     "and the window's own count, which is the other half of ~
                      the event: ~a"
                     (c:window-capture-sessions window))
              (check (not (c:window-captured-p window))
                     "with nothing recording it either")))
           (check (null (c:world-captures *world*))
                  "so the listing is empty and the status line says no REC")))

       ;; ------------------------------------------------------------------
       ;; EVERYTHING IS DIFFED, says the emitter's header, and river's spec has
       ;; an `unresponsive' error it will use if we are slow.  A hundred
       ;; unchanged positions re-sent on every keystroke is the failure the diff
       ;; table exists to prevent, and the way to see it is that a settled
       ;; session queues no work.
       (section "the emitter diffs"
         (let ((window (window-named "latticewm-a" 1)))
           (cond
             ((null window) (missing "no window, so nothing to diff"))
             (t
              (settle)
              (check (not (eq :none (r::emitted window :position)))
                     "the window's position was sent once and remembered")
              (check (not (eq :none (r::emitted window :borders)))
                     "and so were its borders")
              (check (eq t (r::emitted window :shown))
                     "and it is recorded as shown")
              (check (not (eq :none (r::emitted window :dimensions)))
                     "and its dimensions were proposed")
              ;; THE DIFF ITSELF.  Every emission goes through WHEN-CHANGED,
              ;; which writes the value it sent into this table — so a table
              ;; that is byte-for-byte identical after another full relayout is
              ;; a relayout that sent nothing.  This is the check for the bug
              ;; the diff exists to prevent: floats re-proposing identical
              ;; dimensions on every keystroke, into a compositor that
              ;; processes every request we send before it can answer input.
              (let ((before (emitted-snapshot window)))
                (settle)
                (settle)
                (check (equal before (emitted-snapshot window))
                       "and two further round trips re-send none of it"))
              ;; The other direction: a forced relayout is how a hot reconnect
              ;; re-sends everything, and after CLRHASH the table can only be
              ;; repopulated by things actually going back out on the wire.
              (wm (lambda () (relayout :force t)))
              (settle)
              (check (not (eq :none (r::emitted window :dimensions)))
                     "while a forced relayout clears the cache and re-sends")))))

       ;; ------------------------------------------------------------------
       ;; SMART GAPS, AND THE REASON IT IS CHECKED HERE RATHER THAN ONLY IN THE
       ;; UNIT SUITE.  *SMART-GAPS* was registered, exported, documented and
       ;; read by nothing for the whole life of the option: the model was
       ;; perfectly consistent with itself and the screen was wrong, which is
       ;; the exact class every bug this file has found belongs to.  A test
       ;; that constructs a world can assert BORDER-WIDTH returns zero.  Only a
       ;; compositor can say the zero was sent.
       ;;
       ;; This section runs while exactly one window is up, which is the state
       ;; the option is about, and it puts it back before the next one opens.
       (section "smart gaps"
         (let ((window (window-named "latticewm-a" 1)))
           (cond
             ((null window) (missing "no window, so nothing is alone on a screen"))
             ((/= 1 (on-screen-count))
              (skip "~d panes are on the screen, so this one is not alone"
                    (on-screen-count)))
             (t
              (settle)
              (check p:*smart-gaps* "the shipped default is on")
              (let ((leaf (c:leaf-holding (c:world-root *world*) window)))
                (check (eql 0 (p:border-width (current-policy) leaf nil))
                       "and the policy answers zero for the window that is alone"))
              (let ((borders (r::emitted window :borders)))
                (check (and (consp borders) (eql 0 (first borders)))
                       "and the border width river was sent is ~s"
                       (and (consp borders) (first borders))))
              ;; THE OPTION, LIVE.  Turning it off has to bring the border and
              ;; the screen-edge gap back on the next relayout, which is the
              ;; property that was false: the value was settable, printed as
              ;; working, and connected to nothing.
              (let ((screen (c:output-rect (current-output)))
                    (gapped nil)
                    (ungapped nil))
                (wm (lambda () (setf p:*outer-gaps* 20)))
                (settle)
                (setf ungapped (outer-rect (current-policy) (current-output)))
                (check (= (c:rect-w ungapped) (c:rect-w screen))
                       "with the option on, a 20px outer gap costs nothing: ~d of ~d"
                       (c:rect-w ungapped) (c:rect-w screen))
                (wm (lambda () (setf p:*smart-gaps* nil)))
                (settle)
                (setf gapped (outer-rect (current-policy) (current-output)))
                (check (= (- (c:rect-w ungapped) 40) (c:rect-w gapped))
                       "and with it off the same gap costs 40: ~d against ~d"
                       (c:rect-w gapped) (c:rect-w ungapped))
                (let ((borders (r::emitted window :borders)))
                  (check (and (consp borders) (eql p:*border-width* (first borders)))
                         "and the border came back at ~d" p:*border-width*))
                ;; Put the world back for the sections below, which assume the
                ;; shipped values.
                (wm (lambda () (setf p:*smart-gaps* t p:*outer-gaps* 0)))
                (settle)
                (check (= (c:rect-w (outer-rect (current-policy) (current-output)))
                          (c:rect-w screen))
                       "and the shipped values are restored"))))))

       ;; ------------------------------------------------------------------
       (section "two windows and the tree"
         (let ((a (window-named "latticewm-a" 1))
               (b (open-window "latticewm-b")))
           (cond
             ((not (and a b)) (missing "two client windows"))
             (t
              (check (= 2 (tree-window-count)) "both windows are in the tree")
              (let ((ra (poll-until (lambda () (rect-of a)) 5))
                    (rb (poll-until (lambda () (rect-of b)) 5)))
                (check (and ra rb) "and both were placed")
                (when (and ra rb)
                  (check (disjoint-p ra rb)
                         "in rectangles that do not overlap: ~d,~d ~dx~d and ~d,~d ~dx~d"
                         (c:rect-x ra) (c:rect-y ra) (c:rect-w ra) (c:rect-h ra)
                         (c:rect-x rb) (c:rect-y rb) (c:rect-w rb) (c:rect-h rb))))
              (check (poll-until (lambda () (plusp (c:window-width b))) 10)
                     "and river sized the second one too: ~dx~d"
                     (c:window-width b) (c:window-height b))
              ;; River leaves the initial render position of a node undefined,
              ;; so every visible window has to be ordered explicitly or
              ;; overlapping windows flicker between frames.
              (let ((signature (c:prop *world* :render-signature)))
                (check (= 2 (length signature))
                       "and both were given an explicit place in the render order"))))))

       ;; ------------------------------------------------------------------
       ;; THE VERBS.  Forty-four commands ship and five were ever called by a
       ;; test; every binding in both README key tables had a docstring, a gate
       ;; asserting the docstring exists, and nothing that ran it.  What is
       ;; checked here is not the tree surgery -- tests/test-tree.lisp owns that
       ;; and owns it well -- but that the verb reaches the compositor: that the
       ;; command runs from outside a sequence, that the round trip completes,
       ;; and that river was still talking to us afterwards.
       (section "the verbs, against a live compositor"
         (let ((a (window-named "latticewm-a" 1)))
           (cond
             ((null a) (missing "no window, so the verbs have nothing to move"))
             (t
              (multiple-value-bind (result settled) (verb "focus-next")
                (declare (ignore result))
                (check settled "focus-next completes a round trip"))
              (let ((before (current-path)))
                (verb "focus-previous")
                (check (not (equal before (current-path)))
                       "focus-previous moved the cursor: ~s -> ~s"
                       before (current-path)))
              (let ((before (tree-window-count)))
                (multiple-value-bind (result settled) (verb "split" :horizontal)
                  (declare (ignore result))
                  (check settled "split completes a round trip")
                  (check (= before (tree-window-count))
                         "and leaves an empty pane rather than a window")
                  (check (c:leaf-empty-p (current-leaf))
                         "with the cursor standing in it")))
              (multiple-value-bind (result settled) (verb "remove-pane")
                (declare (ignore result))
                (check settled "remove-pane completes a round trip")
                (check (= 2 (tree-window-count)) "and both windows survive it"))
              (multiple-value-bind (result settled) (verb "swap" :right)
                (declare (ignore result))
                (check settled "swap completes a round trip"))
              (multiple-value-bind (result settled) (verb "equalize")
                (declare (ignore result))
                (check settled "equalize completes a round trip"))
              (multiple-value-bind (result settled) (verb "resize" :right 20)
                (declare (ignore result))
                (check settled "resize completes a round trip"))
              (multiple-value-bind (result settled) (verb "tab")
                (declare (ignore result))
                (check settled "tab completes a round trip"))
              (multiple-value-bind (result settled) (verb "untab")
                (declare (ignore result))
                (check settled "untab completes a round trip"))
              (check (server-running *server*)
                     "and after all of them river is still talking to us")
              (check (= 2 (tree-window-count))
                     "with both windows still in the tree")))))

       ;; ------------------------------------------------------------------
       (section "floating, fullscreen and minimizing"
         (let ((a (window-named "latticewm-a" 1)))
           (cond
             ((null a) (missing "no window to float"))
             (t
              (check (stand-on a) "the cursor can be put on the window")
              (verb "toggle-float")
              (check (c:window-floating-p a) "toggle-float takes a window out of the tree")
              (check (c:float-of-window *world* a) "and gives it a rectangle of its own")
              (check (= 1 (on-screen-count))
                     "leaving one window tiled")
              (check (not (eq :none (r::emitted a :tiled)))
                     "and river was told it is no longer tiled")
              ;; DRAG-RESIZE, the third thing README claimed the test suite
              ;; exercised and nothing referenced.  River sends *cumulative*
              ;; deltas, which is the whole reason POINTER-OP keeps the
              ;; rectangle the drag started from; accumulating instead drifts,
              ;; and drifts worst exactly when the pointer leaves the screen and
              ;; comes back.
              ;;
              ;; THE WHOLE DRAG RUNS IN ONE HOP ONTO THE WINDOW MANAGER THREAD,
              ;; and that is not tidiness.  There is no button held down here,
              ;; so the moment op_start_pointer actually reaches river — which,
              ;; since the deferral above, it does — river answers op_release
              ;; and the operation ends.  Driving it a round trip at a time
              ;; would be racing the compositor for the right answer.  What is
              ;; under test is the arithmetic, which is what POINTER-OP's own
              ;; header says this file is for.
              (let ((seat (primary-seat)))
                (cond
                  ((null seat) (skip "no seat, so no drag"))
                  (t
                   (destructuring-bind (operation queued start once twice ended)
                       (or (wm (lambda ()
                                 (let* ((float (c:float-of-window *world* a))
                                        (op (start-pointer-op seat :move a))
                                        (queued (and (r::server-pending-manage-work
                                                      *server*)
                                                     t))
                                        (from (c:copy-rect (c:float-rect float))))
                                   (apply-pointer-delta seat 40 25)
                                   (let ((once (c:copy-rect (c:float-rect float))))
                                     (apply-pointer-delta seat 40 25)
                                     (let ((twice (c:copy-rect (c:float-rect float))))
                                       (end-pointer-op seat)
                                       (list op queued from once twice
                                             (r::seat-pointer-op seat)))))))
                           (list nil nil nil nil nil :unrun))
                     (check operation "a pointer move operation starts on the seat")
                     ;; op_start_pointer IS MANAGE-SEQUENCE-ONLY AND ITS ONLY
                     ;; CALLER IS AN EVENT HANDLER.  Sent directly it was
                     ;; refused, logged and lost, so river never entered
                     ;; pointer-op mode, never sent an op_delta, and Super+drag
                     ;; did nothing at all -- in silence, for the second time and
                     ;; for a different reason than the first.  The queue is the
                     ;; observable proof that it waits for a sequence instead.
                     (check queued
                            "with its manage-only half queued rather than refused")
                     (when (and start once twice)
                       (check (and (= (c:rect-x once) (+ 40 (c:rect-x start)))
                                   (= (c:rect-y once) (+ 25 (c:rect-y start))))
                              "a cumulative delta of 40,25 moves it exactly that far")
                       ;; The same delta again must not move it twice: that is
                       ;; the difference between cumulative and incremental, and
                       ;; it is the drift the struct's docstring is about.
                       (check (c:rect-equal once twice)
                              "and repeating it does not move it again"))
                     (check (null ended) "and releasing clears it from the seat"))
                   (settle)
                   (check (null (r::server-pending-manage-work *server*))
                          "and the manage sequence drained what the drag queued")
                   ;; And a resize, which is the half that informs the client
                   ;; that its size is about to change repeatedly.
                   (destructuring-bind (operation was now)
                       (or (wm (lambda ()
                                 (let* ((float (c:float-of-window *world* a))
                                        (op (start-pointer-op
                                             seat :resize a
                                             :edges (list :right :bottom)))
                                        (was (c:copy-rect (c:float-rect float))))
                                   (apply-pointer-delta seat 30 30)
                                   (let ((now (c:copy-rect (c:float-rect float))))
                                     (end-pointer-op seat)
                                     (list op was now)))))
                           (list nil nil nil))
                     (check operation "a pointer resize operation starts")
                     (when (and was now)
                       (check (> (c:rect-w now) (c:rect-w was))
                              "and dragging the right edge makes it wider: ~d -> ~d"
                              (c:rect-w was) (c:rect-w now))
                       (check (= (c:rect-x now) (c:rect-x was))
                              "without moving the edge that was not dragged")))
                   (settle))))
              (verb "toggle-float")
              (check (not (c:window-floating-p a)) "toggle-float puts it back in the tree")
              (check (= 2 (on-screen-count)) "and the layout holds both again")
              ;; Fullscreen and minimize are window-management state, so a
              ;; command that sent its own requests worked from a key binding
              ;; and failed from a REPL.  These run from a REPL.
              (verb "toggle-fullscreen")
              (check (c:window-fullscreen-p (focused-window))
                     "toggle-fullscreen reaches river from outside a sequence")
              (verb "toggle-fullscreen")
              (check (not (c:window-fullscreen-p (focused-window)))
                     "and back again")
              (let ((window (focused-window)))
                (verb "minimize")
                (check (c:window-minimized-p window) "minimize takes a window off the screen")
                (check (null (r::emitted window :shown))
                       "and river was told to hide it")
                (verb "restore-last")
                (check (not (c:window-minimized-p window)) "and restore-last brings it back")
                (check (eq t (r::emitted window :shown))
                       "and river was told to show it again"))))))

       ;; ------------------------------------------------------------------
       (section "workspaces"
         (let ((a (window-named "latticewm-a" 1)))
           (cond
             ((null a) (missing "no window to send anywhere"))
             (t
              (let ((before (c:container-count (c:world-workspaces *world*))))
                (verb "new-workspace")
                (check (= (1+ before) (c:container-count (c:world-workspaces *world*)))
                       "new-workspace adds one: ~d -> ~d" before
                       (c:container-count (c:world-workspaces *world*))))
              (check (= 0 (on-screen-count))
                     "and the new one is empty, so nothing is on the screen")
              (check (= 2 (tree-window-count))
                     "though both windows still exist, on the workspace we left")
              ;; THE WINDOWS YOU WALKED AWAY FROM ARE NOT INVISIBLE PLACEMENTS,
              ;; THEY ARE ABSENT ONES.  OUTPUT-CONTENT hands the layout the
              ;; workspace the output is showing and no other, so nothing in the
              ;; placement walk mentions workspace 1 while you are on workspace
              ;; 2 -- and for the life of the project nothing hid them.  The
              ;; model was right and the screen was wrong, which is the whole
              ;; category of bug this file exists for.
              (check (poll-until (lambda () (null (r::emitted a :shown))) 5)
                     "and river was told to hide the window left behind")
              (verb "workspace" 1)
              (check (= 2 (on-screen-count)) "going back puts both back on screen")
              (check (poll-until (lambda () (eq t (r::emitted a :shown))) 5)
                     "and river was told to show them")
              ;; Stand on a window rather than on whatever the workspace's
              ;; own DESCEND-TO-LEAF chose.  The sections above leave empty
              ;; panes about, and sending an empty pane to another workspace is
              ;; a perfectly good thing to do and not what this is testing.
              (check (stand-on a) "standing on a window rather than a gap")
              (verb "send-to-workspace" 2)
              (check (= 1 (on-screen-count))
                     "send-to-workspace takes one with it")
              (verb "workspace" 2)
              (check (= 1 (on-screen-count)) "and it is there when you follow")
              ;; A WORKSPACE HOLDING ONE PANE *IS* THAT PANE, and sending it
              ;; away would move the workspace out of the stack rather than the
              ;; window out of the workspace.  SEND-TO-WORKSPACE declines, which
              ;; is a real invariant and worth saying out loud: it is the reason
              ;; the obvious `send my only window back' does nothing.
              (let ((before (on-screen-count)))
                (verb "send-to-workspace" 1)
                (check (= before (on-screen-count))
                       "and a workspace that is a single pane declines to send ~
                        itself away"))
              ;; Give it a pane to move and it moves.
              (verb "split")
              (verb "focus-previous")
              (check (current-window) "with somewhere to stand, the cursor is on a window")
              (verb "send-to-workspace" 1)
              (check (= 0 (on-screen-count)) "which then leaves")
              (verb "workspace" 1)
              (check (= 2 (on-screen-count)) "and comes back")))))

       ;; ------------------------------------------------------------------
       (section "tags and named scratchpads"
         (let ((a (window-named "latticewm-a" 1)))
           (cond
             ((null a) (missing "no window to tag"))
             (t
              (check (stand-on a) "the cursor can be put on the window")
              (verb "tag-window" "music")
              (check (window-tagged-p a "music") "a real window takes a tag")
              (check (member a (windows-tagged "music"))
                     "and is found by it")
              (verb "focus-next")
              (verb "jump-to-tag" "music")
              (check (eq a (current-window)) "jump-to-tag goes back to it")
              (check (poll-until (lambda () (eq a (r::seat-focused (primary-seat)))) 5)
                     "and river was told to focus it")
              (verb "scratchpad-put" "notes")
              (check (c:window-minimized-p a) "scratchpad-put puts it away")
              (check (eql (normalize-tag "notes") (c:prop a :scratchpad))
                     "under the name it was given, normalised to ~s"
                     (c:prop a :scratchpad))
              (verb "scratchpad-show" "notes")
              (check (not (c:window-minimized-p a)) "and scratchpad-show gets it back")
              (verb "untag-window" "music")
              (check (not (window-tagged-p a "music")) "untag-window removes the tag")))))

       ;; ------------------------------------------------------------------
       ;; UNDO IS A COMMAND WRAPPER, so it covers commands a user writes as well
       ;; as the shipped ones -- which means it runs on every verb above and had
       ;; never once been run against a tree holding real windows.
       (section "undo"
         (let ((before (layout-shape)))
           (verb "split" :vertical)
           (let ((split (layout-shape)))
             (check (not (equal before split)) "a split changes the arrangement")
             (verb "undo")
             (check (equal before (layout-shape)) "undo puts it back")
             (check (= 2 (on-screen-count)) "with both windows still on screen")
             (verb "redo")
             (check (equal split (layout-shape)) "and redo does it again")
             (verb "undo")
             (check (equal before (layout-shape)) "and undo again")
             (check (settle) "and the screen follows the tree each time"))))

       ;; ------------------------------------------------------------------
       ;; THE STATE FILE, whose failure mode is silent data loss on restart and
       ;; which nothing had ever written from a live session.  A tag and a
       ;; named scratchpad are the two facts that are *not* in the tree, and
       ;; they are the two that went missing for the life of the project.
       (section "the saved layout"
         (let ((a (window-named "latticewm-a" 1))
               (path (merge-pathnames "layout.lisp" *scratch*)))
           (cond
             ((null a) (missing "no window, so nothing worth saving"))
             (t
              (check (stand-on a) "the cursor can be put on the window")
              (verb "tag-window" "saved")
              (check (window-tagged-p a "saved") "the window carries a tag")
              (check (wm (lambda () (save-state path))) "the layout writes out")
              (check (probe-file path) "to a file that exists")
              (let ((form (with-open-file (in path)
                            (let ((*package* (find-package :keyword)))
                              (read in nil nil)))))
                (check (consp form) "which reads back as one form")
                (check (eql r::+state-version+ (getf form :version))
                       "at the current version")
                (check (getf form :root) "with a tree in it")
                (check (assoc (c:output-name (current-output)) (getf form :outputs)
                              :test #'equal)
                       "and the output's workspace, keyed by the name river gave it")
                ;; The fact that is not in the tree.
                (let ((facts (getf form :windows)))
                  (check (assoc (c:window-identifier a) facts :test #'equal)
                         "and the window's own state, keyed by river identifier")
                  (let ((row (cdr (assoc (c:window-identifier a) facts :test #'equal))))
                    (check (member (normalize-tag "saved") (getf row :tags))
                           "including the tag, which is not part of any node"))))
              ;; And back in.  The windows are the same live windows, so this is
              ;; the restart path minus the process boundary.
              (wm (lambda () (untag-window "saved")))
              (check (not (window-tagged-p a "saved")) "the tag can then be removed")
              (check (wm (lambda () (load-state path))) "the file loads again")
              ;; THE HOOK A POLICY WITH A SHAPE NEEDS.  The restore replaces
              ;; the tree wholesale, so anything the configuration built is
              ;; thrown away and :LAYOUT-RESTORED is the only chance to put it
              ;; back -- without it, enabling the lattice in a config file left
              ;; the policy saying `lattice' and every cell command answering
              ;; NIL.  It is the one hook of the eighteen with a consumer in
              ;; the shipped extension, and nothing had ever watched it fire.
              (check (fired :layout-restored)
                     ":layout-restored ran, so a policy that needs a shape ~
                      gets to re-establish it")
              (settle)
              (check (window-tagged-p a "saved")
                     "and the tag comes back with it")
              (check (= 2 (on-screen-count))
                     "with both windows still placed")
              (wm (lambda () (untag-window "saved")))))))

       ;; ------------------------------------------------------------------
       ;; THE CONTROL SOCKET, which is how a script drives a running window
       ;; manager and which had no test of any kind.  Its whole contract is one
       ;; form in, one line out, whatever happens — including for a form that
       ;; does not read, one that signals, and one that never returns.
       (section "the control socket"
         (let ((path (namestring (merge-pathnames "control.sock" *scratch*))))
           (setf *ipc-socket* path)
           (check (start-ipc-server) "the control socket opens at ~a" path)
           (check (probe-file path) "and the socket file exists")
           (flet ((ask (form)
                    (string-trim '(#\Newline #\Space)
                                 (with-output-to-string (out)
                                   (ipc-evaluate (list form) :path path :stream out)))))
             (check (equal "(:ok 3)" (ask "(+ 1 2)"))
                    "a form goes in and one line comes out: ~s" (ask "(+ 1 2)"))
             (check (search ":ok" (ask "(length (all-windows))"))
                    "it can see the live world")
             (let ((answer (ask "(run-command \"focus-next\")")))
               (check (search ":ok" answer)
                      "and run a command on the window manager thread: ~s" answer))
             ;; (CAR 5) rather than a call to something undefined, which is the
             ;; other obvious choice: EVAL compiles, and compiling a call to a
             ;; function that does not exist prints eight lines of SBCL
             ;; style-warning into the middle of a test run.  A green run that
             ;; looks like a broken one is a cost with no benefit -- and the
             ;; undefined-function hint itself is a pure function over a
             ;; condition, which belongs in tests/ where it is cheaper.
             (let ((answer (ask "(car 5)")))
               (check (search ":error" answer)
                      "a form that signals answers with an error rather than silence"))
             (let ((answer (ask "(")))
               (check (search ":error" answer)
                      "and so does a form that does not even read"))
             (let ((answer (ask "(list 1 2 3)")))
               (check (= 1 (count #\Newline (concatenate 'string answer (string #\Newline))))
                      "and every answer is exactly one line")))
           (stop-ipc-server)
           (setf *ipc-socket* nil)
           (check (not (probe-file path)) "and stopping it takes the socket away")))

       ;; ------------------------------------------------------------------
       ;; INPUT CONFIGURATION.  THE ONLY PLACE THIS CAN BE CHECKED AT ALL:
       ;; three protocols were vendored into src/protocol/ and never compiled,
       ;; and nothing anywhere noticed -- not gate 1, which cannot see an XML no
       ;; component names; not gate 5, which counted only the three that were
       ;; named; not the unit suite, which constructs state and so can construct
       ;; a device without ever asking river for one.
       (section "input configuration"
         (let ((server *server*))
           (check (r::server-input-manager server) "river_input_manager_v1 is bound")
           (check (r::server-xkb-config server) "river_xkb_config_v1 is bound")
           ;; A HEADLESS BACKEND HAS NO INPUT DEVICES AT ALL -- wlroots creates
           ;; a virtual output and no virtual keyboard -- so what follows is
           ;; conditional and says so when it cannot run.  The device path is
           ;; exercised for real by running nested under an ordinary session
           ;; (WLR_BACKENDS=wayland), where river makes one virtual device per
           ;; capability of the parent seat.  It is not done here because
           ;; `make check' would then open a window on whatever desktop it was
           ;; run from.
           (let ((devices (poll-until (lambda () (c:world-inputs *world*)) 3)))
             (cond
               ((null devices)
                (skip "the headless backend has no libinput devices, so no ~
                       device was configured"))
               (t
                (check (some #'c:input-device-name devices)
                       "~d input device~:p, named: ~{~a~^, ~}"
                       (length devices)
                       (remove nil (mapcar #'c:input-device-name devices)))
                ;; Key repeat is river's own request rather than libinput's, so
                ;; it is the one setting a backend with no libinput behind it
                ;; can still take.
                (let ((keyboard (find :keyboard devices :key #'c:input-device-kind)))
                  (cond
                    ((null keyboard) (skip "no keyboard among them"))
                    (t
                     (setf *repeat-rate* 42 *repeat-delay* 250)
                     (wm (lambda () (apply-input-configuration)))
                     (check (eql 42 (c:input-device-setting keyboard :repeat-rate))
                            "and set_repeat_info was accepted at 42/s"))))))))
         (skip "the libinput half — tap, acceleration, click and scroll method ~
                — needs hardware behind the device"))

       ;; ------------------------------------------------------------------
       (section "the layer shell"
         (cond
           ((null (r::server-layer-shell *server*))
            (check nil "zwlr_layer_shell_v1 is bound"))
           (t
            (check t "zwlr_layer_shell_v1 is bound")
            (check (equal '(0 0 0 0) (layer-reserved-edges (current-output)))
                   "and with no panels running it reserves nothing")))
         (skip "a panel or a screen locker: no layer-shell client is available ~
                headless, so the exclusive-zone arithmetic is unexercised"))

       ;; ------------------------------------------------------------------
       ;; THE EXTENSION, AND THE ONE REQUEST NOTHING HAD EVER SENT.
       ;;
       ;; P:CLIP-RECT is river's set_content_clip_box, which DESIGN calls the
       ;; best find in the whole protocol: a cell half over the viewport edge is
       ;; cropped *and its border is redrawn at the crop edge*, free, by the
       ;; compositor.  The lattice's method for it read a property that nothing
       ;; in the tree wrote, so it fell through to the shipped `nothing
       ;; overhangs, clip nothing' on every call for its entire life, and no unit
       ;; test could tell -- an absent property is a legal answer and NIL is a
       ;; legal return.  Gate 13 is the check for the property.
       ;;
       ;; THIS IS THE HALF ONLY A COMPOSITOR CAN SETTLE.  A clip box outside the
       ;; window's content is river's invalid_clip_box error, which does not
       ;; return a value -- it takes the connection down.  So the question "is
       ;; the box we compute one river will accept" cannot be asked of a model,
       ;; of a gate, or of a unit test, and it is the only question left once the
       ;; arithmetic is covered.  It is also the first time anything in this file
       ;; has run the extension against a real compositor at all.
       (section "the plane, and the clip box river accepts"
         (let ((a (window-named "latticewm-a" 1)))
           (cond
             ((null a) (missing "no window, so no cell to crop"))
             ((not (handler-case
                       (progn (handler-bind ((warning #'muffle-warning))
                                (asdf:load-system "lattice"))
                              t)
                     (error (condition)
                       (check nil "the lattice system loads: ~a" condition))))
              nil)
             (t
              (let ((area (outer-rect (current-policy) (current-output)))
                    (mode (late-value "lattice:*zoom-mode*"))
                    (width (late-value "lattice:*cell-width*"))
                    (height (late-value "lattice:*cell-height*")))
                (unwind-protect
                     (progn
                       ;; Three quarters of the plane's rect per cell, so the
                       ;; second of two cells is certain to hang over the edge
                       ;; whatever size the client decides to take.  Full height,
                       ;; so the overhang is on one axis and the numbers below
                       ;; say which.
                       (setf (late-value "lattice:*zoom-mode*") :fixed
                             (late-value "lattice:*cell-width*")
                             (floor (* 3 (c:rect-w area)) 4)
                             (late-value "lattice:*cell-height*") (c:rect-h area))
                       (check (wm (lambda ()
                                    (late-call "lattice:enable"
                                               :cols 2 :rows 1 :keys nil)))
                              "the lattice is enabled live, two cells wide")
                       (check (settle) "and the plane laid out")
                       (check (late-call "lattice:current-grid")
                              "the root of the workspace is a plane")
                       (check (stand-on a) "standing on the window")
                       (verb "move-to-cell" 1 0)
                       (check (settle) "and it is in cell 1,0, the trailing cell")
                       ;; C:WINDOW-RECT rather than RECT-OF, and the difference
                       ;; is the point of GRAVITY: the layout assigned the cell's
                       ;; rectangle, the client came back two pixels narrower
                       ;; than that, and PLACE-RECT centred the shortfall.  The
                       ;; clip box is computed against the rectangle the window
                       ;; actually got, which is the one river is being told
                       ;; about, so that is the one to compare with.
                       (let ((placed (c:window-rect a)))
                         (cond
                           ((null placed)
                            (check nil "the window still has a placement"))
                           ((<= (c:rect-right placed) (c:rect-right area))
                            (skip "the client took a size that fits inside the ~
                                   plane's rect, so nothing overhangs to crop"))
                           (t
                            (check t "the cell overhangs the plane by ~d pixels"
                                   (- (c:rect-right placed) (c:rect-right area)))
                            (let ((clip (r::emitted a :clip)))
                              (check (and (consp clip) (= 4 (length clip)))
                                     "and a content clip box was sent: ~a" clip)
                              (when (and (consp clip) (= 4 (length clip)))
                                (destructuring-bind (x y w h) clip
                                  (declare (ignore y h))
                                  (check (= x (c:rect-x placed))
                                         "starting at the window's own left edge")
                                  (check (= w (- (c:rect-right area) x))
                                         "and ~d pixels wide: the part of it that ~
                                          is on screen" w)))
                              (check (settle)
                                     "and river accepted it -- another sequence ~
                                      completes")
                              (check (server-running *server*)
                                     "with the connection still up, so the box ~
                                      was in range")))))
                       ;; THE DRAWN MAP DOES NOT COVER THE ECHO AREA, and it
                       ;; did.  DRAW-MAP-ON laid its cells out in OUTER-RECT and
                       ;; then sized and committed its canvas at the whole
                       ;; output, so the background fill covered the strip the
                       ;; echo area had reserved and the status line was blank
                       ;; for as long as the map was up.  The policy was right
                       ;; and the screen was wrong, which is the one class of
                       ;; defect no unit test in this project can reach.
                       ;;
                       ;; The threshold is raised rather than the zoom driven
                       ;; out, because the map is a question about the size of a
                       ;; cell and this makes every cell too small by definition.
                       (let ((threshold (late-value "lattice:*map-threshold*")))
                         (unwind-protect
                              (progn
                                (setf (late-value "lattice:*map-threshold*")
                                      (1+ (c:rect-w area)))
                                (wm (lambda () (relayout :force t)))
                                (settle)
                                (let ((map (first (all-overlays :lattice/map)))
                                      (echo (first (all-overlays :echo))))
                                  (check map "the drawn map has an overlay of ~
                                              its own")
                                  (when (and map echo)
                                    (check (overlay-visible-p map)
                                           "and river is showing it")
                                    (let ((drawn (overlay-rect map))
                                          (strip (overlay-rect echo))
                                          (usable (outer-rect (current-policy)
                                                              (current-output))))
                                      (check (null (c:rect-intersect drawn strip))
                                             "and it does not touch the echo ~
                                              area's strip: map ~d..~d, status ~
                                              line ~d..~d"
                                             (c:rect-y drawn) (c:rect-bottom drawn)
                                             (c:rect-y strip) (c:rect-bottom strip))
                                      (check (and (= (c:rect-x drawn) (c:rect-x usable))
                                                  (= (c:rect-y drawn) (c:rect-y usable))
                                                  (= (c:rect-w drawn) (c:rect-w usable))
                                                  (= (c:rect-h drawn) (c:rect-h usable)))
                                             "because it is exactly the area the ~
                                              cells were laid out in")))))
                           (setf (late-value "lattice:*map-threshold*") threshold)
                           (wm (lambda () (relayout :force t)))
                           (settle))))
                  (setf (late-value "lattice:*zoom-mode*") mode
                        (late-value "lattice:*cell-width*") width
                        (late-value "lattice:*cell-height*") height)
                  (wm (lambda () (late-call "lattice:disable")))
                  (settle)))))))

       ;; ------------------------------------------------------------------
       ;; NOTHING IN THIS FILE HAD EVER CLOSED A WINDOW.  Every client it
       ;; launches sits still until the teardown terminates it, so the whole
       ;; removal path -- DETACH-WINDOW, the tree repair through the policy,
       ;; the focus that has to land somewhere -- ran in the unit suite against
       ;; constructed state and never once against a compositor.  The hook
       ;; ledger is what said so: :WINDOW-CLOSED is not a hook a headless
       ;; backend is unable to fire, it is one nothing had asked to fire.
       ;;
       ;; And `close' is the request FINDINGS records being refused on every
       ;; machine, silently, for a week: it is window-management state, it was
       ;; sent from an event handler, and river ignored it.  Super+q did
       ;; nothing at all until CLOSE-WINDOW-LATER put it in a sequence.
       (section "a window going away"
         (let ((window (open-window "latticewm-closing")))
           (cond
             ((null window)
              (missing "no terminal emulator on PATH, so no window to close"))
             (t
              (check (stand-on window) "the cursor is on the window to be closed")
              ;; WHAT THIS PROGRAM OWNS, AND WHERE THAT STOPS.  The close is a
              ;; window-management request, so the assertion is that it is
              ;; queued and that a sequence drains it -- which is the whole of
              ;; the bug FINDINGS records.  Whether the client then goes is the
              ;; *client's* decision: the protocol says in as many words that
              ;; "the window may ignore this request or only close after some
              ;; delay", and a check that waited for foot to agree would be
              ;; this project asserting on somebody else's program.  So the
              ;; window is made to go the commonest way a window goes -- the
              ;; application exits -- and what is asserted below is our own
              ;; removal path.
              (check (wm (lambda () (close-window-later window)))
                     "a close is queued for a manage sequence, which is the ~
                      only place river accepts one")
              (settle)
              (check (null (r::server-pending-closes *server*))
                     "and the sequence drained it, so the request was sent ~
                      rather than refused")
              (let ((process (first *clients*)))
                (when process (ignore-errors (uiop:terminate-process process :urgent t))))
              (check (poll-until (lambda () (not (c:window-live-p window))) 10)
                     "the client exits, river says `closed', and the window ~
                      stops being live")
              (check (null (c:leaf-holding (c:world-root *world*) window))
                     "the tree no longer holds it")
              (let ((firings (fired :window-closed)))
                (check (find window firings :key #'first)
                       ":window-closed ran, carrying the window that went")
                (check (every (lambda (given)
                                (and (= 1 (length given))
                                     (typep (first given) 'window)))
                              firings)
                       "with the one argument it declares, every time"))
              ;; The focus half, which is the regression this commit is for:
              ;; ON-WINDOW-CLOSE wrote WORLD-CURSOR directly and announced
              ;; nothing, so the commonest focus change in the program was
              ;; invisible to the hook documented for status bars.
              (check (fired :focus-changed)
                     "and the focus that had to move announced itself")))))

       ;; ------------------------------------------------------------------
       ;; SHUTDOWN RUNS ONCE.  QUIT and RESTART-WM each ran the hooks and saved
       ;; the layout, and START's UNWIND-PROTECT did both again on the way out,
       ;; so every exit ran every shutdown hook twice.  Harmless for the shipped
       ;; hooks; not harmless for a hook that flushes a file or posts a
       ;; notification, which is what a shutdown hook is for.
       (section "shutting down"
         (let* ((runs 0)
                ;; Bound once: REMOVE-HOOK compares with EQ, and (FUNCTION F)
                ;; on an FLET is not required to return the same object twice.
                (counter (lambda () (incf runs))))
           (add-hook :shutdown counter)
           (unwind-protect
                (progn
                  (wm (lambda () (run-shutdown-once)))
                  (wm (lambda () (run-shutdown-once)))
                  (check (= 1 runs)
                         "the shutdown hooks run once however many times ~
                          shutdown is asked for (~d run~:p)" runs))
             (remove-hook :shutdown counter)))
         (check (probe-file (state-file))
                "and the layout was written on the way out")
         (wm (lambda () (quit)))
         ;; THE THREAD, NOT THE SERVER OBJECT.  This asked whether
         ;; (SERVER-RUNNING *SERVER*) had gone false, and START's UNWIND-PROTECT
         ;; sets *SERVER* to NIL two lines after the loop it ends — so on a
         ;; quick shutdown the predicate was reading a slot off NIL, POLL-UNTIL
         ;; swallowed the type error, and the suite reported that QUIT had not
         ;; ended the session.  It failed *because* the shutdown was fast.
         ;;
         ;; The session ending is START returning, and START returning is this
         ;; thread finishing.  That fact is monotonic and survives the teardown,
         ;; which is what an assertion about a shutdown has to be made of.
         (check (poll-until (lambda ()
                              (not (sb-thread:thread-alive-p *wm-under-test*)))
                            10)
                "and QUIT ends the session"))

       ;; ------------------------------------------------------------------
       ;; LAST, because it reads the whole run.  Every declared hook has had a
       ;; recorder on it since before START was called; this asks what each one
       ;; was handed and when, which is the half of a hook's contract that no
       ;; gate, no document and no unit test can reach.
       (section "the hooks, as a consumer sees them"
         (let ((declared (all-hooks))
               (seen 0))
           (dolist (row declared)
             (destructuring-bind (name documentation attached arguments) row
               (declare (ignore documentation attached))
               (let ((firings (fired name)))
                 (when firings
                   (incf seen)
                   ;; THE DECLARATION, CHECKED AGAINST THE CALL SITE THAT RAN.
                   ;; The compiler macro checks every literal RUN-HOOKS at
                   ;; build time; this checks the ones that actually fired, in
                   ;; the program, against the same declaration.
                   (check (every (lambda (given) (= (length given) (length arguments)))
                                 firings)
                          "~s fired ~d time~:p with the ~d argument~:p it declares"
                          name (length firings) (length arguments))))))
           (check (plusp seen) "~d of ~d declared hooks fired during this run"
                  seen (length declared))
           ;; AND THE ONES THAT DID NOT, which is the half that would have
           ;; caught the original state of this mechanism.  A hook that neither
           ;; fired nor is named as one a headless backend cannot produce is
           ;; a hook nothing in the project has ever executed.
           (dolist (row declared)
             (let ((name (first row)))
               (unless (or (fired name) (member name +cannot-fire-headless+))
                 (check nil "~s never fired, and is not one a headless backend ~
                             is unable to fire.  Drive it, or say here why it ~
                             cannot be driven" name))))
           (dolist (name +cannot-fire-headless+)
             (unless (member name declared :key #'first)
               (check nil "~s is excused from firing and is not a declared ~
                           hook -- the excuse outlived the thing" name)))))

       (section "what the hooks were handed"
         ;; :STARTUP RAN BEFORE THE CONNECTION EXISTED, and said in its own
         ;; docstring that it ran after.  A hook nothing attaches to can say
         ;; anything about itself.
         (check *startup-saw* ":startup ran")
         (when *startup-saw*
           (check (getf *startup-saw* :server)
                  "with the compositor connection up, which is what it promises")
           (check (getf *startup-saw* :manager)
                  "and river_window_manager_v1 bound, so a hook can act on the ~
                   session rather than on an empty world"))

         ;; :OUTPUT-ADDED RAN AT THE MOMENT THE OBJECT WAS MADE, before the
         ;; position, the size and the wl_output join that carries the name and
         ;; the scale.  Read here at the moment of firing, not afterwards,
         ;; because afterwards they have all arrived either way.
         (check *outputs-as-announced* ":output-added ran for the headless output")
         (dolist (row *outputs-as-announced*)
           (destructuring-bind (name width height scale) row
             (check (and name (plusp (length name)))
                    "and the monitor already had its name: ~a" (or name "(none)"))
             (check (and (plusp width) (plusp height))
                    "and its size: ~dx~d" width height)
             (check (and (realp scale) (plusp scale))
                    "and its scale: ~a" scale)))

         ;; :KEYBOARD-FOCUS-CHANGED did not exist.  :FOCUS-CHANGED reports the
         ;; cursor, and the cursor is a place -- focusing a float does not move
         ;; it, so the only focus hook there was could not tell a status bar
         ;; which window was about to receive the keystrokes.
         (let ((firings (fired :keyboard-focus-changed)))
           (check firings ":keyboard-focus-changed ran ~d time~:p" (length firings))
           (check (every (lambda (given)
                           (destructuring-bind (old new) given
                             (and (or (null old) (typep old 'window))
                                  (or (null new) (typep new 'window)))))
                         firings)
                  "with windows or NIL, NIL being the honest answer for an ~
                   empty pane rather than a missing value")
           (check (notany (lambda (given) (eq (first given) (second given))) firings)
                  "and never with the same window twice, because it fires off ~
                   the diff the runtime already had"))

         (let ((firings (fired :focus-changed)))
           (check firings ":focus-changed ran ~d time~:p" (length firings))
           (check (every (lambda (given) (every #'listp given)) firings)
                  "with two paths, which is what a place is"))

         ;; The seam a status bar, a notification popup or a minimap attaches
         ;; to, and the one hook in the system that four things used and
         ;; nothing documented until it was declared.
         (check (fired :draw-overlays)
                ":draw-overlays ran inside the render sequences, ~d time~:p"
                (length (fired :draw-overlays)))
         (check (fired :reserve-space)
                ":reserve-space was asked how much of the screen to keep ~
                 clear, ~d time~:p" (length (fired :reserve-space)))

         ;; The hooks a headless backend structurally cannot fire, reported one
         ;; by one, because "five did not fire" is a sentence somebody has to
         ;; go and decode.  Not fatal, and never made fatal: making them so
         ;; would only teach people to unset the variable.
         (dolist (name +cannot-fire-headless+)
           (unless (fired name)
             (skip "~s: nothing here can produce it -- no input devices, no ~
                    unpluggable monitor, no physical pointer.  Watched in ~
                    tools/hardware-check.lisp instead" name)))))

  ;; --- teardown ---------------------------------------------------------
  (dolist (process *clients*)
    (ignore-errors (uiop:terminate-process process :urgent t)))
  (ignore-errors (stop-ipc-server))
  (when *river* (ignore-errors (uiop:terminate-process *river* :urgent t)))
  (ignore-errors (delete-file *display-file*))
  (when *display* (ignore-errors (delete-file (socket-path))))
  (when *scratch* (ignore-errors (uiop:delete-directory-tree
                                  (uiop:ensure-directory-pathname *scratch*)
                                  :validate t))))

;;; ================================================================ the report

(format t "~&~%======== INTEGRATION ========~%")
(format t "~d check~:p in ~d section~:p~%" *checks* (length *sections*))
(dolist (entry (reverse *sections*))
  (format t "  ~3d  ~a~%" (cdr entry) (car entry)))

(when *skipped*
  (format t "~%~d thing~:p a headless backend cannot have:~%" (length *skipped*))
  (format t "~{  skip  ~a~%~}" (reverse *skipped*)))

(when *missing*
  (format t "~%~d dependenc~:@p that should have been installed:~%" (length *missing*))
  (format t "~{  ~a~%~}" (reverse *missing*)))

(when *failures*
  (format t "~%~d failure~:p:~%" (length *failures*))
  (format t "~{  FAIL  ~a~%~}" (reverse *failures*)))

;;; A GREEN RUN THAT VERIFIED NOTHING IS WORSE THAN A RED ONE.  Under
;;; LATTICEWM_REQUIRE_INTEGRATION a missing dependency is a failure, because the
;;; default state of a CI container is `no terminal emulator on PATH' and the
;;; sections that need one are most of this file.
(let ((fatal (or *failures* (and (strict-p) *missing*))))
  (format t "~%~:[PASS~;FAIL~]~@[  (~a)~]~%"
          fatal
          (cond ((null fatal) nil)
                (*failures* nil)
                (t "dependencies missing under LATTICEWM_REQUIRE_INTEGRATION")))
  (sb-ext:quit :unix-status (if fatal 1 0)))
