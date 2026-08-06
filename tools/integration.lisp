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
  "Poll PREDICATE until it answers or SECONDS elapse.  Returns its answer."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
      (let ((answer (ignore-errors (funcall predicate))))
        (when answer (return answer)))
      (when (> (get-internal-real-time) deadline) (return nil))
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
  (sb-thread:make-thread
   (lambda ()
     (ignore-errors (start :swank-port nil :config nil :restore nil)))
   :name "latticewm-under-test"))

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
         (check (poll-until (lambda () (not (server-running *server*))) 10)
                "and QUIT ends the session")))

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
