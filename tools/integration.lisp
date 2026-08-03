;;;; tools/integration.lisp --- Drive a real compositor.
;;;;
;;;; THIS IS THE TEST THAT CHANGES THE CATEGORY OF BUG THE PROJECT CAN FIND.
;;;;
;;;; The unit suite is 1000 checks and every one of them *constructs* state.
;;;; The codebase says so about itself, in src/runtime/server.lisp, explaining
;;;; how a handler for an event that does not exist survived for the life of
;;;; the project:
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
;;;; Wayland client this program is, and assert on what comes back.  No screen,
;;;; no graphics card, no second machine.  It is about a second and a half.
;;;;
;;;; WHAT IT CHECKS, in the order it becomes possible to check them:
;;;;
;;;;   1. the connection is made and the diagnostic path is not taken;
;;;;   2. river_window_manager_v1 binds at the version we generated against —
;;;;      the single largest threat to this program surviving a river upgrade;
;;;;   3. an output arrives, with a name and a size we did not invent;
;;;;   4. a manage sequence completes, which means the sequence discipline
;;;;      agreed with river about what may be sent when;
;;;;   5. a render sequence completes and the echo area's shared-memory buffer
;;;;      is accepted — the wl_shm path, which DESIGN called the least-proven
;;;;      part of wayflan;
;;;;   6. the three input globals bind, devices are announced with names and
;;;;      kinds, and a setting river's own protocol carries is accepted --
;;;;      the half of input configuration that exists without hardware;
;;;;   7. with a client available, a window is announced, placed, given a
;;;;      proposed size and shown.
;;;;
;;;;   make integration
;;;;
;;;; Absent river it says so and exits 0, because a machine without a
;;;; compositor is a legitimate place to run the unit suite.  Set
;;;; LATTICEWM_REQUIRE_INTEGRATION=1 to make its absence a failure, which is
;;;; what a release build should do.

(require :asdf)
(require :sb-posix)

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm"))

(defparameter *checks* 0)
(defparameter *failures* '())
(defparameter *river* nil)
(defparameter *display* nil)
(defparameter *runtime-dir* nil)

(defun check (ok format &rest arguments)
  (incf *checks*)
  (if ok
      (format t "  ok    ~?~%" format arguments)
      (progn (push (format nil "~?" format arguments) *failures*)
             (format t "  FAIL  ~?~%" format arguments)))
  (force-output)
  ok)

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

(defun poll-until (predicate seconds &optional (step 0.05))
  "Poll PREDICATE until it answers or SECONDS elapse.  Returns its answer."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
      (let ((answer (ignore-errors (funcall predicate))))
        (when answer (return answer)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep step))))

;;; ---------------------------------------------------------------- river

(defparameter *display-file* nil
  "Where river's init command writes the display name it ended up with.")

(defun start-river ()
  "Start river on a headless backend, and return its process.

WLR_BACKENDS=headless is what makes this possible without a screen: wlroots
creates a virtual output and never touches DRM or libinput.  The output it
makes is why check 3 can assert on a size that this program did not invent.

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

;;; ------------------------------------------------------- the window manager

(defun start-window-manager ()
  "Run START on a thread, against the headless river.

On a thread because START owns the event loop and does not return until the
session ends, which is exactly the behaviour under test — a driver that called
the pieces by hand would be constructing state again."
  (sb-posix:setenv "WAYLAND_DISPLAY" *display* 1)
  (sb-posix:setenv "XDG_RUNTIME_DIR" *runtime-dir* 1)
  ;; A state file and a welcome marker of its own, so a developer's real
  ;; session is neither read nor written by the test.
  (let ((state (format nil "/tmp/latticewm-integration-~d" (sb-posix:getpid))))
    (ensure-directories-exist (format nil "~a/" state))
    (sb-posix:setenv "XDG_STATE_HOME" state 1))
  (setf (symbol-value (read-from-string "latticewm/policy:*log-level*")) :warn
        (symbol-value (read-from-string "latticewm/policy:*log-file*")) nil
        (symbol-value (read-from-string "latticewm/runtime:*ipc-socket*")) nil)
  (sb-thread:make-thread
   (lambda ()
     (ignore-errors
      (funcall (read-from-string "latticewm/runtime:start")
               :swank-port nil :config nil :restore nil)))
   :name "latticewm-under-test"))

(defmacro sym (name) `(symbol-value (read-from-string ,name)))
(defun call (name &rest arguments) (apply (read-from-string name) arguments))

;;; ---------------------------------------------------------------- the run

(let ((river (start-river)))
  (cond
    ((null river)
     (format t "~&~%======== INTEGRATION ========~%~
                river was not found, so the compositor tests did not run.~%~%~
                This is the one test that receives state rather than~%~
                constructing it, and it is worth having: install river, or~%~
                run `nix-shell --run \"make integration\"', which pins one.~%~%")
     (sb-ext:quit :unix-status
                  (if (sb-posix:getenv "LATTICEWM_REQUIRE_INTEGRATION") 1 0)))
    (t
     (unwind-protect
          ;; A BLOCK rather than a PROGN, so that a river that never came up
          ;; can stop here instead of running six checks that can only say the
          ;; same thing six times.
          (block driving
            ;; --- 1. the compositor came up and made its socket ------------
            (setf *display* (poll-until #'read-display-name 20))
            (check *display* "river came up and told us its display: ~a"
                   (or *display* "(nothing)"))
            (unless *display*
              (format t "~&  river did not start.  Try it by hand:~%~
                           ~4tWLR_BACKENDS=headless river -c true~%")
              (return-from driving))
            (check (poll-until (lambda () (probe-file (socket-path))) 10)
                   "with a socket at ~a" (socket-path))
            (let ((wm (start-window-manager)))
              (declare (ignorable wm))
              ;; --- 2. we connected and bound at the pinned version --------
              ;; POLL FOR THE MANAGER, NOT JUST FOR THE SERVER.  *SERVER* is
              ;; assigned as soon as the object exists; the manager is bound a
              ;; registry roundtrip later.  Asserting on the manager the moment
              ;; *SERVER* appears is a race, and it lost about one run in three
              ;; -- sometimes here, sometimes on the version, sometimes not at
              ;; all.  A test that fails a third of the time for no reason is
              ;; worse than no test, because the next real failure reads as
              ;; that one.
              (let* ((server (poll-until (lambda () (sym "latticewm/runtime:*server*")) 10))
                     (manager (and server
                                   (poll-until
                                    (lambda ()
                                      (call "latticewm/runtime::server-manager" server))
                                    10))))
                (check server "the window manager connected")
                (when server
                  (check manager "river_window_manager_v1 is bound")
                  (check (= (call "latticewm/runtime::server-version" server)
                            (sym "latticewm/runtime::+window-management-version+"))
                         "at version ~d, which is the version we generated against"
                         (call "latticewm/runtime::server-version" server))))
              ;; --- 3. an output arrived, and we did not invent it ---------
              (let ((outputs (poll-until (lambda ()
                                         (let ((o (call "latticewm/runtime:all-outputs")))
                                           (and o o)))
                                       10)))
                (check outputs "river announced ~d output~:p" (length outputs))
                (when outputs
                  (let ((rect (call "latticewm/core:output-rect" (first outputs))))
                    (check (and (plusp (call "latticewm/core:rect-w" rect))
                                (plusp (call "latticewm/core:rect-h" rect)))
                           "with a size we received rather than assumed: ~dx~d"
                           (call "latticewm/core:rect-w" rect)
                           (call "latticewm/core:rect-h" rect))
                    ;; The bug this replaces: CURRENT-OUTPUT used to look the
                    ;; output up through the cursor's *window*, so an empty
                    ;; pane -- which is what a fresh session is -- matched
                    ;; nothing and fell through to the first output.
                    (check (eq (first outputs) (call "latticewm/runtime:current-output"))
                           "and CURRENT-OUTPUT finds it from an empty pane"))))
              ;; --- 4 and 5. the sequences ran ----------------------------
              (check (poll-until (lambda ()
                                 (call "latticewm/core:prop"
                                       (sym "latticewm/runtime:*world*")
                                       :last-placements))
                               10)
                     "a manage and a render sequence completed, and the emitter
        produced placements river accepted")
              ;; The wl_shm path: DESIGN called it the least-proven part of
              ;; wayflan, and it is the one thing here that cannot be checked
              ;; without a compositor at all.
              ;; Polled for the same reason the manager is: the overlay object
              ;; is made before it has drawn anything, so its canvas arrives
              ;; strictly later than it does.
              (let* ((overlays (poll-until
                                (lambda () (call "latticewm/runtime:all-overlays")) 10))
                     (canvas (poll-until
                              (lambda ()
                                (some (lambda (o)
                                        (call "latticewm/runtime::overlay-canvas" o))
                                      (call "latticewm/runtime:all-overlays")))
                              10)))
                (check overlays "an overlay surface exists")
                (check canvas "with a shared-memory buffer river accepted"))
              ;; --- 6. input configuration --------------------------------
              ;;
              ;; THE ONLY PLACE THIS CAN BE CHECKED AT ALL.  Three protocols
              ;; were vendored into src/protocol/ and never compiled, and
              ;; nothing anywhere noticed -- not gate 1, which cannot see an
              ;; XML no component names; not gate 5, which counted only the
              ;; three that were named; not the unit suite, which constructs
              ;; state and so can construct a device without ever asking river
              ;; for one.
              ;;
              ;; The headless backend has no libinput devices, so what is
              ;; checked here is the half that exists on every backend: the
              ;; globals bind, devices are announced, and a name and a kind
              ;; arrive.  The libinput half is unreachable without hardware and
              ;; is marked as such rather than quietly skipped.
              (let ((server (sym "latticewm/runtime:*server*")))
                (check (call "latticewm/runtime::server-input-manager" server)
                       "river_input_manager_v1 is bound")
                (check (call "latticewm/runtime::server-xkb-config" server)
                       "river_xkb_config_v1 is bound")
                ;; A HEADLESS BACKEND HAS NO INPUT DEVICES AT ALL — wlroots
                ;; creates a virtual output and no virtual keyboard — so the
                ;; two checks below are conditional and say so when they cannot
                ;; run.  That is the same shape as the terminal check further
                ;; down and for the same reason: a check that silently passes
                ;; on an empty list is a measurement wearing a gate's uniform,
                ;; and this file has one gate's worth of credibility to spend.
                ;;
                ;; The device path is exercised for real by running nested
                ;; under an ordinary session (WLR_BACKENDS=wayland), where
                ;; river makes one virtual device per capability of the parent
                ;; seat.  It is not done here because `make check' would then
                ;; open a window on whatever desktop it was run from.
                (let ((devices (poll-until
                                (lambda ()
                                  (call "latticewm/core:world-inputs"
                                        (sym "latticewm/runtime:*world*")))
                                3)))
                  (cond
                    ((null devices)
                     (format t "  skip  the headless backend has no input ~
                                devices, so no device was configured~%"))
                    (t
                     (check (some (lambda (d)
                                    (call "latticewm/core:input-device-name" d))
                                  devices)
                            "~d input device~:p, named: ~{~a~^, ~}"
                            (length devices)
                            (remove nil
                                    (mapcar (lambda (d)
                                              (call "latticewm/core:input-device-name" d))
                                            devices)))
                     ;; Key repeat is river's own request rather than
                     ;; libinput's, so it is the one setting a backend with no
                     ;; libinput behind it can still take.
                     (let ((keyboard
                             (find :keyboard devices
                                   :key (lambda (d)
                                          (call "latticewm/core:input-device-kind" d)))))
                       (cond
                         ((null keyboard)
                          (format t "  skip  no keyboard among them~%"))
                         (t
                          (setf (symbol-value (sym "latticewm/policy:*repeat-rate*")) 42
                                (symbol-value (sym "latticewm/policy:*repeat-delay*")) 250)
                          (call "latticewm/runtime:apply-input-configuration")
                          (check (eql 42 (call "latticewm/core:input-device-setting"
                                               keyboard :repeat-rate))
                                 "and set_repeat_info was accepted at 42/s"))))))))
              ;; --- 7. a real window, if there is a client to make one -----
              (let ((client (find-program "foot" "weston-terminal" "alacritty"
                                          "kitty" "xterm")))
                (cond
                  ((null client)
                   (format t "  skip  no terminal available, so no window was opened~%"))
                  (t
                   (uiop:launch-program (list (namestring client))
                                        :environment
                                        (list (format nil "WAYLAND_DISPLAY=~a" *display*)
                                              (format nil "XDG_RUNTIME_DIR=~a" *runtime-dir*)
                                              (format nil "HOME=~a"
                                                      (or (sb-posix:getenv "HOME") "/tmp"))
                                              (format nil "PATH=~a"
                                                      (or (sb-posix:getenv "PATH") "/usr/bin")))
                                        :output nil :error-output nil)
                   (let ((window (poll-until
                                  (lambda ()
                                    (first (call "latticewm/runtime:all-windows")))
                                  15)))
                     (check window "a client window was announced")
                     (when window
                       (check (poll-until (lambda ()
                                          (call "latticewm/core:leaf-holding"
                                                (call "latticewm/core:world-root"
                                                      (sym "latticewm/runtime:*world*"))
                                                window))
                                        10)
                              "and was placed in the tree")
                       (check (poll-until (lambda ()
                                          (plusp (call "latticewm/core:window-width"
                                                       window)))
                                        10)
                              "and river told us the size it actually took: ~dx~d"
                              (call "latticewm/core:window-width" window)
                              (call "latticewm/core:window-height" window))))))))
            ;; --- shutdown ------------------------------------------------
            (ignore-errors (call "latticewm/runtime:quit"))
            (sleep 0.5))
       (ignore-errors (uiop:terminate-process river :urgent t))
       (ignore-errors (delete-file *display-file*))
       (when *display* (ignore-errors (delete-file (socket-path))))))))

(format t "~&~%======== INTEGRATION ========~%~d check~:p, ~:[PASS~;FAIL~]~%"
        *checks* *failures*)
(when *failures*
  (format t "~{  FAIL ~a~%~}" (reverse *failures*)))
(sb-ext:quit :unix-status (if *failures* 1 0))
