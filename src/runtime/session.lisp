;;;; runtime/session.lisp --- Connecting, and binding the globals.
;;;;
;;;; WHAT IS LEFT HERE after the split, and it is now one responsibility: how
;;;; this program becomes a client of a running compositor.  Everything that
;;;; used to share the file has its own:
;;;;
;;;;   runtime/sequence.lisp   the manage/render loop and the watchdog
;;;;   runtime/outputs.lisp    monitors, including hotplug
;;;;   runtime/seats.lisp      keyboards, bindings, and reading a key
;;;;   runtime/swank.lisp      the REPL server and the cross-thread queue
;;;;   runtime/pointer.lisp    managing windows with the pointer
;;;;   runtime/layer.lisp      panels, bars and screen lockers
;;;;
;;;; The sequence runners are the most delicate code in the program and they
;;;; were in the middle of a 690-line file they shared with a REPL server, a
;;;; keyboard capture table and the output registry.  Six reasons to change one
;;;; file is six reasons to break the other five.
;;;;
;;;; PROTOCOL VERSION IS CHECKED AT BIND TIME AND WE REFUSE TO START ON A
;;;; MISMATCH.  river-window-management-v1 is young and a pre-release protocol
;;;; can break within a version number rather than politely bumping to v2.
;;;; PLAN.org calls that the largest single threat to the "survives without AI"
;;;; requirement — after the budget expires, a river upgrade could break the
;;;; window manager with nobody available to repair it.  A clear refusal naming
;;;; both version numbers is repairable by a non-programmer.  Silent
;;;; misbehaviour is not.

(in-package #:latticewm/runtime)

(defparameter +window-management-version+ 4
  "The river_window_manager_v1 version this build was generated against.

Bumping this means regenerating from a new XML and re-running the codegen
count check.  It is deliberately not `whatever the compositor offers'.")

(defparameter +xkb-bindings-version+ 3
  "The river_xkb_bindings_v1 version this build was generated against.")

(define-condition protocol-version-mismatch (error)
  ((interface :initarg :interface :reader mismatch-interface)
   (wanted :initarg :wanted :reader mismatch-wanted)
   (offered :initarg :offered :reader mismatch-offered))
  (:report
   (lambda (condition stream)
     (format stream
             "~&This build of LatticeWM was generated against ~a version ~d,~%~
              but the running compositor offers version ~d.~%~%~
              Refusing to start rather than misbehave: the two versions may~%~
              disagree about what a request means.~%~%~
              To fix this, re-vendor the protocol XML from the river you are~%~
              running, then rebuild:~%~%~
              ~2tcp $RIVER/share/river-protocols/stable/*.xml src/protocol/~%~
              ~2t# set +WINDOW-MANAGEMENT-VERSION+ in src/runtime/session.lisp~%~
              ~2tmake~%"
             (mismatch-interface condition) (mismatch-wanted condition)
             (mismatch-offered condition))))
  (:documentation
   "Signalled at startup when the compositor's protocol version is not the one
we generated bindings from."))

;;; ------------------------------------------------------------ connecting

(defun dispatch-one-event (display)
  "Dispatch a single event, surviving anything wrong with it.

Two things go wrong here in normal operation and neither should be fatal:

  * an event for an object we already destroyed — the race libwayland has
    zombie proxies for and wayflan does not;
  * an enum value we do not know.  wl_shm advertises every pixel format the
    GPU supports as DRM fourcc codes, and a binding generated from a protocol
    XML knows only the handful the XML names.  *This killed the window manager
    on startup the first time an shm global was bound*, from an event nobody
    reads, about formats nobody asked for.

The general rule they share: a compositor newer than our bindings must be able
to say things we do not understand without taking the session down.  Skipping
one event is always safe, because wayflan drains the whole message body into a
separate buffer before invoking any handler — so the stream is already at the
next message when this returns."
  (handler-case (wl:wl-display-dispatch-event display)
    (wl:wl-message-error (condition)
      (logmsg :debug "stale event ignored: ~a" condition))
    (wl:wl-server-error (condition) (error condition))
    (end-of-file (condition) (error condition))
    (error (condition)
      (logmsg :debug "undecodable event ignored: ~a" condition))))

(defun safe-roundtrip (display)
  "WL-DISPLAY-ROUNDTRIP, tolerating events we cannot decode.

The roundtrip in BIND-GLOBALS dispatches everything the compositor volunteers
about the globals we just bound — including wl_shm's list of every pixel
format the GPU supports, which a generated binding does not know.  Doing this
by hand rather than calling the library's roundtrip is the price of surviving
that."
  (let ((done nil))
    (let ((callback (wl:wl-display.sync display)))
      (push (lambda (event &rest arguments)
              (declare (ignore arguments))
              (when (eq event :done) (setf done t)))
            (wl:wl-proxy-hooks callback))
      (loop until done
            do (dispatch-one-event display)))))

(defun bind-globals (server)
  "Bind the river globals we need, refusing to start on a version mismatch."
  (let ((display (server-display server))
        (found '()))
    (let ((registry (wl:wl-display.get-registry display)))
      (setf (server-registry server) registry)
      (push (wl:evlambda
              (:global (name interface version)
               (push (list name interface version) found)))
            (wl:wl-proxy-hooks registry))
      (safe-roundtrip display)
      (dolist (entry (nreverse found))
        (destructuring-bind (name interface version) entry
          (cond
            ((string= interface "river_window_manager_v1")
             (unless (= version +window-management-version+)
               (error 'protocol-version-mismatch
                      :interface interface :wanted +window-management-version+
                      :offered version))
             (setf (server-version server) version
                   (server-manager server)
                   (wl:wl-registry.bind registry name
                                        'river:river-window-manager-v1 version)))
            ((string= interface "river_xkb_bindings_v1")
             (if (= version +xkb-bindings-version+)
                 (setf (server-bindings server)
                       (wl:wl-registry.bind registry name
                                            'river:river-xkb-bindings-v1 version))
                 (logmsg :warn "river_xkb_bindings_v1 is version ~d, we want ~d; ~
                                keybindings disabled"
                         version +xkb-bindings-version+)))
            ((string= interface "wl_compositor")
             (setf (server-compositor server)
                   (wl:wl-registry.bind registry name 'wl:wl-compositor
                                        (min version 4))))
            ((string= interface "wl_output")
             ;; Bound for the two facts river_output_v1 does not carry: the
             ;; human-readable name, and the scale factor.  wl_output.name
             ;; arrived in version 4; below that there is no name to be had and
             ;; the output stays anonymous, which is degraded rather than
             ;; broken.  Scale has been there since version 2, so the bind
             ;; takes whatever is on offer down to 2 rather than refusing.
             (when (>= version 2)
               (let ((id name)
                     (proxy (wl:wl-registry.bind registry name 'wl:wl-output
                                                 (min version 4))))
                 (on-events (proxy "wl_output")
                   (:name
                    (setf (gethash id (server-output-names server))
                          (first arguments))
                    (name-outputs))
                   (:scale
                    (setf (gethash id (server-output-scales server))
                          (max 1 (or (first arguments) 1)))
                    (dolist (output (c:world-outputs *world*))
                      (adopt-wl-output output)))
                   (t nil)))))
            ((string= interface "wl_shm")
             (setf (server-shm server)
                   (wl:wl-registry.bind registry name 'wl:wl-shm 1)))
            ((string= interface "river_layer_shell_v1")
             (setf (server-layer-shell server)
                   (wl:wl-registry.bind registry name
                                        'river:river-layer-shell-v1 version))))))
      (unless (server-manager server)
        (error "This compositor does not offer river_window_manager_v1.~%~
                LatticeWM is a window manager *for river*, and needs river~%~
                0.4 or later.  Are you running it inside the right~%~
                compositor?  WAYLAND_DISPLAY is ~s."
               (uiop:getenv "WAYLAND_DISPLAY")))
      (logmsg :info "bound river_window_manager_v1 v~d" (server-version server))
      server)))

(defun attach-manager-hooks (server)
  "Listen to the window manager global: the whole protocol arrives here."
  (on-events ((server-manager server) "river_window_manager_v1")
         (:window (attach-window (first arguments)))
         (:output (attach-output (first arguments)))
         (:seat (attach-seat (first arguments)))
         (:manage-start (run-manage-sequence))
         (:render-start (run-render-sequence))
         (:session-locked (setf (c:prop *world* :locked) t))
         (:session-unlocked (setf (c:prop *world* :locked) nil))
         (:finished (setf (server-running server) nil))
         (t (logmsg :debug "manager event ~s ~s" event arguments)))
  server)
