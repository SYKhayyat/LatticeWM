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
;;;; PROTOCOL VERSION IS A FLOOR AND A CEILING, NOT AN EQUALITY.  This was an
;;;; equality until it was noticed that the equality defends against the wrong
;;;; hazard.  The fear it was written for — stated in PLAN.org as the largest
;;;; single threat to the "survives without AI" requirement — is that
;;;; river-window-management-v1 is young, and a pre-release protocol can break
;;;; *within* a version number rather than politely bumping.  That fear is
;;;; sound.  The equality did not address it: a river that changes what a
;;;; request means while still advertising version 5 passes an equality check
;;;; and misbehaves anyway.  What equality caught was the announced, polite
;;;; case — a version *bump* — which is the one case the Wayland object model
;;;; already handles safely, and it caught it by refusing to start.
;;;;
;;;; The cost of that was paid by every user whose distribution ships a river
;;;; half a release ahead of this one: a login screen that refuses, over a
;;;; protocol change that was purely additive.  See PINNED for 4 -> 5, which is
;;;; the only evidence anyone has about how river actually moves, and which is
;;;; two new events and five clarifications of prose.
;;;;
;;;; So: refuse below the FLOOR, bind at (MIN OFFERED CEILING), and let
;;;; anything above the ceiling be somebody else's feature until we vendor it.
;;;; A refusal naming both numbers is still repairable by a non-programmer,
;;;; and it now happens only when it is the truth.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------ ceilings and floors
;;;
;;; TWO NUMBERS PER PROTOCOL, AND THEY ARE NOT THE SAME NUMBER.
;;;
;;; The CEILING is the version the vendored XML declares, which is the version
;;; wayflan generated these bindings from.  We never bind above it.  Binding at
;;; version N is a *promise to river* that we can decode anything version N is
;;; allowed to send, and a promise we cannot keep is not answered by an error —
;;; it is answered by DISPATCH-ONE-EVENT logging "undecodable event ignored" at
;;; :DEBUG and carrying on, which is the silent misbehaviour this file exists to
;;; refuse.  Four of the six protocols below used to bind at whatever river
;;; offered, unclamped, which is exactly that bug pointed the other way.
;;;
;;; The FLOOR is the oldest version that can drive this build: the newest
;;; version that introduced a request we actually send.  Below it we would send
;;; a request the compositor has never heard of, and there is no degrading that
;;; — it is a protocol error and a dead session.
;;;
;;; Between the two we bind (MIN OFFERED CEILING), and the Wayland object model
;;; does the rest: river is *required* to speak the version we bound rather
;;; than the version it has grown to.  A river from next year therefore talks
;;; to this build in the dialect this build understands, and the only thing we
;;; lose is the features we have not vendored yet.  That is the trade, and it
;;; is a much better one than a login screen.
;;;
;;; THE FLOORS ARE READ OFF THE `since' ATTRIBUTES IN THE XML, not guessed.
;;; Gate 5 checks the ceilings against the same XML; the floors are argued in
;;; each docstring, because a floor is a claim about which requests this
;;; program sends and no gate can see that.

(defparameter +window-management-version+ 5
  "The river_window_manager_v1 version this build was generated against.

The ceiling.  Bumping it means re-vendoring the XML and re-running gate 5's
codegen counts.  It is deliberately not `whatever the compositor offers'.

WAS AN EQUALITY, AND IS NOW A CEILING WITH +WINDOW-MANAGEMENT-FLOOR+ under it.
River bumped this interface 4 -> 5 in the 0.4.6 *patch* release, which is the
pre-release-protocol hazard the file header names arriving exactly as
predicted.  See src/protocol/PINNED: every 4 -> 5 change was additive.")

(defparameter +window-management-floor+ 4
  "The oldest river_window_manager_v1 we can drive.  Below this we refuse.

FOUR, AND THE XML SAYS SO.  Of the requests this program sends, the newest to
be introduced are `exit_session', `set_dimension_bounds' and
`set_presentation_mode', all three since=\"4\".  Nothing we send is newer:
version 5 added no requests at all, only the two `capture_sessions' events.

Which is why one build serves river 0.4.5 and 0.4.6 alike.  On a 0.4.5 we bind
at 4, never receive `capture_sessions', and the recording indicator in
runtime/capture.lisp simply never lights — a feature absent rather than a
window manager that will not start.  That is the whole shape of absorbing a
protocol that moves: the ceiling rises when we vendor, the floor rises only
when we send something new, and users in between keep working.")

(defparameter +xkb-bindings-version+ 3
  "The river_xkb_bindings_v1 version this build was generated against.")

(defparameter +xkb-bindings-floor+ 3
  "The oldest river_xkb_bindings_v1 we accept.  Below it, keybindings are off.

THREE RATHER THAN TWO, AND THE REASON IS THE EXTENSION SURFACE.
`modifiers_watch' is since=\"3\", and this program does not call it — but it is
aliased in wire/wrappers.lisp and exported, so it belongs to anybody's config.
A floor of 2 would mean a user's own keymap could send a request their river
has never heard of, and the protocol error lands on them rather than on us.

Missing this global is already survivable: unlike the window manager it is not
refused, it disables keybindings and warns, which is what the branch below has
always done for a version it did not like.")

(defparameter +layer-shell-version+ 1
  "The river_layer_shell_v1 version this build was generated against.")

(defparameter +input-manager-version+ 2
  "The river_input_manager_v1 version this build was generated against.")

(defparameter +libinput-config-version+ 2
  "The river_libinput_config_v1 version this build was generated against.")

(defparameter +xkb-config-version+ 2
  "The river_xkb_config_v1 version this build was generated against.

These last four have no floor because none of them has a floor to have: every
request they carry has been there since version 1, and each protocol is
optional in the sense runtime/session.lisp's binding branch means it — absent,
the program loses a feature and says so.  All three input protocols went 1 -> 2
in river 0.4.6 by adding a `done' event, which this program does not listen for
in any case.  They are here to be *clamped*, which is the half that was
missing: they used to bind at whatever river offered.")

(defun negotiated-version (offered floor ceiling)
  "The version to bind a global with, or NIL if OFFERED is below FLOOR.

THE WHOLE VERSION POLICY, IN ONE FUNCTION WITH NO COMPOSITOR IN IT.  It is
three lines and it decides whether this program starts on anybody's machine,
which is a bad ratio of consequence to coverage — and inline in BIND-ONE-GLOBAL
it was untestable, because reaching it needs a live wl_registry.  Out here the
suite can ask it the five questions that matter (below the floor, at the floor,
between, at the ceiling, above it) by constructing three integers.

The two halves are not symmetric and that is the point.  Below the floor there
is no answer: we would be sending requests the compositor has never heard of,
and NIL means refuse.  Above the ceiling there is always an answer, because
Wayland obliges the compositor to speak the version we bind rather than the
newest it knows — so the clamp is not a compromise, it is the mechanism."
  (when (>= offered floor)
    (min offered ceiling)))

(define-condition protocol-version-too-old (error)
  ((interface :initarg :interface :reader mismatch-interface)
   (wanted :initarg :wanted :reader mismatch-wanted)
   (offered :initarg :offered :reader mismatch-offered))
  (:report
   (lambda (condition stream)
     (format stream
             "~&Your river is too old for this build of LatticeWM.~%~%~
              ~2tit offers ~a version ~d~%~
              ~2twe need   version ~d or newer~%~%~
              Refusing to start rather than misbehave: this build sends~%~
              requests that version ~d does not have, and there is no~%~
              degrading around a request the compositor never heard of.~%~%~
              THE FIX IS TO UPGRADE RIVER.  Any river from version ~d of this~%~
              interface upward will work, including ones newer than the one~%~
              this build was vendored against — src/protocol/PINNED names~%~
              that release, and it is a floor rather than an exact match.~%~%~
              Re-vendoring the protocol XML *downward* from your older river~%~
              is not a fix here and the build will not let you pretend it is:~%~
              the requests this program sends would stop existing, and it~%~
              fails to compile rather than failing at a login screen.~%"
             (mismatch-interface condition) (mismatch-offered condition)
             (mismatch-wanted condition) (mismatch-offered condition)
             (mismatch-wanted condition))))
  (:documentation
   "Signalled at startup when the compositor is older than our protocol floor.

There is deliberately no counterpart for a compositor that is *newer*.  Being
newer is the case the Wayland object model exists to make safe: we bind at our
own ceiling and river speaks that version to us.  A build that refused on
newer is a build that breaks every time a distribution moves first, which is
most of the time and for no gain."))

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

(defun bind-one-global (server name interface version)
  "Bind the global called INTERFACE, if it is one we want.  Returns T if bound.

Split out of BIND-GLOBALS so that the *same* code runs for a global present at
startup and one that appears later, which is not a tidiness argument: the
registry used to be read exactly once, and a monitor plugged in afterwards
therefore never had its wl_output bound.  Its river_output_v1 arrived, so the
window manager saw the monitor; its wl_output did not, so the monitor had no
name and was assumed to be scale 1 — a HiDPI screen docked at lunchtime drew
everything the window manager renders for itself at half size, and the
per-output workspace memory, which is keyed on the name, silently keyed on
NIL.  Docking a laptop is the case, and it is exactly the case a one-shot
registry cannot serve."
  (let ((registry (server-registry server)))
    (cond
      ;; SERVER-VERSION RECORDS WHAT WE BOUND, not what was offered, because
      ;; every downstream `is this feature available' test is asking what river
      ;; will actually send us — and against a river newer than this build
      ;; those two numbers differ.
      ((string= interface "river_window_manager_v1")
       (let ((bound (negotiated-version version +window-management-floor+
                                        +window-management-version+)))
         (unless bound
           (error 'protocol-version-too-old
                  :interface interface :wanted +window-management-floor+
                  :offered version))
         (setf (server-version server) bound
               (server-manager server)
               (wl:wl-registry.bind registry name
                                    'river:river-window-manager-v1 bound))
         (when (> version bound)
           (logmsg :info "river offers river_window_manager_v1 v~d; this ~
                          build speaks v~d and bound that.  Features added ~
                          after v~d are unavailable, nothing else changes."
                   version bound bound))))
      ((string= interface "river_xkb_bindings_v1")
       (let ((bound (negotiated-version version +xkb-bindings-floor+
                                        +xkb-bindings-version+)))
         (if bound
             (setf (server-bindings server)
                   (wl:wl-registry.bind registry name
                                        'river:river-xkb-bindings-v1 bound))
             (logmsg :warn "river_xkb_bindings_v1 is version ~d and we need ~d ~
                            or newer; keybindings disabled"
                     version +xkb-bindings-floor+))))
      ((string= interface "wl_compositor")
       (setf (server-compositor server)
             (wl:wl-registry.bind registry name 'wl:wl-compositor
                                  (min version 4))))
      ((string= interface "wl_output")
       ;; Bound for the two facts river_output_v1 does not carry: the
       ;; human-readable name, and the scale factor.  wl_output.name arrived in
       ;; version 4; below that there is no name to be had and the output stays
       ;; anonymous, which is degraded rather than broken.  Scale has been
       ;; there since version 2, so the bind takes whatever is on offer down to
       ;; 2 rather than refusing.
       (when (>= version 2)
         (let ((id name)
               (proxy (wl:wl-registry.bind registry name 'wl:wl-output
                                           (min version 4))))
           (setf (gethash id (server-wl-outputs server)) proxy)
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
                                  'river:river-layer-shell-v1
                                  (negotiated-version
                                   version 1 +layer-shell-version+))))
      ;; The three input globals.  Each is optional and each degrades to
      ;; exactly what the program did before it bound them: no manager means no
      ;; device list and no key repeat, no libinput config means no touchpad
      ;; settings, no xkb config means whatever layout river started with.
      ;;
      ;; ALL THREE ARE CLAMPED, and all three used to bind at whatever river
      ;; offered.  PINNED said of these that they "are bound at whatever
      ;; version is offered rather than checked for equality, so they degrade
      ;; to their old behaviour against an older river."  That was true and it
      ;; was only half the axis: against a *newer* river the same line promises
      ;; to decode events these bindings have never seen, and the promise is
      ;; broken quietly, one :DEBUG line at a time.
      ((string= interface "river_input_manager_v1")
       (setf (server-input-manager server)
             (attach-input-manager
              (wl:wl-registry.bind registry name
                                   'river:river-input-manager-v1
                                   (negotiated-version
                                    version 1 +input-manager-version+)))))
      ((string= interface "river_libinput_config_v1")
       (setf (server-libinput-config server)
             (attach-libinput-config
              (wl:wl-registry.bind registry name
                                   'river:river-libinput-config-v1
                                   (negotiated-version
                                    version 1 +libinput-config-version+)))))
      ((string= interface "river_xkb_config_v1")
       (setf (server-xkb-config server)
             (attach-xkb-config
              (wl:wl-registry.bind registry name
                                   'river:river-xkb-config-v1
                                   (negotiated-version
                                    version 1 +xkb-config-version+)))))
      (t (return-from bind-one-global nil)))
    t))

(defun forget-global (server id)
  "A global went away.  Drop anything we were keying on its numeric name.

WHY THIS IS NOT MERELY TIDY.  Wayland reuses global ids, so a stale entry is
not a leak that grows — it is a *wrong answer* waiting to happen: unplug the
external monitor and plug in a different one, the compositor reuses the id, and
the new screen inherits the old one's name and scale factor from these two
tables.  On a laptop that is docked and undocked twice a day the failure
arrives within the week, and it looks like the window manager forgetting which
monitor is which."
  (remhash id (server-output-names server))
  (remhash id (server-output-scales server))
  (let ((proxy (gethash id (server-wl-outputs server))))
    (when proxy
      (remhash id (server-wl-outputs server))
      (ignore-errors (wl:wl-output.release proxy))))
  nil)

(defun bind-globals (server)
  "Bind the river globals we need, refusing to start on a version mismatch.

The registry hook stays attached for the life of the connection rather than
being read once and dropped, so a global that appears later — which is what a
monitor being plugged in *is* — goes through exactly the same code."
  (let ((display (server-display server))
        (found '()))
    (let ((registry (wl:wl-display.get-registry display)))
      (setf (server-registry server) registry)
      (push (wl:evlambda
              (:global (name interface version)
               (push (list name interface version) found))
              (:global-remove (name) (forget-global server name)))
            (wl:wl-proxy-hooks registry))
      (safe-roundtrip display)
      (dolist (entry (nreverse found))
        (destructuring-bind (name interface version) entry
          (bind-one-global server name interface version)))
      (unless (server-manager server)
        (error "This compositor does not offer river_window_manager_v1.~%~
                LatticeWM is a window manager *for river*, and the protocol~%~
                through which river hands window management to an outside~%~
                program does not exist before river 0.4.  Are you running it~%~
                inside the right compositor?  WAYLAND_DISPLAY is ~s.~%~%~
                Note that having river is necessary and not sufficient:~%~
                it must offer version ~d of that interface or newer.  Newer~%~
                is fine and always will be; src/protocol/PINNED names the~%~
                release this build was generated against."
               (uiop:getenv "WAYLAND_DISPLAY") +window-management-floor+))
      ;; From here on, a global is bound the moment it is announced.  Replacing
      ;; the hook rather than adding one keeps the accumulating list from
      ;; growing for the life of the session, and there is exactly one reader.
      (setf (wl:wl-proxy-hooks registry)
            (list (wl:evlambda
                    (:global (name interface version)
                     (with-abandon
                       (when (bind-one-global server name interface version)
                         (logmsg :info "global appeared: ~a v~d"
                                 interface version))))
                    (:global-remove (name)
                     (with-abandon (forget-global server name))))))
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
