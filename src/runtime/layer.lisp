;;;; runtime/layer.lisp --- Third-party panels, bars, wallpapers and lockers.
;;;;
;;;; THIS IS THE ECOSYSTEM A TILING WINDOW MANAGER IS ASSEMBLED OUT OF, and it
;;;; was the largest single absence in the program.  waybar, yambar, mako,
;;;; swaybg, swaync, wlogout, every screen locker — all of them are
;;;; wlr-layer-shell clients, and river's own rule is blunt about what happens
;;;; without us:
;;;;
;;;;   "If the window manager does not bind this interface, the compositor
;;;;    should not allow clients to map layer surfaces.  This can be achieved
;;;;    by closing layer surfaces immediately."
;;;;
;;;; So a window manager that does not bind river_layer_shell_v1 does not
;;;; merely lack panel support: it makes every panel on the machine fail to
;;;; start.  The global was bound and then nothing was done with it, which is
;;;; the same outcome with a longer explanation.
;;;;
;;;; WHAT RIVER ASKS OF US IS SMALL, and that is the good news.  We do not
;;;; manage layer surfaces — the compositor positions and stacks them, per the
;;;; layer-shell protocol's own rules.  We answer two questions:
;;;;
;;;;   1. *How much room is left?*  river_layer_shell_output_v1 sends
;;;;      non_exclusive_area: the part of the output that remains after every
;;;;      panel's exclusive zone has been subtracted.  Laying windows out
;;;;      inside it is the whole of "windows sit above the bar rather than
;;;;      under it", and it costs one event handler.
;;;;
;;;;   2. *Who has the keyboard?*  river_layer_shell_seat_v1 says when a layer
;;;;      surface has taken focus.  Two cases matter and they want opposite
;;;;      behaviour:
;;;;
;;;;        focus_exclusive     a screen locker.  Every focus request we make
;;;;                            is ignored until further notice, and trying
;;;;                            anyway is how a locker ends up fighting the
;;;;                            window manager for the keyboard.  We stop
;;;;                            asking, and we stop drawing a focused border
;;;;                            on a window that does not have focus.
;;;;        focus_non_exclusive a launcher or a notification with a text field.
;;;;                            We still own focus and may take it back; the
;;;;                            polite thing is to leave it alone until the
;;;;                            user does something.
;;;;
;;;; The lock case is why this file also closes §7's "no screen locker will
;;;; work correctly".  It is not that a locker could not run; it is that we
;;;; would have kept re-focusing a window underneath it on every manage
;;;; sequence.

(in-package #:latticewm/runtime)

(p:define-option *honour-exclusive-zones* t
  "Lay windows out inside the area panels have left, rather than over them.

T is what everybody means by having a status bar: the bar takes its strip and
the tiled windows use the rest.  NIL gives the full output to the layout and
lets the panel float above it, which is occasionally what somebody wants for a
transparent overlay bar and is otherwise a bug report waiting to happen.

The area comes from the compositor — river subtracts every layer surface's
exclusive zone and tells us what is left — so this works for any panel, on any
edge, at any size, with no per-panel configuration and nothing to keep in sync.")

(defun attach-layer-shell-output (output)
  "Follow OUTPUT's non-exclusive area, so panels can reserve their strip.

Nothing happens when the compositor did not offer river_layer_shell_v1, which
is the honest degradation: layer surfaces simply do not exist on that machine
and the layout gets the whole output."
  (let ((shell (and *server* (server-layer-shell *server*)))
        (proxy (c:output-proxy output)))
    (when (and shell proxy (null (c:prop output :layer-shell)))
      (let ((layer (best-effort "get layer shell output"
                     (w:layer-shell-get-output shell proxy))))
        (when layer
          (setf (c:prop output :layer-shell) layer)
          (on-events (layer "river_layer_shell_output_v1")
            (:non-exclusive-area
             (destructuring-bind (x y width height) arguments
               (setf (c:prop output :non-exclusive-area)
                     (c:make-rect x y width height))
               (logmsg :debug "~a has ~dx~d+~d+~d left after panels"
                       (or (c:output-name output) "output") width height x y)
               (mark-dirty)))
            (t nil))
          layer)))))

(defun attach-layer-shell-seat (seat)
  "Follow which kind of focus a layer surface has taken on SEAT.

Recorded on the seat rather than acted on here, because what it changes is a
*decision* — whether to send focus at all — and that belongs where focus is
decided.  See APPLY-KEYBOARD-FOCUS."
  (let ((shell (and *server* (server-layer-shell *server*)))
        (proxy (seat-proxy seat)))
    (when (and shell proxy (null (c:prop seat :layer-shell)))
      (let ((layer (best-effort "get layer shell seat"
                     (w:layer-shell-get-seat shell proxy))))
        (when layer
          (setf (c:prop seat :layer-shell) layer)
          (on-events (layer "river_layer_shell_seat_v1")
            (:focus-exclusive
             (setf (seat-layer-focus seat) :exclusive)
             (logmsg :info "a layer surface has taken exclusive focus~
                            (a screen locker, most likely)")
             (mark-dirty))
            (:focus-non-exclusive
             (setf (seat-layer-focus seat) :non-exclusive)
             (logmsg :debug "a layer surface has non-exclusive focus")
             (mark-dirty))
            (:focus-none
             (setf (seat-layer-focus seat) nil)
             (logmsg :debug "layer surface focus released")
             (mark-dirty))
            (t nil))
          layer)))))

(defun layer-shell-holds-keyboard-p (&optional (seat (primary-seat)))
  "True when a layer surface has taken the keyboard away from us.

Only :EXCLUSIVE counts.  :NON-EXCLUSIVE means a layer surface *has* focus and
we may take it back whenever we like — a launcher, a notification with a reply
field — and treating that as `hands off' would mean the window manager stopped
responding to focus commands whenever a notification was on screen."
  (and seat (eq (seat-layer-focus seat) :exclusive)))

(defun layer-reserved-edges (output)
  "How much of OUTPUT the panels have taken, as (TOP RIGHT BOTTOM LEFT).

Derived by subtracting the compositor's non-exclusive area from the output's
own rectangle, which turns `here is what is left' into the reservation shape
the rest of the system already speaks.  Zeroes when no panel has claimed
anything, when the compositor does not offer layer shell, or when the option is
off."
  (let ((area (and *honour-exclusive-zones* (c:prop output :non-exclusive-area)))
        (whole (c:output-rect output)))
    (if (null area)
        (list 0 0 0 0)
        (list (max 0 (- (c:rect-y area) (c:rect-y whole)))
              (max 0 (- (c:rect-right whole) (c:rect-right area)))
              (max 0 (- (c:rect-bottom whole) (c:rect-bottom area)))
              (max 0 (- (c:rect-x area) (c:rect-x whole)))))))

(defun set-default-layer-output (output)
  "Tell river that new layer surfaces with no opinion belong on OUTPUT.

Manage sequence only.  Without it the default output is undefined, so a panel
that does not name an output lands wherever the compositor last happened to
decide — which on a two-monitor desktop means the bar moves when you plug
something in."
  (let ((layer (and output (c:prop output :layer-shell))))
    (when layer
      (best-effort "layer shell set_default"
        (w:layer-output-set-default layer))
      t)))

;; The echo area is ours; a panel's strip is somebody else's.  Both are
;; reservations and both accumulate, which is exactly what the reserve hook is
;; for -- see POLICY:RESERVED-SPACE.
(add-hook :reserve-space 'layer-reserved-edges)
