;;;; runtime/pointer.lisp --- Managing windows with the pointer.
;;;;
;;;; "Floating windows are keyboard-only, which is not how anyone uses floating
;;;; windows."  That was the largest gap between this program and a window
;;;; manager somebody else could use, and it was a gap in the *implementation*
;;;; rather than in the protocol: river offers everything needed and none of it
;;;; was consumed.
;;;;
;;;; WHAT RIVER GIVES US, and it is a better shape than X11's:
;;;;
;;;;   window_interaction        somebody clicked, touched or tapped a window.
;;;;                             Deliberately *not* raw pointer events — the
;;;;                             protocol's own rationale is "a policy over
;;;;                             mechanism approach" — so click-to-focus is one
;;;;                             event handler and works for touch and tablet
;;;;                             input without knowing they exist.
;;;;   pointer_enter / leave     the pointer crossed into a window's area,
;;;;                             borders and decorations included.  This is
;;;;                             focus-follows-mouse without hit-testing
;;;;                             rectangles ourselves, and it is *correct*
;;;;                             where our own hit test was approximate.
;;;;   get_pointer_binding       a button plus modifiers, enable/disable, just
;;;;                             like a key binding.  Super+drag.
;;;;   op_start_pointer          take over the pointer for an interactive
;;;;                             operation.  The compositor guarantees no
;;;;                             client has pointer focus for the duration, so
;;;;                             a drag cannot leak into the window under it.
;;;;   op_delta                  *cumulative* motion since the operation began.
;;;;   op_release                the buttons came up.
;;;;   pointer_move_requested    the client asked to be dragged, because
;;;;   pointer_resize_requested  somebody grabbed its own titlebar or corner.
;;;;
;;;; THE ONE THING TO HOLD ON TO: op_delta is cumulative, not incremental.  So
;;;; an operation remembers the rectangle it *started* from and recomputes from
;;;; it each time, rather than accumulating — which is also why a drag that
;;;; hits the edge of the screen and comes back lands exactly where the pointer
;;;; is, instead of drifting.
;;;;
;;;; Everything here routes through policy generics.  Which button does what,
;;;; whether a click focuses, whether a drag on a tiled window moves it or
;;;; resizes its divider — none of that is the runtime's to decide.

(in-package #:latticewm/runtime)

;;; What a click *means* — whether it focuses, whether it raises, which button
;;; drags, how small a window may be dragged to — is policy, and lives in
;;; policy/conventional.lisp and policy/keys.lisp with every other decision.
;;; What lives here is the mechanism: bindings, the operation in progress, and
;;; the arithmetic of turning cumulative deltas into rectangles.

;;; ------------------------------------------------- an operation in progress

(defstruct (pointer-op (:constructor %make-pointer-op))
  "An interactive drag, from the moment a button went down.

RECT is what the window's rectangle was when the drag *started*, and it is the
only reason this struct exists: river sends cumulative deltas, so every update
is computed from the start rather than from the last one.  Accumulating instead
drifts, and drifts worst exactly when the pointer leaves the screen and comes
back."
  (kind :move)
  (window nil)
  (float nil)
  (rect nil)
  (edges nil))

(defun start-pointer-op (seat kind window &key edges)
  "Begin an interactive KIND operation on WINDOW.  Manage sequence only.

Returns the operation, or NIL when there is nothing to drag.  River ignores
op_start_pointer if one is already in progress, so starting a second is
harmless rather than an error — but we would then have two records of one
operation, so the check is here as well."
  (let ((float (and window (c:float-of-window *world* window))))
    (cond
      ((null window) nil)
      ((seat-pointer-op seat) (seat-pointer-op seat))
      ;; A tiled window has no rectangle of its own to drag, so dragging it
      ;; means floating it first.  Doing it here rather than refusing is the
      ;; difference between "drag does nothing" and "drag does the obvious
      ;; thing"; *FLOAT-ON-DRAG* is for people who want the refusal.
      ((and (null float) p:*float-on-drag* (eq kind :move))
       (float-window-now (p:current-policy) window)
       (setf float (c:float-of-window *world* window))
       (when float (start-pointer-op-1 seat kind window float :edges edges)))
      ((null float) nil)
      (t (start-pointer-op-1 seat kind window float :edges edges)))))

(defun start-pointer-op-1 (seat kind window float &key edges)
  "The half of START-POINTER-OP that assumes there is something to drag.

THE TWO REQUESTS BELOW ARE MANAGE-SEQUENCE-ONLY AND THE ONLY CALLER IS AN EVENT
HANDLER, WHICH IS NOT A SEQUENCE.  So they were sent, refused, logged and lost
— river never entered its pointer-op mode, never sent an op_delta, and
Super+drag did nothing at all, for the second time and for a different reason
than the first.  The bookkeeping stays here where the caller needs it; the
protocol half waits for a sequence that may legally carry it."
  (let ((operation (%make-pointer-op :kind kind :window window :float float
                                     :rect (c:copy-rect (c:float-rect float))
                                     :edges edges)))
    (setf (seat-pointer-op seat) operation)
    (defer-to-manage
      (lambda ()
        (best-effort "op_start_pointer" (w:seat-op-start-pointer (seat-proxy seat)))
        (when (eq kind :resize)
          (best-effort "inform_resize_start"
            (w:window-inform-resize-start (c:window-proxy window))))))
    (run-hooks :pointer-op kind window)
    (logmsg :debug "pointer ~(~a~) started on ~s" kind window)
    operation))

(defun end-pointer-op (seat)
  "Finish whatever the pointer was doing.  Manage sequence only."
  (let ((operation (seat-pointer-op seat)))
    (when operation
      (setf (seat-pointer-op seat) nil)
      ;; Deferred for the same reason the start is: op_end and
      ;; inform_resize_end are window-management state, and op_release arrives
      ;; as an event.
      (let ((kind (pointer-op-kind operation))
            (proxy (c:window-proxy (pointer-op-window operation))))
        (defer-to-manage
          (lambda ()
            (when (and (eq kind :resize) proxy)
              (best-effort "inform_resize_end" (w:window-inform-resize-end proxy)))
            (best-effort "op_end" (w:seat-op-end (seat-proxy seat))))))
      (run-hooks :pointer-op nil (pointer-op-window operation))
      (logmsg :debug "pointer ~(~a~) finished" (pointer-op-kind operation))
      (mark-dirty)
      (request-manage))
    operation))

(defun apply-pointer-delta (seat dx dy)
  "Move or resize the dragged window to match a cumulative delta of (DX, DY)."
  (let ((operation (seat-pointer-op seat)))
    (when operation
      (let ((float (pointer-op-float operation))
            (start (pointer-op-rect operation)))
        (when (and float start)
          (setf (c:float-rect float)
                (guarded "pointer-drag"
                  (p:pointer-drag-rect (p:current-policy) *world*
                                       (pointer-op-kind operation)
                                       start dx dy (pointer-op-edges operation))))
          (unless (c:float-rect float)
            (setf (c:float-rect float) start))
          (mark-dirty))))))

;;; ------------------------------------------------------- the seat's events

(defun attach-pointer-bindings (seat)
  "Register every pointer binding in *POINTER-BINDINGS* with river.

Created once per seat and enabled immediately, unlike the capture keys, because
a pointer binding only fires with its modifiers held — so leaving it enabled
costs nothing and cannot swallow an ordinary click."
  (when (c:prop seat :pointer-bindings)
    (return-from attach-pointer-bindings (c:prop seat :pointer-bindings)))
  (setf (c:prop seat :pointer-bindings)
        (loop for entry in p:*pointer-bindings*
              for kind = (car entry)
              for spec = (cdr entry)
              for button = (ignore-errors (p:pointer-button-code (first spec)))
              for modifiers = (ignore-errors (p:modifier-mask (second spec)))
              when button
                collect (let ((binding (best-effort "get_pointer_binding"
                                         (w:seat-get-pointer-binding
                                          (seat-proxy seat) button modifiers)))
                              (kind kind))
                          (when binding
                            (on-events (binding "river_pointer_binding_v1")
                              (:pressed (pointer-binding-pressed seat kind))
                              (:released (pointer-binding-released seat kind))
                              (t nil))
                            ;; POINTER-BINDING-ENABLE, not BINDING-ENABLE.
                            ;; They are different interfaces with identically
                            ;; named requests, and calling the keyboard one on
                            ;; a pointer binding is a type error inside the
                            ;; generated marshaller — caught, logged, and
                            ;; otherwise completely silent.  Super+drag did
                            ;; nothing at all, and the headless integration
                            ;; test found it on its first run.
                            (best-effort "enable pointer binding"
                              (w:pointer-binding-enable binding)))
                          (cons kind binding))))
  (logmsg :info "~d pointer binding~:p" (length (c:prop seat :pointer-bindings)))
  (c:prop seat :pointer-bindings))

(defun pointer-binding-pressed (seat kind)
  "A bound pointer button went down: start dragging whatever is under it."
  (let ((window (or (seat-pointer-window seat)
                    (window-under-pointer seat))))
    (when window
      (guarded "pointer focus" (focus-window-from-pointer window))
      (start-pointer-op seat kind window
                        :edges (when (eq kind :resize)
                                 (p:pointer-resize-edges
                                  (p:current-policy) window
                                  (seat-pointer-x seat) (seat-pointer-y seat)))))))

(defun pointer-binding-released (seat kind)
  "A bound pointer button came up."
  (declare (ignore kind))
  (end-pointer-op seat))

(defun window-under-pointer (seat)
  "The window the pointer is over, worked out from the last layout.

The fallback for when river has not told us — pointer_enter is authoritative
and arrives first in every ordinary case, but a binding can fire on the very
first event after a connect."
  (let ((x (seat-pointer-x seat))
        (y (seat-pointer-y seat)))
    (or (loop for float in (reverse (c:world-floats *world*))
              for window = (c:float-window float)
              for rect = (and window (c:window-rect window))
              when (and rect (c:rect-contains-p rect x y)) return window)
        (loop for window in (all-windows)
              for rect = (c:window-rect window)
              when (and rect (c:window-live-p window)
                        (not (c:window-minimized-p window))
                        (c:rect-contains-p rect x y))
                return window))))

(defun focus-window-from-pointer (window)
  "Give WINDOW the keyboard because the pointer asked.

A float becomes the focused float; a tiled window moves the cursor to its pane.
Both are the *model* change — the emitter turns it into protocol requests in
the next manage sequence, which is the same rule every command follows."
  (when (and window (c:window-live-p window))
    (let ((float (c:float-of-window *world* window)))
      (cond
        (float
         (setf (c:world-focused-float *world*) float)
         (when p:*click-to-raise*
           (setf (c:world-floats *world*)
                 (append (remove float (c:world-floats *world*)) (list float)))))
        (t
         (let* ((root (c:world-root *world*))
                (leaf (c:leaf-holding root window))
                (path (and leaf (c:node-path-to root leaf))))
           (when path
             (p:jump-cursor (p:current-policy) *world* path))))))
    (mark-dirty)
    window))

;;; ----------------------------------------------- the window manager's events
;;;
;;; These are attached to the *seat* and to the *manager*, not here — see
;;; ATTACH-SEAT and EVLAMBDA-FOR-WINDOW.  What lives here is what they call.

(defun on-pointer-enter (seat window)
  "The pointer entered WINDOW's area."
  (setf (seat-pointer-window seat) window)
  (when (and p:*focus-follows-mouse* window (null (seat-pointer-op seat)))
    (guarded "pointer-focus" (focus-window-from-pointer window))))

(defun on-pointer-leave (seat)
  "The pointer left whatever it was over."
  (setf (seat-pointer-window seat) nil))

(defun on-window-interaction (seat window)
  "Somebody clicked, tapped or touched WINDOW.

River sends this rather than raw button events on purpose — its own rationale
is that a window manager needs to know *when to send keyboard focus and when to
raise*, and exposing every pointer event to answer that would be mechanism
where policy belongs.  The consequence is pleasant: this one handler is
click-to-focus for the mouse, for touch, and for a tablet stylus, and it never
had to be told they were different."
  (declare (ignore seat))
  (when (and p:*click-to-focus* window)
    (guarded "click to focus" (focus-window-from-pointer window))
    (request-manage)))

(defun on-client-move-request (window seat)
  "A client asked to be dragged, because somebody grabbed its own titlebar."
  (when (and p:*honour-client-move-requests* seat)
    (start-pointer-op seat :move window)))

(defun on-client-resize-request (window seat edges)
  "A client asked to be resized, because somebody grabbed its own corner."
  (when (and p:*honour-client-move-requests* seat)
    (start-pointer-op seat :resize window :edges edges)))

(defun seat-of-proxy (proxy)
  "Our SEAT for a river_seat_v1 PROXY, or the primary one.

The move and resize requests name a seat, and on every ordinary machine there
is exactly one — but naming it is free and a two-seat machine that silently
dragged with the wrong pointer would be very hard to explain."
  (or (find proxy (server-seats *server*) :key #'seat-proxy)
      (primary-seat)))
