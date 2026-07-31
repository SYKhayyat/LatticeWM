;;;; runtime/emit.lisp --- Turning a layout into protocol calls.
;;;;
;;;; This is the only file that knows both what the model means and what the
;;;; wire wants.  Policy says where things go; this decides which protocol
;;;; sequence each call belongs in, and never lets that distinction leak
;;;; upwards.
;;;;
;;;; THE SHAPE OF THE PROBLEM
;;;;
;;;; A layout produces two kinds of instruction at once, and river will not
;;;; accept them at the same time:
;;;;
;;;;   propose_dimensions, focus, capabilities   MANAGE sequence only
;;;;   set_position, borders, hide/show, order   rendering state
;;;;
;;;; So the layout is computed once, split into two piles, and each pile is
;;;; drained when its sequence comes round.  The manage pile is held on the
;;;; server object between sequences.
;;;;
;;;; EVERYTHING IS DIFFED.  River processes every request we send before it can
;;;; act on further input, and the spec has an `unresponsive' error it will use
;;;; if we are slow.  Re-sending a hundred unchanged positions on every
;;;; keystroke is a real cost for no benefit, so each property remembers what
;;;; was last sent for it.

(in-package #:latticewm/runtime)

(defun emitted (window property)
  "The last value sent for PROPERTY of WINDOW."
  (gethash (cons window property) (server-emitted *server*) :none))

(defun (setf emitted) (value window property)
  (setf (gethash (cons window property) (server-emitted *server*)) value))

(defmacro when-changed ((window property value) &body body)
  "Run BODY only if VALUE differs from what was last sent for PROPERTY."
  (let ((v (gensym "VALUE")) (w (gensym "WINDOW")) (p (gensym "PROPERTY")))
    `(let ((,v ,value) (,w ,window) (,p ,property))
       (unless (equal ,v (emitted ,w ,p))
         (setf (emitted ,w ,p) ,v)
         ,@body))))

(defun forget-window-state (window)
  "Drop the diff cache for WINDOW.

Called when a window goes away, and when we deliberately want the next
relayout to re-send everything — after a hot reconnect, say."
  (let ((table (server-emitted *server*)))
    (loop for key being the hash-keys of table
          when (eq (car key) window) do (remhash key table))))

;;; ------------------------------------------------------------ the layout

(defun compute-layout ()
  "Lay the world out over the outputs and return the placements.

Multi-monitor is one model with one viewport per output (PLAN §fiat), so this
lays the same tree out on each output and lets the policy decide what each one
shows.  With a single output — which is every laptop — it is one call."
  (let* ((policy (p:current-policy))
         (outputs (all-outputs))
         (root (c:world-root *world*)))
    (if (null outputs)
        ;; No outputs yet: lay out over a nominal rectangle so that the tree is
        ;; in a consistent state.  Nothing is drawn, because nothing is
        ;; connected.
        (guarded "layout" (p:layout-node policy root (c:make-rect 0 0 1920 1080)))
        (loop for output in outputs
              append (guarded "layout"
                       (p:layout-node policy root
                                      (p:outer-rect policy output)))))))

(defun index-placements (placements)
  "A node-to-rect hash, cached on the world for motion and pointer hit tests."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (placement placements table)
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path visible))
        (setf (gethash node table) rect)))))

(defun relayout (&key (force nil))
  "Recompute the layout and emit everything that changed.

Called after every command that could have moved something.  It is safe to
call more than once — the diff makes a redundant call nearly free — so callers
should err towards calling it rather than reasoning about whether they must."
  (unless (and *server* *world*)
    (return-from relayout nil))
  (when force
    (clrhash (server-emitted *server*)))
  (let* ((policy (p:current-policy))
         (placements (compute-layout))
         (ordered (guarded "render-order" (p:render-order policy placements))))
    (setf (c:prop *world* :last-placements) placements
          (c:prop *world* :rect-index) (index-placements placements))
    ;; Split the work.  The manage pile waits for the next manage sequence.
    (setf (server-pending-dimensions *server*)
          (collect-dimension-work policy placements))
    (emit-rendering-state policy (or ordered placements))
    (run-hooks :layout-changed)
    placements))

(defun collect-dimension-work (policy placements)
  "The window-management half of the layout: what size each window should be.

Returned as a list of (WINDOW WIDTH HEIGHT) rather than sent, because
propose_dimensions is manage-sequence-only and a relayout can happen at any
time."
  (let ((out '()))
    (dolist (placement placements (nreverse out))
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path))
        (when (and visible (typep node 'c:leaf) (c:leaf-window node))
          (let ((window (c:leaf-window node)))
            (when (and (c:window-live-p window)
                       (not (c:window-minimized-p window))
                       (not (c:window-fullscreen-p window)))
              (multiple-value-bind (width height)
                  (guarded "window-dimensions"
                    (p:window-dimensions policy node rect))
                (when (and width height)
                  (push (list window (max 1 width) (max 1 height)) out))))))))))

(defun emit-dimension-work ()
  "Drain the pending propose_dimensions work.  Manage sequence only."
  (let ((work (server-pending-dimensions *server*)))
    (setf (server-pending-dimensions *server*) '())
    (dolist (entry work)
      (destructuring-bind (window width height) entry
        (when (c:window-proxy window)
          (when-changed (window :dimensions (list width height))
            (guarded "propose_dimensions"
              (w:window-propose-dimensions (c:window-proxy window)
                                           width height))))))))

;;; ------------------------------------------------------- rendering state

(defun emit-rendering-state (policy placements)
  "Position, order, borders, visibility and clipping for everything.

Note the ordering pass at the end: river says the initial position of a node in
the render list is *undefined*, so a node nobody ordered can be drawn anywhere
in the stack.  Every visible node has to be ordered explicitly, every time its
set changes, or overlapping windows flicker between frames."
  (let ((cursor (c:world-cursor *world*))
        (shown '()))
    (dolist (placement placements)
      (destructuring-bind (node path rect visible) placement
        (when (typep node 'c:leaf)
          (let ((window (c:leaf-window node)))
            (when (and window (c:window-proxy window) (c:window-live-p window))
              (cond
                ((and visible (not (c:window-minimized-p window)))
                 (push window shown)
                 (emit-window-visible policy window node path rect cursor))
                (t
                 (when-changed (window :shown nil)
                   (guarded "hide" (w:window-hide (c:window-proxy window)))))))))))
    (emit-floats policy)
    (emit-render-order (nreverse shown))))

(defun emit-window-visible (policy window node path rect cursor)
  "Show WINDOW at RECT, with the border and clip its policy asks for."
  (let* ((proxy (c:window-proxy window))
         (focusedp (c:path-equal path cursor))
         (placed (place-rect policy node rect window)))
    (setf (c:window-rect window) placed)
    (when-changed (window :shown t)
      (guarded "show" (w:window-show proxy)))
    (let ((river-node (window-river-node window)))
      (when river-node
        (when-changed (window :position (list (c:rect-x placed) (c:rect-y placed)))
          (guarded "set_position"
            (w:node-set-position river-node (c:rect-x placed) (c:rect-y placed))))))
    ;; Borders.  Also the only decoration a focused *empty* pane could have —
    ;; but an empty pane has no window, so the cursor there is drawn by the
    ;; overlay instead; see runtime/overlay.lisp.
    (let ((width (guarded "border-width" (p:border-width policy node focusedp))))
      (multiple-value-bind (r g b a)
          (guarded "border-color" (p:border-color policy node focusedp))
        (when (and width r)
          (when-changed (window :borders (list width r g b a))
            (guarded "set_borders"
              (w:window-set-borders proxy w:+edges-all+ width
                                    (w:color-component r) (w:color-component g)
                                    (w:color-component b)
                                    (w:color-component a)))))))
    ;; Content clipping: the viewport edge, rendered by the compositor.
    (let ((clip (guarded "clip-rect" (p:clip-rect policy node placed))))
      (when-changed (window :clip (and clip (list (c:rect-x clip) (c:rect-y clip)
                                                  (c:rect-w clip) (c:rect-h clip))))
        (guarded "set_content_clip_box"
          (if clip
              (w:window-set-content-clip-box
               proxy (- (c:rect-x clip) (c:rect-x placed))
               (- (c:rect-y clip) (c:rect-y placed))
               (c:rect-w clip) (c:rect-h clip))
              ;; A clip box covering everything is how you turn clipping off.
              (w:window-set-content-clip-box proxy 0 0 (c:rect-w placed)
                                             (c:rect-h placed))))))))

(defun place-rect (policy node rect window)
  "Where WINDOW actually goes inside the RECT it was assigned.

River's propose_dimensions is advisory, and the spec calls out terminal
emulators that quantise to their cell size, so a window frequently comes back
smaller than its pane.  GRAVITY decides where the shortfall goes; the shipped
answer is to centre it."
  (let ((width (c:window-width window))
        (height (c:window-height window)))
    (if (and (plusp width) (plusp height)
             (or (< width (c:rect-w rect)) (< height (c:rect-h rect))))
        (or (guarded "gravity" (p:gravity policy node rect width height)) rect)
        rect)))

(defun emit-floats (policy)
  "Position the floating windows.

They are not in the tree, so they are not in the layout; they are placed from
their own rectangles, and an *anchored* float is offset by wherever its anchor
node ended up — which is what makes 'a floating window inside a window' travel
with the window it belongs to."
  (dolist (float (c:world-floats *world*))
    (let* ((window (c:float-window float))
           (proxy (and window (c:window-proxy window))))
      (when (and proxy (c:window-live-p window))
        (if (c:window-minimized-p window)
            (when-changed (window :shown nil)
              (guarded "hide" (w:window-hide proxy)))
            (let* ((anchor (c:float-anchor float))
                   (base (and anchor (gethash anchor (c:prop *world* :rect-index))))
                   (rect (c:float-rect float))
                   (placed (if base
                               (c:make-rect (+ (c:rect-x base) (c:rect-x rect))
                                            (+ (c:rect-y base) (c:rect-y rect))
                                            (c:rect-w rect) (c:rect-h rect))
                               rect)))
              (setf (c:window-rect window) placed)
              (when-changed (window :shown t)
                (guarded "show" (w:window-show proxy)))
              (push (list window (c:rect-w placed) (c:rect-h placed))
                    (server-pending-dimensions *server*))
              (let ((river-node (window-river-node window)))
                (when river-node
                  (when-changed (window :position (list (c:rect-x placed)
                                                        (c:rect-y placed)))
                    (guarded "set_position"
                      (w:node-set-position river-node (c:rect-x placed)
                                           (c:rect-y placed))))))
              (multiple-value-bind (r g b a)
                  (guarded "border-color"
                    (p:border-color policy (c:make-leaf window)
                                    (eq window (current-window))))
                (let ((width (p:border-width policy (c:make-leaf window) nil)))
                  (when (and r width)
                    (when-changed (window :borders (list width r g b a))
                      (guarded "set_borders"
                        (w:window-set-borders proxy w:+edges-all+ width
                                              (w:color-component r)
                                              (w:color-component g)
                                              (w:color-component b)
                                              (w:color-component a)))))))))))))

(defun emit-render-order (tiled)
  "Order every visible node, bottom to top: tiled, then floats, then overlays.

River leaves the initial render position undefined, so this cannot be skipped
for windows we have not moved.  It can, however, be skipped when the *sequence*
has not changed, which is the common case."
  (let* ((floats (loop for float in (c:world-floats *world*)
                       for window = (c:float-window float)
                       when (and window (c:window-live-p window)
                                 (not (c:window-minimized-p window)))
                         collect window))
         (order (append tiled floats)))
    ;; EQUAL on a list of objects compares them with EQL, which is identity —
    ;; exactly the comparison wanted, and it makes the common case (nothing
    ;; changed order) one list walk rather than N protocol messages.
    (unless (equal order (c:prop *world* :render-signature))
      (setf (c:prop *world* :render-signature) (copy-list order))
      (let ((previous nil))
        (dolist (window order)
          (let ((node (window-river-node window)))
            (when node
              (guarded "render order"
                (if previous
                    (w:node-place-above node previous)
                    (w:node-place-bottom node)))
              (setf previous node))))))))
