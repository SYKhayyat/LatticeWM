;;;; policy/defaults-motion.lisp --- The shipped answers for MOTION-POLICY,
;;;; and for the pointer half of INPUT-POLICY.
;;;;
;;;; Where the cursor goes, and what a drag means.  The *algorithm* — the walk
;;;; up the ancestor chain asking each container whether it can satisfy the
;;;; motion — is policy/motion.lisp; what is here is the set of answers the
;;;; shipped containers give.
;;;;
;;;; They are together because they are the same question asked by two devices.
;;;; A keyboard motion and a pointer drag both end in "which place is this",
;;;; and a policy that changes one nearly always wants to change the other.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; MOTION
;;; ==================================================================

(defmethod step-address ((policy motion-policy) (split c:split)
                         address direction)
  "Within a split, motion works only along the split's own axis.

Moving up inside a row of side-by-side panes is not a smaller motion — it is a
motion this container cannot answer, so it returns NIL and the caller ascends
to ask the parent.  That is what makes a vertical move inside a horizontal
split leave the split, which is what every user expects and what most tiling
window managers get wrong on the first try."
  (when (eq (c:direction-axis direction) (c:split-axis split))
    (let ((next (+ address (c:direction-sign direction))))
      (when (and (<= 0 next) (< next (c:container-count split)))
        next))))

(defmethod step-address ((policy motion-policy) (stack c:stack)
                         address direction)
  "A stack answers no spatial direction.

You do not arrive in another workspace, or another tab, by pressing Left.
Switching is its own verb, and that is what keeps a stack legible."
  (declare (ignore address direction))
  nil)

(defmethod entry-address ((policy motion-policy) (split c:split)
                          direction reference rects)
  "Enter through the edge you crossed, at the height you were already at.

Two rules, and they answer different questions:

  * *Along* the split's own axis — travelling right into a horizontal split —
    entry is by edge: you arrive at the leftmost child.  DESIGN D20.  This is
    what guarantees every pane is reachable by directional motion, and it is
    why last-focus memory is not used here: with memory, a cell whose right
    pane was last focused would swallow a rightward move and the left pane
    could never be reached at all.

  * *Across* the axis — travelling right into a vertical split — entry is
    geometric: you arrive at the child that lines up with the pane you left.
    Where the two sides are divided alike, that makes Right-then-Left return
    you exactly where you started.

    IT DOES NOT DO SO IN GENERAL, and the sentence here used to say it did.
    Alignment is computed from the centre of the rect you are leaving, so the
    centre you carry over is not the centre you carry back: with the left
    column split 1:3 and the right column 3:1, rightwards from the top-left
    pane arrives in the tall top-right one, whose centre is below the whole of
    where you came from, and leftwards from there arrives in the bottom-left.
    Returning in every arrangement would mean carrying the coordinate you
    crossed at, which is exactly the kind of remembered state D20 declined —
    and declining it is what keeps every pane reachable.  A real property at
    the cost of an approximate one; TESTS/TEST-MOTION.LISP asserts both halves."
  (let ((n (c:container-count split)))
    (cond ((zerop n) nil)
          ((and direction (eq (c:direction-axis direction) (c:split-axis split)))
           (if (plusp (c:direction-sign direction)) 0 (1- n)))
          ((and direction reference rects)
           (or (best-aligned-address split reference rects (c:split-axis split))
               0))
          (t 0))))

(defmethod entry-address ((policy motion-policy) (stack c:stack)
                          direction reference rects)
  "Entering a stack always lands on the visible child.  Directional motion
does not reveal a hidden tab."
  (declare (ignore direction reference rects))
  (stack-visible-address policy stack))

(defmethod motion-escapes-p ((policy motion-policy) container direction)
  (declare (ignore container direction))
  t)

(defmethod focus-after-remove ((policy motion-policy) world removed-path
                               suggested)
  (declare (ignore removed-path))
  (ecase *focus-after-close*
    (:stay suggested)
    (:next (or (c:next-leaf-path (c:world-root world) suggested) suggested))
    (:mru (or (mru-path world) suggested))))

(defun mru-path (world)
  "The path of the most recently focused surviving window, or NIL.

The recency list lives on the world's PROPS rather than in a slot, because it
is exactly the kind of state an *option* wants and the core does not."
  (loop for window in (c:prop world :focus-history)
        for leaf = (and (c:window-live-p window)
                        (c:leaf-holding (c:world-root world) window))
        when leaf return (c:node-path-to (c:world-root world) leaf)))

(defmethod on-focus-change ((policy motion-policy) world old new)
  "Record the window we are leaving, so that :MRU has something to consult."
  (declare (ignore old))
  (let ((window (c:world-window-at world new)))
    (when window
      (setf (c:prop world :focus-history)
            (cons window (remove window (c:prop world :focus-history)
                                 :count 1)))))
  nil)

(defmethod focus-target ((policy motion-policy) world)
  "D18's rule: a focused float, else the cursor's window, else nothing.

Delegated to C:WORLD-FOCUS-WINDOW rather than restated, because the model
already answers it and two copies of the definition of focus is precisely the
sort of thing that goes wrong quietly.  What is new is that the *question* is
now asked of a policy, so answering it differently is a method rather than a
patch to an event handler."
  (declare (ignore policy))
  (c:world-focus-window world))

(defmethod pointer-focus ((policy motion-policy) world x y)
  "The deepest visible leaf whose rectangle contains the pointer."
  (let ((best nil))
    (dolist (placement (c:prop world :last-placements) best)
      (destructuring-bind (node path rect visible) placement
        (when (and visible (typep node 'c:leaf) (c:rect-contains-p rect x y))
          (setf best path))))))

;;; ------------------------------------------------------- pointer behaviour
;;;
;;; What a click means, what a drag means, and how small a window may be
;;; dragged to are decisions.  They live here rather than beside the protocol
;;; plumbing for the same reason every other decision does — see gate 6, and
;;; the ruling that a DEFINE-OPTION in src/runtime/ is still something a user
;;; has to read the runtime to find.

(define-option *click-to-focus* t
  "Clicking a window moves the cursor to it.

On by default, because a pointer that can point at a window but not focus it is
a pointer that does not work.  Driven by river's window_interaction event
rather than by watching buttons, so it covers touch and tablet input too.")

(define-option *click-to-raise* t
  "Clicking a floating window puts it on top of the other floats.

Only floats: tiled windows do not overlap, so raising one means nothing.")

(define-option *float-on-drag* t
  "Dragging a *tiled* window with the move binding floats it first.

The alternative is that the drag does nothing, which is worse than either
answer: the window is under the pointer, the pointer is moving, and the screen
is not changing.  Set this to NIL to make the move binding apply only to
windows that are already floating.")

(define-option *honour-client-move-requests* t
  "Let a window start its own drag when you grab its titlebar.

Clients that draw their own decorations ask for this through xdg-shell and
river forwards the request.  Refusing it makes a GTK application's titlebar
inert, which reads as the window manager being broken rather than as a policy.")

(define-option *pointer-resize-minimum* '(120 . 80)
  "The smallest a window may be dragged to, as (WIDTH . HEIGHT) in pixels.

Not zero, because a window dragged down to nothing cannot be dragged back —
its corner is no longer anywhere the pointer can reach.")

(define-option *pointer-snap* 12
  "Snap a dragged window to within this many pixels of an output edge.

Zero turns it off.  Twelve gives the behaviour where a window dropped near an
edge lines up with it exactly, which is the one piece of drag polish that is
worth having and the one people notice is missing.")

(defun snap-coordinate (value edges)
  "VALUE pulled to the nearest of EDGES within *POINTER-SNAP* pixels."
  (if (plusp *pointer-snap*)
      (or (loop for edge in edges
                when (<= (abs (- value edge)) *pointer-snap*) return edge)
          value)
      value))

(defun output-edges (world horizontal)
  "Every output edge along one axis, for snapping."
  (when world
    (loop for output in (c:world-outputs world)
          for rect = (c:output-rect output)
          append (if horizontal
                     (list (c:rect-x rect) (c:rect-right rect))
                     (list (c:rect-y rect) (c:rect-bottom rect))))))

(defmethod pointer-drag-rect ((policy input-policy) world kind start dx dy edges)
  "Move by the cumulative delta, or resize the edges being dragged by it.

START plus the *total* motion, never the current rectangle plus the last step:
river reports cumulative deltas, and accumulating instead drifts — worst
exactly when the pointer runs off the edge of the screen and comes back, which
is when a drag most needs to land where you are pointing."
  (let ((minimum-w (car *pointer-resize-minimum*))
        (minimum-h (cdr *pointer-resize-minimum*)))
    (ecase kind
      (:move
       (let* ((x (+ (c:rect-x start) dx))
              (y (+ (c:rect-y start) dy))
              ;; Snap either edge: dragging a window's right side up against
              ;; the screen edge is as common as dragging its left side.
              (snapped-x (snap-coordinate x (output-edges world t)))
              (snapped-x (if (/= snapped-x x)
                             snapped-x
                             (- (snap-coordinate (+ x (c:rect-w start))
                                                 (output-edges world t))
                                (c:rect-w start))))
              (snapped-y (snap-coordinate y (output-edges world nil)))
              (snapped-y (if (/= snapped-y y)
                             snapped-y
                             (- (snap-coordinate (+ y (c:rect-h start))
                                                 (output-edges world nil))
                                (c:rect-h start)))))
         (c:make-rect snapped-x snapped-y (c:rect-w start) (c:rect-h start))))
      (:resize
       (let ((x (c:rect-x start)) (y (c:rect-y start))
             (w (c:rect-w start)) (h (c:rect-h start)))
         (cond ((member :left edges)
                ;; Dragging the left edge moves the origin and changes the
                ;; width by the opposite amount, so the right edge stays put.
                (let ((width (max minimum-w (- w dx))))
                  (setf x (+ x (- w width)) w width)))
               ((member :right edges) (setf w (max minimum-w (+ w dx)))))
         (cond ((member :top edges)
                (let ((height (max minimum-h (- h dy))))
                  (setf y (+ y (- h height)) h height)))
               ((member :bottom edges) (setf h (max minimum-h (+ h dy)))))
         (c:make-rect x y w h))))))

(defmethod pointer-resize-edges ((policy input-policy) (window c:window) x y)
  "The quadrant of the window the pointer is in.

So a resize drag started near the top-left corner moves that corner, which is
what every floating window manager does and what a hand already expects."
  (let ((rect (c:window-rect window)))
    (if (null rect)
        (list :right :bottom)
        (multiple-value-bind (centre-x centre-y) (c:rect-center rect)
          (list (if (< x centre-x) :left :right)
                (if (< y centre-y) :top :bottom))))))