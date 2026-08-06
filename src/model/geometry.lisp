;;;; model/geometry.lisp --- Rectangles and directions.
;;;;
;;;; Pure value types.  Nothing here knows what a window is.
;;;;
;;;; Note on DEFSTRUCT: DESIGN.org rules DEFCLASS for core *state*, because a
;;;; live image must be able to grow a slot on a lattice or a node without
;;;; invalidating existing instances.  A rectangle is not state — it is a value,
;;;; produced by the thousand during layout and never stored anywhere a user
;;;; would want to extend.  The ruling does not apply and the struct is the
;;;; right shape.  Nodes, in model/node.lisp, are DEFCLASS as ruled.

(in-package #:latticewm/core)

;;; ------------------------------------------------------------------ rects

(defstruct (rect (:constructor make-rect (&optional (x 0) (y 0) (w 0) (h 0)))
                 (:copier copy-rect))
  "An axis-aligned rectangle in compositor-logical pixels.

X and Y are the top-left corner, in river's coordinate space, where +Y points
down.  Lattice coordinates point +Y *up* (DESIGN D2) and are inverted exactly
once, at the boundary in the placement emitter.  Do not invert here."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (w 0 :type fixnum)
  (h 0 :type fixnum))

(declaim (ftype (function (rect) fixnum) rect-right rect-bottom))
(defun rect-right (r)
  "The X coordinate one pixel past R's right edge."
  (+ (rect-x r) (rect-w r)))

(defun rect-bottom (r)
  "The Y coordinate one pixel past R's bottom edge."
  (+ (rect-y r) (rect-h r)))

(declaim (ftype (function (rect) boolean) rect-empty-p))
(defun rect-empty-p (r)
  "True when R encloses no pixels."
  (or (<= (rect-w r) 0) (<= (rect-h r) 0)))

(declaim (ftype (function (rect fixnum fixnum) boolean) rect-contains-p))
(defun rect-contains-p (r x y)
  "True when the point (X, Y) lies inside R."
  (and (<= (rect-x r) x) (< x (rect-right r))
       (<= (rect-y r) y) (< y (rect-bottom r))))

(defun rect-equal (a b)
  "True when A and B describe the same rectangle."
  (and (= (rect-x a) (rect-x b)) (= (rect-y a) (rect-y b))
       (= (rect-w a) (rect-w b)) (= (rect-h a) (rect-h b))))

(defun rect-intersect (a b)
  "The overlap of A and B, or NIL when they do not overlap.

This is how a viewport crops a cell that hangs over its edge; the result is
what gets handed to river's set_content_clip_box."
  (let ((x (max (rect-x a) (rect-x b)))
        (y (max (rect-y a) (rect-y b)))
        (r (min (rect-right a) (rect-right b)))
        (bt (min (rect-bottom a) (rect-bottom b))))
    (when (and (< x r) (< y bt))
      (make-rect x y (- r x) (- bt y)))))

(defun rect-inset (r amount &optional (vertical amount))
  "R shrunk by AMOUNT on the left and right and VERTICAL top and bottom.

Negative values grow it.  Never returns a rectangle with negative extent."
  (make-rect (+ (rect-x r) amount)
             (+ (rect-y r) vertical)
             (max 0 (- (rect-w r) (* 2 amount)))
             (max 0 (- (rect-h r) (* 2 vertical)))))

(defun rect-center (r)
  "Two values: the X and Y of R's centre point."
  (values (+ (rect-x r) (floor (rect-w r) 2))
          (+ (rect-y r) (floor (rect-h r) 2))))

(defun divide-rect (r axis weights &key (gap 0))
  "Cut R into (LENGTH WEIGHTS) sub-rectangles along AXIS in proportion to WEIGHTS.

AXIS is :HORIZONTAL — children side by side, cut along X — or :VERTICAL —
children stacked, cut along Y.  GAP pixels are left between adjacent children
and are *not* given to anybody.

Rounding is accumulated rather than applied per child, so the pieces always
tile R exactly: no one-pixel seam appears between panes, and the last child
absorbs the remainder.  This is load-bearing.  A layout that loses a pixel per
split visibly frays after four or five splits."
  (let* ((n (length weights)))
    (when (zerop n) (return-from divide-rect '()))
    (let* ((horizontal (eq axis :horizontal))
           (total-gap (* gap (1- n)))
           (extent (max 0 (- (if horizontal (rect-w r) (rect-h r)) total-gap)))
           (sum (reduce #'+ weights))
           (sum (if (plusp sum) sum 1))
           (origin (if horizontal (rect-x r) (rect-y r)))
           (acc 0)
           (used 0)
           (out '()))
      (loop for i from 0 below n
            for weight in weights
            do (incf acc weight)
               (let* ((edge (if (= i (1- n))
                                extent
                                (round (* extent acc) sum)))
                      (size (max 0 (- edge used)))
                      (start (+ origin used (* i gap))))
                 (push (if horizontal
                           (make-rect start (rect-y r) size (rect-h r))
                           (make-rect (rect-x r) start (rect-w r) size))
                       out)
                 (setf used edge)))
      (nreverse out))))

;;; ------------------------------------------------------------- directions

(defparameter +directions+ '(:left :right :up :down)
  "The four spatial directions every motion and structure verb takes.

They are the whole vocabulary: a verb crossed with a direction crossed with a
scope is how the keymap is generated, rather than by enumerating commands.")

(defun direction-p (x)
  "True when X names a spatial direction."
  (and (member x +directions+) t))

(defun direction-axis (direction)
  "The axis DIRECTION travels along: :HORIZONTAL or :VERTICAL."
  (ecase direction
    ((:left :right) :horizontal)
    ((:up :down) :vertical)))

(defun direction-sign (direction)
  "-1 when DIRECTION decreases its coordinate, +1 when it increases it."
  (ecase direction
    ((:left :up) -1)
    ((:right :down) 1)))

(defun direction-horizontal-p (direction)
  "True for :LEFT and :RIGHT."
  (eq (direction-axis direction) :horizontal))

(defun direction-vertical-p (direction)
  "True for :UP and :DOWN."
  (eq (direction-axis direction) :vertical))

(defun opposite-direction (direction)
  "The direction facing back the way DIRECTION came.

Used by entry resolution: leaving a container rightwards means entering the
next one through its *left* edge, which is what makes motion involutive —
right then left returns you exactly where you started (DESIGN D20)."
  (ecase direction
    (:left :right) (:right :left) (:up :down) (:down :up)))

(defun direction-for (axis sign)
  "The direction travelling along AXIS with SIGN (-1 or +1)."
  (ecase axis
    (:horizontal (if (minusp sign) :left :right))
    (:vertical (if (minusp sign) :up :down))))
