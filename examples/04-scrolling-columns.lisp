;;;; examples/04-scrolling-columns.lisp
;;;;
;;;; TIER 3 — a new container kind, from outside.  The hardest thing the
;;;; extension surface claims to support, and the one the lattice is the
;;;; large-scale version of.
;;;;
;;;; This is niri and PaperWM: an infinite horizontal strip of columns, of which
;;;; a window's worth is on screen at a time, scrolling sideways as you move.
;;;; It is the closest living relative of the lattice — DESIGN calls niri "what
;;;; it gets wrong is the sharpest available spec input" — and it is the lattice
;;;; with one axis instead of two.
;;;;
;;;; Sixty lines.  Nothing under src/ or lattice/ is touched.  If you want to
;;;; understand how to add a layout model to this window manager, read this file
;;;; rather than lattice/, because it is the same shape at a tenth of the size.
;;;;
;;;;     (load "examples/04-scrolling-columns.lisp")
;;;;     (scrolling)

(in-package #:latticewm/user)

;;; ------------------------------------------------- the container

(defclass strip (sequential-container)
  ((offset :initform 0 :accessor strip-offset
           :documentation "Index of the leftmost column on screen.")
   (visible :initarg :visible :initform 2 :accessor strip-visible
            :documentation "How many columns fit on screen at once."))
  (:documentation
   "An unbounded horizontal strip of columns, scrolled by the viewport.

Subclasses SEQUENTIAL-CONTAINER, which supplies the ordered-list machinery —
addresses, insertion, removal — so the only thing left to answer is how it is
*arranged*, which is the point."))

(defmethod default-address ((strip strip))
  "Focus repair lands on the leftmost visible column, never offscreen."
  (min (strip-offset strip) (max 0 (1- (container-count strip)))))

(defmethod simplify-node ((strip strip))
  "A strip survives as a singleton; an empty one regains a column.

Same reasoning as a workspace stack: an empty strip is not a simpler state, it
is a broken one."
  (when (zerop (container-count strip))
    (setf (children strip) (list (make-leaf))))
  strip)

;;; ------------------------------------------------- how it is drawn

(defmethod layout-children ((policy conventional-policy) (strip strip) rect)
  "Draw the visible window of columns, side by side, equally wide.

Everything outside the window is omitted, so the conventional layout driver
walks it and hides it — which is not optional, since river shows a window
unless told otherwise."
  (let* ((n (container-count strip))
         (visible (max 1 (strip-visible strip)))
         (start (max 0 (min (strip-offset strip) (max 0 (- n visible)))))
         (end (min n (+ start visible)))
         (addresses (loop for i from start below end collect i)))
    (when addresses
      (mapcar #'cons addresses
              (divide-rect rect :horizontal
                           (make-list (length addresses) :initial-element 1)
                           :gap (gaps policy strip))))))

;;; ------------------------------------------------- how it is navigated

(defmethod step-address ((policy conventional-policy) (strip strip)
                         address direction)
  "Left and Right move along the strip, scrolling it to follow.

The scroll is the whole feature: moving past the right edge shifts the window
of visible columns by one rather than refusing, which is what makes the strip
feel unbounded instead of merely wide."
  (when (direction-horizontal-p direction)
    (let ((next (+ address (direction-sign direction)))
          (n (container-count strip)))
      (when (and (<= 0 next) (< next n))
        ;; Scroll minimally to bring the target on screen.
        (let ((visible (max 1 (strip-visible strip))))
          (cond ((< next (strip-offset strip))
                 (setf (strip-offset strip) next))
                ((>= next (+ (strip-offset strip) visible))
                 (setf (strip-offset strip) (- next visible -1)))))
        next))))

(defmethod entry-address ((policy conventional-policy) (strip strip)
                          direction reference rects)
  (declare (ignore reference rects))
  (let ((n (container-count strip)))
    (when (plusp n)
      (if (eq direction :left) (1- n) (strip-offset strip)))))

;;; ------------------------------------------------- verbs

(defcommand strip-width (columns)
  "Show COLUMNS columns at a time.  This is the strip's version of zoom."
  (let ((strip (find-if (lambda (node) (typep node 'strip))
                        (resolve-chain (world-root *world*) (current-path)))))
    (when strip
      (setf (strip-visible strip) (max 1 columns))
      (relayout))))

(defcommand scrolling (&optional (visible 2))
  "Turn the current workspace into a scrolling strip of columns.

Every window that was in it becomes a column, in the order it was laid out —
so nothing is lost and nothing is reordered."
  (let* ((world *world*)
         (path (workspace-path world))
         (workspace (resolve-path (world-root world) path))
         (columns (mapcar (lambda (leaf-path)
                            (resolve-path workspace leaf-path))
                          (leaf-paths workspace))))
    (setf (world-root world)
          (tree-replace-at (world-root world) path
                           (make-instance 'strip :children columns
                                                 :visible visible)))
    (setf (world-cursor world) (repair-path (world-root world) path))
    (relayout :force t)))
