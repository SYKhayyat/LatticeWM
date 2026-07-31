;;;; lattice/overlay.lisp --- Coordinates drawn on the cells.  README D5.
;;;;
;;;; The design's own assessment of the risk this answers:
;;;;
;;;;   "This is still the single biggest design risk.  Two dimensions is more
;;;;   than twice as hard to hold in your head as niri's one.  Design against
;;;;   it from day one: minimap, named cells, and the coordinate overlay."
;;;;
;;;; And D5 itself: "Cells display their coordinate (0,0, 5,-2) as an overlay.
;;;; This is what makes the lattice legible and is a direct countermeasure to
;;;; the ZUI get-lost failure mode."
;;;;
;;;; The echo area already says where the *cursor* is, which is most of the
;;;; value for one cell.  This is the other half: at 2x2 and beyond, knowing
;;;; where every *other* cell is, so that a move can be planned rather than
;;;; discovered.  It is also where a cell's name appears once it has one, which
;;;; is D1's third addressing layer and the one humans actually remember.
;;;;
;;;; DRAWN INCREMENTALLY, and that is not premature.  A full-screen ARGB clear
;;;; is two million writes; at a relayout per keystroke that is a visible cost
;;;; for a few dozen small labels.  So the overlay remembers the rectangles it
;;;; drew last time, clears exactly those, and draws the new ones.

(in-package #:lattice)

(p:define-option *overlay-scale* 1
  "Integer scale factor for coordinate labels.  2 on a HiDPI display.")

(p:define-option *overlay-background* '(0.10 0.10 0.14 0.82)
  "Background of a coordinate label, as (R G B A).")

(p:define-option *overlay-foreground* '(0.80 0.84 0.92 1.0)
  "Text colour of a coordinate label.")

(p:define-option *overlay-min-cell* 140
  "Do not label a cell narrower than this many pixels.

Below it the label is a larger fraction of the cell than the cell's contents
are, which is the point at which an orientation aid becomes clutter.")

(defvar *overlay* nil)
(defvar *overlay-dirty* '() "Rectangles drawn last time, to be cleared.")

(defun overlay-wanted-p (grid)
  "Should the coordinate overlay be drawn at all right now?"
  (and grid
       (ecase *coordinate-overlay*
         (:never nil)
         (:always t)
         (:zoomed-out (let ((viewport (grid-viewport grid)))
                        (> (* (viewport-cols viewport) (viewport-rows viewport))
                           1))))))

(defun cell-label (grid address)
  "What to write on the cell at ADDRESS: its name if it has one, else its
coordinate.

The name wins because D1 is explicit that names are \"the layer humans actually
remember\" and coordinates are \"the escape hatch, not the interface\".  The
coordinate is still one keystroke away in the echo area."
  (let ((node (c:child-at grid address)))
    (or (and node (c:node-label node))
        (cell-string address))))

(defun draw-coordinate-overlay ()
  "Label every visible cell.  Called from the runtime's :DRAW-OVERLAYS hook."
  (let* ((grid (current-grid))
         (output (first (r:all-outputs)))
         (policy (p:current-policy)))
    (unless (and output (typep policy 'lattice-policy))
      (return-from draw-coordinate-overlay nil))
    (unless (overlay-wanted-p grid)
      (when *overlay* (r:overlay-hide *overlay*))
      (setf *overlay-dirty* '())
      (return-from draw-coordinate-overlay nil))
    (unless *overlay*
      (setf *overlay* (make-instance 'r:overlay :name "coordinates")))
    (let* ((area (p:outer-rect policy output))
           (canvas (r:ensure-overlay *overlay* (c:rect-w (c:output-rect output))
                                     (c:rect-h (c:output-rect output)))))
      (when canvas
        ;; Clear only what we drew last time.  A full-screen clear is two
        ;; million writes for a few dozen small labels.
        (dolist (rect *overlay-dirty*) (r:canvas-fill canvas 0 rect))
        (setf *overlay-dirty* '())
        (let ((background (apply #'r:argb *overlay-background*))
              (foreground (apply #'r:argb *overlay-foreground*))
              (origin-x (c:rect-x (c:output-rect output)))
              (origin-y (c:rect-y (c:output-rect output)))
              (current (current-cell)))
          (loop for (address . rect) in (cell-rects policy grid area)
                when (and (c:child-at grid address)
                          (>= (c:rect-w rect) *overlay-min-cell*))
                  do (let* ((text (cell-label grid address))
                            (width (+ 10 (r:text-width text :scale *overlay-scale*)))
                            (height (+ 6 (r:text-height :scale *overlay-scale*)))
                            ;; Bottom-left of the cell: the top-left corner is
                            ;; where applications put their own titles, and a
                            ;; label that covers one is worse than no label.
                            (box (c:make-rect (- (c:rect-x rect) origin-x)
                                              (- (c:rect-bottom rect) origin-y height)
                                              width height)))
                       (r:canvas-fill canvas background box)
                       (when (and current (cell-equal address current))
                         (r:canvas-rect canvas box
                                        (apply #'r:argb *overlay-foreground*)
                                        :width 1))
                       (r:canvas-text canvas (+ (c:rect-x box) 5)
                                      (+ (c:rect-y box) 3) text foreground
                                      :scale *overlay-scale*)
                       (push box *overlay-dirty*))))
        (r:overlay-commit *overlay* :rect (c:output-rect output))))))

(r:add-hook :draw-overlays #'draw-coordinate-overlay)
