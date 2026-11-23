;;;; lattice/overlay.lisp --- Coordinates drawn on the cells.  DESIGN D5.
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

(defun overlay-wanted-p (grid rect)
  "Should the coordinate overlay be drawn at all right now?

Not when the drawn map is up: the map labels its own cells, and two overlays
labelling the same cell put the coordinate on it twice, in two places, which
looks like a rendering fault rather than emphasis."
  (and grid
       (not (map-mode-p grid rect))
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

(defun float-over-rect-p (rect)
  "Is any floating window lying on RECT?

The render index places an anchored float against its anchor's rectangle and a
free one not at all, so this asks FLOAT-PLACED-RECT rather than reading either
kind inline -- and a float that cannot be placed is a float that is not on
screen, which is not a reason to label the cell under its old rectangle.
Minimized floats are hidden by the emitter and are not on screen either."
  (let ((world r:*world*)
        (index (and r:*world* (c:prop r:*world* :rect-index))))
    (and world
         (some (lambda (record)
                 (let ((window (c:float-window record))
                       (placed (c:float-placed-rect record index)))
                   (and placed window
                        (c:window-live-p window)
                        (not (c:window-minimized-p window))
                        (c:rect-intersect placed rect))))
               (c:world-floats world)))))

(defun cell-holds-something-p (grid address rect)
  "Is there anything to label on the cell at ADDRESS, whose rectangle is RECT?

THE TREE IS NOT THE WHOLE MODEL (#145, #138).  This used to be the tree's
answer alone -- is there a child at this address -- and the tree gets the
question wrong in both directions:

  - A cell showing only a floating window has no child.  Floats live beside the
tree, so the cell a reader is most likely asking about, the one with a dialog
on it, was the one cell with no coordinate on it.
  - An empty leaf and an empty hole are the same empty cell, and the tree calls
one a child and the other nothing.  So a cell you floated a window out of was
labelled and a cell you closed a window in was not, with nothing on screen to
tell you why.

So the question becomes what it was always meant to be: is anything here?  A
node with a window in it, a node somebody named -- the name is what the label
says, and a name is an address you can jump to -- or a floating window on it."
  (let ((node (c:child-at grid address)))
    (or (and node (or (not (c:node-empty-p node)) (c:node-label node)))
        (float-over-rect-p rect))))

(defun draw-coordinate-overlay ()
  "Label every visible cell, on every output.

Called from the runtime's :DRAW-OVERLAYS hook.  Per output because the labels
have to land on the cells they name, and the cells are wherever the layout put
them — a single surface pinned to the first output drew the second monitor's
coordinates onto the first, at positions that meant nothing there."
  (dolist (output (r:all-outputs))
    (p:guarded "coordinates" (draw-coordinate-overlay-on output))))

(defun draw-coordinate-overlay-on (output)
  "Label the visible cells lying on OUTPUT."
  (let* ((grid (current-grid))
         (policy (p:current-policy))
         (overlay (r:overlay-for :lattice/coordinates output)))
    (unless (and output (typep policy 'lattice-mixin))
      (return-from draw-coordinate-overlay-on nil))
    (unless (overlay-wanted-p grid (p:outer-rect policy output))
      (r:overlay-hide overlay)
      (return-from draw-coordinate-overlay-on nil))
    ;; THE AREA THE CELLS ARE IN, for the surface as well as for the cells.
    ;; This drew into an output-sized canvas committed at the output's own
    ;; rectangle, which is the same mismatch DRAW-MAP-ON had — there it painted
    ;; over the echo area, and here it was invisible only because the labels are
    ;; small and everything between them is transparent.  A latent version of a
    ;; bug that has already been paid for once is worth removing rather than
    ;; leaving for the day somebody adds a background to it.
    (let* ((area (p:outer-rect policy output))
           (canvas (r:ensure-overlay overlay (c:rect-w area) (c:rect-h area))))
      (when canvas
        ;; ENSURE-OVERLAY hands the canvas back already cleared of the frame it
        ;; was holding, and of nothing else: a full-screen clear is two million
        ;; writes for a few dozen small labels.  The record is kept per buffer
        ;; rather than per overlay, which is what makes that correct now there
        ;; is more than one buffer to hold a frame.
        (let ((background (apply #'r:argb *overlay-background*))
              (foreground (apply #'r:argb *overlay-foreground*))
              (origin-x (c:rect-x area))
              (origin-y (c:rect-y area))
              (current (current-cell))
              (drawn '()))
          (loop for (address . rect) in (cell-rects policy grid area)
                when (and (>= (c:rect-w rect) *overlay-min-cell*)
                          (cell-holds-something-p grid address rect))
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
                       (push box drawn)))
          ;; What was drawn: the next frame into this buffer clears exactly
          ;; these, and the compositor is told exactly these changed.
          (r:overlay-drew overlay drawn))
        (r:overlay-commit overlay :rect area)))))

(r:add-hook :draw-overlays 'draw-coordinate-overlay)
