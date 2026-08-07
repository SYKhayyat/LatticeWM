;;;; lattice/map.lisp --- The drawn map at deep zoom.
;;;;
;;;; This answers the last open question in DESIGN, which had a full proposal
;;;; on the table and was awaiting a ruling.  *The ruling is: implement it, and
;;;; the argument against it is answered rather than accepted.*
;;;;
;;;; THE PROBLEM, which is forced by the protocol and not a design choice.
;;;; There is no scaling primitive anywhere in river — no scale, no transform,
;;;; no matrix, no buffer_scale.  So "zoom out" cannot mean "draw the same
;;;; windows smaller"; it can only mean "give every window smaller dimensions".
;;;; Past some point that is not a smaller view of your desktop, it is a
;;;; *reflow* of your desktop: terminals rewrap, browsers relayout, and doing it
;;;; twice per peek to check where something is costs more than the peek is
;;;; worth.
;;;;
;;;; THE PROPOSAL, from DESIGN's open questions: "live windows out to a
;;;; configurable pixel threshold, then stop showing real windows and draw the
;;;; lattice ourselves — cell rectangles, coordinates, cell names, window
;;;; titles, app colours."  Its stated argument for is that the map is *free*:
;;;; nothing resizes to enter or leave it, so hold-to-peek is instant.
;;;;
;;;; THE ARGUMENT AGAINST, and why it does not survive contact.  DESIGN says:
;;;; "at deep zoom you are looking at a diagram of your desktop rather than your
;;;; desktop.  It is a mode, and the original thesis was that zoom is never a
;;;; mode."
;;;;
;;;; That is the right worry aimed at the wrong thing.  What the thesis objects
;;;; to is an overview you *enter and leave* with a command — GNOME's, niri's —
;;;; because then there are two states and you have to know which one you are
;;;; in.  Here there is one control, it does one thing, and it does it
;;;; continuously; the rendering changes at a threshold the way a map changes
;;;; from streets to roads as you zoom out.  Nobody thinks Google Maps has a
;;;; mode.  You never enter the map and you cannot get stuck in it: zoom in and
;;;; it is gone.
;;;;
;;;; And the alternative is not "no mode", it is "cells so small the windows in
;;;; them are unreadable *and* reflowed twice for the privilege".  That is
;;;; worse on both counts.

(in-package #:lattice)

(p:define-option *map-threshold* 320
  "Cell width in pixels below which the plane is drawn rather than rendered.

Zoom out past this and the cells stop showing real windows and start showing a
map of themselves: coordinate, name, and what is in them.  Zoom back in and the
windows return, having never been resized.

DESIGN's proposal suggested 600, on figures that predate exact-fit zoom and
assume a 1x1 base layout.  320 is lower because in practice a 320-pixel-wide
terminal is still *recognisable* — you can see which one is your editor — and
recognisable is what a peek is for.  Below that it is coloured mush and the map
is strictly better.

Set it to 0 to keep real windows all the way out, which is the behaviour this
option exists to let you choose.")

(p:define-option *map-background* '(0.06 0.06 0.09 1.0)
  "Background of the drawn map.")

(p:define-option *map-cell-color* '(0.16 0.17 0.22 1.0)
  "Fill of an occupied cell on the drawn map.")

(p:define-option *map-empty-color* '(0.10 0.10 0.13 1.0)
  "Fill of an empty cell on the drawn map.")

(p:define-option *map-text-color* '(0.72 0.76 0.85 1.0)
  "Coordinate and name colour on the drawn map.")

(p:define-option *map-detail-color* '(0.48 0.52 0.62 1.0)
  "Colour of the what-is-in-this-cell line on the drawn map.

Dimmer than the coordinate, because the coordinate is what you are navigating
by and the contents are what confirm you picked the right cell.")

(defun map-mode-p (grid rect)
  "Should the plane be drawn rather than rendered, at this size?

ASKING CELL-TRACKS RATHER THAN DIVIDING THE OUTPUT.  This used to be
RECT-W / COLS, which is the width of a cell under :FIT and is nothing at all
under :FIXED, where a cell is *CELL-WIDTH* pixels however many of them the
viewport is asking for.  So a :FIXED lattice zoomed to eight columns computed
240 pixels for cells that were 960, decided they were too small to bother
rendering, and put the map up over full-size windows.

The mean rather than the narrowest track, deliberately: the question is whether
cells are worth rendering at this zoom level, and one deliberately narrow column
beside four wide ones is not that.  Under :FIT the mean is the division that used
to be here, short by the gaps, so the behaviour this shipped with is unchanged
except at a threshold set within a few pixels of a boundary."
  (and grid (plusp *map-threshold*)
       (let ((columns (cell-tracks grid rect :horizontal)))
         (and columns
              (< (floor (reduce #'+ (mapcar #'c:rect-w columns)) (length columns))
                 *map-threshold*)))))

(defmethod p:layout-children :around ((policy lattice-mixin) (grid grid) rect)
  "Place nothing at all once the cells are too small to be worth rendering.

Returning no placements is what puts the windows away: the layout driver walks
the addresses it did not place and marks them invisible, and the emitter hides
what is invisible.  So entering the map costs one hide per window and leaving
it costs one show — and *no window is ever resized*, which is the entire point.

An :AROUND method because the decision is about whether to lay out at all,
which is a different question from how; the normal method is untouched and
still does the only thing it knows how to do."
  (if (map-mode-p grid rect)
      '()
      (call-next-method)))

(defun window-summary (node)
  "A short description of what is in a cell: what the windows are called.

Through P:NAME-OF-WINDOW rather than reading the app id, so a window rule's
:LABEL reaches the map as well as the status line.  One question asked in one
place is the whole of that decision -- see P:WINDOW-NAME."
  (let ((names (remove nil (mapcar #'p:name-of-window (c:node-windows node)))))
    (cond ((null names) "")
          ((null (rest names)) (first names))
          (t (format nil "~a +~d" (first names) (1- (length names)))))))

(defun draw-map ()
  "Draw the plane on every output whose cells are too small to render.

Per output, because the decision is per output: two monitors of different sizes
showing the same viewport cross the threshold at different zoom levels, and the
one with room for real windows should keep them."
  (dolist (output (r:all-outputs))
    (p:guarded "map" (draw-map-on output))))

(defun draw-map-on (output)
  "Draw the plane on OUTPUT, or take the map down there.

THE SURFACE IS THE AREA THE CELLS WERE LAID OUT IN, and getting that wrong is
the whole of a bug that reached a real screen.  The cells go in OUTER-RECT —
which is the output less the gaps and less anything a panel or the echo area
reserved — and the canvas was sized and committed at OUTPUT-RECT, the whole
display.  So the background fill covered the strip the layout had deliberately
left alone, and *the window manager's own status line was blank for as long as
the map was up*.

It was not a layer-shell problem, which is what made the first reading of it
wrong: a panel sits on a higher layer and survives the map intact.  It was this
program covering its own echo area, and the tell was internal to this function
— AREA and the canvas rectangle disagreed, and the code already knew which one
was right, because it used it for the cells.

The contrast with OVERLAY-WANTED-P is the instructive part.  That function
suppresses the *coordinate* overlay under the map and spends four lines saying
why.  Nothing said the status line should go, because nothing decided it
should."
  (let* ((grid (current-grid))
         (policy (p:current-policy))
         (overlay (r:overlay-for :lattice/map output)))
    (unless (and grid output (typep policy 'lattice-mixin))
      (r:overlay-hide overlay)
      (return-from draw-map-on nil))
    (let ((area (p:outer-rect policy output)))
      (unless (map-mode-p grid area)
        (r:overlay-hide overlay)
        (return-from draw-map-on nil))
      (let ((canvas (r:ensure-overlay overlay (c:rect-w area) (c:rect-h area))))
        (when canvas
          (r:canvas-fill canvas (apply #'r:argb *map-background*))
          (let ((origin-x (c:rect-x area))
                (origin-y (c:rect-y area))
                (text-color (apply #'r:argb *map-text-color*))
                (current (current-cell)))
            (loop for (address . rect) in (cell-rects policy grid area)
                  for node = (c:child-at grid address)
                  for occupied = (and node (not (c:node-empty-p node)))
                  for box = (c:make-rect (- (c:rect-x rect) origin-x)
                                         (- (c:rect-y rect) origin-y)
                                         (c:rect-w rect) (c:rect-h rect))
                  do (r:canvas-fill canvas
                                    (apply #'r:argb (if occupied
                                                        *map-cell-color*
                                                        *map-empty-color*))
                                    box)
                     ;; The border is the cell's own coordinate colour, so the
                     ;; map and the live view agree about which cell is which.
                     (multiple-value-bind (r g b a)
                         (p:border-color policy (or node (c:make-leaf))
                                         (and current (cell-equal address current)))
                       (r:canvas-rect canvas box (r:argb r g b a)
                                      :width (if (and current
                                                      (cell-equal address current))
                                                 3 1)))
                     (when (> (c:rect-h box) 30)
                       (let ((label (cell-label grid address)))
                         (r:canvas-text canvas (+ (c:rect-x box) 6)
                                        (+ (c:rect-y box) 5) label text-color)))
                     (when (and occupied (> (c:rect-h box) 52)
                                (> (c:rect-w box) 80))
                       (r:canvas-text canvas (+ (c:rect-x box) 6)
                                      (+ (c:rect-y box) 5
                                         (r:text-height :scale 1) 4)
                                      (r:truncate-text
                                       (window-summary node)
                                       (max 0 (floor (- (c:rect-w box) 12)
                                                     (r:text-width "m"))))
                                      (apply #'r:argb *map-detail-color*)))))
          (r:overlay-commit overlay :rect area))))))

(r:add-hook :draw-overlays 'draw-map)
