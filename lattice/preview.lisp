;;;; lattice/preview.lisp --- The whole session, in miniature, before you go.
;;;;
;;;; #23 ASKS FOR THUMBNAILS, AND THIS PROGRAM CANNOT TAKE ONE.  A preview in
;;;; the Mission-Control sense needs the pixels of somebody else's window, which
;;;; means screencopy -- wlr-screencopy or ext-image-copy-capture -- and this
;;;; program speaks neither: doc/FINDINGS.org records the day grim was the
;;;; capture client, because *it* speaks screencopy and we do not.  Adding a
;;;; protocol, an shm-buffer pool, a frame clock and a pixel conversion to a
;;;; window manager that draws with a bitmap font is not a preview; it is a
;;;; second renderer.  So the thumbnail half of the issue is filed separately
;;;; and the half that does not need pixels ships here.
;;;;
;;;; AND THE HALF THAT DOES NOT NEED PIXELS IS THE ONE A TILER WANTS.  In a
;;;; tiling window manager the question is never `which of these is Firefox' --
;;;; a terminal at 200 pixels is unreadable no matter whose pixels they are.
;;;; The question is *what will my screen look like when I go there*, which is a
;;;; question about composition: how many cells, which of them are taken, what
;;;; is in them.  That is a drawing of the model, and the model is what the map
;;;; already draws.
;;;;
;;;; SO THE PREVIEW IS ONE PANEL PER WORKSPACE.  Every workspace in the session,
;;;; with its plane in miniature and the windows named, in reading order; the
;;;; one you are on is outlined.  Click a panel and you are there.  Press
;;;; anything and it is gone -- the same ruling the peek made, for the same
;;;; reason: a glance you cannot get out of is a mode, and this is not one.

(in-package #:lattice)

(p:define-option *preview-background* '(24 24 30 255)
  "The colour a preview panel's plane is drawn on.  RGBA, 0-255.")

(p:define-option *preview-panel-color* '(38 40 50 255)
  "The colour of a preview panel's frame.  RGBA, 0-255.")

(p:define-option *preview-here-color* '(210 190 90 255)
  "The colour of the frame round the workspace you are standing on.

The one thing a wall of panels must not leave you guessing about, and the reason
it is a different colour rather than a thicker line: at eighteen workspaces the
frames are two pixels apart and width stops reading.")

(defvar *preview* nil
  "True while the session is being previewed.

One flag, like the peek's: the panels are derived from the area and the
workspaces, so remembering them would be a second copy of the truth that can
disagree with what is drawn.")

(defun preview-active-p ()
  "True while the session is shown in miniature."
  (and *preview* t))

(defun preview-begin ()
  "Show every workspace over everything.  Moves no window and no camera."
  (prog1 (setf *preview* t)
    (r:mark-dirty)))

(defun preview-end ()
  "Take the preview down.  Answers T when there was one to take down."
  (when *preview*
    (setf *preview* nil)
    (r:mark-dirty)
    t))

;;; --------------------------------------------------------------- the panels

(defun preview-columns (count)
  "How many panels go across: as square a wall as the count allows.

Square because that is the shape every overview has ever been, and because a
single row of eighteen panels is a strip of stamps rather than a map."
  (max 1 (ceiling (sqrt (max 1 count)))))

(defun preview-panels (count area &key (margin 12))
  "Panels for COUNT workspaces inside AREA, as an alist of (INDEX . RECT).

Pure arithmetic, which is the whole point: the panel a click lands in and the
panel drawn at a point are worked out by one function, so they cannot disagree
-- the rule CELL-RECTS and MAP-CELL-AT already follow."
  (when (plusp count)
    (let* ((cols (preview-columns count))
           (rows (ceiling count cols))
           (panel-w (max 1 (floor (- (c:rect-w area) (* margin (1+ cols))) cols)))
           (panel-h (max 1 (floor (- (c:rect-h area) (* margin (1+ rows))) rows))))
      (loop for index from 0 below count
            for col = (mod index cols)
            for row = (floor index cols)
            collect (cons index
                          (c:make-rect (+ (c:rect-x area) margin (* col (+ panel-w margin)))
                                       (+ (c:rect-y area) margin (* row (+ panel-h margin)))
                                       panel-w panel-h))))))

(defun preview-panel-at (panels x y)
  "The workspace index whose panel covers (X, Y), or NIL."
  (loop for (index . rect) in panels
        when (c:rect-contains-p rect x y) return index))

;;; -------------------------------------------------------------- the drawing

(defun draw-preview-on (output)
  "Draw the wall of workspaces on OUTPUT, or take it down there."
  (let* ((policy (p:current-policy))
         (overlay (r:overlay-for :lattice/preview output))
         (root (and r:*world* (c:world-root r:*world*))))
    (unless (and root output (preview-active-p))
      (r:overlay-hide overlay)
      (return-from draw-preview-on nil))
    (let ((area (p:outer-rect policy output)))
      (let ((canvas (r:ensure-overlay overlay (c:rect-w area) (c:rect-h area)))
            (count (if (typep root 'c:stack) (c:container-count root) 1))
            (here (p:output-workspace output)))
        (when canvas
          (r:canvas-fill canvas (apply #'r:argb *preview-background*))
          (loop for (index . rect) in (preview-panels count area)
                for box = (c:make-rect (- (c:rect-x rect) (c:rect-x area))
                                       (- (c:rect-y rect) (c:rect-y area))
                                       (c:rect-w rect) (c:rect-h rect))
                for node = (c:child-at root index)
                do (draw-preview-panel canvas box node index here))
          (r:overlay-commit overlay :rect area))))))

(defun draw-preview-panel (canvas box node index here)
  "One workspace, in BOX: its number, its frame, and its plane in miniature."
  (let ((frame (if (eql index here) *preview-here-color* *preview-panel-color*)))
    (r:canvas-fill canvas (apply #'r:argb *preview-panel-color*) box)
    (r:canvas-rect canvas box (apply #'r:argb frame) :width (if (eql index here) 3 1))
    (r:canvas-text canvas (+ (c:rect-x box) 6) (+ (c:rect-y box) 4)
                   (format nil "~d" (1+ index))
                   (apply #'r:argb (if (eql index here) *preview-here-color*
                                       *map-text-color*)))
    (let ((inner (c:make-rect (+ (c:rect-x box) 6)
                              (+ (c:rect-y box) 4 (r:text-height :scale 1) 4)
                              (max 0 (- (c:rect-w box) 12))
                              (max 0 (- (c:rect-h box) 12
                                        (r:text-height :scale 1) 4)))))
      (unless (or (null node) (c:node-empty-p node)
                  (< (c:rect-w inner) 8) (< (c:rect-h inner) 8))
        (if (typep node 'grid)
            (draw-preview-plane canvas inner node)
            ;; A workspace that is not a plane -- before the lattice arrived, or
            ;; after it was turned off -- is drawn as the one box it is, with
            ;; what it holds.  An overview that refuses to draw what it does not
            ;; recognise is an overview that lies by omission.
            (draw-preview-cell canvas inner (window-summary node)))))))

(defun draw-preview-plane (canvas inner grid)
  "GRID's occupied cells, scaled to fit INNER."
  (let ((cells (occupied-cells grid)))
    (when cells
      (let* ((min-x (reduce #'min cells :key #'cell-x))
             (max-x (reduce #'max cells :key #'cell-x))
             (min-y (reduce #'min cells :key #'cell-y))
             (max-y (reduce #'max cells :key #'cell-y))
             (cols (1+ (- max-x min-x)))
             (rows (1+ (- max-y min-y)))
             (w (max 1 (floor (c:rect-w inner) cols)))
             (h (max 1 (floor (c:rect-h inner) rows))))
        (dolist (address cells)
          (let ((box (c:make-rect (+ (c:rect-x inner) (* (- (cell-x address) min-x) w))
                                  ;; Lattice +Y is up, the canvas's is down: the
                                  ;; same sign inversion CELL-RECTS owns, in the
                                  ;; one other place that draws cells.
                                  (+ (c:rect-y inner) (* (- max-y (cell-y address)) h))
                                  w h))
                (node (c:child-at grid address)))
            (draw-preview-cell canvas box (window-summary node))))))))

(defun draw-preview-cell (canvas box summary)
  "One occupied cell's miniature: a filled box with as much of SUMMARY as fits."
  (r:canvas-fill canvas (apply #'r:argb *map-cell-color*) box)
  (let ((room (c:rect-w box)))
    (when (and (> room 24) (> (c:rect-h box) (r:text-height :scale 1)))
      (r:canvas-text canvas (+ (c:rect-x box) 2) (+ (c:rect-y box) 1)
                     (r:truncate-text summary
                                      (max 1 (floor (- room 4) (r:text-width "m"))))
                     (apply #'r:argb *map-detail-color*)))))

(r:add-hook :draw-overlays 'draw-preview)

(defun draw-preview ()
  "Draw the preview on every output, or take it down everywhere.

Every output rather than the cursor's: the wall is the whole session and does
not depend on which screen you are looking from, so showing it on one monitor
and not the other would be a preview you cannot see from the keyboard you are
not at."
  (dolist (output (r:all-outputs))
    (r:guarded "preview" (draw-preview-on output))))

;;; ------------------------------------------------------------- the press

(defun preview-claim-press (policy world x y)
  "Claim a press on a preview panel: that panel is the workspace you asked for.

Asked BEFORE the map's claim and the plane's, because the wall is on top of
everything by construction: a cell drawn inside a panel is a picture of a cell,
not a cell you can be moved into from here.  Defined as the seam PAN.LISP
declares, rather than as a second P:POINTER-PRESS method -- two methods on one
specializer list are one method, and the loser is silently deleted."
  (when (preview-active-p)
    (let ((output (r:output-at x y)))
      (when output
        (let* ((area (p:outer-rect policy output))
               (root (c:world-root world))
               (count (if (typep root 'c:stack) (c:container-count root) 1))
               (index (preview-panel-at (preview-panels count area) x y)))
          (when index
            (preview-end)
            (when (typep root 'c:stack)
              (r:show-workspace root index)
              (r:mark-dirty))
            t))))))

;;; -------------------------------------------------------------- the verbs

(r:defcommand preview ()
  "Show every workspace in miniature, over everything.

One panel per workspace, its plane drawn small, the one you are on outlined;
click a panel to go there, or press anything and carry on.  Nothing is resized
and no window moves: this is the model drawn, which is why it costs nothing and
why it can show workspaces that are not on any screen."
  (if (preview-active-p) (preview-end) (preview-begin)))

(r:defcommand preview-off ()
  "Take the preview down, if it is up."
  (preview-end))
