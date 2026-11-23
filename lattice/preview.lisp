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
    (r:mark-dirty)
    (p:run-hooks :preview-changed t)))

(defun preview-end ()
  "Take the preview down.  Answers T when there was one to take down."
  (when *preview*
    (setf *preview* nil)
    (r:mark-dirty)
    (p:run-hooks :preview-changed nil)
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
                do (draw-preview-panel canvas box node index here
                                        (floats-of node r:*world*)
                                        (and (eql index here)
                                             (free-floats-of r:*world*))))
          (r:overlay-commit overlay :rect area))))))

(defun draw-preview-panel (canvas box node index here &optional floats free-floats)
  "One workspace, in BOX: its number, its frame, its plane in miniature and its
floats.

FLOATS are the ones anchored in this workspace's tree, drawn inside the cell
their anchor lives in.  FREE-FLOATS are the output-pinned ones, passed only for
the panel you are standing on -- they are over that plane and no other -- and
named at the bottom of it.

A panel says there is a float because an overview that leaves it out answers the
question what is on this workspace with a number that is too low (#138)."
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
            (draw-preview-plane canvas inner node floats)
            ;; A workspace that is not a plane -- before the lattice arrived, or
            ;; after it was turned off -- is drawn as the one box it is, with
            ;; what it holds.  An overview that refuses to draw what it does not
            ;; recognise is an overview that lies by omission.
            (draw-preview-cell canvas inner (window-summary node) node))
        (when free-floats
          (draw-free-floats canvas box free-floats)))))

(defun draw-preview-plane (canvas inner grid &optional floats)
  "GRID's occupied cells, scaled to fit INNER, with FLOATS drawn where they hang."
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
                (node (c:child-at grid address))
                (index (c:prop r:*world* :rect-index)))
            (draw-preview-cell canvas box (window-summary node) node)
            ;; A FLOAT IS DRAWN OVER THE CELL ITS ANCHOR LIVES IN, afterwards,
            ;; because that is what a float is: on top.  A cell holding four
            ;; leaves can have floats on two of them, so this is asked of every
            ;; anchored float rather than of the cell's single summary.
            (draw-cell-floats canvas box node floats index))))))))

;;; ------------------------------------------------------------- the floats

;;; A FLOAT IS NOT IN THE TREE, and everything above draws the tree.  So a
;;; workspace whose cell holds a terminal and has a dialog floating over it was
;;; drawn as one cell -- the overview said one window while the screen showed
;;; two, which is exactly the question the overview exists to answer (#138).
;;;
;;; THE TWO KINDS OF FLOAT ARE PLACED DIFFERENTLY, because they are placed
;;; differently.  An anchored float's rect is relative to the rectangle of the
;;; node it hangs on, and both of those rectangles are in the render index -- so
;;; its place *inside its cell* is a fraction the panel can copy exactly.  An
;;; output-pinned one has a rect in screen coordinates and no cell at all, and
;;; the panel is a drawing of the plane rather than of pixels: nothing maps it,
;;; so it is *named* in the panel's corner instead of being placed at a position
;;; the drawing would have to invent.  Inventing one would be the deck lying
;;; about where something is.

(p:define-option *float-box-color* '(230 200 120 255)
  "The colour a floating window is drawn in, in a preview panel.

The border colour a real float gets is the policy's answer for its node, which
needs a live node and a focused state; the preview is a drawing of the model and
has neither.  One colour for every float is honest about that, and keeping it
separate from the cell colours is what makes a float legible as a float.")

(defun node-in-tree-p (node root)
  "True when NODE is somewhere in ROOT's tree.

By identity, which is the only way the emitter and the panel can agree: what
FLOAT-ANCHOR holds is a node object, and the same object is what the emitter
places the float against."
  (and node root (not (null (c:find-node-if (lambda (n) (eq n node)) root)))))

(defun floats-of (workspace world)
  "The floats anchored somewhere in WORKSPACE's tree.

A float anchored into another workspace is not here at all: it is on that
workspace's panel, because that is where its node is."
  (let ((out '()))
    (dolist (record (c:world-floats world) (nreverse out))
      (let ((anchor (c:float-anchor record)))
        (when (and anchor (node-in-tree-p anchor workspace))
          (push record out))))))

(defun free-floats-of (world)
  "The floats pinned to the output rather than to a node.

They belong to the *screen*, not to a workspace, which is why they are a
separate question: a panel is a workspace, and the panel you are standing on is
the one these are actually over."
  (remove nil (mapcar (lambda (record)
                        (unless (c:float-anchor record) record))
                      (c:world-floats world))))

(defun float-box-in-cell (cell-box cell-node record index)
  "Where an anchored FLOAT goes inside the panel's CELL-BOX, or NIL.

CELL-NODE's rectangle and the float's anchor's rectangle are both in the render
index, and the float's own rect is an offset from its anchor's -- so the float's
place *relative to its cell* is arithmetic on those three, and the panel copies
it by scaling.  NIL when either rectangle is missing or empty, which is an
ordinary frame: the index is filled by the emitter and a cell that has not been
placed yet is in no index."
  (let ((cell-rect (gethash cell-node index))
        (anchor-rect (gethash (c:float-anchor record) index))
        (rect (c:float-rect record)))
    (when (and cell-rect anchor-rect
               (plusp (c:rect-w cell-rect)) (plusp (c:rect-h cell-rect))
               (plusp (c:rect-w rect)) (plusp (c:rect-h rect)))
      (let* ((placed-x (+ (c:rect-x anchor-rect) (c:rect-x rect)))
             (placed-y (+ (c:rect-y anchor-rect) (c:rect-y rect)))
             (fx (/ (- placed-x (c:rect-x cell-rect)) (float (c:rect-w cell-rect))))
             (fy (/ (- placed-y (c:rect-y cell-rect)) (float (c:rect-h cell-rect))))
             (fw (/ (c:rect-w rect) (float (c:rect-w cell-rect))))
             (fh (/ (c:rect-h rect) (float (c:rect-h cell-rect)))))
        (let* ((w (max 2 (min (c:rect-w cell-box) (floor (* (c:rect-w cell-box) (min 1 fw))))))
               (h (max 2 (min (c:rect-h cell-box) (floor (* (c:rect-h cell-box) (min 1 fh))))))
               (x (+ (c:rect-x cell-box)
                     (floor (* (c:rect-w cell-box) (max 0 (min fx 1))))))
               (y (+ (c:rect-y cell-box)
                     (floor (* (c:rect-h cell-box) (max 0 (min fy 1)))))))
          (c:make-rect x y (min w (- (+ (c:rect-x cell-box) (c:rect-w cell-box)) x))
                       (min h (- (+ (c:rect-y cell-box) (c:rect-h cell-box)) y))))))))

(defun draw-preview-float (canvas box record)
  "One anchored float inside the cell box it hangs on: an outline and a name."
  (when box
    (r:canvas-rect canvas box (apply #'r:argb *float-box-color*) :width 1)
    (let ((room (c:rect-w box)))
      (when (and (> room 24) (> (c:rect-h box) (r:text-height :scale 1)))
        (r:canvas-text canvas (+ (c:rect-x box) 1) (+ (c:rect-y box) 1)
                       (r:truncate-text (p:name-of-window (c:float-window record))
                                        (max 1 (floor (- room 2)
                                                      (r:text-width "m"))))
                       (apply #'r:argb *float-box-color*))))))

(defun draw-cell-floats (canvas box cell-node floats index)
  "Draw every float anchored somewhere in CELL-NODE's subtree, inside BOX.

THE ONE FUNCTION BOTH DRAWINGS CALL, which is the point: the deck and the drawn
map are two views of the same plane, and two copies of this loop would drift
apart the way the two answers about a stack's visible window did (#137).  What
decides placement is FLOAT-BOX-IN-CELL; what decides *which* floats is this;
what either drawing looks like is its own."
  (dolist (record floats)
    (when (node-in-tree-p (c:float-anchor record) cell-node)
      (draw-preview-float canvas (float-box-in-cell box cell-node record index)
                           record))))

(defun draw-free-floats (canvas panel-box floats)
  "Name the output-pinned floats at the bottom of PANEL-BOX, one row each.

The word FLOAT is the whole of the correction: a reader who sees a count next to
a cell and `float' in the corner knows which of the two is in the layout.  Rows
count upwards from the bottom edge of the panel, and a panel too short for them
says nothing rather than writing over the cell that is there."
  (let ((row-height (+ (r:text-height :scale 1) 1)))
    (loop for record in floats
          for row from 1
          for y = (- (+ (c:rect-y panel-box) (c:rect-h panel-box))
                     (* row row-height) 2)
          while (> y (* 2 (r:text-height :scale 1)))
          do (r:canvas-text canvas (+ (c:rect-x panel-box) 6) y
                            (format nil "float ~a"
                                    (p:name-of-window (c:float-window record)))
                            (apply #'r:argb *float-box-color*)))))

(defun draw-preview-cell (canvas box summary &optional node)
  "One occupied cell's miniature: a picture of it if there is one, and its name.

THE PICTURE COMES FROM THE POLICY (P:CELL-PICTURE) and the default answer is
NIL, which is what a session with no thumbnails module loaded looks like: the
box and the name, exactly as before.  So this file does not know whether
pictures exist, and a compositor that cannot make one draws the same deck as a
user who turned them off.

WHEN THERE IS A PICTURE IT IS DRAWN INSIDE THE BOX AND THE NAME IS DRAWN OVER
IT, not beside it: the deck's job is the shape of the session, and a caption
strip under every thumbnail is height that comes out of the picture."
  (let ((picture (and node (p:cell-picture (p:current-policy) node)))
        (text-color (apply #'r:argb *map-detail-color*)))
    (r:canvas-fill canvas (apply #'r:argb *map-cell-color*) box)
    (when picture
      (r:canvas-blit canvas picture box))
    (let ((room (c:rect-w box)))
      (when (and (> room 24) (> (c:rect-h box) (r:text-height :scale 1)))
        (when picture
          ;; The name is unreadable on an unknown colour, so it gets a band of
          ;; its own: one row of the cell's colour, the height of the text.
          (r:canvas-fill canvas (apply #'r:argb *map-cell-color*)
                         (c:make-rect (c:rect-x box) (c:rect-y box)
                                      (c:rect-w box) (+ (r:text-height :scale 1) 2))))
        (r:canvas-text canvas (+ (c:rect-x box) 2) (+ (c:rect-y box) 1)
                       (r:truncate-text summary
                                        (max 1 (floor (- room 4) (r:text-width "m"))))
                       text-color)))))

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
