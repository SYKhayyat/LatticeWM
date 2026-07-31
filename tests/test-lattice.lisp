;;;; tests/test-lattice.lisp --- The plane, and its interoperation with splits.
;;;;
;;;; The interop tests are the important half of this file, and they are what
;;;; PLAN.org's Delta 1 predicted:
;;;;
;;;;   "A cell boundary and a split boundary are the same thing seen twice. […]
;;;;   The consequence that makes it worth doing: *directional motion is
;;;;   continuous across the boundary.*  Move left from the leftmost leaf of
;;;;   cell (0,0) and you arrive at the rightmost leaf of cell (-1,0).  No
;;;;   mode, no separate command, no discontinuity in the user's model.
;;;;   Emacs's windmove and lattice cell motion are one verb with one
;;;;   implementation."
;;;;
;;;; If that is true, none of it needed writing and all of it needs testing.

(defpackage #:lattice/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:l #:lattice)
                    (#:t* #:latticewm/tests))
  (:export #:run-all #:plane))

(in-package #:lattice/tests)

(def-suite plane :description "The lattice, and splits inside it.")
(in-suite plane)

(defun run-all ()
  "Run the lattice suite."
  (let ((results (run 'plane)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun pol () (make-instance 'l:lattice-policy))

(defun leaf (app) (t*::leaf-with app))

(defun grid-of (&rest cells)
  "A grid from (X Y NODE) triples."
  (l:make-grid :cols 2 :rows 2
               :cells (loop for (x y node) in cells
                            collect (cons (l:cell x y) node))))

(defun at (root path) (t*::app-at root path))

(defun target (policy root path direction)
  "Where motion goes, as an app-id."
  (let ((to (p:find-motion-target policy root path direction)))
    (and to (at root to))))

;;; ============================================================ the plane

(test a-grid-answers-the-container-protocol
  (let ((grid (grid-of '(0 0 nil) '(1 0 nil))))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (setf (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (is (= 2 (c:container-count grid)))
    (is (equal "a" (let ((n (c:child-at grid (l:cell 0 0))))
                     (c:window-app-id (c:leaf-window n)))))
    (is (null (c:child-at grid (l:cell 9 9)))
        "an unoccupied coordinate answers NIL, which is an ordinary question")
    (is-true (c:address-equal grid (l:cell 1 -2) (l:cell 1 -2)))))

(test negative-coordinates-are-ordinary
  (let ((grid (grid-of '(-3 -2 nil))))
    (setf (c:child-at grid (l:cell -3 -2)) (leaf "far"))
    (is (equal "far" (c:window-app-id
                      (c:leaf-window (c:child-at grid (l:cell -3 -2))))))))

(test the-grid-is-sparse-and-holes-survive
  ;; DESIGN D13: closing leaves a hole, and cells never move on their own.
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 5 0)) (leaf "b"))
    (c:remove-child grid (l:cell 0 0))
    (is (null (c:child-at grid (l:cell 0 0))))
    (is (equal "b" (c:window-app-id
                    (c:leaf-window (c:child-at grid (l:cell 5 0)))))
        "the surviving cell did not shift to fill the hole")))

(test a-grid-never-dissolves
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "only"))
    (is (eq grid (c:simplify-node grid))
        "a plane with one cell is still a plane")
    (c:remove-child grid (l:cell 0 0))
    (c:simplify-node grid)
    (is (= 1 (c:container-count grid))
        "and an emptied plane regains a cell rather than becoming invalid")))

;;; =========================================================== the viewport

(test the-viewport-enumerates-in-reading-order
  ;; +Y is up, so reading order is descending Y.
  (let ((viewport (make-instance 'l:viewport :origin (l:cell 0 0)
                                             :cols 2 :rows 2)))
    (is (equal '((0 . 1) (1 . 1) (0 . 0) (1 . 0))
               (l:viewport-cells viewport)))))

(test the-zoom-ladder-goes-1-2-4-6-8
  (is (equal '(1 2 4 6 8 12 24 48)
             (mapcar (lambda (step) (* (car step) (cdr step)))
                     l:*zoom-ladder*))))

(test zoom-keeps-the-focused-cell-visible
  (let ((grid (grid-of)))
    (dolist (index '(0 1 2 3 4 5))
      (l:set-zoom grid index :focus (l:cell 3 -2))
      (is-true (l:viewport-contains-p (l:grid-viewport grid) (l:cell 3 -2))
               "the focused cell stayed visible at ladder step ~d" index))))

(test zoom-does-not-touch-the-tree
  ;; DESIGN D7: zoom is pure view control.  Stepping out and back returns to
  ;; exactly the previous state.
  (let* ((grid (grid-of))
         (before nil))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b"))))
    (setf before (t*::shape (c:child-at grid (l:cell 0 0))))
    (l:set-zoom grid 4 :focus (l:cell 0 0))
    (l:set-zoom grid 0 :focus (l:cell 0 0))
    (is (equal before (t*::shape (c:child-at grid (l:cell 0 0))))
        "the tree is byte-identical after zooming out and back")))

(test panning-moves-the-view-and-not-the-cursor
  (let ((grid (grid-of)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1
          (l:viewport-origin (l:grid-viewport grid)) (l:cell 0 0))
    (l:ensure-visible grid (l:cell 3 0))
    (is (equal (l:cell 3 0) (l:viewport-origin (l:grid-viewport grid)))
        "a 1x1 viewport follows exactly")
    (setf (l:viewport-cols (l:grid-viewport grid)) 2
          (l:viewport-origin (l:grid-viewport grid)) (l:cell 0 0))
    (l:ensure-visible grid (l:cell 1 0))
    (is (equal (l:cell 0 0) (l:viewport-origin (l:grid-viewport grid)))
        "and does not move at all when the cell is already visible")))

(test the-viewport-scrolls-minimally
  ;; Scrolling further than necessary is how a viewport loses you.
  (let ((grid (grid-of)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 3
          (l:viewport-origin (l:grid-viewport grid)) (l:cell 0 0))
    (l:ensure-visible grid (l:cell 3 0))
    (is (equal (l:cell 1 0) (l:viewport-origin (l:grid-viewport grid)))
        "one cell past the edge shifts the view by exactly one column")))

;;; ============================================================== layout

(test cells-tile-the-output
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*cell-gap* 0))
    (setf (l:viewport-cols (l:grid-viewport grid)) 2
          (l:viewport-rows (l:grid-viewport grid)) 2)
    (dolist (address (list (l:cell 0 0) (l:cell 1 0) (l:cell 0 1) (l:cell 1 1)))
      (setf (c:child-at grid address) (leaf (l:cell-string address))))
    (let ((rects (l:cell-rects policy grid (c:make-rect 0 0 1000 800))))
      (is (= 4 (length rects)))
      (let ((upper-left (cdr (assoc (l:cell 0 1) rects :test #'equal)))
            (lower-left (cdr (assoc (l:cell 0 0) rects :test #'equal))))
        (is (= 0 (c:rect-y upper-left))
            "+Y is up, so cell (0,1) is drawn ABOVE cell (0,0)")
        (is (= 400 (c:rect-y lower-left)))
        (is (= 500 (c:rect-w upper-left)))))))

(test layout-omits-cells-outside-the-viewport
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1
          (c:child-at grid (l:cell 0 0)) (leaf "here")
          (c:child-at grid (l:cell 9 9)) (leaf "far away"))
    (let ((placed (p:layout-children policy grid (c:make-rect 0 0 800 600))))
      (is (= 1 (length placed)))
      (is (equal (l:cell 0 0) (car (first placed)))))
    ;; And the offscreen one is still *visited*, marked invisible, so its
    ;; window gets hidden — river shows a window unless told otherwise.
    (let* ((placements (p:layout-node policy grid (c:make-rect 0 0 800 600)))
           (far (find-if (lambda (pl) (equal '((9 . 9)) (second pl))) placements)))
      (is-true far "the offscreen cell was visited")
      (is-false (fourth far) "and marked invisible"))))

;;; ================================================== SPLITS INSIDE CELLS
;;;
;;; The conventional layer, unchanged, running inside a plane.

(test splits-work-normally-inside-a-cell
  (let* ((policy (pol))
         (grid (grid-of))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (p:on-window-open policy world (t*::win "a"))
    (p:on-window-open policy world (t*::win "b"))
    (p:on-window-open policy world (t*::win "c"))
    (is (= 3 (length (c:node-windows grid)))
        "three windows went into one cell")
    (is (typep (c:child-at grid (l:cell 0 0)) 'c:split)
        "and the cell holds a split, exactly as a workspace would")))

(test motion-inside-a-cell-does-not-leave-it
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "next door"))
    (is (equal "b" (target policy grid (list (l:cell 0 0) 0) :right))
        "the first Right moves within the cell")))

;;; ============================== THE INTEROP: motion across the boundary
;;;
;;; PLAN.org Delta 1, tested.  None of this is implemented anywhere; it is what
;;; the container protocol produces when a grid answers STEP-ADDRESS.

(test motion-runs-straight-through-the-cell-boundary
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0))
          (c:make-split :horizontal (list (leaf "c") (leaf "d"))))
    (is (equal "b" (target policy grid (list (l:cell 0 0) 0) :right)))
    (is (equal "c" (target policy grid (list (l:cell 0 0) 1) :right))
        "running out of panes carries you into the next CELL, no mode, no
second command")
    (is (equal "d" (target policy grid (list (l:cell 1 0) 0) :right)))))

(test crossing-a-cell-boundary-enters-through-the-edge-you-crossed
  ;; The property that makes it feel like one continuous space rather than two
  ;; systems glued together.
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0))
          (c:make-split :horizontal (list (leaf "c") (leaf "d"))))
    (is (equal "c" (target policy grid (list (l:cell 1 0) 1) :left))
        "within the cell first")
    (is (equal "b" (target policy grid (list (l:cell 1 0) 0) :left))
        "and only then leftwards out of it, landing on the RIGHTMOST pane of
the cell to the left — the edge you crossed")))

(test cross-cell-motion-is-involutive
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0))
          (c:make-split :horizontal (list (leaf "c") (leaf "d"))))
    (let* ((start (list (l:cell 0 0) 1))
           (there (p:find-motion-target policy grid start :right))
           (back (p:find-motion-target policy grid there :left)))
      (is (equal start back)
          "right then left across a cell boundary returns you exactly"))))

(test vertical-motion-crosses-cells-with-plus-y-up
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "bottom")
          (c:child-at grid (l:cell 0 1)) (leaf "top"))
    (is (equal "top" (target policy grid (list (l:cell 0 0)) :up))
        "pressing Up goes to the cell with the HIGHER Y, because +Y is up")
    (is (equal "bottom" (target policy grid (list (l:cell 0 1)) :down)))))

(test motion-into-empty-space-creates-the-cell
  ;; D3 says plain motion moves exactly one cell whether or not it is
  ;; occupied; D18 says focus is a place.  Together they require this.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*skip-empty-cells* nil))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (let ((to (p:find-motion-target policy grid (list (l:cell 0 0)) :right)))
      (is (equal (list (l:cell 1 0)) to))
      (is-true (c:child-at grid (l:cell 1 0))
               "the cell was brought into being by arriving at it")
      (is-true (c:leaf-empty-p (c:child-at grid (l:cell 1 0)))
               "and it is an empty pane, which is a first-class object"))))

(test skip-motion-crosses-a-run-of-empty-cells
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*skip-empty-cells* t))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 4 0)) (leaf "b"))
    (is (equal "b" (target policy grid (list (l:cell 0 0)) :right))
        "the spreadsheet Ctrl+Arrow idiom: one press crosses the gap")))

(test motion-never-escapes-the-plane
  (let* ((policy (pol))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (is-false (p:motion-escapes-p policy grid :right)
              "the plane has no edge to fall off")))

;;; ======================= THE INTEROP: moving windows between the two

(test a-pane-can-move-out-of-a-split-into-another-cell
  ;; "you can move a window to be a split window in another window", across a
  ;; cell boundary, using nothing but the core's TREE-MOVE.
  (let* ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "c"))
    (multiple-value-bind (root landed)
        (c:tree-move grid (list (l:cell 0 0) 1) (list (l:cell 1 0))
                     :axis :vertical :join :split)
      (is (eq grid root))
      (is (equal "b" (at root landed)))
      (is (equal '(:leaf "a") (t*::shape (c:child-at grid (l:cell 0 0))))
          "the source cell's split collapsed to its survivor")
      (is (equal '(:v (:leaf "c") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 1 0))))
          "and the target cell now holds a split it did not have before"))))

(test a-pane-can-move-into-an-empty-cell-and-become-its-whole-contents
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (multiple-value-bind (root landed)
        (c:tree-move grid (list (l:cell 0 0) 0) (list (l:cell 1 0)))
      (is (equal "a" (at root landed)))
      (is (equal '(:leaf "a") (t*::shape (c:child-at grid (l:cell 1 0))))
          "an empty cell is filled outright, not split against"))))

(test a-whole-cell-can-move-to-another-coordinate
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b"))))
    (c:tree-transplant grid (list (l:cell 0 0)) '() (l:cell 3 2))
    (is-true (or (null (c:child-at grid (l:cell 0 0)))
                 (c:node-empty-p (c:child-at grid (l:cell 0 0))))
             "the source coordinate is vacant, or holds the empty pane a plane
always keeps")
    (is (equal '(:h (:leaf "a") (:leaf "b"))
               (t*::shape (c:child-at grid (l:cell 3 2))))
        "the cell's whole subtree moved, splits and all")))

(test swapping-two-cells
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 2 1)) (leaf "b"))
    (c:tree-swap grid (list (l:cell 0 0)) (list (l:cell 2 1)))
    (is (equal "b" (at grid (list (l:cell 0 0)))))
    (is (equal "a" (at grid (list (l:cell 2 1)))))))

;;; ================================ THE INTEROP: workspaces of planes

(test workspaces-of-planes-one-behind-another
  ;; "infinite workspaces of lattices one behind another": a stack of grids.
  ;; Nothing was written to make this work.
  (let* ((policy (pol))
         (a (grid-of)) (b (grid-of))
         (root (c:make-stack (list a b) 0)))
    (setf (c:child-at a (l:cell 0 0)) (leaf "plane-a")
          (c:child-at b (l:cell 0 0)) (leaf "plane-b"))
    (is (equal (list 0 (l:cell 0 0)) (c:first-leaf-path root))
        "focus repair descends into the selected plane and then into its first
occupied cell, never into the plane behind")
    (is (equal "plane-a" (at root (c:first-leaf-path root))))
    (setf (c:stack-selected root) 1)
    (is (equal "plane-b" (at root (c:first-leaf-path root)))
        "switching workspace switches plane")
    (setf (c:stack-selected root) 0)
    (let ((there (p:find-motion-target policy root (list 0 (l:cell 0 0)) :right)))
      (is (equal 0 (first there))
          "motion right goes to the next CELL of the same plane, never to the
plane behind it — a workspace boundary is a wall and a cell boundary is not"))))

(test a-plane-can-be-nested-inside-a-split
  ;; Not a feature anybody asked for.  It is free, and its being free is the
  ;; evidence that the container abstraction is at the right level.
  (let* ((policy (pol))
         (inner (grid-of))
         (root (c:make-split :horizontal (list (leaf "outside") inner))))
    (setf (c:child-at inner (l:cell 0 0)) (leaf "inside"))
    (is (equal "inside" (target policy root '(0) :right)))
    (is (equal 2 (length (c:node-windows root))))))

;;; ================================================ turning it on, live

(test enable-wraps-existing-workspaces-without-losing-anything
  (let* ((r:*world* (c:make-world))
         (world r:*world*)
         (p:*policy* (make-instance 'p:conventional-policy)))
    (p:on-window-open p:*policy* world (t*::win "a"))
    (p:on-window-open p:*policy* world (t*::win "b"))
    (let ((before (c:node-windows (c:world-root world))))
      (l:enable :keys nil)
      (is (typep p:*policy* 'l:lattice-policy))
      (let ((workspace (c:child-at (c:world-root world) 0)))
        (is (typep workspace 'l:grid) "the workspace became a plane")
        (is (equal before (c:node-windows (c:world-root world)))
            "and every window survived, in the same order")
        (is (equal '(:h (:leaf "a") (:leaf "b"))
                   (t*::shape (c:child-at workspace (l:cell 0 0))))
            "with its split tree intact, as cell (0,0)"))
      (is-true (c:path-valid-p (c:world-root world) (c:world-cursor world))
               "and the cursor still points at something real"))))

(test the-conventional-policy-treats-an-unknown-container-as-inert
  ;; What DISABLE leaves behind, and the guarantee the container protocol
  ;; makes: a container kind a policy has never heard of degrades to showing
  ;; one child, rather than signalling.
  (let* ((policy (make-instance 'p:conventional-policy))
         (grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (finishes (p:layout-node policy grid (c:make-rect 0 0 800 600)))))

;;; ================================================== spreadsheet geometry

(test resizing-a-column-affects-every-row
  ;; DESIGN D8.  A width belongs to a column and spans every row, which is what
  ;; lets cells be non-uniform without the lattice going ragged.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*cell-gap* 0)
         (l:*zoom-mode* :fit))
    (setf (l:viewport-cols (l:grid-viewport grid)) 2
          (l:viewport-rows (l:grid-viewport grid)) 2)
    (dolist (address (list (l:cell 0 0) (l:cell 1 0) (l:cell 0 1) (l:cell 1 1)))
      (setf (c:child-at grid address) (leaf (l:cell-string address))))
    (setf (l:col-width grid 0) 3)
    (let* ((rects (l:cell-rects policy grid (c:make-rect 0 0 800 600)))
           (top (cdr (assoc (l:cell 0 1) rects :test #'equal)))
           (bottom (cdr (assoc (l:cell 0 0) rects :test #'equal))))
      (is (= 600 (c:rect-w top)))
      (is (= (c:rect-w top) (c:rect-w bottom))
          "both rows of column 0 widened together, so the grid stayed square"))))

(test a-uniform-lattice-does-not-resize-when-you-pan
  ;; PLAN Delta 3's cost, measured: it is real, and it is conditional on the
  ;; lattice being non-uniform.  Until you resize something, :FIT panning
  ;; resizes nothing.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fit)
         (l:*cell-gap* 0))
    (setf (l:viewport-cols (l:grid-viewport grid)) 2)
    (dotimes (x 6) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (is-true (l:uniform-p grid))
    (let ((widths '()))
      (dotimes (offset 4)
        (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell offset 0))
        (let ((rects (l:cell-rects policy grid (c:make-rect 0 0 800 600))))
          (push (c:rect-w (cdr (first rects))) widths)))
      (is (= 1 (length (remove-duplicates widths)))
          "every pan step showed the same cell width: ~s" widths))
    ;; Now make it non-uniform and watch Delta 3's cost appear.
    (setf (l:col-width grid 2) 3)
    (let ((widths '()))
      (dotimes (offset 3)
        (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell offset 0))
        (let ((rects (l:cell-rects policy grid (c:make-rect 0 0 800 600))))
          (push (c:rect-w (cdr (first rects))) widths)))
      (is (< 1 (length (remove-duplicates widths)))
          "and once a column is resized, panning does resize windows — which
is exactly when to switch *ZOOM-MODE* to :FIXED"))))

(test fixed-zoom-keeps-cells-the-same-size-and-crops-the-trailing-one
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 500)
         (l:*cell-gap* 0))
    (setf (l:viewport-cols (l:grid-viewport grid)) 3)
    (dotimes (x 3) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (let ((rects (l:cell-rects policy grid (c:make-rect 0 0 1200 600))))
      (is (every (lambda (entry) (= 500 (c:rect-w (cdr entry)))) rects)
          "every cell is exactly the configured width")
      (let ((last (cdr (assoc (l:cell 2 0) rects :test #'equal))))
        (is (= 1000 (c:rect-x last))
            "and the trailing one overhangs, to be cropped by the clip box")))))

;;; ======================================================== housekeeping

(test tidy-drops-empty-cells-but-not-the-one-you-are-in
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf)
          (c:child-at grid (l:cell 2 0)) (c:make-leaf))
    (is (= 1 (l:tidy-grid grid :keep (list (l:cell 2 0)))))
    (is (null (c:child-at grid (l:cell 1 0))))
    (is-true (c:child-at grid (l:cell 2 0)) "the one you are standing in stays")
    (is-true (c:child-at grid (l:cell 0 0)))))

(test names-are-the-layer-humans-remember
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 3 -1)) (leaf "emacs"))
    (setf (gethash "code" (l:grid-names grid)) (l:cell 3 -1))
    (is (equal (l:cell 3 -1) (gethash "code" (l:grid-names grid))))
    (is (equal "3,-1" (l:cell-string (l:cell 3 -1))))))

(test the-zoom-anchor-covers-the-content-not-the-void
  ;; +Y is up, so "biased top-left" means biasing the *opposite* way on Y from
  ;; X.  With the naive symmetric rule, zooming to two rows from a full
  ;; lattice put the focused cell on the bottom row and filled the top half of
  ;; the screen with the empty row above it.
  (let ((grid (grid-of)))
    (dolist (address (list (l:cell 0 0) (l:cell 1 0) (l:cell 0 1) (l:cell 1 1)))
      (setf (c:child-at grid address) (leaf (l:cell-string address))))
    ;; Focused on the top-left of the occupied block, zooming to 2x2.
    (l:set-zoom grid 2 :focus (l:cell 0 1))
    (let ((viewport (l:grid-viewport grid)))
      (is (equal (l:cell 0 0) (l:viewport-origin viewport))
          "the viewport covers rows 0 and 1, which is where the windows are")
      (dolist (address (list (l:cell 0 0) (l:cell 1 0) (l:cell 0 1) (l:cell 1 1)))
        (is-true (l:viewport-contains-p viewport address)
                 "~a is visible" (l:cell-string address))))))

(test the-zoom-anchor-keeps-the-focus-visible-at-every-rung
  (let ((grid (grid-of)))
    (dolist (focus (list (l:cell 0 0) (l:cell 3 -2) (l:cell -5 7)))
      (dotimes (rung (length l:*zoom-ladder*))
        (l:set-zoom grid rung :focus focus)
        (is-true (l:viewport-contains-p (l:grid-viewport grid) focus)
                 "focus ~a stays visible at rung ~d" (l:cell-string focus) rung)))))
