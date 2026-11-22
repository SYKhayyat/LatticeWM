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

;;; ============================= the cell verbs: split, combine, swap
;;;
;;; Issue #2's concrete minimum, as first-class commands: the coordinate-space
;;; analogues of the pane verbs SPLIT, close and SWAP.  They change the *tree*,
;;; so each is an ordinary tree undo step — the plane ring (issue #26) never
;;; sees them.

(test split-cell-creates-an-empty-cell-beside-and-steps-into-it
  (let* ((grid (grid-of '(0 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:split-cell :horizontal)
      (is (equal '(:leaf nil) (t*::shape (c:child-at grid (l:cell 1 0))))
          "a new empty cell appeared to the right")
      (is (equal (list (l:cell 1 0)) (c:world-cursor world))
          "and the cursor stepped into it, so the next window lands there")
      (l:split-cell :vertical)
      (is-true (c:child-at grid (l:cell 1 1))
               "and splitting again made one above that")
      (is (equal (list (l:cell 1 1)) (c:world-cursor world))))))

(test split-cell-refuses-an-occupied-neighbour-and-enters-an-empty-one
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:split-cell :horizontal)
      (is (equal (list (l:cell 0 0)) (c:world-cursor world))
          "an occupied neighbour refuses, and the cursor does not move")
      (is (equal "b" (at grid (list (l:cell 1 0))))
          "and nothing changed")))
  ;; An existing empty neighbour is the half already made: step into it.
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:split-cell :horizontal)
      (is (equal (list (l:cell 1 0)) (c:world-cursor world))
          "an empty neighbour is entered, not created over"))))

(test combine-cells-absorbs-an-empty-neighbour
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:combine-cells :right)
      (is (null (c:child-at grid (l:cell 1 0)))
          "the empty neighbour simply goes away")
      (is (equal "a" (at grid (list (l:cell 0 0))))
          "and the current cell survives intact"))))

(test combine-cells-joins-an-occupied-neighbour-as-a-split
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:combine-cells :right)
      (is (null (c:child-at grid (l:cell 1 0)))
          "the neighbour cell is gone")
      (is (equal '(:h (:leaf "a") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 0 0))))
          "its window joined the current cell as a horizontal split, on the
side the neighbour was on"))))

(test combine-cells-refuses-with-no-neighbour
  (let* ((grid (grid-of '(0 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:combine-cells :right)
      (is (equal "a" (at grid (list (l:cell 0 0))))
          "nothing changes when there is nothing to combine with"))))

(test swap-cell-exchanges-two-cells-contents
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:swap-cell :right)
      (is (equal "b" (at grid (list (l:cell 0 0))))
          "the neighbour's window is now here")
      (is (equal "a" (at grid (list (l:cell 1 0))))
          "and the focused window went the other way")
      (is (equal (list (l:cell 1 0)) (c:world-cursor world))
          "the cursor followed the window, as pane SWAP does"))))

(test swap-cells-swaps-with-a-named-coordinate
  (let* ((grid (grid-of '(0 0 nil) '(2 1 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 2 1)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:swap-cells 2 1)
      (is (equal "b" (at grid (list (l:cell 0 0)))))
      (is (equal "a" (at grid (list (l:cell 2 1))))))))

(test move-cell-contents-moves-the-whole-subtree
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0) 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:move-cell-contents 1 0)
      (is (equal '(:h (:leaf "a") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 1 0))))
          "the whole split went, not the one pane the cursor was in")
      (is (null (c:child-at grid (l:cell 0 0)))
          "and the source cell is a hole, because the cell itself moved")
      (is (equal (list (l:cell 1 0) 1) (c:world-cursor world))
          "the cursor followed the pane it was standing in"))))

(test move-cell-contents-onto-an-occupied-cell-joins-as-a-split
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :vertical (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "c"))
    (setf (c:world-cursor world) (list (l:cell 0 0) 0))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:move-cell-contents 1 0)
      (is (equal '(:h (:leaf "c") (:v (:leaf "a") (:leaf "b")))
                 (t*::shape (c:child-at grid (l:cell 1 0))))
          "the destination keeps its window and the whole cell joins it"))))

(test move-cell-contents-refuses-to-move-a-cell-onto-itself
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b"))))
    (setf (c:world-cursor world) (list (l:cell 0 0) 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:move-cell-contents 0 0)
      (is (equal '(:h (:leaf "a") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 0 0))))
          "nothing happens, least of all dropping the subtree"))))

(test copy-cell-copies-the-shape-and-not-the-windows
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :vertical (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0) 0))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:copy-cell 1 0)
      (is (equal '(:v (:leaf nil) (:leaf nil))
                 (t*::shape (c:child-at grid (l:cell 1 0))))
          "the arrangement is copied and the panes are empty")
      (is (equal '(:v (:leaf "a") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 0 0))))
          "the source is untouched: a window can only be in one pane")
      (is (not (eq (c:child-at grid (l:cell 0 0))
                   (c:child-at grid (l:cell 1 0))))
          "and the copy is a distinct subtree, not a shared one"))))

(test copy-cell-copies-the-cell-scale-too
  ;; #31 made the scale part of the cell, so a copied cell is the same size.
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (c:world-cursor world) (list (l:cell 0 0))
          (l:cell-scale grid (l:cell 0 0)) 3/2)
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:copy-cell 1 0)
      (is (= 3/2 (l:cell-scale grid (l:cell 1 0)))))))

(test copy-cell-refuses-onto-a-cell-holding-windows
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:copy-cell 1 0)
      (is (equal "b" (at grid (list (l:cell 1 0))))
          "the live cell keeps its window; copy is not a euphemism for move"))))

(test delete-cell-takes-the-cell-and-its-windows-away
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "c"))
    (setf (c:world-cursor world) (list (l:cell 0 0) 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:delete-cell)
      (is (null (c:child-at grid (l:cell 0 0)))
          "the whole cell is gone, not just emptied")
      (is (equal "c" (at grid (list (l:cell 1 0))))
          "and the neighbour is untouched")
      (is (not (equal (list (l:cell 0 0) 1) (c:world-cursor world)))
          "the cursor is repaired off the deleted path, where nothing exists"))))

(test the-whole-cell-verbs-are-one-tree-step-each
  ;; Each is a single node-signature change, so one undo puts it back — which is
  ;; the ruling #32 asks for and the same one the pane-sized cell verbs obey.
  (dolist (verb '((move) (copy) (delete)))
    (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
           (world (c:make-world :root grid)))
      (setf (c:child-at grid (l:cell 0 0))
            (c:make-split :horizontal (list (leaf "a") (leaf "b"))))
      (setf (c:world-cursor world) (list (l:cell 0 0) 0))
      (let ((r:*world* world)
            (p:*policy* (pol)))
        (r::note-layout-settled "baseline")
        (ecase (first verb)
          (move (l:move-cell-contents 1 0))
          (copy (l:copy-cell 1 0))
          (delete (l:delete-cell)))
        (r::note-layout-settled "change")
        (is (= 1 (length (r::undo-ring)))
            "~a is one tree step" (first verb))
        (p:run-command "undo")
        ;; UNDO RESTORES A COPY of the tree, so the grid this test holds is the
        ;; pre-undo one.  Ask the world what it has now.
        (is (equal '(:h (:leaf "a") (:leaf "b"))
                   (t*::shape (c:child-at (c:world-root world)
                                          (l:cell 0 0))))
            "and undo restores the cell, for ~a" (first verb))))))

(test swap-cell-moves-whole-subtrees-not-just-windows
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "c"))
    (setf (c:world-cursor world) (list (l:cell 0 0) 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:swap-cell :right)
      (is (equal '(:h (:leaf "a") (:leaf "b"))
                 (t*::shape (c:child-at grid (l:cell 1 0))))
          "the whole split subtree traded places, splits and all")
      (is (equal "c" (at grid (list (l:cell 0 0))))))))

(test the-cell-verbs-are-tree-steps
  ;; They change the tree, so they land on the tree ring and not the plane
  ;; ring — the two-ring split from issue #26, checked for these three.
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (r::note-layout-settled "baseline")
      (setf (l::plane-baseline) (l:collect-planes grid))
      (l:swap-cell :right)
      (r::note-layout-settled "swap")
      (is (= 1 (length (r::undo-ring)))
          "a swap is a tree step")
      (is (zerop (length (l::plane-ring)))
          "and not a plane step")
      (p:run-command "undo")
      (is (equal "a" (at (c:world-root world) (list (l:cell 0 0))))
          "and undo puts the windows back"))))

(test container-addresses-tells-the-truth-after-every-edit
  "The offscreen half is memoised (#92), and this is the check that would fail
if the memo outlived what it describes.  Every kind of change the plane allows
is here: a cell added, a cell removed, and a camera move -- which changes the
split without changing a single cell, and is why the viewport rectangle is in
the key rather than only the cell set."
  (let ((grid (grid-of '(0 0 nil) '(1 0 nil))))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (is (equal (list (l:cell 0 0) (l:cell 1 0))
               (c:container-addresses grid))
        "both occupied cells are visible")
    (setf (c:child-at grid (l:cell 5 0)) (leaf "c"))
    (is (equal (list (l:cell 0 0) (l:cell 1 0) (l:cell 5 0))
               (c:container-addresses grid))
        "a new offscreen cell appears at the end")
    (c:remove-child grid (l:cell 0 0))
    (is (equal (list (l:cell 1 0) (l:cell 5 0))
               (c:container-addresses grid))
        "and a removed one disappears from it")
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1
          (l:viewport-origin (l:grid-viewport grid)) (l:cell 5 0))
    (is (equal (list (l:cell 5 0) (l:cell 1 0))
               (c:container-addresses grid))
        "and a pan re-partitions the same cells")))

(test removing-a-cell-takes-its-size-with-it
  (let ((grid (grid-of '(0 0 nil) '(1 0 nil))))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (l:cell-scale grid (l:cell 1 0)) 3)
    (c:remove-child grid (l:cell 1 0))
    (is (null (c:child-at grid (l:cell 1 0))))
    (is (= 1 (l:cell-scale grid (l:cell 1 0)))
        "a removed cell does not leave its size behind, which is what keeps
REMOVE-CHILD and TIDY-GRID agreeing")))

;;; ================================ THE INTEROP: workspaces of planes

;;; ------------------------------------------------- sending to a named cell

(defun two-plane-world (&key (source-cell (l:cell 0 0)) (target-cell (l:cell 2 1)))
  "A world of two lattice workspaces, with one window in the first.

Returns (values world stack source-grid target-grid)."
  (let* ((a (grid-of (list (l:cell-x source-cell) (l:cell-y source-cell) nil)))
         (b (grid-of (list (l:cell-x target-cell) (l:cell-y target-cell) nil)))
         (stack (c:make-stack (list a b) 0))
         (world (c:make-world :root stack)))
    (setf (c:child-at a source-cell) (leaf "mover"))
    (setf (c:world-cursor world) (list 0 source-cell))
    (values world stack a b)))

(test send-to-cell-lands-in-a-named-cell-on-another-workspace
  (multiple-value-bind (world stack a b)
      (two-plane-world :target-cell (l:cell 2 1))
    (declare (ignore stack))
    (setf (gethash "mail" (l:grid-names b)) (l:cell 2 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 "mail")
      (is (equal "mover" (at (c:world-root world) (list 1 (l:cell 2 1))))
          "the pane arrived in the cell the name points at")
      (is (c:node-empty-p (c:child-at a (l:cell 0 0)))
          "and the source cell is empty, not still holding the pane"))))

(test send-to-cell-resolves-the-name-in-the-target-plane
  ;; Names are per plane, so "mail" must mean workspace 2's mail even when the
  ;; plane you are standing on has a mail of its own.  Looking the name up in
  ;; the current grid is the easy mistake and it is a different cell entirely.
  (multiple-value-bind (world stack a b)
      (two-plane-world :target-cell (l:cell 2 1))
    (declare (ignore stack))
    (setf (gethash "mail" (l:grid-names a)) (l:cell 0 0)
          (gethash "mail" (l:grid-names b)) (l:cell 2 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 "mail")
      (is (equal "mover" (at (c:world-root world) (list 1 (l:cell 2 1))))
          "the destination is the name on the target plane"))))

(test send-to-cell-creates-a-named-cell-that-does-not-exist-yet
  ;; A name may point at a coordinate no cell has been created at.  Arriving
  ;; there is an ordinary navigation (D3), so the send makes the cell.
  (let* ((a (grid-of '(0 0 nil)))
         (b (l:make-grid :cols 2 :rows 2))
         (stack (c:make-stack (list a b) 0))
         (world (c:make-world :root stack)))
    (declare (ignore stack))
    (setf (c:child-at a (l:cell 0 0)) (leaf "mover"))
    (setf (c:world-cursor world) (list 0 (l:cell 0 0)))
    (setf (gethash "scratch" (l:grid-names b)) (l:cell 5 5))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 "scratch")
      (is (equal "mover" (at (c:world-root world) (list 1 (l:cell 5 5))))
          "the cell was created where the name said"))))

(test send-to-cell-takes-a-coordinate-too
  (multiple-value-bind (world stack a b)
      (two-plane-world :target-cell (l:cell 1 0))
    (declare (ignore stack b))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 (cons 1 0))
      (is (equal "mover" (at (c:world-root world) (list 1 (l:cell 1 0)))))
      (is (c:node-empty-p (c:child-at a (l:cell 0 0)))))))

(test send-to-cell-with-follow-selects-the-workspace-and-lands-on-the-pane
  (multiple-value-bind (world stack a b)
      (two-plane-world :target-cell (l:cell 2 1))
    (declare (ignore a))
    (setf (gethash "mail" (l:grid-names b)) (l:cell 2 1))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 "mail" :follow t)
      (is (= 1 (c:stack-selected stack)) "the workspace was selected")
      (is (equal (list 1 (l:cell 2 1)) (c:world-cursor world))
          "and the cursor arrived with the window"))))

(test send-to-cell-refuses-a-name-the-target-plane-does-not-have
  (multiple-value-bind (world stack a b)
      (two-plane-world :target-cell (l:cell 2 1))
    (declare (ignore stack a b))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (l:send-to-cell 2 "nowhere")
      (is (equal "mover" (at (c:world-root world) (list 0 (l:cell 0 0))))
          "the window stays put rather than going somewhere approximate"))))

(test batch-move-to-cell-moves-every-marked-pane-and-keeps-the-cursor
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil) '(2 0 nil)))
         (world (c:make-world :root (c:make-stack (list grid) 0)))
         (a (t*::win "a"))
         (b (t*::win "b")))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :vertical (list (c:make-leaf a) (c:make-leaf b))))
    (setf (c:world-cursor world) (list 0 (l:cell 0 0) 0))
    (let ((r:*world* world)
          (r:*selection* '())
          (p:*policy* (pol)))
      (mapc #'r:mark-selected (list a b))
      (is (= 2 (l:batch-move-to-cell 2 0)) "both marked panes moved")
      ;; The emptied cell is forgotten (unnamed cell, and it ran dry: debris,
      ;; the same rule as a split) -- what matters is that it no longer holds
      ;; the panes, whichever of the two shapes that takes.
      (is (not (member a (c:node-windows (or (c:child-at grid (l:cell 0 0))
                                             (c:make-leaf)))))
          "the source cell did not keep the panes")
      (is (equal (list a b) (c:node-windows (c:child-at grid (l:cell 2 0))))
          "and they arrived together, in the order they were marked")
      (is (eq a (r:current-window))
          "the cursor followed the pane it was standing in, and did not tour")
      (is (null (r:selected-windows)) "the selection is spent"))))

(test target-here-cell-sends-the-next-spawn-into-the-whole-cell
  ;; TARGET-HERE targets a pane, so the next window subdivides the pane you are
  ;; standing in.  This targets the *cell*: the arrival joins the cell's own
  ;; split, which matters exactly when the cell holds more than one pane.
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root (c:make-stack (list grid) 0)))
         (policy (pol)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (leaf "c"))
    (setf (c:world-cursor world) (list 0 (l:cell 0 0) 0))
    (let ((r:*world* world)
          (p:*policy* policy))
      (l:target-here-cell)
      ;; Walk to the other cell; the latch is about the cell, not the cursor.
      (setf (c:world-cursor world) (list 0 (l:cell 1 0)))
      (p:on-window-open policy world (t*::win "new"))
      (is (equal "new" (at (c:world-root world) (list 0 (l:cell 0 0) 1)))
          "the arrival joined the targeted cell, beside the pane the latch was set on")
      (is (equal "c" (at (c:world-root world) (list 0 (l:cell 1 0))))
          "and the cell the cursor walked to is untouched"))))

(test a-cell-target-with-a-count-re-anchors-on-what-it-placed
  (let* ((grid (grid-of '(0 0 nil)))
         (world (c:make-world :root (c:make-stack (list grid) 0)))
         (policy (pol)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (setf (c:world-cursor world) (list 0 (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* policy))
      (l:target-here-cell 2)
      (p:on-window-open policy world (t*::win "b"))
      (p:on-window-open policy world (t*::win "c"))
      (is (equal "c" (at (c:world-root world) (list 0 (l:cell 0 0) 2)))
          "the second arrival went beside the first, not beside the anchor again")
      (is (null p:*spawn-target*) "and the latch is spent"))))

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

;;; ================================== the Z axis, once the world is running
;;;
;;; The test above proves a *stack of grids* behaves.  These prove the stack
;;; stays made of grids — which it did not, for the life of the extension.
;;; ENABLE wrapped the workspaces that existed when it ran, and every workspace
;;; created afterwards was an empty pane, because the four sites in the core
;;; that grow the workspace list each built one by hand.  Nothing failed.  You
;;; simply arrived on workspace 7 and found that zoom did nothing.

(defmacro with-lattice-world ((&rest bindings) &body body)
  "A live world with the lattice enabled, and no compositor.

Commands are safe to call: MARK-DIRTY no-ops without a server, so a verb
changes the model and stops, which is exactly the half under test."
  `(let* ((r:*world* (c:make-world))
          (world r:*world*)
          (p:*policy* (make-instance 'p:conventional-policy))
          ,@bindings)
     (declare (ignorable world))
     (l:enable :keys nil)
     ,@body))

(test every-workspace-born-after-enable-is-a-plane-too
  (with-lattice-world ()
    (r::workspace 4)
    (let ((stack (c:world-root world)))
      (is (= 4 (c:container-count stack))
          "asking for workspace 4 on a world with one made the other three")
      (dotimes (index 4)
        (is (typep (c:child-at stack index) 'l:grid)
            "workspace ~d is a plane, not a pane" (1+ index))))))

(test new-workspace-and-a-window-sent-past-the-end-make-planes-as-well
  ;; The other two of the four sites.  Each used to build its own empty pane,
  ;; which is how three of them come to disagree with the fourth.
  (with-lattice-world ()
    (r::new-workspace)
    (is (typep (c:child-at (c:world-root world) 1) 'l:grid)
        "NEW-WORKSPACE makes a plane")
    (p:on-window-open p:*policy* world (t*::win "a"))
    (r::send-to-workspace 5)
    (let ((stack (c:world-root world)))
      (is (= 5 (c:container-count stack)))
      (is (typep (c:child-at stack 4) 'l:grid)
          "and so does sending a window past the end of the list"))))

(test a-window-sent-to-another-plane-lands-in-a-cell-of-it
  ;; NOT beside it.  SEND-TO-WORKSPACE used to address the workspace *node*,
  ;; so the destination plane became one half of a split and the window the
  ;; other — viewport, column widths, names and all, demoted to half a screen.
  (with-lattice-world ()
    (p:on-window-open p:*policy* world (t*::win "a"))
    (p:on-window-open p:*policy* world (t*::win "b"))
    (r::send-to-workspace 2)
    (let* ((stack (c:world-root world))
           (there (c:child-at stack 1)))
      (is (typep there 'l:grid) "the destination is still a plane")
      (is (equal '("b") (mapcar #'c:window-app-id (c:node-windows there)))
          "and the window is inside it")
      (is (equal '("a") (mapcar #'c:window-app-id
                                (c:node-windows (c:child-at stack 0))))
          "and is no longer where it came from — sent, not copied"))))

(test a-new-plane-is-born-behind-the-one-you-are-standing-on
  ;; "One behind the other", literally: same zoom, same window of coordinates,
  ;; a different plane under it.
  (with-lattice-world ((l:*new-workspace-zoom* :inherit)
                       (l:*new-workspace-origin* :inherit))
    (let ((here (l:current-grid)))
      (l:goto-cell (l:cell 5 -1))
      (l:set-zoom here 3 :focus (l:current-cell))   ; a rung with room in it
      (let ((origin (l:viewport-origin (l:grid-viewport here))))
        (r::workspace 2)
        (let* ((there (c:child-at (c:world-root world) 1))
               (viewport (l:grid-viewport there)))
          (is (= 3 (l:viewport-cols viewport)) "same zoom")
          (is (= 2 (l:viewport-rows viewport)))
          (is (l:cell-equal origin (l:viewport-origin viewport))
              "the same window of coordinates — behind the plane you were on,
not beside it")
          (is (equal (list 1 (l:cell 5 -1)) (c:world-cursor world))
              "and you are standing at the coordinate you were already at")
          (is-true (c:child-at there (l:cell 5 -1))
                   "on a cell that exists, rather than one at (0,0) off the
edge of a view that starts somewhere else"))))))

(test a-fixed-zoom-and-origin-override-the-plane-you-came-from
  (with-lattice-world ((l:*new-workspace-zoom* (cons 2 2))
                       (l:*new-workspace-origin* (l:cell 0 0)))
    (setf (l:viewport-origin (l:grid-viewport (l:current-grid))) (l:cell 9 9))
    (r::workspace 2)
    (let ((viewport (l:grid-viewport (c:child-at (c:world-root world) 1))))
      (is (= 2 (l:viewport-cols viewport)))
      (is (= 2 (l:viewport-rows viewport)))
      (is (l:cell-equal (l:cell 0 0) (l:viewport-origin viewport))))))

(test switching-workspaces-keeps-your-coordinate-when-asked-to
  ;; *WORKSPACE-ENTRY* :ALIGNED — the planes share a coordinate space and
  ;; switching moves you along Z with X and Y untouched.
  (with-lattice-world ((l:*workspace-entry* :aligned))
    (l:goto-cell (l:cell 2 1))
    (r::workspace 2)
    (is (equal (list 1 (l:cell 2 1)) (c:world-cursor world))
        "you are on the plane behind, at the coordinate you were already on")
    (is-true (c:child-at (c:child-at (c:world-root world) 1) (l:cell 2 1))
             "and arriving created the cell, exactly as walking there would")))

(test a-plane-remembers-the-cell-you-left-it-standing-in
  (with-lattice-world ((l:*workspace-entry* :remembered))
    (r::workspace 2)
    (l:goto-cell (l:cell 3 0))
    (r::workspace 1)
    (r::workspace 2)
    (is (equal (list 1 (l:cell 3 0)) (c:world-cursor world))
        "a workspace is a room you left, not a room you are shown into")))

(test workspace-entry-occupied-is-the-old-behaviour-and-still-available
  (with-lattice-world ((l:*workspace-entry* :occupied)
                       (l:*new-workspace-origin* :origin))
    (l:goto-cell (l:cell 4 4))
    (r::workspace 2)
    (is (equal (list 1 (l:cell 0 0)) (c:world-cursor world))
        "the container is asked, and answers with its first occupied visible
cell — where you were standing is not consulted and no cell is created by
arriving")))

(test disabling-the-lattice-goes-back-to-plain-workspaces
  ;; MAKE-WORKSPACE is answered by the *policy*, so the shape of a new
  ;; workspace follows the policy in force rather than a flag set once.
  (with-lattice-world ()
    (l:disable)
    (r::workspace 2)
    (is (not (typep (c:child-at (c:world-root world) 1) 'l:grid))
        "with the conventional policy back, a new workspace is a plain pane")))

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

(test the-trailing-cell-is-actually-clipped-and-not-only-said-to-be
  ;; THIS TEST NAMED THE CROP AND CHECKED THE ARITHMETIC ABOVE IT.  The one
  ;; before this asserts where the trailing track *starts*, calls it "to be
  ;; cropped by the clip box", and never asks CLIP-RECT anything — which is
  ;; exactly how a method whose bounds nothing wrote passed for its whole life.
  ;; :LATTICE/VIEWPORT-BOUNDS was read here and written nowhere, so CLIP-RECT
  ;; fell through to the shipped `nothing overhangs, clip nothing' every time.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 500)
         (l:*cell-height* 600)
         (l:*cell-gap* 0)
         (rect (c:make-rect 0 0 1200 600)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 3
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (dotimes (x 3) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (let ((placed (p:layout-children policy grid rect)))
      (is (= 3 (length placed)))
      (flet ((clip (x)
               (let* ((node (c:child-at grid (l:cell x 0)))
                      (box (cdr (assoc (l:cell x 0) placed :test #'equal))))
                 (p:clip-rect policy node box))))
        (is (null (clip 0)) "a cell wholly on screen is not clipped at all")
        (is (null (clip 1)) "nor the second one, which ends exactly at 1000")
        (let ((clip (clip 2)))
          (is-true clip "the trailing cell IS clipped")
          (is (= 1000 (c:rect-x clip)))
          (is (= 200 (c:rect-w clip))
              "cropped to the 200 pixels of it that are on screen, not 500")
          (is (= 600 (c:rect-h clip))
              "and full height: the overhang is on one axis only"))))))

(test the-clip-follows-the-plane-and-is-not-remembered
  ;; The bounds are rewritten by every relayout, so they follow the output, the
  ;; reserved space and the gaps without anything having to invalidate them.  A
  ;; cached first answer is the failure this rules out: pan by one cell and the
  ;; cell that was cropped is whole, and a different one is cropped.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 500)
         (l:*cell-height* 600)
         (l:*cell-gap* 0)
         (rect (c:make-rect 0 0 1200 600)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 3
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (dotimes (x 4) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (flet ((clip-of (x)
             (let* ((placed (p:layout-children policy grid rect))
                    (node (c:child-at grid (l:cell x 0)))
                    (box (cdr (assoc (l:cell x 0) placed :test #'equal))))
               (and box (p:clip-rect policy node box)))))
      (is-true (clip-of 2) "at origin 0, cell 2 is the partial one")
      (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell 1 0))
      (is (null (clip-of 2))
          "after one pan it is whole, because the bounds moved with the layout")
      (is-true (clip-of 3) "and cell 3 is the partial one now"))))

(test a-cell-that-has-run-clean-off-the-edge-is-not-placed-at-all
  ;; A partial cell is cropped; a cell with nothing on screen at all is *hidden*,
  ;; which under this layout means omitted, because LAYOUT-CHILDREN's omissions
  ;; are what the driver marks invisible.  Placed-but-offscreen is the one
  ;; outcome that must not happen: river shows a window unless told otherwise,
  ;; so it would be drawn at a negative offset over somebody else's desktop.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 500)
         (l:*cell-height* 600)
         (l:*cell-gap* 0)
         (rect (c:make-rect 0 0 1200 600)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 4
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (dotimes (x 4) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (let ((placed (p:layout-children policy grid rect)))
      (is (= 3 (length placed))
          "three of the four asked-for columns touch the screen")
      (is (null (assoc (l:cell 3 0) placed :test #'equal))
          "the fourth starts at 1500 and is not on it"))
    (let* ((placements (p:layout-node policy grid rect))
           (gone (find-if (lambda (pl) (equal (list (l:cell 3 0)) (second pl)))
                          placements)))
      (is-true gone "it is still visited")
      (is-false (fourth gone) "and marked invisible, so its window is hidden"))))

(test resizing-a-column-works-under-fixed-zoom-too
  ;; RESIZE-COLUMN's first-use warning tells you that :FIT panning resizes
  ;; windows on a non-uniform lattice and that :FIXED is the way out.  Under
  ;; :FIXED the weights were dropped on the floor — every track was exactly
  ;; *CELL-WIDTH* — so the advice sent you to the mode where the thing you had
  ;; just done stopped working, silently.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 400)
         (l:*cell-height* 300)
         (l:*cell-gap* 0)
         (rect (c:make-rect 0 0 2000 900)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 3
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (dotimes (x 3) (setf (c:child-at grid (l:cell x 0)) (leaf "w")))
    (setf (l:col-width grid 1) 2)
    (let* ((rects (l:cell-rects policy grid rect))
           (first (cdr (assoc (l:cell 0 0) rects :test #'equal)))
           (wide (cdr (assoc (l:cell 1 0) rects :test #'equal)))
           (after (cdr (assoc (l:cell 2 0) rects :test #'equal))))
      (is (= 400 (c:rect-w first)))
      (is (= 800 (c:rect-w wide)) "twice the weight is twice the pixels")
      (is (= 400 (c:rect-x wide)) "and it starts where the first one ended")
      (is (= 1200 (c:rect-x after)) "the one after it is pushed along by 800")
      (is (= 400 (c:rect-w after))
          "and is still its own width: absolute sizes do not redistribute"))
    ;; The point of :FIXED, restated as a test: panning across a non-uniform
    ;; lattice does not change the width of anything.
    (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell 1 0))
    (let* ((rects (l:cell-rects policy grid rect))
           (wide (cdr (assoc (l:cell 1 0) rects :test #'equal))))
      (is (= 800 (c:rect-w wide))
          "the resized column is 800 wide wherever the viewport sits"))))

(test the-drawn-map-asks-how-wide-a-cell-really-is
  ;; MAP-MODE-P divided the output by the column count, which is the :FIT
  ;; answer and nothing at all under :FIXED — so a :FIXED lattice zoomed out
  ;; past the threshold's worth of *columns* put the drawn map up over cells
  ;; that were still full size, and no window on screen was anywhere near small
  ;; enough for it.
  (let* ((grid (grid-of))
         (l:*cell-gap* 0)
         (l:*map-threshold* 320)
         (rect (c:make-rect 0 0 1920 1080)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 8
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (let ((l:*zoom-mode* :fit))
      (is-true (l:map-mode-p grid rect)
               "eight :FIT columns of 1920 are 240 wide, so the map is right"))
    (let ((l:*zoom-mode* :fixed)
          (l:*cell-width* 960))
      (is-false (l:map-mode-p grid rect)
                "eight :FIXED columns are 960 wide however many fit on screen"))
    (let ((l:*zoom-mode* :fixed)
          (l:*cell-width* 200))
      (is-true (l:map-mode-p grid rect)
               "and a genuinely tiny :FIXED cell still gets the map"))))

;;; ================================================== per-cell sizing

(test a-cell-can-be-wider-than-its-same-column-neighbours
  ;; D8's escape hatch, in the exact shape the issue names: in a uniform 2x2
  ;; plane every track box is 400x300, and scaling (0,0) by three makes it
  ;; 1200x900 while its same-column and same-row neighbours keep their track
  ;; sizes.  That is the layout the spreadsheet says is impossible by
  ;; construction.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*cell-gap* 0)
         (l:*zoom-mode* :fit))
    (setf (l:viewport-cols (l:grid-viewport grid)) 2
          (l:viewport-rows (l:grid-viewport grid)) 2)
    (dolist (address (list (l:cell 0 0) (l:cell 1 0) (l:cell 0 1) (l:cell 1 1)))
      (setf (c:child-at grid address) (leaf (l:cell-string address))))
    (setf (l:cell-scale grid (l:cell 0 0)) 3)
    (let* ((rects (l:cell-rects policy grid (c:make-rect 0 0 800 600)))
           (scaled (cdr (assoc (l:cell 0 0) rects :test #'equal)))
           (above (cdr (assoc (l:cell 0 1) rects :test #'equal)))
           (right (cdr (assoc (l:cell 1 0) rects :test #'equal))))
      (is (= 1200 (c:rect-w scaled)) "the cell is three track-widths wide")
      (is (= 900 (c:rect-h scaled)) "and three track-heights tall")
      (is (= 400 (c:rect-w above))
          "its same-column neighbour kept its track width")
      (is (= 300 (c:rect-h above)) "and its track height")
      (is (= 400 (c:rect-w right))
          "its same-row neighbour kept its track width")
      (is (= -400 (c:rect-x scaled))
          "the box scaled about the track centre, which is what keeps the
address meaningful: centre was at 200, and 200 - 1200/2 is -400")
      (is (= 0 (c:rect-y scaled)) "and 450 - 900/2 is 0")
      (multiple-value-bind (cx cy) (c:rect-center scaled)
        (is (= 200 cx) "the scaled centre is the track centre")
        (is (= 450 cy) "on both axes")))))

(test a-cell-scale-is-its-own-fact-and-uniform-p-knows-it
  ;; The escape hatch is a *fourth* table, not a rewrite of the first two:
  ;; columns and rows can stay uniform while one cell deviates.  UNIFORM-P has
  ;; to see the deviation, because it is what decides the :FIT pan cost.
  (let ((grid (grid-of)))
    (is-true (l:uniform-p grid))
    (setf (l:cell-scale grid (l:cell 0 0)) 2)
    (is-false (l:uniform-p grid) "a sized cell is a non-uniform lattice")
    (is (= 1 (l:col-width grid 0)) "but no column track was touched")
    (is (= 1 (l:row-height grid 0)) "and no row track was touched")
    (l:unset-cell-scale grid (l:cell 0 0))
    (is-true (l:uniform-p grid)
             "dropping the entry restores uniformity, as EQUALIZE-CELLS does
for all of them at once")
    (is (= 1 (l:cell-scale grid (l:cell 0 0)))
        "and an absent entry reads as scale 1")))

(test resize-cell-grows-only-the-cell-you-are-in
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil) '(0 1 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b")
          (c:child-at grid (l:cell 0 1)) (leaf "c"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world))
      (l:resize-cell 1/10)
      (is (= 11/10 (l:cell-scale grid (l:cell 0 0)))
          "the cell under the cursor grew")
      (is (= 1 (l:cell-scale grid (l:cell 1 0)))
          "its same-row neighbour did not move")
      (is (= 1 (l:cell-scale grid (l:cell 0 1)))
          "nor its same-column neighbour")
      (is (= 1 (l:col-width grid 0)) "and no column track was touched")
      (l:resize-cell -1/10)
      (is (= 1 (l:cell-scale grid (l:cell 0 0)))
          "a second call with the opposite sign undoes it"))
    ;; RESET-CELL-SIZE is the explicit inverse, for the case where the addition
    ;; did not land exactly on 1.
    (setf (l:cell-scale grid (l:cell 1 0)) 3)
    (setf (c:world-cursor world) (list (l:cell 1 0)))
    (let ((r:*world* world))
      (l:reset-cell-size)
      (is (= 1 (l:cell-scale grid (l:cell 1 0)))
          "RESET-CELL-SIZE puts a cell back at its track size"))))

(test per-cell-scale-composes-with-fixed-zoom
  ;; :FIXED tracks are absolute — weight times *CELL-WIDTH* — and the cell
  ;; scale multiplies the absolute size on top, so RESIZE-CELL means the same
  ;; thing in both zoom modes, which RESIZE-COLUMN cannot claim.
  (let* ((policy (pol))
         (grid (grid-of))
         (l:*zoom-mode* :fixed)
         (l:*cell-width* 400)
         (l:*cell-height* 300)
         (l:*cell-gap* 0)
         (rect (c:make-rect 0 0 1200 600)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 2
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (l:cell-scale grid (l:cell 0 0)) 2)
    (let* ((rects (l:cell-rects policy grid rect))
           (wide (cdr (assoc (l:cell 0 0) rects :test #'equal)))
           (right (cdr (assoc (l:cell 1 0) rects :test #'equal))))
      (is (= 800 (c:rect-w wide)) "a 400px track scaled by two")
      (is (= 600 (c:rect-h wide)) "and a 300px track scaled by two")
      (is (= 400 (c:rect-w right)) "the neighbour is untouched")
      (is (= 300 (c:rect-h right)) "on either axis"))))

(test equalize-cells-clears-cell-scales-with-the-tracks
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (l:col-width grid 0) 3
          (l:row-height grid 0) 2
          (l:cell-scale grid (l:cell 0 0)) 5/2)
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world))
      (l:equalize-cells)
      (is (zerop (hash-table-count (l:grid-col-widths grid))))
      (is (zerop (hash-table-count (l:grid-row-heights grid))))
      (is (zerop (hash-table-count (l:grid-cell-scales grid)))
          "EQUALIZE-CELLS restores the uniform lattice, cell scales included")
      (is-true (l:uniform-p grid)))))

(test a-cell-scale-round-trips-through-the-state-file
  (let ((grid (grid-of))
        (index (make-hash-table :test #'equal)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (l:cell-scale grid (l:cell 1 0)) 5/2)
    (let* ((form (r:serialize-node grid))
           (back (r:read-node form index)))
      (is (= 5/2 (l:cell-scale back (l:cell 1 0)))
          "the sized cell came back sized")
      (is (= 1 (l:cell-scale back (l:cell 0 0)))
          "and the plain one came back plain")
      (let ((*package* (find-package :keyword)))
        (is (equal form (read-from-string (prin1-to-string form)))
            "the form is readable, because the state file is a file")))))

(test a-copy-carries-the-cell-scales
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 2 -1)) (leaf "b"))
    (setf (l:cell-scale grid (l:cell 2 -1)) 7/4)
    (let ((copy (c:copy-node grid)))
      (is (= 7/4 (l:cell-scale copy (l:cell 2 -1)))
          "the scale came across the copy")
      (is (= 1 (l:cell-scale copy (l:cell 0 0)))
          "and the untouched cell stayed plain"))))

(test a-cell-scale-is-a-plane-fact-and-not-a-tree-fact
  ;; RESIZE-CELL'S undo step lives on the plane ring (UNDO-PLANE), exactly like
  ;; zoom and pan: the scale is part of the plane state and deliberately *not*
  ;; part of the node signature, so a cell resize is not a tree step and the
  ;; plane ring walks it back.  This is the split the two-rings design stands
  ;; on, and it is tested here for the sixth plane verb.
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (let ((base (c:node-signature grid)))
      (setf (l:cell-scale grid (l:cell 0 0)) 5/2)
      (is (equal base (c:node-signature grid))
          "resizing a cell is not a tree step")
      (let ((state (l:plane-state grid)))
        (is (equal '((0 . 0) . 5/2)
                   (find-if (lambda (entry) (equal (car entry) (l:cell 0 0)))
                            (getf state :cell-scales)))
            "the scale is a plane fact"))))
  ;; Two grids with the same scales entered in a different order have the same
  ;; plane state, which is the guarantee the ring's equality check needs.
  (let ((a (grid-of)) (b (grid-of)))
    (loop for x in '(5 -3 0 12) do (setf (l:cell-scale a (l:cell x 0)) 2))
    (loop for x in '(0 12 -3 5) do (setf (l:cell-scale b (l:cell x 0)) 2))
    (is (equal (l:plane-state a) (l:plane-state b))
        "the plane state sorts the scales, because hash iteration order is not
specified")))

(test the-broom-leaves-a-sized-cell-alone
  ;; FORGET-EMPTY-CELL runs behind the user's back, so it declines to touch a
  ;; cell somebody did something to — a name is one such act, and a deliberate
  ;; size is another.  The size is part of the cell, so the refusal has to
  ;; keep them together.
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf)
          (c:child-at grid (l:cell 2 0)) (c:make-leaf))
    (setf (l:cell-scale grid (l:cell 1 0)) 3)
    (is-false (l:forget-empty-cell grid (l:cell 1 0))
              "a sized cell is somebody's, so the broom leaves it")
    (is (= 3 (l:cell-scale grid (l:cell 1 0)))
        "and the size survives the refusal")
    (is-true (l:forget-empty-cell grid (l:cell 2 0))
             "while the one nobody did anything to goes"))
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (setf (l:cell-scale grid (l:cell 1 0)) 3)
    (is (= 1 (l:tidy-grid grid :keep (list (l:cell 0 0))))
        "TIDY-GRID, being explicit rather than automatic, takes the empty cell")
    (is (= 1 (l:cell-scale grid (l:cell 1 0)))
        "but the scale goes with it rather than lingering on a cell that is
not there anymore")))

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

(test the-broom-refuses-three-cells-and-each-of-them-is-somebodys
  "FORGET-EMPTY-CELL runs behind the user's back, on every focus change, which
is the whole difference between it and TIDY-GRID — so the interesting part of
it is not what it drops but what it declines to."
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (c:make-leaf)
          (c:child-at grid (l:cell 2 0)) (c:make-leaf))
    (setf (gethash "code" (l:grid-names grid)) (l:cell 2 0))
    (is-false (l:forget-empty-cell grid (l:cell 0 0))
              "a cell with a window in it is not litter")
    (is-false (l:forget-empty-cell grid (l:cell 2 0))
              "and neither is one somebody named — naming it is the act that
says 'I mean to come back here', and dropping it would delete the destination
out from under GOTO-CELL while leaving the name pointing at it")
    (is-true (l:forget-empty-cell grid (l:cell 1 0))
             "the one nobody did anything to goes")
    (is (null (c:child-at grid (l:cell 1 0))))
    (is-false (l:forget-empty-cell grid (l:cell 9 9))
              "and a cell that was never there is not an error"))
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (c:make-leaf))
    (is-false (l:forget-empty-cell grid (l:cell 0 0))
              "the last cell standing stays: a plane with no cells at all is a
shape CONTAINER-ADDRESSES, DEFAULT-ADDRESS and LAYOUT-NODE are none of them
written for")
    (is (= 1 (hash-table-count (l:grid-cells grid))))))

(test crossing-the-plane-does-not-make-it-bigger
  "D6 says the plane is infinite, and ENSURE-CELL took that literally: every
step left a cell behind, nothing ever took one away, and CONTAINER-ADDRESSES
sorted the accumulated pile on every relayout.  The cost was proportional to
how long the session had been running, which is the kind of bug that is
invisible for a week."
  (let* ((policy (pol))
         (grid (grid-of))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "home"))
    (flet ((walk-right (steps)
             (let ((path (list (l:cell 0 0))))
               (dotimes (i steps)
                 (let ((next (list (l:cell (1+ i) 0))))
                   (l:ensure-cell grid (first next))
                   (p:on-focus-change policy world path next)
                   (setf path next))))))
      (walk-right 6)
      (is (= 2 (hash-table-count (l:grid-cells grid)))
          "six steps right leaves the cell you started in and the cell you are
standing in, and not the four you merely passed through")
      (is-true (c:child-at grid (l:cell 0 0))
               "the occupied cell you left is still there")
      (is-true (c:child-at grid (l:cell 6 0))
               "and so is the empty one you are in, because you are in it"))
    (let* ((l:*tidy-on-leave* nil)
           (grid (grid-of))
           (world (c:make-world :root grid)))
      (setf (c:child-at grid (l:cell 0 0)) (leaf "home"))
      (dotimes (i 4)
        (let ((from (list (l:cell i 0))) (to (list (l:cell (1+ i) 0))))
          (l:ensure-cell grid (first to))
          (p:on-focus-change policy world from to)))
      (is (= 5 (hash-table-count (l:grid-cells grid)))
          "and the old behaviour is still available to anyone who wants the
map to show where they have been, which is the reason it is an option and not
a fix"))))

(test moving-inside-a-cell-does-not-sweep-the-cell-you-are-in
  (let* ((policy (pol))
         (grid (grid-of))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0))
          (c:make-split :horizontal (list (leaf "a") (leaf "b")))
          (c:child-at grid (l:cell 1 0)) (c:make-leaf))
    (p:on-focus-change policy world (list (l:cell 0 0) 0) (list (l:cell 0 0) 1))
    (is (= 2 (hash-table-count (l:grid-cells grid)))
        "the cell is the same cell at both ends of that focus change, and a
comparison that got it wrong would drop the plane out from under a Right that
never left one leaf")
    (p:on-focus-change policy world (list (l:cell 1 0)) (list (l:cell 0 0) 1))
    (is (= 1 (hash-table-count (l:grid-cells grid)))
        "whereas leaving the empty one for a leaf inside another cell does
drop it, addresses of two different types along the same walk and all")))

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

;;; ==================================================================
;;; THE PLANE SURVIVES BEING COPIED AND BEING SAVED
;;; ==================================================================
;;;
;;; Two protocols the grid used to fall straight through, with one cause: both
;;; COPY-NODE and SERIALIZE-NODE were TYPECASEs over the three core kinds, and
;;; a GRID subclasses CONTAINER directly, so it matched no clause in either.
;;;
;;; The consequences were not subtle and were completely silent.  A copy came
;;; back with no cells at all — which is why layout undo could not have been
;;; built before this was fixed, since every undo would have destroyed the
;;; plane.  And a save came back as a flat split of whatever windows had been
;;; in it: the viewport, the track sizes and every name a user had given a cell
;;; were dropped on *every restart*.  The flagship extension could not survive
;;; the one thing persistence exists for.

(test a-grid-copies-with-everything-on-it
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 2 -1)) (leaf "b")
          (c:child-at grid (l:cell -3 4)) (leaf "c"))
    (setf (l:col-width grid 2) 3
          (l:row-height grid -1) 2)
    (l:set-zoom grid 3 :focus (l:cell 0 0))
    (let ((copy (c:copy-node grid)))
      (is (= 3 (c:container-count copy))
          "every cell came across; the old COPY-NODE returned an empty grid")
      (is (equal '("a" "b" "c")
                 (sort (remove nil (mapcar #'c:window-app-id
                                           (c:node-windows copy)))
                       #'string<))
          "with the windows still in them")
      (is (= 3 (l:col-width copy 2)) "column tracks survived")
      (is (= 2 (l:row-height copy -1)) "row tracks survived")
      (is (= (l:viewport-cols (l:grid-viewport grid))
             (l:viewport-cols (l:grid-viewport copy)))
          "and the zoom level survived")
      (is (not (eq (l:grid-viewport grid) (l:grid-viewport copy)))
          "as a copy rather than as the same object -- panning the copy must
not pan the original, which is the whole reason undo needs this")
      (is (not (eq (c:child-at grid (l:cell 0 0)) (c:child-at copy (l:cell 0 0))))
          "cells are copied structurally")
      (is (eq (c:leaf-window (c:child-at grid (l:cell 0 0)))
              (c:leaf-window (c:child-at copy (l:cell 0 0))))
          "and the windows in them are shared, because there is only one of those"))))

(test a-grid-round-trips-through-the-state-file
  (let ((grid (grid-of))
        (index (make-hash-table :test #'equal)))
    (let ((a (t*::win "a")) (b (t*::win "b")))
      (setf (c:window-identifier a) "ia" (c:window-identifier b) "ib")
      (setf (gethash "ia" index) a (gethash "ib" index) b)
      (setf (c:child-at grid (l:cell 0 0)) (c:make-leaf a)
            (c:child-at grid (l:cell -2 3)) (c:make-leaf b))
      (setf (c:node-label (c:child-at grid (l:cell -2 3))) "notes")
      (setf (l:col-width grid -2) 5/2)
      (l:set-zoom grid 2 :focus (l:cell 0 0))
      (let* ((form (r:serialize-node grid))
             (back (r:read-node form index)))
        (is (typep back 'l:grid)
            "it came back as a plane rather than as a split of its contents")
        (is (= 2 (c:container-count back)))
        (is (eq a (c:leaf-window (c:child-at back (l:cell 0 0))))
            "matched to the live window by river's identifier")
        (is (eq b (c:leaf-window (c:child-at back (l:cell -2 3))))
            "including at a negative coordinate, which is most of the plane")
        (is (equal "notes" (c:node-label (c:child-at back (l:cell -2 3))))
            "and the name somebody gave the cell survived, which is DESIGN D1's
third addressing layer and the one humans actually remember")
        (is (= 5/2 (l:col-width back -2)) "column tracks survived")
        (is (equal (l:viewport-origin (l:grid-viewport grid))
                   (l:viewport-origin (l:grid-viewport back)))
            "and you are looking at the same part of the plane you left")
        ;; And the file has to survive being written and read as text, which is
        ;; what it actually is.
        (let ((*package* (find-package :keyword)))
          (is (equal form (read-from-string (prin1-to-string form)))
              "the form is readable, because the state file is a file"))))))

(test a-grid-a-reader-does-not-know-keeps-its-windows
  ;; The other direction: a layout saved with the lattice loaded, read back by
  ;; an image that does not have it.  The arrangement is lost -- there is
  ;; nothing else it could be -- and the windows must not be.
  (let ((index (make-hash-table :test #'equal))
        (a (t*::win "a")))
    (setf (gethash "ia" index) a)
    (let ((node (r:deserialize-node :some-extension/kind
                                    '(:children ((:leaf :window "ia")))
                                    index)))
      (is (equal '("a") (mapcar #'c:window-app-id (c:node-windows node)))
          "the window survived a container kind this image has never heard of"))))

(test the-plane-s-state-is-not-part-of-its-signature
  "THE SPLIT THE TWO UNDO RINGS STAND ON.

NODE-SIGNATURE is what the tree ring compares at a settle, so the plane state
has to be *out* of it: a zoom is not a tree step, and undoing a tree change
does not drag the camera with it.  It used to be in — this is the five-verb
history, inverted — and the cost was exactly the issue's report: undo after a
tree change silently reverted whatever the camera had done since the snapshot
was taken, and there was no way to undo just the plane.  UNDO-PLANE (the plane
ring) is that way now, and this signature is what keeps the two rings from
treading on each other."
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (l:set-zoom grid 0 :focus (l:cell 0 0))
    (let ((base (c:node-signature grid)))
      (l:set-zoom grid 4 :focus (l:cell 0 0))
      (is (equal base (c:node-signature grid))
          "a zoom is not a tree step")
      (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell 3 -2))
      (is (equal base (c:node-signature grid)) "a pan is not a tree step")
      (setf (l:col-width grid 0) 5/2)
      (is (equal base (c:node-signature grid))
          "a column resize is not a tree step")
      (setf (l:row-height grid 0) 3)
      (is (equal base (c:node-signature grid))
          "a row resize is not a tree step")
      (setf (l:cell-scale grid (l:cell 0 0)) 2)
      (is (equal base (c:node-signature grid))
          "a cell resize is not a tree step")
      (setf (gethash "mail" (l:grid-names grid)) (l:cell 0 0))
      (is (equal base (c:node-signature grid))
          "naming a cell is not a tree step"))
    (let ((base (c:node-signature grid)))
      (setf (c:child-at grid (l:cell 1 0)) (leaf "b"))
      (is (not (equal base (c:node-signature grid)))
          "a tree change still is")))
  ;; And the plane state itself is order-independent, which is the guarantee
  ;; the plane ring needs: two states entered in a different hash iteration
  ;; order must compare EQUAL, or every step would look like a change and the
  ;; ring would fill with identical planes.
  (let ((a (grid-of)) (b (grid-of)))
    (loop for x in '(5 -3 0 12) do (setf (l:col-width a x) 2))
    (loop for x in '(0 12 -3 5) do (setf (l:col-width b x) 2))
    (loop for x in '(5 -3 0 12) do (setf (l:cell-scale a (l:cell x 0)) 2))
    (loop for x in '(0 12 -3 5) do (setf (l:cell-scale b (l:cell x 0)) 2))
    (is (equal (l:plane-state a) (l:plane-state b))
        "the plane state sorts its tables, because hash iteration order is not
specified")))

;;; ================================================== the plane's own undo

;;; The tree ring and the plane ring share one ruling, tested twice.  The tree
;;; ring records a snapshot only when the *tree* changed; the plane ring
;;; records one only when the *plane* changed.  A zoom is not a tree step and a
;;; move is not a plane step, and the second half of the bargain is that
;;; undoing one kind never drags the other kind along with it.

(defun live-grid (world)
  "The grid the world root is, which after a restore is a *new* grid."
  (c:world-root world))

(test plane-verbs-record-only-on-the-plane-ring
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (setf (l::plane-baseline) (l:collect-planes grid))
      (p:run-command "pan" :left)
      (is (equal (l:cell -1 0)
                 (l:viewport-origin (l:grid-viewport (live-grid world))))
          "the pan took effect")
      (is (= 1 (length (l::plane-ring)))
          "a pan recorded a plane step")
      (is (zerop (length (r::undo-ring)))
          "and no tree step at all")
      (p:run-command "undo-plane")
      (is (equal (l:cell 0 0)
                 (l:viewport-origin (l:grid-viewport (live-grid world))))
          "undo-plane walked the pan back")
      (p:run-command "redo-plane")
      (is (equal (l:cell -1 0)
                 (l:viewport-origin (l:grid-viewport (live-grid world))))
          "redo-plane put it forward again"))))

(test undo-of-a-tree-change-does-not-drag-the-camera
  "The issue's headline, stated as a test: zoom out, move a window, undo the
move — and the camera is still zoomed out.  It used to be that a tree snapshot
carried the plane state along, so the undo restored the move-time viewport and
threw the zoom away."
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (r::note-layout-settled "baseline")
      (setf (l::plane-baseline) (l:collect-planes grid))
      ;; The camera moves first.
      (p:run-command "zoom-to" 3 1)
      (is (= 3 (l:viewport-cols (l:grid-viewport (live-grid world))))
          "the camera is zoomed out")
      (is (= 1 (length (l::plane-ring))) "the zoom is a plane step")
      (is (zerop (length (r::undo-ring))) "and not a tree step")
      ;; Then the tree changes, and its settle records a tree step.
      (setf (c:child-at (live-grid world) (l:cell 2 0)) (leaf "c"))
      (r::note-layout-settled "add cell")
      (is (= 1 (length (r::undo-ring))) "the tree step is on the tree ring")
      ;; Undo the tree change.
      (p:run-command "undo")
      (is (null (c:child-at (live-grid world) (l:cell 2 0)))
          "the tree change is undone")
      (is (= 3 (l:viewport-cols (l:grid-viewport (live-grid world))))
          "but the camera is still where the user left it"))))

(test undo-plane-walks-back-each-plane-verb-in-order
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid)))
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1)
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (setf (l::plane-baseline) (l:collect-planes grid))
      (p:run-command "pan" :left)
      (p:run-command "resize-column")
      (p:run-command "zoom-to" 2 1)
      (is (= 3 (length (l::plane-ring)))
          "three plane verbs are three steps")
      (p:run-command "undo-plane")
      (is (= 1 (l:viewport-cols (l:grid-viewport (live-grid world))))
          "undo-plane walked the zoom back")
      (p:run-command "undo-plane")
      (is (= 1 (l:col-width (live-grid world) 0))
          "and the column resize")
      (p:run-command "undo-plane")
      (is (equal (l:cell 0 0)
                 (l:viewport-origin (l:grid-viewport (live-grid world))))
          "and the pan, in the order they happened"))))

(test repeated-plane-verbs-coalesce-into-one-step
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (world (c:make-world :root grid))
         (r:*undo-coalesce-seconds* 10))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a")
          (c:child-at grid (l:cell 1 0)) (leaf "b"))
    (setf (c:world-cursor world) (list (l:cell 0 0)))
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (setf (l::plane-baseline) (l:collect-planes grid))
      (p:run-command "pan" :left)
      (p:run-command "pan" :left)
      (p:run-command "pan" :left)
      (is (= 1 (length (l::plane-ring)))
          "three pans inside the coalesce window are one step")
      (p:run-command "undo-plane")
      (is (equal (l:cell 0 0)
                 (l:viewport-origin (l:grid-viewport (live-grid world))))
          "and one undo-plane walks all three back"))))

(test plane-state-round-trips-through-a-copy
  (let ((grid (grid-of '(0 0 nil))))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (let ((copy (c:copy-node grid)))
      (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell 3 -2)
            (l:col-width grid 0) 5/2
            (l:cell-scale grid (l:cell 0 0)) 7/4)
      (setf (gethash "mail" (l:grid-names grid)) (l:cell 0 0))
      (l:apply-plane-state copy (l:plane-state grid))
      (is (equal (l:plane-state grid) (l:plane-state copy))
          "the copy now sees the same plane")
      (is (eq (c:leaf-window (c:child-at grid (l:cell 0 0)))
              (c:leaf-window (c:child-at copy (l:cell 0 0))))
          "and the window in it is still the same one, because there is only
one of those"))))

(test collect-planes-and-apply-planes-round-trip
  "A plane inside a plane is two captures, and APPLY-PLANES puts both back by
path — which is exactly what a tree restore needs, because after the restore
the grids are copies and only their paths in the tree survive."
  (let* ((outer (grid-of '(0 0 nil)))
         (inner (grid-of '(1 1 nil)))
         (world (c:make-world :root outer)))
    (setf (c:child-at outer (l:cell 0 0)) inner)
    (setf (c:child-at outer (l:cell 1 0)) (leaf "b"))
    (setf (c:child-at inner (l:cell 1 1)) (leaf "deep"))
    (setf (l:col-width outer 0) 2
          (l:row-height inner 1) 3
          (l:cell-scale inner (l:cell 1 1)) 5/2
          (l:viewport-cols (l:grid-viewport inner)) 3)
    (let ((captures (l:collect-planes outer)))
      (is (= 2 (length captures)) "both planes are captured")
      ;; Mutate both planes, then put the captures back.
      (setf (l:col-width outer 0) 9
            (l:viewport-cols (l:grid-viewport inner)) 1)
      (setf (gethash "x" (l:grid-names inner)) (l:cell 1 1))
      (l:apply-planes outer captures)
      (is (equal captures (l:collect-planes outer))
          "applying the captures put every plane back where it was"))))

;;; ================================================ composing with a peer

;;; The claim under test is the one the whole project rests on: that a second
;;; party can extend this.  Their first act is to load their extension next to
;;; the one that ships, and until now the answer was "one of you wins, silently,
;;; by load order" — ENABLE was (setf p:*policy* (make-instance 'lattice-policy))
;;; and DISABLE was a fresh CONVENTIONAL-POLICY, so a policy loaded first was
;;; discarded by the first and could not be got back by the second.
;;;
;;; PEER-POLICY stands in for examples/03-master-stack.lisp: a policy class with
;;; a slot and a method, which is the shape the class idiom exists to support and
;;; exactly what a replacement throws away.

(defclass peer-policy (p:conventional-policy)
  ((flavour :initarg :flavour :initform :peer :accessor peer-flavour)
   (p::%name :initform "peer")))

(defmethod p:gaps ((policy peer-policy) (container c:split))
  "A number no shipped default returns, so 'whose method ran' has one answer."
  37)

(test enable-composes-the-lattice-over-a-policy-that-is-already-there
  (let ((p:*policy* (make-instance 'peer-policy :flavour :mine))
        (r:*world* nil))
    (l:enable :keys nil)
    (is-true (typep p:*policy* 'l:lattice-mixin) "the lattice is on")
    (is-true (typep p:*policy* 'peer-policy)
             "and the peer is still underneath it rather than discarded")
    (is (eq :mine (peer-flavour p:*policy*))
        "carrying the slot it was carrying, because the object was
CHANGE-CLASSed rather than replaced — a policy that keeps per-session state is
the only reason to write a policy class at all")
    (is (= 37 (p:gaps p:*policy* (c:make-split :horizontal (list (c:make-leaf)))))
        "and still answering for everything the lattice does not override")
    (is (equal "lattice over peer" (p:policy-name p:*policy*))
        "which the policy says out loud, because CHANGE-CLASS keeps %NAME and
a composed policy calling itself \"peer\" would be the same silence in reverse")))

(test disable-restores-what-was-there-rather-than-a-fresh-conventional-policy
  (let ((p:*policy* (make-instance 'peer-policy :flavour :mine))
        (r:*world* nil))
    (l:enable :keys nil)
    (is-true (l:disable))
    (is-false (typep p:*policy* 'l:lattice-mixin) "the lattice is off")
    (is-true (typep p:*policy* 'peer-policy) "and what is left is the peer")
    (is (eq :mine (peer-flavour p:*policy*)) "with its state intact")
    (is (equal "peer" (p:policy-name p:*policy*)) "and its name back")
    (is-false (l:disable) "and disabling twice is a no-op, not a fresh policy")))

(test the-shipped-composition-is-the-one-with-a-name
  (let ((p:*policy* (make-instance 'p:conventional-policy))
        (r:*world* nil))
    (l:enable :keys nil)
    (is-true (typep p:*policy* 'l:lattice-policy)
             "over nothing in particular, LATTICE-MIXIN + CONVENTIONAL-POLICY is
LATTICE-POLICY — the combination written down, which is what every document and
worked example already names")
    (is (equal "lattice" (p:policy-name p:*policy*)))
    (l:disable)
    (is (eq (find-class 'p:conventional-policy) (class-of p:*policy*))
        "and taking it off leaves exactly the class that was there")))

(test enabling-twice-composes-once
  (let ((p:*policy* (make-instance 'peer-policy))
        (r:*world* nil))
    (l:enable :keys nil)
    (let ((class (class-of p:*policy*)))
      (finishes (l:enable :keys nil))
      (is (eq class (class-of p:*policy*))
          "LATTICE-MIXIN twice in one precedence list is not a class, and the
error CLOS gives for it names neither this extension nor the second call")
      (l:disable)
      (is-true (typep p:*policy* 'peer-policy)
               "and one DISABLE is still enough to undo one ENABLE"))))

(test the-composed-class-is-interned-and-not-minted-per-call
  (let ((base (find-class 'peer-policy)))
    (is (eq (l:lattice-policy-class base) (l:lattice-policy-class base))
        "a fresh class per call would invalidate every generic function's
dispatch cache for the lifetime of the session")
    (is-true (subtypep (l:lattice-policy-class base) 'l:lattice-mixin))
    (is-true (subtypep (l:lattice-policy-class base) 'peer-policy))
    (is (eq (find-class 'l:lattice-policy)
            (l:lattice-policy-class (find-class 'p:conventional-policy))))
    (is (eq (find-class 'l:lattice-policy)
            (l:lattice-policy-class (find-class 'l:lattice-policy)))
        "and a policy that already has the mixin is returned unchanged")))

;;; ------------------------------------------------------- the vocabulary

(test a-name-already-spoken-for-costs-that-name-and-not-the-vocabulary
  "INSTALL-VOCABULARY used to be (ignore-errors (use-package ...)) under a
HANDLER-BIND hunting for restarts that a symbol conflict does not always offer.
When it did not find one the error escaped, the USE-PACKAGE was abandoned
partway through, and how many names had been imported first was unspecified —
the extension half installed behind one :WARN line, which is the exact failure
the function's own docstring exists to prevent."
  (let ((package (make-package "LATTICE-VOCABULARY-TEST" :use '(#:cl))))
    (unwind-protect
         (let ((theirs (intern "ZOOM-OUT" package)))
           (setf (symbol-value theirs) :theirs)
           (l:install-vocabulary package)
           (is (eq theirs (find-symbol "ZOOM-OUT" package))
               "the name they had already defined is still theirs")
           (is (eq 'l:pan-to-cursor (find-symbol "PAN-TO-CURSOR" package))
               "and one collision costs one name: the rest of the vocabulary
arrived, which is the whole difference between this and an unspecified prefix
of it")
           (is (eq 'l:enable (find-symbol "ENABLE" package))
               "including the two names the documentation tells people to type"))
      (delete-package package))))

(test the-installer-is-one-function-with-a-label
  "The lattice's version was the one with the good answers -- resolve
conflicts first, log the names kept, say which package it installed into --
and it stayed an argument in a docstring while buffers and transient-rules
each re-solved the same problem less completely (#121).  It lives on the
runtime surface now, takes the source package as an argument, and names the
caller in its own log line rather than assuming it is the lattice."
  (let ((package (make-package "LATTICE-VOCABULARY-SHARED-TEST" :use '(#:cl))))
    (unwind-protect
         (progn
           (is (eq package (r:install-vocabulary '#:lattice
                                                 :package package
                                                 :label "shared-test"))
               "it answers the package it installed into")
           (is (eq 'l:zoom-out (find-symbol "ZOOM-OUT" package))
               "and the source's names are there")
           (is (null (r:install-vocabulary '#:no-such-source-package
                                           :package package))
               "a missing source is a log line, not a condition -- an
extension installing before its own package exists is a load-order bug
that should not take the session down"))
      (delete-package package))))

;;; ================================================ A PLANE INSIDE A PLANE
;;;
;;; FINDINGS.org lists plane-inside-a-plane among the things that came for
;;; free, and this suite tested a plane inside a *split*, which is a different
;;; shape and the easy one.  What came for free was the container protocol;
;;; what did not was the three hand-written ancestor walks on top of it, which
;;; disagreed: CURSOR-GRID searched backwards and answered `innermost', while
;;; CURSOR-CELL ten lines below it and GRID-PATH in commands.lisp searched
;;; forwards and answered `outermost'.  Every caller pairs them.

(defun nested-planes ()
  "An outer plane whose (0,0) holds an inner plane whose (1,1) holds a window.

Returns (values WORLD OUTER INNER), cursor already deep inside."
  (let* ((outer (grid-of))
         (inner (grid-of))
         (world (c:make-world :root outer)))
    (setf (c:child-at outer (l:cell 0 0)) inner)
    (setf (c:child-at inner (l:cell 1 1)) (leaf "deep"))
    (setf (c:world-cursor world) (list (l:cell 0 0) (l:cell 1 1)))
    (values world outer inner)))

(test the-three-questions-about-where-you-are-answer-about-one-plane
  (multiple-value-bind (world outer inner) (nested-planes)
    (declare (ignore outer))
    (let ((r:*world* world))
      (multiple-value-bind (grid address base) (l:cursor-plane world)
        (is (eq inner grid) "the plane you are standing in is the inner one")
        (is (equal (l:cell 1 1) address) "and the cell is that plane's cell")
        (is (equal (list (l:cell 0 0)) base) "and the path leads to that plane"))
      (is (eq inner (l:current-grid)))
      (is (equal (l:cell 1 1) (l:current-cell))
          "which used to be (0,0) -- the address of the *outer* grid's cell,
read off the inner grid's question")
      (is (equal (list (l:cell 0 0)) (l:grid-path))
          "which used to be NIL, so CELL-PATH named a top-level cell of the
root and the confusing message about a stack was the good outcome")
      (is (eq (c:child-at inner (l:cell 1 1))
              (c:resolve-path (c:world-root world) (l:cell-path (l:cell 1 1))))
          "CELL-PATH names the cell it says it names, which is the whole
contract the docstring makes"))))

(test goto-cell-creates-the-cell-in-the-plane-it-jumps-into
  "The two halves used to be different planes: ENSURE-CELL ran on the inner
grid and JUMP-CURSOR was handed a path built from the outer grid's, so the cell
appeared in one place and the cursor went to another."
  (multiple-value-bind (world outer inner) (nested-planes)
    (let ((r:*world* world)
          (p:*policy* (pol)))
      (is (equal (list (l:cell 0 0) (l:cell 3 -2)) (l:cell-path (l:cell 3 -2)))
          "the path is inside the inner plane")
      (l:goto-cell (l:cell 3 -2))
      (is-true (c:child-at inner (l:cell 3 -2))
               "the cell was created in the plane the cursor was in")
      (is-false (c:child-at outer (l:cell 3 -2))
                "and not in the one above it")
      (is (equal (list (l:cell 0 0) (l:cell 3 -2)) (c:world-cursor world))
          "and the cursor is standing in it"))))

(test an-invalid-cursor-still-has-a-plane-under-it
  "The state a world is in between a tree edit and the focus repair that
follows it: RESOLVE-CHAIN answers NIL, and `which plane am I in' still has to
answer the root when the root is one."
  (let* ((grid (grid-of))
         (world (c:make-world :root grid)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    (setf (c:world-cursor world) (list (l:cell 9 9) 4 7))
    (multiple-value-bind (found address base) (l:cursor-plane world)
      (is (eq grid found))
      (is (null address) "no cell, because the path names none")
      (is (null base) "and the plane is the root"))))

;;; ------------------------------------------- the lattice's own verbs (#119)
;;;
;;; The verbs that ARE the lattice: the camera, the address book and the broom.
;;; The suite has tested the functions underneath these for a long time (TIDY-
;;; GRID, SET-ZOOM, GOTO-CELL), and gate 23 asked the other question -- whether
;;; the *commands* are named by anything -- and found twelve of them named by
;;; nothing at all.  A command is what a key runs; a suite that only calls what
;;; the command calls is a suite that would not notice the command passing the
;;; wrong argument.

(test zooming-in-and-out-is-a-round-trip
  "Stepping out and back returns to exactly where you were, which is DESIGN
D7's ruling that zoom is a camera and not an edit."
  (with-lattice-world ()
    (let* ((grid (l:current-grid))
           (viewport (l:grid-viewport grid))
           (before (l:zoom-index viewport)))
      (l:zoom-out)
      (is (> (l:zoom-index (l:grid-viewport grid)) before)
          "out is further out on the ladder")
      (l:zoom-in)
      (is (= before (l:zoom-index (l:grid-viewport grid)))
          "and in is the same trip back")
      (l:zoom-out 3)
      (l:zoom-reset)
      (is (= 0 (l:zoom-index (l:grid-viewport grid)))
          "reset is the first rung: one cell filling the screen"))))

(test zoom-fit-finds-the-rung-that-holds-everything
  "The command exists because the ladder is a list of rungs and the right rung
for a given plane is a fact about the plane, not something to try by hand."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
      (setf (c:child-at grid (l:cell 3 2)) (leaf "b"))
      (l:set-zoom grid 0 :focus (l:cell 0 0))
      (l:zoom-fit)
      (let ((viewport (l:grid-viewport grid)))
        (is (>= (* (l:viewport-cols viewport) (l:viewport-rows viewport)) 4)
            "four by three is on no rung, so the viewport holds at least one
cell per axis of what is occupied")
        (is (<= (l:viewport-cols viewport) 8)
            "and it did not zoom out past the far end of the ladder")))))

(test the-address-book-names-a-cell-and-goes-back-to-it
  "Names, not numbers (D1 layer 3): the name is what you remember three days
later, and the coordinate it stores is what the machine uses."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 2 1)) (leaf "editor"))
      (l:goto 2 1)
      (is (equal (l:cell 2 1) (l:current-cell)) "GOTO is the escape hatch")
      (l:name-cell "code")
      (is (equal (l:cell 2 1) (gethash "code" (l:grid-names grid)))
          "the cell is stored by the name it was given")
      (l:goto 0 0)
      (is (equal (l:cell 0 0) (l:current-cell)) "we walked away")
      (l:goto-name "code")
      (is (equal (l:cell 2 1) (l:current-cell))
          "and the name brought us back")
      (l:goto-name "nobody-named-this")
      (is (equal (l:cell 2 1) (l:current-cell))
          "an unknown name leaves you where you are rather than jumping to nothing"))))

(test goto-next-occupied-skips-the-gap
  "The spreadsheet Ctrl+Arrow idiom, and the reason plain motion stays one cell:
distance stays predictable, and skipping is the modified key."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 0 0)) (leaf "here"))
      (setf (c:child-at grid (l:cell 5 0)) (leaf "there"))
      (l:goto 0 0)
      (l:goto-next-occupied :right)
      (is (equal (l:cell 5 0) (l:current-cell))
          "the first occupied cell to the right, not the first cell"))))

(test move-cell-takes-the-pane-with-it-and-leaves-a-hole
  "D13: moving a pane off a cell leaves the cell behind as a hole rather than
an empty pane, and that hole is what TIDY is the broom for."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 0 0)) (leaf "mover"))
      (l:goto 0 0)
      (l:move-cell :right)
      (is-true (c:child-at grid (l:cell 1 0)) "the pane is one cell right")
      (is (= 1 (length (c:node-windows (c:world-root r:*world*))))
          "nothing was copied on the way")
      (is (equal (l:cell 1 0) (l:current-cell))
          "and the cursor followed it, which is :FOLLOW T"))))

(test tidy-sweeps-the-holes-and-keeps-the-one-you-are-in
  "The broom: a hole you made by walking across it is litter, but the one you
are standing in is where you are."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (dotimes (x 4)
        (setf (c:child-at grid (l:cell x 0)) (leaf (format nil "w~d" x))))
      (dotimes (x 4)
        (when (/= x 2)
          (setf (c:child-at grid (l:cell x 0))
                (make-instance 'c:leaf))))
      (l:goto 2 0)
      (l:tidy)
      (is-true (c:child-at grid (l:cell 2 0)) "the cell you are in survives")
      (is-false (c:child-at grid (l:cell 0 0)) "the empty ones do not")
      (is-false (c:child-at grid (l:cell 3 0))))))

(test resizing-a-row-is-one-verb-for-every-column
  "A row is a row across the whole sheet (D8): the verb takes one axis and the
geometry supplies the other, which is why there is no RESIZE-ROW-BY-COLUMN."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 0 1)) (leaf "tall"))
      (l:goto 0 1)
      (let ((before (l:row-height grid 1)))
        (l:resize-row)
        (is (> (l:row-height grid 1) before) "the row got taller")
        (is (= before (+ (l:row-height grid 1) -1/10))
            "and only by the amount asked for")))))

(test lattice-status-says-where-you-are
  "The answer to `where am I', in one line, because the coordinates are on
screen only at some zooms."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:child-at grid (l:cell 1 2)) (leaf "a"))
      (l:goto 1 2)
      (let ((text (with-output-to-string (*standard-output*)
                    (l:lattice-status))))
        (is (search "1" text) "the cell it reports is the one you are in")
        (is (search "occupied" text) "and what is on the plane")))))

(test the-plane-undo-history-shows-what-undo-would-walk-back
  "The list is the ring, so with nothing on it the command has to say so
rather than show an empty page."
  (with-lattice-world ()
    (is (stringp (l:plane-undo-history))
        "an empty history says so in the echo area, not on a page")))

;;; ============================================================ peek (#38)
;;;
;;; The peek is the map drawn over the session, so the two things worth
;;; asserting are the ones its design rests on: it appears at a zoom where the
;;; map would not, and it costs no relayout -- no window changes rectangle, and
;;; the viewport does not move.

(test a-peek-puts-the-map-up-at-a-zoom-where-it-would-not-be
  "The map appears by itself only when cells are too small to render.  A peek
is the request to see it anyway, over full-size windows, which is the whole
difference between looking and zooming out."
  (let* ((grid (grid-of '(0 0 nil)))
         (l:*peek* nil)
         (l:*map-threshold* 320)
         (rect (c:make-rect 0 0 1920 1080)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "editor"))
    (is (null (l:map-mode-p grid rect)) "one full-size cell is not the map")
    (is (null (l:map-shown-p grid rect)))
    (l:peek-begin)
    (is-true (l:peek-active-p))
    (is-true (l:map-shown-p grid rect) "and now it is")
    (l:peek-end)
    (is (null (l:map-shown-p grid rect)) "one keypress later it is not")))

(test a-peek-resizes-nothing-and-moves-nothing
  "The property the whole design is for: zooming costs two relayouts of every
window on screen, and a peek costs none.  Asserted against the layout driver
itself rather than against a viewport field, because the claim is about what a
window is given."
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (policy (pol))
         (l:*peek* nil)
         (rect (c:make-rect 0 0 1920 1080)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "editor"))
    (setf (c:child-at grid (l:cell 1 0)) (leaf "web"))
    (let ((before (p:layout-children policy grid rect))
          (camera (l:grid-viewport grid)))
      (l:peek-begin)
      (is (equalp before (p:layout-children policy grid rect))
          "the same windows, in the same rectangles")
      (is (eq camera (l:grid-viewport grid))
          "the camera itself never moved: a peek sets no zoom")
      (l:peek-end)
      (is (eq camera (l:peek-viewport grid))
          "and with the peek down the drawing asks the camera where it is")
      (is (equalp before (p:layout-children policy grid rect))))))

(test the-peek-shows-the-whole-plane-not-the-visible-cell
  "A peek from a one-cell viewport has to draw the cells the camera is NOT on,
which is why it asks where the windows are instead of where the viewport is."
  (let* ((grid (grid-of '(-2 3 nil) '(0 0 nil) '(1 1 nil)))
         (l:*peek* nil))
    (setf (c:child-at grid (l:cell -2 3)) (leaf "cold-storage"))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "editor"))
    (setf (c:child-at grid (l:cell 1 1)) (leaf "web"))
    (setf (l:viewport-cols (l:grid-viewport grid)) 1
          (l:viewport-rows (l:grid-viewport grid)) 1
          (l:viewport-origin (l:grid-viewport grid)) (l:cell 0 0))
    (is (= 1 (l:viewport-cols (l:grid-viewport grid)))
        "the camera is on one cell")
    (l:peek-begin)
    (let ((viewport (l:peek-viewport grid)))
      (is (= 4 (l:viewport-cols viewport)) "-2..1 across")
      (is (= 4 (l:viewport-rows viewport)) "3..0 is four rows, down to 0")
      (is (equal '(-2 0) (list (l:cell-x (l:viewport-origin viewport))
                               (l:cell-y (l:viewport-origin viewport))))
          "the origin is the corner, not the cell the camera is on"))
    (l:peek-end)
    (is (eq (l:grid-viewport grid) (l:peek-viewport grid))
        "and without a peek it is the grid's own viewport")))

(test an-empty-plane-peeks-into-its-own-view
  "The map of nothing has no bounding box; showing the viewport you are already
looking at is the honest answer, rather than a width of zero."
  (let* ((grid (grid-of))
         (l:*peek* nil))
    (l:peek-begin)
    (is (eq (l:grid-viewport grid) (l:peek-viewport grid)))))

(test any-key-takes-a-peek-down-and-still-does-its-own-job
  "A peek is a glance, not a mode: the keystroke that ends it is the keystroke
that reason for the look, and swallowing it is the one way this would feel like
a mode after all."
  (let ((l:*peek* nil)
        (policy (pol))
        (world (c:make-world)))
    (l:peek-begin)
    (is-true (l:peek-active-p))
    (p:on-key policy world (cons 38 (list :none)))
    (is (null (l:peek-active-p)) "the key ended the peek")
    ;; And the key was not consumed: ON-KEY still answers with what it always
    ;; answers with, which is what lets the caller run the binding.
    (is (null (p:on-key policy world (cons 38 (list :none))))
        "and the key was not swallowed")))

(test peek-commands-toggle-the-picture
  (let ((l:*peek* nil))
    (l:peek-on)
    (is-true (l:peek-active-p))
    (l:peek-toggle)
    (is (null (l:peek-active-p)) "the one chord puts it down")
    (l:peek-toggle)
    (is-true (l:peek-active-p) "and up again")
    (l:peek-off)
    (is (null (l:peek-active-p)))
    (is (null (l:peek-off)) "taking down nothing answers NIL")))

;;; ================================================== clicking on the map (#30)
;;;
;;; The map is a picture of the cells with their addresses on it, so a click on
;;; one is a request to go there.  The decode is a point looked up in the same
;;; rectangles the picture is drawn from, which makes it testable with no
;;; compositor and impossible to disagree with what is on screen.

(defun a-plane-on-a-screen (grid rect)
  "Put GRID in a fresh world with one output of RECT, and answer the world."
  (let ((world (c:make-world :root (c:make-stack (list grid) 0))))
    (setf (c:world-outputs world)
          (list (make-instance 'c:output :name "DP-1" :rect rect)))
    (setf (c:world-cursor world) (list 0 (l:cell 0 0)))
    world))

(defun centre-of (rect)
  "The point in the middle of RECT, as two values."
  (values (+ (c:rect-x rect) (floor (c:rect-w rect) 2))
          (+ (c:rect-y rect) (floor (c:rect-h rect) 2))))

(defun a-two-cell-plane ()
  "A world with two named cells side by side, and the pieces the tests need."
  (let* ((grid (grid-of '(0 0 nil) '(1 0 nil)))
         (rect (c:make-rect 0 0 800 600))
         (world (a-plane-on-a-screen grid rect)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "editor")
          (c:child-at grid (l:cell 1 0)) (leaf "web"))
    (values grid rect world)))

(test a-point-on-the-map-names-the-cell-under-it
  "The decode is the picture: the same CELL-RECTS the drawer walks, so the cell
a click lands in is the cell the pointer is visually on."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*peek* nil))
      (let ((r:*world* world))
        (l:peek-begin)
        (dolist (address (list (l:cell 0 0) (l:cell 1 0)))
          (let ((drawn (cdr (assoc address
                                   (l:cell-rects policy grid rect
                                                 :viewport (l:peek-viewport grid))
                                   :test #'equal))))
            (multiple-value-bind (x y) (centre-of drawn)
              (is (equal address (l:map-cell-at policy grid
                                                (first (c:world-outputs world))
                                                x y))
                  "the middle of a cell names that cell"))))
        (l:peek-end)))))

(test a-point-off-the-plane-is-no-cell-at-all
  "A click that lands on nothing must do nothing rather than guess the nearest
-- the same rule a name four windows share follows."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*peek* nil)
          (output (first (c:world-outputs world))))
      (let ((r:*world* world))
        (l:peek-begin)
        (is (null (l:map-cell-at policy grid output -4000 -4000))
            "far outside the plane is no cell")
        (is (null (l:map-cell-at policy grid output 4000 4000))
            "and so is past the last one")
        (l:peek-end)))))

(test a-press-on-the-map-is-claimed-and-the-cursor-goes-there
  "Super+click means `move a window' everywhere else in this program, so on the
map it has to mean something or nothing.  Claimed means the drag never starts:
a press that both jumped the cursor and grabbed a window would leave the window
stuck to the pointer."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*peek* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        ;; Over a window -- which is what "Super+click drags" means -- nothing
        ;; is claimed, with the map up or down.  Over empty space the press is
        ;; the plane handle instead; see the drag-pan tests.
        (let ((window (t*::win "editor")))
          (setf (c:window-rect window) (c:make-rect 380 280 40 40))
          (setf (c:world-root world) (c:make-stack (list (c:make-leaf window)) 0))
          (is (null (p:pointer-press policy world 400 300 :move))
              "no claim: Super+click on a window still drags it")
          (is (null (l:drag-panning-p)) "and it is not a pan")
          (setf (c:world-root world)
                (c:make-stack (list grid) 0)))
        (l:peek-begin)
        (let* ((output (first (c:world-outputs world)))
               (drawn (cdr (assoc (l:cell 1 0)
                                  (l:cell-rects policy grid rect
                                                :viewport (l:peek-viewport grid))
                                  :test #'equal))))
          (multiple-value-bind (x y) (centre-of drawn)
            (is-true (p:pointer-press policy world x y :move)
                     "with the map up the press is claimed")
            (is (equal (list 0 (l:cell 1 0)) (c:world-cursor world))
                "and the cursor is in the cell that was clicked")
            (is (null r::*server*)
                "and no drag was started, which needs a seat to have one")))))))

(test go-to-cell-makes-a-cell-that-is-not-there
  "An empty cell is not somewhere to stand -- the cursor in a cell you cannot
see into is a cursor behind a door -- so clicking one creates it, which is what
the map is offering when it draws a square with no name on it."
  (with-lattice-world ()
    (let ((grid (l:current-grid)))
      (setf (c:world-cursor world) (list 0 (l:cell 0 0)))
      (is (null (c:child-at grid (l:cell 3 2))))
      (l:goto-cell (l:cell 3 2))
      (is-true (c:child-at grid (l:cell 3 2)) "the cell exists now")
      (is (equal (list 0 (l:cell 3 2)) (c:world-cursor world))
          "and the cursor is in it"))))

(test goto-pointer-says-so-when-there-is-nothing-under-it
  "A verb that silently does nothing is the complaint this whole issue is
about, one layer down -- and with no seat at all, `nothing there' is the
honest answer rather than an error."
  (let ((r::*echo-message* nil))
    (with-lattice-world ()
      (let ((output (make-instance 'c:output :name "DP-1"
                                   :rect (c:make-rect 0 0 800 600))))
        (setf (c:world-outputs world) (list output))
        (l:peek-begin)
        (is (null (l:goto-pointer output)) "nothing under the pointer")
        (is-true (search "not over a cell" (car r::*echo-message*)))
        (l:peek-end)))))

;;; ============================================== pulling the plane (#29)
;;;
;;; The wheel is not available: river's seat protocol has pointer_position,
;;; pointer_enter, pointer_leave and press/release bindings, and no axis event
;;; anywhere.  What the pointer CAN do is press, move and release, which is a
;;; drag -- so the plane is a handle and the jump is aimed, and neither needs a
;;; compositor to be true.

(test a-drag-on-empty-space-takes-hold-of-the-plane
  "Super+click over a gap between cells is a press on nothing, and nothing is
exactly what a plane handle is.  Claimed, so no window drag starts."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*peek* nil)
          (l:*drag-pan* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (is (null (l:drag-panning-p)))
        (is-true (p:pointer-press policy world 400 300 :move)
                 "the press is claimed")
        (is-true (l:drag-panning-p) "and a drag is running")
        (l:drag-pan-end)
        (is (null (l:drag-panning-p)))))))

(test a-drag-over-a-window-is-a-window-drag-not-a-pan
  "The one thing the plane handle must never take: a press on a window is how
windows are moved, and it was before any of this."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (window (t*::win "editor")))
      (setf (c:window-rect window) (c:make-rect 0 0 800 600))
      (setf (c:world-root world)
            (c:make-stack (list (c:make-leaf window)) 0))
      (let ((r:*world* world)
            (p:*policy* policy))
        (is (null (p:pointer-press policy world 400 300 :move))
            "a press on a window is not claimed")
        (is (null (l:drag-panning-p)))))))

(test dragging-a-cell-width-pans-exactly-one-cell
  "By cells, not by pixels: there is no half-cell to pan to, and moving the
camera by pixels would relayout every window on screen sixty times a second and
land on the same integer cell anyway."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let* ((policy (pol))
           (l:*drag-pan* nil)
           (output (first (c:world-outputs world)))
           (viewport (l:grid-viewport grid)))
      (let ((r:*world* world)
            (p:*policy* policy))
        (setf (l:viewport-cols viewport) 2
              (l:viewport-rows viewport) 2
              (l:viewport-origin viewport) (l:cell 0 0))
        (multiple-value-bind (pitch-x pitch-y) (l:cell-pitch policy grid output)
          (declare (ignore pitch-y))
          (l:drag-pan-begin policy grid output 400 300)
          ;; Half a cell of motion is not a pan at all.
          (l:drag-pan-update (round (+ 400 (/ pitch-x 2))) 300)
          (is (equal (l:cell 0 0) (l:viewport-origin viewport))
              "half a cell of drag moves nothing")
          ;; A whole one is.  Dragged right, the plane comes right, so the
          ;; camera goes left.
          (l:drag-pan-update (round (+ 400 pitch-x)) 300)
          (is (equal (l:cell -1 0) (l:viewport-origin viewport))
              "one cell of drag, one cell of pan")
          (l:drag-pan-end))))))

(test a-drag-down-looks-up-the-plane
  "Wayland +Y is down and lattice +Y is up, so a drag towards the bottom of the
screen shows the cells ABOVE where you were.  The sign is worth a test because
it is the one a reader guesses wrong."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let* ((policy (pol))
           (l:*drag-pan* nil)
           (output (first (c:world-outputs world)))
           (viewport (l:grid-viewport grid)))
      (let ((r:*world* world)
            (p:*policy* policy))
        (setf (l:viewport-cols viewport) 2
              (l:viewport-rows viewport) 2
              (l:viewport-origin viewport) (l:cell 0 0))
        (multiple-value-bind (pitch-x pitch-y) (l:cell-pitch policy grid output)
          (declare (ignore pitch-x))
          (l:drag-pan-begin policy grid output 400 300)
          (l:drag-pan-update 400 (round (+ 300 pitch-y)))
          (is (equal (l:cell 0 1) (l:viewport-origin viewport))
              "the camera moved up")
          (l:drag-pan-end))))))

(test letting-go-ends-the-drag-and-motion-away-from-one-does-nothing
  "The hook fires on every motion, so it is the check that has to be cheap and
the release that has to be complete."
  (let ((l:*drag-pan* nil))
    (is (null (l:drag-pan-tracking 10 10))
        "no drag, no work, no error")
    (multiple-value-bind (grid rect world) (a-two-cell-plane)
      (let* ((policy (pol))
             (output (first (c:world-outputs world)))
             (viewport (l:grid-viewport grid)))
        (let ((r:*world* world))
          (setf (l:viewport-origin viewport) (l:cell 0 0))
          (l:drag-pan-begin policy grid output 400 300)
          (l:drag-pan-released :move)
          (is (null (l:drag-panning-p)) "the button coming up ends it")
          (is (equal (l:cell 0 0) (l:viewport-origin viewport))
              "and motion afterwards moves nothing"))))))

(test zoom-at-pointer-anchors-on-the-cell-under-it
  "Zooming toward the thing you are pointing at instead of the thing you are
standing in -- the difference a map makes."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let* ((policy (pol))
           (l:*peek* nil)
           (output (first (c:world-outputs world)))
           (viewport (l:grid-viewport grid)))
      (let ((r:*world* world)
            (p:*policy* policy))
        (setf (l:viewport-cols viewport) 2
              (l:viewport-rows viewport) 2
              (l:viewport-origin viewport) (l:cell 0 0)
              (c:world-cursor world) (list 0 (l:cell 0 0)))
        (let* ((drawn (cdr (assoc (l:cell 1 0)
                                  (l:cell-rects policy grid rect
                                                :viewport (l:grid-viewport grid))
                                  :test #'equal))))
          (multiple-value-bind (x y) (centre-of drawn)
            (l:zoom-at-pointer :in)
            (let ((origin (l:viewport-origin viewport)))
              (is (member (list (l:cell-x origin) (l:cell-y origin))
                          (list (list 1 0) (list 1 -1) (list 0 0) (list 0 -1))
                          :test #'equal)
                  "the zoom landed on the pointer's cell, not on the cursor"))))))))

(defvar *seen-motion* nil "What the :pointer-moved hook was handed, newest first.")

(defun watch-motion (x y)
  (push (cons x y) *seen-motion*))

(test the-drag-runs-on-the-seats-own-hooks
  "Fired through RUN-HOOKS rather than by calling the functions, because the
claim is about the seam: a press, a move and a release are three moments, and a
policy that answered only the middle one would never know the drag ended."
  (let ((l:*drag-pan* nil)
        (*seen-motion* nil))
    (p:add-hook :pointer-moved 'watch-motion)
    (unwind-protect
         (multiple-value-bind (grid rect world) (a-two-cell-plane)
           (let* ((policy (pol))
                  (output (first (c:world-outputs world)))
                  (viewport (l:grid-viewport grid)))
             (let ((r:*world* world))
               (setf (l:viewport-origin viewport) (l:cell 0 0))
               (l:drag-pan-begin policy grid output 400 300)
               (p:run-hooks :pointer-moved 460 300)
               (is (equal '((460 . 300)) *seen-motion*)
                   "the hook was handed the pointer's position")
               (p:run-hooks :pointer-released :move)
               (is (null (l:drag-panning-p))
                   "and the release ended the drag"))))
      (p:remove-hook :pointer-moved 'watch-motion))))

;;; ======================================== dragging a window into a cell (#28)
;;;
;;; Pointer drags already move a window inside its output -- the drag floats it
;;; and the float follows the pointer.  What was missing is the *cell* the hand
;;; let go over: the plane is the workspace, so a drag that ends on a different
;;; cell should land there rather than leaving the window floating on top of the
;;; layout it was aimed through.

(defun a-plane-with-a-window-at (policy world grid address app)
  "Put APP into ADDRESS, and answer the window."
  (let ((window (t*::win app)))
    (setf (c:child-at grid address) (c:make-leaf window))
    (setf (c:window-rect window) (c:make-rect 0 0 400 300))
    (values window (p:name-of-window window))))

(test a-press-on-a-window-that-lives-in-a-cell-remembers-where-it-came-from
  "The press is NOT claimed: the ordinary float-drag has to run, because a
window that does not follow the pointer is not a drag.  What is recorded is the
cell it came from, so that letting go can tell a move from a drag in place."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (l:*drag-dock* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (a-plane-with-a-window-at policy world grid (l:cell 0 0) "editor")
        (setf (c:window-rect (c:leaf-window (c:child-at grid (l:cell 0 0))))
              (c:make-rect 0 0 400 300))
        (is (null (p:pointer-press policy world 100 100 :move))
            "unclaimed, so the float-drag runs")
        (is (equal (l:cell 0 0) (getf l:*drag-dock* :from))
            "and the cell it came from is recorded")
        (setf l:*drag-dock* nil)))))

(test a-press-on-a-float-remembers-nothing
  "A float is not in a cell, so there is no cell to drop it into and no reason
to touch where it lands: dragging a float leaves a float, as it always did."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (l:*drag-dock* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (let ((window (t*::win "mpv")))
          (setf (c:window-rect window) (c:make-rect 0 0 400 300))
          (setf (c:world-floats world)
                (list (make-instance 'c:floating-window :window window
                                     :rect (c:make-rect 0 0 400 300))))
          (is (null (p:pointer-press policy world 100 100 :move)))
          (is (null l:*drag-dock*) "nothing recorded"))))))

(test letting-go-over-another-cell-puts-the-window-there
  "The whole feature, from the press to the tree: a window picked up in cell 0,0
and let go over cell 1,0 is in cell 1,0 afterwards and no longer floating."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (l:*drag-dock* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (a-plane-with-a-window-at policy world grid (l:cell 0 0) "editor")
        (let ((window (c:leaf-window (c:child-at grid (l:cell 0 0)))))
          (setf (c:window-rect window) (c:make-rect 0 0 400 300))
          (p:pointer-press policy world 100 100 :move)
          ;; The drag floats it, exactly as a real one would.
          (r:float-window-now policy window)
          (is-true (c:window-floating-p window) "the drag floated it")
          (let ((drawn (cdr (assoc (l:cell 1 0)
                                   (l:cell-rects policy grid rect
                                                 :viewport (l:grid-viewport grid))
                                   :test #'equal))))
            (multiple-value-bind (x y) (centre-of drawn)
              (l:drag-pan-tracking x y)
              (l:drag-pan-released :move)))
          (is (null (c:window-floating-p window)) "and the drop unfloated it")
          (is (eq window (c:leaf-window (c:leaf-holding (c:child-at grid (l:cell 1 0))
                                                        window)))
              "the window is in the cell the hand let go over"))))))

(test letting-go-where-you-picked-it-up-moves-nothing
  "A drag that ends where it began is a drag, not a move.  The window goes back
into the tree it came out of and the plane keeps its shape."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (l:*drag-dock* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (a-plane-with-a-window-at policy world grid (l:cell 0 0) "editor")
        (let ((window (c:leaf-window (c:child-at grid (l:cell 0 0)))))
          (setf (c:window-rect window) (c:make-rect 0 0 400 300))
          (p:pointer-press policy world 100 100 :move)
          (r:float-window-now policy window)
          (let ((drawn (cdr (assoc (l:cell 0 0)
                                   (l:cell-rects policy grid rect
                                                 :viewport (l:grid-viewport grid))
                                   :test #'equal))))
            (multiple-value-bind (x y) (centre-of drawn)
              (l:drag-pan-tracking x y)
              (l:drag-pan-released :move)))
          (is (null (c:window-floating-p window)))
          (is (equal (l:cell 0 0) (l:cell-holding grid window))
              "and it is back in the cell it came from"))))))

(test letting-go-off-the-plane-leaves-the-window-floating
  "Dropped over nothing, a window is a float -- which is what a drag has always
meant, and what #13's stacking is for."
  (multiple-value-bind (grid rect world) (a-two-cell-plane)
    (let ((policy (pol))
          (l:*drag-pan* nil)
          (l:*drag-dock* nil))
      (let ((r:*world* world)
            (p:*policy* policy))
        (a-plane-with-a-window-at policy world grid (l:cell 0 0) "editor")
        (let ((window (c:leaf-window (c:child-at grid (l:cell 0 0)))))
          (setf (c:window-rect window) (c:make-rect 0 0 400 300))
          (p:pointer-press policy world 100 100 :move)
          (r:float-window-now policy window)
          (l:drag-pan-tracking -4000 -4000)
          (l:drag-pan-released :move)
          (is-true (c:window-floating-p window) "it is still a float")
          (is (null l:*drag-dock*) "and the drag is forgotten"))))))

;;; ============================================ the whole session at once (#23)
;;;
;;; The thumbnail half of the issue needs screencopy, which this program does
;;; not speak -- grim speaks it, and doc/FINDINGS.org records the day.  What
;;; DOES not need pixels is the question a tiler is actually asking: what will
;;; my screen look like when I go there.  That is a drawing of the model.

(test preview-panels-are-one-per-workspace-and-do-not-overlap
  "Pure arithmetic, and the same function draws a panel and finds the panel a
click landed in -- the rule CELL-RECTS and MAP-CELL-AT already follow."
  (let* ((area (c:make-rect 0 0 1600 900))
         (panels (l:preview-panels 4 area)))
    (is (= 4 (length panels)))
    (is (equal '(0 1 2 3) (mapcar #'car panels)) "in workspace order")
    (loop for (a . ra) in panels
          do (loop for (b . rb) in panels
                   unless (eql a b)
                     do (is (null (c:rect-intersect ra rb))
                            "panels ~d and ~d do not overlap" a b)))
    (loop for (index . rect) in panels
          do (is-true (c:rect-contains-p area (c:rect-x rect) (c:rect-y rect))
                      "panel ~d starts inside the area" index))))

(test a-panel-grows-as-the-count-shrinks
  "One workspace gets the whole wall; eighteen get a grid of them."
  (let ((area (c:make-rect 0 0 1600 900)))
    (let* ((one (cdr (first (l:preview-panels 1 area))))
           (many (mapcar #'cdr (l:preview-panels 12 area))))
      (is (> (c:rect-w one) (c:rect-w (first many)))
          "one workspace is a bigger picture than twelve")
      (is (= 1 (length (l:preview-panels 1 area))))
      (is (= 12 (length (l:preview-panels 12 area))))
      (is (= 0 (length (l:preview-panels 0 area))) "no workspaces, no wall")
      (is (= 4 (l:preview-columns 12)) "as square a wall as the count allows"))))

(test a-point-on-a-panel-names-that-workspace
  (let* ((area (c:make-rect 0 0 1600 900))
         (panels (l:preview-panels 6 area)))
    (loop for (index . rect) in panels
          do (multiple-value-bind (x y) (centre-of rect)
               (is (eql index (l:preview-panel-at panels x y))
                   "the middle of panel ~d is panel ~d" index index)))
    (is (null (l:preview-panel-at panels -500 -500)) "and nothing is no panel")))

(test the-preview-shows-shows-the-session-and-any-key-takes-it-down
  "A glance, not a mode -- the same ruling the peek made, for the same reason."
  (with-lattice-world ()
    (let ((l:*preview* nil))
      (r::workspace 3)
      (is-true (l:preview))
      (is-true (l:preview-active-p) "the wall is up")
      (p:on-key p:*policy* world (cons 38 (list :none)))
      (is (null (l:preview-active-p)) "and one keypress later it is not")
      (r:mark-dirty))))

(test clicking-a-panel-goes-to-that-workspace
  "The press decode, end to end: a point in the middle of the third panel puts
you on the third workspace."
  (with-lattice-world ()
    (let ((l:*preview* nil))
      (r::workspace 3)
      (setf (c:world-outputs world)
            (list (make-instance 'c:output :name "DP-1"
                                 :rect (c:make-rect 0 0 1600 900))))
      (let* ((output (first (c:world-outputs world)))
             (area (p:outer-rect p:*policy* output))
             (panels (l:preview-panels 3 area)))
        (l:preview-begin)
        (multiple-value-bind (x y) (centre-of (cdr (nth 2 panels)))
          (is-true (p:pointer-press p:*policy* world x y :move)
                   "the panel claims the press")
          (is (null (l:preview-active-p)) "and puts the wall away")
          (is (eql 2 (p:output-workspace output))
              "where you asked to be"))))))

(test the-preview-draws-every-workspace-including-the-empty-ones
  "An overview that draws only what has windows in it is an overview you cannot
tell you have thirteen workspaces from."
  (with-lattice-world ()
    (r::workspace 4)
    (let* ((root (c:world-root world))
           (count (if (typep root 'c:stack) (c:container-count root) 1))
           (panels (l:preview-panels count (c:make-rect 0 0 1600 900))))
      (is (= 4 count))
      (is (= 4 (length panels)) "four panels for four workspaces, empty or not"))))

(test preview-off-takes-the-wall-down-and-says-whether-it-did
  "The verb for a key of its own, since the panel wall is the one picture a
person may want to look at for longer than one keystroke."
  (let ((l:*preview* nil))
    (is (null (l:preview-off)) "nothing to put away")
    (l:preview-begin)
    (is-true (l:preview-off))
    (is (null (l:preview-active-p)))))
