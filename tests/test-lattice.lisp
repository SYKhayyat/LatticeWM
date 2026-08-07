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

(test the-plane-s-own-state-is-part-of-its-signature
  "UNDO SKIPPED EVERY VERB THAT CHANGES THE PLANE AND NOT THE TREE.

Layout undo records a snapshot only when C:NODE-SIGNATURE changed, and the grid
did not answer that generic — so zoom, pan, resize-column, resize-row and
name-cell recorded nothing.  Pressing undo after any of them jumped back to a
*previous tree* and, because COPY-NODE-SLOTS does its job, silently reverted
the zoom and the tracks along with it.  The visible symptom was undo appearing
to skip a step, which reads as an undo bug rather than as a missing method.

It was found by generating the container-protocol surface and reading it: the
grid answered thirteen of the nineteen members, and nothing before that had
ever stated that there are nineteen."
  (let ((grid (grid-of)))
    (setf (c:child-at grid (l:cell 0 0)) (leaf "a"))
    ;; Start on a rung of the ladder, since that is what SET-ZOOM moves between.
    (l:set-zoom grid 0 :focus (l:cell 0 0))
    (let ((base (c:node-signature grid)))
      ;; Zoom.  Recorded with its origin, because SET-ZOOM re-anchors the
      ;; viewport around the focused cell -- so returning to the same rung is
      ;; the same arrangement only when the origin came back too.
      (l:set-zoom grid 4 :focus (l:cell 0 0))
      (is (not (equal base (c:node-signature grid)))
          "a zoom is invisible to undo")
      (l:set-zoom grid 0 :focus (l:cell 0 0))
      (is (equal base (c:node-signature grid))
          "and zooming back to where you started is the same arrangement"))
    ;; Pan.
    (let ((before (c:node-signature grid)))
      (setf (l:viewport-origin (l:grid-viewport grid)) (l:cell 3 -2))
      (is (not (equal before (c:node-signature grid)))
          "a pan is invisible to undo"))
    ;; Tracks.
    (let ((before (c:node-signature grid)))
      (setf (l:col-width grid 0) 5/2)
      (is (not (equal before (c:node-signature grid)))
          "resizing a column is invisible to undo"))
    (let ((before (c:node-signature grid)))
      (setf (l:row-height grid 0) 3)
      (is (not (equal before (c:node-signature grid)))
          "resizing a row is invisible to undo"))
    ;; Names.
    (let ((before (c:node-signature grid)))
      (setf (gethash "mail" (l:grid-names grid)) (l:cell 0 0))
      (is (not (equal before (c:node-signature grid)))
          "naming a cell is invisible to undo")))
  ;; And the signature must not vary with hash table iteration order, or every
  ;; snapshot looks like a change and the ring fills with identical trees.
  (let ((a (grid-of)) (b (grid-of)))
    (dolist (g (list a b))
      (setf (c:child-at g (l:cell 0 0)) (c:make-leaf)))
    (loop for x in '(5 -3 0 12) do (setf (l:col-width a x) 2))
    (loop for x in '(0 12 -3 5) do (setf (l:col-width b x) 2))
    ;; The first eight elements are the plane's own state; everything after
    ;; them is the container method's answer, which carries node ids that are
    ;; deliberately unique per node and so can never compare equal.
    (is (equal (subseq (c:node-signature a) 0 8)
               (subseq (c:node-signature b) 0 8))
        "two grids with the same tracks entered in a different order must
compare equal")))

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
