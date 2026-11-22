;;;; tests/test-grid.lisp --- A rectangle of cells, without a screen.
;;;;
;;;; The shape every container-kind suite in this project has: what became the
;;;; container, how it divides its rectangle, how motion crosses it, and whether
;;;; its own state survives undo and a round trip through the file format.  The
;;;; last section is the one that is easy to skip and expensive to skip: a grid
;;;; that forgets its column count through undo is a grid that silently becomes
;;;; a list.

(defpackage #:grid/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:g #:grid))
  (:export #:run-all))

(in-package #:grid/tests)

(def-suite grid :description "n windows in a rectangle of cells.")
(in-suite grid)

(t*:register-extension-suite "GRID/TESTS" "GRID")

(defun run-all ()
  (let ((results (run 'grid)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))

(defmacro with-grid ((&key (windows 5) columns) &body body)
  "A fresh world whose workspace has been turned into a grid."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     (open-windows ,windows)
     (g:grid ,columns)
     ,@body))

(defun the-grid (&optional (world r:*world*))
  (c:resolve-path (c:world-root world) '(0)))

(defun visible-leaf-cells (&optional (world r:*world*))
  "The rectangles actually drawn for leaves, keyed by the grid's own address.

The last address of the placement's path is the cell, because the grid sits at
the workspace's path and the cells are its children."
  (let ((placements (p:layout-node p:*policy* (c:world-root world)
                                   (c:make-rect 0 0 1000 800)))
        (by-address (make-hash-table :test #'eql)))
    (dolist (pl placements)
      (when (and (typep (first pl) 'c:leaf) (fourth pl))
        (setf (gethash (car (last (second pl))) by-address) (third pl))))
    by-address))

;;; ================================================================ tests

(test every-window-became-a-cell
  (with-grid (:windows 5)
    (let ((grid (the-grid)))
      (is (g:grid-p grid))
      (is (= 5 (c:container-count grid)) "every window became a cell")
      (is (equal '("app0" "app1" "app2" "app3" "app4")
                 (loop for i from 0 below 5
                       for child = (c:child-at grid i)
                       collect (and (typep child 'c:leaf)
                                    (c:window-app-id (c:leaf-window child)))))
          "in the order they were already in"))))

(test the-default-width-is-as-square-as-it-goes
  (with-grid (:windows 5)
    (is (null (g:grid-column-count (the-grid)))
        "nothing was stated, so the width is computed rather than stored")
    (is (= 3 (length (column-positions (visible-leaf-cells))))
        "*GRID-COLUMNS* is NIL, so five windows give three columns and two rows")
    (is (= 2 (length (row-positions (visible-leaf-cells)))))))

(test a-stated-width-wins
  (with-grid (:windows 5 :columns 2)
    (is (= 2 (g:grid-column-count (the-grid))))))

(test the-option-supplies-the-default-when-no-width-is-typed
  (with-grid (:windows 4)
    ;; ASKED AGAIN through the option, which is what a profile or a rule sets.
    (let ((g:*grid-columns* 4))
      (g:grid)
      (is (= 4 (g:grid-column-count (the-grid))))
      (is (= 4 (g:grid-default-columns 4))))
    (let ((g:*grid-columns* nil))
      (is (= 2 (g:grid-default-columns 4)) "back to square"))))

(test a-nonsense-option-falls-back-to-square
  "The same ruling *workspace-pins* and *workspace-nav* get: a value nobody can
use leaves the desk as it was rather than breaking it."
  (with-grid (:windows 4)
    (let ((g:*grid-columns* :three))
      (is (= 2 (g:grid-default-columns 4))))
    (let ((g:*grid-columns* 0))
      (is (= 2 (g:grid-default-columns 4))))))

(defun column-positions (cells)
  "The distinct x positions drawn, which is how many columns there are."
  (remove-duplicates
   (loop for address from 0 below (hash-table-count cells)
         for rect = (gethash address cells)
         when rect collect (c:rect-x rect))))

(defun row-positions (cells)
  "The distinct y positions drawn, which is how many rows there are."
  (remove-duplicates
   (loop for address from 0 below (hash-table-count cells)
         for rect = (gethash address cells)
         when rect collect (c:rect-y rect))))

(test the-cells-tile-the-rectangle-with-no-overlap-and-no-gap
  (with-grid (:windows 5 :columns 3)
    (let* ((cells (visible-leaf-cells))
           (rects (loop for address from 0 below 5
                        collect (gethash address cells))))
      (is (every (lambda (r) (and r (plusp (c:rect-w r)) (plusp (c:rect-h r))))
                 rects)
          "every cell was drawn")
      (loop for i from 0 below 5
            do (loop for j from (1+ i) below 5
                     do (is (null (c:rect-intersect (nth i rects) (nth j rects)))
                            "cells ~d and ~d overlap" i j)))
      (is (= 3 (length (column-positions cells))) "three columns were drawn")
      (is (= 2 (length (row-positions cells))) "and two rows")
      (is (= 800 (apply #'max (loop for address from 0 below 5
                                    for rect = (gethash address cells)
                                    when rect collect (c:rect-bottom rect))))
          "the rows together are exactly the screen's height")
      (is (<= 999 (loop for x in (column-positions cells)
                        sum (let ((rect (loop for address from 0 below 5
                                              for candidate = (gethash address cells)
                                              when (and candidate
                                                        (= x (c:rect-x candidate)))
                                                return candidate)))
                                (if rect (c:rect-w rect) 0))))
          "and the columns' widths add up to its width, so a full row leaves no
seam"))))

(test the-last-row-keeps-the-columns-lined-up
  "The obvious alternative -- dividing the last row among the children it has --
gives a 5-window grid two bottom cells half as wide again as the three above
them, and the columns stop lining up.  A hole at the end is the better answer."
  (with-grid (:windows 5 :columns 3)
    (let* ((cells (visible-leaf-cells))
           (widths (loop for address from 0 below 5
                         collect (c:rect-w (gethash address cells))))
           (xs (loop for address from 0 below 5
                     collect (c:rect-x (gethash address cells)))))
      (is (>= 1 (- (reduce #'max widths) (reduce #'min widths)))
          "every cell is the same width to within the pixel that rounding
accumulates: ~s" widths)
      (is (= (nth 0 xs) (nth 3 xs))
          "the fourth cell starts where the first one did")
      (is (= (nth 1 xs) (nth 4 xs))
          "and the fifth where the second did")
      (is (= 2 (length (row-positions cells)))
          "two rows, because five does not fill three columns twice"))))

(test one-window-is-not-a-narrow-strip
  (with-grid (:windows 1)
    (let ((rect (gethash 0 (visible-leaf-cells))))
      (is (= 1000 (c:rect-w rect)) "the single cell takes the whole width")
      (is (= 800 (c:rect-h rect))))))

(test a-width-wider-than-the-count-is-clamped
  "Three windows with a stated width of ten would otherwise draw three narrow
cells and seven holes."
  (with-grid (:windows 3 :columns 10)
    (let ((rects (loop for address from 0 below 3
                       collect (gethash address (visible-leaf-cells)))))
      (is (= 3 (length (remove nil rects))))
      (is (= 1000 (reduce #'+ (mapcar #'c:rect-w rects)))))))

;;; ------------------------------------------------------------- motion

(test right-crosses-into-the-next-row-and-left-comes-back
  (with-grid (:windows 5 :columns 3)
    (let ((grid (the-grid)))
      (is (= 1 (p:step-address p:*policy* grid 0 :right)))
      (is (= 3 (p:step-address p:*policy* grid 2 :right))
          "the end of a row is the start of the next one, in reading order")
      (is (= 2 (p:step-address p:*policy* grid 3 :left))
          "and back again")
      (is (null (p:step-address p:*policy* grid 4 :right))
          "the last cell is the last cell")
      (is (null (p:step-address p:*policy* grid 0 :left))))))

(test down-moves-a-whole-row-and-refuses-past-the-edge
  (with-grid (:windows 5 :columns 3)
    (let ((grid (the-grid)))
      (is (= 3 (p:step-address p:*policy* grid 0 :down)))
      (is (= 4 (p:step-address p:*policy* grid 1 :down)))
      (is (null (p:step-address p:*policy* grid 2 :down))
          "the third column of the first row has nothing under it in a short row")
      (is (null (p:step-address p:*policy* grid 3 :down)))
      (is (= 0 (p:step-address p:*policy* grid 3 :up))))))

(test arriving-lands-on-the-first-cell
  (with-grid (:windows 5 :columns 3)
    (let ((grid (the-grid)))
      (is (= 0 (p:entry-address p:*policy* grid :right nil nil)))
      (is (= 0 (p:entry-address p:*policy* grid :left nil nil))))))

(test arriving-from-above-keeps-the-column-you-were-in
  "A screen above a grid that always landed on the first cell would reset the
pointer to the left edge at every boundary."
  (with-grid (:windows 6 :columns 3)
    (is (= 1 (p:entry-address p:*policy* (the-grid) :down 1 nil)))
    (is (= 2 (p:entry-address p:*policy* (the-grid) :down 2 nil)))
    (is (= 0 (p:entry-address p:*policy* (the-grid) :down 3 nil))
        "a reference that names no column comes back to the first")))

;;; ---------------------------------------------------------------- verbs

(test grid-columns-changes-the-drawing-and-not-the-tree
  (with-grid (:windows 5 :columns 2)
    (let* ((tree-before (t*::shape (c:world-root r:*world*)))
           (rows-before (length (remove-duplicates
                                 (loop for address from 0 below 5
                                       collect (c:rect-y (gethash address
                                                                  (visible-leaf-cells))))))))
      (g:grid-columns 5)
      (is (= 5 (g:grid-column-count (the-grid))))
      (is (equal tree-before (t*::shape (c:world-root r:*world*)))
          "only the divisor changed")
      (is (< (length (remove-duplicates
                      (loop for address from 0 below 5
                            collect (c:rect-y (gethash address
                                                       (visible-leaf-cells))))))
             rows-before)
          "and it is now one row tall"))))

(test grid-columns-outside-a-grid-says-so-rather-than-refusing-silently
  (let ((r:*world* (c:make-world))
        (p:*policy* (make-instance 'p:conventional-policy)))
    (open-windows 2)
    (is (stringp (g:grid-columns 3))
        "the echo area was told why nothing happened")))

(test running-grid-again-sets-the-width-instead-of-nesting
  (with-grid (:windows 5 :columns 2)
    (g:grid 4)
    (is (= 4 (g:grid-column-count (the-grid))))
    (is (= 5 (c:container-count (the-grid)))
        "still one container of five cells, not a grid inside a grid")))

;;; --------------------------------------------------------------- state

(test a-grid-remembers-its-width-through-an-undo-snapshot
  "NODE-SIGNATURE includes the width, so setting it records a snapshot and undo
can bring it back -- the same rule the strip's scroll position follows.  The
assertion is the property rather than the shape of the form: two widths are two
different states, and one width is one state however often it is asked for."
  (with-grid (:windows 4 :columns 2)
    (let ((two (c:node-signature (the-grid))))
      (is (equal two (c:node-signature (the-grid)))
          "the same grid is the same state")
      (g:grid-columns 4)
      (is (not (equal two (c:node-signature (the-grid))))
          "a different width is a different state, so undo has something to do"))))

(test a-grid-survives-a-round-trip-through-the-file-format
  (with-grid (:windows 5 :columns 3)
    (let* ((grid (the-grid))
           (form (r:serialize-node grid))
           (back (r:read-node form 0)))
      (is (g:grid-p back))
      (is (= 3 (g:grid-column-count back)) "the width came back")
      (is (= 5 (c:container-count back)) "and every cell"))))

(test an-old-file-with-no-width-still-makes-a-grid
  "A hand-edited or older file produces a grid with fewer facts rather than an
error, and SIMPLIFY-NODE guarantees it is still valid."
  (let ((back (r:read-node (list :grid/grid :label "old" :cells nil) 0)))
    (is (g:grid-p back))
    (is (null (g:grid-column-count back)) "back to as-square-as-it-goes")
    (is (= 1 (c:container-count back)) "an empty grid is not a valid state")))

;;; --------------------------------------------------- it changes nothing else

(test the-shipped-policy-still-draws-a-split-the-old-way
  "A container kind must not change what other containers do.  Loading this
module adds methods on GRID-CONTAINER; a split is answered by the shipped
method, and two windows in a split are two panes rather than two cells."
  (let ((r:*world* (c:make-world))
        (p:*policy* (make-instance 'p:conventional-policy)))
    (open-windows 2)
    (let* ((root (c:world-root r:*world*))
           (placements (p:layout-node p:*policy* root (c:make-rect 0 0 1000 800)))
           (leaves (remove-if-not (lambda (pl) (typep (first pl) 'c:leaf))
                                  placements)))
      (is (= 2 (length leaves)))
      (is (not (g:grid-p (c:resolve-path root '(0))))
          "the workspace is still a stack, because nothing turned it into a
grid -- the module is loaded, and that is all"))))
