;;;; grid/grid.lisp --- A rectangle of cells, in reading order.
;;;;
;;;; A container kind, like the strip in extensions/scrolling-columns and like
;;;; the lattice: the current workspace's windows become the children of one
;;;; container that decides where they are drawn.  Nothing in the tree changes
;;;; shape -- the leaves stay leaves, in the order they were already in -- so
;;;; turning a workspace into a grid and back is a swap of the container at the
;;;; workspace's path and nothing else.
;;;;
;;;; WHY THE LAST ROW DOES NOT REFLOW.  Every row is divided into the same
;;;; number of columns, and a row with fewer children simply leaves the last
;;;; cells empty.  The alternative -- dividing each row among the children it
;;;; has -- gives a 5-window 3-column grid two cells across the bottom that are
;;;; half as wide again as the ones above them, and the columns stop lining up,
;;;; which is the one thing a grid is for.  A hole at the end of the last row is
;;;; what a person draws on paper, and it is what CSS grid does by default.

(in-package #:grid)

;;; ------------------------------------------------------------- the option

(p:define-option *grid-columns* nil
  "How many columns a new grid has, or NIL to work it out from the count.

NIL -- the shipped answer -- means *as square as it goes*: (CEILING (SQRT N))
columns for n windows, which is one, two, two, two, three, three, three for one
to seven.  A number is a desk that always wants that many columns, including a
single narrow column of full-width rows, which is what (GRID 1) gives you and
what some people mean by a list.

Read through GRID-DEFAULT-COLUMNS, which refuses a value that would make an
empty grid.")

(defun grid-default-columns (count)
  "Columns for COUNT windows when nobody has said otherwise."
  (let ((want *grid-columns*))
    (cond
      ((and (integerp want) (plusp want)) want)
      ((not (integerp count)) 1)
      (t (max 1 (ceiling (sqrt (max 1 count))))))))

;;; ------------------------------------------------------------- the container

(defclass grid-container (c:sequential-container)
  ((columns :initarg :columns :initform nil :accessor grid-column-count
            :documentation "Fixed column count, or NIL to compute it from the
number of children.  Held on the container rather than in the policy because
it is a property of this workspace's arrangement and of nothing else.  Named
-count and not -columns so that the command GRID-COLUMNS and the reader do not
collide, which is the bug this project has already paid for twice."))
  (:documentation
   "A rectangle of cells, filled in reading order: left to right, then down."))

(defun grid-p (thing)
  "True when THING is a grid."
  (typep thing 'grid-container))

(defun row-count (grid)
  "How many rows the grid needs at its current width."
  (let ((n (c:container-count grid)))
    (ceiling n (max 1 (column-count grid)))))

(defun column-count (grid)
  "The columns this grid actually uses.

Never more than there are children, so a 3-column grid holding two windows draws
two cells side by side rather than two narrow ones and a hole."
  (let ((n (c:container-count grid)))
    (max 1 (min (max 1 (or (grid-column-count grid)
                           (grid-default-columns n)))
                (max 1 n)))))

(defmethod c:default-address ((grid grid-container))
  "Focus repair lands on the first cell, which is where a reader starts."
  0)

(defmethod c:simplify-node ((grid grid-container))
  "A grid survives as a singleton; an empty one regains a cell.

An empty grid is not a simpler state, it is a broken one -- the same ruling the
strip makes."
  (when (zerop (c:container-count grid))
    (setf (c:children grid) (list (c:make-leaf))))
  grid)

;;; ---------------------------------------------------------- how it is drawn

(defmethod p:layout-children ((policy p:conventional-policy) (grid grid-container)
                              rect)
  "Divide RECT into rows, then each row into cells.

Rows first, because a grid's rows are the unit that stays put: a window added to
the last row must not resize every cell above it more than it has to, and a
division done column-major gives different rounding.  C: DIVIDE-RECT accumulates
its rounding, so the cells tile the rectangle exactly and no seam appears."
  (let* ((n (c:container-count grid))
         (columns (column-count grid))
         (rows (max 1 (ceiling n columns)))
         (gap (p:gaps policy grid))
         (row-rects (c:divide-rect rect :vertical
                                    (make-list rows :initial-element 1)
                                    :gap gap))
         (placements '()))
    (loop for row from 0 below rows
          for row-rect in row-rects
          for start = (* row columns)
          for width = (min columns (- n start))
          ;; THE WHOLE ROW IS DIVIDED, and only the cells that have a child are
          ;; used, so a column keeps the same x on every row.
          do (loop for cell in (c:divide-rect row-rect :horizontal
                                              (make-list columns :initial-element 1)
                                              :gap gap)
                   for offset from 0
                   while (< offset width)
                   do (push (cons (+ start offset) cell) placements)))
    (nreverse placements)))

;;; ------------------------------------------------------ how it is navigated

(defmethod p:step-address ((policy p:conventional-policy) (grid grid-container)
                           address direction)
  "Reading order sideways, whole rows up and down.

Left and Right are ADDRESS +/- 1, which crosses into the next row at a row's end
and back into the previous one at its start -- that is what reading order means,
and refusing at the end of a row would make a grid harder to cross than a list.
Up and Down are ADDRESS +/- COLUMNS, and they answer NIL past the edge, because
there is nothing above the first row and nothing below the last: the grid is
bounded, unlike the strip."
  (let ((n (c:container-count grid))
        (columns (column-count grid)))
    (cond
      ((c:direction-horizontal-p direction)
       (let ((next (+ address (c:direction-sign direction))))
         (when (and (<= 0 next) (< next n)) next)))
      ((c:direction-vertical-p direction)
       (let ((next (+ address (* columns (c:direction-sign direction)))))
         (when (and (<= 0 next) (< next n)) next))))))

(defmethod p:entry-address ((policy p:conventional-policy) (grid grid-container)
                            direction reference rects)
  "Arrive at the first cell, whichever way you come at it.

The exception is a step DOWN from something above the grid, which lands in the
column you were in rather than the first one -- the reference tells us where you
came from, so a row of screens reads as one surface rather than resetting to the
left edge at every boundary."
  (declare (ignore rects))
  (let ((n (c:container-count grid)))
    (when (plusp n)
      (or (and (eq direction :down)
               reference
               (let* ((columns (column-count grid))
                      (wanted (mod reference columns)))
                 (min wanted (1- n))))
          0))))

;;; -------------------------------------------------- undo and snapshots

(defmethod c:copy-node-slots progn ((new grid-container) (old grid-container))
  "The column count is part of the arrangement.

Without this, undo restores the cells but not how they were divided -- the same
failure the strip records for its scroll position."
  (setf (grid-column-count new) (grid-column-count old)))

(defmethod c:node-signature ((grid grid-container))
  "The children, plus the width, so changing GRID-COLUMNS records a snapshot."
  (list* 'grid (grid-column-count grid) (call-next-method)))

;;; -------------------------------------------------------- persistence

(defmethod r:serialize-node ((grid grid-container))
  "The grid as a readable form, under a namespaced tag."
  (list :grid/grid
        :label (c:node-label grid)
        :columns (grid-column-count grid)
        :cells (let ((out '()))
                 (dotimes (i (c:container-count grid) (nreverse out))
                   (let ((child (c:child-at grid i)))
                     (when child (push (r:serialize-node child) out)))))))

(defmethod r:deserialize-node ((tag (eql :grid/grid)) plist index)
  "Rebuild the grid cell by cell.  Every field degrades rather than erroring: a
hand-edited or older file produces a grid with fewer facts, and SIMPLIFY-NODE
guarantees validity at the end."
  (let ((grid (make-instance 'grid-container
                             :columns (let ((columns (getf plist :columns)))
                                        (and (integerp columns)
                                             (plusp columns)
                                             columns)))))
    (setf (c:node-label grid) (getf plist :label))
    (loop for form in (getf plist :cells)
          do (let ((child (ignore-errors (r:read-node form index))))
               (when child
                 (c:insert-child grid (c:container-count grid) child))))
    (c:simplify-node grid)))

;;; ------------------------------------------------------- reachability

;;; A CONFIGURATION FILE IS READ IN LATTICEWM/USER, so an option whose symbol
;;; that package cannot see is not merely inconvenient: (setf *grid-columns* 3)
;;; in an init.lisp interns a NEW symbol there, sets that, and changes nothing.
;;; Every extension that defines an option says so here for the same reason
;;; buffers, launcher and transient-rules do.
(r:install-vocabulary '#:grid)

;;; --------------------------------------------------------------- verbs

(defun current-grid ()
  "The GRID-CONTAINER the cursor is inside, or NIL.

Looked for along the chain from the root to the cursor rather than by type on the
workspace alone, because a grid can be nested inside a split -- a common thing to
do to one half of a workspace."
  (find-if #'grid-p (c:resolve-chain (c:world-root r:*world*) (r:current-path))))

(r:defcommand grid-columns (columns)
  "Draw the grid the cursor is in with COLUMNS columns.

This is the grid's version of zoom, and it changes nothing about the tree: only
the divisor in the drawing.  Says so in the echo area when the cursor is not in
a grid, rather than refusing silently."
  (:interactive :number)
  (let ((grid (current-grid)))
    (if (null grid)
        (r:notify "the cursor is not in a grid")
        (progn
          (setf (grid-column-count grid) (max 1 columns))
          (r:relayout :force t)
          (r:notify "grid: ~d columns" (column-count grid))))))

(r:defcommand grid (&optional (columns nil))
  "Turn the current workspace into a grid of COLUMNS columns.

Every window that was in it becomes a cell, in reading order -- nothing is lost
and nothing is reordered.  With no argument the width is *GRID-COLUMNS* if it is
set, and otherwise as square as the count allows.

Running it on a workspace that is already a grid sets the width instead of
nesting one grid inside another, because that is what a person means by running
it twice, and a grid inside a grid is a thing you can still build by hand if you
want one."
  (:interactive :number)
  (let* ((world r:*world*)
         (path (c:workspace-path world))
         (workspace (c:resolve-path (c:world-root world) path))
         ;; THE CHAIN, not the node: the workspace is a stack holding the
         ;; windows, and asking a stack to FIND-IF is a type error.  Walking
         ;; root to cursor also catches a grid the user built inside one half
         ;; of a split, which is the case somebody will hit first.
         (existing (and workspace
                        (find-if #'grid-p (c:resolve-chain workspace '()))))
         (leaves (mapcar (lambda (leaf-path)
                           (c:resolve-path workspace leaf-path))
                         (c:leaf-paths workspace))))
    (cond
      ((null workspace)
       (r:notify "there is no current workspace"))
      (existing
       (setf (grid-column-count existing)
             (max 1 (or columns (grid-column-count existing)
                        (grid-default-columns (c:container-count existing)))))
       (r:relayout :force t)
       (r:notify "grid: ~d columns" (column-count existing)))
      (t
       (setf (c:world-root world)
             (c:tree-replace-at (c:world-root world) path
                                (make-instance 'grid-container
                                               :columns columns
                                               :children leaves)))
       (setf (c:world-cursor world) (c:repair-path (c:world-root world) path))
       (r:relayout :force t)
       (r:notify "grid: ~d cell~:p, ~d column~:p"
                 (length leaves)
                 (column-count (c:resolve-path (c:world-root world) path)))))))
