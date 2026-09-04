;;;; lattice/grid.lisp --- The infinite plane, as a fourth container kind.
;;;;
;;;; THIS FILE IS THE EXPERIMENT.
;;;;
;;;; DESIGN D21 makes the extensibility claim falsifiable by requiring the
;;;; lattice to be built from outside layer 0: "The rule: no core edits.  Every
;;;; time one appears necessary, stop and write down what was missing and why.
;;;; That list is the actual result of this project's central experiment."
;;;;
;;;; The sharper form the core made possible: the lattice is not a special mode
;;;; bolted onto the tree, it is *a fourth container kind*.  SPLIT is ordered
;;;; and dense; STACK is ordered and shows one child; GRID is sparse and
;;;; addressed by coordinate.  If the container protocol was drawn in the right
;;;; place, GRID needs to answer the same six questions the other two answer
;;;; and everything else — motion, layout recursion, tree surgery, focus
;;;; repair, persistence — works without being told the lattice exists.
;;;;
;;;; It does.  The findings are recorded in FINDINGS.org; the short version is
;;;; that one export was missing from the core and nothing else was.
;;;;
;;;; COORDINATES.  DESIGN D2: origin (0,0), +X right, +Y *up*.  Mathematical
;;;; convention, deliberately not Wayland's +Y-down, because it matches how a
;;;; person reads a map and the lattice is a map.  The cost is one sign
;;;; inversion, in CELL-RECTS below, and it is intentional — do not "fix" it.

(in-package #:lattice)

;;; ------------------------------------------------------------ coordinates

(declaim (inline cell cell-x cell-y))
(defun cell (x y) "The address of the cell at (X, Y)." (cons x y))
(defun cell-x (address) (car address))
(defun cell-y (address) (cdr address))

(defun cell-equal (a b)
  "True when A and B name the same cell."
  (and (consp a) (consp b) (eql (car a) (car b)) (eql (cdr a) (cdr b))))

(defun cell-string (address)
  "A cell address the way it is displayed: 0,0 or -3,2."
  (format nil "~d,~d" (cell-x address) (cell-y address)))

;;; ------------------------------------------------------------- the viewport

(defclass viewport ()
  ((origin :initarg :origin :initform (cell 0 0) :accessor viewport-origin
           :documentation
           "The lower-left visible cell.  Lower-left rather than upper-left
because +Y is up: the cell with the smallest X and smallest Y is the one whose
coordinates are easiest to reason about, and the inversion to screen space
happens in one place.")
   (cols :initarg :cols :initform 1 :accessor viewport-cols)
   (rows :initarg :rows :initform 1 :accessor viewport-rows)
   (props :initform '() :accessor c:props))
  (:documentation
   "The rectangle of lattice space currently on screen.

Zoom is the choice of COLS by ROWS.  That is the novel part of the whole
design: zoom is normally a *mode* you enter and leave — GNOME's overview,
niri's — and here it is a continuous layout control, one knob replacing both
workspace switching and tiling-layout selection."))

(defmethod print-object ((v viewport) stream)
  (print-unreadable-object (v stream :type t :identity nil)
    (format stream "~dx~d at ~a" (viewport-cols v) (viewport-rows v)
            (cell-string (viewport-origin v)))))

(defun viewport-cells (viewport)
  "Every visible cell address, in reading order — left to right, top to bottom.

Reading order, not coordinate order, because it is the order LAYOUT-CHILDREN
and RENDER-ORDER walk and the order the jump labels are assigned in.  With +Y
up, reading order means *descending* Y."
  (let ((ox (cell-x (viewport-origin viewport)))
        (oy (cell-y (viewport-origin viewport))))
    (loop for row from (1- (viewport-rows viewport)) downto 0
          append (loop for col from 0 below (viewport-cols viewport)
                       collect (cell (+ ox col) (+ oy row))))))

(defun viewport-contains-p (viewport address)
  "True when ADDRESS is inside VIEWPORT."
  (let ((ox (cell-x (viewport-origin viewport)))
        (oy (cell-y (viewport-origin viewport))))
    (and (<= ox (cell-x address) (+ ox (viewport-cols viewport) -1))
         (<= oy (cell-y address) (+ oy (viewport-rows viewport) -1)))))

;;; ------------------------------------------------------------------- grid

(defclass grid (c:container)
  ((cells :initform (make-hash-table :test #'equal) :accessor grid-cells
          :documentation "(X . Y) -> node.  Sparse: absent means empty space.")
   (viewport :initform (make-instance 'viewport) :accessor grid-viewport)
   (col-widths :initform (make-hash-table) :accessor grid-col-widths
               :documentation
               "X -> relative width.  DESIGN D8's spreadsheet geometry: a width
belongs to a *column* and spans every row, so cells can be non-uniform without
the lattice going ragged.  Freely-sized independent cells would destroy the
alignment that makes integer coordinates describe anything real, and with it
the whole spatial-memory argument for a lattice over a continuous plane.")
   (row-heights :initform (make-hash-table) :accessor grid-row-heights
                :documentation "Y -> relative height.  As above, per row.")
   (cell-scales :initform (make-hash-table :test #'equal)
                :accessor grid-cell-scales
                :documentation
                "(X . Y) -> relative size.  DESIGN D8's escape hatch: the
column/row track is still the default, and a cell that wants to be the one
unusual cell scales its own track box about its centre.  Positions stay on the
track grid — nothing shifts, so every address still names the place a person
put it — and only the *size* deviates.  That is the one layout the spreadsheet
cannot express: a cell wider than its same-column neighbours, by construction
rather than by absence of a binding.")
   (names :initform (make-hash-table :test #'equal) :accessor grid-names
          :documentation "NAME -> (X . Y).  DESIGN D1 layer 3: durable names
are the layer humans actually remember."))
  (:documentation
   "An infinite two-dimensional plane of cells, each holding a subtree.

Sparse: only occupied coordinates exist, and coordinates may be negative in
both axes, which is what river's set_position makes possible at all — 'the x
and y coordinates may be positive or negative'.

WHY A LATTICE AND NOT A CONTINUOUS PLANE.  Zooming user interfaces
historically die because users get lost — Pad++, Raskin's Archy, Eagle Mode —
and that failure mode belongs to *continuous* planes.  Integer coordinates
give cells addresses, which buys jump-to-(3,-2), jump-to-name, relative
movement by one cell, and an overlay that is a finite grid rather than a fog.
Discrete geometry is dramatically more memorable than continuous space.

Two dimensions is still more than twice as hard to hold in your head as
niri's one, and that remains the single biggest design risk in the project."))

(defun make-grid (&key (cols 1) (rows 1) cells)
  "A grid showing COLS by ROWS cells, optionally pre-populated.

CELLS is an alist of ((X . Y) . NODE)."
  (let ((grid (make-instance 'grid)))
    (setf (grid-viewport grid) (make-instance 'viewport :cols cols :rows rows))
    (loop for (address . node) in cells
          do (setf (gethash address (grid-cells grid)) node))
    (unless cells
      (setf (gethash (cell 0 0) (grid-cells grid)) (c:make-leaf)))
    grid))

;;; ------------------------------------------- the container protocol, answered
;;;
;;; Six methods.  That is the entire cost of teaching the core about an
;;; infinite plane, and it is the measurement D21 asked for.

(defmethod c:container-addresses ((grid grid))
  "Visible cells first, in reading order, then everything else.

The order matters twice over: LAYOUT-CHILDREN walks it to place cells, and the
conventional layout driver walks the *remainder* to hide what it did not
place.  Putting the visible ones first means the common case touches the front
of the list and stops."
  (let* ((viewport (grid-viewport grid))
         (visible (remove-if-not (lambda (address)
                                   (gethash address (grid-cells grid)))
                                 (viewport-cells viewport)))
         (rest '()))
    (maphash (lambda (address node)
               (declare (ignore node))
               (unless (viewport-contains-p viewport address)
                 (push address rest)))
             (grid-cells grid))
    (append visible (sort rest #'cell-before-p))))

(defun cell-before-p (a b)
  "Reading order, for a stable enumeration of offscreen cells."
  (or (> (cell-y a) (cell-y b))
      (and (= (cell-y a) (cell-y b)) (< (cell-x a) (cell-x b)))))

(defmethod c:child-at ((grid grid) address)
  (and (consp address) (gethash address (grid-cells grid))))

(defmethod (setf c:child-at) (node (grid grid) address)
  (setf (gethash address (grid-cells grid)) node))

(defmethod c:insert-child ((grid grid) address node)
  "Fill a coordinate.  Nothing shifts.

DESIGN D13: opening and closing never shift anything.  A cell is where you put
it, and it stays there — which is D8's own argument reused, because the entire
point of spreadsheet geometry is that every address stays where you left it.
Automatic shifting would move things you were not touching."
  (setf (gethash address (grid-cells grid)) node)
  grid)

(defmethod c:remove-child ((grid grid) address)
  "Empty a coordinate, leaving a hole.  Holes are allowed and are not filled.

D6 made the lattice infinite, so there is no scarcity to compact *for*, and
D3's skip motion already exists to cross gaps cheaply.  COMPACT is an explicit
command for when you want it."
  (let ((node (gethash address (grid-cells grid))))
    (remhash address (grid-cells grid))
    node))

(defmethod c:container-count ((grid grid))
  (hash-table-count (grid-cells grid)))

(defmethod c:default-address ((grid grid))
  "The first *occupied* visible cell, so focus repair lands somewhere real."
  (or (find-if (lambda (address) (gethash address (grid-cells grid)))
               (viewport-cells (grid-viewport grid)))
      (first (c:container-addresses grid))))

(defmethod c:simplify-node ((grid grid))
  "A grid always survives, and always has at least one cell.

Never dissolves into its single child: a plane with one cell on it is still a
plane, and collapsing it would silently throw away the viewport, the column
widths and the names."
  (when (zerop (hash-table-count (grid-cells grid)))
    (setf (gethash (cell 0 0) (grid-cells grid)) (c:make-leaf))
    (setf (viewport-origin (grid-viewport grid)) (cell 0 0)))
  grid)

(defmethod c:container-alternatives-p ((grid grid))
  "Every visible cell is on screen at once, so a grid is not alternatives."
  (declare (ignore grid))
  nil)

(defmethod c:container-splits-along-p ((grid grid) axis)
  "A grid divides space along *both* axes, but it is not a split and a fresh
split must never join it.

Answering NIL is the honest answer to the question TREE-SPLIT-AT is asking —
`should a new pane become a sibling of this container's children?' — because a
cell's neighbours are addressed by coordinate, not by index, and inserting a
sibling into a plane means nothing."
  (declare (ignore grid axis))
  nil)

;;; ---------------------------------------------------- copying and saving
;;;
;;; TWO PROTOCOLS THE GRID USED TO FALL THROUGH, WITH THE SAME CAUSE.  Both
;;; COPY-NODE and SERIALIZE-NODE were TYPECASEs over the three core kinds, so a
;;; GRID matched no clause: a copy came back with no cells at all, and a save
;;; came back as a flat split of whatever windows had been in it.  The whole
;;; plane — the viewport, the track sizes, every name a user had given a cell —
;;; was silently dropped on every restart.
;;;
;;; Both are generics now.  These are the four methods that cost.

(defun copy-viewport (viewport)
  "A fresh viewport with the same origin and the same zoom."
  (make-instance 'viewport
                 :origin (cell (cell-x (viewport-origin viewport))
                               (cell-y (viewport-origin viewport)))
                 :cols (viewport-cols viewport)
                 :rows (viewport-rows viewport)))

(defun copy-table (table)
  "A shallow copy of a hash table, keeping its test."
  (let ((new (make-hash-table :test (hash-table-test table)
                              :size (max 1 (hash-table-count table)))))
    (maphash (lambda (key value) (setf (gethash key new) value)) table)
    new))

(defmethod c:copy-node-slots progn ((new grid) (old grid))
  "The plane's own state.  The cells themselves arrived through the container
method, which walked CONTAINER-ADDRESSES and INSERT-CHILDed a copy of each —
so nothing here has to know that a cell address is a coordinate."
  (setf (grid-viewport new) (copy-viewport (grid-viewport old))
        (grid-col-widths new) (copy-table (grid-col-widths old))
        (grid-row-heights new) (copy-table (grid-row-heights old))
        (grid-cell-scales new) (copy-table (grid-cell-scales old))
        (grid-names new) (copy-table (grid-names old))))

(defun sorted-table (table)
  "TABLE as a sorted alist, so two equal tables compare EQUAL."
  (sort (let ((out '()))
          (maphash (lambda (key value) (push (cons key value) out)) table)
          out)
        #'string< :key (lambda (entry) (princ-to-string (car entry)))))

(defmethod c:node-signature ((grid grid))
  "Identity and every cell — and deliberately *not* the plane's own state.

This used to add the viewport, the track sizes, the cell scales and the names,
so that zoom, pan, resize-column, resize-row, resize-cell and name-cell would
be recorded at all by layout undo.  That worked and it was the wrong altitude:
a tree snapshot carries the plane state along with it, so undo after a tree
change silently reverted whatever the camera had done since the snapshot was
taken, and there was no way to undo just the camera.

The plane state is its own fact now, tracked by the plane ring and walked back
by UNDO-PLANE (lattice/undo.lisp).  Keeping it out of this signature is what
makes the two rings independent: the tree ring sees only the tree, a zoom is
not a tree step, and undoing a tree change does not drag the camera with it.
Two grids with the same cells and the same node identities compare EQUAL here,
which is exactly the comparison the tree ring needs to make."
  (list* :lattice/grid
         (call-next-method)))

(defmethod r:serialize-node ((grid grid))
  "The plane as a readable form, under a namespaced tag.

Cells are written as (X Y FORM) triples rather than as a plist keyed by a cons,
because a cons key printed and read back is fine but reads badly, and the point
of this file is that a human can open it."
  (list :lattice/grid
        :label (c:node-label grid)
        :viewport (let ((viewport (grid-viewport grid)))
                    (list :x (cell-x (viewport-origin viewport))
                          :y (cell-y (viewport-origin viewport))
                          :cols (viewport-cols viewport)
                          :rows (viewport-rows viewport)))
        :columns (let ((out '()))
                   (maphash (lambda (x w) (push (list x w) out))
                            (grid-col-widths grid))
                   (sort out #'< :key #'first))
        :rows (let ((out '()))
                (maphash (lambda (y h) (push (list y h) out))
                         (grid-row-heights grid))
                (sort out #'< :key #'first))
        :cell-scales (let ((out '()))
                       (maphash (lambda (address scale)
                                  (push (list (cell-x address) (cell-y address)
                                              scale)
                                        out))
                                (grid-cell-scales grid))
                       (sort out (lambda (a b)
                                   (or (< (first a) (first b))
                                       (and (= (first a) (first b))
                                            (< (second a) (second b)))))))
        :cells (let ((out '()))
                 (dolist (address (c:container-addresses grid))
                   (let ((child (c:child-at grid address)))
                     (when child
                       (push (list (cell-x address) (cell-y address)
                                   (r:serialize-node child))
                             out))))
                 (nreverse out))))

(defmethod r:deserialize-node ((tag (eql :lattice/grid)) plist index)
  "Rebuild the plane, cell by cell.

Every field is optional and every one degrades: a file written by an older
version, or edited by hand into something not quite right, produces a plane
with fewer facts rather than an error.  SIMPLIFY-NODE at the end guarantees the
result is a valid grid — at least one cell — whatever came out of the file."
  (let ((grid (make-instance 'grid)))
    (setf (c:node-label grid) (getf plist :label))
    (let ((viewport (getf plist :viewport)))
      (when viewport
        (setf (grid-viewport grid)
              (make-instance 'viewport
                             :origin (cell (or (getf viewport :x) 0)
                                           (or (getf viewport :y) 0))
                             :cols (max 1 (or (getf viewport :cols) 1))
                             :rows (max 1 (or (getf viewport :rows) 1))))))
    (loop for entry in (getf plist :columns)
          when (and (consp entry) (realp (first entry)) (realp (second entry)))
            do (setf (col-width grid (first entry)) (second entry)))
    (loop for entry in (getf plist :rows)
          when (and (consp entry) (realp (first entry)) (realp (second entry)))
            do (setf (row-height grid (first entry)) (second entry)))
    (loop for entry in (getf plist :cell-scales)
          when (and (consp entry) (= 3 (length entry))
                    (realp (first entry)) (realp (second entry))
                    (realp (third entry)))
            do (setf (cell-scale grid (cell (first entry) (second entry)))
                     (third entry)))
    (loop for entry in (getf plist :cells)
          when (and (consp entry) (= 3 (length entry)))
            do (destructuring-bind (x y form) entry
                 (setf (gethash (cell x y) (grid-cells grid))
                       (r:read-node form index))))
    ;; Names are derived rather than stored: a cell's name is its node's label,
    ;; and keeping a second copy in the grid's own table is how the two come to
    ;; disagree after a rename.
    (maphash (lambda (address node)
               (let ((label (c:node-label node)))
                 (when label (setf (gethash label (grid-names grid)) address))))
             (grid-cells grid))
    (c:simplify-node grid)))

;;; ------------------------------------------------------------- cell access

(defun ensure-cell (grid address)
  "The node at ADDRESS, creating an empty pane there if the cell is vacant.

This is what makes the plane infinite in practice rather than only in
principle: moving into empty space is an ordinary navigation, and D3 says
plain motion moves exactly one cell 'whether or not that cell is occupied'.
The cell has to exist for focus to rest on it, so arriving creates it."
  (or (gethash address (grid-cells grid))
      (setf (gethash address (grid-cells grid)) (c:make-leaf))))

(defun occupied-cells (grid)
  "Every cell address that holds at least one window, in reading order."
  (let ((out '()))
    (maphash (lambda (address node)
               (unless (c:node-empty-p node) (push address out)))
             (grid-cells grid))
    (sort out #'cell-before-p)))

(defun tidy-grid (grid &key keep)
  "Drop every cell that holds nothing, except those in KEEP.

The lattice accumulates empty cells simply by being walked across, and while a
hole is a legitimate object (D13), a hole nobody made on purpose is litter.
This is the broom, and it is explicit rather than automatic for the same
reason nothing else here is automatic."
  (let ((doomed '()))
    (maphash (lambda (address node)
               (when (and (c:node-empty-p node)
                          (not (member address keep :test #'cell-equal)))
                 (push address doomed)))
             (grid-cells grid))
    (dolist (address doomed)
      (unset-cell-scale grid address)
      (remhash address (grid-cells grid)))
    (c:simplify-node grid)
    (length doomed)))

(defun cell-named-p (grid address)
  "True when some name in GRID points at ADDRESS."
  (let ((found nil))
    (maphash (lambda (name at)
               (declare (ignore name))
               (when (cell-equal at address) (setf found t)))
             (grid-names grid))
    found))

(defun forget-empty-cell (grid address)
  "Drop ADDRESS from GRID when arriving there is all that ever happened to it.

ENSURE-CELL creates a cell because focus has to rest on something, so crossing
the plane leaves one behind per step — and nothing ever took them away, because
TIDY-GRID is deliberately manual.  The count only rises: CONTAINER-ADDRESSES
sorts it on every relayout, LAYOUT-NODE walks it on every relayout, and
SERIALIZE-NODE writes it to the state file forever.  The core assumes a
container is finite *and small*; this is the one place the lattice fights that,
and it used to pay at a cost proportional to how long the session had been
running.

Refuses four things, and each of them is somebody's cell rather than litter: a
cell with anything in it, a cell somebody named, a cell somebody sized, and the
last cell standing — a plane with no cells at all is a shape nothing downstream
is written for."
  (let ((node (gethash address (grid-cells grid))))
    (when (and node
               (typep node 'c:leaf)
               (c:leaf-empty-p node)
               (> (hash-table-count (grid-cells grid)) 1)
               (not (cell-named-p grid address))
               (not (nth-value 1 (gethash address (grid-cell-scales grid)))))
      (unset-cell-scale grid address)
      (remhash address (grid-cells grid))
      t)))

;;; ------------------------------------------------------------- track sizes

(defun col-width (grid x)
  "The relative width of column X.  1 unless it has been resized."
  (gethash x (grid-col-widths grid) 1))

(defun (setf col-width) (value grid x)
  (setf (gethash x (grid-col-widths grid)) (max 1/100 value)))

(defun row-height (grid y)
  "The relative height of row Y.  1 unless it has been resized."
  (gethash y (grid-row-heights grid) 1))

(defun (setf row-height) (value grid y)
  (setf (gethash y (grid-row-heights grid)) (max 1/100 value)))

(defun cell-scale (grid address)
  "The scale of the cell at ADDRESS relative to its track box.  1 unless resized.

The escape hatch from the spreadsheet, deliberately smaller than the track
accessors: a width still belongs to a column and spans every row, and a height
to a row and every column — this one cell is allowed to disagree, about both at
once, and nothing else is."
  (gethash address (grid-cell-scales grid) 1))

(defun (setf cell-scale) (value grid address)
  (setf (gethash address (grid-cell-scales grid)) (max 1/100 value)))

(defun unset-cell-scale (grid address)
  "Return ADDRESS's cell to its track size, dropping the entry entirely.

Dropping rather than writing 1 keeps the plane's own state tables as small as
the facts that are not defaults, which is the same reason EQUALIZE-CELLS clears
rather than writes."
  (remhash address (grid-cell-scales grid)))

(defun uniform-p (grid)
  "True when no column, row or cell has been resized away from 1.

Worth knowing, because it decides whether panning can resize anything.  See
CELL-RECTS."
  (and (zerop (hash-table-count (grid-col-widths grid)))
       (zerop (hash-table-count (grid-row-heights grid)))
       (zerop (hash-table-count (grid-cell-scales grid)))))

;;; ------------------------------------------------------ the plane state
;;;
;;; What a GRID is as a *view* rather than a tree: where the camera is, what
;;; has been resized and what has been named.  CAPTURE-PLANES and APPLY-PLANES
;;; make that set of facts a value that can be put on a ring and walked back by
;;; UNDO-PLANE — and because they are the whole of the plane, they are also what
;;; keeps a tree undo from dragging the camera along with it.

(defun plane-state (grid)
  "GRID's plane facts as a fresh plist: the camera, every size and every name.

A plist rather than the live tables, so a snapshot cannot be invalidated by a
later mutation and two equal planes compare EQUAL.  SORTED-TABLE is reused so
that two states built in a different hash iteration order still compare EQUAL,
which is the same guarantee the old signature made and the one the plane ring
needs."
  (list :origin (cell (cell-x (viewport-origin (grid-viewport grid)))
                      (cell-y (viewport-origin (grid-viewport grid))))
        :cols (viewport-cols (grid-viewport grid))
        :rows (viewport-rows (grid-viewport grid))
        :columns (sorted-table (grid-col-widths grid))
        :row-heights (sorted-table (grid-row-heights grid))
        :cell-scales (sorted-table (grid-cell-scales grid))
        :names (sorted-table (grid-names grid))))

(defun apply-plane-state (grid state)
  "Make GRID's plane facts those in STATE, which PLANE-STATE produced.

The tables are cleared and refilled rather than replaced, so a live handle to
GRID-COL-WIDTHS or a sibling is not invalidated by a restore — the same reason
COPY-NODE-SLOTS copies rather than swaps."
  (let ((origin (getf state :origin)))
    (setf (viewport-origin (grid-viewport grid))
          (cell (cell-x origin) (cell-y origin))
          (viewport-cols (grid-viewport grid)) (max 1 (getf state :cols))
          (viewport-rows (grid-viewport grid)) (max 1 (getf state :rows))))
  (flet ((refill (table entries)
           (clrhash table)
           (loop for (key . value) in entries
                 do (setf (gethash key table) value))))
    (refill (grid-col-widths grid) (getf state :columns))
    (refill (grid-row-heights grid) (getf state :row-heights))
    (refill (grid-cell-scales grid) (getf state :cell-scales))
    (refill (grid-names grid) (getf state :names)))
  grid)

(defun collect-planes (root)
  "Every plane reachable from ROOT as (PATH . PLANE-STATE), in tree order.

PATH is the address list from ROOT to the grid, which is what lets
APPLY-PLANES put a captured state back on the *same* grid after a restore has
replaced the tree — the grid is a copy, its path in the tree is not."
  (let ((out '()))
    (labels ((walk (node path)
               (when (typep node 'grid)
                 (push (cons path (plane-state node)) out))
               (when (c:container-p node)
                 (dolist (address (c:container-addresses node))
                   (walk (c:child-at node address)
                         (append path (list address)))))))
      (walk root '()))
    (nreverse out)))

(defun apply-planes (root captures)
  "Put each captured plane state back on the grid at its path in ROOT.

Silent about a path that no longer names a grid: a restore that reshaped the
tree out from under a plane leaves that plane's camera where the restore put
it rather than guessing.  Degrades, like every other reader of the state
file."
  (dolist (capture captures)
    (let ((node (c:resolve-path root (car capture))))
      (when (typep node 'grid)
        (apply-plane-state node (cdr capture)))))
  root)
