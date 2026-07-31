;;;; lattice/commands.lisp --- Zoom, pan, jump, and turning the plane on.

(in-package #:lattice)

(defun current-grid ()
  "The grid the cursor is inside, or NIL.

Searches the cursor's ancestor chain rather than assuming the plane sits at
any particular depth, so a grid nested inside a split inside another grid
works — which is not a feature anybody asked for, but is free, and its being
free is the sign the container abstraction is right."
  (let* ((root (c:world-root r:*world*))
         (path (c:world-cursor r:*world*))
         (chain (c:resolve-chain root path)))
    (loop for node in (reverse (or chain (list root)))
          when (typep node 'grid) return node)))

(defun current-cell ()
  "The address of the cell the cursor is in, or NIL."
  (let* ((root (c:world-root r:*world*))
         (path (c:world-cursor r:*world*))
         (chain (c:resolve-chain root path)))
    (loop for node in chain
          for address in path
          when (typep node 'grid) return address)))

(defun grid-path ()
  "The path of the enclosing grid, or NIL."
  (let* ((root (c:world-root r:*world*))
         (path (c:world-cursor r:*world*))
         (chain (c:resolve-chain root path)))
    (loop for node in chain
          for i from 0
          when (typep node 'grid) return (subseq path 0 i))))

(defun cell-path (address &optional (grid-path (grid-path)))
  "The world path of cell ADDRESS.

Always use this rather than (list address).  A cell address is *relative to its
grid*, and the grid is not the root — it sits inside the workspace stack — so a
bare (list address) resolves against the root and names a workspace index that
does not exist.  That is the single easiest mistake to make when writing a
window rule, and it fails with a confusing message about a stack."
  (append grid-path (list address)))

(defmacro with-grid ((grid) &body body)
  "Run BODY with GRID bound to the enclosing grid, or do nothing."
  `(let ((,grid (current-grid)))
     (when ,grid ,@body (r:mark-dirty))))

;;; ==================================================================
;;; ZOOM
;;; ==================================================================

(r:defcommand zoom-out (&optional (steps 1))
  "Show more of the plane: 1 cell, then 2, then 4, 6, 8, and on.

Pure view control.  Stepping out and back in returns to exactly the previous
state, because zoom changes nothing about the layout — only how much of it you
are looking at.  That is DESIGN D7's central ruling and the reason zoom feels
like a camera rather than a command.

Note what it costs, because it is not free and cannot be made free: there is
no scaling primitive anywhere in the river protocol, so every zoom step gives
every visible window new dimensions.  Terminals rewrap.  Browsers reflow.  This
is why zoom is not animated — it would relayout every window every frame."
  (with-grid (grid)
    (let ((focus (current-cell)))
      (set-zoom grid (+ (zoom-index (grid-viewport grid)) steps) :focus focus))))

(r:defcommand zoom-in (&optional (steps 1))
  "Show less of the plane, down to a single cell filling the screen."
  (zoom-out (- steps)))

(r:defcommand zoom-reset ()
  "Back to one cell, filling the screen."
  (with-grid (grid) (set-zoom grid 0 :focus (current-cell))))

(r:defcommand zoom-to (cols rows)
  "Show exactly COLS by ROWS cells, whether or not that is on the ladder."
  (with-grid (grid)
    (let ((viewport (grid-viewport grid))
          (focus (current-cell)))
      (setf (viewport-cols viewport) (max 1 cols)
            (viewport-rows viewport) (max 1 rows))
      (when focus
        (setf (viewport-origin viewport) (zoom-origin focus cols rows))
        (ensure-visible grid focus)))))

(r:defcommand zoom-fit ()
  "Zoom out just far enough to show everything that has a window in it."
  (with-grid (grid)
    (let ((cells (occupied-cells grid)))
      (when cells
        (let ((min-x (reduce #'min cells :key #'cell-x))
              (max-x (reduce #'max cells :key #'cell-x))
              (min-y (reduce #'min cells :key #'cell-y))
              (max-y (reduce #'max cells :key #'cell-y))
              (viewport (grid-viewport grid)))
          (setf (viewport-cols viewport) (1+ (- max-x min-x))
                (viewport-rows viewport) (1+ (- max-y min-y))
                (viewport-origin viewport) (cell min-x min-y)))))))

;;; ==================================================================
;;; PAN
;;; ==================================================================

(r:defcommand pan (direction &optional (cells 1))
  "Move the viewport DIRECTION without moving the cursor.

'Where am I standing?', as against zoom's 'how much can I see?'.  The same
amount stays visible; a different part of the plane is under it.

Panning is cheap — it is set_position only, no resize — which is why panning
could be animated later and zooming could not."
  (with-grid (grid)
    (let* ((viewport (grid-viewport grid))
           (dx (if (c:direction-horizontal-p direction)
                   (* cells (c:direction-sign direction)) 0))
           (dy (if (c:direction-vertical-p direction)
                   (* cells (- (c:direction-sign direction))) 0)))
      (setf (viewport-origin viewport)
            (cell (+ (cell-x (viewport-origin viewport)) dx)
                  (+ (cell-y (viewport-origin viewport)) dy))))))

(r:defcommand pan-to-cursor ()
  "Bring the viewport back to wherever the cursor is.

The escape hatch for having panned away and lost yourself, which is the
failure mode a zooming interface has to have an answer for."
  (with-grid (grid)
    (let ((focus (current-cell)))
      (when focus (ensure-visible grid focus)))))

;;; ==================================================================
;;; JUMPING — DESIGN D1's four layers of addressing
;;; ==================================================================

(defun goto-cell (address &key (create t))
  "Put the cursor in the cell at ADDRESS."
  (let* ((grid (current-grid))
         (base (grid-path)))
    (when grid
      (when create (ensure-cell grid address))
      (when (c:child-at grid address)
        (ensure-visible grid address)
        (p:jump-cursor (p:current-policy) r:*world*
                       (append base (list address)))
        (r:mark-dirty)
        address))))

(r:defcommand goto (x y)
  "Jump to the cell at coordinate X, Y.  Negative coordinates are ordinary.

DESIGN D1 layer 4.  Coordinates are displayed everywhere and typeable as a
jump target, but they are the escape hatch rather than the interface —
relative motion is the overwhelming majority of real navigation, and durable
names are the layer humans actually remember."
  (goto-cell (cell x y)))

(r:defcommand name-cell (name)
  "Give the current cell a durable name, so it can be jumped to by it.

DESIGN D1 layer 3.  `code', `mail', `scratch'.  Names, not numbers: numbers
are for the machine and for the escape hatch, and a name is what you actually
remember three days later."
  (with-grid (grid)
    (let ((address (current-cell)))
      (when address
        (setf (gethash name (grid-names grid)) address)
        (let ((node (c:child-at grid address)))
          (when node (setf (c:node-label node) name)))
        name))))

(r:defcommand goto-name (name)
  "Jump to the cell named NAME."
  (let* ((grid (current-grid))
         (address (and grid (gethash name (grid-names grid)))))
    (when address (goto-cell address))))

(r:defcommand goto-next-occupied (direction)
  "Skip to the next cell in DIRECTION that actually holds something.

The spreadsheet Ctrl+Arrow idiom.  DESIGN D3 makes this the *modified* motion
and plain single-cell motion the default, because zoom-out is the primary
orientation tool and keeping default motion distance-predictable preserves
muscle memory.  This is one modifier away for when you are crossing a gap."
  (with-grid (grid)
    (let* ((from (current-cell))
           (dx (if (c:direction-horizontal-p direction)
                   (c:direction-sign direction) 0))
           (dy (if (c:direction-vertical-p direction)
                   (- (c:direction-sign direction)) 0)))
      (when from
        (loop with here = from
              repeat 1000
              do (setf here (cell (+ (cell-x here) dx) (+ (cell-y here) dy)))
                 (let ((node (c:child-at grid here)))
                   (when (and node (not (c:node-empty-p node)))
                     (goto-cell here)
                     (return here))))))))

;;; ==================================================================
;;; MOVING THINGS BETWEEN CELLS
;;; ==================================================================

(r:defcommand move-to-cell (x y &key (follow t))
  "Move the focused pane to the cell at X, Y.

If that cell is empty the pane becomes its whole contents; if it is occupied
the pane joins as a split, which is the shipped answer to move-onto-occupied
and the only one that never destroys structure."
  (let* ((grid (current-grid))
         (base (grid-path))
         (from (c:world-cursor r:*world*))
         (target (cell x y)))
    (when (and grid (> (length from) (length base)))
      (ensure-cell grid target)
      (let ((to (append base (list target))))
        (multiple-value-bind (root landed)
            (c:tree-move (c:world-root r:*world*) from to :join :split)
          (setf (c:world-root r:*world*) root)
          (if follow
              (progn (ensure-visible grid target)
                     (p:jump-cursor (p:current-policy) r:*world* landed))
              (p:repair-cursor (p:current-policy) r:*world* from))
          (r:mark-dirty)
          landed)))))

(r:defcommand move-cell (direction &key (follow t))
  "Move the focused pane one cell DIRECTION."
  (let ((from (current-cell)))
    (when from
      (let ((dx (if (c:direction-horizontal-p direction)
                    (c:direction-sign direction) 0))
            (dy (if (c:direction-vertical-p direction)
                    (- (c:direction-sign direction)) 0)))
        (move-to-cell (+ (cell-x from) dx) (+ (cell-y from) dy) :follow follow)))))

;;; ==================================================================
;;; SPREADSHEET GEOMETRY — D8 and D12
;;; ==================================================================

(defvar *warned-about-pan-resize* nil)

(r:defcommand resize-column (&optional (amount 1/10))
  "Widen the current column by AMOUNT, across every row.

DESIGN D8: a width belongs to a *column* and spans every row, exactly like a
spreadsheet.  That is what lets cells be non-uniform without the lattice going
ragged — freely-sized independent cells would destroy the alignment that makes
integer coordinates describe anything real."
  (with-grid (grid)
    (let ((address (current-cell)))
      (when address
        (warn-about-pan-resize grid)
        (setf (col-width grid (cell-x address))
              (+ (col-width grid (cell-x address)) amount))))))

(r:defcommand resize-row (&optional (amount 1/10))
  "Make the current row taller by AMOUNT, across every column."
  (with-grid (grid)
    (let ((address (current-cell)))
      (when address
        (warn-about-pan-resize grid)
        (setf (row-height grid (cell-y address))
              (+ (row-height grid (cell-y address)) amount))))))

(defun warn-about-pan-resize (grid)
  "Say, once, what the first column resize costs under :FIT zoom.

PLAN.org's Delta 3 is right that :FIT makes a column's rendered width depend
on which columns are beside it, so panning across a *non-uniform* lattice
resizes every window on screen.  That cost does not exist until this moment —
it arrives with the first resize — so this is the moment to mention it, rather
than defaulting to :FIXED for everybody to avoid a cost most people never
incur."
  (when (and (eq *zoom-mode* :fit) (uniform-p grid)
             (not *warned-about-pan-resize*))
    (setf *warned-about-pan-resize* t)
    (r:logmsg :info "~
This is the first column or row you have resized.  Under :FIT zoom, a~%~
column's width now depends on which other columns are beside it, so panning~%~
will resize the windows on screen.  If that turns out to be intolerable:~%~
  (setf lattice:*zoom-mode* :fixed)")))

(r:defcommand equalize-cells ()
  "Give every column and row an equal share again."
  (with-grid (grid)
    (clrhash (grid-col-widths grid))
    (clrhash (grid-row-heights grid))))

;;; ==================================================================
;;; HOUSEKEEPING
;;; ==================================================================

(r:defcommand tidy ()
  "Drop every empty cell except the one you are standing in.

Holes are legitimate and are never removed automatically (D13), but a hole you
made by walking across it is litter rather than intent.  This is the broom."
  (with-grid (grid)
    (let ((dropped (tidy-grid grid :keep (list (current-cell)))))
      (r:logmsg :info "dropped ~d empty cell~:p" dropped)
      (p:repair-cursor (p:current-policy) r:*world*))))

(r:defcommand lattice-status ()
  "Print where you are on the plane, and what is on it."
  (let ((grid (current-grid)))
    (if (null grid)
        (format t "~&not inside a lattice~%")
        (let ((viewport (grid-viewport grid)))
          (format t "~&cell ~a   viewport ~dx~d at ~a   ~d cell~:p, ~d occupied~%"
                  (cell-string (or (current-cell) (cell 0 0)))
                  (viewport-cols viewport) (viewport-rows viewport)
                  (cell-string (viewport-origin viewport))
                  (c:container-count grid) (length (occupied-cells grid)))
          (let ((names '()))
            (maphash (lambda (name address) (push (cons name address) names))
                     (grid-names grid))
            (when names
              (format t "names: ~{~a=~a~^  ~}~%"
                      (loop for (name . address) in (sort names #'string< :key #'car)
                            append (list name (cell-string address))))))))))

;;; ==================================================================
;;; TURNING IT ON
;;; ==================================================================

(defun enable (&key (cols 1) (rows 1) (keys t))
  "Switch the running window manager over to the lattice.  Live.

    (asdf:load-system \"lattice\")
    (lattice:enable)

Wraps each existing workspace in a grid, installs the lattice policy, binds
the lattice keys, and relays out.  Every window you have open stays open and
stays where it was, because nothing about them changed — a workspace that was
a split tree becomes cell (0,0) of a plane whose other cells are empty.

This is the whole extensibility claim, executed: a new container kind, a new
layout model, a new set of commands, and a live switch, with no edit to
anything under src/ and no restart."
  (let* ((world r:*world*)
         (root (c:world-root world)))
    (setf p:*policy* (make-instance 'lattice-policy))
    ;; Wrap each workspace that is not already a plane.
    (if (typep root 'c:stack)
        (loop for index from 0 below (c:container-count root)
              for child = (c:child-at root index)
              unless (typep child 'grid)
                do (setf (c:child-at root index)
                         (make-grid :cols cols :rows rows
                                    :cells (list (cons (cell 0 0) child)))))
        (unless (typep root 'grid)
          (setf (c:world-root world)
                (make-grid :cols cols :rows rows
                           :cells (list (cons (cell 0 0) root))))))
    (setf (c:world-cursor world)
          (c:repair-path (c:world-root world) (c:world-cursor world)))
    (when keys (install-lattice-keys))
    (tag-cell-parity)
    (r:relayout :force t)
    (r:logmsg :info "lattice enabled: ~dx~d viewport" cols rows)
    t))

(defun tag-cell-parity ()
  "Mark each cell's subtree with its coordinate parity, for the border tint.

Done as a PROP rather than a slot, because it is presentation state that the
node has no business carrying permanently — which is precisely what PROPS is
for, and precisely the case DESIGN D20 predicted an extension would need."
  (let ((grid (current-grid)))
    (when grid
      (maphash (lambda (address node)
                 (setf (c:prop node :lattice/parity)
                       (evenp (+ (cell-x address) (cell-y address)))))
               (grid-cells grid)))))

(defun install-lattice-keys (&optional (keymap r:*keymap*))
  "Bind the lattice commands.  Generated, like the core keymap.

Everything the conventional layer bound still works: Super+arrows still move
between panes, and now keep going into the cell next door when they run out of
panes, because motion never knew the difference."
  (flet ((mod+ (&rest extra) (apply #'r::modifier-string extra)))
    (let ((mod (mod+)) (shift (mod+ :shift)) (ctrl (mod+ :ctrl)))
      ;; Zoom.  minus and equal because that is what every other zoom is.
      (r:define-key keymap (format nil "~aminus" mod) '("zoom-out"))
      (r:define-key keymap (format nil "~aequal" mod) '("zoom-in"))
      (r:define-key keymap (format nil "~a0" ctrl) '("zoom-reset"))
      (r:define-key keymap (format nil "~aequal" shift) '("zoom-fit"))
      ;; Pan, and the cell-wise motions, on the same direction table the core
      ;; keymap uses — so nothing here has to know which keys mean which way.
      (loop for (direction . keys) in r::+direction-keys+
            do (dolist (key keys)
                 (r:define-key keymap (format nil "~a~a" ctrl key)
                   (list "pan" direction))
                 (r:define-key keymap (format nil "~a~a" (mod+ :ctrl :shift) key)
                   (list "goto-next-occupied" direction))
                 (r:define-key keymap (format nil "~a~a" (mod+ :alt :shift) key)
                   (list "move-cell" direction))))
      (r:define-key keymap (format nil "~ag" mod) '("pan-to-cursor"))
      (r:define-key keymap (format nil "~at" shift) '("tidy"))
      ;; NOT Super+/.  That is HELP -- the one key that finds every other key,
      ;; the key the status-line hint points at, and the key a lost user is
      ;; told to press.  Taking it meant enabling the lattice silently removed
      ;; the way out of not knowing anything, and the hint became a lie.
      ;; lattice-status goes in the question submap beside the other "tell me
      ;; about this" commands, where it belongs.
      (let ((help-map (r:lookup-key keymap (r:kbd (format nil "~aquestion" shift)))))
        (when (typep help-map 'r:keymap)
          (r:define-key help-map "l" '("lattice-status"))))
      (r:rebind-keys)
      keymap)))

(defun disable ()
  "Go back to the conventional policy without unwrapping the grids.

The plane stays in the tree; it simply lays out as a single cell again because
the conventional policy has no idea what to do with a grid — which is worth
seeing once, because it is what the container protocol guarantees: an unknown
container is inert, not broken."
  (setf p:*policy* (make-instance 'p:conventional-policy))
  (r:relayout :force t)
  t)

;;; ==================================================================
;;; MAKING THE VOCABULARY REACHABLE
;;; ==================================================================

(defun install-vocabulary (&optional (package '#:latticewm/user))
  "Make the lattice's names visible in PACKAGE, which defaults to the one
configuration files and the REPL are read in.

Without this, loading the system gives you a working lattice whose commands
you cannot type: DEFCOMMAND interns its symbols in the package the file was
compiled in, which is LATTICE, and LATTICEWM/USER has no reason to know about
a package that did not exist when it was defined.  So (zoom-out) at a REPL is
an undefined function, and the whole extension appears to be broken while
working perfectly.

USE-PACKAGE at load time rather than a hand-maintained re-export list, so that
a command added tomorrow is reachable tomorrow.  Conflicts are reported and
skipped rather than signalling, because a configuration file that has already
defined its own ZOOM-OUT should keep it — it is theirs, and refusing to load
over it would be the wrong way round."
  (handler-bind ((package-error
                   (lambda (condition)
                     (r:logmsg :warn "lattice: ~a (skipping that name)" condition)
                     (let ((restart (or (find-restart 'cl:continue condition)
                                        (find-restart 'sb-impl::take-new condition))))
                       (when restart (invoke-restart restart))))))
    (ignore-errors (use-package '#:lattice package)))
  package)

;; Do it on load, so that `asdf:load-system "lattice"' is the whole of the
;; installation and `lattice:enable' is genuinely optional.
(eval-when (:load-toplevel :execute)
  (install-vocabulary))
