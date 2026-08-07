;;;; lattice/commands.lisp --- Zoom, pan, jump, and turning the plane on.

(in-package #:lattice)

(defun current-grid ()
  "The grid the cursor is inside, or NIL.

Searches the cursor's ancestor chain rather than assuming the plane sits at
any particular depth, so a grid nested inside a split inside another grid
works — which is not a feature anybody asked for, but is free, and its being
free is the sign the container abstraction is right.

CURSOR-GRID with the world supplied.  Two names for one walk because the
commands here always mean the live world and the policy methods are handed one,
and a policy method reaching for R:*WORLD* is how a method stops working in a
test."
  (cursor-grid r:*world*))

(defun current-cell ()
  "The address of the cell the cursor is in, or NIL."
  (cursor-cell r:*world*))

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

(defvar *policy-before-enable* nil
  "What the policy was before ENABLE composed the lattice over it, as
(CLASS . NAME), or NIL when the lattice is not enabled.

DISABLE puts that back.  It used to construct a fresh CONVENTIONAL-POLICY,
which is only the right answer for a session that never had anything else —
and the sessions that had something else are exactly the ones the extension
story is about.")

(defun lattice-policy-class (base)
  "The class of BASE with LATTICE-MIXIN in front of it.

Three cases, and only the third is interesting.  A policy that is already a
LATTICE-MIXIN is returned unchanged, because composing the mixin over itself is
not a class — the two occurrences of LATTICE-MIXIN in the precedence list
cannot both be more specific than the other, and CLOS says so with an error
that names neither this function nor the second call to ENABLE that caused it.
A plain CONVENTIONAL-POLICY gets LATTICE-POLICY, which is that combination
written down.  Anything else is composed here, and the class is interned under
a derived name so that enabling twice in one session finds the same class
rather than minting a new one and invalidating every generic function's cache.

This is the composable half of the class idiom, which CLOS has always given
away and this extension spent its whole life not taking: the mixin goes in
front, CALL-NEXT-METHOD reaches the policy that was already installed, and
whatever slots it was carrying survive because DISABLE changes the class of the
object rather than replacing it."
  (cond ((subtypep base 'lattice-mixin) base)
        ((eq base (find-class 'p:conventional-policy)) (find-class 'lattice-policy))
        (t (closer-mop:ensure-class
            (intern (format nil "LATTICE-OVER-~a"
                            (or (class-name base) "ANONYMOUS-POLICY"))
                    '#:lattice)
            :direct-superclasses (list (find-class 'lattice-mixin) base)))))

(defun enable (&key (cols 1) (rows 1) (keys t))
  "Switch the running window manager over to the lattice.  Live.

    (load-extension \"lattice\")
    (lattice:enable)

Wraps each existing workspace in a grid, installs the lattice policy, binds
the lattice keys, and relays out.  Every window you have open stays open and
stays where it was, because nothing about them changed — a workspace that was
a split tree becomes cell (0,0) of a plane whose other cells are empty.

AND WHATEVER POLICY WAS ALREADY INSTALLED STAYS INSTALLED, underneath.  This
used to be `(setf p:*policy* (make-instance 'lattice-policy))', so loading
examples/03-master-stack.lisp, running (master-stack) and then calling this
discarded the master-stack policy without a word — the second party's first
act, answered with silence and load order.  LATTICE-MIXIN is now composed over
the class of the policy in force and the object is CHANGE-CLASSed rather than
replaced, so any slots that policy was carrying survive and CALL-NEXT-METHOD in
every lattice method reaches its answers.  Calling this twice is a no-op on the
policy rather than an error.

AND EVERY WORKSPACE MADE AFTERWARDS IS A PLANE TOO, which is not the same
statement and used not to be true.  This wraps what exists; P:MAKE-WORKSPACE,
answered by LATTICE-POLICY, is what makes a *new* workspace a plane — so
'infinite workspaces of lattices one behind another' is the permanent shape of
the thing rather than an arrangement that stops at whatever was open when this
was called.  See *NEW-WORKSPACE-ZOOM*, *NEW-WORKSPACE-ORIGIN* and
*WORKSPACE-ENTRY* for what a new plane is born looking at, and where you land
on it.

This is the whole extensibility claim, executed: a new container kind, a new
layout model, a new set of commands, and a live switch, with no edit to
anything under src/ and no restart.

*Safe to call before there is a world*, which is not a hypothetical: it is what
`latticewm --check-config' does, and what a REPL does when somebody evaluates
their configuration file by hand before starting anything.  The policy and the
keys are installed either way; the tree surgery waits for a tree.  Without this
the shipped starter configuration -- which offers exactly these two lines --
reported an error under the very tool written to check configurations."
  (let* ((policy (p:current-policy))
         (base (class-of policy)))
    (if (typep policy 'lattice-mixin)
        (r:logmsg :debug "lattice: already composed over ~a; policy left alone"
                  (p:policy-name policy))
        (let ((was (p:policy-name policy)))
          (setf *policy-before-enable* (cons base was))
          (change-class policy (lattice-policy-class base))
          ;; CHANGE-CLASS keeps the value of every slot both classes have, and
          ;; %NAME is one of them — so the initform on LATTICE-MIXIN never runs
          ;; and the composed policy would have gone on calling itself
          ;; "conventional".  Naming it here also lets it say what it is
          ;; composed over, which is the one thing `latticewm --status' could
          ;; not have told you before.
          (setf (p:policy-name policy)
                (if (eq base (find-class 'p:conventional-policy))
                    "lattice"
                    (format nil "lattice over ~a" was)))
          (r:logmsg :info "lattice: policy is now ~a" (p:policy-name policy))))
    (setf p:*policy* policy))
  ;; Remembered before anything else, so that a workspace created hours later
  ;; is born the same shape as the ones created here.  Without it, ENABLE's
  ;; COLS and ROWS described a moment rather than a session, and workspace 7
  ;; opened at a zoom nobody had asked for.
  (setf *default-viewport* (cons (max 1 cols) (max 1 rows)))
  (when keys (install-lattice-keys))
  ;; And again after a saved layout is restored, because that replaces the tree
  ;; wholesale — including the planes this call just made.  Enabling the
  ;; lattice from a configuration file and then having the restore quietly
  ;; unwrap it is exactly the failure :LAYOUT-RESTORED was declared for.
  (r:add-hook :layout-restored 'rewrap-after-restore)
  (let ((world r:*world*))
    (unless world
      (r:logmsg :info "lattice policy installed; no world yet, so nothing to wrap")
      (return-from enable :policy-only))
    (enable-in world :cols cols :rows rows)))

(defun rewrap-after-restore ()
  "Put the planes back after a saved layout replaced the tree.

Idempotent: ENABLE-IN wraps only workspaces that are not already planes, so a
state file written by a version that *does* save the grid — every version since
SERIALIZE-NODE became a protocol — comes back already correct and this does
nothing at all."
  (let ((world r:*world*))
    (when (and world (typep p:*policy* 'lattice-mixin))
      (enable-in world))))

(defun enable-in (world &key (cols 1) (rows 1))
  "Wrap every workspace of WORLD in a plane, and relayout."
  (let ((root (c:world-root world)))
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
    ;; Every path in the world just grew a cell on the front of it, so the
    ;; cursor moves even though nothing the user can see did.  Through
    ;; REPAIR-CURSOR rather than into the slot: it is REPAIR-PATH plus
    ;; ON-FOCUS-CHANGE plus the :FOCUS-CHANGED hook, and a status bar that
    ;; shows the address needs to hear about this one most of all.
    (p:repair-cursor (p:current-policy) world)
    (r:relayout :force t)
    (r:logmsg :info "lattice enabled: ~dx~d viewport" cols rows)
    t))

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
  "Take the lattice back off, restoring whatever it was composed over.

Not `go back to the conventional policy'.  That is what this did — a fresh
CONVENTIONAL-POLICY, so a session that had loaded somebody else's policy first
lost it here as surely as it lost it in ENABLE, and any slots it was carrying
went with it.  The class the policy had is remembered in
*POLICY-BEFORE-ENABLE* and put back, on the same object, so a policy carrying
state comes back carrying it.

The plane stays in the tree; it simply lays out as a single cell again because
the policy underneath has no idea what to do with a grid — which is worth
seeing once, because it is what the container protocol guarantees: an unknown
container is inert, not broken."
  (let ((policy p:*policy*))
    (cond ((not (typep policy 'lattice-mixin))
           (r:logmsg :info "lattice: not enabled, so nothing to take off")
           nil)
          (t (destructuring-bind (class . name)
                 (or *policy-before-enable*
                     (cons (find-class 'p:conventional-policy) "conventional"))
               (change-class policy class)
               (setf (p:policy-name policy) name))
             (setf *policy-before-enable* nil)
             ;; The hook goes too.  It is inert once the policy is no longer a
             ;; LATTICE-MIXIN, but a hook that survives the feature it belongs
             ;; to is a thing to have to reason about later, and REMOVE-HOOK
             ;; exists precisely so nobody has to.
             (r:remove-hook :layout-restored 'rewrap-after-restore)
             (r:relayout :force t)
             t))))

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
a command added tomorrow is reachable tomorrow.  Conflicts are resolved in
favour of the name that was already there, because a configuration file that
has already defined its own ZOOM-OUT should keep it — it is theirs, and
refusing to load over it would be the wrong way round.

THE CONFLICTS ARE RESOLVED BEFORE THE USE-PACKAGE RATHER THAN DURING IT.  This
used to be `(ignore-errors (use-package ...))' under a HANDLER-BIND hunting for
CL:CONTINUE or SB-IMPL::TAKE-NEW, and if neither restart was found — which is
not a hypothetical, it is what a symbol conflict between two *used* packages
gives you — the error escaped to the IGNORE-ERRORS, the whole USE-PACKAGE was
abandoned partway, and how many names got imported first was unspecified.  The
extension was then half installed behind one :WARN line, which is precisely the
failure the paragraph above exists to prevent.  Every name that is already
spoken for is SHADOWING-IMPORTed first — which pins the user's meaning whether
theirs is present or inherited — so the USE-PACKAGE that follows has nothing
left to signal about, and a collision costs one name and one log line."
  (let ((target (find-package package))
        (source (find-package '#:lattice)))
    (unless (and target source)
      (r:logmsg :warn "lattice: no package ~a; vocabulary not installed" package)
      (return-from install-vocabulary nil))
    (let ((taken '()))
      (do-external-symbols (symbol source)
        (multiple-value-bind (theirs status)
            (find-symbol (symbol-name symbol) target)
          (when (and status (not (eq theirs symbol)))
            (push theirs taken))))
      (when taken
        ;; Theirs, kept: SHADOWING-IMPORT of a symbol already present is how
        ;; you say "this name is decided" without changing what it means.
        (shadowing-import taken target)
        (r:logmsg :warn "lattice: ~d name~:p in ~a already meant something else ~
and still do: ~{~a~^ ~}"
                  (length taken) (package-name target)
                  (sort (mapcar #'symbol-name taken) #'string<)))
      (use-package source target))
    target))

;; Do it on load, so that `(load-extension "lattice")' is the whole of the
;; installation and `lattice:enable' is genuinely optional.
(eval-when (:load-toplevel :execute)
  (install-vocabulary))
