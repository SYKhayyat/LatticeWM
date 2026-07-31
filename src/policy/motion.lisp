;;;; policy/motion.lisp --- Directional motion, written once for every boundary.
;;;;
;;;; THE ALGORITHM
;;;;
;;;; From the cursor's leaf, walk *up* the ancestor chain.  At each container
;;;; ask STEP-ADDRESS whether it can satisfy the motion.  The first one that
;;;; can, does; then descend into whatever is there, asking ENTRY-ADDRESS which
;;;; child to land on, until a leaf is reached.
;;;;
;;;; Thirty lines.  What they buy:
;;;;
;;;;   * motion between panes of a split;
;;;;   * motion that *leaves* a split when the direction is across its axis —
;;;;     pressing Up inside a row of side-by-side panes goes to whatever is
;;;;     above the row, not to a neighbour in it;
;;;;   * motion that crosses from a split into the lattice cell next door and
;;;;     lands on that cell's *nearest* pane, entering through the edge it
;;;;     crossed, with no mode and no second command.  PLAN.org's Delta 1 is
;;;;     this property, and it falls out rather than being built;
;;;;   * motion that skips a stack rather than surfacing a hidden tab;
;;;;   * motion through container kinds that did not exist when this was
;;;;     written, provided they answer STEP-ADDRESS and ENTRY-ADDRESS.
;;;;
;;;; Nothing here knows what a split, a stack or a grid is.  That is the test
;;;; of whether the container protocol was drawn in the right place.
;;;;
;;;; WHY MOTION IS GEOMETRIC
;;;;
;;;; Consider a row of two columns, each split into a top and a bottom pane.
;;;; You are in the *bottom* left pane and press Right.  Structural entry rules
;;;; land you at the first child of the right column — the *top* pane — and
;;;; pressing Left then returns you to the top left pane, not where you began.
;;;;
;;;; Motion that does not come back is disorienting out of all proportion to
;;;; how small the bug sounds, because it silently destroys the mental model
;;;; that the layout is a place.  So entry across a container's axis is decided
;;;; by *where you are*, not by an index: the child whose rectangle best lines
;;;; up with the one you left.  That is what Emacs's windmove and i3 both do,
;;;; and it is involutive by construction.
;;;;
;;;; The rectangles come from the last layout, which the runtime caches.  When
;;;; there is no cache — during startup, and in unit tests — a layout is
;;;; computed on the spot over a nominal output.  A tree has tens of nodes, so
;;;; that costs nothing worth measuring, and it means motion behaves identically
;;;; whether or not anything has been drawn yet.

(in-package #:latticewm/policy)

(define-option *motion-reference-rect* '(0 0 1920 1080)
  "The nominal output used to compute geometry for motion when no layout has
been drawn yet, as (X Y WIDTH HEIGHT).

Only the aspect ratio matters, and only for deciding which pane lines up with
which across a boundary.  You will not need to change this.")

(defun motion-rects (policy root)
  "A hash of NODE to RECT for everything under ROOT, from a fresh layout."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (placement (layout-node policy root
                                    (apply #'c:make-rect *motion-reference-rect*))
                       table)
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path visible))
        (setf (gethash node table) rect)))))

(defun best-aligned-address (container reference rects axis)
  "Which child of CONTAINER lines up best with REFERENCE along AXIS?

Returns an address, or NIL when no child has a known rectangle.  'Best' is:
the child whose extent along AXIS contains the reference's centre, and failing
that the child whose extent is nearest to it.  Overlap beats proximity, because
a pane you are partly in front of is the one you mean."
  (when (null rects) (return-from best-aligned-address nil))
  (let* ((horizontal (eq axis :horizontal))
         (centre (if horizontal
                     (+ (c:rect-x reference) (floor (c:rect-w reference) 2))
                     (+ (c:rect-y reference) (floor (c:rect-h reference) 2))))
         (best nil)
         (best-distance nil))
    (dolist (address (c:container-addresses container) best)
      (let* ((child (c:child-at container address))
             (rect (and child (gethash child rects))))
        (when rect
          (let* ((low (if horizontal (c:rect-x rect) (c:rect-y rect)))
                 (high (if horizontal (c:rect-right rect) (c:rect-bottom rect)))
                 (distance (cond ((< centre low) (- low centre))
                                 ((>= centre high) (- centre high -1))
                                 (t 0))))
            (when (or (null best-distance) (< distance best-distance))
              (setf best address best-distance distance))))))))

(defun descend-to-leaf (policy node path direction &key reference rects)
  "Descend from NODE to a leaf, entering each container as if travelling DIRECTION.

REFERENCE is the rectangle being left, and RECTS maps nodes to rectangles;
together they let each container answer geometrically rather than by index.
DIRECTION may be NIL for a non-directional arrival — a jump to a name, a typed
coordinate, a click — in which case each container answers with its first
child instead of its edge-adjacent one."
  (loop
    (unless (c:container-p node) (return path))
    (let ((address (entry-address policy node direction reference rects)))
      (when (null address) (return path))
      (let ((child (c:child-at node address)))
        (when (null child) (return path))
        (setf node child
              path (c:path-append path address))))))

(defun find-motion-target (policy root path direction &key (rects nil rects-supplied))
  "The path motion DIRECTION from PATH arrives at, or NIL when it hits an edge.

This is the whole of directional navigation.  Specialize STEP-ADDRESS or
ENTRY-ADDRESS to change *what a container does*; redefine this only to change
the walk itself, which almost nothing wants to.

RECTS maps nodes to rectangles and is what makes entry geometric.  Omit it and
one is computed; pass NIL explicitly to force purely structural entry."
  (let ((chain (c:resolve-chain root path))
        (rects (if rects-supplied rects (motion-rects policy root))))
    (when (null chain) (return-from find-motion-target nil))
    (let ((reference (gethash (car (last chain)) rects)))
      ;; CHAIN is (root … node); walk it from the deepest container upwards.
      (loop for depth from (1- (length chain)) downto 1
            for container = (nth (1- depth) chain)
            for address = (nth (1- depth) path)
            do (unless (c:container-p container) (return nil))
               (let ((next (step-address policy container address direction)))
                 (cond
                   (next
                    (let ((child (c:child-at container next))
                          (base (c:path-append (subseq path 0 (1- depth)) next)))
                      (return-from find-motion-target
                        (if child
                            ;; Enter through the edge we crossed: travelling
                            ;; right means arriving at the *left* of what is
                            ;; there, at the height we were already at.
                            (descend-to-leaf policy child base direction
                                             :reference reference :rects rects)
                            base))))
                   ((not (motion-escapes-p policy container direction))
                    ;; A container that traps motion: stop rather than ascend.
                    (return-from find-motion-target nil)))))
      nil)))

(defun move-cursor (policy world direction)
  "Move the world's cursor one step DIRECTION.  Returns the new path, or NIL.

Returning NIL means the motion hit the edge of the world and nothing moved.
Callers should treat that as a no-op and not as an error: bumping into the
edge is an ordinary thing to do and must not produce a beep, a log line, or a
condition."
  (let* ((cached (c:prop world :rect-index))
         (target (find-motion-target policy (c:world-root world)
                                     (c:world-cursor world) direction
                                     :rects (or cached
                                                (motion-rects
                                                 policy (c:world-root world))))))
    (when target
      (let ((old (c:world-cursor world)))
        (setf (c:world-cursor world) target)
        (ignore-errors (on-focus-change policy world old target))
        target))))

(defun jump-cursor (policy world path)
  "Put the cursor at PATH, resolving into a leaf if PATH names a container.

Non-directional arrival, so containers answer with their first child rather
than an edge — README D20's second rule."
  (let* ((root (c:world-root world))
         (node (c:resolve-path root path))
         (target (if node
                     (descend-to-leaf policy node path nil)
                     (c:repair-path root path))))
    (let ((old (c:world-cursor world)))
      (setf (c:world-cursor world) target)
      (ignore-errors (on-focus-change policy world old target))
      target)))

(defun repair-cursor (policy world &optional (was (c:world-cursor world)))
  "Put the cursor somewhere valid after the tree changed under it.

WAS may be a path that no longer names a leaf — the usual case being that the
node it pointed at just became a split, so the cursor is now pointing at a
container rather than a place.  Every verb that restructures the tree calls
this; it is REPAIR-PATH plus the focus-change hook."
  (let* ((root (c:world-root world))
         (target (c:repair-path root was))
         (old (c:world-cursor world)))
    (setf (c:world-cursor world) target)
    (unless (c:path-equal old target)
      (ignore-errors (on-focus-change policy world old target)))
    target))
