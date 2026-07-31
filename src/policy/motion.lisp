;;;; policy/motion.lisp --- Directional motion, written once for every boundary.
;;;;
;;;; THE ALGORITHM
;;;;
;;;; From the cursor's leaf, walk *up* the ancestor chain.  At each container
;;;; ask STEP-ADDRESS whether it can satisfy the motion.  The first one that
;;;; can, does; then descend into whatever is there, asking ENTRY-ADDRESS which
;;;; child to land on, until a leaf is reached.
;;;;
;;;; Twenty lines.  What they buy:
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

(in-package #:latticewm/policy)

(defun descend-to-leaf (policy node path direction)
  "Descend from NODE to a leaf, entering each container as if travelling DIRECTION.

Returns the path of the leaf.  DIRECTION may be NIL for a non-directional
arrival — a jump to a name, a typed coordinate, a click — in which case each
container answers with its first child instead of its edge-adjacent one."
  (loop
    (unless (c:container-p node) (return path))
    (let ((address (entry-address policy node direction)))
      (when (null address) (return path))
      (let ((child (c:child-at node address)))
        (when (null child) (return path))
        (setf node child
              path (c:path-append path address))))))

(defun find-motion-target (policy root path direction)
  "The path motion DIRECTION from PATH arrives at, or NIL when it hits an edge.

This is the whole of directional navigation.  Specialize STEP-ADDRESS or
ENTRY-ADDRESS to change *what a container does*; redefine this only to change
the walk itself, which almost nothing wants to."
  (let ((chain (c:resolve-chain root path)))
    (when (null chain) (return-from find-motion-target nil))
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
                          ;; there.  This is what makes motion involutive.
                          (descend-to-leaf policy child base direction)
                          base))))
                 ((not (motion-escapes-p policy container direction))
                  ;; A container that traps motion: stop rather than ascend.
                  (return-from find-motion-target nil)))))
    nil))

(defun move-cursor (policy world direction)
  "Move the world's cursor one step DIRECTION.  Returns the new path, or NIL.

Returning NIL means the motion hit the edge of the world and nothing moved.
Callers should treat that as a no-op and not as an error: bumping into the
edge is an ordinary thing to do and must not produce a beep, a log line, or a
condition."
  (let ((target (find-motion-target policy (c:world-root world)
                                    (c:world-cursor world) direction)))
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
