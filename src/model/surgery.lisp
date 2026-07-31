;;;; model/surgery.lisp --- Pure tree surgery.
;;;;
;;;; "Pure" means: no protocol calls, no globals, no side effects outside the
;;;; tree you hand in.  These functions mutate the tree — a window manager is
;;;; not a good place for persistent data structures, and StumpWM has mutated
;;;; happily for twenty-five years — but they touch nothing else, so all of it
;;;; is unit-testable without a compositor.  Take that free lunch; it is the
;;;; only part of a window manager that offers one.
;;;;
;;;; Every function here returns
;;;;
;;;;     (values NEW-ROOT NEW-PATH …)
;;;;
;;;; NEW-ROOT because collapsing a degenerate container can replace the root
;;;; itself, and NEW-PATH because the caller's focus path is stale the instant
;;;; the tree changes.  Callers that ignore the second value have a focus bug
;;;; they have not noticed yet.
;;;;
;;;; The pattern used throughout for keeping focus honest: resolve the focused
;;;; *node* before mutating, mutate, then ask where that node ended up.  Trying
;;;; to compute path deltas through insertion, removal and collapse is the
;;;; obvious approach and it is wrong — it has a dozen cases and each one is a
;;;; separate opportunity to be subtly off by one.

(in-package #:latticewm/core)

(defun %simplify-upwards (root path &optional (allow-collapse t))
  "Collapse degenerate containers from the node at PATH up to ROOT.

ALLOW-COLLAPSE is T, NIL, or a predicate of one node.  It gates only the
*dissolve into my one remaining child* case — i3 keeps such containers, Emacs
does not, and that is a genuine preference.  It does not gate structural
invariant repair: a container that answers NIL is always removed, and a
container that quietly fixes itself up (a workspace stack regaining an empty
workspace) always does, because those are not preferences but the difference
between a valid tree and a broken one.

Returns the possibly-new root.  Walks bottom-up because collapsing a child can
make its parent degenerate in turn: close two of three windows in a nested
split and the whole nest should unwind in one step, not leave a ladder of
one-child splits behind.

SIMPLIFY-NODE may answer NIL, meaning 'remove me from my parent entirely'.
That is the case that makes the unwinding total rather than leaving an empty
pane the user never asked for — see SIMPLIFY-NODE's docstring for why debris
must not be able to impersonate D17's deliberate empty pane.  The root itself
is never removed; if it simplifies to nothing it becomes an empty leaf, since
something has to be there."
  (let ((chain (resolve-chain root path)))
    (when (null chain) (return-from %simplify-upwards root))
    (loop for i from (1- (length chain)) downto 0
          for node = (nth i chain)
          for simpler = (let ((s (simplify-node node)))
                          (if (and s (not (eq s node))
                                   (not (if (functionp allow-collapse)
                                            (funcall allow-collapse node)
                                            allow-collapse)))
                              node          ; the collapse was declined
                              s))
          do (cond
               ((eq simpler node))          ; the common case: nothing to do
               ((zerop i) (setf root (or simpler (make-leaf))))
               (simpler
                (setf (child-at (nth (1- i) chain) (nth (1- i) path)) simpler))
               (t
                (remove-child (nth (1- i) chain) (nth (1- i) path))))
          finally (return root))))

;;; ----------------------------------------------------------------- insert

(defun tree-insert-at (root path node &key (focus-path nil focus-supplied))
  "Insert NODE into ROOT so that it comes to occupy PATH.

PATH's parent must be a container.  For a dense container this shifts the
existing children from that address on; for a sparse one it simply fills the
address.

Returns (values ROOT REPAIRED-FOCUS).  When FOCUS-PATH is supplied it is
tracked across the insertion; otherwise the path of NODE itself is returned,
which is what a spawn wants."
  (let* ((parent-path (parent-path path))
         (address (path-last path))
         (parent (resolve-path root parent-path))
         (focused (and focus-supplied (resolve-path root focus-path))))
    (unless (container-p parent)
      (error "Cannot insert at ~s: its parent ~s is not a container." path parent))
    (insert-child parent address node)
    (values root
            (cond (focused (or (node-path-to root focused)
                               (repair-path root focus-path)))
                  (focus-supplied (repair-path root focus-path))
                  (t (or (node-path-to root node) (repair-path root path)))))))

;;; ----------------------------------------------------------------- remove

(defun tree-remove-at (root path &key (simplify t) (focus-path path))
  "Take the node at PATH out of ROOT.

Returns (values REMOVED-NODE NEW-ROOT NEW-FOCUS-PATH).

SIMPLIFY is T, NIL, or a predicate of one node deciding whether *that*
container may dissolve into its last child.  With T (the default) any container
left degenerate by the removal is collapsed.  Pass NIL when you are about to
put something back and do not want the tree to unwind first; TREE-MOVE relies
on that.  Pass a predicate to honour a per-container preference — which is what
the shipped policy does, so that SHOULD-COLLAPSE-P is consulted per node rather
than once for the whole operation.

NEW-FOCUS-PATH is where a cursor sitting at FOCUS-PATH should land.  If the
focused node itself was what got removed, the result is REPAIR-PATH's answer:
the deepest surviving ancestor's first leaf.  That is deliberately *not* 'the
most recently used window' — README D18's governing payoff is that nothing
ever moves the viewport except the user, and MRU can be anywhere on the plane,
so honouring it here would let a close teleport you across your desktop.
Policy can still ask for MRU; see FOCUS-AFTER-REMOVE."
  (when (null path)
    (error "Refusing to remove the root of the tree."))
  (let* ((parent-path (parent-path path))
         (address (path-last path))
         (parent (resolve-path root parent-path))
         (focused (resolve-path root focus-path))
         (removing (resolve-path root path)))
    (unless (container-p parent)
      (error "Cannot remove ~s: its parent is not a container." path))
    ;; If focus is inside the subtree we are about to remove, it cannot
    ;; survive; fall back to path-based repair afterwards.
    (when (and focused removing (or (eq focused removing)
                                    (node-path-to removing focused)))
      (setf focused nil))
    (let ((removed (remove-child parent address)))
      (when simplify
        (setf root (%simplify-upwards root parent-path simplify)))
      (values removed
              root
              (or (and focused (node-path-to root focused))
                  (repair-path root (if (path-equal focus-path path)
                                        parent-path
                                        focus-path)))))))

;;; ---------------------------------------------------------------- replace

(defun tree-replace-at (root path node &key (focus-path nil focus-supplied))
  "Put NODE at PATH, discarding whatever was there.

Returns (values NEW-ROOT NEW-FOCUS-PATH).  Replacing the root — PATH being the
empty list — is legal and returns NODE as the new root."
  (let ((focused (and focus-supplied (resolve-path root focus-path))))
    (if (null path)
        (setf root node)
        (let ((parent (resolve-path root (parent-path path))))
          (unless (container-p parent)
            (error "Cannot replace ~s: its parent is not a container." path))
          (setf (child-at parent (path-last path)) node)))
    (values root
            (cond ((and focused (node-path-to root focused)))
                  (focus-supplied (repair-path root focus-path))
                  (t (repair-path root path))))))

;;; ------------------------------------------------------------------ split

(defun tree-split-at (root path new-node
                      &key (axis :horizontal) (side :after) weights)
  "Replace the node at PATH with a split holding it and NEW-NODE.

AXIS is :HORIZONTAL (side by side) or :VERTICAL (stacked).  SIDE is :AFTER —
NEW-NODE lands right of, or below, the existing node — or :BEFORE.

When the node at PATH is *already* a split along AXIS, NEW-NODE joins it as a
sibling rather than creating a nested split.  That is what stops a tree from
growing a ladder of two-child splits when the user presses the same key three
times, and it is why three windows side by side stay one split of three rather
than becoming a lopsided nest.

Returns (values NEW-ROOT PATH-OF-NEW-NODE)."
  (let ((existing (resolve-path root path)))
    (unless existing
      (error "Cannot split at ~s: nothing is there." path))
    ;; Case 1: the *parent* is already a split along this axis — join it.
    (let ((parent (and path (resolve-path root (parent-path path)))))
      (when (and (typep parent 'split) (eq (split-axis parent) axis))
        (let* ((address (path-last path))
               (target (if (eq side :after) (1+ address) address)))
          (insert-child parent target new-node)
          (return-from tree-split-at
            (values root (path-append (parent-path path) target))))))
    ;; Case 2: build a fresh split in place.
    (let* ((children (if (eq side :after)
                         (list existing new-node)
                         (list new-node existing)))
           (split (make-split axis children weights))
           (index (if (eq side :after) 1 0)))
      (multiple-value-bind (new-root) (tree-replace-at root path split)
        (values new-root (path-append path index))))))

;;; ------------------------------------------------------------------- swap

(defun tree-swap (root path-a path-b &key (focus-path nil focus-supplied))
  "Exchange the subtrees at PATH-A and PATH-B in place.

Neither subtree changes shape and neither container changes size, so this is
the cheap, predictable way to rearrange: two things trade places and nothing
else moves.  Refuses when one path is an ancestor of the other, which would
make a cycle.

Returns (values NEW-ROOT NEW-FOCUS-PATH)."
  (let ((a (resolve-path root path-a))
        (b (resolve-path root path-b)))
    (unless (and a b)
      (return-from tree-swap (values root (repair-path root (or focus-path path-a)))))
    (when (or (node-path-to a b) (node-path-to b a))
      (error "Refusing to swap ~s with ~s: one contains the other." path-a path-b))
    (let ((focused (and focus-supplied (resolve-path root focus-path))))
      (setf root (tree-replace-at root path-a b))
      (setf root (tree-replace-at root path-b a))
      (values root
              (cond ((and focused (node-path-to root focused)))
                    (focus-supplied (repair-path root focus-path))
                    (t (repair-path root path-b)))))))

;;; ------------------------------------------------------------------- move

(defun tree-move (root from to &key (axis :horizontal) (side :after) (join :split))
  "Move the subtree at FROM so that it lands at TO.

TO may name

  * an empty leaf         — the subtree replaces it outright;
  * an occupied node      — JOIN decides:
                              :SPLIT  the two become a split along AXIS
                                      (the shipped default: it is the only
                                      option that never destroys structure),
                              :SWAP   they trade places instead,
                              :STACK  they become a stack, i.e. tabs;
  * a container's address — the subtree is inserted there.

Returns (values NEW-ROOT NEW-PATH-OF-MOVED-SUBTREE).

The removal happens with :SIMPLIFY NIL and simplification is deferred until
after reinsertion, because collapsing the source container first can shift the
destination path out from under us — the classic bug in this operation."
  (when (or (null from) (path-equal from to))
    (return-from tree-move (values root from)))
  (let ((moving (resolve-path root from))
        (destination (resolve-path root to)))
    (unless moving
      (return-from tree-move (values root (repair-path root from))))
    (when (and destination (node-path-to moving destination))
      (error "Refusing to move ~s into ~s, which it contains." from to))
    (when (null destination)
      (return-from tree-move (values root (repair-path root from))))
    ;; Swap needs no detachment at all.
    (when (eq join :swap)
      (multiple-value-bind (new-root) (tree-swap root from to)
        (return-from tree-move
          (values new-root (or (node-path-to new-root moving)
                               (repair-path new-root to))))))
    ;; Detach without simplifying.  Simplification is deferred until after
    ;; reinsertion because collapsing the source container first can shift the
    ;; destination out from under us — the classic bug in this operation.
    ;;
    ;; Note that TO is re-derived from the destination *node* rather than
    ;; reused: removing a child renumbers every later sibling, so the literal
    ;; path the caller handed in may now name something else entirely.  That
    ;; was the other classic bug, and it silently deleted the window being
    ;; moved rather than misplacing it.
    (multiple-value-bind (removed after-remove) (tree-remove-at root from
                                                                :simplify nil)
      (declare (ignore removed))
      (setf root after-remove)
      (let* ((to (node-path-to root destination))
             (source-parent (parent-path from)))
        (cond
          ;; The destination did not survive the detachment.  Put the subtree
          ;; back rather than dropping it on the floor.
          ((null to)
           (setf root (tree-replace-at root (repair-path root from) moving))
           (values root (or (node-path-to root moving) (repair-path root from))))
          ;; An empty pane simply becomes the thing.
          ((and (typep destination 'leaf) (leaf-empty-p destination))
           (setf root (tree-replace-at root to moving))
           (setf root (%simplify-upwards root source-parent))
           (values root (or (node-path-to root moving) (repair-path root to))))
          (t
           (let ((landed
                   (ecase join
                     (:split
                      (multiple-value-bind (r p)
                          (tree-split-at root to moving :axis axis :side side)
                        (setf root r) p))
                     (:stack
                      (let ((stack (make-stack
                                    (if (eq side :after)
                                        (list destination moving)
                                        (list moving destination))
                                    (if (eq side :after) 1 0))))
                        (setf root (tree-replace-at root to stack))
                        (path-append to (if (eq side :after) 1 0)))))))
             (setf root (%simplify-upwards root source-parent))
             (values root (or (node-path-to root moving)
                              (repair-path root landed))))))))))

;;; -------------------------------------------------------------- transplant

(defun tree-transplant (root from to-container-path address &key (simplify t))
  "Move the subtree at FROM to ADDRESS inside the container at TO-CONTAINER-PATH.

The general form used by 'send this window to workspace 3' and by the lattice's
'move this window to cell (2,-1)'.  Unlike TREE-MOVE it addresses a *slot in a
container* rather than a node, so it can target places that are currently
empty — which is exactly what a sparse lattice needs.

Returns (values NEW-ROOT NEW-PATH-OF-MOVED-SUBTREE)."
  (let ((moving (resolve-path root from))
        (target (resolve-path root to-container-path)))
    (unless (and moving (container-p target))
      (return-from tree-transplant (values root (repair-path root from))))
    (when (node-path-to moving target)
      (error "Refusing to transplant ~s into its own descendant." from))
    (multiple-value-bind (removed after-remove) (tree-remove-at root from :simplify nil)
      (declare (ignore removed))
      (setf root after-remove)
      ;; Re-find the target by identity, not by the path handed in: removing a
      ;; child renumbers its later siblings, so that path may now name a
      ;; different node.
      ;;
      ;; NODE-CONTAINS-P rather than NODE-PATH-TO, because the path to the root
      ;; *is* the empty list, which is false — so testing survival with
      ;; NODE-PATH-TO reports the root as missing.  That was a real bug and it
      ;; silently dropped a subtree whenever the destination was the root
      ;; container, which for a lattice is every single cell move.
      (let ((target (and (node-contains-p root target) target))
            (to-container-path (node-path-to root target)))
        (cond
          ((not (container-p target))
           (setf root (%simplify-upwards root (parent-path from)))
           (values root (or (node-path-to root moving) (repair-path root from))))
          (t
           (let ((occupant (child-at target address)))
             (if (and occupant (not (and (typep occupant 'leaf)
                                         (leaf-empty-p occupant))))
                 (insert-child target address moving)
                 (if occupant
                     (setf (child-at target address) moving)
                     (insert-child target address moving))))
           (when simplify
             (setf root (%simplify-upwards root (parent-path from))))
           (values root (or (node-path-to root moving)
                            (repair-path root to-container-path)))))))))
