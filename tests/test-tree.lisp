;;;; tests/test-tree.lisp --- The container protocol, paths, and surgery.

(in-package #:latticewm/tests)
(in-suite tree)

;;; ------------------------------------------------- the container protocol

(test split-addressing
  (let* ((kids (leaves 3))
         (s (c:make-split :horizontal kids)))
    (is (equal '(0 1 2) (c:container-addresses s)))
    (is (eq (second kids) (c:child-at s 1)))
    (is (null (c:child-at s 3)) "an out-of-range address answers NIL, not an error")
    (is (null (c:child-at s -1)))
    (is (= 3 (c:container-count s)))))

(test split-insert-keeps-weights-parallel
  (let ((s (c:make-split :horizontal (leaves 2))))
    (c:insert-child s 1 (c:make-leaf))
    (is (= 3 (length (c:children s))))
    (is (= 3 (length (c:weights s)))
        "weights must stay parallel to children or layout desynchronises")))

(test split-remove-keeps-weights-parallel
  (let ((s (c:make-split :horizontal (leaves 3) '(1 5 1))))
    (c:remove-child s 1)
    (is (= 2 (length (c:children s))))
    (is (equal '(1 1) (c:weights s)) "the removed child's weight went with it")))

(test split-simplifies-to-its-only-child
  (let* ((kid (c:make-leaf))
         (s (c:make-split :horizontal (list kid))))
    (is (eq kid (c:simplify-node s))))
  (is (null (c:simplify-node (c:make-split :horizontal '())))
      "and a split with no children asks to be removed outright, rather than
becoming an empty pane the user never asked for"))

(test stack-survives-as-a-singleton
  ;; A one-workspace workspace list is a thing you are about to add to, not
  ;; debris.  This is the one place STACK and SPLIT deliberately differ.
  (let ((s (c:make-stack (leaves 1))))
    (is (eq s (c:simplify-node s)))))

(test stack-selection-follows-removal
  (let ((s (c:make-stack (leaves 4) 3)))
    (c:remove-child s 1)
    (is (= 2 (c:stack-selected s)) "the selected child kept its identity"))
  (let ((s (c:make-stack (leaves 4) 3)))
    (c:remove-child s 3)
    (is (= 2 (c:stack-selected s)) "selection never points off the end"))
  (let ((s (c:make-stack (leaves 1) 0)))
    (c:remove-child s 0)
    (is (= 0 (c:stack-selected s)))))

(test stack-insert-before-selection-shifts-it
  (let ((s (c:make-stack (leaves 3) 2)))
    (c:insert-child s 0 (c:make-leaf))
    (is (= 3 (c:stack-selected s)))))

(test adjust-weight-only-disturbs-the-neighbour
  (let ((s (c:make-split :horizontal (leaves 3) '(1 1 1))))
    (c:adjust-weight s 0 1/2)
    (is (equal '(3/2 1/2 1) (c:weights s))
        "dragging one divider must not move the divider beyond it")))

(test adjust-weight-cannot-annihilate-a-pane
  (let ((s (c:make-split :horizontal (leaves 2) '(1 1))))
    (c:adjust-weight s 0 1000)
    (is (every #'plusp (c:weights s)))))

;;; ---------------------------------------------------------------- paths

(test resolve-path
  (let* ((deep (c:make-leaf))
         (root (c:make-stack
                (list (c:make-split :horizontal
                                    (list (c:make-leaf)
                                          (c:make-split :vertical
                                                        (list (c:make-leaf) deep))))))))
    (is (eq deep (c:resolve-path root '(0 1 1))))
    (is (eq root (c:resolve-path root '())))
    (is (null (c:resolve-path root '(0 9))))
    (is (null (c:resolve-path root '(0 1 1 0))) "cannot descend past a leaf")
    (is (equal '(0 1 1) (c:node-path-to root deep)))
    (is (= 4 (length (c:resolve-chain root '(0 1 1))))
        "the chain includes the root and the target")))

(test leaf-paths-are-in-layout-order
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical
                                                (list (leaf-with "b")
                                                      (leaf-with "c")))
                                  (leaf-with "d")))))
    (is (equal '((0) (1 0) (1 1) (2)) (c:leaf-paths root)))
    (is (equal '(0) (c:first-leaf-path root)))
    (is (equal '(2) (c:last-leaf-path root)))
    (is (equal '(1 0) (c:next-leaf-path root '(0))))
    (is (equal '(0) (c:next-leaf-path root '(2))) "wraps by default")
    (is (equal '(2) (c:previous-leaf-path root '(0))))))

(test first-leaf-path-follows-the-stack-selection
  ;; You must not land on a hidden tab.
  (let ((root (c:make-stack (list (leaf-with "a") (leaf-with "b")) 1)))
    (is (equal '(1) (c:first-leaf-path root)))
    (is (equal "b" (app-at root (c:first-leaf-path root))))))

(test leaf-enumeration-skips-hidden-alternatives
  ;; The root of the shipped world is a stack of workspaces; enumerating leaves
  ;; for focus-cycling must stay inside the visible one, or FOCUS-NEXT walks
  ;; into a window on a workspace you cannot see.  First/last/next/previous must
  ;; all agree with FIRST-LEAF-PATH, which already follows the selection.
  (let ((root (c:make-stack
               (list (leaf-with "visible")
                     (c:make-split :horizontal
                                   (list (leaf-with "hidden-1")
                                         (leaf-with "hidden-2"))))
               0)))
    (is (equal '((0)) (c:leaf-paths root))
        "only the selected workspace's leaves are places focus may land")
    (is (equal '(0) (c:last-leaf-path root)) "last visible leaf, not a hidden one")
    (is (equal '(0) (c:next-leaf-path root '(0)))
        "next from the only visible leaf wraps to itself, never to a hidden workspace")
    (is (equal '(0) (c:previous-leaf-path root '(0))))))

(test repair-path-is-total
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (is (equal '(0) (c:repair-path root '(0))) "a valid path is unchanged")
    (is (equal '(0) (c:repair-path root '(0 5 5)))
        "descends no further than the tree allows")
    (is (equal '(0) (c:repair-path root '(9)))
        "a wholly invalid path lands on the root's first leaf")
    (is (equal '() (c:repair-path (c:make-leaf) '(1 2 3))))))

;;; --------------------------------------------------------------- surgery

(test remove-collapses-the-degenerate-parent
  (let ((root (c:make-stack
               (list (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))))))
    (multiple-value-bind (removed new-root focus)
        (c:tree-remove-at root '(0 1))
      (declare (ignore removed))
      (is (equal '(:stack 0 (:leaf "a")) (shape new-root))
          "the split dissolved into its survivor")
      (is (equal "a" (app-at new-root focus))))))

(test remove-without-simplify-leaves-the-container
  (let ((root (c:make-stack
               (list (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))))))
    (multiple-value-bind (removed new-root) (c:tree-remove-at root '(0 1)
                                                              :simplify nil)
      (declare (ignore removed))
      (is (equal '(:stack 0 (:h (:leaf "a"))) (shape new-root))))))

(test remove-unwinds-a-whole-nest-in-one-step
  ;; Close two of three windows in a nested split and the ladder of one-child
  ;; splits must not survive.
  (let ((root (c:make-split
               :horizontal
               (list (leaf-with "a")
                     (c:make-split :vertical
                                   (list (c:make-split :horizontal
                                                       (list (leaf-with "b")))))))))
    (multiple-value-bind (removed new-root) (c:tree-remove-at root '(1 0 0))
      (declare (ignore removed))
      (is (equal '(:leaf "a") (shape new-root))
          "every level of the nest evaporated, leaving no empty pane behind"))))

(test remove-repairs-focus-that-was-inside-the-removed-subtree
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical
                                                (list (leaf-with "b")
                                                      (leaf-with "c")))))))
    (multiple-value-bind (removed new-root focus)
        (c:tree-remove-at root '(1) :focus-path '(1 0))
      (declare (ignore removed))
      (is (c:path-valid-p new-root focus))
      (is (equal "a" (app-at new-root focus))))))

(test remove-keeps-focus-on-an-unrelated-node
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a") (leaf-with "b") (leaf-with "c")))))
    (multiple-value-bind (removed new-root focus)
        (c:tree-remove-at root '(0) :focus-path '(2))
      (declare (ignore removed))
      (is (equal "c" (app-at new-root focus))
          "focus followed the node, not the index"))))

(test remove-refuses-the-root
  (signals error (c:tree-remove-at (c:make-leaf) '())))

(test replace-at-swaps-the-node-and-keeps-outside-focus
  ;; The workhorse under split/swap/transplant, and the carrier of the
  ;; :focus-path repair contract -- the "subtly wrong focus repair" class the
  ;; suite header calls near-undebuggable -- yet it had no direct test.
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a") (leaf-with "b") (leaf-with "c")))))
    (multiple-value-bind (new-root focus)
        (c:tree-replace-at root '(1) (leaf-with "x") :focus-path '(2))
      (is (equal '(:h (:leaf "a") (:leaf "x") (:leaf "c")) (shape new-root)))
      (is (equal '(2) focus) "focus outside the replaced node is unchanged")
      (is (equal "c" (app-at new-root focus))))))

(test replace-at-repairs-focus-that-was-inside-the-replaced-node
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical
                                                (list (leaf-with "b")
                                                      (leaf-with "c")))))))
    ;; Focus sat on (1 0); replacing (1) outright destroys the node it named,
    ;; so the repair must land on a valid leaf at or near the old focus path.
    (multiple-value-bind (new-root focus)
        (c:tree-replace-at root '(1) (leaf-with "x") :focus-path '(1 0))
      (is (equal '(:h (:leaf "a") (:leaf "x")) (shape new-root)))
      (is (c:resolve-path new-root focus) "the repaired focus is a real place")
      (is (equal '(1) focus) "and it is the nearest surviving node to the old one"))))

(test replace-at-the-root-is-legal
  (let ((root (leaf-with "a")))
    (multiple-value-bind (new-root focus)
        (c:tree-replace-at root '() (leaf-with "b") :focus-path '())
      (is (equal '(:leaf "b") (shape new-root)) "the empty path replaces the root")
      (is (equal '() focus)))))

(test split-at-creates-a-split
  (let ((root (c:make-stack (list (leaf-with "a")))))
    (multiple-value-bind (new-root path)
        (c:tree-split-at root '(0) (leaf-with "b") :axis :vertical)
      (is (equal '(:stack 0 (:v (:leaf "a") (:leaf "b"))) (shape new-root)))
      (is (equal '(0 1) path))
      (is (equal "b" (app-at new-root path))))))

(test split-at-before
  (let ((root (c:make-stack (list (leaf-with "a")))))
    (multiple-value-bind (new-root path)
        (c:tree-split-at root '(0) (leaf-with "b") :side :before)
      (is (equal '(:stack 0 (:h (:leaf "b") (:leaf "a"))) (shape new-root)))
      (is (equal '(0 0) path)))))

(test split-at-joins-an-existing-split-of-the-same-axis
  ;; This is what stops a tree growing a ladder of two-child splits when the
  ;; user presses the same key three times.
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (multiple-value-bind (new-root path)
        (c:tree-split-at root '(1) (leaf-with "c") :axis :horizontal)
      (is (equal '(:h (:leaf "a") (:leaf "b") (:leaf "c")) (shape new-root))
          "three side by side stayed one split of three")
      (is (equal '(2) path)))))

(test split-at-nests-when-the-axis-differs
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (multiple-value-bind (new-root)
        (c:tree-split-at root '(1) (leaf-with "c") :axis :vertical)
      (is (equal '(:h (:leaf "a") (:v (:leaf "b") (:leaf "c")))
                 (shape new-root))))))

(test swap-exchanges-in-place
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical (list (leaf-with "b")
                                                                (leaf-with "c")))))))
    (multiple-value-bind (new-root) (c:tree-swap root '(0) '(1 1))
      (is (equal '(:h (:leaf "c") (:v (:leaf "b") (:leaf "a")))
                 (shape new-root))))))

(test swap-refuses-an-ancestor
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical (list (leaf-with "b")))))))
    (signals error (c:tree-swap root '(1) '(1 0)))))

(test move-onto-an-empty-pane-fills-it
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (c:make-leaf)))))
    (multiple-value-bind (new-root path) (c:tree-move root '(0) '(1))
      (is (equal '(:leaf "a") (shape new-root))
          "the source pane went with it and the split collapsed")
      (is (equal '() path)))))

(test move-onto-an-occupied-pane-splits
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a") (leaf-with "b") (leaf-with "c")))))
    (multiple-value-bind (new-root path)
        (c:tree-move root '(0) '(2) :axis :vertical)
      (is (equal '(:h (:leaf "b") (:v (:leaf "c") (:leaf "a"))) (shape new-root)))
      (is (equal "a" (app-at new-root path))))))

(test move-with-swap-join
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (multiple-value-bind (new-root) (c:tree-move root '(0) '(1) :join :swap)
      (is (equal '(:h (:leaf "b") (:leaf "a")) (shape new-root))))))

(test move-with-stack-join-makes-tabs
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (multiple-value-bind (new-root path) (c:tree-move root '(0) '(1) :join :stack)
      (is (equal '(:stack 1 (:leaf "b") (:leaf "a")) (shape new-root)))
      (is (equal "a" (app-at new-root path))))))

(test move-out-of-a-nested-split-joins-without-leaving-debris
  ;; The destination join renumbers the source parent's siblings, so the
  ;; source parent must be re-derived by identity before it is simplified --
  ;; otherwise the emptied one-child split it left behind survives, which is
  ;; the exact ladder-of-splits this whole file exists to prevent.
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-split :vertical
                                                (list (leaf-with "b")
                                                      (leaf-with "c")))))))
    (multiple-value-bind (new-root path) (c:tree-move root '(1 0) '(0)
                                                      :axis :horizontal)
      (is (equal '(:h (:leaf "a") (:leaf "b") (:leaf "c")) (shape new-root))
          "b joins the horizontal root and the emptied vertical split collapses")
      (is (equal "b" (app-at new-root path))))))

(test move-refuses-into-its-own-descendant
  (let ((root (c:make-split :horizontal
                            (list (c:make-split :vertical (list (leaf-with "a")
                                                                (leaf-with "b")))
                                  (leaf-with "c")))))
    (signals error (c:tree-move root '(0) '(0 1)))))

(test move-to-self-is-a-no-op
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")))))
    (multiple-value-bind (new-root) (c:tree-move root '(0) '(0))
      (is (equal '(:h (:leaf "a") (:leaf "b")) (shape new-root))))))

(test transplant-into-another-container
  ;; "Send this window to workspace 1" is this, and nothing else.
  (let ((root (c:make-stack
               (list (c:make-split :horizontal (list (leaf-with "a")
                                                     (leaf-with "b")))
                     (c:make-split :horizontal (list (leaf-with "z")))))))
    (multiple-value-bind (new-root path) (c:tree-transplant root '(0 0) '(1) 0)
      (is (equal '(:stack 0 (:leaf "b") (:h (:leaf "a") (:leaf "z")))
                 (shape new-root)))
      (is (equal "a" (app-at new-root path))))))

;;; ------------------------------------------------------------ traversal

(test node-windows-and-leaves
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a") (c:make-leaf) (leaf-with "b")))))
    (is (= 3 (length (c:node-leaves root))))
    (is (equal '("a" "b") (mapcar #'c:window-app-id (c:node-windows root)))
        "an empty pane is a leaf but not a window")))

(test node-empty-p-distinguishes-panes-from-debris
  (is-true (c:node-empty-p (c:make-leaf)))
  (is-false (c:node-empty-p (leaf-with "a")))
  (is-true (c:node-empty-p (c:make-split :horizontal (leaves 3))))
  (is-false (c:node-empty-p (c:make-split :horizontal (list (leaf-with "a"))))))

(test copy-node-shares-windows-but-not-structure
  (let* ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
         (copy (c:copy-node root)))
    (is (equal (shape root) (shape copy)))
    (is (not (eq root copy)))
    (is (eq (c:leaf-window (first (c:children root)))
            (c:leaf-window (first (c:children copy))))
        "there is only ever one of a window")))

(test copy-node-keeps-weights-and-selection
  ;; THE ORDER OF THE COPY-NODE-SLOTS METHODS IS LOAD-BEARING.  The CONTAINER
  ;; method inserts the children, and inserting a child into a split
  ;; *recomputes its weights* -- so a SPLIT method that ran before it would
  ;; have its weights immediately overwritten with the mean.  PROGN combination
  ;; is declared :MOST-SPECIFIC-LAST precisely so base-class state is filled in
  ;; first and the subclass gets the last word.
  (let* ((split (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b")
                                                (leaf-with "c"))
                              '(5 2 1)))
         (copy (c:copy-node split)))
    (is (equal '(5 2 1) (c:weights copy))
        "a copied split keeps its proportions rather than being re-equalized"))
  (let* ((stack (c:make-stack (leaves 4) 2))
         (copy (c:copy-node stack)))
    (is (= 2 (c:container-selection copy))
        "and a copied stack is still showing the same child")))

;;; ------------------------- a container kind addressed by something else
;;;
;;; The RING in test-surface.lisp subclasses SEQUENTIAL-CONTAINER, so it is
;;; addressed by integer index like everything in the core.  This one is not:
;;; it subclasses CONTAINER *directly* and is addressed by keyword, which is
;;; the shape the lattice's GRID has and the shape every one of the core's
;;; TYPECASEs silently mishandled.
;;;
;;; Six methods, exactly as the protocol advertises.  Nothing else.

(defclass pigeonhole (c:container)
  ((slots :initform (make-hash-table :test #'eq) :accessor pigeonhole-slots))
  (:documentation
   "A container addressed by keyword, from outside the core.

Deliberately as unlike a SPLIT as the protocol permits: sparse, unordered until
asked, and addressed by something that is not a number.  If the core's generic
operations are really generic, this participates in all of them without a
single method beyond the six below."))

(defmethod c:container-addresses ((p pigeonhole))
  (sort (loop for key being the hash-keys of (pigeonhole-slots p) collect key)
        #'string< :key #'symbol-name))

(defmethod c:child-at ((p pigeonhole) address)
  (and (symbolp address) (gethash address (pigeonhole-slots p))))

(defmethod (setf c:child-at) (node (p pigeonhole) address)
  (setf (gethash address (pigeonhole-slots p)) node))

(defmethod c:insert-child ((p pigeonhole) address node)
  (setf (gethash address (pigeonhole-slots p)) node)
  p)

(defmethod c:remove-child ((p pigeonhole) address)
  (let ((node (gethash address (pigeonhole-slots p))))
    (remhash address (pigeonhole-slots p))
    node))

(defmethod c:container-count ((p pigeonhole))
  (hash-table-count (pigeonhole-slots p)))

(defun a-pigeonhole ()
  "A pigeonhole holding two windows at keyword addresses."
  (let ((p (make-instance 'pigeonhole)))
    (c:insert-child p :first (leaf-with "x"))
    (c:insert-child p :second (leaf-with "y"))
    p))

(test copy-node-is-total-over-container-kinds
  ;; THE DEEPEST BUG THIS SUITE COVERS, and it was invisible for the life of
  ;; the project.  COPY-NODE was a DEFUN dispatching by TYPECASE on the three
  ;; core classes, so a kind that subclassed CONTAINER directly matched no
  ;; clause and came back carrying props and a label and *nothing else* -- no
  ;; children at all.  Silently.  No error, no warning.
  ;;
  ;; It was exported, documented and tested -- against the core kinds only, so
  ;; the suite actively certified the broken function.  This is that hole.
  ;;
  ;; Note that PIGEONHOLE defines no COPY-NODE-SLOTS method of its own: the
  ;; whole point is that walking CONTAINER-ADDRESSES and INSERT-CHILDing a copy
  ;; is correct for a kind nobody has heard of.
  (let* ((bag (a-pigeonhole))
         (copy (c:copy-node bag)))
    (is (= 2 (c:container-count copy))
        "every child came across, through the protocol and nothing else")
    (is (equal '("x" "y")
               (mapcar #'c:window-app-id (c:node-windows copy)))
        "and landed at the right addresses")
    (is (not (eq (c:child-at bag :first) (c:child-at copy :first)))
        "structurally copied, not shared")
    (is (eq (c:leaf-window (c:child-at bag :first))
            (c:leaf-window (c:child-at copy :first)))
        "but the windows in it are the same windows")))

(test a-foreign-kind-answers-the-whole-protocol
  ;; The generics the core grew so that it would stop asking TYPEP.
  (let ((bag (a-pigeonhole)))
    (is-false (c:container-alternatives-p bag)
              "it shows everything at once, so it is not a workspace list")
    (is-false (c:container-splits-along-p bag :horizontal)
              "and it does not divide space, so a split must not join it")
    (is-false (c:node-empty-p bag))
    (is-true (c:node-empty-p (make-instance 'pigeonhole)))
    (is (equal (c:node-signature bag) (c:node-signature bag))
        "it signs consistently, so undo can tell whether it changed")
    (is (equal '(:second) (c:node-path-to bag (c:child-at bag :second)))
        "and a path into it is a path like any other")))

(test node-signature-notices-what-undo-needs-to-notice
  ;; Undo keeps a snapshot only when the tree actually changed, and this is the
  ;; test of "actually changed".  A focus move must not register; a resize must.
  (let* ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
         (before (c:node-signature root)))
    (is (equal before (c:node-signature root))
        "the same tree signs the same twice")
    (c:adjust-weight root 0 1/4)
    (is (not (equal before (c:node-signature root)))
        "a resize is a change, so it is undoable"))
  (let* ((stack (c:make-stack (leaves 3) 0))
         (before (c:node-signature stack)))
    (setf (c:container-selection stack) 2)
    (is (not (equal before (c:node-signature stack)))
        "and so is switching a tab")))

(test props-are-per-node
  (let ((n (c:make-leaf)))
    (is (null (c:prop n :missing)))
    (is (eq :fallback (c:prop n :missing :fallback)))
    (setf (c:prop n :my-extension/pinned) t)
    (is-true (c:prop n :my-extension/pinned))))

(test the-collapse-predicate-is-consulted-per-container
  ;; i3 keeps a container that is down to one child; Emacs dissolves it.  Both
  ;; are reasonable, so it is a preference — and the predicate has to be asked
  ;; about *each* container rather than once for the whole operation, or a
  ;; workspace stack's answer would decide a split's fate.
  (let ((root (c:make-stack
               (list (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))))))
    (multiple-value-bind (removed new-root)
        (c:tree-remove-at root '(0 1) :simplify (lambda (node)
                                                  (declare (ignore node))
                                                  nil))
      (declare (ignore removed))
      (is (equal '(:stack 0 (:h (:leaf "a"))) (shape new-root))
          "the split was kept, as i3 would keep it")))
  (let ((root (c:make-stack
               (list (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))))))
    (multiple-value-bind (removed new-root)
        (c:tree-remove-at root '(0 1) :simplify (lambda (node)
                                                  (typep node 'c:split)))
      (declare (ignore removed))
      (is (equal '(:stack 0 (:leaf "a")) (shape new-root))
          "and dissolved when the predicate allows it"))))

;;; ------------------------------------------------- what a walk has to cost

(test the-first-address-of-a-sequence-is-known-without-building-the-list
  "DEFAULT-ADDRESS is REPAIR-PATH's and FIRST-LEAF-PATH's descent step, which
makes it the hottest thing in the model, and the inherited method was (FIRST
(CONTAINER-ADDRESSES C)) — the whole index list consed to take the head off it.
The answer for a sequence is 0 and the shape of the question does not change."
  (let ((split (c:make-split :horizontal (leaves 3)))
        (empty (c:make-split :horizontal '())))
    (is (eql 0 (c:default-address split)))
    (is (eql (first (c:container-addresses split)) (c:default-address split))
        "the same answer the inherited method gave, which is the point")
    (is (null (c:default-address empty))
        "and nothing to descend into is still NIL rather than 0")))

(test counting-windows-does-not-build-three-lists-to-do-it
  "The status line asks how many windows there are on every draw, and
(LENGTH (NODE-WINDOWS ROOT)) is the leaves, then a MAPCAR, then a REMOVE — three
lists over every leaf in the world to produce an integer."
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a")
                                  (c:make-leaf)
                                  (c:make-stack (list (leaf-with "b")
                                                      (leaf-with "c")))))))
    (is (= 3 (c:node-window-count root)))
    (is (= (length (c:node-windows root)) (c:node-window-count root))
        "the same number the list form gives, which is the only thing that
matters about it")
    (is (= 0 (c:node-window-count (c:make-leaf)))
        "an empty pane holds none, and a leaf is not a container")))
