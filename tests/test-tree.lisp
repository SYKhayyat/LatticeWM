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
