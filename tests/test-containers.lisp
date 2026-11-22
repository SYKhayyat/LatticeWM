;;;; tests/test-containers.lisp --- Changing what a container is.
;;;;
;;;; #134: every paradigm is a way of arranging *some* container, so the seam
;;;; that changes one is what makes them combinable.  What is being tested is
;;;; the property that makes it safe to offer at all: a restyle moves nothing.
;;;; The children are the same objects, in the same order, in a different box,
;;;; and the cursor comes back to where it was.

(in-package #:latticewm/tests)
(in-suite containers)

;;; ------------------------------------------------------------- fixtures

(defmacro with-a-restylable-world ((count) &body body)
  "A fresh world with COUNT windows, the cursor on the first one."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     (dotimes (i ,count)
       (p:on-window-open p:*policy* r:*world*
                         (make-instance 'c:window
                                        :app-id (format nil "app~d" i))))
     ,@body))

(defun cursor-container (&optional (path (r:current-path)))
  (r:cursor-container path))

(defun window-names (node)
  "Every window app-id under NODE, in tree order."
  (let ((out '()))
    (labels ((walk (n)
               (etypecase n
                 (c:leaf (push (c:window-app-id (c:leaf-window n)) out))
                 (c:container (dotimes (i (c:container-count n))
                                (walk (c:child-at n i)))))))
      (walk node))
    (nreverse out)))

;;; ================================================================ the registry

(test the-kinds-every-program-has-are-registered
  (dolist (kind '(:split :stack :tabs))
    (is-true (r:container-kind-registered-p kind) kind)
    (is-true (stringp (r:container-kind-description kind)) kind))
  (is (not (r:container-kind-registered-p :no-such-kind)))
  (is (member :split (r:container-kind-names))))

(test registering-a-kind-twice-replaces-it
  "Loading an extension twice is what SHIFT+SUPER+C does all evening; the second
load must not be an error and must not leave two entries."
  (let ((before (length (r:container-kind-names))))
    (unwind-protect
         (progn
           (r:define-container-kind :test-kind (lambda (children &key argument)
                                                 (declare (ignore argument))
                                                 (c:make-stack children 0))
                                    :description "a stand-in")
           (is (member :test-kind (r:container-kind-names)))
           (r:define-container-kind :test-kind (lambda (children &key argument)
                                                 (declare (ignore argument))
                                                 (c:make-split :vertical children))
                                    :description "the second answer")
           (is (= (1+ before) (length (r:container-kind-names)))
               "one entry, not two")
           (is (string= "the second answer"
                        (r:container-kind-description :test-kind))))
      (remhash :test-kind r::*container-kinds*))))

;;; ================================================================ restyling

(test a-restyle-keeps-every-window-in-order
  (with-a-restylable-world (4)
    (let ((before (window-names (c:world-root r:*world*)))
          (container (cursor-container)))
      (is-true container "the cursor is in a container")
      (multiple-value-bind (replacement path)
          (r:restyle-container container :split)
        (declare (ignore path))
        (is-true replacement "a split was made")
        (is (typep replacement 'c:split))
        (is (equal before (window-names replacement))
            "the same windows, in the same order")
        (is (= 4 (length (window-names (c:world-root r:*world*))))
            "and the tree holds them once")))))

(test a-restyle-moves-nothing-out-of-the-tree
  "The children are the same OBJECTS, which is what makes the swap safe: a
window is not reopened, a leaf is not rebuilt, and nothing that hangs off a
child -- tags, a border colour, a saved place -- can be lost on the way."
  (with-a-restylable-world (3)
    (let* ((container (cursor-container))
           (kids (loop for address in (c:container-addresses container)
                       collect (c:child-at container address))))
      (let ((replacement (r:restyle-container container :stack)))
        (is (every (lambda (kid)
                     (eq kid (c:child-at replacement
                                         (position kid kids))))
                   kids)
            "the split now holds the very leaves the stack held")))))

(test restyling-a-half-of-a-split-leaves-the-other-half-alone
  "The hybrid case, which is the reason for the whole seam: two paradigms in one
workspace."
  (with-a-restylable-world (3)
    (let* ((world r:*world*)
           (root (c:world-root world)))
      ;; Put the first window aside in its own split, so the cursor's container
      ;; is a pane and not the workspace.
      (let* ((container (cursor-container))
             (path (c:node-path-to root container))
             (kids (loop for address in (c:container-addresses container)
                         collect (c:child-at container address)))
             (first (first kids))
             (rest (cdr kids)))
        (declare (ignore rest))
        (setf (c:world-root world)
              (c:tree-replace-at root path
                                 (c:make-split :horizontal
                                               (list first
                                                     (c:make-stack (cdr kids))))))
        (setf (c:world-cursor world)
              (c:repair-path (c:world-root world) (append path '(1 0))))
        ;; The cursor's container is now the inner stack holding the other two.
        (let ((inner (cursor-container)))
          (is-true inner)
          (r:restyle-container inner :split)
          (let* ((new-root (c:world-root world))
                 (outer (c:resolve-path new-root path)))
            (is (typep outer 'c:split) "the outer split is still a split")
            (is (= 2 (length (c:container-addresses outer))))
            (is (typep (c:child-at outer 1) 'c:split)
                "the half that was restyled is a split")
            (is (= 3 (length (window-names new-root)))
                "and no window was lost on the way")))))))

(test plain-tiling-builds-a-balanced-tree
  "Four children become two splits of two, not one split of four: the resize
verbs assume a binary tree, and eight equal slots that can each be resized
unequally is an arrangement no command can make sense of."
  (with-a-restylable-world (4)
    (let ((replacement (r:restyle-container (cursor-container) :split)))
      (is (typep replacement 'c:split))
      (is (= 2 (length (c:container-addresses replacement))))
      (is (every (lambda (kid) (typep kid 'c:split))
                 (loop for address in (c:container-addresses replacement)
                       collect (c:child-at replacement address)))
          "both children are splits")
      (is (not (eq (c:split-axis replacement)
                   (c:split-axis (c:child-at replacement 0))))
          "and the axes alternate, so it reads as a grid of two by two"))))

(test a-stack-shows-one
  (with-a-restylable-world (3)
    (let ((replacement (r:restyle-container (cursor-container) :stack)))
      (is (= 3 (c:container-count replacement)))
      (is (zerop (c:stack-selected replacement))
          "the first child is the one shown, which is the one that was leftmost"))))

;;; ================================================================ the verbs

(test the-restyle-command-answers-the-echo-area
  (with-a-restylable-world (3)
    (is (search "split" (r:restyle "split")))
    (is (typep (cursor-container) 'c:split))
    (is (search "stack" (r:restyle "stack")))
    (is (typep (cursor-container) 'c:stack))))

(test an-unknown-kind-changes-nothing-and-says-which
  (with-a-restylable-world (2)
    (let ((before (shape (c:world-root r:*world*))))
      (let ((message (r:restyle "banana")))
        (is (stringp message) "the echo area was told")
        (is (search "banana" message) "and by name"))
      (is (equal before (shape (c:world-root r:*world*)))
          "nothing was touched"))))

(test the-kind-is-given-a-word-for-itself
  "\"split vertical\" names the kind and its argument; the split kind reads
:VERTICAL and turns the tree on its side."
  (with-a-restylable-world (2)
    (is (stringp (r:restyle "split vertical")))
    (is (eq :vertical (c:split-axis (cursor-container))))))

(test a-trailing-number-reaches-the-kind-that-wants-one
  "The same reading for every kind, so no kind has to parse anything: the word
after the kind name arrives as a keyword or an integer."
  (is (= 3 (r:kind-argument "grid 3")))
  (is (eq :vertical (r:kind-argument "split vertical")))
  (is (null (r:kind-argument "stack")))
  (is (null (r:kind-argument "stack ")) "trailing space is not a word"))

(test my-container-names-the-class
  (with-a-restylable-world (2)
    (r:restyle "split")
    (let ((message (r:my-container)))
      (is (stringp message))
      (is (search "split" message)))))

(test container-kinds-lists-what-can-be-made
  (let ((page (r:container-kinds)))
    (is-true page "a page was drawn")
    (is-true (search "split" (format nil "~s" (cdr p:*help-visible*))))))

;;; ============================================================ the cursor

(test the-cursor-comes-back-to-a-real-place
  "Focus repair, not focus restoration: a restyle keeps the same children, so
the path that pointed at a leaf still points at it, and the cursor never lands
on a node that is not there."
  (with-a-restylable-world (3)
    (let ((before (copy-list (r:current-path))))
      (r:restyle "split")
      (is (c:path-valid-p (c:world-root r:*world*) (r:current-path))
          "the cursor names something in the tree")
      (is (equal before (r:current-path))
          "and it is the same place"))))
