;;;; tests/test-container.lisp --- The *other* extension surface.
;;;;
;;;; tests/test-surface.lisp tests the policy protocol as a product: a DEFMETHOD
;;;; from outside changes behaviour, live, with no core edit.  This file asks
;;;; the same questions of the container protocol, which had nobody asking them.
;;;;
;;;; The container protocol is what a new *container kind* answers, as against
;;;; what a new *policy* answers.  It had no membership test, no generated
;;;; document and no documentation gate — so its failures were the ones only
;;;; absence of a check produces: a generic exported and never defined, two
;;;; members declared in the wrong package, and no statement anywhere of how
;;;; many members there are.  A grid written against the source alone answered
;;;; six of them, loaded without complaint, and broke on copy, on save and on
;;;; focus repair.
;;;;
;;;; Each test here is either that class of bug, caught, or an extension
;;;; somebody might really write.

(in-package #:latticewm/tests)
(in-suite container)

;;; ------------------------------------------------- the protocol as a list

(test the-container-protocol-can-say-what-it-is
  "There is a list, it is not empty, and it contains the members that are
actually missed.

The six named here are the six that a kind can skip while still loading, in the
order they were really skipped.  Every one of them has a default that is right
for a dense integer-addressed container and wrong for anything else, which is
exactly why skipping them is silent."
  (let ((members (c:container-protocol-generics)))
    (is (< 10 (length members))
        "the container protocol has ~d members" (length members))
    (dolist (name '(c:container-addresses c:child-at c:insert-child
                    c:remove-child c:container-count c:address-equal
                    c:default-address c:simplify-node c:node-signature
                    c:copy-node c:copy-node-slots c:node-empty-p
                    c:container-alternatives-p c:container-selection
                    c:container-splits-along-p
                    c:serialize-node c:deserialize-node))
      (is (member name members :test #'equal)
          "~a is part of the container protocol and is not on the list" name))
    ;; The two whose reader is on the list and whose writer is the half a kind
    ;; forgets.  Setting a child and setting the selection are separate
    ;; obligations, and a document that listed only the readers would have said
    ;; a sparse grid was complete when it could not be written to.
    (dolist (name '((setf c:child-at) (setf c:container-selection)))
      (is (member name members :test #'equal)
          "~a is a member in its own right" name))))

(test the-container-protocol-is-decisions-and-not-slots
  "A slot accessor is state; the surface is decisions.

This is the bug POLICY-GENERICS records learning the hard way — ten CLOS
accessors became extension-surface entries and the document's own printed
contract became false.  The container protocol has *more* accessors on the node
lineage than the policy one has on policies, so the same mistake was one
structural test away from being made twice."
  (dolist (name '(c:children c:weights c:split-axis c:stack-selected
                  c:props c:node-label c:leaf-window c:node-id))
    (is (typep (fdefinition name) 'generic-function)
        "~a really is a generic, so this test is not vacuous" name)
    (is (not (c:container-protocol-p name))
        "~a is a slot accessor, not something a container kind answers" name)
    (is (not (member name (c:container-protocol-generics) :test #'equal))
        "~a must not be on the container protocol" name))
  ;; And something that genuinely is on it still is.
  (is (c:container-protocol-p 'c:container-addresses))
  (is (c:container-protocol-p '(setf c:child-at))))

(test every-container-protocol-generic-is-documented
  "Gate 2, as a test as well as a build step.

The gate asked this of the policy protocol only, for the life of the project."
  (let ((undocumented (c:undocumented-container-generics)))
    (is (null undocumented)
        "undocumented container protocol generics: ~{~a~^ ~}" undocumented)))

(test every-symbol-the-core-exports-names-something
  "REPLACE-CHILD WAS EXPORTED FROM LATTICEWM/CORE AND DEFINED NOWHERE.

It sat in package.lisp under the comment `the container protocol', for the whole
life of the project, on a surface an extension author is told to program
against.  A DEFGENERIC that does not exist.

Nothing could see it.  Gate 2 walked the policy generics.  Gate 10 checks that
the core dispatches *through* the protocol and says nothing about what the
protocol contains.  The unit suite tests the functions that exist.  The
generated document is built from the live image, so a name with nothing behind
it simply does not appear — which is why the fix is this test rather than the
document: a document cannot report an absence it cannot see.

The check generalises past the one bug, which is the point: every exported
symbol must name a function, a variable, a class, or a type.  An export that
names none of those is a promise with nothing behind it."
  (let ((empty '()))
    (do-external-symbols (symbol '#:latticewm/core)
      (unless (or (fboundp symbol)
                  (fboundp (list 'setf symbol))
                  (boundp symbol)
                  (find-class symbol nil)
                  ;; A DEFTYPE or a DEFSTRUCT predicate-less type name.
                  (ignore-errors (subtypep symbol symbol)))
        (push symbol empty)))
    (is (null empty)
        "LATTICEWM/CORE exports ~d symbol~:p that name nothing: ~{~a~^ ~}"
        (length empty) empty)))

(test the-container-surface-prints-every-member
  "The document is generated from the image, so this is the only way it can be
wrong: by not being printed at all."
  (let ((text (with-output-to-string (out) (c:print-container-surface out))))
    (is (search "LatticeWM container protocol" text))
    (is (not (search "UNDOCUMENTED" text))
        "the generated container surface flags something as undocumented")
    (dolist (name (c:container-protocol-generics))
      (let ((printed (string-downcase
                      (symbol-name (if (consp name) (second name) name)))))
        (is (search printed text)
            "~a is on the protocol and not in the printed document" name)))))

;;; -------------------------------------- a container kind from outside

(defclass strip (c:container)
  ((cells :initform (make-hash-table :test #'eql) :accessor strip-cells)
   (scroll :initform 0 :accessor strip-scroll))
  (:documentation
   "A container kind defined in a test file, answering the whole protocol.

Deliberately not a grid and not in lattice/: the lattice is one extension by the
author of the core, and a test that only exercises it cannot tell `the protocol
is complete' from `the protocol is whatever the lattice needed'.  A STRIP is
sparse, integer-addressed, keeps state of its own (SCROLL), and is written the
way the generated document says to write one -- which is not a coincidence: the
container protocol's own docstrings name `a strip that scrolls' twice as the
kind whose state the defaults cannot guess."))

(defmethod c:container-addresses ((r strip))
  (sort (loop for k being the hash-keys of (strip-cells r) collect k) #'<))

(defmethod c:child-at ((r strip) address)
  (gethash address (strip-cells r)))

(defmethod (setf c:child-at) (node (r strip) address)
  (setf (gethash address (strip-cells r)) node))

(defmethod c:insert-child ((r strip) address node)
  (setf (gethash address (strip-cells r)) node)
  r)

(defmethod c:remove-child ((r strip) address)
  (let ((kid (gethash address (strip-cells r))))
    (remhash address (strip-cells r))
    kid))

(defmethod c:simplify-node ((r strip)) r)

(defmethod c:node-signature ((r strip))
  (list* :strip (strip-scroll r) (call-next-method)))

(defmethod c:copy-node-slots progn ((new strip) (old strip))
  (setf (strip-scroll new) (strip-scroll old)))

(defmethod r:serialize-node ((r strip))
  (list :test/strip :scroll (strip-scroll r)
                   :addresses (c:container-addresses r)
                   :children (r:serialize-children r)))

(defmethod r:deserialize-node ((tag (eql :test/strip)) plist index)
  (let ((r (make-instance 'strip)))
    (setf (strip-scroll r) (getf plist :scroll))
    (loop for address in (getf plist :addresses)
          for form in (getf plist :children)
          do (setf (c:child-at r address) (r:read-node form index)))
    r))

(defun strip-of (&rest pairs)
  "A STRIP holding PAIRS, given as address then node, address then node."
  (let ((r (make-instance 'strip)))
    (loop for (address node) on pairs by #'cddr
          do (c:insert-child r address node))
    r))

(test a-container-kind-from-outside-survives-every-total-operation
  "The six that are silently skipped, exercised against a kind the core has
never heard of.

This is the test that would have failed for the grid a stranger wrote: it
answered CONTAINER-ADDRESSES, CHILD-AT and the rest of the obvious six, and
every assertion below is one of the ones it did not answer."
  (let ((r (strip-of 0 (leaf-with "a") 7 (leaf-with "b") 12 (c:make-leaf))))
    ;; Traversal and emptiness, which every verb in the program is built on.
    (is (equal '(0 7 12) (c:container-addresses r)))
    (is (= 3 (c:container-count r)))
    (is (not (c:node-empty-p r)))
    (is (c:node-empty-p (strip-of 3 (c:make-leaf))))
    ;; Sparseness survives a copy, and so does the kind's own state.
    (setf (strip-scroll r) 4)
    (let ((copy (c:copy-node r)))
      (is (typep copy 'strip))
      (is (not (eq copy r)))
      (is (equal '(0 7 12) (c:container-addresses copy))
          "COPY-NODE dropped a sparse container's addresses")
      (is (= 4 (strip-scroll copy))
          "COPY-NODE-SLOTS did not carry the kind's own state")
      (is (equal "a" (c:window-app-id (c:leaf-window (c:child-at copy 0))))
          "the copy's children are not the originals' contents")
      (is (not (eq (c:child-at copy 0) (c:child-at r 0)))
          "COPY-NODE must copy children, not share them"))
    ;; Undo tells a structural change from a focus move by this and nothing
    ;; else, so a kind whose state is invisible here has an undo that skips.
    (let ((before (c:node-signature r)))
      (setf (strip-scroll r) 5)
      (is (not (equal before (c:node-signature r)))
          "NODE-SIGNATURE ignored the kind's own state, so undo would skip it")
      (setf (strip-scroll r) 4)
      (is (equal before (c:node-signature r))))
    ;; Focus repair must land somewhere real.
    (is (eql 0 (c:default-address r)))
    (is (equal '(0) (c:repair-path r '(99))))))

(test a-container-kind-from-outside-round-trips-through-the-state-file
  "SERIALIZE-NODE and DESERIALIZE-NODE are the half of the protocol whose
failure mode is silent data loss on restart, and they were the half no document
named — they were declared in LATTICEWM/RUNTIME rather than with the protocol."
  (let* ((window (win "held"))
         (r (strip-of 2 (c:make-leaf window) 9 (c:make-leaf)))
         (index (make-hash-table :test #'equal)))
    (setf (c:window-identifier window) "id-held"
          (strip-scroll r) 3)
    (let* ((form (r:serialize-node r))
           (back (r:read-node form index)))
      ;; Nothing in the index, so the window is gone and its pane is empty —
      ;; which is the ordinary case after a reboot and must not lose the shape.
      (is (typep back 'strip))
      (is (= 3 (strip-scroll back)))
      (is (equal '(2 9) (c:container-addresses back)))
      ;; And with the window present, it comes back in the same cell.
      (setf (gethash "id-held" index) window)
      (let ((restored (r:read-node form index)))
        (is (eq window (c:leaf-window (c:child-at restored 2)))
            "the window did not come back where it was"))))
  ;; A kind this image does not have a method for keeps its windows.  The
  ;; forward-compatibility path, asserted for a tag nobody has defined.
  (let* ((window (win "orphan"))
         (index (make-hash-table :test #'equal)))
    (setf (c:window-identifier window) "id-orphan"
          (gethash "id-orphan" index) window)
    (let ((back (r:read-node '(:nobody/knows :children ((:leaf :window "id-orphan")))
                             index)))
      (is (member window (c:node-windows back))
          "an unknown container tag threw the windows away"))))

;;; --------------------------------------------- what is not in the tree

(test tags-and-named-scratchpads-survive-the-state-file
  "runtime/tags.lisp opens by arguing that a dead slot in a *persisted* state
file is worse than one in memory.  WINDOW-TAGS had never been in that file.

A named scratchpad is a minimized window — out of the tree by construction — so
this could never have been fixed in SERIALIZE-NODE.  Put a terminal away under
the name `music', restart, and it came back as an anonymous window in the tree:
the name gone, the putting away undone, and the one command for getting it back
keyed on the name."
  (let* ((world (c:make-world))
         (kept (win "music"))
         (plain (win "editor"))
         (index (make-hash-table :test #'equal)))
    (setf (c:window-identifier kept) "id-music"
          (c:window-identifier plain) "id-editor"
          (c:window-tags kept) '(:music :audio)
          (c:prop kept :scratchpad) :music
          (c:window-minimized-p kept) t)
    (let ((facts (r:window-facts (list kept plain))))
      (is (= 1 (length facts))
          "a window with nothing to remember should not be written out")
      ;; A fresh session: the windows are back, and know nothing about
      ;; themselves.  This is exactly the state after a window manager restart.
      (let ((fresh-kept (win "music")))
        (setf (c:window-identifier fresh-kept) "id-music"
              (gethash "id-music" index) fresh-kept)
        (r:restore-window-facts world facts index)
        (is (equal '(:music :audio) (c:window-tags fresh-kept))
            "the tags did not survive")
        (is (eql :music (c:prop fresh-kept :scratchpad))
            "the scratchpad name did not survive")
        (is (c:window-minimized-p fresh-kept)
            "a window that was put away came back on screen")
        (is (member fresh-kept (c:world-scratchpad world))
            "the restore did not go through ON-MINIMIZE")))))

(test restoring-window-facts-unions-tags-rather-than-replacing-them
  "The file describes the world as it was a moment ago, not as it must be — so
a rule that tagged the window while it was reopening keeps its tag."
  (let* ((world (c:make-world))
         (window (win "term"))
         (index (make-hash-table :test #'equal)))
    (setf (c:window-identifier window) "id-term"
          (c:window-tags window) '(:from-a-rule)
          (gethash "id-term" index) window)
    (r:restore-window-facts world '(("id-term" :tags (:from-the-file))) index)
    (is (member :from-a-rule (c:window-tags window)))
    (is (member :from-the-file (c:window-tags window))))
  ;; And a hand-edited file does not take the session down with it.
  (let ((world (c:make-world))
        (index (make-hash-table :test #'equal)))
    (finishes (r:restore-window-facts world '(:not-a-row ("nobody" :tags (:x)))
                                      index))))
