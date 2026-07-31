;;;; tests/test-surface.lisp --- The extension surface, tested as a product.
;;;;
;;;; PLAN.org: "Nothing verifies that the decomposition is good.  Gate 2 checks
;;;; that generics have docstrings.  It cannot check that they are the right
;;;; generics."  That is true, and these tests do not fix it.  What they do fix
;;;; is the weaker claim that is nonetheless the one that fails first: that the
;;;; surface *works* — that a DEFMETHOD from outside actually changes
;;;; behaviour, live, with no core edit and no restart.
;;;;
;;;; Each test here is an extension somebody might really write.

(in-package #:latticewm/tests)
(in-suite surface)

(test every-policy-generic-is-documented
  ;; Gate 2, as a test as well as a build step.  An undocumented generic is an
  ;; extension point nobody can find.
  (let ((undocumented
          (remove-if (lambda (symbol) (documentation symbol 'function))
                     (p:policy-generics))))
    (is (null undocumented)
        "undocumented extension-surface generics: ~{~a~^ ~}" undocumented)))

(test the-surface-is-neither-ceremony-nor-monolith
  ;; PLAN.org: "If this list reaches thirty, the decomposition has gone wrong in
  ;; the direction of ceremony.  If it drops below ten, it has gone wrong in the
  ;; direction of a monolith."
  ;;
  ;; The ceiling has moved twice and both moves are recorded rather than
  ;; quietly applied, because a tripwire somebody steps over without comment is
  ;; not a tripwire.  Thirty to forty was the layout and lifecycle surface
  ;; settling; forty to forty-five was the interactive layer, which added
  ;; COMPLETE-CANDIDATES and ARGUMENT-TYPE-FOR.  Both earn it: completion style
  ;; is the most personal decision in the minibuffer, and a naming convention
  ;; that cannot see which command it is talking about cannot be corrected per
  ;; command.
  ;;
  ;; What would make this wrong is generics arriving one per feature.  If the
  ;; count passes forty-five, read the last five before raising it again.
  (let ((n (length (p:policy-generics))))
    (is (<= 10 n 45) "the extension surface has ~d generics" n)))

(test every-option-is-documented-and-has-a-default
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore value))
      (is (stringp documentation) "~a has no docstring" key)
      (is (not (eq default :unset)) "~a has no default" variable))))

(test every-option-is-reachable-from-a-config-file
  "A config file is read in LATTICEWM/USER, so an option whose symbol that
package cannot see is not merely inconvenient — it fails silently.

  (setf *terminal* \"alacritty\")

in an init.lisp interns a *new* symbol in LATTICEWM/USER, sets that, and
changes nothing whatsoever.  No error, no warning, and the starter config
this program writes named *TERMINAL* on exactly that line.  Twenty-four of
the thirty-one runtime options were in this state and every one of them was
documented, registered and listed by --list-options, so nothing else in the
build had any reason to complain.

The check is symbol identity rather than accessibility: an option that is
merely PRESENT in LATTICEWM/USER because something else interned it there is
still the wrong symbol."
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore key value default documentation))
      (let* ((name (symbol-name variable))
             (in-user (find-symbol name '#:latticewm/user)))
        (is (eq in-user variable)
            "~a is not reachable from a config file: LATTICEWM/USER sees ~
             ~:[nothing by that name~;a different symbol~].  Export it from ~a."
            variable in-user (package-name (symbol-package variable)))))))

(test options-round-trip
  (let ((before (p:option :gaps)))
    (unwind-protect
         (progn (setf (p:option :gaps) 12)
                (is (= 12 p:*gaps*) "the keyword and the variable are one thing"))
      (setf (p:option :gaps) before))))

;;; ---------------------------------------------------------- tier 1: a method

(defclass gapless-policy (p:conventional-policy) ()
  (:documentation "A policy that never leaves a gap.  Tier 1, one method."))

(defmethod p:gaps ((policy gapless-policy) container)
  (declare (ignore container))
  0)

(test tier-1-a-method-from-outside-changes-behaviour
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
        (p:*gaps* 20))
    (let ((wide (p:layout-node (make-instance 'p:conventional-policy) root
                               (c:make-rect 0 0 100 100)))
          (tight (p:layout-node (make-instance 'gapless-policy) root
                                (c:make-rect 0 0 100 100))))
      (is (= 40 (c:rect-w (third (second wide)))))
      (is (= 50 (c:rect-w (third (second tight))))
          "and no core file was edited to get it"))))

;;; ------------------------------------------------- tier 2: method plus state

(defclass remembering-policy (p:conventional-policy) ()
  (:documentation
   "Entry resolution with last-focus memory — the behaviour DESIGN D20
deliberately did *not* ship, added from outside with no core edit.

The state has nowhere to live except PROPS, which is exactly the case D20
predicted and exactly why every node carries one."))

(defmethod p:entry-address ((policy remembering-policy) (split c:split)
                            direction reference rects)
  (declare (ignore direction reference rects))
  (or (c:prop split :remembering/last) (call-next-method)))

(defmethod p:on-focus-change ((policy remembering-policy) world old new)
  (declare (ignore old))
  (let ((chain (c:resolve-chain (c:world-root world) new)))
    (loop for node in chain
          for address in new
          when (typep node 'c:split)
            do (setf (c:prop node :remembering/last) address)))
  (call-next-method))

(test tier-2-behaviour-plus-state-with-no-core-edit
  (let* ((policy (make-instance 'remembering-policy))
         (inner (c:make-split :horizontal (list (leaf-with "b") (leaf-with "c"))))
         (root (c:make-split :horizontal (list (leaf-with "a") inner)))
         (world (c:make-world :root root :cursor '(0))))
    ;; Visit "c", which teaches the inner split that 1 was last focused.
    (p:jump-cursor policy world '(1 1))
    (p:jump-cursor policy world '(0))
    (is (equal "c" (app-at root (p:find-motion-target policy root '(0) :right)))
        "memory sent us back to the pane we had been in")
    ;; And the shipped policy, on the same tree, still does the geometric thing.
    (is (equal "b" (app-at root (p:find-motion-target (policy) root '(0) :right))))))

;;; -------------------------------------------- tier 3: a whole new behaviour

(defclass monocle-policy (p:conventional-policy) ()
  (:documentation
   "Every split shows only its focused child, full size — the 'monocle' or
'maximized' layout, as a policy rather than as a mode.

A whole layout model in four lines, with zero core edits.  This is the shape
the lattice has to take, in miniature."))

(defmethod p:layout-children ((policy monocle-policy) (split c:split) rect)
  (let ((address (or (c:prop split :monocle/focus) 0)))
    (when (c:child-at split address)
      (list (cons address rect)))))

(test tier-3-a-new-layout-model-with-zero-core-edits
  (let* ((policy (make-instance 'monocle-policy))
         (root (c:make-split :horizontal
                             (list (leaf-with "a") (leaf-with "b") (leaf-with "c"))))
         (placements (p:layout-node policy root (c:make-rect 0 0 800 600))))
    (let ((visible (remove-if-not #'fourth (rest placements))))
      (is (= 1 (length visible)) "exactly one pane is drawn")
      (is (= 800 (c:rect-w (third (first visible)))) "and it has the whole rect"))
    (setf (c:prop root :monocle/focus) 2)
    (let* ((placements (p:layout-node policy root (c:make-rect 0 0 800 600)))
           (visible (remove-if-not #'fourth (rest placements))))
      (is (equal "c" (app-at root (second (first visible))))))))

;;; ------------------------------------------ a new container kind from outside

(defclass ring (c:sequential-container)
  ((offset :initform 0 :accessor ring-offset))
  (:documentation
   "A container whose motion wraps around — the last child's right neighbour is
the first.

The point of this test is not the ring.  It is that a *container kind the core
has never heard of* participates fully in motion, layout, surgery and focus
repair, with no edit to anything under src/.  If this test passes, the lattice
can be added the same way; if it could not, the container protocol was drawn in
the wrong place and D21's experiment has already failed."))

(defmethod p:layout-children ((policy p:conventional-policy) (r ring) rect)
  (mapcar #'cons (c:container-addresses r)
          (c:divide-rect rect :horizontal
                         (make-list (c:container-count r) :initial-element 1))))

(defmethod p:step-address ((policy p:conventional-policy) (r ring) address direction)
  (when (eq (c:direction-axis direction) :horizontal)
    (mod (+ address (c:direction-sign direction)) (c:container-count r))))

(defmethod p:entry-address ((policy p:conventional-policy) (r ring)
                            direction reference rects)
  (declare (ignore reference rects))
  (if (eq direction :left) (1- (c:container-count r)) 0))

(test a-container-kind-the-core-never-heard-of-works-everywhere
  (let* ((policy (policy))
         (r (make-instance 'ring
                           :children (list (leaf-with "a") (leaf-with "b")
                                           (leaf-with "c"))))
         (root (c:make-stack (list r))))
    ;; Motion, including the wrap that no core container does.
    (is (equal "b" (app-at root (p:find-motion-target policy root '(0 0) :right))))
    (is (equal "a" (app-at root (p:find-motion-target policy root '(0 2) :right)))
        "it wrapped, and motion never had to be told what a ring is")
    ;; Layout: the stack, the ring, and its three children.
    (let* ((placements (p:layout-node policy root (c:make-rect 0 0 300 100)))
           (leaves (remove-if-not (lambda (pl) (typep (first pl) 'c:leaf))
                                  placements)))
      (is (= 5 (length placements)))
      (is (= 3 (length leaves)))
      (is (every (lambda (pl) (= 100 (c:rect-w (third pl)))) leaves)
          "the ring divided its rectangle evenly, using core geometry it did
not have to reimplement"))
    ;; Surgery, and focus repair through it.
    (multiple-value-bind (removed new-root focus)
        (c:tree-remove-at root '(0 1) :focus-path '(0 2))
      (declare (ignore removed))
      (is (equal "c" (app-at new-root focus))
          "focus followed the node through a container the core cannot name"))))

;;; ------------------------------------------------------------ live surgery

(test redefining-a-method-takes-effect-immediately
  ;; The claim the whole language decision rests on, reduced to an assertion.
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
        (policy (policy)))
    (flet ((first-width ()
             (c:rect-w (third (second (p:layout-node policy root
                                                     (c:make-rect 0 0 100 100)))))))
      (is (= 50 (first-width)))
      (defmethod p:layout-children ((p p:conventional-policy) (s c:split) rect)
        (declare (ignore rect))
        (list (cons 0 (c:make-rect 0 0 77 100))))
      (unwind-protect
           (is (= 77 (first-width)) "no restart, no rebuild, no lost state")
        ;; Put the shipped method back.
        (remove-method #'p:layout-children
                       (find-method #'p:layout-children '()
                                    (list (find-class 'p:conventional-policy)
                                          (find-class 'c:split)
                                          (find-class 't))))
        (defmethod p:layout-children ((policy p:conventional-policy)
                                      (split c:split) rect)
          (mapcar #'cons (c:container-addresses split)
                  (c:divide-rect rect (c:split-axis split) (c:weights split)
                                 :gap (p:gaps policy split)))))
      (is (= 50 (first-width)) "and the restore took effect the same way"))))

(test adding-a-slot-to-a-live-class-migrates-existing-instances
  ;; SPIKE-WEEK0 §ext-3, which is what settled DEFCLASS over DEFSTRUCT for core
  ;; state.  An instance made *before* the redefinition gains the slot.
  (let ((before (c:make-leaf)))
    (eval '(defclass c:leaf (c:node)
             ((window :initarg :window :initform nil :accessor c:leaf-window)
              (test-only-slot :initform :migrated :accessor leaf-test-only-slot))))
    (unwind-protect
         (is (eq :migrated (funcall (read-from-string "latticewm/tests::leaf-test-only-slot")
                                    before))
             "an instance that predates the redefinition has the new slot")
      (eval '(defclass c:leaf (c:node)
               ((window :initarg :window :initform nil :accessor c:leaf-window)))))))
