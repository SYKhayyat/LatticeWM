;;;; tests/test-lifecycle.lisp --- Spawning, closing, floating, minimizing.

(in-package #:latticewm/tests)
(in-suite lifecycle)

(defun fresh-world ()
  "A world in its shipped starting shape: one workspace, one empty pane."
  (c:make-world))

(test a-fresh-world-is-one-empty-pane
  (let ((world (fresh-world)))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world))))
    (is (equal '(0) (c:world-cursor world)))
    (is (null (c:world-focus-window world)))))

(test first-window-fills-the-empty-pane
  ;; An empty pane exists because the user made a place for something.  Putting
  ;; the next thing there is the only reading that respects the gesture.
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "emacs"))
    (is (equal '(:stack 0 (:leaf "emacs")) (shape (c:world-root world))))
    (is (equal "emacs" (c:window-app-id (c:world-focus-window world))))))

(test second-window-splits-the-focused-pane
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "emacs"))
    (p:on-window-open pol world (win "term"))
    (is (equal '(:stack 0 (:h (:leaf "emacs") (:leaf "term")))
               (shape (c:world-root world))))
    (is (equal "term" (c:window-app-id (c:world-focus-window world)))
        "and focus followed the new window")))

(test three-windows-stay-one-split
  (let ((world (fresh-world)) (pol (policy))
        (p:*split-axis* :horizontal))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    (is (equal '(:stack 0 (:h (:leaf "a") (:leaf "b") (:leaf "c")))
               (shape (c:world-root world)))
        "n-ary splits keep three side-by-side windows flat")))

(test split-axis-longer-alternates
  (let ((world (fresh-world)) (pol (policy)))
    ;; A wide output makes the first cut horizontal and the second vertical.
    (push (make-instance 'c:output :rect (c:make-rect 0 0 1000 500))
          (c:world-outputs world))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (eq :horizontal
            (c:split-axis (c:resolve-path (c:world-root world) '(0)))))))

(test focus-new-windows-can-be-turned-off
  (let ((world (fresh-world)) (pol (policy))
        (p:*focus-new-windows* nil))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (equal "a" (c:window-app-id (c:world-focus-window world))))))

(test dialogs-float-and-are-not-tiled
  (let* ((world (fresh-world)) (pol (policy))
         (parent (win "gimp"))
         (dialog (win "gimp")))
    (setf (c:window-parent-window dialog) parent)
    (p:on-window-open pol world parent)
    (is (null (p:on-window-open pol world dialog))
        "a floated window returns no tiled path")
    (is-true (c:window-floating-p dialog))
    (is (equal '(:stack 0 (:leaf "gimp")) (shape (c:world-root world)))
        "and never entered the tree")))

(test a-fixed-size-window-floats
  (let ((world (fresh-world)) (pol (policy)) (w (win "splash")))
    (setf (c:window-min-width w) 400 (c:window-max-width w) 400
          (c:window-min-height w) 300 (c:window-max-height w) 300)
    (is-true (p:should-float-p pol w))
    (p:on-window-open pol world w)
    (is-true (c:window-floating-p w))))

(test floating-can-be-turned-off-wholesale
  (let ((pol (policy)) (p:*float-dialogs* nil)
        (dialog (win "gimp")))
    (setf (c:window-parent-window dialog) (win "gimp"))
    (is-false (p:should-float-p pol dialog))))

(test closing-collapses-and-the-sibling-grows
  (let ((world (fresh-world)) (pol (policy)) (a (win "a")))
    (p:on-window-open pol world a)
    (p:on-window-open pol world (win "b"))
    (let ((path (c:node-path-to (c:world-root world)
                                (c:leaf-holding (c:world-root world) a))))
      (p:on-window-close pol world a path))
    (is (equal '(:stack 0 (:leaf "b")) (shape (c:world-root world))))
    (is (equal "b" (c:window-app-id (c:world-focus-window world))))))

(test closing-the-last-window-leaves-an-empty-pane-not-a-hole
  (let ((world (fresh-world)) (pol (policy)) (a (win "a")))
    (p:on-window-open pol world a)
    (p:on-window-close pol world a '(0))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world)))
        "the workspace survives as a place you can spawn into")
    (is (equal '(0) (c:world-cursor world)))))

(test focus-after-close-stays-put-by-default
  ;; The governing property: nothing moves the viewport except the user.
  (let ((world (fresh-world)) (pol (policy)))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    (let* ((root (c:world-root world))
           (b (c:leaf-window (c:resolve-path root '(0 1)))))
      (p:on-window-close pol world b '(0 1))
      (is (equal "c" (c:window-app-id (c:world-focus-window world)))
          "landed on the pane that took the closed one's place"))))

(test focus-after-close-mru-is-one-option-away
  (let ((world (fresh-world)) (pol (policy))
        (p:*focus-after-close* :mru))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    ;; Visit "a" so it is the most recent survivor, then come back and close "c".
    (p:jump-cursor pol world '(0 0))
    (p:jump-cursor pol world '(0 2))
    (let ((c-window (c:leaf-window (c:resolve-path (c:world-root world) '(0 2)))))
      (p:on-window-close pol world c-window '(0 2))
      (is (equal "a" (c:window-app-id (c:world-focus-window world)))))))

(test minimize-takes-the-window-out-of-the-tree
  ;; The stated requirement, honoured literally: the remaining windows retile
  ;; without it.  Not "hide it somewhere".
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (let ((b (c:leaf-window (c:resolve-path (c:world-root world) '(0 1)))))
      (p:on-minimize pol world b)
      (is (equal '(:stack 0 (:leaf "a")) (shape (c:world-root world))))
      (is (equal (list b) (c:world-scratchpad world)))
      (is-true (c:window-minimized-p b))
      (is (equal '(0 1) (c:window-home-path b)) "it remembered where it was"))))

(test restore-returns-to-the-remembered-slot-when-it-survives
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    ;; Clear the pane rather than closing it, so the slot outlives the window.
    (let* ((root (c:world-root world))
           (leaf (c:resolve-path root '(0 1)))
           (b (c:leaf-window leaf)))
      (setf (c:window-home-path b) '(0 1)
            (c:leaf-window leaf) nil)
      (push b (c:world-scratchpad world))
      (p:on-restore pol world b)
      (is (equal '(:stack 0 (:h (:leaf "a") (:leaf "b")))
                 (shape (c:world-root world))))
      (is (null (c:world-scratchpad world))))))

(test restore-falls-back-to-the-cursor
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (let ((b (c:leaf-window (c:resolve-path (c:world-root world) '(0 1)))))
      (p:on-minimize pol world b)
      (p:on-restore pol world b)
      (is (= 2 (length (c:node-windows (c:world-root world)))))
      (is (equal "b" (c:window-app-id (c:world-focus-window world)))))))

(defclass ruled-policy (p:conventional-policy) ()
  (:documentation "A policy with a declarative window rule.  The escape hatch
for people who do not want to write methods — implemented, of course, as a
method."))

(defmethod p:window-rule-for ((pol ruled-policy) (w c:window))
  (when (equal (c:window-app-id w) "pinentry") (list :float t)))

(test window-rules-override-the-computed-guess
  (let ((world (fresh-world)) (pol (make-instance 'ruled-policy)))
    (p:on-window-open pol world (win "pinentry"))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world)))
        "the rule floated it, so the tree is untouched")))

(test empty-pane-keys-only-fire-on-an-empty-pane
  (let ((world (fresh-world)) (pol (policy)))
    (is (equal "terminal" (p:key-unbound pol world #\t))
        "the cursor starts on an empty pane")
    (p:on-window-open pol world (win "emacs"))
    (is (null (p:key-unbound pol world #\t))
        "and an unbound key is simply unbound once something is there")))

(test spawn-mode-stack-makes-tabs
  (let ((world (fresh-world)) (pol (policy)) (p:*spawn-mode* :stack))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (equal '(:stack 0 (:stack 1 (:leaf "a") (:leaf "b")))
               (shape (c:world-root world))))))

(test spawn-mode-fill-first-finds-a-hole
  (let ((world (fresh-world)) (pol (policy)) (p:*spawn-mode* :fill-first))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    ;; Clear the first pane, then spawn: it should land in the hole.
    (setf (c:leaf-window (c:resolve-path (c:world-root world) '(0 0))) nil)
    (p:on-window-open pol world (win "c"))
    (is (equal '(:stack 0 (:h (:leaf "c") (:leaf "b")))
               (shape (c:world-root world))))))

;;; ------------------------------------------------------------- layout

(test layout-divides-by-weight
  (let* ((pol (policy))
         (root (c:make-split :horizontal
                             (list (leaf-with "a") (leaf-with "b")) '(1 3)))
         (placements (p:layout-node pol root (c:make-rect 0 0 400 100))))
    (destructuring-bind (whole a b) placements
      (declare (ignore whole))
      (is (= 100 (c:rect-w (third a))))
      (is (= 300 (c:rect-w (third b))))
      (is-true (fourth a)))))

(test layout-hides-unselected-tabs-but-still-visits-them
  ;; River shows a window unless it is explicitly hidden, so a hidden tab that
  ;; the layout never visited would be drawn on top of everything.
  (let* ((pol (policy))
         (root (c:make-stack (list (leaf-with "front") (leaf-with "back")) 0))
         (placements (p:layout-node pol root (c:make-rect 0 0 100 100))))
    (is (= 3 (length placements)) "both children were visited")
    (let ((back (find-if (lambda (pl)
                           (equal "back" (let ((w (and (typep (first pl) 'c:leaf)
                                                       (c:leaf-window (first pl)))))
                                           (and w (c:window-app-id w)))))
                         placements)))
      (is-false (fourth back) "and the hidden one is marked invisible"))))

(test layout-gaps-come-out-of-the-panes
  (let* ((pol (policy)) (p:*gaps* 10)
         (root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
         (placements (p:layout-node pol root (c:make-rect 0 0 100 100))))
    (is (= 45 (c:rect-w (third (second placements)))))))

(test window-dimensions-leave-room-for-the-border
  (let* ((pol (policy)) (p:*border-width* 3)
         (leaf (leaf-with "a")))
    (multiple-value-bind (w h) (p:window-dimensions pol leaf (c:make-rect 0 0 100 50))
      (is (= 94 w))
      (is (= 44 h)))))

(test gravity-centres-a-window-that-refused-its-size
  (let* ((pol (policy))
         (r (p:gravity pol (leaf-with "a") (c:make-rect 0 0 100 100) 40 40)))
    (is (= 30 (c:rect-x r)))
    (is (= 30 (c:rect-y r)))))

(test render-order-puts-floats-above-tiles
  (let* ((pol (policy))
         (tiled (leaf-with "tiled"))
         (floated (leaf-with "floated")))
    (setf (c:window-floating-p (c:leaf-window floated)) t)
    (let ((ordered (p:render-order pol (list (list floated '(1) nil t)
                                             (list tiled '(0) nil t)))))
      (is (eq tiled (first (first ordered))))
      (is (eq floated (first (second ordered)))))))
