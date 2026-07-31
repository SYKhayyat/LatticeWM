;;;; tests/test-motion.lisp --- Directional motion across every boundary.
;;;;
;;;; These are the tests that decide whether the container protocol was drawn
;;;; in the right place.  If motion needs a special case per container kind,
;;;; the abstraction is wrong and the lattice will not be addable from outside.

(in-package #:latticewm/tests)
(in-suite motion)

(defun target (root path direction)
  "Where motion DIRECTION from PATH lands, as an app-id, or NIL."
  (let ((to (p:find-motion-target (policy) root path direction)))
    (and to (app-at root to))))

(test motion-within-a-split
  (let ((root (c:make-split :horizontal
                            (list (leaf-with "a") (leaf-with "b") (leaf-with "c")))))
    (is (equal "b" (target root '(0) :right)))
    (is (equal "c" (target root '(1) :right)))
    (is (null (target root '(2) :right)) "the edge of the world is a no-op")
    (is (equal "b" (target root '(2) :left)))
    (is (null (target root '(1) :up))
        "a horizontal split cannot answer a vertical motion")))

(test motion-leaves-a-split-when-crossing-its-axis
  ;; Pressing Up inside a row of side-by-side panes must go to whatever is
  ;; above the row.  Most tiling window managers get this wrong on the first
  ;; try, and it is the single most jarring navigation bug.
  (let ((root (c:make-split
               :vertical
               (list (leaf-with "top")
                     (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))))))
    (is (equal "top" (target root '(1 0) :up)))
    (is (equal "top" (target root '(1 1) :up))
        "either pane of the row escapes upwards")
    (is (equal "b" (target root '(1 0) :right)))))

(test motion-enters-through-the-edge-it-crossed
  ;; README D20.  Travelling right into a container means arriving at its
  ;; leftmost child.
  (let ((root (c:make-split
               :horizontal
               (list (leaf-with "a")
                     (c:make-split :horizontal
                                   (list (leaf-with "b") (leaf-with "c")))))))
    (is (equal "b" (target root '(0) :right))
        "entering rightwards lands on the leftmost pane, not the last-focused")
    (is (equal "b" (target root '(1 1) :left))
        "and motion works within the inner split before leaving it")
    (is (equal "a" (target root '(1 0) :left))
        "only then does it escape")))

(test motion-is-involutive
  ;; Right then left must return you exactly where you started.  This is the
  ;; property that memory-based entry resolution would destroy.
  (let* ((pol (policy))
         (root (c:make-split
                :horizontal
                (list (c:make-split :vertical
                                    (list (leaf-with "a") (leaf-with "b")))
                      (c:make-split :vertical
                                    (list (leaf-with "c") (leaf-with "d")))))))
    (dolist (start '((0 0) (0 1) (1 0) (1 1)))
      (dolist (direction '(:left :right :up :down))
        (let ((there (p:find-motion-target pol root start direction)))
          (when there
            (let ((back (p:find-motion-target pol root there
                                              (c:opposite-direction direction))))
              (is (equal start back)
                  "~s then ~s from ~s came back to ~s"
                  direction (c:opposite-direction direction) start back))))))))

(test every-pane-is-reachable-by-directional-motion
  ;; The concrete failure that last-focus memory would cause: a pane that no
  ;; amount of pressing Right can ever reach.
  (let* ((pol (policy))
         (root (c:make-split
                :horizontal
                (list (leaf-with "a")
                      (c:make-split :horizontal
                                    (list (leaf-with "b") (leaf-with "c"))))))
         (seen '())
         (here '(0)))
    (loop repeat 10
          do (push (app-at root here) seen)
             (let ((next (p:find-motion-target pol root here :right)))
               (if next (setf here next) (return))))
    (is (null (set-difference '("a" "b" "c") seen :test #'equal))
        "rightward motion visited every pane: ~s" (reverse seen))))

(test motion-does-not-enter-a-hidden-tab
  (let ((root (c:make-split
               :horizontal
               (list (leaf-with "a")
                     (c:make-stack (list (leaf-with "visible")
                                         (leaf-with "hidden")) 0)))))
    (is (equal "visible" (target root '(0) :right)))))

(test motion-does-not-escape-through-a-stack
  ;; You do not arrive in another workspace by pressing Left.
  (let ((root (c:make-stack (list (leaf-with "ws0") (leaf-with "ws1")) 0)))
    (is (null (target root '(0) :right)))
    (is (null (target root '(0) :down)))))

(test motion-crosses-workspaces-never-but-crosses-splits-always
  (let ((root (c:make-stack
               (list (c:make-split :horizontal
                                   (list (leaf-with "a") (leaf-with "b")))
                     (leaf-with "other-workspace"))
               0)))
    (is (equal "b" (target root '(0 0) :right)))
    (is (null (target root '(0 1) :right))
        "the workspace boundary is a wall, and that is deliberate")))

(test motion-through-deep-nesting
  (let ((root (c:make-split
               :horizontal
               (list (c:make-split
                      :vertical
                      (list (leaf-with "a")
                            (c:make-split :horizontal
                                          (list (leaf-with "b") (leaf-with "c")))))
                     (leaf-with "d")))))
    (is (equal "d" (target root '(0 1 1) :right))
        "ascends two levels to find a direction it can satisfy")
    (is (equal "a" (target root '(0 1 0) :up)))
    (is (equal "c" (target root '(0 1 0) :right)))))

(test motion-escapes-p-can-build-a-wall
  (let* ((root (c:make-split
                :horizontal
                (list (leaf-with "outside")
                      (c:make-split :horizontal
                                    (list (leaf-with "in1") (leaf-with "in2"))))))
         (trap (c:child-at root 1)))
    ;; A container that traps motion.  One EQL-specialized method, no core edit.
    (defmethod p:motion-escapes-p ((pol p:conventional-policy)
                                   (container (eql trap)) direction)
      (declare (ignore direction))
      nil)
    (unwind-protect
         (progn
           (is (equal "in1" (target root '(1 1) :left)) "moves inside freely")
           (is (null (target root '(1 0) :left)) "but cannot get out"))
      (remove-method #'p:motion-escapes-p
                     (find-method #'p:motion-escapes-p '()
                                  (list (find-class 'p:conventional-policy)
                                        (closer-mop:intern-eql-specializer trap)
                                        (find-class 't)))))))

(test move-cursor-updates-the-world
  (let* ((pol (policy))
         (world (c:make-world
                 :root (c:make-split :horizontal
                                     (list (leaf-with "a") (leaf-with "b"))))))
    (is (equal '(0) (c:world-cursor world)))
    (is (equal '(1) (p:move-cursor pol world :right)))
    (is (equal '(1) (c:world-cursor world)))
    (is (null (p:move-cursor pol world :right)) "bumping the edge changes nothing")
    (is (equal '(1) (c:world-cursor world)))))

(test jump-cursor-resolves-into-a-leaf
  (let* ((pol (policy))
         (world (c:make-world
                 :root (c:make-split
                        :horizontal
                        (list (leaf-with "a")
                              (c:make-split :vertical (list (leaf-with "b")
                                                            (leaf-with "c"))))))))
    (is (equal '(1 0) (p:jump-cursor pol world '(1)))
        "a non-directional jump lands on the first child, not an edge")))

(test cursor-may-rest-on-an-empty-pane
  ;; README D18: focus is a place.  This is the property everything else
  ;; downstream assumes.
  (let* ((pol (policy))
         (world (c:make-world
                 :root (c:make-split :horizontal
                                     (list (leaf-with "a") (c:make-leaf))))))
    (p:move-cursor pol world :right)
    (is (equal '(1) (c:world-cursor world)))
    (is (null (c:world-focus-window world))
        "and there is simply no window to give Wayland focus to")))
