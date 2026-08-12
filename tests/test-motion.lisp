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
  ;; DESIGN D20.  Travelling right into a container means arriving at its
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

(test motion-is-involutive-when-the-panes-line-up
  "Right then left returns you where you started, and this is not general.

IT WAS ASSERTED AS GENERAL AND IT IS FALSE.  The tree below is built with no
weights, so every child is equal and every centre lands in the sibling that
faces it.  Under unequal weights it does not hold, it cannot be made to hold,
and the test below this one is the counterexample -- see there for why.  What
this test is actually for is still worth having: it is the property that
last-focus entry resolution would destroy even in the easy case."
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

(test motion-is-not-involutive-once-the-panes-stop-lining-up
  "The counterexample, written down because the guarantee above was general.

DEFAULTS-MOTION SAID `this is what makes Right-then-Left return you exactly
where you started', and the test that stood over it built a split with no
weights -- four equal quadrants, the one arrangement in which it is true.

Entry across the axis picks the child whose extent contains the *centre of the
rect you left*, and that rule cannot be involutive when the two sides are
divided differently: the centre you carry rightwards is computed from your
extent, and the centre you carry back is computed from the extent of wherever
you landed.  Left column 1:3 and right column 3:1 is the smallest case.  From
the top-left pane, rightwards lands in the top-right one, whose centre is
lower than the whole of the left column's top pane -- so leftwards from there
lands in the *bottom* left.

Fixing it would mean carrying the coordinate you crossed at, which is state
motion does not have and which D20 declined for the same reason it declined
last-focus memory.  So this is a documented property of the model rather than
a bug, and the way to keep it documented is to assert it."
  (let* ((pol (policy))
         (root (c:make-split
                :horizontal
                (list (c:make-split :vertical
                                    (list (leaf-with "l-top") (leaf-with "l-bottom"))
                                    '(1 3))
                      (c:make-split :vertical
                                    (list (leaf-with "r-top") (leaf-with "r-bottom"))
                                    '(3 1))))))
    (let* ((there (p:find-motion-target pol root '(0 0) :right))
           (back (and there (p:find-motion-target pol root there :left))))
      (is (equal "r-top" (app-at root there))
          "rightwards from the left column's top pane enters the tall top-right one")
      (is (not (equal '(0 0) back))
          "and leftwards from there does not come back: ~s" (and back (app-at root back)))
      (is (equal "l-bottom" (app-at root back))
          "it lands in the pane whose extent contains the centre it left"))))

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
  ;; DESIGN D18: focus is a place.  This is the property everything else
  ;; downstream assumes.
  (let* ((pol (policy))
         (world (c:make-world
                 :root (c:make-split :horizontal
                                     (list (leaf-with "a") (c:make-leaf))))))
    (p:move-cursor pol world :right)
    (is (equal '(1) (c:world-cursor world)))
    (is (null (c:world-focus-window world))
        "and there is simply no window to give Wayland focus to")))

;;; ------------------------------------------- the screens, and your place on them
;;;
;;; Per-output workspaces have worked for a while and nothing could reach them:
;;; eighty commands and not one named an output, so `the other monitor' was
;;; addressable only by putting a workspace on the screen you were already
;;; looking at.  Two rulings decide the shape of the fix and neither is
;;; overturned here -- the cursor stays one place in one model
;;; (SHOW-WORKSPACE-ON), and which workspace an output displays stays a
;;; property of the output (OUTPUT-CONTENT).  So crossing is a cursor move
;;; within one tree, and these say so.

(defun two-screen-world (&key (left-workspace 0) (right-workspace 1))
  "A world with two workspaces of two panes and a monitor showing each.

Side by side in one logical coordinate space, which is how river describes a
multi-monitor arrangement and why OUTPUT-IN-DIRECTION is ordinary geometry.

THE RIGHT-HAND WORKSPACE IS SPLIT VERTICALLY, and that is not decoration: it
makes leaving that screen by walking left possible from *either* of its panes,
so a test can cross away from the second one without first walking to the
first one -- which is a move within the screen and would update the very
memory the test is about.  Getting this wrong is how the first draft of
YOU-ARRIVE-WHERE-YOU-LEFT-OFF asserted something false and blamed the code."
  (let* ((root (c:make-stack
                (list (c:make-split :horizontal
                                    (list (leaf-with "l0") (leaf-with "l1")))
                      (c:make-split :vertical
                                    (list (leaf-with "r0") (leaf-with "r1"))))
                0))
         (world (c:make-world :root root))
         (left (make-instance 'c:output :name "LEFT"))
         (right (make-instance 'c:output :name "RIGHT")))
    (setf (c:output-rect left) (c:make-rect 0 0 1920 1080)
          (c:output-rect right) (c:make-rect 1920 0 1920 1080)
          (c:prop left :workspace) left-workspace
          (c:prop right :workspace) right-workspace
          (c:world-outputs world) (list left right))
    (values world left right)))

(test which-screen-is-that-way
  (multiple-value-bind (world left right) (two-screen-world)
    (is (eq right (p:output-in-direction world left :right)))
    (is (eq left (p:output-in-direction world right :left)))
    (is (null (p:output-in-direction world right :right))
        "there is nothing past the last screen, and that is not an error")
    (is (null (p:output-in-direction world left :up))
        "two monitors side by side are not above each other")
    (is (eq right (p:output-showing world 1))
        "and the output showing a workspace is found by the one function that
answers that -- it used to be answered in the runtime as well")))

(test walking-off-the-edge-of-one-screen-arrives-on-the-next
  "Motion is continuous across every boundary, and the screen was the last one
it was not crossing.  It costs no key: with one monitor there is nothing in any
direction and the behaviour is unchanged."
  (multiple-value-bind (world) (two-screen-world)
    (setf (c:world-cursor world) '(0 1))     ; right-hand pane of the left screen
    (let ((landed (p:move-cursor (policy) world :right)))
      (is (equal '(1 0) landed)
          "off the right edge of the left screen and onto the right one")
      (is (equal '(1 0) (c:world-cursor world))))
    (let ((landed (p:move-cursor (policy) world :down)))
      (is (equal '(1 1) landed) "and ordinary motion resumes on the new screen"))))

(test crossing-can-be-turned-off-and-then-the-edge-is-the-edge
  (multiple-value-bind (world) (two-screen-world)
    (setf (c:world-cursor world) '(0 1))
    (let ((p:*motion-crosses-outputs* nil))
      (is (null (p:move-cursor (policy) world :right))
          "the edge of the workspace is the edge of the world again")
      (is (equal '(0 1) (c:world-cursor world)) "and nothing moved"))))

(test you-arrive-where-you-left-off
  "The piece that was genuinely missing.  Without it every crossing costs you
your place, which is what made the explicit commands not worth pressing."
  (multiple-value-bind (world) (two-screen-world)
    (setf (c:world-cursor world) '(0 0))
    ;; Work on the right-hand screen, ending up in its *lower* pane.
    (p:move-cursor (policy) world :right)      ; (0 1), the left screen's second
    (p:move-cursor (policy) world :right)      ; crosses to (1 0)
    (p:move-cursor (policy) world :down)       ; (1 1)
    (is (equal '(1 1) (c:world-cursor world)))
    ;; Leave for the other screen without moving within this one first, which
    ;; is why that workspace is split the other way.
    (p:move-cursor (policy) world :left)
    (is (equal '(0 1) (c:world-cursor world))
        "and the left screen remembered us too -- (0 1) is where we left it")
    (p:move-cursor (policy) world :right)
    (is (equal '(1 1) (c:world-cursor world))
        "the right-hand screen still has us in its lower pane")))

(test a-remembered-place-that-no-longer-exists-is-repaired-rather-than-checked
  "A stale path goes through REPAIR-PATH like every other stale path in this
program, which lands you at the deepest surviving part of where you were.

NOTE-PLACE is internal on purpose and these reach in with a double colon.  The
memory is recorded on every *announced* cursor arrival, which is what `the
cursor moved' means here -- and examples/02 shows why that is not the same as
every SETF of the cursor: it moves the cursor into another workspace to place
a window and puts it straight back, which nobody sees and which must not
record anything.  Exporting a call for extensions to make would be a rule the
shipped examples do not follow, which is the defect gate 11 exists for."
  (multiple-value-bind (world) (two-screen-world)
    (setf (c:world-cursor world) '(1 1))
    (p::note-place world '(1 1))
    ;; That whole workspace becomes a single pane while we are not looking.
    (setf (c:child-at (c:world-root world) 1) (leaf-with "survivor"))
    (is (equal '(1) (p:jump-cursor (policy) world (p:remembered-place world 1)))
        "the deepest thing that still exists")))

(test remembering-can-be-turned-off
  (multiple-value-bind (world) (two-screen-world)
    (let ((p:*remember-place* nil))
      (setf (c:world-cursor world) '(1 1))
      (p::note-place world '(1 1))
      (is (equal '(1) (p:remembered-place world 1))
          "nothing is recorded and nothing is remembered, so it is the workspace"))))
