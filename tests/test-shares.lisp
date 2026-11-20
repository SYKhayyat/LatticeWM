;;;; tests/test-shares.lisp --- One plane across two monitors (#96).
;;;;
;;;; The feature is the one place in this program where a *display* question
;;;; and a *model* question meet: a shared group draws one workspace, laid out
;;;; over the union of its monitors, and every member but the leader draws
;;;; nothing.  Those are the two halves worth pinning, and they are checkable
;;;; with no compositor at all -- an output is a rectangle and a name, and the
;;;; layout is a function of the world.
;;;;
;;;; Two rulings under test, both decided rather than discovered:
;;;;
;;;;   * A FOLLOWER ASKS NOTHING.  Its :WORKSPACE is the group's, but it
;;;;     produces no layout entry, because two entries for one plane is the
;;;;     duplicate-node collapse that ENSURE-WORKSPACES-FOR-OUTPUTS exists to
;;;;     prevent -- arrived at on purpose by a feature instead of by accident
;;;;     through a restore.
;;;;
;;;;   * THE INVARIANT STILL HOLDS AROUND THE GROUP.  Two outputs may share one
;;;;     index only while they are in one group; a third screen that lands on
;;;;     the same index without joining is still moved off it.  The exception is
;;;;     narrow on purpose, and the last test is what keeps it narrow.

(in-package #:latticewm/tests)
(in-suite shares)

;;; ------------------------------------------------------------------ fixtures

(defun two-output-world (&key (w 1000) (h 800))
  "A world with a monitor left of another, each showing a workspace of its own."
  (let* ((world (c:make-world))
         (left (make-instance 'c:output :name "left"
                              :rect (c:make-rect 0 0 w h)))
         (right (make-instance 'c:output :name "right"
                               :rect (c:make-rect w 0 w h))))
    (setf (c:world-outputs world) (list left right))
    (p:ensure-workspaces-for-outputs world)
    (values world left right)))

(defun three-output-world (&key (w 1000) (h 800))
  "Three monitors in a row, so that a group can grow past two."
  (let* ((world (c:make-world))
         (left (make-instance 'c:output :name "left"
                              :rect (c:make-rect 0 0 w h)))
         (middle (make-instance 'c:output :name "middle"
                                :rect (c:make-rect w 0 w h)))
         (right (make-instance 'c:output :name "right"
                               :rect (c:make-rect (* 2 w) 0 w h))))
    (setf (c:world-outputs world) (list left middle right))
    (p:ensure-workspaces-for-outputs world)
    (values world left middle right)))

;;; -------------------------------------------------------------------- tests

(test the-union-of-two-monitors-is-the-rectangle-around-both
  "The geometry the whole feature rests on, checked on its own because
RECT-UNION is the only place a group's area comes from."
  (let ((left (c:make-rect 0 0 1000 800))
        (right (c:make-rect 1000 0 1000 800)))
    (is (c:rect-equal (c:rect-union (list left right))
                      (c:make-rect 0 0 2000 800))
        "a row of two is twice as wide and the same height")
    (is (c:rect-equal (c:rect-union (list right (c:make-rect 0 200 1920 1080)))
                      (c:make-rect 0 0 2000 1280))
        "monitors need not share an edge, and the union still contains both")
    (is (null (c:rect-union '()))
        "no rectangles is no rectangle, rather than one of no size")
    (is (c:rect-equal (c:rect-union (list left (c:make-rect 0 0 0 0))) left)
        "and an empty rectangle cannot drag the union back to the origin")))

(test sharing-lays-one-workspace-out-across-both-monitors
  "Before sharing there are two layout entries; after it there is one, and its
rectangle is the union.  That single entry is what stops the duplicate-node
collapse."
  (multiple-value-bind (world left right) (two-output-world)
    (declare (ignore right))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (r::*echo-message* nil))
      (is (= 2 (length (r::output-contents p:*policy*)))
          "two monitors, two workspaces, two entries")
      (r:share-workspace :right)
      (is (= 1 (length (r::output-contents p:*policy*)))
          "one plane, one entry")
      (is (c:rect-equal (c:output-share-rect left)
                        (c:make-rect 0 0 2000 800))
          "the leader presents the union of the group")
      (is-true (c:share-follower-p (second (c:world-outputs world)))
               "and the other screen is a follower, not a second copy")
      (is (> (c:rect-w (p:outer-rect p:*policy* left)) 1900)
          "so the gaps go round the arrangement, not round each monitor in it"))))

(test a-group-grows-past-two-screens
  "A key that only ever reached two screens would be a demo rather than a
feature: the neighbour you are already sharing with is exactly the one a
`next screen that way' search finds, so the search excludes it."
  (multiple-value-bind (world left middle right) (three-output-world)
    (declare (ignore middle))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:share-workspace :right)
      (is (eq (c:output-share-group left) (c:output-share-group middle))
          "the middle screen joined the one on the left")
      (is (= 2 (length (r::output-contents p:*policy*)))
          "one entry for the pair, one for the screen that is not in it")
      (r:share-workspace :right)
      (is (= 1 (length (r::output-contents p:*policy*)))
          "and the third joins the same plane")
      (is (c:rect-equal (c:output-share-rect left)
                        (c:make-rect 0 0 3000 800))
          "all three, one rectangle")
      (is-true (c:share-follower-p middle))
      (is-true (c:share-follower-p right)))))

(test unsharing-gives-this-screen-a-workspace-of-its-own
  "The other half of the key, and the half that has to be right: a screen left
on the group's index with no group to justify it is the collapse."
  (multiple-value-bind (world left right) (two-output-world)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:share-workspace :right)
      (r:unshare-workspace)
      (is (= 2 (length (r::output-contents p:*policy*)))
          "two planes again")
      (is (null (c:output-share-rect left)) "the leader presents itself again")
      (is (null (c:share-follower-p right)) "and the follower is a screen again")
      (is (/= (c:prop left :workspace) (c:prop right :workspace))
          "on a workspace of its own, not the group's"))))

(test unsharing-stops-one-screen-and-leaves-the-group-alone
  "With three screens, giving one back is not the same as dissolving the
group -- and the two that remain keep the plane they were sharing."
  (multiple-value-bind (world left middle right) (three-output-world)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:share-workspace :right)
      (r:share-workspace :right)
      (is (= 1 (length (r::output-contents p:*policy*))) "three, one plane")
      ;; The cursor is on the group's workspace, whose output is the leader.
      (r:unshare-workspace)
      (is (= 2 (length (r::output-contents p:*policy*)))
          "one screen left, two still sharing")
      (let ((group (c:output-share-group middle)))
        (is (member right group :test #'eq) "the middle and the right keep it")
        (is (not (member left group :test #'eq)) "and the left is out")))))

(test the-invariant-moves-a-stranger-off-a-shared-workspace
  "THE EXCEPTION IS ONE GROUP WIDE.  Two outputs may share an index because
they are one plane; a third arriving on that index without joining is still
the collapse, and the invariant is what has to catch it."
  (multiple-value-bind (world left middle right) (three-output-world)
    (declare (ignore left))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:share-workspace :right)                ; left + middle share
      ;; A third screen handed the group's index without joining.
      (setf (c:prop right :workspace) (c:prop middle :workspace))
      (p:ensure-workspaces-for-outputs world)
      (is (/= (c:prop right :workspace) (c:prop middle :workspace))
          "the stranger was moved off the shared plane")
      (is (= (c:prop left :workspace) (c:prop middle :workspace))
          "and the group itself was left alone"))))

(test list-shared-names-the-screens-showing-the-plane
  "The command the other two are found by.  The empty case is the one every
user meets first, and it must say so."
  (multiple-value-bind (world left right) (two-output-world)
    (declare (ignore right))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (r::*help-visible* nil) (r::*echo-message* nil))
      (is (stringp (r:list-shared)) "nothing shared: it says so in the echo area")
      (is (null r::*help-visible*) "and there is no page")
      (r:share-workspace :right)
      (is-true (r:list-shared) "with a group, a page")
      (let ((page (format nil "~s" r::*help-visible*)))
        (is-true (search "left" page) "naming one screen")
        (is-true (search "right" page) "and the other")
        (is-true (search "1" page) "and the workspace they share")))))
