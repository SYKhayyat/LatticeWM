;;;; tests/test-placement.lisp --- Where a window sits when you say where.
;;;;
;;;; #14.  Snapping is a place, not a layout engine: SNAP-RECT asks the layout
;;;; its own space, the command floats a window into it, and a window rule can
;;;; say :SNAP to open there.  All three ask the same geometry question, and the
;;;; question has no compositor in it.

(in-package #:latticewm/tests)
(in-suite placement)

;;; ------------------------------------------------------------------ fixtures

(defun a-world-with-an-output (width height)
  "A world with one output of WIDTH x HEIGHT and nothing else.
Returns (values world output)."
  (let* ((output (make-instance 'c:output :name "screen"
                                :rect (c:make-rect 0 0 width height)))
         (world (c:make-world)))
    (setf (c:world-outputs world) (list output))
    (p:ensure-workspaces-for-outputs world)
    (values world output)))

(defmacro with-a-screen ((world output width height) &body body)
  `(multiple-value-bind (,world ,output) (a-world-with-an-output ,width ,height)
     (let ((r:*world* ,world) (p:*policy* (policy)) (p:*seat* nil))
       ,@body)))

;;; -------------------------------------------------------------- the geometry

(test the-eight-regions-tile-the-screen-with-no-gap-and-no-overlap
  "Two halves tile the screen, and a corner is where a vertical half meets a
horizontal one.  The odd screen is the one that catches division errors: on
1281 the two halves are 640 and 641, and a naive `half then repeat' would leave
a column of pixels belonging to neither."
  (with-a-screen (world output 1281 801)
    (declare (ignore world))
    (let* ((policy (policy))
           (base (p:outer-rect policy output))
           (left (p:snap-rect policy output :left))
           (right (p:snap-rect policy output :right))
           (top (p:snap-rect policy output :top))
           (bottom (p:snap-rect policy output :bottom))
           (corner (p:snap-rect policy output :bottom-right))
           (x (c:rect-x base)) (y (c:rect-y base))
           (w (c:rect-w base)) (h (c:rect-h base)))
      ;; the vertical pair: together the whole width, side by side
      (is (= x (c:rect-x left)))
      (is (= (+ x w) (+ (c:rect-x right) (c:rect-w right)))
          "the right edge of the right half is the screen's")
      (is (= 640 (c:rect-w left)))
      (is (= 641 (c:rect-w right)) "and the odd pixel went somewhere")
      (is (= (+ (c:rect-x left) (c:rect-w left)) (c:rect-x right))
          "no gap and no overlap down the seam")
      (is (= h (c:rect-h left)) "a vertical half is the full height")
      ;; the horizontal pair, the same way
      (is (= y (c:rect-y top)))
      (is (= 400 (c:rect-h top)))
      (is (= 401 (c:rect-h bottom)))
      (is (= (+ (c:rect-y top) (c:rect-h top)) (c:rect-y bottom)))
      ;; a corner is the intersection of two halves
      (is (= (c:rect-x right) (c:rect-x corner)))
      (is (= (c:rect-y bottom) (c:rect-y corner)))
      (is (= (c:rect-w right) (c:rect-w corner)))
      (is (= (c:rect-h bottom) (c:rect-h corner)))
      ;; and maximize is the whole space -- not a river fullscreen, which would
      ;; be no rectangle at all but a window the compositor draws alone.
      (let ((max (p:snap-rect policy output :maximize)))
        (is (= x (c:rect-x max)))
        (is (= y (c:rect-y max)))
        (is (= w (c:rect-w max)))
        (is (= h (c:rect-h max)))))))

(test the-regions-take-the-gaps-the-layout-took
  "SNAP-RECT answers in the LAYOUT's space (#14) -- the same OUTER-RECT the
tiling layout draws into, which is where a gap or a panel or a shared group's
union has already been subtracted.  A snapped window overlapping the panel it
was told to respect would be a second opinion about the screen."
  (with-a-screen (world output 800 800)
    (declare (ignore world))
    (let ((p:*outer-gaps* 8)
          (p:*gaps* 16))
      (let ((left (p:snap-rect (policy) output :left)))
        (is (= 8 (c:rect-x left)) "inside the outer gap")
        (is (= 392 (c:rect-w left)) "half of what is left, not half of 800")
        (is (= 8 (c:rect-y left)) "and the top gap too"))
      ;; and the space is the SHARED one when there is one (#96): a plane over
      ;; two monitors has one left edge, and the left half of it straddles the
      ;; seam rather than ending at it.
      (setf (c:output-share-rect output) (c:make-rect 0 0 1600 800))
      (let ((left (p:snap-rect (policy) output :left)))
        (is (= 8 (c:rect-x left)))
        (is (= 792 (c:rect-w left)) "the union's half, seam and all")))))

(test center-is-half-the-space-and-centred
  "A window of half the space, in the middle of it.  `Snap it to the middle'
must mean the same thing every time, which is why it does not depend on the
size the window happens to have."
  (with-a-screen (world output 1000 600)
    (declare (ignore world))
    (let ((centre (p:snap-rect (policy) output :center)))
      (is (= 500 (c:rect-w centre)))
      (is (= 300 (c:rect-h centre)))
      (is (= 250 (c:rect-x centre)))
      (is (= 150 (c:rect-y centre))))))

(test a-strange-place-refuses-to-be-one
  (with-a-screen (world output 800 600)
    (declare (ignore world))
    (is (null (p:snap-place-p :sideways)))
    (is (null (p:snap-place-p nil)))
    (is-true (p:snap-place-p :bottom))))

;;; ----------------------------------------------------------------- the verb

(test snap-lands-the-focused-window-in-the-region
  "The command floats the window and places it in one move: TRANSITION-WINDOW's
`floating' is how a tiled window leaves the tree (#25), and the float it makes
gets the rectangle straight away, so there is no moment where the window is
floating at 640 x 480 and not where you asked."
  (with-window-world (world window "note")
    (let ((output (first (c:world-outputs world))))
      (is-true (r:snap :left :output output))
      (let ((float (c:float-of-window world window)))
        (is-true float)
        (is (equal '(:floating) (c:window-state world window)))
        (is (= 0 (c:rect-x (c:float-rect float))) "left edge")
        (is (= 640 (c:rect-w (c:float-rect float))) "and the left half")))))

(test snap-refuses-with-nothing-to-move
  (with-a-screen (world output 800 600)
    (declare (ignore world))
    (let ((r::*echo-message* nil))
      (is (stringp (r:snap :left :output output))
          "it answers, instead of crashing")
      (is-true (search "no window" (car r::*echo-message*))))))

;;; --------------------------------------------------------------- the rule

(test a-window-rule-can-say-where-a-window-opens
  "The declarative half: `(:app-id \"note\") :snap :right' floats the window
and puts it on the right half, with no command run -- the same SNAP-RECT the
command uses, which is what makes the two halves agree.  :SNAP alone already
implies floating (lifecycle.lisp), so a rule need not say :FLOAT twice."
  (let ((p:*window-rules* '(((:app-id "note") :snap :right))))
    (with-window-world (world window "note")
      ;; ATTACH-WINDOW's own sequence: the policy says the window floats, and
      ;; then the runtime makes the float -- the line that reads the place.
      (when (c:window-floating-p window)
        (r::float-window-now p:*policy* window))
      (let ((float (c:float-of-window world window)))
        (is-true float)
        (is (= 640 (c:rect-x (c:float-rect float))))
        (is (= 640 (c:rect-w (c:float-rect float))))
        (is (= 720 (c:rect-h (c:float-rect float)))
            "the full height, not the 480 a default float would be")
        (is (equal '(:floating) (c:window-state world window)))))))
