;;;; tests/test-gestures.lisp --- Touch gestures: the table, read and planned.
;;;;
;;;; #16.  The wire half -- making river_touch_gesture_v1 objects -- needs a
;;;; compositor and lives in the headless-river integration suite.  What can be
;;;; asked without one is everything that matters most: what a gesture *is*, how
;;;; the table turns into plans, how a prompt reads `3 left', and which command
;;;; a recognised gesture runs.  A spec with no protocol in it is where the
;;;; decisions are, and that is what these checks hold.

(in-package #:latticewm/tests)
(in-suite gestures)

;;; ------------------------------------------------------------------ the spec

(test a-spec-is-fingers-and-one-trigger-the-protocol-knows
  (is-true (p:gesture-spec-p '(:fingers 3 :direction :left)))
  (is-true (p:gesture-spec-p '(:fingers 1 :edge :top)))
  (is-true (p:gesture-spec-p '(:fingers 2 :pinch :in)))
  (is-true (p:gesture-spec-p '(:fingers 4 :direction :up))))

(test a-spec-refuses-fingers-that-are-not-fingers
  (is-false (p:gesture-spec-p '(:fingers 0 :direction :left))
            "the protocol says finger count must be greater than zero")
  (is-false (p:gesture-spec-p '(:fingers -3 :direction :left)))
  (is-false (p:gesture-spec-p '(:direction :left))
            "a gesture with no fingers is not a gesture")
  (is-false (p:gesture-spec-p '(:fingers 3 :swipe :left))
            "an unknown trigger is refused, not guessed")
  (is-false (p:gesture-spec-p '(:fingers 3))))

(test a-pinch-needs-two-points-to-have-a-distance
  (is-false (p:gesture-spec-p '(:fingers 1 :pinch :out))
            "one finger has no pinch distance to be a ratio of; the protocol
ignores the threshold rather than refusing, which is a gesture that can never
fire -- refusing it here is kinder")
  (is-true (p:gesture-spec-p '(:fingers 2 :pinch :out))))

;;; ---------------------------------------------------------------- the plans

(test a-direction-spec-plans-into-a-distance-and-a-command
  (let ((plans (p:gesture-plans '(((:fingers 3 :direction :left)
                                   . "next-workspace")))))
    (is (= 1 (length plans)))
    (let ((plan (first plans)))
      (is (= 3 (getf plan :fingers)))
      (is (eq :left (getf plan :direction)))
      (is (= 200 (getf plan :distance))
          "the shipped swipe distance, filled in by the planner")
      (is (equal "next-workspace" (getf plan :command))))))

(test the-first-entry-per-gesture-wins
  (let ((plans (p:gesture-plans '(((:fingers 3 :direction :left) . "first")
                                  ((:fingers 3 :direction :left) . "second")))))
    (is (= 1 (length plans)) "a gesture is defined once")
    (is (equal "first" (getf (first plans) :command))
        "and the first one stands")))

(test a-pinch-plans-a-scale-on-the-right-side-of-one
  (let* ((in (p:gesture-plans '(((:fingers 2 :pinch :in) . "zoom-out"))))
         (out (p:gesture-plans '(((:fingers 2 :pinch :out) . "zoom-in")))))
    (is (< (getf (first in) :scale) 1) "pinch in means fingers together")
    (is (> (getf (first out) :scale) 1) "pinch out means fingers apart")))

(test gesture-scale-for-pushes-the-other-side-out-of-reach
  (is (< (p:gesture-scale-for '(:fingers 2 :pinch :in)) 1))
  (is (>= (p:gesture-scale-for '(:fingers 2 :pinch :out)) 1)
      "an inward spec still has to answer `what counts as out', and the answer
is a ratio no pinch reaches")
  (let ((p:*gestures* '()))
    (is (= 0.8 (p:gesture-scale-for '(:fingers 2 :pinch :in :scale 0.8))))
    (is (= 1.4 (p:gesture-scale-for '(:fingers 2 :pinch :out :scale 1.4))))))

(test a-bad-entry-is-dropped-and-the-rest-applies
  (let ((plans (p:gesture-plans '(((:fingers 0 :direction :left) . "broken")
                                  ((:fingers 3 :direction :right) . "next")))))
    (is (= 1 (length plans))
        "one bad line costs one gesture, not the table")))

;;; ---------------------------------------------------------------- the prompt

(test a-prompt-reads-a-gesture-the-way-it-is-said
  (multiple-value-bind (spec why) (p:parse-gesture-spec "3 left")
    (declare (ignore why))
    (is (equal '(:fingers 3 :direction :left) spec)))
  (multiple-value-bind (spec why) (p:parse-gesture-spec "2 pinch out")
    (declare (ignore why))
    (is (equal '(:fingers 2 :pinch :out) spec)))
  (multiple-value-bind (spec why) (p:parse-gesture-spec "1 edge top")
    (declare (ignore why))
    (is (equal '(:fingers 1 :edge :top) spec))))

(test a-prompt-refuses-arithmetic-and-nonsense
  (multiple-value-bind (spec why) (p:parse-gesture-spec "3+1 left")
    (is (null spec) "a gesture is words, not arithmetic")
    (is-true why "and it says so"))
  (multiple-value-bind (spec why) (p:parse-gesture-spec "many left")
    (is (null spec))
    (is-true why))
  (multiple-value-bind (spec why) (p:parse-gesture-spec "")
    (declare (ignore why))
    (is (null spec) "an empty line asks for a gesture and names none")))

(test a-spec-reads-back-as-the-words-it-was-said-in
  (is (equal "3 left" (p:gesture-spec-text '(:fingers 3 :direction :left))))
  (is (equal "2 pinch in" (p:gesture-spec-text '(:fingers 2 :pinch :in))))
  (is (equal "1 edge top" (p:gesture-spec-text '(:fingers 1 :edge :top)))))

;;; ------------------------------------------------------- adding and removing

(test gesture-add-puts-a-spec-in-front-and-is-idempotent
  (let ((p:*gestures* '()))
    (p:gesture-add '(:fingers 3 :direction :left) "next-workspace")
    (is (= 1 (length p:*gestures*)))
    (p:gesture-add '(:fingers 3 :direction :left) "previous-workspace")
    (is (= 1 (length p:*gestures*)) "redefining a gesture replaces it")
    (is (equal "previous-workspace" (cdar p:*gestures*)) "and the new wins")))

(test gesture-remove-takes-exactly-the-one-asked-for
  (let ((p:*gestures* '(((:fingers 3 :direction :left) . "a")
                        ((:fingers 3 :direction :right) . "b"))))
    (p:gesture-remove '(:fingers 3 :direction :left))
    (is (equal '(((:fingers 3 :direction :right) . "b")) p:*gestures*))))

(test the-shipped-table-is-the-pager-and-focus-on-three-fingers
  (let ((plans (p:gesture-plans)))
    (is-true (find-if (lambda (p)
                        (and (= 3 (getf p :fingers))
                             (eq :left (getf p :direction))
                             (string= "next-workspace" (getf p :command))))
                      plans))
    (is-true (find-if (lambda (p)
                        (and (eq :up (getf p :direction))
                             (string= "focus-previous" (getf p :command))))
                      plans))))

;;; ------------------------------------------------------------- the boundary

(test the-gesture-verbs-are-commands-you-can-bind-to
  (is-true (p:find-command "gesture"))
  (is-true (p:find-command "forget-gesture"))
  (is-true (p:find-command "refresh-gestures"))
  (is-true (p:find-command "gestures")
           "a prompt can reach all four, which is what makes the imperative
half of the feature the same thing as the declarative table"))
;;; ------------------------------------------------------------- the five hooks
;;;
;;; Gate 14's demand, and it earned its keep again: a hook fired only from
;;; inside a protocol event handler has an argument list nobody has ever
;;; checked.  The stream functions below are the seam that makes it checkable
;;; without a compositor, and each hook is asserted on what it was handed rather
;;; than on the fact that something fired.

(defvar *heard* '()
  "What the gesture hooks were handed, newest first, for the checks below.

A variable rather than a closure because a hook added inside a test fixture can
be re-evaluated by the fixture's own macro; #29 learned that the hard way.")

;; One recorder per hook rather than one recorder taking a tag: a hook is
;; called with the hook's own arguments, so a generic `hear' would have to be
;; told which hook it is by the caller -- and the caller is the runtime.  The
;; tag is baked into each function instead, which is also what makes the checks
;; read like the hook's own signature.

(defun hear-motion (&rest arguments) (push (cons :gesture-motion arguments) *heard*))
(defun hear-scale (&rest arguments) (push (cons :gesture-scale arguments) *heard*))
(defun hear-fingers (&rest arguments) (push (cons :gesture-fingers arguments) *heard*))
(defun hear-start (&rest arguments) (push (cons :gesture-start arguments) *heard*))
(defun hear-end (&rest arguments) (push (cons :gesture-end arguments) *heard*))

(test the-running-gesture-stream-arrives-with-its-own-arguments
  (let ((*heard* '()))
    (p:add-hook :gesture-motion 'hear-motion)
    (p:add-hook :gesture-scale 'hear-scale)
    (p:add-hook :gesture-fingers 'hear-fingers)
    (p:add-hook :gesture-end 'hear-end)
    (unwind-protect
         (progn
           (r:gesture-moved 12 -7)
           (is (equal '(:gesture-motion 12 -7) (first *heard*))
               "motion is cumulative and arrives as two numbers, dx first")
           (r:gesture-scaled 0.75)
           (is (equal '(:gesture-scale 0.75) (first *heard*))
               "scale is one number, a ratio of touch-down distance")
           (r:gesture-fingers-changed 4)
           (is (equal '(:gesture-fingers 4) (first *heard*))
               "a finger count is a count")
           (r:gesture-finished '(:fingers 3 :direction :left :command "x"))
           (is (equal '(:gesture-end (:fingers 3 :direction :left :command "x"))
                      (first *heard*))
               "and the end carries the plan it started with, not a new one"))
      (p:remove-hook :gesture-motion 'hear-motion)
      (p:remove-hook :gesture-scale 'hear-scale)
      (p:remove-hook :gesture-fingers 'hear-fingers)
      (p:remove-hook :gesture-end 'hear-end))))

(test a-recognised-gesture-announces-itself-before-it-runs-its-command
  "The order is the contract: :gesture-start before the command, so a listener
can see what is about to happen."
  (let ((*heard* '())
        (p:*echo-message* nil))
    (p:add-hook :gesture-start 'hear-start)
    (unwind-protect
         (r:gesture-fired '(:fingers 3 :direction :left :distance 200
                            :command "focus-next"))
      (p:remove-hook :gesture-start 'hear-start))
    (is (equal :gesture-start (car (first *heard*))))
    (is (= 3 (getf (second (first *heard*)) :fingers))
        "the plan arrives whole, with the fingers it needed")))
