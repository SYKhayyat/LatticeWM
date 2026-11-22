;;;; tests/test-pins.lisp --- A workspace that belongs to a screen (#132).
;;;;
;;;; The rule a pin collides with is #126's trade: switching a screen to a
;;;; workspace another screen is showing makes the two trade.  A pin says the
;;;; opposite about the same pair, so one of the two answers has to give.  The
;;;; answer taken here is `the pin wins': a pin is a statement about where a
;;;; workspace *is*, so asking another screen for it takes you to that screen.
;;;; The two alternatives are argued down in the option's docstring -- a pin
;;;; anything can override is not a pin, and refusing what the user asked for is
;;;; the annoying answer -- and what is checked below is that the pin really
;;;; does hold when the two rules meet.

(in-package #:latticewm/tests)
(in-suite pins)

(defmacro with-two-screens-and-no-pins (&body body)
  "Two monitors side by side, the cursor on the left one, and no pins anywhere.

The pin table is a global, so a suite that leaves one behind changes what a later
suite's switch does.  The screens are left showing the first and the second
workspace, in that order -- TWO-OUTPUT-WORLD's job rather than this macro's."
  `(unwind-protect
        (multiple-value-bind (world left right) (two-output-world)
          (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
                (p:*echo-message* nil)
                (r:*workspace-pins* nil))
            (declare (ignorable left right))
            (setf (c:world-cursor world) '(0 0))
            ,@body))
     (setf r:*workspace-pins* nil)))

(defun screen (name)
  "The output called NAME in the current world."
  (find name (c:world-outputs r:*world*) :key #'c:output-name :test #'string=))

;;; ------------------------------------------------------- the table and verbs

(test a-pin-is-a-number-and-an-output-name
  "The name, not the object: river has no stable output identity, and a pin
written in a config file has to mean the same screen tomorrow."
  (with-two-screens-and-no-pins
    (is (= 2 (r:pin-workspace 2 (screen "right"))))
    (is (equal '((2 . "right")) r:*workspace-pins*))
    ;; The helpers speak internal indexes, like the rest of the runtime, and
    ;; translate to the numbers a human types.
    (is (string= "right" (r:pinned-output-name 1))
        "internal index one is the second workspace")
    (is (null (r:pinned-output-name 0)) "and only that one is pinned")))

(test unpinning-takes-the-entry-away-and-says-when-there-was-nothing-there
  (with-two-screens-and-no-pins
    (r:pin-workspace 2 (screen "right"))
    (is (= 2 (r:unpin-workspace 2)))
    (is (null r:*workspace-pins*))
    (is (null (r:unpin-workspace 2)) "unpinning nothing answers nothing")))

(test pins-are-kept-in-order-because-a-table-is-read-in-order
  (with-two-screens-and-no-pins
    (r:pin-workspace 2 (screen "right"))
    (r:pin-workspace 1 (screen "right"))
    (is (equal '(1 2) (mapcar #'car r:*workspace-pins*)))))

;;; ------------------------------------------------------------ and the switch

(test pinning-a-workspace-to-another-screen-moves-it-there-at-once
  "A pin that left the workspace on the wrong screen until something else
happened would be a pin you cannot check."
  (with-two-screens-and-no-pins
    (is (= 0 (c:prop (screen "left") :workspace))
        "left starts on the first workspace")
    (is (= 1 (c:prop (screen "right") :workspace))
        "and right on the second")
    (r:pin-workspace 1 (screen "right"))
    (is (= 0 (c:prop (screen "right") :workspace))
        "workspace one is on the right screen the moment it is pinned there")
    (is (= 1 (c:prop (screen "left") :workspace))
        "and the left screen has what the right one was showing")))

(test asking-another-screen-for-a-pinned-workspace-goes-to-its-screen
  "The whole point.  Screen A is asked for a workspace pinned to screen B: B goes
on showing it, A keeps what it had rather than trading, and the cursor is on the
workspace because you asked to see it."
  (with-two-screens-and-no-pins
    (r:pin-workspace 1 (screen "right"))
    (is (= 0 (c:prop (screen "right") :workspace)))
    (is (= 1 (c:prop (screen "left") :workspace)))
    ;; Ask the left screen for the pinned workspace.  Without a pin this would
    ;; trade them back; with one, the pinned screen keeps it.
    (r::show-workspace-on (screen "left") 0)
    (is (= 0 (c:prop (screen "right") :workspace)) "the pinned screen still has it")
    (is (= 1 (c:prop (screen "left") :workspace))
        "and the asking screen did not take it -- no trade happened")
    (is (= 0 (first (c:world-cursor r:*world*)))
        "and the cursor went to the workspace you asked for")))

(test a-pin-to-a-screen-that-is-not-here-is-not-a-pin
  "An unplugged monitor must not make a workspace unreachable: the entry stays in
the table, so plugging the screen back in restores the pin, but until then the
workspace behaves as if it were free."
  (with-two-screens-and-no-pins
    (setf r:*workspace-pins* '((2 . "a-screen-that-is-not-here")))
    (is (null (r:pinned-output-for 1)) "no live screen answers to that name")
    (r::show-workspace-on (screen "left") 1)
    (is (= 1 (c:prop (screen "left") :workspace))
        "so the switch behaves as it did before")
    (is (equal '((2 . "a-screen-that-is-not-here")) r:*workspace-pins*)
        "and the entry is still there for when the screen comes back")))

;;; --------------------------------------------------------------- the report

(test the-report-says-what-is-pinned-and-whether-the-screen-is-here
  (with-two-screens-and-no-pins
    (r:pins)
    (is (string= "workspace pins" (car p:*help-visible*)))
    (is-true (search "no pins" (cdar (cdr p:*help-visible*)))
             "an empty table says so rather than drawing an empty page")
    (r:pin-workspace 2 (screen "right"))
    (setf r:*workspace-pins* (acons 9 "gone" r:*workspace-pins*))
    (r:pins)
    (let ((rows (cdr p:*help-visible*)))
      (is (= 2 (length rows)))
      (is-true (some (lambda (row) (search "right" (cdr row))) rows))
      (is-true (some (lambda (row) (search "not plugged in" (cdr row))) rows)
               "a pin to a screen that is not here is reported as such")
      (is-true (some (lambda (row) (search "workspace 2" (car row))) rows)
               "and the row says which workspace"))
    (setf p:*help-visible* nil)))

(test the-pin-commands-exist
  (dolist (name '("pin-workspace" "unpin-workspace" "pins"))
    (is-true (p:find-command name) name)))

(test the-pin-table-is-an-option-a-config-file-writes
  (let ((row (find :workspace-pins (p:all-options)
                   :key (lambda (row) (first row)))))
    (is-true row "the declarative half is registered")
    (is (null (third row)) "and empty by default")))
