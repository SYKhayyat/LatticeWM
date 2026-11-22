;;;; tests/test-outputs.lisp --- Monitors (#8).
;;;;
;;;; Most of what the issue asked for was already true, and the reason is the
;;;; model: one stack of workspaces and one viewport per output.  A number
;;;; means the same workspace on every screen because there is one stack, and
;;;; "mirror" in the sense people mean it is impossible for a Wayland client
;;;; (#96, #125).  What was missing was the verb for `this workspace belongs
;;;; over there' and the report that says which screen shows what -- and these
;;;; are those, checked with no compositor, because an output is a rectangle
;;;; and a name.

(in-package #:latticewm/tests)
(in-suite outputs)

(defmacro with-two-screens (&body body)
  "A world with two monitors side by side and the cursor on the left one."
  (let ((world (gensym "WORLD")) (left (gensym "LEFT")) (right (gensym "RIGHT")))
    `(multiple-value-bind (,world ,left ,right) (two-output-world)
       (let ((r:*world* ,world) (p:*policy* (policy)) (p:*seat* nil)
             (p:*echo-message* nil))
         (setf (c:world-cursor ,world) '(0 0))
         ,@body))))

;;; ---------------------------------------------------------- moving a workspace

(test moving-a-workspace-puts-it-on-the-other-screen
  "The workspace moves whole -- a workspace is one node of one tree, so there
is nothing to take apart -- and the screen it left trades for what the other
one was showing."
  (with-two-screens
    (r:workspace 2)
    (let ((here (r::cursor-output)))
      (is (= 1 (r:move-workspace-to-output :right))
          "workspace two (index one) is the one that moved")
      (is (= 1 (c:prop (second (c:world-outputs r:*world*)) :workspace))
          "the right-hand screen is showing it now")
      (is (= 0 (c:prop (first (c:world-outputs r:*world*)) :workspace))
          "and the screen it left traded for workspace one, rather than going
blank -- SHOW-WORKSPACE-ON's answer, which is the whole of the command")
      (is (eq here (r::cursor-output))
          "without :follow the keyboard stays where it was"))))

(test moving-a-workspace-with-follow-takes-the-keyboard-along
  (with-two-screens
    (r:workspace 2)
    (r:move-workspace-to-output :right :follow t)
    (is (eq (second (c:world-outputs r:*world*)) (r::cursor-output))
        "the cursor crossed the seam with the work it was in")))

(test moving-a-workspace-needs-a-screen-that-way
  (with-two-screens
    (is (null (r:move-workspace-to-output :left))
        "there is nothing to the left, and the answer is not a moved workspace")
    (is (search "no screen" (p:current-message)))
    (is (null (r:move-workspace-to-output :nowhere)))
    (is (= 0 (c:prop (first (c:world-outputs r:*world*)) :workspace))
        "nothing moved")))

(test every-screen-still-shows-a-workspace-afterwards
  "The invariant #96 protects: two outputs may show one index only while they
are one shared group.  A move is a trade, so it cannot break that."
  (with-two-screens
    (r:move-workspace-to-output :right)
    (let ((indices (mapcar (lambda (output) (c:prop output :workspace))
                           (c:world-outputs r:*world*))))
      (is (= 2 (length indices)))
      (is (= 2 (length (remove-duplicates indices)))
          "no two screens on one workspace"))))

;;; ---------------------------------------------------------------- the report

(test list-outputs-says-what-each-screen-is-showing
  (with-two-screens
    (r:workspace 2)
    (let ((r::*help-visible* nil))
      (is (= 2 (length (r:list-outputs))))
      (is (consp r::*help-visible*) "the page is up")
      (let ((rows (cdr r::*help-visible*))
            (text nil))
        (is (= 2 (length rows)))
        (setf text (format nil "~s" rows))
        (is (search "left" text))
        (is (search "right" text))
        (is (search "1000x800" text) "the size of each screen")
        (is (search "workspace 2" text) "and what it is showing")
        (is (search "*" (caar rows))
            "the cursor's screen is marked")))))

(test list-outputs-with-no-screens-says-so
  (let ((r:*world* (c:make-world)) (p:*policy* (policy)) (p:*echo-message* nil))
    (let ((r::*help-visible* nil))
      (is (null (r:list-outputs)))
      (is (null r::*help-visible*) "no page for no monitors")
      (is (search "no outputs" (p:current-message))))))

(test a-shared-pair-is-named-in-the-report
  "The report is where somebody looks when two monitors are behaving as one, so
it has to say so -- and it says it with the name of the other member."
  (with-two-screens
    (r:share-workspace :right)
    (let* ((r::*help-visible* nil)
           (rows (cdr (progn (r:list-outputs) r::*help-visible*))))
      (is (find-if (lambda (row) (search "sharing with" (cdr row))) rows)
          "one of the rows says the screens are one plane"))))

(test the-output-commands-exist
  (is-true (p:find-command "move-workspace-to-output"))
  (is-true (p:find-command "list-outputs"))
  (is-true (p:find-command "share-workspace"))
  (is-true (p:find-command "unshare-workspace"))
  (is-true (p:find-command "focus-output")))
