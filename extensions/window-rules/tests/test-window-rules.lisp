;;;; tests/test-window-rules.lisp --- The table, the doors, the switch.

(defpackage #:window-rules/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:wr #:window-rules))
  (:export #:run-all))

(in-package #:window-rules/tests)

(def-suite window-rules :description "Function-match window rules.")
(in-suite window-rules)

(t*:register-extension-suite "WINDOW-RULES/TESTS" "WINDOW-RULES")

(defun run-all ()
  "Run the WINDOW-RULES suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'window-rules)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defmacro with-rules ((&rest rules) &body body)
  "A fresh world with RULES installed and enabled, restored afterwards."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy))
         (wr::*rules* ',rules)
         (wr::*enabled* nil))
     (wr:enable)
     (unwind-protect (progn ,@body) (wr:disable))))

;;; ================================================================ tests

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))


(test a-string-match-floats
  (with-rules (("pavucontrol" :float t))
    (let ((win (make-instance 'c:window :app-id "pavucontrol")))
      (is (equal '(:float t) (p:window-rule-for p:*policy* win)))
      (p:on-window-open p:*policy* r:*world* win)
      (is-true (c:window-floating-p win))
      (is (equal '(:stack 0 (:leaf nil)) (t*::shape (c:world-root r:*world*)))
          "a floated window never entered the tree"))))

(test a-function-match-looks-at-the-window
  "The whole point of the module over the built-in string table: the match is
a predicate, so it can look at anything about the window.  The rule is built
at runtime -- a QUOTED lambda is a cons, not a function."
  (let ((r:*world* (c:make-world))
        (p:*policy* (make-instance 'p:conventional-policy))
        (wr::*enabled* nil))
    (setf wr::*rules*
          (list (cons (lambda (win) (equal "big" (c:window-app-id win)))
                      '(:float t))))
    (wr:enable)
    (unwind-protect
         (let ((win (make-instance 'c:window :app-id "big")))
           (is (equal '(:float t) (p:window-rule-for p:*policy* win))))
      (wr:disable))))

(test an-unmatched-window-tiles-and-falls-through
  (with-rules (("pavucontrol" :float t))
    (let ((plain (make-instance 'c:window :app-id "anything-else")))
      (is-false (p:window-rule-for p:*policy* plain)
                "no rule matched, so the shipped answer stands")
      (p:on-window-open p:*policy* r:*world* plain)
      (is-false (c:window-floating-p plain)))))

(test a-workspace-rule-creates-and-places-without-stealing-focus
  "The half the shipped policy has no opinion about.  The cursor goes back to
where it was, because the rule said :FOCUS NIL -- placement without capture."
  (with-rules (("thunderbird" :workspace 2 :focus nil))
    (open-windows 1)
    (let ((cursor-before (c:world-cursor r:*world*))
          (win (make-instance 'c:window :app-id "thunderbird")))
      (p:on-window-open p:*policy* r:*world* win)
      (is (<= 3 (c:container-count (c:world-root r:*world*)))
          "the workspace was created")
      (is (equal "thunderbird"
                 (let ((leaf (c:leaf-holding (c:world-root r:*world*) win)))
                   (and leaf (c:window-app-id (c:leaf-window leaf)))))
          "and the window is in it")
      ;; The rule said :FOCUS NIL, so the cursor went back to where it was.
      (is (c:path-equal cursor-before (c:world-cursor r:*world*))
          "focus was not stolen"))))
(test disable-really-stops-the-rules
  "The switch is exact: nothing was replaced, so disabling leaves exactly the
shipped behaviour, rules still in the table for the next enable."
  (with-rules (("pavucontrol" :float t))
    (let ((win (make-instance 'c:window :app-id "pavucontrol")))
      (wr:disable)
      (is-false (p:window-rule-for p:*policy* win)
                "disabled, the table is not consulted")
      (wr:enable)
      (is (equal '(:float t) (p:window-rule-for p:*policy* win))
          "and re-enabling puts it back to work"))))
