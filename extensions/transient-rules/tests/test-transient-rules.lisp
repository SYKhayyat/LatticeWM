;;;; tests/test-transient-rules.lisp --- Once means once.

(defpackage #:transient-rules/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:tr #:transient-rules))
  (:export #:run-all))

(in-package #:transient-rules/tests)

(def-suite transient-rules :description "One-shot window rules.")
(in-suite transient-rules)

(t*:register-extension-suite "TRANSIENT-RULES/TESTS" "TRANSIENT-RULES")

(defun run-all ()
  "Run the TRANSIENT-RULES suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'transient-rules)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-queue (&body body)
  `(let ((tr::*queue* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ================================================================ tests

(test the-rule-fires-exactly-once
  "The whole feature: first matching window gets the overrides, second is an
ordinary window, queue is empty.  The entry is consumed by the consult that
placement itself makes, so the test opens the window rather than asking
WINDOW-RULE-FOR first -- asking first would BE the consumption."
  (with-queue
    (tr:add-rule "installer" :float t)
    (let ((first (make-instance 'c:window :app-id "installer")))
      (p:on-window-open p:*policy* r:*world* first)
      (is-true (c:window-floating-p first))
      (is (= 0 (length tr::*queue*)) "consumed by the first placement")))
    (let ((second (make-instance 'c:window :app-id "installer")))
      (is-false (p:window-rule-for p:*policy* second)
                "the entry was consumed")
      (p:on-window-open p:*policy* r:*world* second)
      (is-false (c:window-floating-p second)
                "and the second window tiles like anything else")))

(test non-matching-windows-leave-the-queue-alone
  "An entry waits for ITS window; other windows pass through and the rule
stays armed."
  (with-queue
    (tr:add-rule "installer" :float t)
    (let ((other (make-instance 'c:window :app-id "firefox")))
      (is-false (p:window-rule-for p:*policy* other))
      (is (= 1 (length tr::*queue*)) "still armed")))
  ;; A function match fires on its own predicate.
  (with-queue
    (tr:add-rule (lambda (win) (equal "big" (c:window-app-id win))) :float t)
    (let ((small (make-instance 'c:window :app-id "small"))
          (big (make-instance 'c:window :app-id "big")))
      (is-false (p:window-rule-for p:*policy* small))
      (is (= 1 (length tr::*queue*)))
      (is (equal '(:float t) (p:window-rule-for p:*policy* big)))
      (is (= 0 (length tr::*queue*))))))

(test first-match-wins-and-consumption-is-per-entry
  "Two waiting rules, both matching one window: only the first fires, and the
second stays for the NEXT window."
  (with-queue
    (tr:add-rule "dialog" :workspace 3)
    (tr:add-rule "dialog" :float t)
    (let ((first (make-instance 'c:window :app-id "dialog")))
      ;; Placement consumes one entry per window; ask for the overrides the way
      ;; placement does, once, and check which one came back.
      (is (equal '(:float t)
                 (p:window-rule-for p:*policy* first))
          "most recently added wins -- PUSH order")
      (is (= 1 (length tr::*queue*)) "the other stays armed"))))

(test clear-disarms-everything
  (with-queue
    (tr:add-rule "a" :float t)
    (tr:add-rule "b" :float t)
    (tr:clear-rules)
    (is (= 0 (length tr::*queue*)))
    (let ((win (make-instance 'c:window :app-id "a")))
      (is-false (p:window-rule-for p:*policy* win)))))

(test a-workspace-rule-places-without-stealing-focus
  (with-queue
    (open-windows 2)
    (setf (c:world-cursor r:*world*) '(0 1))
    (let ((before (c:world-cursor r:*world*)))
      (tr:add-rule "thunderbird" :workspace 3 :focus nil)
      (p:on-window-open p:*policy* r:*world*
                        (make-instance 'c:window :app-id "thunderbird"))
      (is (<= 3 (c:container-count (c:world-root r:*world*)))
          "the workspace was created -- workspaces count from one")
      (is (c:path-equal before (c:world-cursor r:*world*))
          "and focus was not stolen"))))

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))
