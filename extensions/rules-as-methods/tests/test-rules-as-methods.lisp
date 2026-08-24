;;;; tests/test-rules-as-methods.lisp --- A rule is a method, and acts like one.

(defpackage #:rules-as-methods/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ram #:rules-as-methods))
  (:export #:run-all))

(in-package #:rules-as-methods/tests)

(def-suite rules-as-methods :description "Rules stored as methods.")
(in-suite rules-as-methods)

(t*:register-extension-suite "RULES-AS-METHODS/TESTS" "RULES-AS-METHODS")

(defun run-all ()
  "Run the RULES-AS-METHODS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'rules-as-methods)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-rules (&body body)
  `(let ((ram::*enabled* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ================================================================ tests

(test define-makes-a-method-that-placement-sees
  "DEFINE-APP-ID-RULE expands to a method on an eql specializer, and the
bridge carries its answer through :WINDOW-RULE into placement."
  (with-rules
    (ram:define-app-id-rule "pavucontrol" :float t)
    (let ((win (make-instance 'c:window :app-id "pavucontrol")))
      (is (equal '(:float t) (ram:rule-overrides "pavucontrol"))
          "the method answers directly")
      (ram:enable)
      (unwind-protect
           (progn
             ;; Through the hook, the way placement asks.
             (is (equal '(:float t)
                        (first (remove nil (p:run-hooks :window-rule win))))
                 "the bridge answered for the window")
             ;; And through window-rule-for itself.
             (is (equal '(:float t) (p:window-rule-for p:*policy* win))))
        (ram:disable)))))

(test unmatched-applications-get-nil-and-no-crash
  "An application with no rule falls through: NIL from the generic, NIL out
of the bridge."
  (with-rules
    (ram:define-app-id-rule "pavucontrol" :float t)
    (is-false (ram:rule-overrides "firefox"))
    (ram:enable)
    (unwind-protect
         ;; RUN-HOOKS returns every answer as a list, so the honest check is
         ;; that no non-NIL answer appears -- what placement itself does.
         (is-false (remove nil (p:run-hooks :window-rule
                                            (make-instance 'c:window
                                                           :app-id "firefox"))))
      (ram:disable))))

(test redefining-replaces-live
  "Defining the same app-id again replaces the method -- CLOS's own
semantics, which is the whole argument for rules as methods."
  (with-rules
    (ram:define-app-id-rule "pavucontrol" :float t)
    (ram:define-app-id-rule "pavucontrol" :workspace 2)
    (is (= 1 (length (ram:all-rules))) "one rule, not two")
    (is (equal '(:workspace 2) (ram:rule-overrides "pavucontrol")))))

(test remove-takes-the-method-away
  "REMOVE-APP-ID-RULE removes the METHOD; the generic goes back to default."
  (with-rules
    (ram:define-app-id-rule "pavucontrol" :float t)
    (is (equal "pavucontrol" (ram:remove-app-id-rule "pavucontrol")))
    (is-false (ram:rule-overrides "pavucontrol"))
    (is-false (ram:remove-app-id-rule "never-existed"))))

(test all-rules-lists-the-truth
  "ALL-RULES reads the generic's methods, sorted, so the listing and the
truth are the same object rather than two copies."
  (with-rules
    (ram:define-app-id-rule "zeta" :float t)
    (ram:define-app-id-rule "alpha" :float t)
    (is (equal '("alpha" "zeta") (mapcar #'first (ram:all-rules))))))

(test disable-stops-consulting-but-forgets-nothing
  "DISABLE unhooks the bridge; the defined methods survive, because stopping
consulting is not forgetting."
  (with-rules
    (ram:define-app-id-rule "pavucontrol" :float t)
    (ram:enable)
    (ram:disable)
    (is-true (ram:rule-overrides "pavucontrol"))
    (is-false (member 'ram::rule-hook-bridge
                      (gethash :window-rule p:*hooks*)))))
