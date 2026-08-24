;;;; rules-as-methods.asd --- Rules stored as methods, ordered by CLOS.

(defsystem "rules-as-methods"
  :description "Window rules as eql-specialized methods, not table rows."
  :long-description
  "The rule table is a parallel mini-language.  This module dissolves it:
a rule IS a method, on a generic whose specializer is the application id
itself.

    (load-extension \"window-rules\")   ; no -- this module stands alone
    (load-extension \"rules-as-methods\")
    (rules-as-methods:enable)

    (rules-as-methods:define-app-id-rule \"pavucontrol\" :float t)
    (rules-as-methods:define-app-id-rule \"thunderbird\"
        :workspace 3 :focus nil)

Each rule is one method on RULE-OVERRIDES, eql-specialized on the app-id:
listable (every method is inspectable), removable by name or by method,
redefinable live from a REPL, with CLOS computing whatever ordering exists.
The bridge into placement is the runtime's :WINDOW-RULE hook, so no :AROUND
methods fight over specializers."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:file "package")
   (:file "rules-as-methods")))

(defsystem "rules-as-methods/tests"
  :description "Tests for the rules-as-methods extension."
  :depends-on ("rules-as-methods" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-rules-as-methods"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
