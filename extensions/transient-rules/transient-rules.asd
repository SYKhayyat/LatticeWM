;;;; transient-rules.asd --- Rules that fire once, for the next matching window.

(defsystem "transient-rules"
  :description "One-shot window rules: the NEXT window matching this, once."
  :long-description
  "A permanent rule says pavucontrol always floats.  A transient rule says the
NEXT window matching this floats, once, and then the rule is gone -- for
installers, dialogs that follow a main window, anything whose second window
differs from its first.

    (load-extension \"transient-rules\")
    (transient-rules:float-next \"steam\")     ; by app-id
    (transient-rules:add-rule \"steam\" :workspace 3)  ; any overrides

Entries wait in a queue until a window matches; non-matching windows leave the
queue untouched."
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
   (:file "transient-rules")))

(defsystem "transient-rules/tests"
  :description "Tests for one-shot transient rules."
  :depends-on ("transient-rules" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-transient-rules"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
