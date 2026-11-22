;;;; launcher.asd --- Everything you can start, behind one prompt.

(defsystem "launcher"
  :description "One searchable surface for applications, commands, projects and actions."
  :long-description
  "A launcher built out of the pieces this window manager already has: the
prompt, the command registry, the notification path and a resident Lisp.

    (load-extension \"launcher\")
    (launcher:launcher)            ; or bind it to a key
    (launcher:define-action \"notes\" \"Today's notes\"
      (lambda () (r:spawn \"emacs\" \"~/notes/today.md\")))

Candidates come from desktop entries, `$PATH', the projects registry,
arithmetic and your own actions.  A source is a function returning records;
adding one is a call to DEFINE-SOURCE, not an edit to the module.

There is no second daemon, no second window and no second configuration
language, which is the whole reason this is an extension."
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
   (:file "launcher")))

(defsystem "launcher/tests"
  :description "Tests for the launcher extension."
  :depends-on ("launcher" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-launcher"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
