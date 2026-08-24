;;;; keyboard-macros.asd --- Record a sequence of commands; play it back.

(defsystem "keyboard-macros"
  :description "Record command sequences and play them back, vim-style."
  :long-description
  "Every user action routes through one command path, so recording is
interception at a single choke point rather than input synthesis:

    (load-extension \"keyboard-macros\")
    (keyboard-macros:enable)
    (define-key *keymap* \"Super+x (\" '(\"start-macro\"))
    (define-key *keymap* \"Super+x )\" '(\"stop-macro\"))
    (define-key *keymap* \"Super+x p\" '(\"play-macro\"))

Do things between the parens; =Super+x p= does them again.  Sequences over
arbitrary commands, prompts included -- the prompt's answer is part of the
arguments the command was given."
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
   (:file "keyboard-macros")))

(defsystem "keyboard-macros/tests"
  :description "Tests for the keyboard-macros extension."
  :depends-on ("keyboard-macros" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-keyboard-macros"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
