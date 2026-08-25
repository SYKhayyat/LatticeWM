;;;; tabs.asd --- Many windows in one pane, switched like editor tabs.

(defsystem "tabs"
  :description "Tab groups inside a split or cell: one pane, many windows."
  :long-description
  "A tab group is a pane that holds several windows and shows one at a
time -- browser/editor tabs for the tree.

    (load-extension \"tabs\")
    (tabs:enable)
    (define-key *keymap* \"Super+t\" '(\"tab-add\"))
    (define-key *keymap* \"Super+Left\" '(\"tab-prev\"))
    (define-key *keymap* \"Super+Right\" '(\"tab-next\"))
    (define-key *keymap* \"Super+d\" '(\"untab\"))

TAB-HERE makes the focused pane a group; TAB-ADD pulls a live window into
it; TAB-NEXT / TAB-PREV cycle; UNTAB pops the visible window back out as an
ordinary pane.  Hidden members are simply unplaced, which the runtime
already renders as invisible.  Switching is not an undo step by default
(*undo-includes-tab-switches*)."
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
   (:file "tabs")))

(defsystem "tabs/tests"
  :description "Tests for the tabs extension."
  :depends-on ("tabs" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-tabs"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
