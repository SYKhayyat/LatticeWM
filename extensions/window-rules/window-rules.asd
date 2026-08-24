;;;; window-rules.asd --- Function-match window rules with workspace placement.

(defsystem "window-rules"
  :description "Window rules where a match may be any predicate of the window."
  :long-description
  "The built-in *WINDOW-RULES* table matches on app-id and title strings.  This
module adds the front the tier-2 user actually wants: a rule whose MATCH is
any function of the window -- so a predicate can look at anything, consult
anything, and change at runtime -- plus :WORKSPACE placement, which the
shipped table leaves to the policy.

    (load-extension \"window-rules\")
    (window-rules:enable)

then add rules to WINDOW-RULES:*RULES*, most specific first; the first match
wins, and an unmatched window behaves exactly as before."
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
   (:file "window-rules")))

(defsystem "window-rules/tests"
  :description "Tests for the window-rules module."
  :depends-on ("window-rules" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-window-rules"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
