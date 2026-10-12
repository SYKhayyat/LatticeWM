;;;; auto-respawn.asd --- Pick up where you left off.

(defsystem "auto-respawn"
  :description "Record what launched each window and bring the session back."
  :long-description
  "The core saves the session's layout and reads it back at startup keyed on
river window identifiers, which survive a window manager restart but not a
reboot.  layout-persistence matches a saved arrangement to windows that are
*already running*; declared-sessions rebuilds a session from a manifest the
user writes by hand.  This module fills the gap in between: it *records* what
launched each window while you work, saves that beside the per-workspace
arrangement keyed by application id, and RESPAWN-SESSION runs those commands
again into the panes they left -- no manifest, no memory.

    (load-extension \"auto-respawn\")
    (auto-respawn:enable)
    ;; next login — or now:
    (auto-respawn:respawn-session)"

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
   (:file "auto-respawn")))

(defsystem "auto-respawn/tests"
  :description "Tests for the auto-respawn extension."
  :depends-on ("auto-respawn" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-auto-respawn"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))