;;;; declared-sessions.asd --- Sessions declared forward, not remembered backward.

(defsystem "declared-sessions"
  :description "A Lisp manifest of workspaces and applications, loaded on demand."
  :long-description
  "Restore-style persistence answers \"what was open when I left\".  This
module answers \"what SHOULD be open\": a text file -- Lisp, because the
manifest is code when you want it and data when you don't -- naming
workspaces, applications and placement:

    ;; ~/.config/latticewm/sessions/work.lisp
    (workspace 1
      (split :horizontal
             (app \"emacsclient -c\")
             (app \"foot\")))

    M-x load-session  (or (declared-sessions:load-session \"work\"))

The skeleton is built first as empty panes; each application is spawned and,
when its window arrives, placed into the pane that was waiting for it.  A
window that never starts leaves an honest empty pane rather than a broken
session."
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
   (:file "declared-sessions")))

(defsystem "declared-sessions/tests"
  :description "Tests for the declared-sessions extension."
  :depends-on ("declared-sessions" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-declared-sessions"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
