;;;; session-dump.asd --- The session as a dumped image.

(defsystem "session-dump"
  :description "Dump the running session to a core; next login resumes it."
  :long-description
  "LatticeWM already ships as a dumped SBCL image -- StumpWM's model.  This
module turns the same mechanism around: from inside a live session,

    M-x dump-session

writes the image -- extensions loaded, options changed, macros defined,
hooks attached -- and exits.  The next login resumes that image instead of
the stock one re-reading init.lisp:

    sbcl --core ~/.local/state/latticewm/session.core

The resumed image calls START again, which reconnects to the fresh river,
builds a new world and restores the saved layout.  Code and settings
persist; Wayland connections cannot, and do not pretend to."
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
   (:file "session-dump")))

(defsystem "session-dump/tests"
  :description "Tests for the session-dump extension."
  :depends-on ("session-dump" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-session-dump"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
