;;;; layout-persistence.asd --- Arrangements that survive logout, and then some.

(defsystem "layout-persistence"
  :description "Named workspace layouts, matched across restarts by app-id."
  :long-description
  "The core saves the session's layout automatically and reads it back at
startup, matching windows by river identifier -- which survives a window
manager restart but not a reboot.  This module adds the two things that
outlive identifiers:

  * named layouts for the workspace under the cursor, saved as files keyed
    by application id, so the same arrangement comes back tomorrow;

    (save-layout \"writing\")
    (restore-layout \"writing\")

  * a strictness dial for restore.  :BEST-EFFORT (the default) places every
    window it can and leaves empty panes where an application has not come
    back yet; :EXACT refuses unless the arrangement can be honoured whole --
    half a remembered layout is worse than none, if you say so."

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
   (:file "layout-persistence")))

(defsystem "layout-persistence/tests"
  :description "Tests for the layout-persistence extension."
  :depends-on ("layout-persistence" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-layout-persistence"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
