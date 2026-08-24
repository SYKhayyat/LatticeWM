;;;; command-journal.asd --- Every state-changing command, recorded and replayable.

(defsystem "command-journal"
  :description "Record the commands you ran; replay them into any session."
  :long-description
  "Every user action routes through one command path -- keys, socket forms,
SLIME -- so recording is interception at a single choke point.  Start a
journal, do things, stop; replay the journal into this or any future
session:

    (load-extension \"command-journal\")
    (command-journal:enable)
    M-x start-journal ... M-x stop-journal
    M-x replay-journal

Journals are plain lists of (COMMAND . ARGUMENTS) forms, saved where layouts
are kept, readable and editable because that is what plain means."
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
   (:file "command-journal")))

(defsystem "command-journal/tests"
  :description "Tests for the command-journal extension."
  :depends-on ("command-journal" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-command-journal"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
