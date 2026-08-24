;;;; projects.asd --- Workspaces bound to directories.

(defsystem "projects"
  :description "Workspaces bound to directories: spawning there starts there."
  :long-description
  "A project is a folder.  This extension binds a workspace to one: a
terminal opened on that workspace starts in the directory, the editor opens
it, and switching projects switches everything at once.

    (load-extension \"projects\")
    (projects:enable)
    (projects:define-project \"lattice\" \"~/src/latticewm\")
    (projects:switch-to-project \"lattice\")

A project is any workspace labelled with the project's name; the label is
the whole binding, so workspaces you already have can become projects by
being named after them."
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
   (:file "projects")))

(defsystem "projects/tests"
  :description "Tests for the projects extension."
  :depends-on ("projects" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-projects"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
