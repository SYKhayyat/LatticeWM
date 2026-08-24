;;;; nested-cells.asd --- Other compositors as panes.

(defsystem "nested-cells"
  :description "Run a nested compositor, supervised, as an ordinary client."
  :long-description
  "LatticeWM runs nested inside river every day of development: any
compositor that nests is just another Wayland client.  This module makes
that a managed object -- a NAMED CELL with a supervised child process:

    (load-extension \"nested-cells\")
    (nested-cells:open-cell \"river\" '(\"river\"))

Closing the cell stops the child; a child that dies on its own is reported
by the supervision check.  Placement of the cell's window into a particular
pane composes with other modules (transient-rules, window-rules) rather
than duplicating them."
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
   (:file "nested-cells")))

(defsystem "nested-cells/tests"
  :description "Tests for the nested-cells extension."
  :depends-on ("nested-cells" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-nested-cells"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
