;;;; grid.asd --- n windows in a rectangle, as a container kind.

(defsystem "grid"
  :description "n windows in a rectangle of cells, in reading order."
  :long-description
  "The current workspace's windows become the cells of a grid with a chosen
number of columns, filled left to right and then down; the last row is short
rather than stretched.  Motion runs along a row and crosses into the next one.

    (load-extension \"grid\")
    (grid:grid)        ; as square as it goes
    (grid:grid 3)      ; three columns
    (grid:grid-columns 2)

GRID-COLUMNS is the grid's zoom, and the arrangement survives undo and a session
save because the module answers the state half of the container protocol."
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
   (:file "grid")))

(defsystem "grid/tests"
  :description "Tests for the grid container."
  :depends-on ("grid" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-grid"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
