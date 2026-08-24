;;;; scrolling-columns.asd --- niri's model, as a fifth container kind.
;;;;
;;;; The third promotion from EXTENSION-IDEAS.org's first section, and the
;;;; strongest of the four by its own argument: every additional layout model
;;;; outside src/ is another data point for the claim that the conventional
;;;; policy is not privileged.  This one is a whole CONTAINER KIND, not just
;;;; policy methods -- the thing the lattice is the two-dimensional version of.
;;
;;;; Unlike the example it was promoted from, this module answers the
;;;; persistence half of the container protocol for its own state (offset,
;;;; visible), because EXTENDING.org's table names what skipping those costs:
;;;; undo that skips past scroll positions and arrangements lost on restart.

(defsystem "scrolling-columns"
  :description "A scrolling strip of columns: niri's model as a container kind."
  :long-description
  "An unbounded horizontal strip of columns, of which a window's worth is on
screen at a time, scrolling sideways as you move.  The current workspace
becomes a strip; every window in it becomes a column, in the order it was
laid out.

    (load-extension \"scrolling-columns\")
    (scrolling-columns:scrolling)     ; or (scrolling-columns:scrolling 3)

STRIP-WIDTH changes how many columns are visible -- the strip's zoom."
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
   (:file "strip")))

(defsystem "scrolling-columns/tests"
  :description "Tests for the scrolling-columns strip."
  :depends-on ("scrolling-columns" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-scrolling-columns"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
