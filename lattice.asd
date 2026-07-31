;;;; lattice.asd --- The lattice, as a separate system.
;;;;
;;;; It is a separate system on purpose, and the purpose is falsifiability.
;;;;
;;;; PLAN.org Delta 2: "the lattice lives in a separate ASDF system that
;;;; depends only on the core's exported policy package.  Core loads, starts
;;;; and runs with the lattice absent.  Any core edit the lattice demands shows
;;;; up as a failing gate, on the day it happens, from day one.  This keeps
;;;; D21's falsifiability and removes its scheduling risk.  It costs nothing."
;;;;
;;;; Two rules, and they are the project:
;;;;
;;;;   1. Nothing in lattice/ may edit anything in src/.
;;;;   2. The core must load, start and run with this system absent.
;;;;
;;;; Both are checked by tools/gates.lisp, gates 3 and 4, on every build.

(defsystem "lattice"
  :description "An infinite zoomable plane of cells, as an extension to LatticeWM."
  :author "Shaul Khayyat"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:module "lattice"
    :serial t
    :components
    ((:file "package")
     (:file "grid")
     (:file "policy")
     (:file "commands")
     (:file "overlay")))))

(defsystem "lattice/tests"
  :description "Tests for the lattice, and for its interoperation with splits."
  :depends-on ("lattice" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "test-lattice"))))
  :perform (test-op (o c) (symbol-call :lattice/tests :run-all)))
