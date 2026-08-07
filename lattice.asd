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
  :license "GPL-3.0-or-later"
  ;; The same VERSION file the core reads, and deliberately not a version of
  ;; its own.  The lattice is a separate *system* so that the core can be built
  ;; without it; it is not a separately *released* thing, and giving it a
  ;; number that can drift from the core's would invent a compatibility
  ;; question nobody has asked.  See latticewm.asd for why the file exists.
  :version (:read-file-line "VERSION")
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
     (:file "map")
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
