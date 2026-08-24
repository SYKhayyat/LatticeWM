;;;; focus-follows-mouse.asd --- Focus follows the pointer, except over floats.
;;;;
;;;; The first of the promoted examples: doc/EXTENSION-IDEAS.org's "Promotions"
;;;; section, made real.  examples/01-focus-follows-mouse.lisp stays where it
;;;; is, because it is the file a newcomer reads to learn the shape; this is
;;;; the file a user loads to *have* the behaviour, without copying Lisp out of
;;;; a teaching directory.
;;;;
;;;; The two rules every extension here holds itself to, lattice.asd's own:
;;;;
;;;;   1. Nothing in this directory edits anything in src/.
;;;;   2. The core loads, starts and runs with this system absent.

(defsystem "focus-follows-mouse"
  :description "Focus follows the pointer, but a floating window keeps its focus."
  :long-description
  "The shipped option *focus-follows-mouse* moves focus wherever the pointer
goes.  This module adds the refinement people want after a day of it: the
pointer passing over the tiled panes underneath a floating window does not
steal focus from the float, and keyboard motion warps the pointer along so the
two never disagree about where focus is.

    (load-extension \"focus-follows-mouse\")
    (focus-follows-mouse:enable)

and back off again with (focus-follows-mouse:disable), live, no restart."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  ;; Its own version, per EXTENDING.org §versioning: the extensions are not in
  ;; the flagship's position of shipping from one VERSION file with the core.
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:file "package")
   (:file "focus-follows-mouse")))

(defsystem "focus-follows-mouse/tests"
  :description "Tests for the focus-follows-mouse refinement."
  :depends-on ("focus-follows-mouse" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-focus-follows-mouse"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
