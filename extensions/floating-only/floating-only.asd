;;;; floating-only.asd --- A policy, not a hack: no layout at all.

(defsystem "floating-only"
  :description "Every window floats; the tiler never tiles."
  :long-description
  "Some users do not want tiling at all.  If the layout model is genuinely a
replaceable policy, then \"no layout\" must be expressible AS a policy -- a
placement answer that declines to tile -- rather than a pile of :float rules
bolted onto a tiler that still wants to tile.  This module is that proof:

    (load-extension \"floating-only\")
    (floating-only:enable)

One class, one overridden method answering \"does this float?\" with an
unconditional yes.  The conventional tree is never entered."
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
   (:file "floating-only")))

(defsystem "floating-only/tests"
  :description "Tests for the floating-only policy."
  :depends-on ("floating-only" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-floating-only"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
