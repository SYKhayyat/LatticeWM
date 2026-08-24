;;;; master-stack.asd --- dwm's layout, as a policy mixin.
;;;;
;;;; The second promotion from doc/EXTENSION-IDEAS.org's first section.  Like
;;;; focus-follows-mouse, this is example 03 made loadable; unlike it, the
;;;; module does not define a subclass of CONVENTIONAL-POLICY and replace
;;;; whatever policy was installed.  It is a MIXIN, composed over whatever is
;;;; in force -- including the lattice's -- by CHANGE-CLASS, per
;;;; EXTENDING.org §"Two extensions at once".  Switching layouts should not
;;;; have to mean abandoning every other extension's behaviour.
;;;;
;;;; The two rules: nothing here edits src/, and the core runs with this
;;;; system absent.

(defsystem "master-stack"
  :description "The master-and-stack layout, composed over the current policy."
  :long-description
  "One large pane on the left holding the window you are working in, and
everything else stacked in a column beside it -- dwm's layout, xmonad's
default.  The tree is untouched; only the answer to \"how does a split divide
its rectangle\" changes, so switching layouts loses nothing.

    (load-extension \"master-stack\")
    (master-stack:enable)     ; or the \"master-stack\" command

and back with (master-stack:disable).  Because it composes over whatever
policy is installed rather than replacing it, it works under the lattice too."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("latticewm" "closer-mop")
  :serial t
  :components
  ((:file "package")
   (:file "master-stack")))

(defsystem "master-stack/tests"
  :description "Tests for the master-and-stack layout module."
  :depends-on ("master-stack" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-master-stack"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
