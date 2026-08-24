;;;; master-stack/package.lisp

(defpackage #:master-stack
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "The master-and-stack layout, composed over the current policy.

A MIXIN, not a replacement: ENABLE composes MASTER-STACK-MIXIN in front of
whatever policy is installed -- conventional, the lattice's, or a later
extension's composed form -- by CHANGE-CLASS under a derived name, and
DISABLE changes back to the class that was underneath.  The tree is never
touched; only LAYOUT-CHILDREN and STEP-ADDRESS answer differently, so
switching layouts moves no window.

    (load-extension \"master-stack\")
    (master-stack:enable)

Knobs: RATIO (the master column's share of the width) and MASTERS (how many
panes share it), with commands MASTER-WIDER, MASTER-NARROWER, MORE-MASTERS
and FEWER-MASTERS.")
  (:export #:master-stack-mixin
           #:master-ratio #:master-count
           #:enable #:disable #:enabled-p))
