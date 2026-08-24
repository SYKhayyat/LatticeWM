;;;; floating-only/package.lisp

(defpackage #:floating-only
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "A policy, not a hack: no layout at all.

    (floating-only:enable)

Every window floats, drag-and-resize everywhere, nothing snaps.  Expressed
as a policy -- one method answering \"does this float?\" with an
unconditional yes -- rather than a pile of :float rules.")
  (:export #:floating-only-policy
           #:enable #:disable #:enabled-p))
