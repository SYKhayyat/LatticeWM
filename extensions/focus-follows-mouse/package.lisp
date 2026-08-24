;;;; focus-follows-mouse/package.lisp --- One package, three dependencies.
;;;;
;;;; The lattice's shape, at a tenth of its size: depend on the core's
;;;; exported packages and on nothing else of this project's.

(defpackage #:focus-follows-mouse
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Focus follows the pointer, but a floating window keeps its focus.

Two refinements over the shipped behaviour, both methods on the policy the
user already has:

  * POINTER-FOCUS ignores the pointer while it is over a float.  A float is
    deliberately on top of something; the pane underneath is exactly the thing
    you did not mean to focus.
  * ON-FOCUS-CHANGE warps the pointer along when focus moves by keyboard, so
    the pointer is never left sitting over one pane while the keyboard talks
    to another.

    (load-extension \"focus-follows-mouse\")
    (focus-follows-mouse:enable)")
  (:export #:enable #:disable #:enabled-p))
