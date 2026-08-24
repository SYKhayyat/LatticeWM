;;;; nested-cells/package.lisp

(defpackage #:nested-cells
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Other compositors as panes.

    (open-cell \"river\" '(\"river\"))

A named cell is a supervised child compositor, connected to this session's
Wayland display like any other client.  CLOSE-CELL stops it; the
supervision check reports children that died on their own.")
  (:export #:*cells*
           #:enable #:disable #:enabled-p
           #:open-cell #:close-cell
           #:all-cell-names
           #:cell-alive-p
           #:check-cells))
