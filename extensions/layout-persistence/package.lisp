;;;; layout-persistence/package.lisp

(defpackage #:layout-persistence
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Named workspace layouts, matched across restarts by app-id.

    (save-layout \"writing\")     ; the workspace under the cursor
    (restore-layout \"writing\")  ; ...brought back tomorrow, by name

Restore is :best-effort by default -- missing applications leave empty panes
-- or :exact, which refuses unless the arrangement can be honoured whole.")
  (:export #:*mode*
           #:*save-on-change*
           #:*layouts-directory*
           #:enable #:disable #:enabled-p
           #:save-layout #:restore-layout #:delete-layout
           #:all-layout-names
           #:restore-into-workspace))
