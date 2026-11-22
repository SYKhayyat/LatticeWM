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
-- or :exact, which refuses unless the arrangement can be honoured whole.

A saved arrangement is also a library: LIST-LAYOUTS shows what you have,
FORGET-LAYOUT and RENAME-LAYOUT manage it, USE-LAYOUT opens one in a workspace
of its own (leaving this one alone) and SPAWN-LAYOUT does the same while
starting the applications the arrangement names and the session does not
have.")
  (:export #:*mode*
           #:*save-on-change*
           #:*layouts-directory*
           #:enable #:disable #:enabled-p
           #:save-layout #:restore-layout #:delete-layout
           #:all-layout-names
           #:restore-into-workspace
           ;; #10: the library half -- seeing the names, throwing one away,
           ;; renaming one, and opening one in a workspace of its own.
           #:list-layouts #:forget-layout #:rename-layout
           #:use-layout #:spawn-layout #:layout-into-new-workspace
           #:spawn-missing-applications #:layout-summary #:layout-form
           #:*spawn-function*))
