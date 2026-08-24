;;;; projects/package.lisp

(defpackage #:projects
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Workspaces bound to directories.

    (define-project \"lattice\" \"~/src/latticewm\")
    (switch-to-project \"lattice\")     ; a workspace labelled \"lattice\"

While the cursor is on a project's workspace, everything spawned -- terminal,
editor, a key on an empty pane -- starts in that project's directory.  The
binding IS the label: any workspace named after a registered project is that
project.")
  (:export #:*projects*
           #:enable #:disable #:enabled-p
           #:define-project #:remove-project
           #:project-directory
           #:all-project-names
           #:current-project-name #:current-project-directory
           #:ensure-project-workspace
           #:switch-to-project))
