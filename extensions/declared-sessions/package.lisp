;;;; declared-sessions/package.lisp

(defpackage #:declared-sessions
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Sessions declared forward, not remembered backward.

    ;; sessions/work.lisp
    (workspace 1
      (split :horizontal
             (app \"emacsclient -c\")
             (app \"foot\")))

The skeleton is built as empty panes; applications are spawned into the
panes that wait for them.")
  (:export #:*sessions-directories*
           #:load-session
           #:session-file
           #:all-session-names
           #:pending-arrivals))
