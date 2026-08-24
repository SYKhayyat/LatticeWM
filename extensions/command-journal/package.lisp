;;;; command-journal/package.lisp

(defpackage #:command-journal
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Every state-changing command, recorded and replayable.

    (start-journal) ... do things ... (stop-journal)
    (replay-journal)                 ; now, or in tomorrow's session

One wrapper on the one command path is the whole mechanism; the journal is
storage discipline over it.")
  (:export #:*recording*
           #:*journal*
           #:*excluded-commands*
           #:*journals-directory*
           #:enable #:disable #:enabled-p
           #:start-journal #:stop-journal
           #:clear-journal
           #:replay-journal
           #:save-journal #:load-journal
           #:all-journal-names
           #:journal-entry-count))
