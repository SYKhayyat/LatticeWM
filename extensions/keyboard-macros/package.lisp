;;;; keyboard-macros/package.lisp

(defpackage #:keyboard-macros
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Record a sequence of commands; play it back.

    (define-key *keymap* \"Super+x (\" '(\"start-macro\"))
    (define-key *keymap* \"Super+x )\" '(\"stop-macro\"))
    (define-key *keymap* \"Super+x p\" '(\"play-macro\"))

One wrapper on the command path is the whole mechanism.")
  (:export #:*macro*
           #:*macros*
           #:*excluded-commands*
           #:enable #:disable #:enabled-p
           #:recording-p
           #:start-macro #:stop-macro
           #:play-macro
           #:save-macro
           #:all-macro-names
           #:delete-macro))
