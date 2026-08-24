;;;; window-restarts/package.lisp

(defpackage #:window-restarts
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "When an application exits unexpectedly: retry, undo, dismiss.

    (define-key *keymap* \"Super+Escape\" window-restarts:*menu*)

A window that goes away without your having closed it is a BROKEN-WINDOW
condition and a menu: R respawns the application, U reverts the last layout
change, D carries on.")
  (:export #:broken-window
           #:broken-app-id
           #:*menu*
           #:*user-close-commands*
           #:enable #:disable #:enabled-p
           #:last-broken-app-id
           #:retry-broken-window
           #:undo-for-broken-window
           #:dismiss-broken-window))
