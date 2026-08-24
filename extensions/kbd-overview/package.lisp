;;;; kbd-overview/package.lisp

(defpackage #:kbd-overview
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Zoom out to a keyboard of windows; type to navigate.

    (define-key *keymap* \"Super+k\" '(\"toggle-kbd-overview\"))

A letter GOes to its window; SHIFT+letter PULL-marks it; RET gathers the
marked ones where you started; ESC cancels.  Snap-back restores the exact
prior trees.")
  (:export #:*letter-layout*
           #:*windows-function*
           #:active-p
           #:enter-overview
           #:exit-overview
           #:assignments
           #:toggle-kbd-overview-command
           #:overview-keymap))
