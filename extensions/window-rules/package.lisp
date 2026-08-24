;;;; window-rules/package.lisp

(defpackage #:window-rules
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Window rules where a match may be any predicate of the window.

Each entry of *RULES* is (MATCH . OVERRIDES): MATCH is an app-id string or a
function of the window; OVERRIDES is a plist -- :FLOAT, :WORKSPACE, :FOCUS.
First match wins.  An unmatched window is placed exactly as the shipped policy
would place it.

    (window-rules:enable)
    (push '((lambda (win) (< (window-area win) 400)) :float t) window-rules:*rules*)")
  (:export #:*rules* #:rule-for #:enable #:disable #:enabled-p))
