;;;; rules-as-methods/package.lisp

(defpackage #:rules-as-methods
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Window rules as eql-specialized methods.

    (define-app-id-rule \"pavucontrol\" :float t)

Each rule is a method on RULE-OVERRIDES whose specializer is the application
id string.  Listable, removable, redefinable live -- the table stops being a
mini-language and becomes the language.")
  (:export #:rule-overrides
           #:define-app-id-rule
           #:remove-app-id-rule
           #:all-rules
           #:enable #:disable #:enabled-p))
