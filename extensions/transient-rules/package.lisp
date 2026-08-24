;;;; transient-rules/package.lisp

(defpackage #:transient-rules
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "One-shot window rules.

    (add-rule \"steam\" :float t)     ; the next steam window floats, once
    (float-next \"steam\")            ; shorthand for exactly that

Entries wait in QUEUE until a matching window appears; the entry is consumed
on first match.  A match is an app-id string or a function of the window.")
  (:export #:*queue* #:add-rule #:float-next
           #:pending-rules #:clear-rules))
