;;;; thumbnails/package.lisp

(defpackage #:thumbnails
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Pictures of windows in the deck, off by default.

    (load-extension \"thumbnails\")   ; nothing changes on screen
    (thumbnails on)                  ; M-x thumbnails on
    (preview)                        ; the deck, now with pictures

This is the module half of #130; the protocol half is #135.  Its whole job is to
answer P:CELL-PICTURE, which the deck asks, so the drawing code does not know
whether pictures exist -- and a session with this module off draws the deck it
always drew.

WHY IT IS A MODULE AND OFF BY DEFAULT.  A picture of a window costs a copy out of
the compositor, and a deck is drawn every time something changes.  A feature that
spends that on behalf of a user who never asked for it is a feature that makes
the program slower for a decoration.  Turn it on and the cost is a screenshot per
window per look; leave it off and nothing is asked for at all.")
  (:export #:*thumbnails*
           #:*picture-source* #:*captures* #:*pictures*
           #:enable #:disable #:enabled-p
           #:forget-pictures #:picture-for))
