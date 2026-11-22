;;;; examples/03-master-stack.lisp
;;;;
;;;; TIER 3 — a whole layout model, in one method.
;;;;
;;;; The master-and-stack layout: one large pane on the left holding the window
;;;; you are working in, and everything else in a column down the right.  It is
;;;; dwm's layout, xmonad's default, and the thing most people mean by "tiling
;;;; window manager".  LatticeWM does not ship it as a default, because splits
;;;; are more general and master-stack is a *restriction* of them.
;;;;
;;;; WHY THIS IS THE INTERESTING EXAMPLE.  It is not a tweak — it changes what
;;;; the window manager *is*.  And it does so without touching the tree: the
;;;; splits are still there, still nested, still hold exactly what they held.
;;;; Only the answer to "how does this container divide its rectangle" changed.
;;;; Switch back and every window is where you left it.
;;;;
;;;; That separation — structure in the tree, arrangement in the policy — is
;;;; what makes a new layout cost one method instead of a fork.
;;;;
;;;; THE POLICY MOVED TO extensions/master-stack, WHICH IS WHY THIS FILE IS
;;;; SHORTER THAN THE LESSON.  It composed badly as an example: the inline copy
;;;; was a *subclass*, so it could not be turned off, could not be stacked with
;;;; another layout tweak, and silently replaced the shipped LAYOUT-CHILDREN
;;;; method instead of sitting on top of it.  The module is a mixin installed
;;;; with CHANGE-CLASS, which means the same object comes through the switch
;;;; carrying whatever state it had -- and DISABLE hands back the class that was
;;;; there before.  Read extensions/master-stack/master-stack.lisp for the
;;;; method; what follows is what a user writes.

(in-package #:latticewm/user)

(load-extension "master-stack")
(master-stack:enable)

;;; Two knobs, because a layout nobody can adjust is a layout nobody keeps:
;;;
;;;   (setf (master-stack:master-ratio (current-policy)) 2/3)  ; wider master
;;;   (setf (master-stack:master-count (current-policy)) 2)    ; two on top
;;;
;;; and the way out is any of:
;;;
;;;   (master-stack:disable)                    back to what was there before
;;;   (master-stack:enable)                     idempotent, not an accumulation
;;;   (setf *policy* (make-instance 'conventional-policy))   a fresh one
