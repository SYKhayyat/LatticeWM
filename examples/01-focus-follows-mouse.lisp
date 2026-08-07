;;;; examples/01-focus-follows-mouse.lisp
;;;;
;;;; TIER 1 — one method, no new state.  The simplest shape an extension takes.
;;;;
;;;; Load it by adding this to ~/.config/latticewm/init.lisp:
;;;;
;;;;     (load "/path/to/examples/01-focus-follows-mouse.lisp")
;;;;
;;;; or tell the running window manager to load it, where it takes effect
;;;; immediately — no restart, no relogin:
;;;;
;;;;     latticewm --eval '(load "examples/01-focus-follows-mouse.lisp")'
;;;;
;;;; WHAT TO NOTICE, if you are learning the shape:
;;;;
;;;;   * The whole extension is a value and a method.
;;;;   * Nothing was edited.  This file only *adds*.
;;;;   * The method calls CALL-NEXT-METHOD, so the shipped behaviour is still
;;;;     underneath and still reachable.
;;;;   * (describe 'pointer-focus) at a REPL tells you the contract.  Every
;;;;     generic in the surface answers that question about itself.

(in-package #:latticewm/user)

;;; The shipped policy already has the option; turning it on is tier 0 and does
;;; not need this file at all.
(setf *focus-follows-mouse* t)

;;; What this file adds is the *refinement* people actually want after a day of
;;; using it: do not steal focus from a floating window just because the
;;; pointer passed over the tiled pane underneath it.

(defmethod pointer-focus ((policy conventional-policy) world x y)
  "Ignore the pointer while it is over a floating window.

A float is deliberately on top of something, so the pane underneath is exactly
the thing you did *not* mean to focus.  Without this, dragging a floating
terminal across the screen repeatedly steals focus from itself."
  (let ((over-a-float
          (some (lambda (float)
                  (let ((window (float-window float)))
                    (and window (window-rect window)
                         (rect-contains-p (window-rect window) x y))))
                (world-floats world))))
    (unless over-a-float
      (call-next-method))))

;;; And the other half of the same idea, which is a different generic because
;;; it answers a different question: when focus *does* move, warp the pointer
;;; to the new pane if it moved by keyboard.  Otherwise the pointer sits over
;;; one pane while the keyboard talks to another, and focus-follows-mouse
;;; fights every keyboard motion.

(defmethod on-focus-change ((policy conventional-policy) world old new)
  (call-next-method)
  ;; :RECT-INDEX IS AN ARTIFACT OF THE LAST FRAME AND NOT A PART OF THE WORLD.
  ;; EMIT writes it after a layout; before the first one there is no table at
  ;; all, and `(gethash node nil)' is a type error rather than a miss.  Focus
  ;; changes before the first frame -- restoring a session, a config that opens
  ;; something, any policy driven from a test -- so the index has to be asked
  ;; for as a thing that may not be there yet.  MOTION.LISP and SERVER.LISP
  ;; both already do; this method was the one place that assumed.
  (let* ((index (prop world :rect-index))
         (node (and index (resolve-path (world-root world) new)))
         (rect (and node (gethash node index))))
    (when (and rect *focus-follows-mouse* (primary-seat))
      (multiple-value-bind (x y) (rect-center rect)
        ;; WARP-POINTER is window-management state, so it may only be sent in a
        ;; manage sequence.  The runtime is already inside one when focus
        ;; changes from a key binding — but IN-WM makes it safe from a REPL too.
        (in-wm (warp-pointer x y))))))
