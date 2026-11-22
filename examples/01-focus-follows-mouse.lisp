;;;; examples/01-focus-follows-mouse.lisp
;;;;
;;;; TIER 1 — one method, no new state.  The simplest shape an extension takes.
;;;;
;;;; SINCE THIS WAS WRITTEN, THE CODE MOVED OUT AND THIS FILE BECAME A WAY TO
;;;; TURN IT ON.  extensions/focus-follows-mouse holds the methods now, with an
;;;; on/off switch, tests and a container-surface row; an inline copy here would
;;;; be a second version of the same logic and would drift the moment either
;;;; changed.  It already had: this file's copy had stopped calling
;;;; CALL-NEXT-METHOD in one place and had no answer for a world that had never
;;;; been laid out.  A teaching file that teaches *stale* code is worse than no
;;;; teaching file, so the body is now three lines and the lesson is the same
;;;; one it always was, in a form the program actually runs.
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
;;;;   * An extension is a value and a method, and the load is a *name*:
;;;;     nothing here knows where the module lives on this machine.
;;;;   * Nothing was edited.  LOAD-EXTENSION only *adds*, and the shipped
;;;;     behaviour stays underneath and stays reachable -- see
;;;;     extensions/focus-follows-mouse/focus-follows-mouse.lisp, where every
;;;;     method is :AROUND and calls CALL-NEXT-METHOD while enabled, which is
;;;;     what makes DISABLE possible.
;;;;   * (describe 'pointer-focus) at a REPL tells you the contract.  Every
;;;;     generic in the surface answers that question about itself.

(in-package #:latticewm/user)

;;; The shipped policy already has the option; turning it on is tier 0 and does
;;; not need the module at all -- the module adds the *refinement* people
;;; actually want after a day of using it: do not steal focus from a floating
;;; window just because the pointer passed over the tiled pane underneath, and
;;; warp the pointer along when focus moves by keyboard.
(setf *focus-follows-mouse* t)

(load-extension "focus-follows-mouse")
(focus-follows-mouse:enable)

;;; To go back:  (focus-follows-mouse:disable)
