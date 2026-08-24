;;;; focus-follows-mouse/focus-follows-mouse.lisp --- The whole module.
;;;;
;;;; Two methods and a switch.  Both methods are :AROUND on
;;;; CONVENTIONAL-POLICY, which is what makes ENABLE/DISABLE possible without
;;;; method surgery: when the switch is off, each is a pure pass-through, so
;;;; DISABLE needs to know nothing about CLOS internals and ENABLE twice is a
;;;; value assignment rather than an accumulation.
;;;;
;;;; The example this was promoted from defines plain methods instead, because
;;;; a teaching file has no off switch to honour.  EXTENDING.org's contract for
;;;; a module with an on/off switch -- enable composes, enable twice is a
;;;; no-op, disable restores what was underneath, enabling is live -- is met
;;;; here by construction: nothing is replaced, ever.

(in-package #:focus-follows-mouse)

(defvar *enabled* nil
  "Whether the two refinements are in force.  Toggled by ENABLE and DISABLE.

Loading the system installs nothing behaviourally; until ENABLE runs, both
methods pass straight through.  That ordering is what makes this loadable from
a configuration file that only *sometimes* wants it, and it is why DISABLE can
promise that the shipped behaviour is exactly what remains.")

(defun enabled-p () "True when the refinements are in force." *enabled*)

;;; ------------------------------------------------------------- the methods

(defmethod p:pointer-focus :around ((policy p:conventional-policy) world x y)
  "Ignore the pointer while it is over a floating window.

A float sits on top of the tiled pane underneath it on purpose, so that pane is
exactly the one you did not mean to focus: without this refinement, dragging a
floating terminal across the screen repeatedly steals focus from itself."
  (when *enabled*
    (let ((over-a-float
            (some (lambda (float)
                    (let ((window (c:float-window float)))
                      (and window
                           (c:window-rect window)
                           (c:rect-contains-p (c:window-rect window) x y))))
                  (c:world-floats world))))
      (when over-a-float
        (return-from p:pointer-focus nil))))
  (call-next-method))

(defmethod p:on-focus-change :around ((policy p:conventional-policy)
                                      world old new)
  "Warp the pointer to the newly focused pane when focus moved by keyboard.

Otherwise the pointer sits over one pane while the keyboard talks to another,
and the next accidental twitch of the mouse undoes a deliberate motion."
  ;; :RECT-INDEX IS AN ARTIFACT OF THE LAST FRAME AND NOT A PART OF THE WORLD.
  ;; EMIT writes it after a layout; before the first one there is no table at
  ;; all, and `(gethash node nil)' is a type error rather than a miss.  Focus
  ;; changes before the first frame -- restoring a session, a config that opens
  ;; something, any policy driven from a test -- so the index has to be asked
  ;; for as a thing that may not be there yet.  (The example carried this
  ;; comment before it carried the guard, and a test carries the regression.)
  (call-next-method)
  (when *enabled*
    (let* ((index (c:prop world :rect-index))
           (node (and index (c:resolve-path (c:world-root world) new)))
           (rect (and node (gethash node index))))
      (when (and rect p:*focus-follows-mouse* (r:primary-seat))
        (multiple-value-bind (x y) (c:rect-center rect)
          ;; WARP-POINTER is window-management state, so it may only be sent
          ;; in a manage sequence.  The runtime is already inside one when
          ;; focus changes from a key binding -- but IN-WM makes it safe from
          ;; a REPL too.
          (r:in-wm (r:warp-pointer x y)))))))

;;; ------------------------------------------------------- enable / disable

(defun enable ()
  "Turn both refinements on, live, and turn the shipped option on with them.

THE OPTION IS THE MODULE'S OWN PREREQUISITE, so ENABLE sets it rather than
leaving the user to discover that loading did nothing: without
*FOCUS-FOLLOWS-MOUSE*, the runtime never asks POINTER-FOCUS where the pointer
is, and the first refinement refines a question nobody asked.  DISABLE does
not unset it -- it is the core's knob, you may have set it yourself, and
turning a module off should not reach past the module."
  (setf *enabled* t)
  (setf p:*focus-follows-mouse* t)
  (values))

(defun disable ()
  "Turn both refinements off, live.  Nothing restarted, nothing lost.

What remains afterwards is the shipped behaviour, exactly: the methods stay
installed and pass through, so re-enabling later cannot accumulate anything."
  (setf *enabled* nil)
  (values))
