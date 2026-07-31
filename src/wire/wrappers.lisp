;;;; wire/wrappers.lisp --- A checked wrapper for every generated request.
;;;;
;;;; wayflan generates requests as *plain functions* named `interface.request'
;;;; in one flat package.  Two consequences, both mechanical:
;;;;
;;;;   1. The sequence discipline cannot be attached with DEFMETHOD, because
;;;;      there is no generic function to specialize.
;;;;   2. Redefining a generated function in place would work, and would be a
;;;;      maintenance crime, because the next scanner run silently reverts it.
;;;;
;;;; So every request gets a wrapper.  They are generated rather than written,
;;;; by walking the protocol package at compile time — which means a protocol
;;;; update cannot leave a request unwrapped, and the count is a build gate.
;;;;
;;;; The wrappers keep each request's real arity rather than taking &REST, so
;;;; that a wrong-arity call is still a compile-time error.  PLAN.org's gate 1
;;;; exists to recover the largest slice of what a type system would have
;;;; caught, and throwing it away at the widest interface in the program would
;;;; have been an odd way to honour that.
;;;;
;;;; Below the wrappers is a deliberately smaller vocabulary — WINDOW-HIDE,
;;;; NODE-SET-POSITION — which is what the runtime actually calls.  README's
;;;; ruling: "The wrapper layer should export a deliberately smaller vocabulary
;;;; and leave RIVER as plumbing that Layer 1 never sees."

(in-package #:latticewm/wire)

;;; --------------------------------------------------- the classification

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *manage-only-requests*
  '(;; river_window_v1 — window-management state
    "RIVER-WINDOW-V1.CLOSE"
    "RIVER-WINDOW-V1.PROPOSE-DIMENSIONS"
    "RIVER-WINDOW-V1.USE-CSD"
    "RIVER-WINDOW-V1.USE-SSD"
    "RIVER-WINDOW-V1.SET-TILED"
    "RIVER-WINDOW-V1.INFORM-RESIZE-START"
    "RIVER-WINDOW-V1.INFORM-RESIZE-END"
    "RIVER-WINDOW-V1.SET-CAPABILITIES"
    "RIVER-WINDOW-V1.INFORM-MAXIMIZED"
    "RIVER-WINDOW-V1.INFORM-UNMAXIMIZED"
    "RIVER-WINDOW-V1.INFORM-FULLSCREEN"
    "RIVER-WINDOW-V1.INFORM-NOT-FULLSCREEN"
    "RIVER-WINDOW-V1.FULLSCREEN"
    "RIVER-WINDOW-V1.EXIT-FULLSCREEN"
    "RIVER-WINDOW-V1.SET-DIMENSION-BOUNDS"
    ;; river_seat_v1
    "RIVER-SEAT-V1.FOCUS-WINDOW"
    "RIVER-SEAT-V1.FOCUS-SHELL-SURFACE"
    "RIVER-SEAT-V1.CLEAR-FOCUS"
    "RIVER-SEAT-V1.OP-START-POINTER"
    "RIVER-SEAT-V1.OP-END"
    "RIVER-SEAT-V1.POINTER-WARP"
    ;; bindings
    "RIVER-POINTER-BINDING-V1.ENABLE"
    "RIVER-POINTER-BINDING-V1.DISABLE"
    "RIVER-XKB-BINDING-V1.SET-LAYOUT-OVERRIDE"
    "RIVER-XKB-BINDING-V1.ENABLE"
    "RIVER-XKB-BINDING-V1.DISABLE"
    "RIVER-XKB-BINDINGS-SEAT-V1.ENSURE-NEXT-KEY-EATEN"
    "RIVER-XKB-BINDINGS-SEAT-V1.CANCEL-ENSURE-NEXT-KEY-EATEN"
    "RIVER-XKB-BINDINGS-SEAT-V1.MODIFIERS-WATCH")
  "Requests that modify window-management state and are manage-sequence only.

Transcribed from river-window-management-v1.xml's own classification.  If the
protocol adds a request, it lands in the :ANY class by default and this list
is where it gets promoted."))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *render-requests*
  '("RIVER-NODE-V1.SET-POSITION"
    "RIVER-NODE-V1.PLACE-TOP"
    "RIVER-NODE-V1.PLACE-BOTTOM"
    "RIVER-NODE-V1.PLACE-ABOVE"
    "RIVER-NODE-V1.PLACE-BELOW"
    "RIVER-WINDOW-V1.HIDE"
    "RIVER-WINDOW-V1.SHOW"
    "RIVER-WINDOW-V1.SET-BORDERS"
    "RIVER-WINDOW-V1.SET-CLIP-BOX"
    "RIVER-WINDOW-V1.SET-CONTENT-CLIP-BOX"
    "RIVER-DECORATION-V1.SET-OFFSET"
    "RIVER-DECORATION-V1.SYNC-NEXT-COMMIT"
    "RIVER-SHELL-SURFACE-V1.SYNC-NEXT-COMMIT"
    "RIVER-OUTPUT-V1.SET-PRESENTATION-MODE")
  "Requests that modify rendering state.  Legal in a render sequence always,
and in a manage sequence subject to *RENDER-LEGAL-IN-MANAGE*."))

(defvar *request-classes* (make-hash-table :test #'eq)
  "SYMBOL -> :MANAGE | :RENDER | :ANY, for every wrapped request.")

(defun request-sequence-class (symbol)
  "Which sequence class SYMBOL belongs to, or NIL when it is not a request."
  (gethash symbol *request-classes*))

(defun all-wrapped-requests ()
  "Every wrapped request, as (SYMBOL . CLASS), sorted by name.

Its length is a build gate: a protocol update that adds a request without
adding a wrapper would otherwise be invisible until the request was first
called, at runtime, in front of a user."
  (let ((out '()))
    (maphash (lambda (symbol class) (push (cons symbol class) out))
             *request-classes*)
    (sort out #'string< :key (lambda (row) (symbol-name (car row))))))

;;; ------------------------------------------------------ the generator
;;;
;;; These exist only to run the macro below at compile time, so they are
;;; :COMPILE-TOPLEVEL :EXECUTE rather than the usual three — defining them at
;;; load time as well would be a redefinition, and gate 1 wants zero warnings.

(eval-when (:compile-toplevel :execute)
  (defun %request-class (name)
    (cond ((member name *manage-only-requests* :test #'string=) :manage)
          ((member name *render-requests* :test #'string=) :render)
          (t :any)))

  (defun %plain-lambda-list-p (lambda-list)
    "True when LAMBDA-LIST is all required arguments — no &OPTIONAL, &REST or
&KEY.  Every generated request is; anything that is not would need a wrapper
written by hand rather than generated, and we would rather find out loudly."
    (and lambda-list
         (notany (lambda (symbol)
                   (and (symbolp symbol)
                        (plusp (length (symbol-name symbol)))
                        (char= (char (symbol-name symbol) 0) #\&)))
                 lambda-list)))

  (defun %protocol-requests ()
    "Every callable request in the generated protocol package, as
(SYMBOL ARITY), sorted so the expansion is stable across builds."
    (let ((out '()))
      (do-external-symbols (symbol '#:latticewm/river)
        (when (and (fboundp symbol)
                   (not (typep (fdefinition symbol) 'generic-function)))
          (let ((lambda-list (sb-introspect:function-lambda-list symbol)))
            (when (%plain-lambda-list-p lambda-list)
              (pushnew (list symbol (length lambda-list)) out
                       :key #'first)))))
      (sort out #'string< :key (lambda (row) (symbol-name (first row)))))))

(defmacro define-request-wrappers ()
  "Emit a checked wrapper for every generated request.

Each wrapper has the same name as the generated function, lives in this
package, keeps the original arity, consults CHECK-SEQUENCE, and forwards."
  (let ((forms '()))
    (dolist (entry (%protocol-requests))
      (destructuring-bind (river-symbol arity) entry
        (let* ((name (symbol-name river-symbol))
               (class (%request-class name))
               (wrapper (intern name '#:latticewm/wire))
               (args (loop for i from 0 below arity
                           collect (intern (format nil "A~d" i) '#:latticewm/wire))))
          (push `(progn
                   (setf (gethash ',wrapper *request-classes*) ,class)
                   (defun ,wrapper ,args
                     ,(format nil "Checked wrapper for ~a (~(~a~) state)." name class)
                     ,@(unless (eq class :any)
                         `((check-sequence ,class ',wrapper)))
                     (,river-symbol ,@args)))
                forms))))
    `(progn ,@(nreverse forms))))

(define-request-wrappers)

;;; ------------------------------------------- the small vocabulary above it

;;; The aliases keep the wrapped request's real arity too, for the same reason
;;; the wrappers do: a wrong-arity call must stay a compile-time error.

(defmacro alias (name target &optional documentation)
  "Define NAME as a readable name for the wrapped request TARGET.

Arity comes from the *generated* function of the same name, which is loaded by
the time this file is compiled — the wrapper itself is not yet FBOUNDP in the
compilation environment, so asking it would fail."
  (let* ((river-symbol (or (find-symbol (symbol-name target) '#:latticewm/river)
                           (error "No generated request named ~a." target)))
         (arity (length (sb-introspect:function-lambda-list river-symbol)))
         (args (loop for i from 0 below arity
                     collect (intern (format nil "A~d" i) '#:latticewm/wire))))
    `(defun ,name ,args
       ,(or documentation (format nil "See ~a." target))
       (,target ,@args))))

(progn
  ;; window manager
  (alias wm-manage-finish river-window-manager-v1.manage-finish
         "End the manage sequence.  Prefer WITH-MANAGE-SEQUENCE.")
  (alias wm-render-finish river-window-manager-v1.render-finish
         "End the render sequence.  Prefer WITH-RENDER-SEQUENCE.")
  (alias wm-manage-dirty river-window-manager-v1.manage-dirty
         "Ask river to start another manage sequence.

There is no frame or timer event in this protocol, so this is the only way to
drive anything of our own — an animation, a deferred relayout.  It costs a full
manage round trip, so it is not a frame clock and must not be treated as one.")
  (alias wm-exit-session river-window-manager-v1.exit-session
         "Ask river to end the session.  This logs the user out.")
  (alias wm-stop river-window-manager-v1.stop)
  (alias wm-get-shell-surface river-window-manager-v1.get-shell-surface
         "Wrap one of our own wl_surfaces so it can be positioned like a window.
This is how the cursor decoration, the coordinate overlay and the minimap get
onto the screen.")
  ;; windows — management state
  (alias window-close river-window-v1.close)
  (alias window-propose-dimensions river-window-v1.propose-dimensions
         "Ask a window to become WIDTH by HEIGHT.

Advisory: the spec calls out terminal emulators that quantise to their cell
size.  The real size arrives later in a dimensions event.")
  (alias window-set-dimension-bounds river-window-v1.set-dimension-bounds)
  (alias window-set-capabilities river-window-v1.set-capabilities)
  (alias window-set-tiled river-window-v1.set-tiled)
  (alias window-use-csd river-window-v1.use-csd)
  (alias window-use-ssd river-window-v1.use-ssd)
  (alias window-fullscreen river-window-v1.fullscreen
         "Make a window the only thing rendered on an output.

Cheap: clip boxes are ignored and borders are not drawn while fullscreen, so
entering and leaving cost no relayout at all.")
  (alias window-exit-fullscreen river-window-v1.exit-fullscreen)
  (alias window-inform-maximized river-window-v1.inform-maximized)
  (alias window-inform-unmaximized river-window-v1.inform-unmaximized)
  (alias window-inform-fullscreen river-window-v1.inform-fullscreen)
  (alias window-inform-not-fullscreen river-window-v1.inform-not-fullscreen)
  (alias window-inform-resize-start river-window-v1.inform-resize-start)
  (alias window-inform-resize-end river-window-v1.inform-resize-end)
  ;; windows — rendering state
  (alias window-hide river-window-v1.hide
         "Stop rendering a window.

Newly created windows are shown unless explicitly hidden, so anything placed
offscreen must be hidden deliberately.")
  (alias window-show river-window-v1.show)
  (alias window-set-borders river-window-v1.set-borders)
  (alias window-set-clip-box river-window-v1.set-clip-box
         "Crop a window *including* its borders and decorations.")
  (alias window-set-content-clip-box river-window-v1.set-content-clip-box
         "Crop a window's content, *excluding* borders and decorations.

The best request in the protocol.  Borders are drawn around the intersection
of the content and the clip box, so a cell half-scrolled off the viewport edge
is cropped and its border is redrawn at the crop edge — it reads as a cleanly
cut cell rather than a window sliced in half.  Free, from the compositor.")
  (alias window-get-node river-window-v1.get-node)
  (alias window-destroy river-window-v1.destroy)
  ;; nodes
  (alias node-set-position river-node-v1.set-position
         "Place a node.  X and Y may be negative — which is the whole reason an
unbounded lattice is possible at all.")
  (alias node-place-top river-node-v1.place-top)
  (alias node-place-bottom river-node-v1.place-bottom)
  (alias node-place-above river-node-v1.place-above
         "Order one node above another.

River says the initial position of a node in the render list is undefined, so
every node must be ordered explicitly or overlapping windows flicker.")
  (alias node-place-below river-node-v1.place-below)
  (alias node-destroy river-node-v1.destroy)
  ;; seats
  (alias seat-focus-window river-seat-v1.focus-window)
  (alias seat-clear-focus river-seat-v1.clear-focus
         "Give keyboard focus to nothing.

This is what an empty focused pane resolves to.  Focus is a *place*; when the
place holds no window there is nothing for Wayland focus to be.")
  (alias seat-focus-shell-surface river-seat-v1.focus-shell-surface)
  (alias seat-set-xcursor-theme river-seat-v1.set-xcursor-theme)
  (alias seat-pointer-warp river-seat-v1.pointer-warp)
  (alias seat-op-start-pointer river-seat-v1.op-start-pointer)
  (alias seat-op-end river-seat-v1.op-end)
  (alias seat-get-pointer-binding river-seat-v1.get-pointer-binding)
  ;; outputs
  (alias output-set-presentation-mode river-output-v1.set-presentation-mode)
  ;; keyboard bindings
  (alias binding-enable river-xkb-binding-v1.enable)
  (alias binding-disable river-xkb-binding-v1.disable)
  (alias binding-set-layout-override river-xkb-binding-v1.set-layout-override)
  (alias bindings-get-seat river-xkb-bindings-v1.get-seat)
  (alias bindings-get-xkb-binding river-xkb-bindings-v1.get-xkb-binding)
  (alias bindings-seat-ensure-next-key-eaten
         river-xkb-bindings-seat-v1.ensure-next-key-eaten
         "Ask for the next keypress even if it is unbound.

The protocol's own rationale is chorded bindings: without a way to eat the
next key press, a submap has no way to know it should error out and exit.  It
is also what README D19's typing-in-an-empty-pane rests on.")
  (alias bindings-seat-cancel-ensure-next-key-eaten
         river-xkb-bindings-seat-v1.cancel-ensure-next-key-eaten)
  (alias bindings-seat-modifiers-watch
         river-xkb-bindings-seat-v1.modifiers-watch
         "Ask to be told when modifier state changes, for hold-to-peek."))

;;; ---------------------------------------------------------------- enums

(defconstant +mod-none+ 0)
(defconstant +mod-shift+ 1)
(defconstant +mod-ctrl+ 4)
(defconstant +mod-alt+ 8 "mod1, commonly alt.")
(defconstant +mod-mod3+ 32)
(defconstant +mod-super+ 64 "mod4, commonly super or logo.")
(defconstant +mod-mod5+ 128)
;; 2 and 16 — capslock and numlock — are deliberately absent from the
;; protocol: locked modifiers make no sense in a binding.

(defconstant +edge-none+ 0)
(defconstant +edge-top+ 1)
(defconstant +edge-bottom+ 2)
(defconstant +edge-left+ 4)
(defconstant +edge-right+ 8)
(defconstant +edges-all+ 15)

(defconstant +cap-window-menu+ 1)
(defconstant +cap-maximize+ 2)
(defconstant +cap-fullscreen+ 4)
(defconstant +cap-minimize+ 8)
(defconstant +caps-all+ 15)

(defparameter *modifier-names*
  `((:shift . ,+mod-shift+) (:ctrl . ,+mod-ctrl+) (:control . ,+mod-ctrl+)
    (:alt . ,+mod-alt+) (:mod1 . ,+mod-alt+) (:meta . ,+mod-alt+)
    (:mod3 . ,+mod-mod3+) (:super . ,+mod-super+) (:mod4 . ,+mod-super+)
    (:logo . ,+mod-super+) (:hyper . ,+mod-mod5+) (:mod5 . ,+mod-mod5+))
  "Keyword to bit, with the aliases people actually type.")

(defun modifier-mask (modifiers)
  "The bitfield for MODIFIERS, a list of keywords such as (:super :shift)."
  (reduce #'logior modifiers :initial-value 0
          :key (lambda (m)
                 (or (cdr (assoc m *modifier-names*))
                     (error "Unknown modifier ~s.  Known: ~{~s~^ ~}"
                            m (remove-duplicates (mapcar #'car *modifier-names*)))))))

(defun modifier-names (mask)
  "The canonical keywords named by bitfield MASK."
  (loop for (name . bit) in '((:shift . 1) (:ctrl . 4) (:alt . 8)
                              (:mod3 . 32) (:super . 64) (:mod5 . 128))
        when (logtest mask bit) collect name))
