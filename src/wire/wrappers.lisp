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
;;;; NODE-SET-POSITION — which is what the runtime actually calls.  DESIGN's
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

(eval-when (:compile-toplevel :execute)
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
       (,target ,@args)))))

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
  (alias output-destroy river-output-v1.destroy
         "Let go of an output that has gone away.

THE TEARDOWNS ARE THE HALF THAT WAS BEING CALLED RAW.  Every one of these
aliases existed as a generated wrapper already -- there is one for all 123
requests -- and eleven call sites in runtime/ reached past them for
`river:river-output-v1.destroy' and its siblings, so the discipline the wire
layer exists to impose was applied to the requests somebody remembered and to
no others.  Gate 22 asks the question now: outside src/wire/, the protocol
package may be named for an *interface*, which is how a global is bound, and
never for a request.")
  ;; layer shell
  (alias layer-shell-get-output river-layer-shell-v1.get-output)
  (alias layer-shell-get-seat river-layer-shell-v1.get-seat)
  (alias layer-output-set-default river-layer-shell-output-v1.set-default)
  (alias layer-output-destroy river-layer-shell-output-v1.destroy)
  (alias layer-seat-destroy river-layer-shell-seat-v1.destroy)
  (alias shell-surface-get-node river-shell-surface-v1.get-node)
  (alias shell-surface-destroy river-shell-surface-v1.destroy)
  ;; keyboard bindings
  (alias binding-enable river-xkb-binding-v1.enable)
  (alias binding-disable river-xkb-binding-v1.disable)
  (alias binding-destroy river-xkb-binding-v1.destroy)
  (alias bindings-seat-destroy river-xkb-bindings-seat-v1.destroy)
  (alias seat-destroy river-seat-v1.destroy)
  ;; pointer bindings.  A SEPARATE INTERFACE WITH A SEPARATE ENABLE, and the
  ;; reason this alias exists is that the first version of the pointer code
  ;; called BINDING-ENABLE on a river_pointer_binding_v1 -- which is a type
  ;; error inside the generated marshaller, caught by GUARDED, logged to a
  ;; stderr nobody reads, and otherwise silent.  Super+drag simply did nothing.
  ;;
  ;; Nothing in the unit suite could see it: the suite constructs state, and a
  ;; proxy is not something you can construct.  The headless-river integration
  ;; test found it on its first run, which is the entire argument for having
  ;; one.
  (alias pointer-binding-enable river-pointer-binding-v1.enable
         "Enable a pointer binding.  Manage sequence only.")
  (alias pointer-binding-disable river-pointer-binding-v1.disable)
  (alias binding-set-layout-override river-xkb-binding-v1.set-layout-override)
  (alias bindings-get-seat river-xkb-bindings-v1.get-seat)
  (alias bindings-get-xkb-binding river-xkb-bindings-v1.get-xkb-binding)
  (alias bindings-seat-ensure-next-key-eaten
         river-xkb-bindings-seat-v1.ensure-next-key-eaten
         "Ask for the next keypress even if it is unbound.

The protocol's own rationale is chorded bindings: without a way to eat the
next key press, a submap has no way to know it should error out and exit.  It
is also what DESIGN D19's typing-in-an-empty-pane rests on.")
  (alias bindings-seat-cancel-ensure-next-key-eaten
         river-xkb-bindings-seat-v1.cancel-ensure-next-key-eaten)
  (alias bindings-seat-modifiers-watch
         river-xkb-bindings-seat-v1.modifiers-watch
         "Ask to be told when modifier state changes.  Nothing sends this.

Wrapped because the wire layer wraps the protocol and not the subset in use --
an unwrapped request is how the next feature ends up reaching round the
package boundary.  The docstring used to say `for hold-to-peek', which named a
feature the program does not have: without this request the modifiers_update
event never fires, and runtime/seats.lisp records deleting the handler that
was waiting for it rather than inventing the feature to justify it.")
  ;; INPUT CONFIGURATION.  Three protocols outside river_window_manager_v1
  ;; entirely, so none of these is sequence-bound: a touchpad setting can be
  ;; sent at any moment on the window manager's thread, which is why
  ;; `(setf *tap-to-click* t) (reload-input)' works at a REPL with no round
  ;; trip.  It is the only subsystem here of which that is true.
  (alias input-destroy river-input-device-v1.destroy)
  (alias input-set-repeat-info river-input-device-v1.set-repeat-info
         "Key repeats per second, and the milliseconds before repeating starts.

One request carrying both, so a configuration that sets only one has to supply
the other from what is already in force rather than from a zero.")
  (alias input-set-scroll-factor river-input-device-v1.set-scroll-factor
         "Multiply this device's scroll distance.  River's own knob, so it works
on devices libinput does not drive.")
  (alias input-map-to-output river-input-device-v1.map-to-output
         "Confine a stylus or a touchscreen to one monitor.
Takes the wl_output, which is why the wl_output proxies are kept.")
  (alias input-assign-to-seat river-input-device-v1.assign-to-seat)
  ;; libinput.  Every setter takes the value and hands back a
  ;; river_libinput_result_v1 that answers success, unsupported or invalid
  ;; exactly once -- which is the only way a wrong value in a configuration
  ;; file becomes a sentence somebody can read.
  (alias libinput-destroy river-libinput-device-v1.destroy)
  (alias libinput-set-send-events river-libinput-device-v1.set-send-events)
  (alias libinput-set-tap river-libinput-device-v1.set-tap
         "Tap the pad to click.  The first thing anybody turns on.")
  (alias libinput-set-tap-button-map river-libinput-device-v1.set-tap-button-map)
  (alias libinput-set-drag river-libinput-device-v1.set-drag)
  (alias libinput-set-drag-lock river-libinput-device-v1.set-drag-lock)
  (alias libinput-set-three-finger-drag
         river-libinput-device-v1.set-three-finger-drag)
  (alias libinput-set-accel-profile river-libinput-device-v1.set-accel-profile)
  (alias libinput-set-accel-speed river-libinput-device-v1.set-accel-speed
         "Pointer speed in [-1, 1], as the raw bytes of a double.")
  (alias libinput-set-natural-scroll river-libinput-device-v1.set-natural-scroll)
  (alias libinput-set-left-handed river-libinput-device-v1.set-left-handed)
  (alias libinput-set-click-method river-libinput-device-v1.set-click-method)
  (alias libinput-set-clickfinger-button-map
         river-libinput-device-v1.set-clickfinger-button-map)
  (alias libinput-set-middle-emulation
         river-libinput-device-v1.set-middle-emulation)
  (alias libinput-set-scroll-method river-libinput-device-v1.set-scroll-method)
  (alias libinput-set-scroll-button river-libinput-device-v1.set-scroll-button)
  (alias libinput-set-scroll-button-lock
         river-libinput-device-v1.set-scroll-button-lock)
  (alias libinput-set-dwt river-libinput-device-v1.set-dwt
         "Ignore the touchpad briefly after a keystroke, so a palm does not move
the cursor mid-sentence.")
  (alias libinput-set-dwtp river-libinput-device-v1.set-dwtp)
  (alias libinput-set-rotation river-libinput-device-v1.set-rotation)
  (alias libinput-set-calibration-matrix
         river-libinput-device-v1.set-calibration-matrix)
  ;; xkb.  A keymap is compiled elsewhere and arrives here as a file
  ;; descriptor; layout switching needs no keymap at all, which is what makes
  ;; NEXT-KEYBOARD-LAYOUT work on a machine with no xkbcli on it.
  (alias xkb-create-keymap river-xkb-config-v1.create-keymap
         "Hand river a compiled XKB keymap on a file descriptor.")
  (alias xkb-keymap-destroy river-xkb-keymap-v1.destroy)
  (alias xkb-keyboard-destroy river-xkb-keyboard-v1.destroy)
  (alias xkb-keyboard-set-keymap river-xkb-keyboard-v1.set-keymap)
  (alias xkb-keyboard-set-layout-by-index
         river-xkb-keyboard-v1.set-layout-by-index
         "Switch layouts within the current keymap.  River wraps an index past
the end back to zero, so `the next one' needs no layout count.")
  (alias xkb-keyboard-set-layout-by-name river-xkb-keyboard-v1.set-layout-by-name)
  (alias xkb-keyboard-capslock-enable river-xkb-keyboard-v1.capslock-enable)
  (alias xkb-keyboard-capslock-disable river-xkb-keyboard-v1.capslock-disable)
  (alias xkb-keyboard-numlock-enable river-xkb-keyboard-v1.numlock-enable)
  (alias xkb-keyboard-numlock-disable river-xkb-keyboard-v1.numlock-disable))

;;; ---------------------------------------------------------------- enums
;;;
;;; WAYFLAN REPRESENTS A BITFIELD AS A LIST OF KEYWORDS, not as an integer,
;;; and that is the whole story of this section.  Passing 15 where river's
;;; `edges' bitfield is expected does not produce a wrong border — it produces
;;; a type error inside the generated marshaller, which is much better, but
;;; only if you know to expect it.
;;;
;;; So the vocabulary above takes keyword lists, and the integers below exist
;;; only as documentation of what the protocol actually puts on the wire.

(defparameter +edges-none+ '())
(defparameter +edge-top+ '(:top))
(defparameter +edge-bottom+ '(:bottom))
(defparameter +edge-left+ '(:left))
(defparameter +edge-right+ '(:right))
(defparameter +edges-all+ '(:top :bottom :left :right)
  "Every edge.  What a fully tiled window is adjacent on.")

(defparameter +cap-window-menu+ '(:window-menu))
(defparameter +cap-maximize+ '(:maximize))
(defparameter +cap-fullscreen+ '(:fullscreen))
(defparameter +cap-minimize+ '(:minimize))
(defparameter +caps-all+ '(:window-menu :maximize :fullscreen :minimize)
  "Every capability.  We honour all four, so we declare all four.")

(defparameter +protocol-modifier-bits+
  '((:shift . 1) (:ctrl . 4) (:mod1 . 8) (:mod3 . 32) (:mod4 . 64) (:mod5 . 128))
  "River's modifier bitfield, for reference.  Note the absences: 2 and 16 —
capslock and numlock — are deliberately not in the protocol, because a locked
modifier in a binding makes no sense.")

;;; ------------------------------------------------------------- colour
;;;
;;; THERE IS ONE ANSWER TO "WHAT IS A COLOUR IN THIS PROGRAM", and this is it:
;;;
;;;   *A colour on the policy surface is four straight-alpha floats in [0,1].
;;;    Every layer that puts pixels anywhere premultiplies at its own boundary.*
;;;
;;; It has to be stated once and obeyed everywhere, because it was not.  The
;;; overlay path (RUNTIME:ARGB) premultiplied, with a docstring explaining that
;;; getting it wrong shows up as a halo around everything; the border path did
;;; not.  So the same colour returned from BORDER-COLOR produced two different
;;; pixels depending on whether it landed on a border or on the echo area, and
;;; every border with alpha below 1.0 was too bright — increasingly so as the
;;; alpha fell.  An extension author had no single answer to give.
;;;
;;; river-window-management-v1.xml, on set_borders: "The color is defined by
;;; four 32-bit RGBA values.  Unless specified in another protocol extension,
;;; the RGBA values use pre-multiplied alpha."

(defun clamp-unit (value)
  "VALUE as a float in [0,1].  Anything unreadable becomes 0.0.

Tolerant on purpose: a colour comes from a policy method, which is user code,
and a theme with a typo in it should produce a wrong colour rather than take
down the manage sequence that was drawing a border."
  (let ((number (if (realp value) (float value 1.0) 0.0)))
    (max 0.0 (min 1.0 number))))

(defun color-component (value &optional (alpha 1.0))
  "One straight-alpha component as the 32-bit unsigned value river wants.

ALPHA premultiplies it, per the protocol.  Passing no alpha means 1.0, which is
the identity — so an opaque colour reads the same as it always did, and the
alpha channel itself is written by passing it as VALUE with no second argument.

Nobody wants to write colours premultiplied, so the policy surface uses
straight-alpha floats and the conversion happens here: once, at the boundary,
rather than in every theme anybody ever writes."
  (max 0 (min #xffffffff
              (round (* (clamp-unit value) (clamp-unit alpha) #xffffffff)))))

(defun premultiplied-rgba (r g b &optional (a 1.0))
  "Four straight-alpha floats as the four premultiplied words set_borders wants.

Returns (values R G B A).  This is the only correct way to hand a policy colour
to set_borders, and it exists as one function so that there is exactly one
place where the multiplication either happens or does not."
  (values (color-component r a)
          (color-component g a)
          (color-component b a)
          (color-component a)))
