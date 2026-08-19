;;;; policy/devices.lisp --- How the machine's own hardware is configured.
;;;;
;;;; THE LARGEST ABSENCE THE PROGRAM HAD, and it was invisible from the inside.
;;;; river vendors three protocols for input configuration —
;;;; river_input_management_v1, river_libinput_config_v1 and
;;;; river_xkb_config_v1 — and this window manager bound none of them.  The XML
;;;; was in the tree.  The consequence is not subtle:
;;;;
;;;;   * a laptop user could not turn on tap-to-click, which is the first thing
;;;;     anybody does on a new install and the reason a touchpad feels broken
;;;;     until it is done;
;;;;   * nobody could set their key repeat rate, so the shipped one is whatever
;;;;     the compositor guessed;
;;;;   * *anybody who does not type on a US keyboard could not use the program
;;;;     at all* except by setting XKB_DEFAULT_LAYOUT before river starts,
;;;;     which is not something a window manager may ask of its user.
;;;;
;;;; None of that is a missing feature in the sense of an unwritten nicety.  A
;;;; tiling window manager on river owns input configuration the way it owns
;;;; the layout: there is nothing else on the machine that will do it.
;;;;
;;;; WHY THIS IS POLICY AND NOT RUNTIME.  Applying a setting is mechanism and
;;;; lives in runtime/input.lisp.  *Which* setting applies to *which* device is
;;;; a decision, and it is one people have strong and specific opinions about —
;;;; natural scrolling on the touchpad but not on the mouse is the canonical
;;;; example and it is not expressible by any number of global flags.  So the
;;;; decision is a generic taking a device, the tier-0 options are what the
;;;; shipped method reads, and a rule table sits between them for the ninety
;;;; percent case that wants neither.
;;;;
;;;; THE THREE TIERS, exactly as DESIGN's tier table describes them:
;;;;
;;;;   tier 0  (setf *tap-to-click* t)
;;;;   tier 0  (setf *input-rules* '(("Logitech" :natural-scroll nil)))
;;;;   tier 2  (defmethod input-settings ((p my-policy) device)
;;;;             (if (mine-p device) '(:accel-speed 0.4d0) (call-next-method)))
;;;;
;;;; WHY THE FIFTEEN GLOBALS ARE NOT FOLDED INTO *INPUT-RULES*, which is the
;;;; obvious next thought and has been had more than once: the two really are
;;;; the same shape, and shipping *INPUT-RULES* with a leading (T :TAP-TO-CLICK
;;;; T ...) entry really would make OPTION-SETTINGS unnecessary.  Three things
;;;; are lost, and together they are more than the twenty lines gained.
;;;;
;;;;   * Fifteen docstrings.  `--list-options' prints them, the generated
;;;;     extension surface carries them, and half of them are the only place
;;;;     the program explains what :CLICKFINGER is or why tap-to-click ships on
;;;;     against libinput's default.  A plist has nowhere to put any of that.
;;;;   * The first line of the tier table above.  (SETF *TAP-TO-CLICK* T) is
;;;;     what a person types; editing one entry of a list of lists in an
;;;;     init.lisp is not the same act, and tier 0 exists for the people who
;;;;     will never write a DEFMETHOD.
;;;;   * Gate 11.  It certifies that every registered option is *read* by
;;;;     something, using the cross-references SBCL recorded while compiling.
;;;;     A table walked with SYMBOL-VALUE records no such reference, so the
;;;;     one instrument that finds a dead knob would go blind on all fifteen.
;;;;
;;;; The duplication the fold would remove was never the expensive part.  What
;;;; was expensive was that the plist those globals produce was then filtered
;;;; by a reader that dropped every false value — see SETTINGS-TO-SEND in
;;;; runtime/input.lisp.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; TIER 0 — the values
;;; ==================================================================

(define-option *tap-to-click* t
  "Tap a touchpad to click, rather than pressing it down.

T on every touchpad that supports it and ignored by everything that does not,
which is the shape most of these options have: a setting a device cannot honour
is reported in `list-inputs' and never sent.

On by default, against libinput's own default of off, and deliberately: every
desktop environment that ships a touchpad panel turns this on, the people who
want it off know they want it off, and a window manager whose touchpad appears
not to respond to taps reads as broken rather than as conservative.")

(define-option *natural-scroll* nil
  "Scroll content in the direction of the fingers, as on a phone.

NIL matches libinput's default and the behaviour of a mouse wheel.  Set it per
device rather than globally if you want it on the touchpad and off on the
mouse, which is what most people who want it at all actually want:

    (setf *input-rules* '((\"Touchpad\" :natural-scroll t)))")

(define-option *disable-while-typing* t
  "Ignore the touchpad briefly after a keystroke, so a palm does not move the
cursor mid-sentence.

libinput calls this `dwt'.  On by default because the failure it prevents —
the cursor jumping into the middle of a line you are typing — is one people
blame on the machine rather than on a setting.")

(define-option *click-method* nil
  "How a touchpad press becomes a left, middle or right click.

  NIL             leave the device's own default alone
  :BUTTON-AREAS   the bottom of the pad is divided into left/middle/right
  :CLICKFINGER    one, two or three fingers on the pad choose the button

:CLICKFINGER is what a MacBook does and what most people mean by `it should
just work'; :BUTTON-AREAS is what a ThinkPad's physical buttons imply.  The
default is NIL because libinput already picks the right one per device far
more often than a global setting could.")

(define-option *scroll-method* nil
  "How scrolling is expressed on a touchpad or trackpoint.

  NIL              leave the device's own default alone
  :TWO-FINGER      two fingers anywhere on the pad
  :EDGE            one finger along the right-hand edge
  :ON-BUTTON-DOWN  hold *SCROLL-BUTTON* and move — the trackpoint answer
  :NO-SCROLL       off

:ON-BUTTON-DOWN with *SCROLL-BUTTON* set to the middle button is the
configuration every ThinkPad user recreates by hand on every new machine.")

(define-option *scroll-button* nil
  "The button held down for :ON-BUTTON-DOWN scrolling, as an evdev code.

274 is BTN_MIDDLE, which is the one anybody means.  NIL leaves the device's
default.  Only consulted when *SCROLL-METHOD* is :ON-BUTTON-DOWN.")

(define-option *middle-emulation* nil
  "Pressing left and right together counts as a middle click.

For the machines that have two physical buttons and no third.  NIL leaves the
device's default, which is off.")

(define-option *left-handed* nil
  "Swap the left and right buttons.")

(define-option *tap-and-drag* t
  "Tap, then hold and move, to drag without pressing the pad down.

Only meaningful where *TAP-TO-CLICK* is on.  Matches libinput's default.")

(define-option *drag-lock* nil
  "Keep a tap-drag going across a brief lift of the finger.

  NIL       off
  :TIMEOUT  the drag survives a short pause and then ends by itself
  :STICKY   the drag continues until you tap again

Off by default because a drag that does not end when you lift your finger is
astonishing the first time it happens.")

(define-option *accel-profile* nil
  "How pointer speed responds to how fast you move.

  NIL        leave the device's own default alone
  :ADAPTIVE  faster movement covers more ground — what a touchpad wants
  :FLAT      one to one — what a gaming mouse and a drawing tablet want
  :NONE      no acceleration handling at all")

(define-option *accel-speed* nil
  "Pointer speed, from -1 (slowest) through 0 (the device's default) to 1.

A double float or NIL.  The scale is libinput's and is not linear; 0.3 is a
noticeably brisk touchpad and 1.0 is faster than most people can aim.")

(define-option *scroll-factor* nil
  "Multiply every scroll event's distance by this.

A positive real, or NIL for the compositor's default of 1.  This is river's own
knob rather than libinput's, so it works on devices libinput does not drive,
and it is the right answer for `my scrolling is too fast in every application'
— which is a compositor-level complaint dressed as an application one.")

(define-option *repeat-rate* nil
  "Key repeats per second once a held key starts repeating, or NIL for the
compositor's default.

25 is a common choice and 50 is fast.  Set together with *REPEAT-DELAY*: the
two are one decision and river takes them in one request.")

(define-option *repeat-delay* nil
  "Milliseconds a key must be held before it starts repeating, or NIL.

300 is brisk, 600 is the usual default, and below about 200 an ordinary
keypress starts repeating before your finger has left it.")

(define-option *xkb-layout* nil
  "The keyboard layout the compositor should use: \"us\", \"de\", \"us,de\".

NIL — the default — leaves whatever river started with, which comes from
XKB_DEFAULT_LAYOUT in the session environment.  Setting this compiles a keymap
and hands it to every keyboard; see RUNTIME:APPLY-KEYBOARD-LAYOUT for what that
needs.

*THIS IS THE OPTION THAT DECIDES WHETHER THE PROGRAM IS USABLE AT ALL* for
somebody who does not type on a US keyboard, and until it existed the only
answer was to set an environment variable before river started — which is not
something a window manager may ask of its user.

NOT THE SAME THING AS *KEYBOARD-LAYOUT*, and the difference is worth one
sentence because the names are close.  This one changes *what the keys do*, for
every application on the machine.  *KEYBOARD-LAYOUT* is a shift map: it tells
the window manager what the shifted glyphs are, so that its own prompt inserts
the right character.  Setting this one adopts the matching shift map
automatically when a table of that name is registered, so in the ordinary case
you set this and nothing else.")

(define-option *xkb-variant* nil
  "The XKB variant, e.g. \"nodeadkeys\", \"dvorak\", \"colemak\", or NIL.

Comma separated, one per layout, when *XKB-LAYOUT* names several.")

(define-option *xkb-options* nil
  "XKB options, comma separated, e.g. \"ctrl:nocaps,grp:alt_shift_toggle\".

`ctrl:nocaps' — caps lock becomes control — is the single most requested one
and is most of why this option exists.")

(define-option *xkb-model* nil
  "The XKB model, e.g. \"pc105\".  NIL uses the default, which is nearly always
right on anything made this century.")

(define-option *xkb-rules* nil
  "The XKB rules set, e.g. \"evdev\".  NIL uses the default.")

(define-option *numlock* nil
  "Turn num lock on for every keyboard as it appears.

T is what somebody with a full-size keyboard and a numeric keypad wants and
nothing else does.")

(define-option *input-rules* '()
  "Per-device settings, as a list of (MATCHER . PLIST).

The tier-0 answer to the fact that one set of global values cannot describe a
laptop, because a laptop has a touchpad *and* a mouse and they want opposite
things:

    (setf *input-rules*
          '((\"Touchpad\"  :natural-scroll t :accel-speed 0.3d0)
            (\"Logitech\"  :accel-profile :flat)
            (:keyboard   :repeat-rate 50 :repeat-delay 300)))

MATCHER is T for every device, a keyword for every device of that kind
(:KEYBOARD :POINTER :TOUCH :TABLET), a string matched case-insensitively
against the device name, or a function of one device.  Every matching rule
applies, in order, and a later one wins — so a rule for T is a default and a
rule naming a device is an override.

The property names are the option names without the stars: :TAP-TO-CLICK,
:NATURAL-SCROLL, :ACCEL-SPEED, :REPEAT-RATE and the rest.  `latticewm
--list-options' prints the whole set, and `(list-inputs)' prints what each
device present actually supports.")

;;; ==================================================================
;;; THE SHIPPED ANSWERS
;;;
;;; The generics themselves — INPUT-SETTINGS and KEYBOARD-LAYOUT-FOR — are
;;; declared in policy/protocol.lisp with the rest of the extension surface,
;;; because that file is meant to be readable end to end as the map of what is
;;; changeable.  What lives here is the answers.
;;; ==================================================================

(defun option-settings ()
  "The tier-0 options, as an INPUT-SETTINGS plist.

Only the ones that are set: NIL means `leave the device alone' for every
option whose default is NIL, and skipping them here is what makes that true
rather than merely documented."
  (let ((out '()))
    (flet ((put (key value) (when value (setf (getf out key) value))))
      ;; Booleans whose default is meaningful in both directions have to be
      ;; sent either way, so they are not filtered through PUT.
      (setf (getf out :tap-to-click) (and *tap-to-click* t)
            (getf out :tap-and-drag) (and *tap-and-drag* t)
            (getf out :natural-scroll) (and *natural-scroll* t)
            (getf out :disable-while-typing) (and *disable-while-typing* t)
            (getf out :left-handed) (and *left-handed* t)
            (getf out :middle-emulation) (and *middle-emulation* t))
      (put :drag-lock *drag-lock*)
      (put :click-method *click-method*)
      (put :scroll-method *scroll-method*)
      (put :scroll-button *scroll-button*)
      (put :accel-profile *accel-profile*)
      (put :accel-speed *accel-speed*)
      (put :scroll-factor *scroll-factor*)
      (put :repeat-rate *repeat-rate*)
      (put :repeat-delay *repeat-delay*)
      ;; :NUMLOCK travels the same per-device pipeline as everything else, so a
      ;; rule can set it for one keyboard.  It is applied by the runtime's lock
      ;; path, not sent as a libinput setting -- see SETTINGS-TO-SEND, which
      ;; excludes it exactly as it excludes the two repeat halves.
      (put :numlock *numlock*))
    out))

(defun apply-input-rules (device settings &optional (rules *input-rules*))
  "SETTINGS with every rule in RULES that matches DEVICE laid over it.

In order, later wins, so a rule for T reads as a default and a rule naming a
device reads as an override — which is the order people write them in and the
order they expect them to resolve in."
  (let ((out (copy-list settings)))
    (dolist (rule rules out)
      (let ((matcher (first rule))
            (plist (rest rule)))
        (when (c:input-device-matches-p device matcher)
          (loop for (key value) on plist by #'cddr
                do (setf (getf out key) value)))))))

(defmethod input-settings ((policy input-policy) device)
  "The tier-0 options, with *INPUT-RULES* laid over them.

Keyboards get only the two settings that mean anything to a keyboard, because
sending `natural scroll' to one is a round trip whose answer is `unsupported'
and a line in the log that says nothing."
  (let ((settings (apply-input-rules device (option-settings))))
    (if (eq :keyboard (c:input-device-kind device))
        (list :repeat-rate (getf settings :repeat-rate)
              :repeat-delay (getf settings :repeat-delay)
              ;; :NUMLOCK is a keyboard setting too; dropping it here filtered a
              ;; per-device numlock rule out with the pointer-only options.
              :numlock (getf settings :numlock))
        settings)))

(defmethod keyboard-layout-for ((policy input-policy) device)
  "*XKB-LAYOUT* and friends, for every keyboard, or NIL when it is unset."
  (declare (ignore device))
  (when *xkb-layout*
    (values *xkb-layout* *xkb-variant* *xkb-options* *xkb-model* *xkb-rules*)))
