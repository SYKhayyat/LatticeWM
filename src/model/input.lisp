;;;; model/input.lisp --- What the window manager knows about an input device.
;;;;
;;;; The same argument as model/window.lisp, one subsystem over: policy has to
;;;; be able to ask a device about itself — INPUT-SETTINGS wants its name and
;;;; its kind, KEYBOARD-REPEAT wants to know it is a keyboard — and policy may
;;;; not depend on the runtime.  So the class is model state, the runtime owns
;;;; the three proxies and fills these slots from protocol events, and nothing
;;;; here calls through any of them.
;;;;
;;;; THREE PROXIES FOR ONE DEVICE, and that is river's decomposition rather
;;;; than ours.  A touchpad is:
;;;;
;;;;   river_input_device_v1     every device has one.  Name, kind, key repeat,
;;;;                             scroll factor, which output a stylus maps to.
;;;;   river_libinput_device_v1  only devices libinput drives.  Tap-to-click,
;;;;                             natural scrolling, acceleration — everything a
;;;;                             person means by "configure my touchpad".
;;;;   river_xkb_keyboard_v1     only keyboards.  Layout, capslock, numlock.
;;;;
;;;; They arrive on three separate globals, in any order, and are joined by
;;;; object identity: the libinput and xkb objects each send an `input_device'
;;;; event naming the river_input_device_v1 they belong to.  This class is
;;;; where the three halves meet, which is why it exists at all rather than the
;;;; runtime keeping three hash tables and hoping.

(in-package #:latticewm/core)

(defparameter +input-device-types+ '(:keyboard :pointer :touch :tablet)
  "The device kinds river_input_device_v1.type can report, in protocol order.

Indexed by the wire value, so the order is the protocol's and not ours.")

(defclass input-device ()
  ((proxy :initarg :proxy :initform nil :accessor input-device-proxy
          :documentation
          "The river_input_device_v1 object.  Opaque here, as with a window.")
   (libinput :initform nil :accessor input-device-libinput
             :documentation
             "The river_libinput_device_v1, or NIL when libinput does not drive
this device.  A virtual keyboard from a remote-desktop client has none, and
that is an ordinary answer rather than a failure.")
   (keyboard :initform nil :accessor input-device-keyboard
             :documentation "The river_xkb_keyboard_v1, or NIL.")
   (name :initarg :name :initform nil :accessor input-device-name
         :documentation
         "What libinput calls this device, e.g. \"ELAN0678:00 04F3:3195
Touchpad\".  This is what a configuration rule matches on, so it is worth
knowing that it is the *device* name and not the product name on the box.
`latticewm --eval (list-inputs)' prints the ones actually present.")
   (kind :initarg :kind :initform nil :accessor input-device-kind
         :documentation
         "One of +INPUT-DEVICE-TYPES+, or NIL before the type event arrives.")
   (capabilities :initform '() :accessor input-device-capabilities
                 :documentation
                 "PROPERTY -> what this device supports, as reported by the
`_support' events.  A plist, because it is read far more often than written and
is never large.

Consulted before a setting is sent, so that asking a trackball for
tap-to-click is a line in `list-inputs' rather than a protocol round trip that
answers `unsupported'.")
   (settings :initform '() :accessor input-device-settings
             :documentation
             "PROPERTY -> the value currently in force, as last reported by the
`_current' events.  A plist.  This is the *device's* answer, not ours: it is
what makes the difference between `we asked for tap' and `tap is on' visible.")
   (props :initform '() :accessor props))
  (:documentation
   "One input device: a keyboard, a mouse, a touchpad, a touchscreen, a stylus.

Created when river announces it and retained until it is removed.  Everything
configurable about it is reached from here, and every configuration decision is
a policy question — see POLICY:INPUT-SETTINGS."))

(defmethod print-object ((device input-device) stream)
  (print-unreadable-object (device stream :type t :identity nil)
    (format stream "~@[~(~a~) ~]~s~@[ ~a~]"
            (input-device-kind device)
            (or (input-device-name device) "")
            (cond ((input-device-keyboard device) "xkb")
                  ((input-device-libinput device) "libinput")))))

(defun input-device-capability (device property)
  "What DEVICE supports for PROPERTY, or NIL when it has not said.

NIL is `no answer yet' rather than `not supported', because the support events
arrive after the device does and a caller in between must not conclude the
device is limited.  The runtime waits for the first round trip before applying
anything, so in practice a NIL here means the device genuinely never mentioned
it."
  (getf (input-device-capabilities device) property))

(defun (setf input-device-capability) (value device property)
  (setf (getf (input-device-capabilities device) property) value))

(defun input-device-setting (device property)
  "The value of PROPERTY currently in force on DEVICE, or NIL."
  (getf (input-device-settings device) property))

(defun (setf input-device-setting) (value device property)
  (setf (getf (input-device-settings device) property) value))

(defun input-device-matches-p (device matcher)
  "Does MATCHER select DEVICE?

Three kinds of matcher, in the order somebody reaches for them:

  T                 every device.
  :KEYBOARD etc.    every device of that kind.
  \"Touchpad\"        every device whose name contains that text, case
                    insensitively — because the real names have vendor ids and
                    bus numbers in them and nobody types those correctly twice.

A function is also accepted and is given the device, for the rule that is
genuinely a program.  That is the escape hatch, and it exists so that the
string case can stay simple rather than growing a pattern language."
  (etypecase matcher
    ((eql t) t)
    (keyword (eq matcher (input-device-kind device)))
    (string (let ((name (input-device-name device)))
              (and name (search matcher name :test #'char-equal) t)))
    (function (and (funcall matcher device) t))
    (symbol (eq matcher (input-device-kind device)))))
