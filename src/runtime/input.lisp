;;;; runtime/input.lisp --- Configuring the machine's keyboards and pointers.
;;;;
;;;; THE LARGEST ABSENCE THE PROGRAM HAD.  river vendors three protocols for
;;;; input configuration, all three XML files were in this tree, and none of
;;;; them was bound.  On river there is nothing else on the system that does
;;;; this — no settings daemon, no `xinput', no compositor-side config file —
;;;; so a window manager that does not do it means the machine cannot be
;;;; configured at all.  Concretely: no tap-to-click, no key repeat rate, and
;;;; no keyboard layout other than whatever XKB_DEFAULT_LAYOUT happened to say
;;;; before river started.
;;;;
;;;; THE THREE PROTOCOLS, and why there are three:
;;;;
;;;;   river_input_manager_v1     every device.  Name, kind, key repeat, scroll
;;;;                              factor, and which output a stylus maps to.
;;;;   river_libinput_config_v1   the devices libinput drives.  Tap, natural
;;;;                              scroll, acceleration, click and scroll
;;;;                              method — everything a person means by
;;;;                              "configure my touchpad".
;;;;   river_xkb_config_v1        keyboards.  The keymap, layout switching,
;;;;                              capslock and numlock.
;;;;
;;;; A touchpad therefore arrives three times, on three globals, in an order
;;;; nothing guarantees, and the three halves are joined by object identity:
;;;; the libinput and xkb objects each send an `input_device' event naming the
;;;; river_input_device_v1 they belong to.  CORE:INPUT-DEVICE is where they
;;;; meet; SERVER-DEVICE-INDEX maps any of the three proxies to it.
;;;;
;;;; NOTHING IS APPLIED WHERE IT IS LEARNED.  A device announces itself with a
;;;; burst of twenty-odd events — what it is, what it supports, what is
;;;; currently in force — and configuring it after the first would mean
;;;; deciding what a touchpad supports before it had said so.  So every event
;;;; sets SERVER-INPUTS-DIRTY and the work happens once per pass of the event
;;;; loop, after the whole burst has been read.  Re-running it is free, because
;;;; every setting is diffed against what the device says is already in force.
;;;;
;;;; NONE OF THIS IS SEQUENCE-BOUND.  These are separate globals, outside
;;;; river_window_manager_v1 entirely, so the manage/render discipline does not
;;;; apply and a setting can be sent at any moment on the window manager's own
;;;; thread.  That is why `(setf *tap-to-click* t) (reload-input)' works from a
;;;; REPL with no round trip, and it is the only subsystem here of which that
;;;; is true.

(in-package #:latticewm/runtime)

;;; ==================================================================
;;; THE SETTING TABLE
;;; ==================================================================
;;;
;;; One row per libinput setting: the property name a policy uses, the request
;;; that sends it, how a Lisp value becomes the protocol's, and which
;;; `_support' answer gates it.
;;;
;;; What it is *diffed* against is not in the row, because it does not have to
;;; be: the `_current' event handlers record every answer under the same
;;; property name, so the comparison is a lookup by the key that is already
;;; here.  A row naming its own current-value key was one more thing that could
;;; be wrong by one, in a table whose whole argument is that there is one place
;;; for each fact.
;;;
;;; A TABLE RATHER THAN TWENTY FUNCTIONS because every one of these is the same
;;; four steps in the same order, and twenty copies of four steps is twenty
;;; chances for one of them to check the wrong support flag.  The row is also
;;; what LIST-INPUTS prints, so the documentation of what is configurable
;;; cannot drift from what is configurable.

(defstruct (input-setting (:constructor make-input-setting (property setter
                                                            &key coder support)))
  "One configurable libinput property.  See +LIBINPUT-SETTINGS+."
  (property nil :type keyword)
  (setter nil :type symbol)
  (coder nil)
  (support nil :type symbol))

(defun boolean-state (value)
  "T or NIL as libinput's two-valued enum."
  (if value :enabled :disabled))

(defun one-of (value table)
  "VALUE mapped through TABLE, an alist, or NIL when it is not in it.

NIL for an unrecognised value rather than an error, and that is deliberate: a
configuration file is user input, and a typo in `(setf *click-method*
:clickfingers)' should leave the touchpad alone and say so in the log rather
than take down the startup that was reading it."
  (cdr (assoc value table)))

(defparameter +click-methods+
  '((:none . :none) (:button-areas . :button-areas) (:clickfinger . :clickfinger)))

(defparameter +scroll-methods+
  '((:no-scroll . :no-scroll) (:none . :no-scroll) (:two-finger . :two-finger)
    (:edge . :edge) (:on-button-down . :on-button-down) (:button . :on-button-down)))

(defparameter +accel-profiles+
  '((:none . :none) (:flat . :flat) (:adaptive . :adaptive) (:custom . :custom)))

(defparameter +drag-locks+
  '((nil . :disabled) (:disabled . :disabled)
    (:timeout . :enabled-timeout) (t . :enabled-timeout)
    (:sticky . :enabled-sticky)))

(defparameter +button-maps+ '((:lrm . :lrm) (:lmr . :lmr)))

(defparameter +three-finger-drags+
  '((nil . :disabled) (:disabled . :disabled)
    (t . :enabled-3fg) (3 . :enabled-3fg) (:3 . :enabled-3fg)
    (4 . :enabled-4fg) (:4 . :enabled-4fg)))

(defparameter +send-events-modes+
  '((t . :enabled) (:enabled . :enabled) (nil . :disabled) (:disabled . :disabled)
    (:disabled-on-external-mouse . :disabled-on-external-mouse)))

(defun double-bytes (value)
  "A real as the eight little-endian bytes libinput's `array' arguments carry.

The protocol says `array' and the summary says `double', which means the raw
bytes of an IEEE 754 double in host order — every machine this runs on is
little-endian, and one that is not would need this function and nothing else."
  (let* ((x (float value 1d0))
         (bits (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits x)) 32)
                       (sb-kernel:double-float-low-bits x)))
         (out (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (i 8 out)
      (setf (aref out i) (ldb (byte 8 (* 8 i)) bits)))))

(defun bytes-double (bytes)
  "The inverse of DOUBLE-BYTES, or NIL when BYTES is not eight bytes long."
  (when (and bytes (= 8 (length bytes)))
    (let ((bits 0))
      (dotimes (i 8)
        (setf bits (logior bits (ash (elt bytes i) (* 8 i)))))
      (sb-kernel:make-double-float
       (let ((high (ldb (byte 32 32) bits)))
         (if (logbitp 31 high) (- high (ash 1 32)) high))
       (ldb (byte 32 0) bits)))))

(defparameter +libinput-settings+
  (list
   (make-input-setting :tap-to-click 'w:libinput-set-tap
                       :coder #'boolean-state
                       :support :tap)
   (make-input-setting :tap-and-drag 'w:libinput-set-drag
                       :coder #'boolean-state)
   (make-input-setting :tap-button-map 'w:libinput-set-tap-button-map
                       :coder (lambda (v) (one-of v +button-maps+))
                       :support :tap)
   (make-input-setting :drag-lock 'w:libinput-set-drag-lock
                       :coder (lambda (v) (one-of v +drag-locks+)))
   (make-input-setting :three-finger-drag 'w:libinput-set-three-finger-drag
                       :coder (lambda (v) (one-of v +three-finger-drags+))
                       :support :three-finger-drag)
   (make-input-setting :natural-scroll 'w:libinput-set-natural-scroll
                       :coder #'boolean-state
                       :support :natural-scroll)
   (make-input-setting :left-handed 'w:libinput-set-left-handed
                       :coder #'boolean-state
                       :support :left-handed)
   (make-input-setting :middle-emulation 'w:libinput-set-middle-emulation
                       :coder #'boolean-state
                       :support :middle-emulation)
   (make-input-setting :disable-while-typing 'w:libinput-set-dwt
                       :coder #'boolean-state
                       :support :dwt)
   (make-input-setting :disable-while-trackpointing 'w:libinput-set-dwtp
                       :coder #'boolean-state
                       :support :dwtp)
   (make-input-setting :click-method 'w:libinput-set-click-method
                       :coder (lambda (v) (one-of v +click-methods+))
                       :support :click-method)
   (make-input-setting :clickfinger-button-map
                       'w:libinput-set-clickfinger-button-map
                       :coder (lambda (v) (one-of v +button-maps+)))
   (make-input-setting :scroll-method 'w:libinput-set-scroll-method
                       :coder (lambda (v) (one-of v +scroll-methods+))
                       :support :scroll-method)
   (make-input-setting :scroll-button 'w:libinput-set-scroll-button
                       :coder (lambda (v) (and (integerp v) v)))
   (make-input-setting :scroll-button-lock 'w:libinput-set-scroll-button-lock
                       :coder #'boolean-state)
   (make-input-setting :accel-profile 'w:libinput-set-accel-profile
                       :coder (lambda (v) (one-of v +accel-profiles+))
                       :support :accel-profiles)
   (make-input-setting :accel-speed 'w:libinput-set-accel-speed
                       :coder (lambda (v) (and (realp v) (double-bytes v))))
   (make-input-setting :send-events 'w:libinput-set-send-events
                       :coder (lambda (v) (one-of v +send-events-modes+))
                       :support :send-events)
   (make-input-setting :rotation 'w:libinput-set-rotation
                       :coder (lambda (v) (and (integerp v) (mod v 360)))
                       :support :rotation))
  "Every libinput property this window manager knows how to set.

Order is the order LIST-INPUTS prints them in, which is roughly the order
somebody configuring a touchpad thinks about them.")

(defun find-input-setting (property)
  "The row for PROPERTY, or NIL."
  (find property +libinput-settings+ :key #'input-setting-property))

(defun supported-p (device setting)
  "Does DEVICE support SETTING?

Three answers rather than two.  A row with no support key is sent regardless,
because the protocol offers no way to ask and the result object will say; a
device that has not answered yet is treated as *supporting* it, because the
alternative is skipping every setting on the first pass and the work is
re-run when the answer arrives; and an explicit answer is believed."
  (let ((key (input-setting-support setting)))
    (if (null key)
        t
        (let ((answer (c:input-device-capability device key)))
          (cond ((null answer) t)
                ((integerp answer) (plusp answer))
                ((listp answer) (and answer t))
                (t (and answer t)))))))

;;; ==================================================================
;;; APPLYING
;;; ==================================================================

(defun libinput-result-hook (device property)
  "A hook for the river_libinput_result_v1 a setter hands back.

Every libinput setter returns one of these and it answers exactly once:
success, unsupported, or invalid.  Listening is not optional bookkeeping — it
is the only way a wrong value in a configuration file ever becomes a sentence
somebody can read.  `unsupported' is logged at debug because it is ordinary
(a trackball has no tap); `invalid' is logged at warn because it means the
configuration said something that is not a thing."
  (lambda (event &rest arguments)
    (declare (ignore arguments))
    (with-abandon
      (case event
        (:success (logmsg :debug "~a: ~(~a~) set"
                          (or (c:input-device-name device) "device") property))
        (:unsupported
         (logmsg :debug "~a: ~(~a~) is not supported by this device"
                 (or (c:input-device-name device) "device") property))
        (:invalid
         (logmsg :warn "~a: ~(~a~) was refused as invalid -- check the value ~
                        in your configuration"
                 (or (c:input-device-name device) "device") property))
        (t nil)))))

(defun send-libinput-setting (device setting value)
  "Send one setting to DEVICE, if it is supported and not already in force.

Returns T when something was sent.  The diff is against the device's own
`_current' answer rather than against what we last sent, which is the
difference between `we asked for tap' and `tap is on' — and the reason
re-applying the whole configuration on every reload costs nothing."
  (let ((proxy (c:input-device-libinput device))
        (property (input-setting-property setting)))
    (when (and proxy (supported-p device setting))
      (let* ((coder (input-setting-coder setting))
             (wire (if coder (funcall coder value) value)))
        (cond
          ((null wire)
           (logmsg :warn "~a: ~s is not a value ~(~a~) accepts"
                   (or (c:input-device-name device) "device") value property)
           nil)
          ((equalp wire (c:input-device-setting device property)) nil)
          (t
           (let ((result (best-effort "libinput set"
                           (funcall (input-setting-setter setting) proxy wire))))
             (when result
               (push (libinput-result-hook device property)
                     (wl:wl-proxy-hooks result))
               ;; Recorded optimistically, and corrected by the `_current'
               ;; event that follows a successful set.  Without this a device
               ;; that reports nothing back would be re-sent the same value on
               ;; every pass of the event loop, forever.
               (setf (c:input-device-setting device property) wire)
               t))))))))

(defun send-device-setting (device property value)
  "Send PROPERTY to DEVICE, whichever protocol carries it.

Three properties are river's rather than libinput's — the two halves of key
repeat and the scroll factor — because they apply to devices libinput does not
drive, which includes every virtual keyboard a remote-desktop client creates."
  (case property
    ((:repeat-rate :repeat-delay)
     ;; One request carrying both, so half of it is still a call to the
     ;; function that sends the pair -- which fills the other half in from what
     ;; is already in force.  Returning NIL here instead, on the grounds that
     ;; APPLY-DEVICE-SETTINGS handles the pair, made (SET-INPUT "kbd"
     ;; :REPEAT-RATE 40) report `0 devices' and do nothing: correct for the
     ;; caller it was written for and wrong for the one a person types.
     (apply-repeat-info device (list property value)))
    (:scroll-factor
     (let ((proxy (c:input-device-proxy device)))
       (when (and proxy (realp value) (plusp value)
                  (not (eql value (c:input-device-setting device :scroll-factor))))
         (best-effort "set_scroll_factor" (w:input-set-scroll-factor proxy (float value)))
         (setf (c:input-device-setting device :scroll-factor) value)
         t)))
    (:map-to-output
     (let ((proxy (c:input-device-proxy device))
           (output (wl-output-named value)))
       (when (and proxy output
                  (not (equal value (c:input-device-setting device :map-to-output))))
         (best-effort "map_to_output" (w:input-map-to-output proxy output))
         (setf (c:input-device-setting device :map-to-output) value)
         t)))
    (t (let ((setting (find-input-setting property)))
         (cond (setting (send-libinput-setting device setting value))
               (t (logmsg :warn "no such input setting: ~(~a~)" property)
                  nil))))))

(defun apply-repeat-info (device settings)
  "Send key repeat, which is one request carrying two numbers.

Both or neither: river takes rate and delay together, so setting only one in a
configuration file has to mean `keep the other', and the value it keeps is the
one already in force rather than a zero."
  (let ((rate (getf settings :repeat-rate))
        (delay (getf settings :repeat-delay))
        (proxy (c:input-device-proxy device)))
    (when (and proxy (or rate delay))
      (let ((rate (or rate (c:input-device-setting device :repeat-rate) 25))
            (delay (or delay (c:input-device-setting device :repeat-delay) 600)))
        (unless (and (eql rate (c:input-device-setting device :repeat-rate))
                     (eql delay (c:input-device-setting device :repeat-delay)))
          (best-effort "set_repeat_info"
            (w:input-set-repeat-info proxy (max 0 rate) (max 0 delay)))
          (setf (c:input-device-setting device :repeat-rate) rate
                (c:input-device-setting device :repeat-delay) delay)
          t)))))

(defun first-of-each (plist)
  "PLIST with each property's *first* occurrence kept and the rest dropped.

GETF answers with the first match, so that is what a plist means and that is
what the generic's docstring promises when it shows

    (append '(:accel-profile :flat) (call-next-method))

as the way to add one setting.  Sending every pair in order instead would send
the shipped answer *after* the override and quietly undo it -- the extension
would appear to work, because the first request really did go out, and then be
countermanded a microsecond later by the second."
  (let ((out '())
        (seen '()))
    (loop for (property value) on plist by #'cddr
          unless (member property seen)
            do (push property seen)
               (push property out)
               (push value out))
    (nreverse out)))

(defun settings-to-send (settings)
  "SETTINGS minus the pair APPLY-REPEAT-INFO sends as one request.

A KEY PRESENT WITH A VALUE OF NIL IS A SETTING OF NIL, and that sentence is
the whole reason this is a function with a name.  It used to be an inline
`UNLESS (OR (NULL VALUE) ...)', which dropped every setting whose value was
false — so `(setf *tap-to-click* nil)' sent nothing and the touchpad kept
tapping, and so did the rule `(\"Logitech\" :natural-scroll nil)', which is
the shape of the example in *NATURAL-SCROLL*'s own docstring.

NOTHING ABOVE THIS COULD SEE IT.  OPTION-SETTINGS goes out of its way to send
the six meaningful booleans either way, and says so in a comment; the tests
assert that the plist coming out of INPUT-SETTINGS carries `:NATURAL-SCROLL
NIL' when a rule asks for it.  Both were right.  The plist was correct all the
way down and then the last reader of it threw those pairs away.

There is no ambiguity left to resolve here: OPTION-SETTINGS omits an unset
option rather than writing NIL for it, and *INPUT-RULES* says in as many words
that a rule which does not mention a key does not set it.  Absent is `leave it
alone'; present is a value, whatever the value is."
  (loop for (property value) on settings by #'cddr
        ;; :NUMLOCK rides in INPUT-SETTINGS so a per-device rule reaches the
        ;; keyboard, but it is applied by APPLY-LOCK-STATE, not sent as a
        ;; libinput setting -- excluded here for the same reason the repeat pair
        ;; is, which SEND-DEVICE-SETTING would otherwise reject as unknown.
        unless (member property '(:repeat-rate :repeat-delay :numlock))
          collect property and collect value))

(defun apply-device-settings (device)
  "Ask the policy what DEVICE should be set to, and send whatever changed."
  (let ((settings (guarded "input-settings"
                    (p:input-settings (p:current-policy) device))))
    (if (listp settings)
        (let ((settings (first-of-each settings)))
          (+ (if (apply-repeat-info device settings) 1 0)
             (loop for (property value) on (settings-to-send settings) by #'cddr
                   count (send-device-setting device property value))))
        0)))

(defun apply-input-configuration ()
  "Configure every device the policy has an opinion about.

Called once per pass of the event loop when something has changed, which is
what makes the whole subsystem eventually consistent rather than
order-dependent: a device that has told us its name but not yet what it
supports is configured as far as it can be now and the rest when it says."
  (when (and *server* *world*)
    (setf (server-inputs-dirty *server*) nil)
    (let ((changed 0))
      (dolist (device (c:world-inputs *world*))
        (incf changed (or (apply-device-settings device) 0))
        (when (eq :keyboard (c:input-device-kind device))
          (when (apply-keyboard-layout device) (incf changed))
          ;; Here as well as at the join, because *NUMLOCK* can be set after a
          ;; keyboard has already arrived -- which is what happens on every
          ;; machine, since the configuration file is read before the
          ;; compositor has said what is plugged into it.
          (when (apply-lock-state device) (incf changed))))
      (when (plusp changed)
        (logmsg :debug "input: ~d setting~:p applied" changed))
      changed)))

(defun mark-inputs-dirty ()
  "Ask for the input configuration to be reconciled at the next opportunity."
  (when *server* (setf (server-inputs-dirty *server*) t))
  nil)

(defun apply-input-if-needed ()
  "Reconcile the input configuration if anything asked for it.  Cheap when not."
  (when (and *server* (server-inputs-dirty *server*))
    (with-abandon (apply-input-configuration))))

;;; ==================================================================
;;; THE KEYBOARD LAYOUT
;;; ==================================================================
;;;
;;; THE ONE PLACE THIS PROGRAM SHELLS OUT, and it is worth saying why.
;;; river_xkb_config_v1.create_keymap takes a *compiled* keymap on a file
;;; descriptor: rules, model, layout, variant and options are libxkbcommon's
;;; input, not the protocol's.  Compiling one means either linking
;;; libxkbcommon — a foreign dependency, in a program that currently has none
;;; and whose stated requirement is that it survive without anybody to
;;; maintain it — or running `xkbcli compile-keymap', which ships with
;;; libxkbcommon and is therefore already on any machine running river.
;;;
;;; The second, then, and the degradation is honest: no xkbcli means one log
;;; line naming the program that is missing and a keymap left exactly as river
;;; had it, which is the same behaviour as not setting the option.

(defvar *keymap-cache* (make-hash-table :test #'equal)
  "The xkb specification -> the river_xkb_keymap_v1 compiled from it.

Compiling is a fork, an exec and a file, and every keyboard on the machine
wants the same keymap.  Doing it once is the difference between plugging in a
second keyboard costing nothing and costing a process.")

(defun compile-xkb-keymap-text (layout &key variant options model rules)
  "The compiled XKB keymap for these names, as a string, or NIL.

Runs `xkbcli compile-keymap'.  NIL — with a log line saying which — for a
missing xkbcli, a layout name xkb does not know, or anything else that makes
the compiler unhappy; the caller leaves the keyboard alone in every case."
  (let ((arguments (append (list "compile-keymap" "--layout" layout)
                           (when variant (list "--variant" variant))
                           (when options (list "--options" options))
                           (when model (list "--model" model))
                           (when rules (list "--rules" rules)))))
    (handler-case
        (multiple-value-bind (output error status)
            (uiop:run-program (cons "xkbcli" arguments)
                              :output :string :error-output :string
                              :ignore-error-status t)
          (cond
            ((and (eql 0 status) (plusp (length output))) output)
            (t (logmsg :warn "could not compile the keymap for layout ~s: ~a"
                       layout (string-trim '(#\Newline #\Space) (or error "")))
               nil)))
      (error (condition)
        (logmsg :warn "could not run xkbcli to compile a keymap: ~a~%~
                       *XKB-LAYOUT* needs xkbcli, which ships with ~
                       libxkbcommon.  Without it the keyboard keeps the layout ~
                       river started with."
                condition)
        nil))))

(defun keymap-from-text (text)
  "Hand TEXT to river as a keymap, and return the river_xkb_keymap_v1.

The file descriptor is a temporary file that is unlinked the moment it exists,
so nothing is left behind by a crash — the same trick the shm path uses, for
the same reason.  A trailing NUL is written because that is what the format
this enum names means everywhere else in Wayland, and river's answer to a
wrong guess is a `failure' event carrying the compiler's own message."
  (let ((config (and *server* (server-xkb-config *server*))))
    (when config
      (let ((fd -1))
        (unwind-protect
             (let ((path (format nil "~a/latticewm-keymap-XXXXXX"
                                 (or (uiop:getenv "XDG_RUNTIME_DIR") "/tmp"))))
               (multiple-value-bind (descriptor file) (sb-posix:mkstemp path)
                 (setf fd descriptor)
                 (sb-posix:unlink file)
                 (let ((stream (sb-sys:make-fd-stream fd :output t :input t
                                                         :external-format :utf-8
                                                         :auto-close nil)))
                   (write-string text stream)
                   (write-char (code-char 0) stream)
                   (finish-output stream))
                 (sb-posix:lseek fd 0 sb-posix:seek-set)
                 (let ((keymap (best-effort "create_keymap"
                                 (w:xkb-create-keymap config fd :text-v1))))
                   (when keymap
                     (on-events (keymap "river_xkb_keymap_v1")
                       (:success (logmsg :info "keyboard layout compiled and accepted"))
                       (:failure (logmsg :error "river refused the keymap: ~a"
                                         (first arguments)))
                       (t nil)))
                   keymap)))
          (when (>= fd 0) (ignore-errors (sb-posix:close fd))))))))

(defvar *keymap-pending* (make-hash-table :test #'equal)
  "The keymap specs a worker is compiling right now, so we do not fork twice
for the same one while the first is still running.  Touched only on the wm
thread, like *KEYMAP-CACHE*.")

(defun keymap-for (layout variant options model rules)
  "The river_xkb_keymap_v1 for these names, compiled once and kept.

Compiling is a fork, an exec of `xkbcli' and a file read, and it blocks -- and
river waits on the window manager between messages, so doing it on the event
loop stalls the whole dispatch and therefore input, which is the one thing the
rest of this file works to keep non-blocking.  So the compile runs on a short
worker thread, and its result -- which needs the server proxy -- is applied
back on the wm thread through CALL-IN-WM-THREAD, the same primitive the IPC and
SWANK layers marshal through.

Until the compile lands KEYMAP-FOR answers NIL and the keyboard keeps the
layout river gave it; when it lands MARK-INPUTS-DIRTY re-runs the input
configuration, which now finds the keymap cached and applies it.  The new
layout arrives a beat later rather than after a stall, and a failed compile
caches NIL exactly as the synchronous version did -- so it is not retried and
the keyboard is simply left alone.

All of *KEYMAP-CACHE* and *KEYMAP-PENDING* are read and written on the wm
thread only (KEYMAP-FOR runs there, and the worker touches neither -- it hands
back through CALL-IN-WM-THREAD), so no lock is needed."
  (let ((key (list layout variant options model rules)))
    (multiple-value-bind (cached foundp) (gethash key *keymap-cache*)
      (cond
        (foundp cached)
        ((gethash key *keymap-pending*) nil)   ; a worker is already on it
        (t
         (setf (gethash key *keymap-pending*) t)
         (bt:make-thread
          (lambda ()
            (let ((text (compile-xkb-keymap-text
                         layout :variant variant :options options
                                :model model :rules rules)))
              (call-in-wm-thread
               (lambda ()
                 (unwind-protect
                      (setf (gethash key *keymap-cache*)
                            (and text (ignore-errors (keymap-from-text text))))
                   (remhash key *keymap-pending*))
                 (mark-inputs-dirty)))))
          :name "latticewm-xkb-compile")
         nil)))))

(defun adopt-shift-map-for (layout)
  "Use the shift map called LAYOUT, if one is registered.

*THE HALF THAT IS EASY TO FORGET.*  Setting *XKB-LAYOUT* to \"de\" changes what
the keys do for every application on the machine and changes nothing about what
this window manager believes the shifted glyphs are — so its own prompt would
go on inserting American punctuation on a German keyboard, which is exactly the
bug *KEYBOARD-LAYOUT* exists to prevent and exactly the one nobody would think
to look for.

Only when a table of that name is registered, and never over a table the layout
does not name: `(setf *xkb-layout* \"gb\")' leaves a hand-written map alone and
says so, because a wrong table is worse than the fallback."
  (let ((name (and (stringp layout)
                   (string-downcase (subseq layout 0 (or (position #\, layout)
                                                         (length layout)))))))
    (cond
      ((null name) nil)
      ((equal name (and (stringp p:*keyboard-layout*)
                        (string-downcase p:*keyboard-layout*)))
       nil)
      ((p:find-shift-map name)
       (setf p:*keyboard-layout* name)
       (logmsg :info "shift map ~s adopted to match the keyboard layout" name)
       t)
      (t (logmsg :info "no shift map called ~s; *KEYBOARD-LAYOUT* is still ~s, ~
                        so the window manager's own prompt will use that layout's ~
                        punctuation.  (REGISTER-SHIFT-MAP ~s ...) teaches it."
                 name p:*keyboard-layout* name)
         nil))))

(defun apply-keyboard-layout (device)
  "Give DEVICE the keymap the policy asks for, if it asks for one."
  (let ((keyboard (c:input-device-keyboard device)))
    (when keyboard
      (multiple-value-bind (layout variant options model rules)
          (guarded "keyboard-layout-for"
            (p:keyboard-layout-for (p:current-policy) device))
        (when (stringp layout)
          (let ((wanted (list layout variant options model rules)))
            (unless (equal wanted (c:input-device-setting device :xkb))
              (let ((keymap (keymap-for layout variant options model rules)))
                (when keymap
                  (best-effort "set_keymap" (w:xkb-keyboard-set-keymap keyboard keymap))
                  (setf (c:input-device-setting device :xkb) wanted)
                  (adopt-shift-map-for layout)
                  t)))))))))

(defun apply-lock-state (device)
  "Turn num lock on where the configuration -- global or per-device -- asks.

The wanted state is resolved through the same policy path as every other input
setting, so a per-device *INPUT-RULES* entry that sets :NUMLOCK for one keyboard
is honoured rather than overridden by the global *NUMLOCK* alone."
  (let ((keyboard (c:input-device-keyboard device))
        (wanted (let ((settings (guarded "input-settings"
                                  (p:input-settings (p:current-policy) device))))
                  (and (listp settings) (getf settings :numlock)))))
    (when (and keyboard wanted
               (not (eq t (c:input-device-setting device :numlock))))
      (best-effort "numlock_enable" (w:xkb-keyboard-numlock-enable keyboard))
      (setf (c:input-device-setting device :numlock) t))))

;;; ==================================================================
;;; THE EVENTS
;;; ==================================================================

(defun device-of-proxy (proxy)
  "Our INPUT-DEVICE for any of a device's three proxies, or NIL."
  (and *server* proxy (gethash proxy (server-device-index *server*))))

(defun forget-input-device (device)
  "A device was unplugged.  Take it out of everything.

The proxies go too: leaving a dead river_input_device_v1 in the index is how a
later event for a *reused* object id finds the wrong device, which is a class
of bug that only appears on the machine somebody docks twice a day."
  (when device
    (let ((index (server-device-index *server*)))
      (dolist (proxy (list (c:input-device-proxy device)
                           (c:input-device-libinput device)
                           (c:input-device-keyboard device)))
        (when proxy (remhash proxy index))))
    (setf (c:world-inputs *world*) (remove device (c:world-inputs *world*)))
    (run-hooks :input-removed device)
    (logmsg :info "input device removed: ~s" device))
  nil)

(defun attach-input-device (proxy)
  "Register a device river has announced, and follow what it says about itself."
  (let ((device (make-instance 'c:input-device :proxy proxy)))
    (setf (gethash proxy (server-device-index *server*)) device
          (c:world-inputs *world*)
          (append (c:world-inputs *world*) (list device)))
    (on-events (proxy "river_input_device_v1")
      (:name (setf (c:input-device-name device) (first arguments))
             (mark-inputs-dirty))
      (:type (setf (c:input-device-kind device)
                   (let ((value (first arguments)))
                     (if (integerp value)
                         (nth value c:+input-device-types+)
                         value)))
             (mark-inputs-dirty))
      (:removed
       (forget-input-device device)
       (best-effort "input device destroy" (w:input-destroy proxy)))
      (t nil))
    (run-hooks :input-added device)
    (mark-inputs-dirty)
    device))

(defun attach-libinput-device (proxy)
  "Follow a libinput device: what it supports and what is in force on it."
  (on-events (proxy "river_libinput_device_v1")
    ;; Always first, and exactly once, per the protocol.  This is the join.
    (:input-device
     (let ((device (device-of-proxy (first arguments))))
       (when device
         (setf (c:input-device-libinput device) proxy
               (gethash proxy (server-device-index *server*)) device)
         (mark-inputs-dirty))))
    (:removed
     (let ((device (device-of-proxy proxy)))
       (when device (setf (c:input-device-libinput device) nil))
       (remhash proxy (server-device-index *server*)))
     (best-effort "libinput destroy" (w:libinput-destroy proxy)))
    (t nil))
  ;; What the device supports and what is currently in force, as a second
  ;; handler on the same proxy.  Two handlers rather than one CASE of forty
  ;; clauses because the two are answering different questions -- the first is
  ;; the object's *lifecycle*, this is its *state* -- and because a hook list
  ;; is a list: both run, each ignores what it does not name, and gate 8 sees
  ;; the union.
  ;;
  ;; Every clause is one line of the same shape on purpose.  The bug this
  ;; shape prevents is recording `natural scroll' as the answer to `does this
  ;; device support tap', which is invisible to every test that constructs
  ;; state rather than receiving it -- the failure mode gate 8 was written for,
  ;; one interface over.
  (on-events (proxy "river_libinput_device_v1")
    (:send-events-support
     (libinput-record proxy :capability :send-events (first arguments)))
    (:send-events-current
     (libinput-record proxy :setting :send-events (first arguments)))
    (:tap-support (libinput-record proxy :capability :tap (first arguments)))
    (:tap-current (libinput-record proxy :setting :tap-to-click (first arguments)))
    (:tap-button-map-current
     (libinput-record proxy :setting :tap-button-map (first arguments)))
    (:drag-current (libinput-record proxy :setting :tap-and-drag (first arguments)))
    (:drag-lock-current
     (libinput-record proxy :setting :drag-lock (first arguments)))
    (:three-finger-drag-support
     (libinput-record proxy :capability :three-finger-drag (first arguments)))
    (:three-finger-drag-current
     (libinput-record proxy :setting :three-finger-drag (first arguments)))
    (:accel-profiles-support
     (libinput-record proxy :capability :accel-profiles (first arguments)))
    (:accel-profile-current
     (libinput-record proxy :setting :accel-profile (first arguments)))
    (:accel-speed-current
     (libinput-record proxy :setting :accel-speed (first arguments)))
    (:natural-scroll-support
     (libinput-record proxy :capability :natural-scroll (first arguments)))
    (:natural-scroll-current
     (libinput-record proxy :setting :natural-scroll (first arguments)))
    (:left-handed-support
     (libinput-record proxy :capability :left-handed (first arguments)))
    (:left-handed-current
     (libinput-record proxy :setting :left-handed (first arguments)))
    (:click-method-support
     (libinput-record proxy :capability :click-method (first arguments)))
    (:click-method-current
     (libinput-record proxy :setting :click-method (first arguments)))
    (:clickfinger-button-map-current
     (libinput-record proxy :setting :clickfinger-button-map (first arguments)))
    (:middle-emulation-support
     (libinput-record proxy :capability :middle-emulation (first arguments)))
    (:middle-emulation-current
     (libinput-record proxy :setting :middle-emulation (first arguments)))
    (:scroll-method-support
     (libinput-record proxy :capability :scroll-method (first arguments)))
    (:scroll-method-current
     (libinput-record proxy :setting :scroll-method (first arguments)))
    (:scroll-button-current
     (libinput-record proxy :setting :scroll-button (first arguments)))
    (:scroll-button-lock-current
     (libinput-record proxy :setting :scroll-button-lock (first arguments)))
    (:dwt-support (libinput-record proxy :capability :dwt (first arguments)))
    (:dwt-current
     (libinput-record proxy :setting :disable-while-typing (first arguments)))
    (:dwtp-support (libinput-record proxy :capability :dwtp (first arguments)))
    (:dwtp-current
     (libinput-record proxy :setting :disable-while-trackpointing (first arguments)))
    (:rotation-support
     (libinput-record proxy :capability :rotation (first arguments)))
    (:rotation-current (libinput-record proxy :setting :rotation (first arguments)))
    (:calibration-matrix-support
     (libinput-record proxy :capability :calibration-matrix (first arguments)))
    (t nil))
  proxy)

(defun libinput-record (proxy kind property value)
  "Record one `_support' or `_current' answer against PROXY's device.

Arriving before the join — which the protocol forbids, since `input_device' is
always first — would mean no device to record against, so this quietly does
nothing rather than signalling.  A device we cannot find is a device that was
unplugged between the event being sent and being read."
  (let ((device (device-of-proxy proxy)))
    (when device
      (ecase kind
        (:capability (setf (c:input-device-capability device property)
                           (if (eq value :none) nil value)))
        (:setting (setf (c:input-device-setting device property)
                        (normalize-libinput-value property value))))
      (mark-inputs-dirty))))

(defun normalize-libinput-value (property value)
  "A `_current' answer in the shape SEND-LIBINPUT-SETTING will compare against.

The diff is between what the device says is in force and what we are about to
send, so the two have to be in the same units.  Everything is already a keyword
except acceleration, which arrives as eight bytes and would otherwise never
compare equal to anything."
  (if (eq property :accel-speed)
      (let ((speed (bytes-double value)))
        (if speed (double-bytes speed) value))
      value))

(defun attach-xkb-keyboard (proxy)
  "Follow a keyboard's layout and lock state."
  (on-events (proxy "river_xkb_keyboard_v1")
    (:input-device
     (let ((device (device-of-proxy (first arguments))))
       (when device
         (setf (c:input-device-keyboard device) proxy
               (gethash proxy (server-device-index *server*)) device)
         (best-effort "lock state" (apply-lock-state device))
         (mark-inputs-dirty))))
    (:layout
     (let ((device (device-of-proxy proxy)))
       (when device
         (setf (c:input-device-setting device :layout-index) (first arguments)
               (c:input-device-setting device :layout-name) (second arguments))
         (logmsg :info "~a: layout ~d (~a)"
                 (or (c:input-device-name device) "keyboard")
                 (first arguments) (second arguments))
         (run-hooks :keyboard-layout-changed device (second arguments)))))
    (:capslock-enabled (record-lock proxy :capslock t))
    (:capslock-disabled (record-lock proxy :capslock nil))
    (:numlock-enabled (record-lock proxy :numlock t))
    (:numlock-disabled (record-lock proxy :numlock nil))
    (:removed
     (let ((device (device-of-proxy proxy)))
       (when device (setf (c:input-device-keyboard device) nil))
       (remhash proxy (server-device-index *server*)))
     (best-effort "xkb keyboard destroy" (w:xkb-keyboard-destroy proxy)))
    (t nil))
  proxy)

(defun record-lock (proxy property state)
  "Remember that capslock or numlock went on or off."
  (let ((device (device-of-proxy proxy)))
    (when device (setf (c:input-device-setting device property) state))))

;;; Each of the three globals is hooked where it is bound rather than in one
;;; pass afterwards, because a global can arrive at any time — the registry
;;; listener stays attached for the life of the connection — and a second pass
;;; would either miss the late ones or hook the early ones twice.

(defun attach-input-manager (proxy)
  "Listen for the machine's input devices."
  (on-events (proxy "river_input_manager_v1")
    (:input-device (attach-input-device (first arguments)))
    ;; `finished' means the compositor has taken the protocol away, which
    ;; happens when another client claims it.  Dropping the reference is the
    ;; whole of the correct response: the devices stay in the list, readable
    ;; and no longer settable.
    (:finished (when *server* (setf (server-input-manager *server*) nil)))
    (t nil))
  proxy)

(defun attach-libinput-config (proxy)
  "Listen for the devices libinput drives."
  (on-events (proxy "river_libinput_config_v1")
    (:libinput-device (attach-libinput-device (first arguments)))
    (:finished (when *server* (setf (server-libinput-config *server*) nil)))
    (t nil))
  proxy)

(defun attach-xkb-config (proxy)
  "Listen for the machine's keyboards."
  (on-events (proxy "river_xkb_config_v1")
    (:xkb-keyboard (attach-xkb-keyboard (first arguments)))
    (:finished (when *server* (setf (server-xkb-config *server*) nil)))
    (t nil))
  proxy)

;;; ==================================================================
;;; THE COMMANDS
;;; ==================================================================

(defun input-devices (&optional matcher)
  "Every input device, or the ones MATCHER selects.

The matcher is INPUT-DEVICE-MATCHES-P's: T, a kind keyword, a substring of the
name, or a function."
  (let ((all (and *world* (c:world-inputs *world*))))
    (if (null matcher)
        all
        (remove-if-not (lambda (device) (c:input-device-matches-p device matcher))
                       all))))

(defun describe-input-device (device stream)
  "One device, its kind, and everything configurable about it."
  (format stream "~&~a~30t~(~a~)~@[~a~]~%"
          (or (c:input-device-name device) "(unnamed)")
          (or (c:input-device-kind device) "?")
          (cond ((c:input-device-keyboard device) "  [xkb]")
                ((c:input-device-libinput device) "  [libinput]")
                (t "")))
  (when (eq :keyboard (c:input-device-kind device))
    (format stream "    ~a~24t~@[~a~]~@[  (layout ~d)~]~%" "layout"
            (c:input-device-setting device :layout-name)
            (c:input-device-setting device :layout-index))
    (format stream "    ~a~24t~:[the compositor's default~;~:*~d repeats/s after ~d ms~]~%"
            "repeat"
            (c:input-device-setting device :repeat-rate)
            (c:input-device-setting device :repeat-delay)))
  (when (c:input-device-libinput device)
    (dolist (setting +libinput-settings+)
      (let ((property (input-setting-property setting)))
        (format stream "    ~(~a~)~24t~a~@[   ~a~]~%"
                property
                (let ((value (c:input-device-setting device property)))
                  (cond ((eq property :accel-speed)
                         (let ((speed (bytes-double value)))
                           (if speed (format nil "~,2f" speed) "-")))
                        ((null value) "-")
                        (t (string-downcase (princ-to-string value)))))
                (unless (supported-p device setting) "(unsupported)"))))))

(defcommand list-inputs (&optional name)
  "Print every input device, or the ones whose name contains NAME.

The answer to `what is my touchpad called', which is the first question
*INPUT-RULES* asks and the one nothing else on a river system will tell you.
Everything printed here is settable; the value shown is what the *device* says
is in force, not what the configuration asked for, so the two disagreeing is
visible rather than mysterious."
  (:interactive :string)
  (let ((devices (input-devices name)))
    (if (null devices)
        (notify "no input devices~@[ matching ~a~]~
               ~:[~;  (the compositor offers no input management)~]"
              name (null (and *server* (server-input-manager *server*))))
        (let ((text (with-output-to-string (out)
                      (dolist (device devices) (describe-input-device device out)))))
          (format t "~&~a" text)
          (notify "~d input device~:p; the full listing is on the log stream"
                (length devices))
          text))))

(defcommand reload-input ()
  "Re-apply the input configuration to every device.

What to run after changing an option at a REPL.  Nothing else needs it: a
device that arrives later is configured when it arrives, and the settings are
diffed, so running this when nothing has changed sends nothing."
  (let ((changed (apply-input-configuration)))
    (notify "input: ~d setting~:p applied" (or changed 0))
    changed))

(defcommand set-input (name property value)
  "Set one input PROPERTY to VALUE on every device whose name contains NAME.

For finding out what you want before writing it down:

    (set-input \"Touchpad\" :natural-scroll t)

It is deliberately *not* persistent — it adds no rule and survives no restart —
because a setting you cannot find again is worse than one you have to type
twice.  Put the answer in *INPUT-RULES* once you like it."
  (:interactive :string :name :sexp)
  (let ((devices (input-devices name))
        (property (if (keywordp property)
                      property
                      (intern (string-upcase (string property)) :keyword))))
    (if (null devices)
        (notify "no input device matching ~a" name)
        (let ((changed (loop for device in devices
                             count (send-device-setting device property value))))
          (notify "~(~a~) = ~s on ~d device~:p" property value changed)
          changed))))

(defcommand keyboard-layout (name)
  "Set the keyboard layout for every keyboard: \"us\", \"de\", \"fr\".

Compiles a keymap with xkbcli and hands it to river, so it changes what the
keys do for every application on the machine — and adopts the matching shift
map so that this window manager's own prompt agrees with the keyboard.

Persistent for the session but not across one: put it in *XKB-LAYOUT* to keep
it."
  (:interactive :string)
  (setf p:*xkb-layout* (and (plusp (length (string name))) (string name)))
  (let ((changed (loop for device in (input-devices :keyboard)
                       count (apply-keyboard-layout device))))
    (notify "keyboard layout ~a~[ (no keyboard took it)~:;~]" name changed)
    changed))

(defcommand next-keyboard-layout ()
  "Switch to the next layout of the current keymap, wrapping.

For a keymap compiled from \"us,de\": this is the other half of what
`grp:alt_shift_toggle' does, available as a command so it can be on any key you
like rather than only on the chord xkb reserves."
  (let ((changed 0))
    (dolist (device (input-devices :keyboard) changed)
      (let ((keyboard (c:input-device-keyboard device))
            (index (or (c:input-device-setting device :layout-index) 0)))
        (when keyboard
          (best-effort "set_layout_by_index"
            (w:xkb-keyboard-set-layout-by-index keyboard (1+ index)))
          (incf changed))))
    ;; River wraps an out-of-range index to zero itself, and answers with a
    ;; `layout' event either way -- so the new name arrives rather than being
    ;; predicted here, which is the only version of this that is correct on a
    ;; keymap whose layout count we never asked for.
    (notify "next keyboard layout")
    changed))
