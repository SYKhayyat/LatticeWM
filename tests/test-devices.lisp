;;;; tests/test-devices.lisp --- Input device configuration, without hardware.
;;;;
;;;; WHAT IS TESTABLE HERE AND WHAT IS NOT, stated up front because this
;;;; subsystem is the clearest case in the program of the split the whole
;;;; project keeps rediscovering.
;;;;
;;;; The *decision* — which settings a device should have, given the options,
;;;; the rules and the policy — is pure.  It takes a device and returns a
;;;; plist, it touches no proxy, and it is the half where a mistake is silent:
;;;; a rule that matches the wrong device turns natural scrolling on for the
;;;; mouse and nobody can say why.  All of that is checked below.
;;;;
;;;; The *sending* is not testable here at all, and pretending otherwise would
;;;; be the exact failure src/runtime/server.lisp names — constructing state
;;;; rather than receiving it.  A proxy cannot be constructed.  That half is
;;;; checked in tools/integration.lisp against a real river, and the libinput
;;;; half of it needs real hardware, which is recorded as a skip rather than
;;;; passed over in silence.
;;;;
;;;; The encoding is the interesting middle case: DOUBLE-BYTES puts the raw
;;;; bytes of an IEEE double on the wire, which is exactly the kind of thing
;;;; that is wrong by one byte order and looks fine until a touchpad is
;;;; suddenly the fastest object in the room.  It is pure, so it is checked.

(in-package #:latticewm/tests)
(in-suite devices)

(defun device (&key name (kind :pointer))
  "An INPUT-DEVICE with no proxies, which is all a decision needs."
  (make-instance 'c:input-device :name name :kind kind))

(defclass test-device-policy (p:conventional-policy) ()
  (:documentation "A policy that adds one setting to the shipped answer.

Defined at top level rather than inside the test, because a DEFMETHOD
evaluated inside a test body is a redefinition on the second run and gate 1
counts redefinitions."))

(defmethod p:input-settings ((policy test-device-policy) device)
  (append (when (equal "Wacom" (c:input-device-name device))
            '(:accel-profile :flat))
          (call-next-method)))

;;; ------------------------------------------------------------ matching

(test t-matches-every-device
  (is (c:input-device-matches-p (device :name "anything") t))
  (is (c:input-device-matches-p (device :name nil :kind :keyboard) t)))

(test a-keyword-matches-by-kind
  (let ((touchpad (device :name "ELAN Touchpad" :kind :pointer))
        (keyboard (device :name "AT keyboard" :kind :keyboard)))
    (is (c:input-device-matches-p touchpad :pointer))
    (is (not (c:input-device-matches-p touchpad :keyboard)))
    (is (c:input-device-matches-p keyboard :keyboard))))

(test a-string-matches-a-substring-of-the-name-case-insensitively
  ;; The real names have vendor ids and bus numbers in them, so a rule that
  ;; had to spell one exactly would be a rule nobody types correctly twice.
  (let ((touchpad (device :name "ELAN0678:00 04F3:3195 Touchpad")))
    (is (c:input-device-matches-p touchpad "Touchpad"))
    (is (c:input-device-matches-p touchpad "touchpad"))
    (is (c:input-device-matches-p touchpad "ELAN"))
    (is (not (c:input-device-matches-p touchpad "Trackpoint")))))

(test a-nameless-device-matches-no-string-and-does-not-error
  ;; The name arrives in an event of its own, so there is a window in which a
  ;; device is real and anonymous.  A rule evaluated in that window must
  ;; decline rather than signal, because the work is re-run when the name
  ;; arrives and a signal there would abandon the whole pass.
  (is (not (c:input-device-matches-p (device :name nil) "Touchpad"))))

(test a-function-matcher-is-given-the-device
  (let ((seen '()))
    (is (c:input-device-matches-p
         (device :name "Wacom")
         (lambda (d) (push (c:input-device-name d) seen) t)))
    (is (equal '("Wacom") seen))))

;;; ------------------------------------------------------------- the rules

(test rules-apply-in-order-and-the-later-one-wins
  ;; Which is what makes a rule for T read as a default and a rule naming a
  ;; device read as an override -- the order people write them in.
  (let ((touchpad (device :name "Touchpad")))
    (is (equal '(t)
               (list (getf (p:apply-input-rules touchpad '()
                                                '((t :natural-scroll t)))
                           :natural-scroll))))
    (is (null (getf (p:apply-input-rules
                     touchpad '()
                     '((t :natural-scroll t) ("Touchpad" :natural-scroll nil)))
                    :natural-scroll)))))

(test a-rule-that-does-not-match-changes-nothing
  (let ((mouse (device :name "Logitech USB Receiver")))
    (is (equal '(:accel-speed 0.5d0)
               (p:apply-input-rules mouse '(:accel-speed 0.5d0)
                                    '(("Touchpad" :accel-speed 0.1d0)))))))

(test rules-lay-over-the-options-rather-than-replacing-them
  ;; The property a rule does not name has to survive it, or every rule would
  ;; have to restate the whole configuration to change one thing.
  (let* ((touchpad (device :name "Touchpad"))
         (result (p:apply-input-rules touchpad
                                      '(:tap-to-click t :natural-scroll nil)
                                      '(("Touchpad" :natural-scroll t)))))
    (is (eq t (getf result :tap-to-click)))
    (is (eq t (getf result :natural-scroll)))))

;;; --------------------------------------------------------- the decision

(test the-shipped-policy-answers-with-the-options
  (let ((p:*tap-to-click* t)
        (p:*natural-scroll* nil)
        (p:*accel-speed* 0.25d0)
        (p:*input-rules* '()))
    (let ((settings (p:input-settings (policy) (device :name "Touchpad"))))
      (is (eq t (getf settings :tap-to-click)))
      (is (null (getf settings :natural-scroll)))
      (is (eql 0.25d0 (getf settings :accel-speed))))))

(test an-option-left-at-nil-is-absent-rather-than-sent-as-nil
  ;; NIL means `leave the device alone' everywhere in this subsystem, so an
  ;; unset option must not appear in the plist at all -- otherwise the first
  ;; startup would reset every acceleration profile on the machine to nothing.
  (let ((p:*accel-profile* nil) (p:*scroll-method* nil) (p:*input-rules* '()))
    (let ((settings (p:input-settings (policy) (device :name "Mouse"))))
      (is (not (member :accel-profile settings)))
      (is (not (member :scroll-method settings))))))

(test turning-a-setting-off-is-a-setting-and-not-an-absence
  "The last reader of the plist threw away every pair whose value was false.

Everything above it was right and said so out loud: OPTION-SETTINGS goes out
of its way to send the six meaningful booleans either way and explains why in
a comment, *INPUT-RULES* documents that a rule which does not mention a key
does not set it, and the two tests above assert that the plist carries
:NATURAL-SCROLL NIL when a rule asks for it.  Then APPLY-DEVICE-SETTINGS
filtered on (NULL VALUE), so `(setf *tap-to-click* nil)' sent nothing at all
and the touchpad went on tapping.  Every instrument in the file agreed the
plist was correct, which it was.

This is the decision half, which is testable; that the request then goes out
is the compositor's half and is in tools/integration.lisp, as the header of
this file explains."
  (is (equal '(:natural-scroll nil)
             (r::settings-to-send '(:natural-scroll nil)))
      "a boolean set to NIL is a value to be sent")
  (is (equal '(:tap-to-click t :natural-scroll nil :accel-speed 0.25d0)
             (r::settings-to-send
              '(:tap-to-click t :repeat-rate 50 :natural-scroll nil
                :repeat-delay 300 :accel-speed 0.25d0)))
      "and the only pairs held back are the two APPLY-REPEAT-INFO sends
itself, as one request, because river takes them together")
  (is (null (r::settings-to-send '(:repeat-rate nil :repeat-delay nil)))
      "which is what a keyboard's whole answer is made of"))

(test a-keyboard-is-not-asked-about-natural-scrolling
  ;; Sending a touchpad setting to a keyboard is a round trip whose answer is
  ;; `unsupported' and a log line that says nothing.
  (let ((p:*tap-to-click* t) (p:*repeat-rate* 50) (p:*input-rules* '()))
    (let ((settings (p:input-settings (policy) (device :kind :keyboard))))
      (is (null (getf settings :tap-to-click)))
      (is (eql 50 (getf settings :repeat-rate))))))

(test a-method-can-add-one-setting-without-restating-the-rest
  ;; The shape the generic's docstring promises works.  It is the whole
  ;; argument for the defaults living on a strictly more general class -- the
  ;; finding recorded as core edit 3 in FINDINGS.org, checked here for the
  ;; newest protocol on the surface.
  (let ((p:*tap-to-click* t) (p:*input-rules* '()))
    (let* ((policy (make-instance 'test-device-policy))
           (wacom (p:input-settings policy (device :name "Wacom")))
           (other (p:input-settings policy (device :name "Mouse"))))
      (is (eq :flat (getf wacom :accel-profile)))
      (is (eq t (getf wacom :tap-to-click)) "and the shipped answer survived")
      (is (null (getf other :accel-profile))))))

(test a-property-named-twice-is-sent-once-and-the-first-wins
  ;; APPEND is what the docstring shows, and APPEND leaves duplicates.  GETF
  ;; answers with the first, so that is what a plist means -- but the applier
  ;; walks the pairs, and walking them all would send the shipped answer
  ;; *after* the override and undo it a microsecond later.  The extension would
  ;; appear to have worked, which is the worst way for this to be wrong.
  (let ((settings (r::first-of-each
                   '(:accel-profile :flat :tap-to-click t
                     :accel-profile :adaptive :tap-to-click nil))))
    (is (eq :flat (getf settings :accel-profile)))
    (is (eq t (getf settings :tap-to-click)))
    (is (= 4 (length settings)) "and each property appears exactly once")))

;;; -------------------------------------------------------- the keyboard

(test no-xkb-layout-means-leave-the-compositor-alone
  ;; The default has to be `do nothing'.  A default of "us" would silently
  ;; overwrite the layout of every user whose session already set
  ;; XKB_DEFAULT_LAYOUT -- which is every user who does not type in English,
  ;; and who would experience it as the window manager breaking their keyboard.
  (let ((p:*xkb-layout* nil))
    (is (null (p:keyboard-layout-for (policy) (device :kind :keyboard))))))

(test an-xkb-layout-comes-back-with-its-variant-and-options
  (let ((p:*xkb-layout* "de")
        (p:*xkb-variant* "nodeadkeys")
        (p:*xkb-options* "ctrl:nocaps"))
    (multiple-value-bind (layout variant options)
        (p:keyboard-layout-for (policy) (device :kind :keyboard))
      (is (equal "de" layout))
      (is (equal "nodeadkeys" variant))
      (is (equal "ctrl:nocaps" options)))))

;;; ------------------------------------------------------- the wire values

(test acceleration-survives-the-round-trip-through-eight-bytes
  ;; libinput's `array' arguments carry the raw bytes of an IEEE double, which
  ;; is precisely the kind of thing that is wrong by one byte order and looks
  ;; plausible until a touchpad becomes the fastest object in the room.
  (dolist (speed '(0d0 1d0 -1d0 0.25d0 -0.75d0 0.3333333333333333d0))
    (is (= speed (r::bytes-double (r::double-bytes speed)))
        "~a survives the round trip" speed))
  (is (= 8 (length (r::double-bytes 0.5d0))))
  (is (null (r::bytes-double #(1 2 3))) "and a wrong-length array is refused"))

(test acceleration-is-little-endian
  ;; Written out rather than derived, so that the day this runs on a
  ;; big-endian machine it fails here and not on somebody's desk.
  (is (equalp #(0 0 0 0 0 0 240 63) (r::double-bytes 1d0)))
  (is (equalp #(0 0 0 0 0 0 0 0) (r::double-bytes 0d0))))

(test every-setting-in-the-table-names-a-real-wrapped-request
  ;; The table is the whole of what is configurable, so a typo in a setter name
  ;; is twenty settings that look present and one that silently is not.  It
  ;; would show up as `the function is undefined' inside GUARDED -- a log line
  ;; nobody reads, and a touchpad option that does nothing.
  (dolist (setting r:+libinput-settings+)
    (let ((setter (r::input-setting-setter setting)))
      (is (fboundp setter) "~a is a real function" setter)
      ;; And it goes through LATTICEWM/WIRE rather than round it.  The package
      ;; boundary is the architecture -- "policy and the runtime talk to the
      ;; wrappers, the wrappers talk to the protocol" -- and a table of twenty
      ;; symbols is exactly the shape in which one of them quietly names
      ;; LATTICEWM/RIVER instead, which works, and which the next scanner run
      ;; would leave unwrapped.
      (is (eq (find-package '#:latticewm/wire) (symbol-package setter))
          "~a is reached through the wire layer rather than round it"
          setter))))

(test every-setting-in-the-table-is-a-property-the-shipped-policy-can-produce
  ;; The other direction: a property the policy can return that the table does
  ;; not know is a setting silently dropped.  Both lists are short and both
  ;; are edited by hand, which is exactly when they drift.
  (let ((p:*input-rules* '())
        (p:*click-method* :clickfinger)
        (p:*scroll-method* :two-finger)
        (p:*accel-profile* :adaptive)
        (p:*accel-speed* 0.2d0)
        (p:*drag-lock* :sticky)
        (p:*scroll-button* 274))
    (let ((settings (p:input-settings (policy) (device :name "Touchpad"))))
      (loop for (property nil) on settings by #'cddr
            do (is (or (member property '(:repeat-rate :repeat-delay
                                          :scroll-factor :map-to-output))
                       (r::find-input-setting property))
                   "~(~a~) is a property something knows how to send"
                   property)))))

(test an-unrecognised-value-is-refused-rather-than-guessed
  ;; A configuration file is user input.  `(setf *click-method* :clickfingers)'
  ;; should leave the touchpad alone and say so, not take down the startup that
  ;; was reading it and not pick the nearest thing.
  (is (null (r::one-of :clickfingers r::+click-methods+)))
  (is (eq :clickfinger (r::one-of :clickfinger r::+click-methods+)))
  (is (eq :no-scroll (r::one-of :none r::+scroll-methods+))
      "and a synonym somebody would plausibly write is accepted"))

;;; ------------------------------------------------------------ capture keys

(defclass modal-policy (p:conventional-policy) ()
  (:documentation
   "A policy with a modal editing layer, which is the single most obvious thing
an Emacs-shaped window manager's users ask for and was the exact thing the old
+CAPTURE-KEYS+ made impossible."))

(defmethod p:capture-keys ((policy modal-policy))
  (append (call-next-method)
          ;; The twelve XF86 media keysyms, which is the modern shape of the
          ;; example: F1 through F12 used to be it, and the core ships those
          ;; now so that `any key closes this' is true of an overlay.  A key
          ;; the core never heard of has to be a key the core never heard of.
          (loop for keysym from #x1008ff11 to #x1008ff1c
                collect (cons keysym '()))))

(test capture-keys-is-the-whole-of-what-a-prompt-can-read
  "River delivers keys to the focused *window* and hands the window manager only
what it asked for, so this list is not a convenience: a key that is not on it is
not merely unbound, it is unreadable, and no keymap entry can rescue it.

It was a DEFPARAMETER in src/runtime/, which is why these assertions are worth
making at all -- the shape being checked is `the answer comes from a policy'."
  (let ((keys (p:capture-keys (policy))))
    (is (< 200 (length keys))
        "printable ASCII bare and shifted alone is 190 entries; got ~d"
        (length keys))
    ;; The bug you find by trying to type a bracket: river matches on keysym
    ;; *and* modifiers, and sends the unshifted keysym with Shift still set.
    (is (member (cons (char-code #\9) '()) keys :test #'equal))
    (is (member (cons (char-code #\9) '(:shift)) keys :test #'equal)
        "without the shifted copy, M-: cannot read an open bracket")
    ;; Escape, Return, Backspace and the arrows, which are how a prompt is left.
    (dolist (keysym '(#xff08 #xff0d #xff1b #xff51 #xff53))
      (is (member (cons keysym '()) keys :test #'equal)
          "keysym #x~x is not readable" keysym))
    ;; The readline chords, and the option that decides which.
    (is (member (cons (char-code #\a) '(:ctrl)) keys :test #'equal))
    (let ((p:*readline-chords* "ae"))
      (let ((narrowed (p:capture-keys (policy))))
        (is (member (cons (char-code #\e) '(:ctrl)) narrowed :test #'equal))
        (is (not (member (cons (char-code #\w) '(:ctrl)) narrowed :test #'equal))
            "*READLINE-CHORDS* decides which chords a prompt reads")))
    ;; Nothing is listed twice: every entry becomes a river_xkb_binding_v1, and
    ;; a duplicate is a second binding on the same key racing the first.
    (is (= (length keys) (length (remove-duplicates keys :test #'equal)))
        "the capture list has duplicates in it")))

(test a-modal-layer-can-add-a-key-the-core-never-heard-of
  "The failure this replaces: bind a key in a keymap, press it, and river never
delivers it -- with nothing anywhere to say why, because the keymap is right,
the command exists, and the key was simply never requested."
  (let ((shipped (p:capture-keys (policy)))
        (modal (p:capture-keys (make-instance 'modal-policy))))
    (is (not (member (cons #x1008ff14 '()) shipped :test #'equal))
        "XF86AudioPlay is not readable by default, which is the premise")
    (is (member (cons #x1008ff14 '()) modal :test #'equal)
        "and one method makes it readable")
    (is (= (+ 12 (length shipped)) (length modal))
        "CALL-NEXT-METHOD kept everything the shipped answer had")))

(test an-overlay-can-be-dismissed-by-the-keys-somebody-actually-presses
  "`Any key closes this' is printed across the top of every overlay, and the
five keys a new user reaches for did nothing at all.

Half of that was structural and is asserted in test-surface; this is the other
half, which is that the keys have to be *askable for* in the first place.  A
key absent from CAPTURE-KEYS is not unbound, it is undeliverable: river hands
the window manager only what it requested and gives the rest to the focused
window, so no handler anywhere can rescue it."
  (let ((keys (p:capture-keys (policy))))
    (dolist (entry '((#xff1b . "Escape")   (#x20   . "space")
                     (#xff0d . "Return")   (#x71   . "q")
                     (#x78   . "x")        (#xff09 . "Tab")
                     (#xffbe . "F1")       (#xff56 . "Page_Down")
                     (#xff55 . "Page_Up")  (#xff63 . "Insert")))
      (is (member (cons (car entry) '()) keys :test #'equal)
          "~a cannot reach the window manager, so it cannot close an overlay"
          (cdr entry)))))
