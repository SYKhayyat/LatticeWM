;;;; policy/keys.lisp --- Keysyms, key specs, and keymaps.
;;;;
;;;; RIVER DOES THE XKB WORK AND WE DO NOT.  This is worth stating clearly
;;;; because it deletes an entire subsystem that every other window manager
;;;; has to have.  river_seat_v1 has no keyboard binding mechanism at all; the
;;;; separate river-xkb-bindings-v1 protocol takes a keysym and a modifier
;;;; bitfield and sends us `pressed', `released' and `stop_repeat'.
;;;;
;;;; Consequence: no libxkbcommon, no keymap file descriptor, no xkb state
;;;; machine, no wl_keyboard plumbing.  What remains is a table mapping the
;;;; names people type to X11 keysym numbers, which is data rather than code.
;;;;
;;;; KEYMAPS ARE A TREE, because chorded bindings are a first-class protocol
;;;; feature.  ensure_next_key_eaten exists precisely so that a submap can know
;;;; when to give up: from the spec's own rationale, "the window manager may
;;;; wish to implement 'chorded' keybindings where triggering a binding
;;;; activates a 'submap' with a different set of keybindings.  Without a way
;;;; to eat the next key press event, there is no good way for the window
;;;; manager to know that it should error out and exit the submap."

(in-package #:latticewm/policy)

;;; ----------------------------------------------------- what people type
;;;
;;; Lifted out of src/wire/, where it sat because river's binding requests
;;; take the canonical names.  It does not belong there: nothing in wire ever
;;; used it, and deciding that `super', `mod4', `logo', `win' and `s' all mean
;;; the same key is not a protocol fact.  It is a judgement about what people
;;; will type, which is the definition of a thing a user should be able to
;;; disagree with.

(define-option *modifier-aliases*
  '((:shift . :shift)
    (:ctrl . :ctrl) (:control . :ctrl) (:c . :ctrl)
    (:alt . :mod1) (:mod1 . :mod1) (:meta . :mod1) (:m . :mod1)
    (:mod3 . :mod3)
    (:super . :mod4) (:mod4 . :mod4) (:logo . :mod4) (:win . :mod4)
    (:s . :mod4)
    (:hyper . :mod5) (:mod5 . :mod5))
  "What people type, mapped to what the protocol calls it.

Both `super' and `mod4' work, and so do `C-' and `ctrl', because muscle memory
differs and refusing one of them is a pointless fight to pick.

Add your own spellings rather than learning ours.  This is a tier-0 value, so
a config file can do it without a method:

    (push '(:h . :mod5) *modifier-aliases*)")

(defparameter +modifier-order+ '(:shift :ctrl :mod1 :mod3 :mod4 :mod5)
  "Canonical order, so that (:super :shift) and (:shift :super) are the same
key as far as EQUAL is concerned — which matters, because keys are hash keys.")

(defun modifier-mask (modifiers)
  "The canonical keyword list for MODIFIERS.

MODIFIERS is a list of the names people type — (:super :shift), (:ctrl) — and
the result is what river's generated bindings want, in a fixed order so that
two spellings of the same chord are EQUAL."
  (let ((canonical '()))
    (dolist (modifier modifiers)
      (let ((mapped (cdr (assoc modifier *modifier-aliases*))))
        (unless mapped
          (error "Unknown modifier ~s.  Known: ~{~(~a~)~^ ~}"
                 modifier (remove-duplicates (mapcar #'car *modifier-aliases*))))
        (pushnew mapped canonical)))
    (remove-if-not (lambda (m) (member m canonical)) +modifier-order+)))

(defun modifier-names (modifiers)
  "MODIFIERS rendered the way a person would write them."
  (mapcar (lambda (m)
            (case m (:mod4 :super) (:mod1 :alt) (t m)))
          (remove-if-not (lambda (m) (member m modifiers)) +modifier-order+)))

;;; --------------------------------------------------------------- keysyms

(defparameter +named-keysyms+
  '(;; The ones with names.  Printable ASCII needs no table: an X11 keysym for
    ;; a printable character *is* its character code, which is the single most
    ;; useful fact about keysyms and is nowhere obvious in the documentation.
    ("return" . #xff0d) ("enter" . #xff0d) ("ret" . #xff0d)
    ("escape" . #xff1b) ("esc" . #xff1b)
    ("tab" . #xff09) ("backspace" . #xff08) ("bs" . #xff08)
    ("delete" . #xffff) ("del" . #xffff) ("insert" . #xff63)
    ("space" . #x0020) ("spc" . #x0020)
    ("left" . #xff51) ("up" . #xff52) ("right" . #xff53) ("down" . #xff54)
    ("home" . #xff50) ("end" . #xff57)
    ("pageup" . #xff55) ("prior" . #xff55)
    ("pagedown" . #xff56) ("next" . #xff56)
    ("menu" . #xff67) ("print" . #xff61) ("pause" . #xff13)
    ("kp_enter" . #xff8d) ("kp_add" . #xffab) ("kp_subtract" . #xffad)
    ("kp_multiply" . #xffaa) ("kp_divide" . #xffaf)
    ;; Media keys, which are what people actually want volume bound to.
    ("xf86audioraisevolume" . #x1008ff13)
    ("xf86audiolowervolume" . #x1008ff11)
    ("xf86audiomute" . #x1008ff12)
    ("xf86audioplay" . #x1008ff14)
    ("xf86audionext" . #x1008ff17)
    ("xf86audioprev" . #x1008ff16)
    ("xf86monbrightnessup" . #x1008ff02)
    ("xf86monbrightnessdown" . #x1008ff03)
    ;; Punctuation under its X11 name.  A key spec must be writable without
    ;; the character itself, because "Super+-" and "Super++" cannot be parsed
    ;; — the separator and the key would be the same character.
    ("space" . #x0020) ("exclam" . #x0021) ("quotedbl" . #x0022)
    ("numbersign" . #x0023) ("dollar" . #x0024) ("percent" . #x0025)
    ("ampersand" . #x0026) ("apostrophe" . #x0027) ("quoteright" . #x0027)
    ("parenleft" . #x0028) ("parenright" . #x0029) ("asterisk" . #x002a)
    ("plus" . #x002b) ("comma" . #x002c) ("minus" . #x002d)
    ("period" . #x002e) ("slash" . #x002f) ("colon" . #x003a)
    ("semicolon" . #x003b) ("less" . #x003c) ("equal" . #x003d)
    ("greater" . #x003e) ("question" . #x003f) ("at" . #x0040)
    ("bracketleft" . #x005b) ("backslash" . #x005c) ("bracketright" . #x005d)
    ("asciicircum" . #x005e) ("underscore" . #x005f)
    ("grave" . #x0060) ("quoteleft" . #x0060)
    ("braceleft" . #x007b) ("bar" . #x007c) ("braceright" . #x007d)
    ("asciitilde" . #x007e))
  "Name to X11 keysym, for keys whose name is not simply their character.")

(defun keysym-named (name)
  "The X11 keysym for NAME, or NIL.

Recognises the table above, single printable characters, and Fn keys."
  (let ((lower (string-downcase name)))
    (or (cdr (assoc lower +named-keysyms+ :test #'string=))
        (when (= 1 (length lower))
          (let ((code (char-code (char name 0))))
            (when (<= #x20 code #x7e) code)))
        (when (and (> (length lower) 1) (char= (char lower 0) #\f)
                   (every #'digit-char-p (subseq lower 1)))
          (let ((n (parse-integer lower :start 1)))
            (cond ((<= 1 n 12) (+ #xffbe (1- n)))
                  ((<= 13 n 24) (+ #xffca (- n 13)))))))))

(defun keysym-name (keysym)
  "A printable name for KEYSYM, for status output and key echo."
  (or (car (rassoc keysym +named-keysyms+))
      (when (<= #x21 keysym #x7e) (string (code-char keysym)))
      (when (<= #xffbe keysym #xffc9) (format nil "F~d" (1+ (- keysym #xffbe))))
      (format nil "0x~x" keysym)))

;;; ------------------------------------------------------------- key specs

(defun parse-key (spec)
  "Parse SPEC into (values KEYSYM MODIFIERS), where MODIFIERS is a keyword list.

SPEC is a string of modifiers and a key joined by `+' or `-':

    \"Super+Return\"   \"C-x\"   \"Super+Shift+Left\"   \"XF86AudioMute\"

Both separators work because muscle memory differs: people who came from Emacs
type C-x and people who came from i3 type Super+x, and refusing one of them is
a pointless fight.  Modifier names are case-insensitive and the usual aliases
all work — super/mod4/logo, ctrl/control/C, alt/mod1/meta/M."
  (let* ((parts (let ((out '()) (start 0))
                  (loop for i from 0 below (length spec)
                        when (and (member (char spec i) '(#\+ #\-))
                                  (< (1+ i) (length spec))
                                  (> i start))
                          do (push (subseq spec start i) out)
                             (setf start (1+ i)))
                  (push (subseq spec start) out)
                  (nreverse out)))
         (key (car (last parts)))
         (modifiers (butlast parts))
         (keywords '()))
    (dolist (modifier modifiers)
      (push (intern (string-upcase modifier) :keyword) keywords))
    (let ((mask (handler-case (modifier-mask keywords)
                  (error (condition)
                    (error "~a~%  in key spec ~s" condition spec))))
          (keysym (keysym-named key)))
      (unless keysym
        (error "~s is not a key name, in key spec ~s.~%~
                Use the X11 name for punctuation: bracketright, comma, minus."
               key spec))
      (values keysym mask))))

(defun kbd (spec)
  "The internal key representation for SPEC: a cons of keysym and modifiers."
  (multiple-value-bind (keysym mask) (parse-key spec)
    (cons keysym mask)))

(defun key-to-string (key)
  "Render a (KEYSYM . MODIFIERS) cons the way a person would type it."
  (format nil "~{~:(~a~)+~}~a"
          (modifier-names (cdr key)) (keysym-name (car key))))

;;; ---------------------------------------------------------------- keymaps

(defclass keymap ()
  ((entries :initform (make-hash-table :test #'equal) :accessor keymap-entries
            :documentation "(KEYSYM . MODIFIERS) -> COMMAND-FORM or KEYMAP.")
   (name :initarg :name :initform nil :accessor keymap-name)
   (parent :initarg :parent :initform nil :accessor keymap-parent
           :documentation "Consulted when this keymap has no entry.")
   (props :initform '() :accessor c:props))
  (:documentation
   "A table from keys to commands, or to further keymaps.

Binding a key to a keymap is a chord: press the first key and the second key
is looked up in the submap.  River's ensure_next_key_eaten makes that work
even for keys the submap does not bind, which is what lets a submap exit
cleanly on an unrecognised key instead of leaking it into an application."))

(defun make-keymap (&key name parent)
  "A fresh keymap."
  (make-instance 'keymap :name name :parent parent))

(defvar *keymap* (make-keymap :name "global")
  "The root keymap.

Rebind anything from a configuration file:

    (define-key *keymap* \"Super+q\" '(\"close\"))")

(defvar *pending-keymap* nil
  "The submap we are inside, or NIL.  Set by a chord's first key.")

(defvar *help-visible* nil
  "What the help overlay is showing: NIL for nothing, T for the keymap, or a
cons of a title and a list of (LEFT . RIGHT) rows for anything else.

One variable with three states rather than three variables, because the key
handler's rule is `any key puts it away' and that rule has to be able to put
away whatever is up without knowing what it is.

Declared here rather than in help.lisp because the key handler consults it and
loads first, and a special variable whose meaning depends on load order is a
bug waiting for a rainy day.")

(defun define-key (keymap spec target)
  "Bind SPEC in KEYMAP to TARGET.

TARGET is one of

  a list         a command name and its arguments: (\"focus\" :left)
  a function     called with no arguments
  a KEYMAP       SPEC becomes a chord prefix
  NIL            unbind

A command is named as a *string* rather than passed as a function so that
redefining the command later takes effect without rebinding the key, which is
the whole point of having a registry."
  (let ((key (kbd spec)))
    (if (null target)
        (remhash key (keymap-entries keymap))
        (setf (gethash key (keymap-entries keymap)) target))
    key))

(defun lookup-key (keymap key)
  "The binding for KEY in KEYMAP or its parents, or NIL."
  (loop for map = keymap then (keymap-parent map)
        while map
        for found = (gethash key (keymap-entries map))
        when found return found))

(defun keymap-keys (keymap)
  "Every key bound in KEYMAP itself, sorted for display."
  (let ((out '()))
    (maphash (lambda (key target) (push (cons key target) out))
             (keymap-entries keymap))
    (sort out #'string< :key (lambda (row) (key-to-string (car row))))))

(defun all-bound-keys (&optional (keymap *keymap*))
  "Every key bound anywhere under KEYMAP, as (KEY . TARGET) with chord prefixes
flattened.  This is what the binding registration walks, because river needs to
be told about the *first* key of every chord and nothing else."
  (let ((out '()))
    (labels ((walk (map)
               (maphash (lambda (key target)
                          (push (cons key target) out)
                          (when (typep target 'keymap) (walk target)))
                        (keymap-entries map))
               (when (keymap-parent map) (walk (keymap-parent map)))))
      (walk keymap))
    (nreverse out)))

