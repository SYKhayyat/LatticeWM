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

;;; ---------------------------------------------------------- shift maps
;;;
;;; RIVER SENDS THE UNSHIFTED KEYSYM, and everything below follows from that
;;; one fact.  The design assumed the opposite — that xkb would produce
;;; `parenleft' for Shift+9 and river would pass it through with Shift still in
;;; the modifier set — and bound every printable keysym twice on that
;;; reasoning.  Measured against a real keyboard on bare metal, what arrives is
;;; keysym `9' with Shift set, so the prompt inserted a nine: typing (+ 1 2)
;;; into M-: produced `9= 1 20', which is a symbol called 9= and an
;;; unbound-variable error.
;;;
;;; So the shifted glyph cannot be derived.  It has to be *declared*, per
;;; layout — and a program that ships one hard-coded US table and no way to
;;; replace it is a program that cannot be typed into on a German keyboard.
;;; What ships instead is a registry, four layouts, and a generic.

(defvar *shift-maps* (make-hash-table :test #'equal)
  "Layout name -> an alist of unshifted character to shifted character.

Letters are never in a table: CHAR-UPCASE is right for every alphabet SBCL
knows and is the fallback.  What a table holds is the punctuation and digit
row, which is the part that differs between layouts and the part that cannot be
computed.")

(defun register-shift-map (name pairs)
  "Register PAIRS as the shift map called NAME, replacing any of that name.

    (register-shift-map \"my-layout\" '((#\\1 . #\\!) (#\\2 . #\\\") ...))
    (setf *keyboard-layout* \"my-layout\")

Three lines, and they are the whole of teaching this window manager a keyboard
it has never seen."
  (setf (gethash (string-downcase (string name)) *shift-maps*) pairs))

(defun find-shift-map (name)
  "The registered shift map called NAME, or NIL.  A list passes through."
  (etypecase name
    (null nil)
    (cons name)
    (string (gethash (string-downcase name) *shift-maps*))
    (symbol (gethash (string-downcase (symbol-name name)) *shift-maps*))))

(defun shift-map-names ()
  "Every registered layout name, sorted."
  (sort (loop for name being the hash-keys of *shift-maps* collect name)
        #'string<))

(register-shift-map "us"
  '((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
    (#\- . #\_) (#\= . #\+) (#\[ . #\{) (#\] . #\}) (#\\ . #\|)
    (#\; . #\:) (#\' . #\") (#\, . #\<) (#\. . #\>) (#\/ . #\?)
    (#\` . #\~)))

;; Dvorak and Colemak move the letters and leave most of the punctuation where
;; QWERTY has it, so they differ from "us" in a handful of keys rather than
;; wholesale.  Written out in full anyway: a table you can read beside the
;; keyboard in front of you is worth more than a diff against another table.
(register-shift-map "dvorak"
  '((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
    (#\[ . #\{) (#\] . #\}) (#\/ . #\?) (#\= . #\+) (#\\ . #\|)
    (#\' . #\") (#\, . #\<) (#\. . #\>) (#\; . #\:) (#\- . #\_)
    (#\` . #\~)))

(register-shift-map "colemak"
  '((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
    (#\- . #\_) (#\= . #\+) (#\[ . #\{) (#\] . #\}) (#\\ . #\|)
    (#\; . #\:) (#\' . #\") (#\, . #\<) (#\. . #\>) (#\/ . #\?)
    (#\` . #\~)))

(register-shift-map "uk"
  '((#\1 . #\!) (#\2 . #\") (#\3 . #\£) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
    (#\- . #\_) (#\= . #\+) (#\[ . #\{) (#\] . #\}) (#\# . #\~)
    (#\; . #\:) (#\' . #\@) (#\, . #\<) (#\. . #\>) (#\/ . #\?)
    (#\` . #\¬) (#\\ . #\|)))

(register-shift-map "de"
  '((#\1 . #\!) (#\2 . #\") (#\3 . #\§) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\&) (#\7 . #\/) (#\8 . #\() (#\9 . #\)) (#\0 . #\=)
    (#\ß . #\?) (#\+ . #\*) (#\# . #\') (#\, . #\;) (#\. . #\:)
    (#\- . #\_) (#\< . #\>) (#\´ . #\`)))

(register-shift-map "fr"
  '((#\& . #\1) (#\é . #\2) (#\" . #\3) (#\' . #\4) (#\( . #\5)
    (#\- . #\6) (#\è . #\7) (#\_ . #\8) (#\ç . #\9) (#\à . #\0)
    (#\) . #\°) (#\= . #\+) (#\^ . #\¨) (#\$ . #\£) (#\, . #\?)
    (#\; . #\.) (#\: . #\/) (#\! . #\§) (#\* . #\µ)))

(define-option *keyboard-layout* "us"
  "Which shift map to use, by name.  See (SHIFT-MAP-NAMES).

Shipped: \"us\", \"uk\", \"de\", \"fr\", \"dvorak\" and \"colemak\".  A list of
(UNSHIFTED . SHIFTED) pairs may be given directly, and REGISTER-SHIFT-MAP adds
a named one.

This is the one setting somebody outside the United States has to change before
the minibuffer can be typed into, so it is a tier-0 value with a table behind
it rather than a constant behind a function.")

(define-option *shift-map* nil
  "An explicit shift map, overriding *KEYBOARD-LAYOUT*, or NIL to use it.

Kept because it was the documented way to do this and configurations in the
wild will have set it.  Prefer *KEYBOARD-LAYOUT*, which names a whole layout
rather than requiring you to write one out:

    (setf *keyboard-layout* \"de\")

To adjust a shipped layout rather than replace it, append to it:

    (setf *shift-map* (append '((#\\8 . #\\() (#\\9 . #\\)))
                              (find-shift-map \"us\")))")

(defun current-shift-map ()
  "The shift map in force: *SHIFT-MAP* if set, else *KEYBOARD-LAYOUT*'s."
  (or *shift-map*
      (find-shift-map *keyboard-layout*)
      (find-shift-map "us")))

(defmethod shifted-character ((policy input-policy) character)
  "The current layout's table, falling back to CHAR-UPCASE."
  (or (cdr (assoc character (current-shift-map)))
      (char-upcase character)))

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

;;; ------------------------------------------------------- pointer buttons
;;;
;;; The same idea as the keysym table, for the other input device.  A pointer
;;; binding takes a *Linux input event code* — BTN_LEFT is 0x110, not 1 — and
;;; that is exactly the sort of detail that produces a binding which silently
;;; never fires.

(defparameter +pointer-buttons+
  '((:btn-left . 272) (:btn-right . 273) (:btn-middle . 274)
    (:btn-side . 275) (:btn-extra . 276) (:btn-forward . 277)
    (:btn-back . 278) (:btn-task . 279))
  "Mouse buttons by name, from linux/input-event-codes.h.")

(defun pointer-button-code (button)
  "BUTTON as a Linux input event code.  A keyword is looked up; a number passes."
  (cond ((integerp button) button)
        ((cdr (assoc button +pointer-buttons+)))
        (t (error "~s is not a pointer button.  Known: ~{~(~a~)~^ ~}"
                  button (mapcar #'car +pointer-buttons+)))))

(define-option *pointer-bindings*
  '((:move   :btn-left  (:super))
    (:resize :btn-right (:super)))
  "Which button, with which modifiers, starts which interactive operation.

A list of (OPERATION BUTTON MODIFIERS).  OPERATION is :MOVE or :RESIZE — or
anything an extension has taught POINTER-DRAG-RECT about.  BUTTON is a keyword
from +POINTER-BUTTONS+ or a raw event code, and MODIFIERS is written the way a
key binding's is.

Super+drag to move and Super+right-drag to resize is the convention every
floating window manager has used for thirty years, and it is chosen here for
the reason it was chosen there: it works anywhere in the window, so you never
have to hit a title bar or a five-pixel corner.

Note what is deliberately absent: a *bare* left-drag binding.  A window manager
that grabs unmodified left-click cannot be used with any application, and the
mistake is easy to make because it looks like it works right up until you try
to select text.")

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

(define-option *warn-on-rebinding* t
  "Say so when DEFINE-KEY replaces a binding with a *different* command.

Rebinding is the point of a keymap and is not worth mentioning.  Silently
taking a key that already meant something else is different, and it is how an
extension removes a feature nobody knows it removed.

The lattice did exactly that: enabling it rebound Super+/ from `help\' to
`lattice-status\', so loading an optional extension quietly deleted the one
key that finds every other key -- while the status line went on advising
people to press it.  Nothing said anything, because nothing was watching.

Re-binding a key to the same command is silent, so reloading a config file
says nothing.")

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
  (let* ((key (kbd spec))
         (existing (gethash key (keymap-entries keymap))))
    (when (and *warn-on-rebinding* target existing
               (not (equal existing target)))
      (logmsg :warn "~a was ~s and is now ~s"
              (key-to-string key) existing target))
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

(defun bindable-keys (&optional (keymap *keymap*))
  "Every key river should be told about: the first key of every chord, and
nothing inside one.

THIS IS NOT ALL-BOUND-KEYS, AND THE DIFFERENCE WAS A REAL BUG.  Registration
used to walk ALL-BOUND-KEYS, which descends into submaps -- so the help
submap's second keys (b, c, f, k, o, a, s) were registered with river as
*bare, unmodified, global* bindings.

River then ate every one of those keypresses, because a registered binding is
by definition not delivered to the focused window.  And LATTICE-KEY did
nothing with them, because LOOKUP-KEY searched the global keymap, where `s' is
not bound -- it is bound one level down, inside a submap that was not pending.

The result: seven letters silently vanished.  Not misbehaved -- vanished.
Typing `ls' in a terminal produced `l'.  It survived every nested session
because every nested session was driven through the SWANK bridge, and nobody
had ever sat down and typed into a window.

A submap's keys are reached through the capture bindings instead, the same way
a prompt and an empty pane reach theirs -- see CAPTURE-WANTED-P, which asks the
one question all three share."
  (let ((out '()))
    (labels ((walk (map)
               (maphash (lambda (key target) (push (cons key target) out))
                        (keymap-entries map))
               (when (keymap-parent map) (walk (keymap-parent map)))))
      (walk keymap))
    (nreverse out)))

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

