;;;; runtime/keys.lisp --- Keybindings.
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

(in-package #:latticewm/runtime)

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
    (let ((mask (handler-case (w:modifier-mask keywords)
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
          (w:modifier-names (cdr key)) (keysym-name (car key))))

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

;;; --------------------------------------------------------- dispatching

(defun run-key-target (target)
  "Invoke whatever a key was bound to.

A key bound to a command that needs arguments and given none *asks* for them
rather than failing.  That is what makes ~(\"describe-key\")~ a sensible thing
to put in a keymap: the binding names the verb, and the verb collects its own
object.  Without it, half the command set could only be bound by also deciding
its arguments in the configuration file, which is a strange thing to have to
do for `go to workspace' and an impossible one for `describe this key'."
  (etypecase target
    (null nil)
    (keymap (setf *pending-keymap* target)
            ;; The arming happens in the manage sequence — see
            ;; ARM-EMPTY-PANE-CAPTURE — because ensure_next_key_eaten is
            ;; window-management state and a key binding may fire outside one.
            ;; Redrawing is not optional here: the echo area is where the
            ;; submap says what it offers, and a which-key that appears one
            ;; keystroke late is worse than none.
            (mark-dirty)
            t)
    (function (guarded "key binding" (funcall target)) t)
    (cons (let ((command (find-command (first target))))
            (if (and command
                     (null (rest target))
                     (some (lambda (argument) (eq :required (third argument)))
                           (command-arguments command)))
                (call-interactively command)
                (apply #'run-command (first target) (rest target))))
          t)
    (string (run-key-target (list target)))))

(defun handle-key (key)
  "A bound key fired.  Returns T when it was handled.

Order matters and is deliberate: a submap in progress wins over everything,
because a half-entered chord that silently ran a global binding is the single
most confusing thing a keymap can do."
  (let ((policy (p:current-policy)))
    (cond
      ;; The help overlay is dismissed by anything, including the key that
      ;; opened it.  A help screen you have to remember how to close is a help
      ;; screen that has failed at its one job.
      ((and *help-visible* (not (reading-p))
            (progn (setf *help-visible* nil) (mark-dirty) t))
       t)
      ;; Inside a chord.
      (*pending-keymap*
       (let ((target (lookup-key *pending-keymap* key))
             (name (or (keymap-name *pending-keymap*) "this submap")))
         (setf *pending-keymap* nil)
         (mark-dirty)
         (cond (target (run-key-target target))
               ;; Said out loud rather than logged.  A chord that silently
               ;; evaporates is indistinguishable from a keyboard that missed
               ;; the keypress, and the two want opposite responses.
               (t (notify "~a is not bound in ~a" (key-to-string key) name)
                  t))))
      ;; A policy that wants to intercept everything — a resize mode, say.
      ((guarded "on-key" (p:on-key policy *world* key)) t)
      (t
       (let ((target (lookup-key *keymap* key)))
         (cond (target (run-key-target target))
               (t nil)))))))

(defun handle-unbound-key (keysym)
  "An unbound key arrived because we had asked to see it.

Two callers: a submap that is waiting for its second key, and README D19's
typing-in-an-empty-pane, which is what gives the empty pane something to be."
  (cond
    (*pending-keymap*
     (setf *pending-keymap* nil)
     (mark-dirty)
     (logmsg :debug "~a exits the submap" (keysym-name keysym))
     t)
    (t
     (let* ((policy (p:current-policy))
            (character (when (<= #x20 keysym #x7e) (code-char keysym)))
            (command (guarded "key-unbound"
                       (p:key-unbound policy *world* (or character keysym)))))
       (when command (run-command command) t)))))
