;;;; shortcuts.lisp --- Handing the keyboard back, and saying what we take (#11).
;;;;
;;;; #11 is `shortcuts that work even when an application has grabbed the
;;;; keyboard', and half of the answer is that they always did.  River's binding
;;;; model is the opposite of grab-sensitive: a key registered with
;;;; river_xkb_binding_v1 is delivered to the window manager and *not* to the
;;;; focused client, so every key in the keymap is global by construction and
;;;; there is no protocol to opt into.  The `wlroots-global-shortcuts' the issue
;;;; names does not exist in river, and `per-application shortcuts' would be a
;;;; second list of names for what the keymap already decides.
;;;;
;;;; The half that was genuinely missing is the other direction.  A game that
;;;; wants Super for itself, a terminal multiplexer whose prefix collides with a
;;;; chord, a remote desktop where every key belongs to the remote -- each wants
;;;; the window manager to *stop taking keys*, and none of them can ask for it.
;;;; These three commands are the ask, the answer, and the report.

(in-package #:latticewm/runtime)

(defun shortcut-first-line (command)
  "The first line of COMMAND's documentation, for a one-row-per-key listing.

A full docstring in a table row is not a listing, it is a wall -- and every
command in this program has a first sentence that says what it is, which is
what a shortcut table is for."
  (let ((text (and command (p:command-documentation command))))
    (cond ((null text) "")
          (t (let ((end (position #\Newline text)))
               (subseq text 0 end))))))

(defun shortcut-summary (target)
  "What TARGET is, in a few words, for the report."
  (cond ((typep target 'p:keymap) "a chord prefix (more keys follow)")
        ((stringp target) (shortcut-first-line (p:find-command target)))
        ((and (consp target) (stringp (car target)))
         (format nil "~a ~{~a~^ ~}"
                 (shortcut-first-line (p:find-command (car target)))
                 (cdr target)))
        ((functionp target) "a function")
        (t (format nil "~s" target))))

(defun shortcut-rows ()
  "One row per key the window manager takes, sorted as the keymap draws them."
  (loop for (key . target) in (p:bindable-keys)
        collect (cons (p:key-to-string key) (shortcut-summary target))))

(defcommand global-shortcuts ()
  "List every key the window manager takes, and what it does.

These are global in the sense #11 asks about: river delivers a registered key to
the window manager and not to the focused window, so a bound key does not reach
the application even when the application has the keyboard.  What is *not* here
is the app's own shortcuts, which the application handles itself."
  (let ((suspended (p:keys-suspended-p)))
    (show-help-page
     (if suspended "global shortcuts (suspended)" "global shortcuts")
     (let ((rows (shortcut-rows)))
       (append
        rows
        (when suspended
          (list (cons "" "everything else is the application's until you resume")))))))
  t)

(defcommand suspend-keys ()
  "Give the keyboard to the focused application, keeping one chord back.

For a game that wants Super, a multiplexer whose prefix is a chord, a remote
desktop session.  Every binding is disabled at the compositor -- not merely
ignored, because a registered key never reaches the application at all -- except
the way back, which is named in the message."
  (if (p:keys-suspended-p)
      (notify "the keys are already suspended; ~a resumes them"
              (p:key-to-string (p:way-back-key)))
      (progn
        (p:give-keys-away)
        (rebind-keys)
        (notify "keys handed to the application; ~a resumes them"
                (p:key-to-string (p:way-back-key))))))

(defcommand resume-keys ()
  "Take the keyboard back."
  (p:take-keys-back)
  (rebind-keys)
  (notify "keys back to the window manager"))

(defcommand keys-suspended ()
  "Say whether the window manager is taking keys, and how to change that."
  (if (p:keys-suspended-p)
      (notify "suspended; ~a resumes them"
              (p:key-to-string (p:way-back-key)))
      (notify "taking keys; M-x suspend-keys hands them to the application")))
