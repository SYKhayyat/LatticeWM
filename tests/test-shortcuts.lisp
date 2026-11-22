;;;; tests/test-shortcuts.lisp --- The keyboard, both directions (#11).
;;;;
;;;; #11 asks for shortcuts that work while an application has the keyboard.
;;;; They always did -- a key registered with river goes to the window manager
;;;; and not to the focused client, which is what `global' means there -- so
;;;; what is tested here is the claim that makes that true (only the keys the
;;;; policy names are registered) and the half that was missing: handing the
;;;; keyboard back, with one way back registered.

(in-package #:latticewm/tests)
(in-suite shortcuts)

(defmacro with-the-keys-restored (&body body)
  "Run BODY and put the keyboard state back afterwards.
The suspension flag is process state, so a suite that leaves it set makes every
later suite's key test a lie."
  `(unwind-protect
        (progn (p:take-keys-back) ,@body)
     (p:take-keys-back)))

(defmacro with-a-keymap (&body body)
  "Run BODY over a keymap that exists.

The shipped keymap is installed by START, not by the tests, so a test that
merely reads P:*KEYMAP* would be asserting about an empty table.  This installs
the shipped map -- dozens of chords -- and names the way back."
  `(let ((p:*keymap* (p:make-keymap)))
     (p:install-default-keymap p:*keymap*)
     (p:define-key p:*keymap* (suspension-chord) '("resume-keys"))
     ,@body))

(defun suspension-chord ()
  "The chord the window manager keeps for itself while suspended, as a spec."
  (p:key-to-string (p:way-back-key)))

(test the-way-back-is-a-chord-and-not-a-bare-key
  "A bare Escape would be a way back that also stops you pressing Escape in the
application you handed the keyboard to."
  (let ((spec (suspension-chord)))
    (is-true (search "escape" spec :test #'char-equal) "named for what it is")
    (is-true (search "Super" spec) "and a modifier, which is the whole point")))

(test suspending-leaves-exactly-one-key-registered
  "This is the mechanism: `stop taking keys' has to mean river is told about
none of them.  Ignoring a press would not do -- river hands a registered key to
the window manager and never to the window."
  (with-the-keys-restored
    (with-a-keymap
      (let ((before (p:bindable-keys)))
        (is (> (length before) 20) "the keymap registers its chords, as it always did")
        (p:give-keys-away)
        (is-true (p:keys-suspended-p))
        (let ((after (p:bindable-keys)))
          (is (= 1 (length after)) "one key left: the way back")
          (is (equal (p:way-back-key) (caar after))
              "and it is the one the report names"))
        (p:take-keys-back)
        (is (not (p:keys-suspended-p)))
        (is (equal before (p:bindable-keys))
            "and resuming is the same set coming back, key for key")))))

(test a-suspended-window-manager-does-not-act-on-a-chord
  "The compositor-side disabling is not instant, so the handler refuses too: a
command that ran while the user had been promised the application has the
keyboard is a surprise that loses work."
  (with-the-keys-restored
    (with-a-keymap
      (p:give-keys-away)
      (is (null (r::handle-key (p:kbd "Super+Return")))
          "a chord that is bound does nothing while suspended")
      (is (null (r::handle-key (p:kbd "Super+Shift+Left")))))))

(test the-way-back-still-works-while-suspended
  (with-the-keys-restored
    (with-a-keymap
      (p:give-keys-away)
      (is-true (r::handle-key (p:way-back-key))
               "it is handled, and handling it resumes")
      (is (not (p:keys-suspended-p))))))

(test the-report-lists-what-the-window-manager-takes
  (with-the-keys-restored
    (with-a-keymap
      (let ((rows (r:shortcut-rows)))
        (is (< 20 (length rows)))
        (is-true (every (lambda (row) (stringp (car row))) rows))
        (is-true (every (lambda (row) (stringp (cdr row))) rows)
                 "and every row says what the key does")
        (is-true (some (lambda (row) (plusp (length (cdr row)))) rows)
                 "at least one of them has a description, not an empty cell")))))

(test a-shortcut-row-says-what-the-key-does-not-just-its-name
  (with-a-keymap
    (is-true (plusp (length (r:shortcut-summary "focus"))))
    (is (string= "a chord prefix (more keys follow)"
                 (r:shortcut-summary (p:make-keymap)))
        "a prefix is named as a prefix, because pressing it alone does nothing")))

(test the-report-is-a-page-and-says-so-when-suspended
  (with-the-keys-restored
    (with-a-keymap
      (r:global-shortcuts)
      (is (string= "global shortcuts" (car p:*help-visible*)))
      (is (< 1 (length (cdr p:*help-visible*))))
      (p:give-keys-away)
      (r:global-shortcuts)
      (is (string= "global shortcuts (suspended)" (car p:*help-visible*)))
      (setf p:*help-visible* nil))))

(test suspending-twice-says-where-the-way-back-is-instead-of-doing-nothing
  (with-the-keys-restored
    (p:give-keys-away)
    (let ((message (r:suspend-keys)))
      (is-true (search "already suspended" message))
      (is-true (search (suspension-chord) message)))))

(test the-three-commands-exist
  (dolist (name '("global-shortcuts" "suspend-keys" "resume-keys"
                  "keys-suspended"))
    (is-true (p:find-command name) name)))

(test starting-suspended-is-an-option-and-not-only-a-command
  "The declarative half.  A kiosk or a games console wants the keyboard given
away from the first second, and there is no key to press for that."
  (let ((row (find :start-suspended (p:all-options)
                   :key (lambda (row) (first row)))))
    (is-true row "the option is in the table a config file writes to")
    (is (null (third row)) "and off by default")))