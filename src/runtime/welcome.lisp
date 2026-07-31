;;;; runtime/welcome.lisp --- The first thing a new user sees.
;;;;
;;;; A window manager has a discoverability problem no editor has: when it
;;;; starts, the screen is empty and there is nothing to click.  Every key is
;;;; unlabelled and the only way to learn one is to already know it.
;;;;
;;;; help.lisp solves the general case — Super+/ draws the whole keymap — but
;;;; that only helps somebody who knows to press Super+/, which is exactly the
;;;; thing a new user does not know.  niri answers this by putting an important
;;;; hotkeys overlay on the screen the first time it runs, and it is the single
;;;; cheapest thing that turns "my keyboard is broken" into "oh, it is Super".
;;;;
;;;; So: shown once, on the first start, and never again.  The rows are derived
;;;; from *MODIFIER* and from each command's own docstring rather than written
;;;; out, for the same reason the help overlay is — a welcome screen that
;;;; disagrees with the keymap is worse than none, and it would disagree the
;;;; first time somebody rebound a key.

(in-package #:latticewm/runtime)

(p:define-option *welcome-on-first-run* t
  "Show the welcome overlay the first time LatticeWM starts.

Once only, and it writes a marker under $XDG_STATE_HOME so it never appears
again.  Set this to NIL to suppress it entirely; M-x welcome shows it on
demand either way.")

(defun welcome-marker ()
  "The file whose existence means the welcome overlay has been shown.

Beside the saved layout, under $XDG_STATE_HOME, because it is state rather
than configuration: it records what happened, not what the user wants.
Deleting it makes the overlay appear once more."
  (merge-pathnames "latticewm/welcome-seen"
                   (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
                       (merge-pathnames ".local/state/" (user-homedir-pathname)))))

(defun welcome-rows ()
  "The important keys, as overlay rows.

Deliberately about a dozen entries.  The full keymap is one key away and is a
*reference*; this is the smaller thing a reference cannot be — the six or so
facts somebody needs before they can use the machine at all, ending with how
to get out.

Both halves are derived: the key names from *MODIFIER*, so rebinding it moves
every row, and the descriptions from each command's own docstring, so they
cannot drift from what the command actually does."
  (let ((mod (string-capitalize (string *modifier*))))
    (flet ((row (keys command &optional text)
             (let ((found (find-command command)))
               (cons keys (or text
                              (summary-of (and found
                                               (command-documentation found)))
                              command)))))
      (list
       (row (format nil "~a+Return" mod) "terminal")
       (row (format nil "~a+d / ~a+s" mod mod) "split"
            "Split the focused pane, side by side or stacked")
       (row (format nil "~a+h j k l" mod) "focus"
            "Move focus -- the arrow keys do this too")
       (row (format nil "~a+q" mod) "close")
       (row (format nil "~a+1 ... ~a+0" mod mod) "workspace"
            "Go to a workspace")
       (row (format nil "~a+Space" mod) "toggle-float")
       (cons "" "")
       (row (format nil "~a+/" mod) "help"
            "Every key binding, with what it does")
       (row (format nil "~a+x" mod) "run-command-by-name"
            "Run any command by name")
       (row (format nil "Shift+~a+?" mod) "describe-key"
            "Ask about a key, a command or a setting")
       (row (format nil "~a+;" mod) "eval-expression"
            "Evaluate a Lisp form inside the running window manager")
       (cons "" "")
       (row (format nil "Shift+~a+Escape" mod) "quit")))))

(defcommand welcome ()
  "Show the handful of keys worth knowing before anything else.

This is what appears by itself the first time LatticeWM runs.  Super+/ is the
complete keymap; this is the short version, and it ends with how to quit
because a window manager you cannot leave is a frightening thing to try."
  (show-help-page "Welcome to LatticeWM" (welcome-rows)))

(defun maybe-show-welcome ()
  "Show the welcome overlay if this is the first run.  Never signals.

The marker is written *before* the overlay goes up, not after.  If drawing it
were to fail, a marker written afterwards would leave the failure repeating on
every single start — and the cost of the two orderings is not symmetric: one
loses a welcome screen, the other breaks every subsequent launch."
  (when (and *welcome-on-first-run* (not (probe-file (welcome-marker))))
    (guarded "welcome"
      (let ((path (welcome-marker)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
          (format out ";;;; LatticeWM has shown its welcome overlay.~%~
                       ;;;; Delete this file to see it again, or run ~
                       M-x welcome at any time.~%")))
      (welcome)
      t)))
