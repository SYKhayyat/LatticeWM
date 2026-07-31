;;;; runtime/keys.lisp --- Key dispatch.
;;;;
;;;; The keymap itself -- keysyms, key specs, the tree, and the vocabulary
;;;; of modifier names -- is policy/keys.lisp.  What is left here is the
;;;; part that cannot be: running what a key was bound to, which reaches
;;;; MARK-DIRTY and NOTIFY and therefore the server.

(in-package #:latticewm/runtime)

;;; --------------------------------------------------------- dispatching

(defmacro reporting-sequence-violations (&body body)
  "Run BODY, and say out loud if it used a request the sequence forbade.

A SEQUENCE-VIOLATION is always a bug in this program and never something a
user did.  It was nevertheless invisible: GUARDED turned it into a log line on
a stderr nobody reads, so Super+q refused to close a window seven times in a
row and the screen said nothing at all.

Twenty-nine requests are manage-sequence-only and every command runs outside a
sequence, so this is a mistake that can be made again.  The echo area is where
this program tells you things; a violation belongs there."
  `(handler-bind ((w:sequence-violation
                    (lambda (condition)
                      (notify "~a needs a manage sequence -- this is a bug"
                              (w:sequence-violation-request condition))
                      (p:logmsg :error "sequence violation: ~a" condition))))
     ,@body))

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
    (function (reporting-sequence-violations
                (guarded "key binding" (funcall target)))
              t)
    (cons (reporting-sequence-violations
            (let ((command (find-command (first target))))
            (if (and command
                     (null (rest target))
                     (some (lambda (argument) (eq :required (third argument)))
                           (command-arguments command)))
                (call-interactively command)
                (apply #'run-command (first target) (rest target)))))
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
      ;;
      ;; But dismissing is not the same as *consuming*.  This used to return T
      ;; here, so the keystroke that took the overlay down did nothing else --
      ;; and on a first run the overlay is the welcome screen, which means the
      ;; first deliberate keypress of somebody's first session was swallowed.
      ;; The report was "the first super return did not open term", and they
      ;; were right: it closed the welcome screen instead, with nothing to say
      ;; that was what had happened.  A window manager that ignores your first
      ;; instruction is broken at the only moment first impressions are formed.
      ;;
      ;; So: dismiss, then let the key do its job as well.  The one exception
      ;; is the key bound to HELP itself, which toggles -- dismissing and then
      ;; toggling would put the overlay straight back up, so Super+/ would
      ;; refuse to close what Super+/ opened.
      ((and *help-visible* (not (reading-p)))
       (setf *help-visible* nil)
       (mark-dirty)
       (let ((target (lookup-key *keymap* key)))
         (if (and (consp target) (equal (first target) "help"))
             t
             (progn (run-key-target target) t))))
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

Two callers: a submap that is waiting for its second key, and DESIGN D19's
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
