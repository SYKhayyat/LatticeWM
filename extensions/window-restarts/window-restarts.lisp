;;;; window-restarts/window-restarts.lisp --- A crash is a question, not a log line.

(in-package #:window-restarts)

;;; ================================================================ state

(defvar *enabled* nil "True while crashes are being noticed.")

(defvar *user-close-commands* '("close")
  "Commands whose running means a window that vanishes next was ASKED to.
A close you requested is not a crash, however much it may resemble one.")

(defparameter *user-close-window* 500
  "Milliseconds within which a window-closed counts as caused by a close
command just run.  Internal time: one tick is a millisecond here only by
agreement between this variable and INTERNAL-REAL-TIME.")

(defvar *last-user-close* nil
  "Internal real time of the most recent user-requested close.")

(defvar *last-broken* nil
  "The most recent unexpected exit, as (:APP-ID string :TIME universal-time),
or NIL once dismissed or superseded.")

(defun enabled-p () "True when crashes are being noticed." *enabled*)

(defun last-broken-app-id ()
  "The application id of the window that last exited unexpectedly, or NIL."
  (getf *last-broken* :app-id))

;;; =========================================================== the condition

(define-condition broken-window (condition)
  ((app-id :initarg :app-id :reader broken-app-id
           :documentation "The application id of the window that went away."))
  (:report (lambda (condition stream)
             (format stream "the application ~s exited unexpectedly"
                     (broken-app-id condition))))
  (:documentation "Signalled when a window goes away unasked-for.

RETRY, UNDO-LAYOUT and DISMISS restarts are established wherever this module
offers the choice; a HANDLER-BIND elsewhere may intervene first, which is
what a condition is FOR."))

;;; ================================================================ actions

;;; ================================================================ plumbing

(defun note-user-close (command arguments thunk)
  "Record that a close was just asked for, so the vanishing it causes is
not mistaken for a crash."
  (declare (ignore arguments))
  (when (member (p:command-name command) *user-close-commands*
                :test #'string=)
    (setf *last-user-close* (get-internal-real-time)))
  (funcall thunk))

(defun note-window-closed (window)
  "The hook body: decide whether this departure was wanted.

A close we asked for recently is life working normally.  Anything else is a
broken window: say so, remember who it was, and offer the menu."
  (let ((app-id (c:window-app-id window)))
    (unless (and *last-user-close*
                 (<= (- (get-internal-real-time) *last-user-close*)
                     *user-close-window*))
      (setf *last-broken* (list :app-id app-id
                                :time (get-universal-time)))
      (signal 'broken-window :app-id app-id)
      ;; SIGNAL returns when nobody handles -- which is the common case at
      ;; the keyboard, where the handlers are fingers.  Say what happened
      ;; and what the choices are, in the order the menu offers them.
      (r:notify "~a exited unexpectedly -- retry: undo: dismiss" (or app-id "?")))))

(defun enable ()
  "Start noticing.  Idempotent: the wrapper and the hook are added by
identity and by name respectively."
  (p:add-command-wrapper #'note-user-close)
  (p:add-hook :window-closed 'note-window-closed)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop noticing.  A remembered crash stays remembered until dismissed."
  (p:remove-command-wrapper #'note-user-close)
  (p:remove-hook :window-closed 'note-window-closed)
  (setf *enabled* nil)
  nil)

;;; ================================================================== menu

(r:defcommand retry-broken-window ()
  "Spawn the application whose window exited unexpectedly.

The same program name again: an installer crashed mid-run wants rerunning,
and a terminal that died wants reopening.  What was on screen is gone --
windows are not processes -- so this is a respawn, not a resurrection."
  (let ((app-id (last-broken-app-id)))
    (cond
      ((null app-id) (r:notify "nothing to retry") nil)
      (t (r:spawn app-id)
         (dismiss-broken-window)
         app-id))))

(r:defcommand undo-for-broken-window ()
  "Revert the last layout change, as UNDO does.

A crash takes a pane with it and the tree retiles around the hole; if the
retile itself is what looks wrong, this walks it back.  The undo ring is the
core's own -- this command exists so the menu can reach it under the same
key as the other two answers."
  (r:undo))

(r:defcommand dismiss-broken-window ()
  "Carry on.  Forgets the crash; nothing else changes."
  (setf *last-broken* nil)
  nil)

(defvar *menu*
  (let ((map (r:make-keymap :name "restarts")))
    (r:define-key map "r" '("retry-broken-window"))
    (r:define-key map "u" '("undo-for-broken-window"))
    (r:define-key map "d" '("dismiss-broken-window"))
    map)
  "The SLDB-shaped menu: R retry, U undo, D dismiss.

Bind it behind whatever key you like -- a chord's second key is looked up in
here, and river's ensure_next_key_eaten makes any key the menu does not bind
leave the menu cleanly rather than leak into an application.")

(defvar *menu*
  (let ((map (r:make-keymap :name "restarts")))
    (r:define-key map "r" '(:retry-broken-window))
    (r:define-key map "u" '(:undo-for-broken-window))
    (r:define-key map "d" '(:dismiss-broken-window))
    map)
  "The SLDB-shaped menu: R retry, U undo, D dismiss.

Bind it behind whatever key you like -- a chord's second key is looked up in
here, and river's ensure_next_key_eaten makes any key the menu does not bind
leave the menu cleanly rather than leak into an application.")
