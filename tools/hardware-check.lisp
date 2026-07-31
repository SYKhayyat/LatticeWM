;;;; tools/hardware-check.lisp --- Answer the three questions that need hands.
;;;;
;;;; PLAN §log2 has carried three open items since the first session, and all
;;;; three are open for the same reason: they are facts about what river and
;;;; libinput actually do with a real keyboard on a real display, and every
;;;; session so far ran nested and was driven through the SWANK bridge.
;;;;
;;;;   1. Does river report Shift in the modifier set for a shifted keysym, or
;;;;      mask it out?  Both bindings exist so one fires either way, but which
;;;;      one has never been observed.
;;;;   2. Does ate_unbound_key fire when the seat has no keyboard focus?  D19's
;;;;      machinery is written and bound and has never been seen to run.
;;;;   3. Does multi-monitor work?  It is written and has only ever met one
;;;;      nested output.
;;;;
;;;; Load it from ~/.config/latticewm/init.lisp -- there is deliberately no
;;;; --config flag, so this is a LOAD line rather than an argument:
;;;;
;;;;   (load "/home/you/LatticeWM/tools/hardware-check.lisp")
;;;;
;;;; Then press the keys it asks for and quit.  Every answer is appended to
;;;; ~/latticewm-hardware.txt as it happens, so the whole thing is one pass on
;;;; the tty rather than a conversation across a VT switch, and a crash keeps
;;;; whatever was learned before it.
;;;;
;;;; It records rather than concludes.  Every line is what was observed.

(in-package #:latticewm/user)

(defparameter *report* (merge-pathnames "latticewm-hardware.txt"
                                        (user-homedir-pathname)))

(defun note (format &rest arguments)
  "Append one observation to the report, immediately.

Immediately, and not buffered, because the most interesting way for this to
end is the window manager dying — and a report that is lost precisely when
something goes wrong is worse than no report."
  (with-open-file (out *report* :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
    (apply #'format out format arguments)
    (terpri out)))

;;; ------------------------------------------------------------- question 1
;;;
;;; Every printable keysym is bound twice, bare and with Shift, because river
;;; matches on keysym AND modifiers and xkb produces `parenleft' for Shift+9
;;; with Shift possibly still in the set.  This records which of the two
;;; actually fired.

(defvar *seen-shifted* '())

(defmethod on-key ((policy conventional-policy) world key)
  (declare (ignore world))
  (let* ((keysym (car key))
         (modifiers (cdr key))
         (name (keysym-name keysym)))
    ;; The interesting keys are the ones that only exist as a shifted glyph.
    (when (member name '("parenleft" "parenright" "colon" "question"
                         "underscore" "plus" "asciitilde" "at")
                  :test #'string-equal)
      (pushnew (list name modifiers) *seen-shifted* :test #'equal)
      (note "SHIFT   keysym=~a modifiers=~s  -> river ~:[MASKS SHIFT OUT~;REPORTS SHIFT~]"
            name modifiers (member :shift modifiers))))
  nil)

;;; ------------------------------------------------------------- question 2
;;;
;;; D19: an unbound key pressed while the cursor rests on an empty pane spawns
;;; something.  It rests on ate_unbound_key firing when the seat has no
;;; keyboard focus, which is exactly the state an empty pane puts it in.

(defmethod key-unbound ((policy conventional-policy) world keysym)
  (note "UNBOUND ate_unbound_key FIRED, keysym=~a (~d) -- D19 works on bare metal"
        (keysym-name keysym) keysym)
  (call-next-method))

;;; ------------------------------------------------------------- question 3

(defun record-outputs ()
  "Every output river gave us, with its geometry and workspace."
  (let ((outputs (all-outputs)))
    (note "OUTPUTS ~d output~:p" (length outputs))
    (dolist (output outputs)
      (let ((r (output-rect output)))
        (note "OUTPUT  name=~a  ~dx~d at (~d,~d)  workspace=~a"
              (output-name output) (rect-w r) (rect-h r) (rect-x r) (rect-y r)
              (prop output :workspace))))))

;;; ------------------------------------------------------------- the preamble

(add-hook :startup 'hardware-check-banner)

(defun hardware-check-banner ()
  (note "~%======== LatticeWM hardware check ========")
  (note "river   ~a" (or (uiop:getenv "WAYLAND_DISPLAY") "bare metal, no parent"))
  (note "session ~a" (or (uiop:getenv "XDG_SESSION_TYPE") "?"))
  (note "font    ~a ~dx~d" (font-name *default-font*)
        (font-width *default-font*) (font-height *default-font*)))

;;; Outputs are not bound yet at :STARTUP, so ask once the first layout runs.
;;; :LAYOUT-CHANGED, not :RELAYOUT -- the latter is not a hook this system
;;; runs, which is the kind of thing that is free to discover here and
;;; expensive to discover on a tty with no editor.
(add-hook :layout-changed 'record-outputs-once)

(defvar *outputs-recorded* nil)

(defun record-outputs-once ()
  (unless *outputs-recorded*
    (setf *outputs-recorded* t)
    (ignore-errors (record-outputs))))

(defcommand hardware-report ()
  "Write what has been observed so far, and say where.

Bound to Super+F12 below.  Run it before quitting if you want to be certain
the file is complete, though every line is written as it happens."
  (note "SUMMARY shifted keysyms observed: ~s" *seen-shifted*)
  (ignore-errors (record-outputs))
  (notify "hardware report: ~a" *report*))

(define-key *keymap* "Super+F12" '("hardware-report"))
