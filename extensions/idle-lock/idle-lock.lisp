;;;; idle-lock/idle-lock.lisp --- The session went quiet; act like you noticed.

(in-package #:idle-lock)

;;; ================================================================ state

(defvar *enabled* nil "True while the idle timer is running.")

(defvar *idle-steps* '()
  "What to do as the session goes quiet, as (SECONDS . COMMAND) pairs.

COMMAND is a list of strings -- program and arguments, no shell.  Steps fire
in ascending order of SECONDS, each exactly once per quiet period: dim at
ten minutes, lock at twenty, DPMS off at thirty.

    '((600 . (\"brightnessctl\" \"--set\" \"30%\"))
      (900 . (\"swaylock\" \"-f\")))

NIL, the default, is a session that never times out -- the module enabled
with no steps costs one timestamp comparison every few seconds.")

(defvar *resume-commands* '()
  "What to do when presence returns after any step fired.

A list of commands in the same shape as *IDLE-STEPS*' commands.  This is
where the screen comes back: brightness restored, outputs on.  Run in order,
once, only if at least one step actually fired -- waking from a quiet period
that never dimmed anything should not flash the backlight.")

(defvar *poll-interval-seconds* 5
  "How often the timer wakes to compare now against last activity.

The thresholds are honoured to within this interval; a step set to 90 seconds
fires between 90 and 95.  Finer than five seconds buys nothing: the hook that
feeds it fires on human timescales.")

(defvar *last-activity* 0 "Universal-time of the last thing the user did.")

(defvar *fired-steps* '()
  "The thresholds already fired in this quiet period, so a step is a once-per-
quiet-period fact rather than a once-per-tick accident.")

(defun enabled-p () "True when the idle timer is running." *enabled*)

(defvar *lock-command* '("swaylock" "-f")
  "What LOCK-NOW runs.  Program and arguments, no shell; a locker speaking
=ext-session-lock-v1=, which the window manager already knows how to make
room for.")

;;; ============================================================ the pieces

(defun note-activity ()
  "Record that the user did something, and undo what idleness did.

Attached to :user-activity by ENABLE.  Cheap by contract: a SETF, and on the
wake from a fired quiet period, the resume commands.  Everything expensive
about noticing presence was paid by whoever fired the hook."
  (setf *last-activity* (get-universal-time))
  (when *fired-steps*
    (setf *fired-steps* '())
    (dolist (command *resume-commands*)
      (apply #'r:spawn command))))

(defun idle-seconds ()
  "How long the session has been quiet, in whole seconds."
  (max 0 (- (get-universal-time) *last-activity*)))

(defun tick ()
  "Fire every step whose time has come and not already come.

Runs on the timer's thread.  A locked session takes no steps: whatever is on
screen belongs to the locker now, and a locker plus a dimmed screen is two
programs fighting over one brightness key."
  (when (and *enabled*
             (not (c:prop r:*world* :locked)))
    (let ((quiet (idle-seconds)))
      (dolist (step *idle-steps*)
        (destructuring-bind (seconds . command) step
          (when (and (<= seconds quiet)
                     (not (member seconds *fired-steps* :test #'=)))
            (push seconds *fired-steps*)
            (apply #'r:spawn command)))))))

;;; ============================================================== plumbing

(defun note-user-activity ()
  "The named function attached to :user-activity.

Named because ADD-HOOK keeps what it is given, and a fresh closure per call
would accumulate rather than replace."
  (note-activity))

(defun enable ()
  "Start watching for quiet.  Idempotent: the hook and the timer are added by
name, so enabling twice leaves one of each."
  (p:add-hook :user-activity 'note-user-activity)
  (r:add-timer "idle-lock" *poll-interval-seconds* 'tick)
  (setf *last-activity* (get-universal-time)
        *fired-steps* '()
        *enabled* t)
  nil)

(defun disable ()
  "Stop watching.  A quiet period in progress ends un-resumed -- DISABLE is a
decision to stop caring, not an instruction to wake the screen."
  (r:remove-timer "idle-lock")
  (p:remove-hook :user-activity 'note-user-activity)
  (setf *enabled* nil
        *fired-steps* '())
  nil)

;;; ============================================================== commands

(r:defcommand lock-now ()
  "Lock the session right now, with *LOCK-COMMAND*."
  (apply #'r:spawn *lock-command*))
