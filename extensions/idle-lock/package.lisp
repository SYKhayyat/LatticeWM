;;;; idle-lock/package.lisp

(defpackage #:idle-lock
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Idle timers on the :user-activity hook.

    (setf *idle-steps* '((900 . (\"swaylock\" \"-f\"))))
    (enable)

After SECONDS of nothing -- no bound key, no pointer move -- each step's
command runs, once, in ascending order.  When presence returns, every command
in *RESUME-COMMANDS* runs and the quiet period starts over.")
  (:export #:*idle-steps*
           #:*resume-commands*
           #:*poll-interval-seconds*
           #:enable #:disable #:enabled-p
           #:idle-seconds
           #:note-activity
           #:tick
           #:lock-now))
