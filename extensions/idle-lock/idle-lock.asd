;;;; idle-lock.asd --- Idle timers: dim, lock, sleep the screen.

(defsystem "idle-lock"
  :description "Run commands after the session has gone quiet, undo them when it wakes."
  :long-description
  "hypridle-equivalent behaviour from the hook surface: after N seconds
without a bound key or a pointer move, run a command -- dim the screen,
start a locker, turn the outputs off -- and when presence returns, run the
commands that put things back.

    (load-extension \"idle-lock\")
    (idle-lock:enable)
    (setf idle-lock:*idle-steps*
          '((900 . (\"brightnessctl\" \"--set\" \"30%\"))
            (1200 . (\"swaylock\" \"-f\"))
            (1500 . (\"wlopm\" \"--off\" \"*\"))))
    (setf idle-lock:*resume-commands*
          '((\"wlopm\" \"--on\" \"*\")
            (\"brightnessctl\" \"--restore\")))

Presence is the runtime's :user-activity hook; a step fires once per quiet
period and never twice."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:file "package")
   (:file "idle-lock")))

(defsystem "idle-lock/tests"
  :description "Tests for the idle-and-lock extension."
  :depends-on ("idle-lock" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-idle-lock"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
