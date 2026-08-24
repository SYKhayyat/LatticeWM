;;;; tests/test-idle-lock.lisp --- Once per quiet period means once.

(defpackage #:idle-lock/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:il #:idle-lock))
  (:export #:run-all))

(in-package #:idle-lock/tests)

(def-suite idle-lock :description "Idle timers on :user-activity.")
(in-suite idle-lock)

(t*:register-extension-suite "IDLE-LOCK/TESTS" "IDLE-LOCK")

(defun run-all ()
  "Run the IDLE-LOCK suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'idle-lock)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-idle (&body body)
  `(let ((il::*enabled* nil)
         (il::*idle-steps* '())
         (il::*resume-commands* '())
         (il::*last-activity* (get-universal-time))
         (il::*fired-steps* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun age-quiet-period-by (seconds)
  "Pretend the last user action happened SECONDS ago."
  (decf il::*last-activity* seconds))

(defun touch-marker (name)
  "A command that leaves a file behind, so a fired step is observable
without pretending the test can see processes."
  (list "sh" "-c" (format nil "touch ~a" (namestring
                                          (merge-pathnames
                                           name (uiop:temporary-directory))))))

(defun marker-exists (name)
  (probe-file (merge-pathnames name (uiop:temporary-directory))))

(defun wait-for-marker (name)
  "Steps go through SPAWN, which detaches; give the child its moment
rather than pretending a write was synchronous."
  (let ((file (merge-pathnames name (uiop:temporary-directory))))
    (loop repeat 50
          unless (probe-file file) do (sleep 0.1)
          finally (return (probe-file file)))))

;;; ================================================================ tests

(test a-step-fires-when-its-time-has-come
  "Past the threshold, one tick runs the step; before it, nothing."
  (with-idle
    (setf il::*enabled* t
          il::*idle-steps* `((60 . ,(touch-marker "il-fired"))))
    (age-quiet-period-by 30)
    (il:tick)
    (is-false (marker-exists "il-fired") "thirty quiet seconds fire nothing")
    (age-quiet-period-by 40)
    (il:tick)
    (is-true (wait-for-marker "il-fired") "seventy do")))

(test a-step-fires-once-per-quiet-period
  "Five ticks deep into a quiet period record ONE firing -- *FIRED-STEPS*
is the memory, checked with = so a threshold cannot be half-fired."
  (with-idle
    (setf il::*enabled* t
          il::*idle-steps* (list '(10 . ("true"))))
    (age-quiet-period-by 100)
    (dotimes (i 5) (il:tick))
    (is (= 1 (length il::*fired-steps*))
        "five ticks past the threshold, one firing recorded")))

(test steps-fire-in-order-and-independently
  "An early step does not block a later one from also firing on the same
tick when both thresholds have passed -- and each fires once."
  (with-idle
    (setf il::*enabled* t
          il::*idle-steps* `((10 . ,(touch-marker "il-first"))
                             (20 . ,(touch-marker "il-second"))))
    (age-quiet-period-by 25)
    (il:tick)
    (is-true (wait-for-marker "il-first"))
    (is-true (wait-for-marker "il-second"))
    (is (= 2 (length il::*fired-steps*)))))

(test activity-resets-and-resumes
  "Presence clears the quiet period, runs the resume commands exactly once,
and lets the steps arm again."
  ;; A step actually fired, then presence returns: the resume runs.
  (with-idle
    (setf il::*enabled* t
          il::*idle-steps* (list '(10 . ("true")))
          il::*resume-commands* (list (touch-marker "il-resumed")))
    (age-quiet-period-by 100)
    (il:tick)
    (is (= 1 (length il::*fired-steps*)) "the step fired")
    (il:note-activity)
    (is-true (wait-for-marker "il-resumed") "a fired step resumes on wake")
    (is (= 0 (length il::*fired-steps*)) "and the period starts over")
    ;; Arming again: a second quiet period fires the step a second time.
    (age-quiet-period-by 100)
    (il:tick)
    (is (= 1 (length il::*fired-steps*)) "re-armed after the wake"))
  (with-idle
    (setf il::*enabled* t
          il::*resume-commands* (list (touch-marker "il-resumed-2")))
    ;; Waking WITHOUT anything having fired must not resume: there is
    ;; nothing to undo, and flashing the backlight proves only that we can.
    (il:note-activity)
    (is-false (marker-exists "il-resumed-2") "no resume for no steps")))

(test a-locked-session-takes-no-steps
  "While a locker holds the session, the timer stands down -- two programs
fighting over one brightness key is not a feature."
  (with-idle
    (setf il::*enabled* t
          il::*idle-steps* `((10 . ,(touch-marker "il-while-locked"))))
    (setf (c:prop r:*world* :locked) t)
    (age-quiet-period-by 100)
    (il:tick)
    (is-false (marker-exists "il-while-locked"))
    (is (= 0 (length il::*fired-steps*)) "nothing recorded as fired")))

(test disabled-is-inert
  "DISABLED stops the ticking.  The registry of steps survives, because
switching a module off is not permission to lose what was configured."
  (with-idle
    (setf il::*idle-steps* `((10 . ,(touch-marker "il-disabled"))))
    (age-quiet-period-by 100)
    (il:tick)
    (is-false (marker-exists "il-disabled") "*ENABLED* gates the tick")
    (is-true il::*idle-steps*)))

(test enable-and-disable-wire-and-unwire
  "ENABLE installs the timer and the hook; DISABLE takes both back.  Idempotent:
enabling twice leaves one timer under the one name, because ADD-TIMER replaces."
  (with-idle
    (unwind-protect
         (progn
           (il:enable)
           (is-true (gethash "idle-lock" r::*timers*) "the timer exists")
           ;; ALL-HOOKS reports how many functions are attached; ours is one
           ;; of them, and it is a named function rather than a closure.
           (is-true (plusp (third (assoc :user-activity (p:all-hooks))))
                    "something is attached to :user-activity")
           (il:enable)
           (let ((matches 0))
             (maphash (lambda (k v)
                        (declare (ignore v))
                        (when (equal k "idle-lock") (incf matches)))
                      r::*timers*)
             (is (= 1 matches) "still one timer after enabling twice")))
      (il:disable))
    (is-false (gethash "idle-lock" r::*timers*) "disable unwired it")))
