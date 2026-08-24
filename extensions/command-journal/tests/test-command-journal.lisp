;;;; tests/test-command-journal.lisp --- Storage discipline, verified.

(defpackage #:command-journal/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:cj #:command-journal))
  (:export #:run-all))

(in-package #:command-journal/tests)

(def-suite command-journal :description "Record and replay.")
(in-suite command-journal)

(t*:register-extension-suite "COMMAND-JOURNAL/TESTS" "COMMAND-JOURNAL")

(defun run-all ()
  "Run the COMMAND-JOURNAL suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'command-journal)))
    (explain! results)
    (values (results-status results) (length results))))

;;; A command the tests can watch go through the ordinary path.  Defined at
;;; load time like any other command; nothing here pretends otherwise.
(defvar *marks* '())
(r:defcommand cj-test-mark (&optional (label "x"))
  "Leave a mark the journal tests can find."
  (push label *marks*)
  label)

(defun fresh-journals-directory ()
  (let ((dir (merge-pathnames (format nil "cj-test-~d/" (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defmacro with-journal (&body body)
  `(let ((cj::*enabled* nil)
         (cj::*recording* nil)
         (cj::*journal* '())
         (cj::*journals-directory* (fresh-journals-directory))
         (p:*last-command* nil)
         (*marks* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun record-run (name &rest args)
  "Run a command the way the real path does, through every wrapper."
  (apply #'p:run-command name args))

;;; ================================================================ tests

(test recording-captures-the-ordinary-path
  "With recording on, a command run through RUN-COMMAND lands in the
journal as (NAME . ARGS) -- one choke point, the whole mechanism."
  (with-journal
    (cj:enable)
    (unwind-protect
         (progn
           (cj:start-journal)
           (record-run "cj-test-mark")
           (record-run "cj-test-mark" "second")
           (is (= 2 (cj:journal-entry-count)))
           (is (equal '("cj-test-mark" . ("second"))
                      (first (last cj::*journal*))))
           (is (equal '("cj-test-mark")
                      (first cj::*journal*))))
      (cj:disable))))

(test not-recording-captures-nothing
  "The wrapper is installed by ENABLE and armed by START; without either,
nothing is recorded."
  (with-journal
    ;; Enabled but never started.
    (cj:enable)
    (unwind-protect
         (progn
           (record-run "cj-test-mark")
           (is (= 0 (cj:journal-entry-count)) "armed matters"))
      (cj:disable)))
  (with-journal
    ;; Started but never enabled -- no wrapper, no record.
    (cj:start-journal)
    (record-run "cj-test-mark")
    (is (= 0 (cj:journal-entry-count)) "installed matters")))

(test excluded-commands-pass-through
  "UNDO and REPEAT are looked straight through: a journal replays actions,
not retractions of actions or echoes of actions."
  (with-journal
    (cj:enable)
    (unwind-protect
         (progn
           (cj:start-journal)
           (record-run "undo")
           (record-run "repeat")
           (record-run "start-journal")
           (is (= 0 (cj:journal-entry-count)))))
      (cj:disable)))

(test stop-stops-without-forgetting
  "STOP ends recording; the entries stay until cleared."
  (with-journal
    (cj:enable)
    (unwind-protect
         (progn
           (cj:start-journal)
           (record-run "cj-test-mark")
           (cj:stop-journal)
           (record-run "cj-test-mark")
           (is (= 1 (cj:journal-entry-count))
               "one recorded, one after the stop not")))
      (cj:disable)))

(test replay-runs-everything-in-order
  "Replay sends each entry through RUN-COMMAND, in order, and reports."
  (with-journal
    (cj:enable)
    (unwind-protect
         (progn
           (cj:start-journal)
           (record-run "cj-test-mark" "first")
           (record-run "cj-test-mark" "second")
           (cj:stop-journal)
           (setf *marks* '())
           (multiple-value-bind (total failed) (cj:replay-journal)
             (is (= 2 total))
             (is (= 0 failed))
             ;; MARKS pushes, so REVERSE puts them in replay order.
             (is (equal '("first" "second") (reverse *marks*)))))
      (cj:disable))))

(test save-and-load-roundtrip
  "A saved journal reads back as the same entries, into a session that was
never running when they were recorded."
  (with-journal
    (cj:enable)
    (unwind-protect
         (progn
           (cj:start-journal)
           (record-run "cj-test-mark" "kept")
           (cj:save-journal-as "morning")
           (is (equal '("morning") (cj:all-journal-names)))
           (cj:clear-journal)
           (is (= 0 (cj:journal-entry-count)))
           (is (equal "morning" (cj:load-journal "morning")))
           (is (equal '(("cj-test-mark" . ("kept"))) cj::*journal*)))
      (cj:disable))))

(test load-of-nonsense-declines
  "A file that is not a readable journal is declined, not half-loaded --
the same ruling the core makes about hand-edited state files."
  (with-journal
    (let ((file (merge-pathnames "nonsense.lisp" cj::*journals-directory*)))
      (with-open-file (out file :direction :output)
        (princ "(this is not a journal" out))
      (is-false (cj:load-journal "nonsense"))
      (is (= 0 (cj:journal-entry-count))))))
