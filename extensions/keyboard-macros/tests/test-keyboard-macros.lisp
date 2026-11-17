;;;; tests/test-keyboard-macros.lisp --- Record, play, save.

(defpackage #:keyboard-macros/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:km #:keyboard-macros))
  (:export #:run-all))

(in-package #:keyboard-macros/tests)

(def-suite keyboard-macros :description "Record and play command sequences.")
(in-suite keyboard-macros)

(t*:register-extension-suite "KEYBOARD-MACROS/TESTS" "KEYBOARD-MACROS")

(defun run-all ()
  "Run the KEYBOARD-MACROS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'keyboard-macros)))
    (explain! results)
    (values (results-status results) (length results))))

(defvar *marks* '())
(r:defcommand km-test-mark (&optional (label "x"))
  "Leave a mark the macro tests can find."
  (push label *marks*)
  label)

(defmacro with-macros (&body body)
  `(let ((km::*enabled* nil)
         (km::*recording-p* nil)
         (km::*macro* '())
         (km::*macros* '())
         (p:*last-command* nil)
         (*marks* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ================================================================ tests

(test recording-captures-and-playing-replays
  "The whole feature in one breath: record two steps through the ordinary
path, play once, both steps ran in order."
  (with-macros
    (km:enable)
    (unwind-protect
         (progn
           (km:start-macro)
           (p:run-command "km-test-mark" "first")
           (p:run-command "km-test-mark" "second")
           (km:stop-macro)
           (is (= 2 (length km::*macro*)))
           (setf *marks* '())
           (is-true (km:play-macro))
           ;; MARKS pushes, so REVERSE is replay order.
           (is (equal '("first" "second") (reverse *marks*))))
      (ignore-errors (km:disable)))))

(test count-plays-it-more-than-once
  "PLAY-MACRO with a count runs the whole sequence that many times."
  (with-macros
    (km:enable)
    (unwind-protect
         (progn
           (km:start-macro)
           (p:run-command "km-test-mark" "tick")
           (km:stop-macro)
           (setf *marks* '())
           (km:play-macro "3")
           (is (= 3 (length *marks*)) "three ticks for a count of three")))
    (ignore-errors (km:disable))))

(test excluded-verbs-pass-through
  "START/STOP/PLAY and friends are not recorded -- recording the recording
is a fixed point nobody wants.  UNDO is not recorded either."
  (with-macros
    (km:enable)
    (unwind-protect
         (progn
           (km:start-macro)
           (p:run-command "undo")
           (p:run-command "start-macro")
           (p:run-command "play-macro")
           (is (= 0 (length km::*macro*))))
      (ignore-errors (km:disable)))))

(test starting-a-new-macro-discards-the-old
  "START after a finished macro begins fresh: an un-saved sequence had its
chance to be named."
  (with-macros
    (km:enable)
    (unwind-protect
         (progn
           (km:start-macro)
           (p:run-command "km-test-mark" "old")
           (km:stop-macro)
           (km:start-macro)
           (is (= 0 (length km::*macro*)) "fresh start")
           (p:run-command "km-test-mark" "new")
           (km:stop-macro)
           (is (equal '(("km-test-mark" . ("new"))) km::*macro*))))
    (ignore-errors (km:disable))))

(test save-names-the-last-macro
  "SAVE-MACRO keeps the last recorded sequence under a name; DELETE-MACRO
takes it back off the list."
  (with-macros
    (km:enable)
    (unwind-protect
         (progn
           (km:start-macro)
           (p:run-command "km-test-mark" "kept")
           (km:stop-macro)
           (is (equal "daily" (km:save-macro "daily")))
           (is (member "daily" (km:all-macro-names) :test #'equal))
           (is (equal "daily" (km:delete-macro "daily")))
           (is-false (member "daily" (km:all-macro-names) :test #'equal))))
    (ignore-errors (km:disable))))

(test save-with-nothing-recorded-declines
  "No macro, no save -- declining beats writing an empty file nobody can use."
  (with-macros
    (is-false (km:save-macro "empty"))))

;;; ------------------------------------------------------- a failed save
;;
;; #104.  SAVE-MACRO opened the named file with :IF-EXISTS :SUPERSEDE, which
;; truncates it before the first byte -- so the write whose whole job is to
;; keep a macro was a write that could lose the last one.  The check stands
;; where the save stands: the temporary the save now writes through is made
;; impossible, so the write fails, and the file must still be yesterday's
;; macro.

(defun block-the-replacement (file)
  "Make FILE's temporary unopenable, so the save cannot start writing."
  (let* ((temporary (r:temporary-state-path file))
         ;; The namestring plus a slash, because ENSURE-DIRECTORY-PATHNAME
         ;; would re-escape the dot SBCL escapes in the temporary's type.
         (blocked (pathname (concatenate 'string (namestring temporary) "/"))))
    (ensure-directories-exist blocked)
    blocked))

(defun unblock-the-replacement (temporary)
  (ignore-errors
    (uiop:delete-directory-tree temporary :validate t
                                :if-does-not-exist :ignore)))

(defun saved-macro-entries (file)
  "The :ENTRIES in the macro file FILE, read the way the extension writes them."
  (with-open-file (in file)
    (let ((*package* (find-package :keyword)))
      (getf (read in) :entries))))

(defun fresh-macros-directory ()
  "A directory that exists and holds nobody else's macros."
  (let ((dir (merge-pathnames (format nil "km-test-~d-~d/"
                                      (sb-posix:getpid) (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(test a-save-that-cannot-write-keeps-the-last-macro
  "A save that fails leaves the previous macro file exactly as it was."
  (let ((km::*macros-directory* (fresh-macros-directory)))
    (with-macros
      (setf km::*macro* '(("km-test-mark" . ("yesterday"))))
      (is (equal "keepme" (km:save-macro "keepme")))
      (let* ((file (merge-pathnames (make-pathname :name "keepme" :type "lisp")
                                    (km::macros-directory)))
             (blocked (block-the-replacement file)))
        (unwind-protect
             (progn
               (setf km::*macro* '(("km-test-mark" . ("today"))))
               (is (eq :failed
                       (handler-case (progn (km:save-macro "keepme") :saved)
                         (error () :failed)))
                   "the save could not be written")
               (is (equal '(("km-test-mark" . ("yesterday")))
                          (saved-macro-entries file))
                   "and yesterday's macro is still in the file"))
          (unblock-the-replacement blocked))
        (is-false (probe-file (r:temporary-state-path file))
                  "with no temporary left behind")
        (ignore-errors (delete-file file))))))
