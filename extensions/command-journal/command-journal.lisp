;;;; command-journal/command-journal.lisp --- Storage discipline over one door.

(in-package #:command-journal)

;;; ================================================================ state

(defvar *enabled* nil "True while the recording wrapper is installed.")

(defvar *recording* nil "True while a journal is being written.")

(defvar *journal* '()
  "The entries so far, most recent LAST, as (COMMAND-NAME . ARGUMENTS).")

(defvar *excluded-commands* '("start-journal" "stop-journal" "clear-journal"
                              "replay-journal" "load-journal"
                              "save-journal-as"
                              "repeat" "undo" "redo"
                              "run-command-by-name")
  "Commands the journal looks straight through.

The journal's own verbs, because replaying a recording is not part of what
the recording did.  UNDO and REDO, because a journal replays ACTIONS, not
retractions of actions -- replaying yesterday's journal into today's session
should not un-do today's work on the way past.  And REPEAT, because what
`.' repeated was already journaled under its own name.")

(defvar *journals-directory* nil
  "Where saved journals are written, or NIL for <data directory>/journals/.
Bound by tests; left alone by everybody else.")

(defun enabled-p () "True when the recording wrapper is installed." *enabled*)

(defun journal-entry-count ()
  "How many commands the journal holds."
  (length *journal*))

;;; ============================================================ the wrapper

(defun record-command (command arguments thunk)
  "Append the command to the journal, then run the rest of the chain.

Recording happens BEFORE the thunk, not after: a command that errors or is
vetoed still happened as an INTENT, and the journal is a record of what you
asked for -- but see *EXCLUDED-COMMANDS*, which is where the verbs that
must never be journaled are named once rather than special-cased here."
  (when (and *recording*
             (not (member (p:command-name command) *excluded-commands*
                          :test #'string=)))
    (setf *journal*
          (append *journal* (list (cons (p:command-name command)
                                        (copy-list arguments))))))
  (funcall thunk))

(defun enable ()
  "Install the recorder.  Idempotent by identity."
  (p:add-command-wrapper #'record-command)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Uninstall the recorder.  The journal itself survives; DISABLE is not
permission to lose what was recorded."
  (p:remove-command-wrapper #'record-command)
  (setf *enabled* nil)
  nil)

;;; ============================================================== commands

(r:defcommand start-journal ()
  "Begin recording every state-changing command into the journal.

A journal in progress keeps its entries: START twice is one journal with
more in it, not two journals."
  (cond (*recording* (r:notify "already recording") nil)
        (t (setf *recording* t) t)))

(r:defcommand stop-journal ()
  "Stop recording.  The entries stay until cleared or saved-and-cleared."
  (cond ((not *recording*) (r:notify "not recording") nil)
        (t (setf *recording* nil) t)))

(r:defcommand clear-journal ()
  "Forget every recorded entry.  Saved journals are files; they stay."
  (setf *journal* '())
  nil)

(defun journals-directory ()
  "Where saved journals live, like layouts do: the first data directory
with a journals/ subdirectory we can write."
  (or *journals-directory*
      (let ((dir (find-if
                  (lambda (root)
                    (let ((journals (merge-pathnames "journals/" root)))
                      (ignore-errors
                       (ensure-directories-exist journals)
                       (probe-file journals))))
                  (r:data-directories))))
        (and dir (merge-pathnames "journals/" dir)))))

(defun journal-file (name)
  "The file journal NAME is kept in, or NIL with nowhere to keep it."
  (let ((dir (journals-directory)))
    (and dir (merge-pathnames (make-pathname :name name :type "lisp") dir))))

(defun all-journal-names ()
  "Every saved journal name, sorted."
  (let ((dir (journals-directory)))
    (when dir
      (sort (mapcar #'pathname-name
                    (directory (merge-pathnames "*.lisp" dir)))
            #'string<))))

(r:defcommand save-journal-as (name)
  "Write the journal to file NAME, keeping it in memory as well."
  (:interactive :name)
  (let ((file (journal-file name)))
    (cond
      ((null file) (r:notify "nowhere to save journals") nil)
      (t (ensure-directories-exist file)
         (with-open-file (out file :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
           (let ((*package* (find-package :keyword)))
             (write (list :version 1 :entries *journal*) :stream out)))
         (r:logmsg :info "saved journal ~a (~d entr~:@p)"
                   name (length *journal*))
         name))))

(r:defcommand load-journal (name)
  "Replace the in-memory journal with saved journal NAME's entries."
  (:interactive :journal-name)
  (let ((file (journal-file name)))
    (cond
      ((or (null file) (not (probe-file file)))
       (r:notify "no journal named ~a" name)
       nil)
      (t (let ((form (ignore-errors
                      (with-open-file (in file)
                        (let ((*package* (find-package :keyword)))
                          (read in nil nil))))))
           (cond
             ((and (consp form) (getf form :entries))
              (setf *journal* (copy-list (getf form :entries)))
              name)
             (t (r:notify "~a is not readable as a journal" name)
                nil)))))))

(p:define-argument-type :journal-name "journal: "
  :documentation "A saved journal, with completion over the files."
  :candidates (all-journal-names))

(defun replay-entry (entry)
  "Run one journaled entry through the ordinary command path.

RUN-COMMAND, not the command function directly: wrappers are other
extensions' doors too, and a replay that bypasses them replays something
other than what happened."
  (guarded-run (car entry) (cdr entry)))

(defun guarded-run (name arguments)
  "RUN-COMMAND that reports failure instead of stopping the replay."
  (ignore-errors (apply #'p:run-command name arguments)))

(r:defcommand replay-journal ()
  "Run every journaled command, in order, through the ordinary path.

Into THIS session -- into a fresh one tomorrow after loading the journal it
saved.  An entry that fails is reported and skipped, because a journal is a
list of intentions and the world they meet has moved on."
  (let ((total (length *journal*)) (failed 0))
    (dolist (entry *journal*)
      (unless (guarded-run (car entry) (cdr entry))
        (incf failed)))
    (cond
      ((zerop total) (r:notify "journal is empty"))
      ((zerop failed) (r:notify "replayed ~d command~:p" total))
      (t (r:notify "replayed ~d of ~d; ~d failed" (- total failed) total failed)))
    (values total failed)))
