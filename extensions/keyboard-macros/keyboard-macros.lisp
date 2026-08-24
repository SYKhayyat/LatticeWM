;;;; keyboard-macros/keyboard-macros.lisp --- Vim-style repeat, over everything.

(in-package #:keyboard-macros)

;;; ================================================================ state

(defvar *enabled* nil "True while the recording wrapper is installed.")

(defvar *recording-p* nil "True between START-MACRO and STOP-MACRO.")

(defvar *macro* '()
  "The macro being recorded, or the last one recorded: a list of
(COMMAND-NAME . ARGUMENTS), oldest first.")

(defvar *macros* '()
  "Named macros, as an alist of NAME to entries.  Names are how a sequence
outlives the session that recorded it -- the file system does the rest.")

(defvar *excluded-commands* '("start-macro" "stop-macro" "play-macro"
                              "save-macro" "delete-macro"
                              "undo" "redo" "repeat"
                              "run-command-by-name")
  "Commands the recorder looks straight through.

The macro's own verbs, because recording the recording is a fixed point
nobody wants.  UNDO and REDO, because replaying actions should not replay
their retraction.  And REPEAT for the reason that makes the exclusion list
in command-journal honest: what `.' repeated already happened under its own
name, and it is THAT name which belongs in the macro.")

(defun enabled-p () "True when the recording wrapper is installed." *enabled*)

(defun recording-p () "True while a macro is being recorded." *recording-p*)

;;; ============================================================ the wrapper

(defun record-command (command arguments thunk)
  "Append the command to the macro being recorded, then run the rest."
  (when (and *recording-p*
             (not (member (p:command-name command) *excluded-commands*
                          :test #'string=)))
    (setf *macro*
          (append *macro* (list (cons (p:command-name command)
                                      (copy-list arguments))))))
  (funcall thunk))

(defun enable ()
  "Install the recorder.  Idempotent by identity."
  (p:add-command-wrapper #'record-command)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Uninstall the recorder.  The last macro and the named ones stay."
  (p:remove-command-wrapper #'record-command)
  (setf *enabled* nil
        *recording-p* nil)
  nil)

;;; ============================================================== commands

(r:defcommand start-macro ()
  "Begin recording commands into the macro.

Starting a new macro discards the previous un-saved one -- SAVE-MACRO is how
a sequence says it means to be kept."
  (cond
    (*recording-p* (r:notify "already recording") nil)
    (t (setf *macro* '() *recording-p* t) t)))

(r:defcommand stop-macro ()
  "Stop recording.  The macro stays ready for PLAY-MACRO."
  (cond
    ((not *recording-p*) (r:notify "not recording") nil)
    (t (setf *recording-p* nil)
       (r:notify "macro: ~d step~:p" (length *macro*))
       t)))

(r:defcommand play-macro (&optional (count "1"))
  "Play the macro back COUNT times (default once).

Plays through RUN-COMMAND like everything else, so wrappers see it; a step
that fails still counts as played, because a macro is what you did, not a
promise."
  (let ((times (parse-integer (princ-to-string count) :junk-allowed t)))
    (cond
      ((null times) (r:notify "~s is not a count" count) nil)
      ((null *macro*) (r:notify "no macro recorded") nil)
      ((> times 100) (r:notify "refusing to play more than 100 times") nil)
      (t (dotimes (i times t)
           (dolist (entry *macro*)
             (ignore-errors (apply #'p:run-command (car entry) (cdr entry)))))
         (r:notify "played ~d time~:p" times)))))

(defun macros-directory ()
  "Where named macros live: the first data directory with a macros/
subdirectory we can write."
  (let ((dir (find-if
              (lambda (root)
                (let ((macros (merge-pathnames "macros/" root)))
                  (ignore-errors
                   (ensure-directories-exist macros)
                   (probe-file macros))))
              (r:data-directories))))
    (and dir (merge-pathnames "macros/" dir))))

(defun all-macro-names ()
  "Every saved macro name, sorted.  Named macros are files, so the registry
and the directory are read together."
  (let ((dir (macros-directory)))
    (sort (append (mapcar #'car *macros*)
                  (when dir
                    (mapcar #'pathname-name
                            (directory (merge-pathnames "*.lisp" dir)))))
          #'string<)))

(r:defcommand save-macro (name)
  "Save the last recorded macro as NAME."
  (:interactive :name)
  (cond
    ((null *macro*) (r:notify "no macro recorded") nil)
    (t (setf *macros* (acons name (copy-tree *macro*)
                             (remove name *macros* :key #'car :test #'equal)))
       (let ((dir (macros-directory)))
         (when dir
           (let ((file (merge-pathnames (make-pathname :name name :type "lisp")
                                        dir)))
             (ensure-directories-exist file)
             (with-open-file (out file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
               (let ((*package* (find-package :keyword)))
                 (write (list :version 1 :entries *macro*) :stream out))))))
       name)))

(r:defcommand delete-macro (name)
  "Forget the named macro, from memory and from disk."
  (:interactive :string)
  (setf *macros* (remove name *macros* :key #'car :test #'equal))
  (let ((dir (macros-directory)))
    (when dir
      (let ((file (merge-pathnames (make-pathname :name name :type "lisp") dir)))
        (when (probe-file file) (delete-file file)))))
  name)
