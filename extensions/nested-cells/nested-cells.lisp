;;;; nested-cells/nested-cells.lisp --- A pane that owns a process.

(in-package #:nested-cells)

;;; ================================================================ state

(defvar *enabled* nil "True while the supervision timer runs.")

(defvar *cells* '()
  "The open cells, as an alist of NAME to a plist (:command :pid).

A cell is a fact about a name and a child process.  The window the child's
surfaces arrive in is placed by whatever rules are in force -- transient
rules, declared sessions, the shipped table -- because placement already
has a language and this module has nothing to add to it.")

(defun enabled-p () "True when the supervision timer runs." *enabled*)

;;; ============================================================ lifecycle

(defun cell-alive-p (name)
  "Does the child behind cell NAME still answer kill(0)?"
  (let ((entry (assoc name *cells* :test #'equal)))
    (when entry
      (let ((pid (getf (cdr entry) :pid)))
        (and pid
             (ignore-errors (sb-posix:kill pid 0) t))))))

(defun stop-child (pid)
  "Terminate a child: TERM, then KILL if it is still there."
  (ignore-errors (sb-posix:kill pid sb-posix:sigterm))
  ;; A compositor may take a moment to tear down; escalate once.
  (sleep 0.2)
  (ignore-errors (sb-posix:kill pid sb-posix:sigkill)))

(defun close-cell (name)
  "Stop the child behind cell NAME and forget the cell."
  (let ((entry (assoc name *cells* :test #'equal)))
    (when entry
      (let ((pid (getf (cdr entry) :pid)))
        (when pid (stop-child pid))
        (setf *cells* (remove name *cells* :key #'car :test #'equal))
        name))))

(defun all-cell-names ()
  "Every open cell, sorted."
  (sort (mapcar #'car *cells*) #'string<))

;;; ============================================================== commands

(defun open-cell (name command)
  "Open a nested cell called NAME running COMMAND (a list of strings).

The child inherits this session's WAYLAND_DISPLAY -- which is exactly how
the development loop has always nested river -- so its windows arrive here
as ordinary clients and ordinary placement decides where they go.

Re-opening an existing name stops the old child first: one name, one
process, no orphans."
  (close-cell name)
  ;; SPAWN rather than RUN-PROGRAM directly: same detachment, same output
  ;; discipline, one funnel.
  (let ((pid (detached-pid command)))
    (cond
      (pid
       (push (list name :command (copy-list command) :pid pid) *cells*)
       (r:logmsg :info "cell ~a opened (~{~a~^ ~})" name command)
       name)
      (t (r:notify "failed to start ~a" (first command)) nil))))

(r:defcommand open-cell-command (command-line)
  "Open a nested cell running COMMAND-LINE.

The cell is named after the command's first word, which keeps one name per
process without asking anybody to invent two strings for one thing."
  (:interactive :shell-command)
  (let ((argv (uiop:split-string (string-trim " " command-line))))
    (cond
      ((null argv) (r:notify "nothing to run") nil)
      (t (open-cell (first argv) argv)))))

(defun detached-pid (command)
  "Spawn COMMAND detached and return its pid, or NIL.

SBCL's run-program does not hand back the pid through its API portably, but
the PROCESS object carries it; we keep only what supervision needs."
  (ignore-errors
   (let ((process (sb-ext:run-program (first command) (rest command)
                                      :search t :wait nil
                                      :output nil :error nil :input nil)))
     (when process
       (sb-ext:process-pid process)))))

(r:defcommand close-cell-command (name)
  "Stop the child behind cell NAME and forget the cell."
  (:interactive :string)
  (cond
    ((null (assoc name *cells* :test #'equal))
     (r:notify "no cell named ~a" name) nil)
    (t (close-cell name))))

(defun check-cells ()
  "Report cells whose child died without being asked.

Called from the supervision timer.  Death is not an ERROR -- compositors
crash -- but it is worth saying out loud, once, and then the dead cell is
forgotten because there is nothing left to supervise."
  (dolist (entry *cells*)
    (destructuring-bind (name . plist) entry
      (declare (ignore plist))
      (unless (cell-alive-p name)
        (setf *cells* (remove name *cells* :key #'car :test #'equal))
        (r:notify "cell ~a exited" name)
        (return)))))

;;; =============================================================== plumbing

#+sbcl
(defparameter *supervision-interval-seconds* 10
  "How often CHECK-CELLS runs.")

(defun enable ()
  "Start supervising cells.  Idempotent by timer name."
  (r:add-timer "nested-cells-supervision"
               *supervision-interval-seconds*
               'check-cells)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop supervising.  Open cells keep running as children of the session --
DISABLE is not permission to kill anybody's desktop."
  (r:remove-timer "nested-cells-supervision")
  (setf *enabled* nil)
  nil)
