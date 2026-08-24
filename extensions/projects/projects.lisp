;;;; projects/projects.lisp --- A project is a folder; a workspace wears its name.

(in-package #:projects)

;;; ================================================================ state

(defvar *enabled* nil "True while projects are in force.")

(defvar *projects* '()
  "The registered projects, as an alist of NAME (string) to DIRECTORY.

A project is a fact about a name and a place, deliberately nothing more: no
workspace is created or owned here, because the tree belongs to the user.
The binding is looked up through workspace LABELS at the moment it is needed,
so workspaces can be rearranged, destroyed and rebuilt underneath it without
this table ever noticing.")

(defun enabled-p () "True when projects are in force." *enabled*)

;;; ============================================================== plumbing

(defun spawn-directory-wrapper (command arguments thunk)
  "Bind R:*SPAWN-DIRECTORY* to the current project's directory, if any.

Installed around every command by ENABLE.  Every path into a program --
SPAWN itself, TERMINAL, EDITOR, an unbound key on an empty pane -- funnels
through the one SPAWN command, which consults the variable; this is the
single place the answer has to be supplied.  Off the project, the variable
is bound to NIL, which is also what it is when the module is disabled: the
child starts wherever the compositor did."
  (declare (ignore command arguments))
  (let ((r:*spawn-directory* (and *enabled* (current-project-directory))))
    (funcall thunk)))

(defun enable ()
  "Start resolving spawned programs against the current project.

Idempotent: the wrapper is added by identity, so enabling twice installs
one wrapper, not two."
  (p:add-command-wrapper #'spawn-directory-wrapper)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop resolving spawns against projects.  The registry is untouched."
  (p:remove-command-wrapper #'spawn-directory-wrapper)
  (setf *enabled* nil)
  nil)

;;; ============================================================= registry

(defun all-project-names ()
  "Every registered project name, sorted."
  (sort (mapcar #'car *projects*) #'string<))

(defun define-project (name directory)
  "Register project NAME living at DIRECTORY, and give it its workspace.

NAME is a string and doubles as the workspace label.  Defining the same name
again moves the project rather than duplicating it.

The workspace is created here if no labelled one exists, because a project
you cannot switch to is only a table row; but a workspace you already
labelled NAME becomes the project's home without being asked, since the
label is the whole of the binding."
  (let ((dir (uiop:directory-exists-p
              (etypecase directory
                (string (uiop:parse-unix-namestring directory))
                (pathname directory)))))
    (cond
      ((null dir)
       (warn "project ~a: no directory ~a; not registered" name directory)
       nil)
      (t
       (setf *projects* (acons name dir (remove name *projects* :key #'car
                                               :test #'equal)))
       (ensure-project-workspace name)
       name))))

(defun remove-project (name)
  "Forget project NAME.  Its workspace, and any windows on it, stay put:
they were never ours."
  (setf *projects* (remove name *projects* :key #'car :test #'equal))
  nil)

(defun project-directory (name)
  "DIRECTORY of project NAME, or NIL."
  (cdr (assoc name *projects* :test #'equal)))

;;; ============================================================ workspaces

(defun ensure-project-workspace (name)
  "The zero-based index of the workspace labelled NAME, making one if need be.

Made at the end of the stack with whatever the policy says a workspace is
made of -- never a bare leaf at a call site -- and labelled, because the
label is the binding.  NIL when there is no workspace stack to hold one,
which inside a live world does not happen."
  (let ((stack (c:world-workspaces r:*world*)))
    (when stack
      (or (loop for i from 0 below (c:container-count stack)
                when (equal name (c:node-label (c:child-at stack i)))
                return i)
          (let ((index (c:container-count stack)))
            (c:insert-child stack index
                            (or (p:make-workspace (p:current-policy)
                                                  r:*world* index)
                                (c:make-leaf)))
            (setf (c:node-label (c:child-at stack index)) name)
            index)))))

(defun current-project-name ()
  "The project whose workspace the cursor is on, or NIL.

Resolved through the label at the moment of asking, so it answers honestly
after any amount of rearranging.  Workspaces are the root's direct children,
so the first step of the cursor path names one."
  (let ((stack (c:world-workspaces r:*world*))
        (path (c:world-cursor r:*world*)))
    (when (and stack path)
      (let ((index (first path)))
        (when (< index (c:container-count stack))
          (let ((label (c:node-label (c:child-at stack index))))
            (when label
              (car (assoc label *projects* :test #'equal)))))))))

(defun current-project-directory ()
  "DIRECTORY of the project under the cursor, or NIL off any project."
  (let ((name (current-project-name)))
    (and name (project-directory name))))

;;; ============================================================== commands

(p:define-argument-type :project "project: "
  :documentation "A registered project, with completion over the registry."
  :candidates (all-project-names))

(r:defcommand switch-to-project (name)
  "Switch to the workspace of project NAME, creating it if need be.

Switching projects switches everything at once: the windows come with the
workspace they were already on, and everything spawned from here on starts
in the project's directory."
  (:interactive :project)
  (cond
    ((null (project-directory name))
     (r:notify "no project named ~a" name))
    (t (let ((index (ensure-project-workspace name)))
         ;; The WORKSPACE command, not a hand-set cursor: it moves the
         ;; output's idea of what is displayed as well as the stack's
         ;; selection, jumps to where you last were on that workspace, and
         ;; says so.  Setting WORLD-CURSOR alone is how a second monitor
         ;; ends up mirroring the first.
         (r:workspace (1+ index))
         name))))
