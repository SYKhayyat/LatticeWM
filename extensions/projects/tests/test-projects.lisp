;;;; tests/test-projects.lisp --- The label is the binding.

(defpackage #:projects/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:prj #:projects))
  (:export #:run-all))

(in-package #:projects/tests)

(def-suite projects :description "Workspaces bound to directories.")
(in-suite projects)

(t*:register-extension-suite "PROJECTS/TESTS" "PROJECTS")

(defun run-all ()
  "Run the PROJECTS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'projects)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-projects (&body body)
  `(let ((prj::*projects* '())
         (prj::*enabled* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun temp-directory ()
  "A directory that exists, for projects to live in, as a namestring --
the form a config file would write."
  (namestring
   (ensure-directories-exist
    (merge-pathnames (format nil "prj-test-~d/" (random (expt 2 30)))
                     (uiop:temporary-directory)))))

(defun goto-workspace (index)
  "Stand the cursor in workspace INDEX, zero-based.  Workspaces are the
root's direct children, so the path is one step deep."
  (setf (c:world-cursor r:*world*)
        (c:repair-path (c:world-root r:*world*) (list index))))

;;; ================================================================ tests

(test define-project-registers-and-labels
  "Defining a project registers the name and gives it a labelled workspace.
A fresh world ships one unlabelled workspace, so the project's is the second,
at index one -- and a project you cannot switch to is only a table row, so
the workspace really is created."
  (with-projects
    (is (= 1 (c:container-count (c:world-workspaces r:*world*)))
        "a fresh world has one workspace")
    (let ((name (prj:define-project "lattice" (temp-directory))))
      (is (equal "lattice" name))
      (is (= 1 (length prj::*projects*)))
      (is-true (prj:project-directory "lattice"))
      (is (= 1 (prj:ensure-project-workspace "lattice")))
      (is (= 2 (c:container-count (c:world-workspaces r:*world*)))
          "the workspace was added")
      (is (equal "lattice"
                 (c:node-label (c:child-at (c:world-workspaces r:*world*) 1)))
          "and wears the name"))))

(test an-existing-labelled-workspace-is-adopted
  "Naming a workspace after a project makes it the project's home: the label
is the whole of the binding, so DEFINE finds nothing to create."
  (with-projects
    (let ((stack (c:world-workspaces r:*world*))
          (dir (temp-directory)))
      ;; A workspace the user made and labelled by hand.
      (c:insert-child stack 1 (c:make-leaf))
      (setf (c:node-label (c:child-at stack 1)) "mine")
      (prj:define-project "mine" dir)
      (is (= 2 (c:container-count stack)) "no third workspace appeared")
      (is (= 1 (prj:ensure-project-workspace "mine"))
          "and it is current"))))

(test define-project-is-idempotent-by-name
  "Defining the same project again moves it; no second workspace appears."
  (with-projects
    (prj:define-project "lattice" (temp-directory))
    (let ((stack (c:world-workspaces r:*world*)))
      (is (= 2 (c:container-count stack)) "one project workspace")
      (prj:define-project "lattice" (temp-directory))
      (is (= 1 (length prj::*projects*)) "still one registration")
      (is (= 2 (c:container-count stack)) "and still one workspace"))))

(test define-project-rejects-a-missing-directory
  "A project with nowhere to live is refused at define time, not at spawn
time, because a chdir that fails halfway through spawning a terminal costs
the user the terminal and teaches them nothing."
  (with-projects
    (is-false (prj:define-project "nowhere" "/no/such/directory/anywhere"))
    (is (= 0 (length prj::*projects*)))))

(test current-project-resolves-through-the-label
  "The cursor's workspace label decides which project is current, asked
anew every time -- so rearranging workspaces cannot stale the answer."
  (with-projects
    (prj:define-project "lattice" (temp-directory))
    (goto-workspace 1)
    (is (equal "lattice" (prj:current-project-name)))
    (is-true (prj:current-project-directory))
    (goto-workspace 0)
    (is-false (prj:current-project-name) "an unlabelled workspace is nobody")
    (is-false (prj:current-project-directory))))

(test spawn-wrapper-binds-the-directory
  "With the module enabled, commands run with R:*SPAWN-DIRECTORY* bound to
the current project's directory -- and to NIL off any project.  SPAWN is the
one funnel every spawn path goes through; this wrapper is where the answer
is supplied."
  (with-projects
    (let ((seen :unset))
      (prj:define-project "lattice" (temp-directory))
      (prj:enable)
      (unwind-protect
           (progn
             (goto-workspace 1)
             (funcall #'prj::spawn-directory-wrapper nil nil
                      (lambda () (setf seen r:*spawn-directory*)))
             (is (equal (namestring (prj:current-project-directory))
                        (namestring seen))
                 "the directory under the cursor")
             (goto-workspace 0)
             (funcall #'prj::spawn-directory-wrapper nil nil
                      (lambda () (setf seen r:*spawn-directory*)))
             (is (null seen) "NIL away from projects"))
        (prj:disable)))))

(test disable-stops-resolving-without-forgetting
  "DISABLE uninstalls the wrapper; the registry survives, because switching
a module off is not permission to lose what was registered."
  (with-projects
    (let ((seen :unset))
      (prj:define-project "lattice" (temp-directory))
      (prj:enable)
      (goto-workspace 1)
      (prj:disable)
      (funcall #'prj::spawn-directory-wrapper nil nil
               (lambda () (setf seen r:*spawn-directory*)))
      (is (null seen) "nothing bound after disable")
      (is-true (prj:project-directory "lattice") "registry untouched"))))

(test remove-project-forgets-but-leaves-the-workspace
  "Removing a project drops the registration; its workspace stays put --
it holds windows by now, and they were never ours to take back."
  (with-projects
    (prj:define-project "lattice" (temp-directory))
    (prj:remove-project "lattice")
    (is-false (prj:project-directory "lattice"))
    (is (= 2 (c:container-count (c:world-workspaces r:*world*))))))

(test all-project-names-sorts
  "Completion candidates come out sorted, whatever order they were defined in."
  (with-projects
    (prj:define-project "zeta" (temp-directory))
    (prj:define-project "alpha" (temp-directory))
    (is (equal '("alpha" "zeta") (prj:all-project-names)))))

(test spawned-children-start-in-the-directory
  "The whole point, end to end: SPAWN with R:*SPAWN-DIRECTORY* bound runs
the child IN that directory -- and leaves the compositor's own working
directory exactly where it was."
  (with-projects
    (let ((dir (ensure-directories-exist (temp-directory)))
          (before (uiop:getcwd)))
      ;; A child whose only act is to write its working directory down.
      (let ((r:*spawn-directory* (pathname dir)))
        (r:spawn "sh" "-c" "pwd > where-am-i"))
      (is (equal (namestring before) (namestring (uiop:getcwd)))
          "the compositor did not move")
      ;; SPAWN detaches, so the child may still be starting; give it a
      ;; moment rather than pretending the write was synchronous.
      (let ((file (merge-pathnames "where-am-i" dir)))
        (loop repeat 50
              unless (probe-file file) do (sleep 0.1)
              finally (is (probe-file file) "the child wrote something"))
        (let ((answer (with-open-file (in file) (read-line in))))
          ;; PWD says the directory without the trailing slash; compare
          ;; truenames so symlink and spelling differences cannot bite.
          (is (equal (truename dir)
                     (truename (uiop:ensure-directory-pathname answer)))
              "and the child started in the project"))))))
