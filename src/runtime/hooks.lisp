;;;; runtime/hooks.lisp --- Named hook lists.
;;;;
;;;; Hooks and the policy generics answer different questions, and confusing
;;;; them produces a bad design in both directions.
;;;;
;;;;   A GENERIC decides *what happens*.  One answer wins; CALL-NEXT-METHOD
;;;;   composes them; the return value matters.
;;;;
;;;;   A HOOK notices *that something happened*.  Every function runs; the
;;;;   return value is ignored; nobody is in charge.
;;;;
;;;; Use a hook when you want to update a status bar, log something, or start a
;;;; program.  Use a method when you want to change a decision.  If you find
;;;; yourself wanting a hook's return value, you wanted a method.

(in-package #:latticewm/runtime)

(defvar *hooks* (make-hash-table :test #'eq)
  "NAME -> (list of functions), most recently added first.")

(defvar *hook-documentation* (make-hash-table :test #'eq)
  "NAME -> docstring, for the generated extension-surface document.")

(defmacro defhook (name lambda-list documentation)
  "Declare a hook and what its functions are called with.

    (defhook :window-opened (window) \"Run after WINDOW has been placed.\")

Declaring is not required — ADD-HOOK works on any name — but an undeclared
hook does not appear in the extension surface document, which means nobody
finds it."
  (declare (ignore lambda-list))
  `(progn (setf (gethash ,name *hook-documentation*) ,documentation)
          (unless (nth-value 1 (gethash ,name *hooks*))
            (setf (gethash ,name *hooks*) '()))
          ,name))

(defun add-hook (name function &key append)
  "Add FUNCTION to hook NAME, at the front unless APPEND.

*Pass a symbol rather than #'a-function where you can.*  The list holds
whatever it is given, and a function object is a snapshot: redefining the
function afterwards leaves the hook calling the old one, and re-evaluating the
ADD-HOOK adds a second entry because the new object is not EQL to the old.
Both of those bit this project — the help overlay drew itself twice, once
through each generation of the same function, and the newer drawing lost.

A symbol has neither problem.  It is looked up when the hook runs, so
redefinition takes effect, and a second ADD-HOOK of the same symbol replaces
rather than accumulates.  An anonymous lambda has the same trouble as #' and no
way out of it, so give it a name first."
  (let ((existing (remove function (gethash name *hooks*))))
    (setf (gethash name *hooks*)
          (if append (append existing (list function)) (cons function existing))))
  function)

(defun remove-hook (name function)
  "Take FUNCTION off hook NAME."
  (setf (gethash name *hooks*) (remove function (gethash name *hooks*)))
  nil)

(defun run-hooks (name &rest arguments)
  "Call every function on hook NAME with ARGUMENTS.

Each is guarded separately: one broken hook function must not stop the others
from running, and must certainly not abort whatever the window manager was
doing when it fired."
  (dolist (function (gethash name *hooks*))
    (guarded (format nil "hook ~s" name) (apply function arguments)))
  nil)

(defun all-hooks ()
  "Every declared hook, as (NAME DOCUMENTATION COUNT), sorted by name."
  (let ((out '()))
    (maphash (lambda (name documentation)
               (push (list name documentation (length (gethash name *hooks*)))
                     out))
             *hook-documentation*)
    (sort out #'string< :key (lambda (row) (string (first row))))))

;;; The shipped hooks.  Keep this list short: a hook that nobody would attach
;;; to is a maintenance cost with no benefit, and the policy generics already
;;; cover every point where a *decision* is made.

(defhook :startup () "Run once, after the compositor connection is up and the
configuration file has been loaded, before the first layout.")

(defhook :shutdown () "Run once, on the way out, before the connection closes.
State persistence already happens without your help; this is for anything of
your own that needs flushing.")

(defhook :window-opened (window) "Run after WINDOW has been placed in the tree
or floated.  Too late to change the placement — for that, specialize
SPAWN-TARGET or ON-WINDOW-OPEN.")

(defhook :window-closed (window) "Run after WINDOW is gone and the tree has
been repaired.")

(defhook :focus-changed (old-path new-path) "Run after the cursor moves.
For status bars.  ON-FOCUS-CHANGE is the method form, and runs first.")

(defhook :layout-changed () "Run after every relayout, once the placements have
been emitted.  Fires often; keep it cheap.")

(defhook :workspace-changed (index) "Run after switching to a different
workspace.")

(defhook :draw-overlays () "Run inside every render sequence, for anything
that draws pixels of its own: the echo area, the empty-pane cursor, the help
screen, the lattice's coordinate overlay and map.

This is the seam a status bar, a notification popup or a minimap attaches to,
and it is declared here rather than left implicit because it was the one hook
in the system that nothing documented and four things used.")
