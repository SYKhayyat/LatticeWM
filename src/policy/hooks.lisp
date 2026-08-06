;;;; policy/hooks.lisp --- Named hook lists.
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

(in-package #:latticewm/policy)

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

(define-option *warn-on-undeclared-hooks* t
  "Complain when ADD-HOOK is given a name no DEFHOOK declared.

An undeclared hook is not an error -- ADD-HOOK works on any name, and an
extension is entitled to invent its own and run it itself.  It is, however,
the single easiest way to write a line of configuration that does nothing at
all, silently, forever.

That happened while writing the hardware check for this project: it hung its
output recording on :RELAYOUT, which is not a hook this system runs.  The
config loaded, the function was never called, and the report was empty with no
indication why.  On a tty, with no editor, that costs the trip.

The check is a hash lookup on a path taken a handful of times at startup, so
it is free in any sense that matters.  Set this to NIL if you run your own
hooks and would rather not hear about it.")

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
  (when (and *warn-on-undeclared-hooks*
             (not (nth-value 1 (gethash name *hook-documentation*))))
    (logmsg :warn "~s is not a declared hook, so nothing will ever run it.~%~
                   Declared hooks are: ~{~s~^ ~}"
            name (mapcar #'first (all-hooks))))
  (let ((existing (remove function (gethash name *hooks*))))
    (setf (gethash name *hooks*)
          (if append (append existing (list function)) (cons function existing))))
  function)

(defun remove-hook (name function)
  "Take FUNCTION off hook NAME."
  (setf (gethash name *hooks*) (remove function (gethash name *hooks*)))
  nil)

(defun run-hooks (name &rest arguments)
  "Call every function on hook NAME with ARGUMENTS, and return their answers.

Each is guarded separately: one broken hook function must not stop the others
from running, and must certainly not abort whatever the window manager was
doing when it fired.  A function that signalled contributes NIL.

THE RETURN VALUE IS FOR THE *ACCUMULATING* HOOKS AND NOTHING ELSE.  A hook
notices that something happened and its answer is normally meaningless — if you
find yourself wanting one function's return value, you wanted a method.  But a
handful of hooks are genuinely additive: two status bars should each get their
strip of the screen, and the second must not have to know about the first.
Those read this list.  Everything else ignores it, which is free."
  (let ((out '()))
    (dolist (function (gethash name *hooks*) (nreverse out))
      (push (guarded (format nil "hook ~s" name) (apply function arguments))
            out))))

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

(defhook :reserve-space (output) "Run to ask how much of OUTPUT to keep clear.

*The one hook whose return value matters.*  Each function answers
(TOP RIGHT BOTTOM LEFT) in pixels, and the answers are added together — so two
status bars each get their strip and the second does not have to know about the
first.  Anything else, including NIL, contributes nothing.

    (defun my-bar-edges (output)
      (declare (ignore output))
      (list 28 0 0 0))
    (add-hook :reserve-space 'my-bar-edges)

THIS USED TO BE A SPECIAL VARIABLE OUTSIDE THE HOOK MECHANISM ENTIRELY --
P:*RESERVE-HOOKS*, which you pushed a symbol onto -- so the program had two ways
to hook, one documented and gated and one neither.  A user reading the
extension guide could not find the second, and gate 7 could not see it.  There
is one way now, and this is it.")

(defhook :layout-restored () "Run after a saved layout has replaced the tree.

*The hook a policy with a shape needs.*  Startup order is: make a world, load
the configuration, run :STARTUP, connect, then restore the saved layout — so
anything the configuration did to the *tree* is replaced wholesale by whatever
was on disk.  A policy that requires a shape, as the lattice does, has to be
given a chance to re-establish it, and this is that chance.

Without it, enabling the lattice in a configuration file wrapped an empty
world in a plane and then the restored layout quietly threw the plane away —
so the policy said `lattice' and every cell command answered NIL.")

(defhook :output-added (output) "Run after a monitor has appeared and been
given a workspace.  For anything that needs a surface or a process per screen.")

(defhook :output-removed (output) "Run after a monitor has gone away and its
overlays have been released.  The output object is still readable; its proxy is
already NIL.")

(defhook :pointer-op (kind window) "Run when an interactive pointer operation
starts or ends.  KIND is :MOVE, :RESIZE or NIL for the end of one.")

(defhook :input-added (device) "Run when an input device is announced, before
anything has been configured on it.  The device knows its own proxy and little
else at this point -- its name and its kind arrive in the events that follow --
so a hook that wants to know what it is should look at :KEYBOARD-LAYOUT-CHANGED
or ask again from a later hook.")

(defhook :input-removed (device) "Run after an input device is unplugged and
forgotten.  The device object is still readable; every proxy on it is stale.")

(defhook :capture-changed (subject count) "Run when the number of screen
capture sessions on a window or an output changes.  SUBJECT is a C:WINDOW or a
C:OUTPUT and COUNT is how many sessions are now recording it, zero included.

For a recording indicator of your own — a red dot on a status bar, a light on a
keyboard, a script that mutes a microphone — and for the case the shipped
indicator deliberately does not cover: reacting to it rather than displaying it.

Fires once per subject at startup as well, with whatever count river reports on
creation, so a hook that draws something does not have to wait for the first
change to learn the state.")

(defhook :keyboard-layout-changed (device name) "Run when a keyboard's active
layout changes, whether we asked for it or the user pressed the xkb toggle.
NAME is the layout's own name, e.g. \"German\".  For a status bar showing which
layout is live, which is the one thing a two-layout user needs on screen.")
