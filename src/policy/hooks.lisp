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
;;;;
;;;; THE THIRD MECHANISM IS THE OPTION, and it is not a third answer to the
;;;; same question -- it is what a generic's shipped method returns.  Change
;;;; the option to change the shipped answer; write the method to change the
;;;; decision; a policy that overrides the method stops reading the option.
;;;; That relationship used to live only in prose, which is why *SMART-GAPS*
;;;; could be a documented value wired to nothing.  It is data now:
;;;; OPTION-READERS answers it from the compiled image, the generated
;;;; extension surface prints it under every option, and gate 11 fails the
;;;; build on an option no method and no function reads.
;;;;
;;;; INCLUDING THE LAST CLAUSE, WHICH STAYED PROSE ONE COMMIT LONGER THAN THE
;;;; REST OF THE SENTENCE.  "A policy that overrides the method stops reading
;;;; the option" was true of *NEW-WORKSPACE* the whole time the lattice has
;;;; existed, and no instrument in the project could see it: the reader is
;;;; still there, gate 11 still finds it, and it simply never runs.
;;;; OPTION-SHADOWS asks the method list who wins over a reader, the surface
;;;; prints that under the option too, and gate 15 requires an override that
;;;; wins everywhere to compose or to ship an option of its own.  The rule is
;;;; a trade, not a ban: the lattice takes *NEW-WORKSPACE* and gives
;;;; *NEW-WORKSPACE-CELLS*, so the decision stays tier-0 for a user who will
;;;; never write a DEFMETHOD.
;;;;
;;;; AND THE HOOK WAS THE ONE OF THE THREE WITH NOTHING ASKING THAT QUESTION.
;;;; Fourteen of the seventeen declared hooks had never been attached to by
;;;; anything -- not by the lattice, not by the four worked examples, not by a
;;;; test.  Gate 7 asked whether a declared name is also a run name, which is a
;;;; question about two greps rather than about the program, and gate 2 asked
;;;; whether each has a docstring, which DEFHOOK makes mandatory anyway.
;;;;
;;;; A HOOK'S CONTRACT IS NOT ITS NAME.  It is what the functions are called
;;;; with and the moment they are called, and neither of those had ever been
;;;; executed by a consumer.  Three of them were wrong:
;;;;
;;;;   * :STARTUP ran before the compositor connection existed, which its own
;;;;     docstring denied and :LAYOUT-RESTORED's docstring, twelve declarations
;;;;     below it in this file, contradicted;
;;;;   * :OUTPUT-ADDED ran at the moment the object was made, before the
;;;;     monitor's name, position, size or scale had arrived -- the four facts
;;;;     "a surface or a process per screen" needs;
;;;;   * :FOCUS-CHANGED, documented "run after the cursor moves", was skipped
;;;;     by two of the five places that move the cursor, one of them being
;;;;     closing a window.
;;;;
;;;; So the declaration is load-bearing now.  DEFHOOK's lambda list used to be
;;;; DECLARE IGNOREd; it is recorded, ADD-HOOK checks the function you attach
;;;; against it, a compiler macro checks every RUN-HOOKS call site against it
;;;; at compile time, and gate 14 requires every declared hook to be attached
;;;; to and watched firing by the unit suite or the integration run.  There is
;;;; also a generated document now -- `latticewm --hooks', doc/HOOKS.txt --
;;;; because this was the only extension mechanism whose surface was a list
;;;; somebody typed by hand, and it was four names short.

(in-package #:latticewm/policy)

(defvar *hooks* (make-hash-table :test #'eq)
  "NAME -> (list of functions), most recently added first.")

(defvar *hook-documentation* (make-hash-table :test #'eq)
  "NAME -> docstring, for the generated hook-surface document.")

(defvar *hook-arguments* (make-hash-table :test #'eq)
  "NAME -> the declared lambda list, as a list of symbols.

THIS USED TO BE THROWN AWAY.  DEFHOOK took a lambda list and began
(DECLARE (IGNORE LAMBDA-LIST)), so the one statement of what a hook function
is called with was a comment that happened to be parenthesised: nothing
compared it to the RUN-HOOKS call sites, nothing compared it to the function
you attached, and a mismatch in either direction reached you as a hook that
silently did nothing.  RUN-HOOKS guards each function separately -- rightly,
since one broken hook must not stop the others -- so a function of the wrong
arity signals once per fire, is caught, contributes NIL, and leaves a log line
in a file nobody has open.")

(defmacro defhook (name lambda-list documentation)
  "Declare a hook and what its functions are called with.

    (defhook :window-opened (window) \"Run after WINDOW has been placed.\")

LAMBDA-LIST is checked, in both directions and at the moment each mistake is
made: ADD-HOOK complains about a function that cannot accept these arguments,
and a compiler macro on RUN-HOOKS fails gate 1 on a call site that does not
pass them.

Declaring is not required — ADD-HOOK works on any name — but an undeclared
hook appears in no generated document, which means nobody finds it."
  `(progn (setf (gethash ,name *hook-documentation*) ,documentation
                (gethash ,name *hook-arguments*) ',lambda-list)
          (unless (nth-value 1 (gethash ,name *hooks*))
            (setf (gethash ,name *hooks*) '()))
          ,name))

(defun hook-arguments (name)
  "The declared lambda list of hook NAME, and whether it was declared at all."
  (gethash name *hook-arguments*))

(defun lambda-list-arity (lambda-list)
  "(MIN . MAX) arguments LAMBDA-LIST accepts.  MAX is NIL when unbounded.

Compared by symbol *name* rather than by identity, because this is asked of
lambda lists SB-INTROSPECT reconstructed as well as of ones written here, and
those arrive with whatever package their &-markers were interned in."
  (let ((min 0) (max 0) (optional nil))
    (dolist (item lambda-list (cons min max))
      (if (and (symbolp item) (plusp (length (symbol-name item)))
               (char= #\& (char (symbol-name item) 0)))
          (cond ((string= item '#:&optional) (setf optional t))
                ((string= item '#:&aux) (return (cons min max)))
                (t (return (cons min nil))))
          (if optional (incf max) (progn (incf min) (incf max)))))))

(defun accepted-arity (function)
  "(MIN . MAX) arguments FUNCTION accepts, or NIL when it cannot be told.

Asked of the compiler's FUNCTION type rather than of the lambda list, because
the lambda list of a function with no debug information is NIL, and NIL is
also the lambda list of a function of no arguments -- so the one shape that
must not produce a complaint would have produced one.  A &REST function types
as (FUNCTION * ...), which is the same answer as `no idea', and both mean say
nothing: this check exists to catch a definite mistake, and a check that
guesses is one people learn to ignore."
  (let ((object (cond ((functionp function) function)
                      ((and (symbolp function) (fboundp function))
                       (symbol-function function)))))
    (when object
      (let ((type (ignore-errors (sb-introspect:function-type object))))
        (when (and (consp type) (eq (first type) 'function))
          (let ((arguments (second type)))
            (cond ((eq arguments '*) nil)
                  ((null arguments) (cons 0 0))
                  ((listp arguments) (lambda-list-arity arguments)))))))))

(defun arity-accepts-p (arity count)
  "Can something of ARITY -- (MIN . MAX), or NIL for unknown -- take COUNT?"
  (or (null arity)
      (and (<= (car arity) count)
           (or (null (cdr arity)) (<= count (cdr arity))))))

(defun arity-covers-p (function-arity hook-arity)
  "Can a function of FUNCTION-ARITY be called with every count HOOK-ARITY passes?

Both are (MIN . MAX) with MAX NIL for unbounded, or NIL for `cannot be told' --
in which case say yes, because this check exists to catch a definite mistake
and one that guesses is one people learn to ignore."
  (or (null function-arity) (null hook-arity)
      (destructuring-bind (fmin . fmax) function-arity
        (destructuring-bind (hmin . hmax) hook-arity
          (and (<= fmin hmin)
               (or (null fmax) (and hmax (>= fmax hmax))))))))

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
  ;; THE OTHER HALF OF THE SAME MISTAKE, and unconditional where the one above
  ;; is optional: attaching to a name nobody declared is something an extension
  ;; is entitled to do, and attaching a function that cannot be called with the
  ;; arguments the declaration promises never is.  It arrives as a hook that
  ;; does nothing either way, and this one would otherwise arrive as a log line
  ;; per fire, from inside GUARDED, saying `invalid number of arguments'.
  (multiple-value-bind (arguments declared) (hook-arguments name)
    (when declared
      ;; LAMBDA-LIST-ARITY, not (LENGTH ARGUMENTS): a hook declared with an
      ;; &optional or &rest tail is run with a *range* of counts, and counting
      ;; the raw list length counted the &-markers and the optionals as though
      ;; they were always passed -- which flagged a perfectly good function on
      ;; the first hook that had such a tail.
      (let ((wanted (lambda-list-arity arguments))
            (arity (accepted-arity function)))
        (unless (arity-covers-p arity wanted)
          (logmsg :warn "~s cannot be called the way hook ~s runs it ~
                         (~{~(~a~)~^ ~}), so it will never run."
                  function name arguments)))))
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

THE RETURN VALUE IS FOR THE ACCUMULATING HOOKS AND NOTHING ELSE.  A hook
notices that something happened and its answer is normally meaningless — if you
find yourself wanting one function's return value, you wanted a method.  But a
handful of hooks are genuinely additive: two status bars should each get their
strip of the screen, and the second must not have to know about the first.
Those read this list.  Everything else ignores it, which is free."
  (let ((out '()))
    (dolist (function (gethash name *hooks*) (nreverse out))
      (push (guarded (format nil "hook ~s" name) (apply function arguments))
            out))))

(define-compiler-macro run-hooks (&whole form name &rest arguments)
  "Check a literal call site against the declaration, at compile time.

Every RUN-HOOKS in the tree names its hook with a literal keyword, so this
sees all of them, and gate 1 -- zero compiler warnings, from our files only --
is what turns the warning into a failed build.  The declaration is in the
image by now because the system is :SERIAL: this file is compiled and *loaded*
before the first file that runs a hook is compiled.

Nothing is transformed.  The form is returned as it arrived; the value of a
compiler macro here is the compile-time look, not the code it emits."
  (when (keywordp name)
    (multiple-value-bind (declared present) (hook-arguments name)
      (when (and present (/= (length declared) (length arguments)))
        (warn "~s is declared with ~d argument~:p (~{~(~a~)~^ ~}) and run with ~
               ~d here.  One of the two is wrong, and a hook function written ~
               against the declaration will signal on every fire."
              name (length declared) declared (length arguments)))))
  form)

(defun all-hooks ()
  "Every declared hook, as (NAME DOCUMENTATION COUNT ARGUMENTS), by name."
  (let ((out '()))
    (maphash (lambda (name documentation)
               (push (list name documentation (length (gethash name *hooks*))
                           (gethash name *hook-arguments*))
                     out))
             *hook-documentation*)
    (sort out #'string< :key (lambda (row) (string (first row))))))

;;; The shipped hooks.  Keep this list short: a hook that nobody would attach
;;; to is a maintenance cost with no benefit, and the policy generics already
;;; cover every point where a *decision* is made.

(defhook :startup () "Run once, after the compositor connection is up and the
configuration file has been loaded, before the first layout.

THAT SENTENCE WAS FALSE FOR THE LIFE OF THE PROGRAM.  The call sat above
CONNECT-TO-COMPOSITOR, so a hook here ran against a world with no display, no
outputs, no seats and no windows — and this file said so twelve declarations
below, where :LAYOUT-RESTORED describes the startup order as \"run :STARTUP,
connect\".  Two docstrings in one file disagreeing about when a hook fires is
what a mechanism with no consumer looks like from the inside.

It also could not be paired.  A failure to connect returned out of START before
the UNWIND-PROTECT existed, so :STARTUP had run and :SHUTDOWN never would —
which for the one thing a startup hook reliably does, acquiring something,
means leaking it on the one path where that is least recoverable.  The call is
inside the UNWIND-PROTECT now, so the pairing is structural rather than
remembered.")

(defhook :shutdown () "Run once, on the way out, before the connection closes.
State persistence already happens without your help; this is for anything of
your own that needs flushing.")

(defhook :window-opened (window) "Run after WINDOW has been placed in the tree
or floated.  Too late to change the placement — for that, specialize
SPAWN-TARGET or ON-WINDOW-OPEN.")

(defhook :window-closed (window) "Run after WINDOW is gone and the tree has
been repaired.")

(defhook :focus-changed (old-path new-path) "Run after the cursor moves.
ON-FOCUS-CHANGE is the method form, and runs first.

*This is the cursor, which is a place.*  It is what the highlight is around and
what a directional key moves.  It is not always what has the keyboard: focusing
a floating window does not move the cursor, so a status bar showing the focused
*window* wants :KEYBOARD-FOCUS-CHANGED, which is derived and fires for both.

It used to be skipped by two of the five places that move the cursor —
ON-WINDOW-CLOSE, which is the commonest focus change there is, and the
lattice's ENABLE-IN, which repaths every cursor in the world.  Both wrote the
slot directly.  Both go through REPAIR-CURSOR now, which is REPAIR-PATH plus
this hook and existed the whole time.")

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
the configuration, connect, run :STARTUP, then restore the saved layout — so
anything the configuration *or* a startup hook did to the tree is replaced
wholesale by whatever was on disk.  A policy that requires a shape, as the
lattice does, has to be given a chance to re-establish it, and this is that
chance.

Without it, enabling the lattice in a configuration file wrapped an empty
world in a plane and then the restored layout quietly threw the plane away —
so the policy said `lattice' and every cell command answered NIL.")

(defhook :keyboard-focus-changed (old-window new-window) "Run when the window
holding the keyboard changes.  Either may be NIL.

*The derived half of D18, and the one a status bar wants.*  Focus is a place;
which window that place gives the keyboard to is P:FOCUS-TARGET's answer, and
the runtime asks it once per manage sequence and sends river a focus request
only when the answer changed.  This fires from that same comparison, so it
fires exactly when focus really moved and never when it merely could have.

It covers what :FOCUS-CHANGED structurally cannot: clicking a dialog, cycling
floats, calling up a named scratchpad, and a window closing while the cursor
stays where it was.  NEW-WINDOW is NIL for an empty pane — D18's honest answer,
not a missing value — and also while a screen locker holds the keyboard, since
for as long as that lasts every focus request we make is ignored.")

(defhook :output-added (output) "Run once a monitor has appeared, been given a
workspace, and said what it is.  For anything that needs a surface or a process
per screen.

*Deliberately not the moment the object was made*, which is where this used to
fire.  river_output_v1 arrives empty: the position, the size, and the wl_output
that carries the name and the scale all come in events that follow, so a hook
run at creation was handed a nameless monitor of no size — and a surface per
screen needs the size and the scale, while a process per screen is almost
always keyed on the name.  It is drained in the manage sequence instead, beside
the windows, which is the same answer PLACE-UNPLACED-WINDOWS gives to the same
question: at the moment `window' arrives we know nothing about the window.")

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
