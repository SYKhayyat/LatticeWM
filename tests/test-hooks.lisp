;;;; tests/test-hooks.lisp --- The third extension mechanism, pulled.
;;;;
;;;; A generic decides, an option supplies a shipped answer, and a hook
;;;; notices.  The first two had a check asking whether they were connected to
;;;; the program — gate 6 counts the generics with a specializing method from
;;;; outside src/, gate 11 fails the build on an option nothing reads.  The
;;;; third had gate 7, which compares the set of declared names with the set of
;;;; run names, and both of those are written by the same hand on the same
;;;; afternoon.
;;;;
;;;; So fourteen of the seventeen hooks had never been attached to by anything
;;;; at all, and a hook's contract is not its name — it is the arguments its
;;;; functions are called with and the moment they are called.  Neither had
;;;; ever been executed for any of the fourteen, and three were wrong.
;;;;
;;;; WHAT IS HERE AND WHAT IS NOT.  A hook fires from where the thing it
;;;; notices happens, so which suite can reach it is decided by what has to
;;;; exist for that to be true.  :FOCUS-CHANGED needs a world and a policy, so
;;;; it is here.  :WINDOW-OPENED needs a compositor to open a window, so it is
;;;; in tools/integration.lisp.  :INPUT-ADDED needs a keyboard to be plugged
;;;; in, which a headless backend has none of, so it is in
;;;; tools/hardware-check.lisp.  Gate 14 requires every declared hook to be
;;;; attached to in one of the three.

(in-package #:latticewm/tests)
(in-suite hooks)

(defvar *fired* '()
  "(NAME . ARGUMENTS) for every hook a test watched, most recent first.")

(defun note-hook (name)
  "A recorder for hook NAME: a symbol, so ADD-HOOK's own advice is followed."
  ;; A closure would be a fresh object each time this is called, so REMOVE-HOOK
  ;; could not find the one that was added and ADD-HOOK could not replace it —
  ;; which is the accumulation bug ADD-HOOK's docstring describes, reproduced
  ;; by the file that is supposed to be checking the mechanism.  Interning one
  ;; named function per hook avoids it and costs nothing.
  (let ((symbol (intern (format nil "NOTE-HOOK-~a" name) '#:latticewm/tests)))
    (unless (fboundp symbol)
      (setf (symbol-function symbol)
            (lambda (&rest arguments) (push (cons name arguments) *fired*) nil)))
    symbol))

(defmacro watching ((&rest names) &body body)
  "Run BODY with a recorder on each of NAMES, and nothing left behind."
  `(let ((*fired* '()))
     (dolist (name ',names) (p:add-hook name (note-hook name)))
     (unwind-protect (progn ,@body)
       (dolist (name ',names) (p:remove-hook name (note-hook name))))))

(defun firings (name)
  "Every argument list hook NAME was run with, oldest first."
  (loop for (fired . arguments) in (reverse *fired*)
        when (eq fired name) collect arguments))

(defmacro complaint (&body body)
  "Run BODY and return everything the log said, as one string.

To stderr and to a string of our own rather than to the log file, because a
test that asserts on a warning must not depend on whether the machine running
it has a writable state directory."
  `(let ((p:*log-file* nil)
         (p:*log-to-stderr* t)
         (p:*log-level* :warn)
         (p:*log-stream* (make-string-output-stream)))
     (progn ,@body)
     (get-output-stream-string p:*log-stream*)))

;;; ------------------------------------------- the declaration is the contract

(test every-declared-hook-says-what-it-passes-and-why
  ;; DEFHOOK took a lambda list and began (DECLARE (IGNORE LAMBDA-LIST)), so
  ;; the one statement of what a hook function receives was a comment that
  ;; happened to be parenthesised.  It is recorded now, and everything below
  ;; this line is a consequence of that.
  (dolist (row (p:all-hooks))
    (destructuring-bind (name documentation attached arguments) row
      (declare (ignore attached))
      (is (stringp documentation) "~s has a docstring" name)
      (is (listp arguments) "~s records what it passes" name)
      (is (every #'symbolp arguments)
          "~s's argument list is names, not types" name)
      (is (eq (nth-value 1 (p:hook-arguments name)) t)
          "~s is findable by name" name))))

(test the-arity-of-a-lambda-list-is-a-range
  (is (equal '(2 . 2) (p:lambda-list-arity '(a b))))
  (is (equal '(0 . 0) (p:lambda-list-arity '())))
  (is (equal '(1 . 2) (p:lambda-list-arity '(a &optional b))))
  (is (equal '(1 . nil) (p:lambda-list-arity '(a &rest more))))
  (is (equal '(1 . nil) (p:lambda-list-arity '(a &key b))))
  ;; &AUX is not a parameter, and reading it as an unbounded tail would make
  ;; every function with a local binding look like it accepts anything.
  (is (equal '(1 . 1) (p:lambda-list-arity '(a &aux b)))))

(defun takes-two-for-test (a b) (list a b))
(defun takes-none-for-test () nil)
(defun takes-anything-for-test (&rest arguments) arguments)

(test a-function-that-cannot-be-called-is-not-a-mystery
  (is (equal '(2 . 2) (p:accepted-arity 'takes-two-for-test)))
  (is (equal '(0 . 0) (p:accepted-arity 'takes-none-for-test)))
  ;; &REST types as (FUNCTION * ...), which is also what `no debug information'
  ;; looks like — so the answer is `cannot tell', and a check that guesses is
  ;; one people learn to ignore.
  (is (null (p:accepted-arity 'takes-anything-for-test)))
  (is (null (p:accepted-arity 'no-such-function-anywhere)))
  (is-true (p:arity-accepts-p '(1 . 2) 2))
  (is-false (p:arity-accepts-p '(1 . 2) 3))
  (is-true (p:arity-accepts-p nil 7) "unknown accepts anything"))

(test attaching-a-function-of-the-wrong-shape-says-so
  ;; VERIFIED BY BREAKING IT.  :WINDOW-OPENED is run with one argument; a
  ;; function of none signals on every fire, inside the guard that keeps one
  ;; broken hook from stopping the others, so what reaches the user is a hook
  ;; that does nothing and a log line in a file they do not have open.
  (let ((said (complaint
                (unwind-protect
                     (p:add-hook :window-opened 'takes-none-for-test)
                  (p:remove-hook :window-opened 'takes-none-for-test)))))
    (is-true (search "never run" said)
             "attaching a nullary function to a hook of one argument complains"))
  ;; And the shape that must not complain, which is the reason ACCEPTED-ARITY
  ;; asks the compiler's function type rather than the lambda list: the lambda
  ;; list of a function of no arguments is NIL, and so is the lambda list of a
  ;; function nothing is known about.
  (let ((said (complaint
                (unwind-protect
                     (p:add-hook :layout-changed 'takes-none-for-test)
                  (p:remove-hook :layout-changed 'takes-none-for-test)))))
    (is-false (search "never run" said)
              "a nullary function on a hook of no arguments is silent")))

(test a-run-hooks-that-passes-the-wrong-number-fails-the-build
  ;; The other direction, and the one gate 1 enforces: a compiler macro on
  ;; RUN-HOOKS compares each literal call site with the declaration.  Every
  ;; RUN-HOOKS in the tree names its hook with a literal keyword, so it sees
  ;; all of them.
  (let ((warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned (princ-to-string condition))
                              (muffle-warning condition))))
      (compile nil '(lambda () (p:run-hooks :window-opened))))
    (is-true warned ":window-opened takes one argument and this passes none")
    (is-true (and warned (search "declared" warned))))
  (let ((warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (compile nil '(lambda (w) (p:run-hooks :window-opened w))))
    (is-false warned "and the correct call site is silent")))

;;; ------------------------------------------------ the cursor is one funnel

(test moving-the-cursor-runs-the-hook-every-way-it-can-move
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (watching (:focus-changed)
      (p:jump-cursor pol world '(0 0))
      (p:move-cursor pol world :right)
      (is (= 2 (length (firings :focus-changed)))
          "a jump and a directional move each announce themselves")
      (dolist (arguments (firings :focus-changed))
        (is (= 2 (length arguments)) "with the two paths it declares")))))

(test closing-a-window-announces-where-focus-went
  ;; THE COMMONEST FOCUS CHANGE THERE IS, and the hook documented "run after
  ;; the cursor moves" did not run for it.  ON-WINDOW-CLOSE wrote WORLD-CURSOR
  ;; directly, so it skipped both halves of the notification: the
  ;; ON-FOCUS-CHANGE method, which is what :MRU consults, and this hook, which
  ;; is what a status bar consults.
  (let ((world (fresh-world)) (pol (policy)) (a (win "a")))
    (p:on-window-open pol world a)
    (p:on-window-open pol world (win "b"))
    (let ((path (c:node-path-to (c:world-root world)
                                (c:leaf-holding (c:world-root world) a))))
      (p:jump-cursor pol world path)
      (watching (:focus-changed)
        (p:on-window-close pol world a path)
        (let ((firings (firings :focus-changed)))
          (is (= 1 (length firings))
              "closing the window under the cursor is a focus change")
          (when firings
            (is (equal (c:world-cursor world) (second (first firings)))
                "and the new path it reports is where the cursor now is")))))))

(test a-cursor-that-does-not-move-says-nothing
  ;; The other half of the same rule.  REPAIR-CURSOR is called by every verb
  ;; that restructures the tree, and most of the time the cursor is already
  ;; where it belongs — a status bar redrawing on every relayout is the reason
  ;; :LAYOUT-CHANGED's docstring has to say "fires often; keep it cheap", and
  ;; the focus hook must not join it.
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (watching (:focus-changed)
      (p:repair-cursor pol world)
      (p:repair-cursor pol world)
      (is (null (firings :focus-changed))))))

;;; -------------------------------------------------------- the one that sums

(defun reserve-a-for-test (output) (declare (ignore output)) (list 28 0 0 0))
(defun reserve-b-for-test (output) (declare (ignore output)) (list 4 0 10 0))

(test reserve-space-adds-the-answers-up
  ;; The exception that proves the rule: every other hook's return value is
  ;; meaningless, and this one's is added together so that two status bars each
  ;; get their strip without the second having to know about the first.
  (let ((output (make-instance 'c:output :rect (c:make-rect 0 0 800 600)))
        (pol (policy)))
    (p:add-hook :reserve-space 'reserve-a-for-test)
    (p:add-hook :reserve-space 'reserve-b-for-test)
    (unwind-protect
         (is (equal '(32 0 10 0) (p:reserved-space pol output))
             "two panels, one screen, neither aware of the other")
      (p:remove-hook :reserve-space 'reserve-a-for-test)
      (p:remove-hook :reserve-space 'reserve-b-for-test))))

(defun breaking-hook-for-test (&rest ignored)
  (declare (ignore ignored))
  (error "a hook function that signals"))

(test a-hook-that-signals-does-not-take-the-others-with-it
  ;; RUN-HOOKS guards each function separately, which is right and is also why
  ;; an arity mistake is silent — hence the two checks above.
  (p:add-hook :window-opened 'breaking-hook-for-test)
  (unwind-protect
       (watching (:window-opened)
         (complaint (p:run-hooks :window-opened (win "a")))
         (is (= 1 (length (firings :window-opened)))
             "the recorder ran even though the hook beside it signalled"))
    (p:remove-hook :window-opened 'breaking-hook-for-test)))

;;; ---------------------------------------------- the document, versus the code

(test the-generated-hook-document-names-every-hook
  ;; The check that compares two independently-maintained artifacts, which is
  ;; the shape of every useful check in this project.  Until this document
  ;; existed the hooks were the one extension mechanism whose surface was a
  ;; list somebody typed into doc/EXTENDING.org — thirteen names against the
  ;; program's seventeen, missing :LAYOUT-RESTORED, which is the hook the
  ;; flagship extension itself uses.
  (let ((printed (with-output-to-string (out) (p:print-hook-surface out))))
    (dolist (row (p:all-hooks))
      (is-true (search (format nil "~s" (first row)) printed)
               "~s is in the generated document" (first row)))
    (is-true (search (format nil "~d hooks" (length (p:all-hooks))) printed)
             "and it counts them from the image rather than from a sentence")))

;;; ------------------------------------------- advice, and the way back off it

;; THE THIRD MECHANISM'S THIRD SHAPE, AND IT HAD NO TEST AT ALL.  A generic
;; decides, an option supplies an answer, a hook notices — and a *command
;; wrapper* runs around the whole of a command and can refuse it.  Layout undo
;; is built on it and nothing else, and until this the only thing that had ever
;; exercised it was layout undo.
;;
;; REMOVE-COMMAND-WRAPPER is why this is here.  It was exported, defined, and
;; reached by nothing: no caller, no test, and doc/EXTENDING.org taught
;; ADD-COMMAND-WRAPPER without ever saying how to take one off.  A wrapper runs
;; around *every* command, so the half-written one is the one that stops the
;; desktop responding to keys, and the off switch is the only way back short of
;; a restart.  Gate 16 is what noticed it was unreachable.

(defvar *wrapper-marks* '()
  "What ran, innermost last, for the wrapper test below.")

(p:defcommand command-for-wrapper-test ()
  "Exists so that the wrapper chain has something to run around."
  (push :command *wrapper-marks*)
  :ran)

(defun outer-wrapper-for-test (command arguments thunk)
  (declare (ignore command arguments))
  (push :outer *wrapper-marks*)
  (funcall thunk))

(defun inner-wrapper-for-test (command arguments thunk)
  (declare (ignore command arguments))
  (push :inner *wrapper-marks*)
  (funcall thunk))

(test a-command-wrapper-composes-and-comes-off-again
  (let ((p::*command-wrappers* '()))
    (p:add-command-wrapper 'inner-wrapper-for-test)
    (p:add-command-wrapper 'outer-wrapper-for-test)
    (let ((*wrapper-marks* '()))
      (is (eq :ran (p:run-command "command-for-wrapper-test"))
          "what the command returned is what RUN-COMMAND returns")
      (is (equal '(:outer :inner :command) (reverse *wrapper-marks*))
          "added later runs outside, which is what the docstring promises"))
    ;; The half this exists for.
    (p:remove-command-wrapper 'outer-wrapper-for-test)
    (let ((*wrapper-marks* '()))
      (p:run-command "command-for-wrapper-test")
      (is (equal '(:inner :command) (reverse *wrapper-marks*))
          "the removed wrapper is gone and the one beside it is untouched"))
    (p:remove-command-wrapper 'inner-wrapper-for-test)
    (let ((*wrapper-marks* '()))
      (p:run-command "command-for-wrapper-test")
      (is (equal '(:command) (reverse *wrapper-marks*))
          "and taking off the last one leaves the command running bare"))
    (is (null p::*command-wrappers*)
        "removing every wrapper empties the list rather than leaving a stub")))

(test removing-a-wrapper-that-was-never-added-is-not-an-error
  ;; It is called from a REPL, by somebody whose wrapper has just wedged their
  ;; session, who does not remember whether the ADD took.  Signalling there
  ;; would be answering "is it off?" with a debugger.
  (let ((p::*command-wrappers* (list 'inner-wrapper-for-test)))
    (is (null (p:remove-command-wrapper 'outer-wrapper-for-test)))
    (is (equal '(inner-wrapper-for-test) p::*command-wrappers*))))
