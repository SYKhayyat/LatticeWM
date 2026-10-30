;;;; tests/test-ipc.lisp --- one form in, one line out, from a socket with no compositor.
;;;;
;;;; THE CONTROL SOCKET HAD NO UNIT TEST.  The reason is the shape of the
;;;; file: it answers a form on a thread that is not the window manager's, so
;;;; every check wants a second thread, and the fixtures in this suite
;;;; construct state rather than run it.  So the one surface the program
;;;; advertises on — "scripting it needs nothing installed that running it did
;;;; not already need" — had exactly the coverage its fixtures allowed, which
;;;; was none.
;;;;
;;;; WHAT IS HERE IS THE HALF THAT NEEDS NO COMPOSITOR, which is most of the
;;;; interesting part.  #85 is not a Wayland bug; it is a *thread* bug.  A form
;;;; that never returned answered the terminal and then kept running, so the
;;;; next form queued behind it and the socket echoed its own timeout message
;;;; forever.  That is answerable with a thread and a flag, and it is answered
;;;; below: evaluate a form that loops, assert that the answer arrives, that it
;;;; names the right failure, and that the socket still works afterwards.
;;;;
;;;; THE REST NEEDS A REAL COMPOSITOR: tools/integration.lisp drives the
;;;; actual daemon, and its control-socket section now checks the same two
;;;; facts against the real event loop — that the answer arrives and that the
;;;; next form runs.  These checks are the ones that fail first.

(in-package #:latticewm/tests)
(in-suite ipc)

;;; ------------------------------------------------------------------ fixtures

(defvar *ipc-mark* nil
  "Set by a form under test, so a check can see that the form *ran*.

A form is read and evaluated in LATTICEWM/USER, so the form names this symbol
by its package-qualified name rather than by a bare `*ipc-mark*' — the latter
would intern a second, unrelated special in the user package and the check
would read a variable nothing wrote.")

(defun marking-form (&optional (body ""))
  "BODY, then a marker, then a form that never returns.

The whole point of the fixture: a form that has demonstrably started and
demonstrably never finished is the subject of #85."
  ;; ONE FORM, because the protocol is one form per line and READ-FROM-STRING
  ;; reads exactly the first — a fixture that sent two would silently test
  ;; only the marker.
  (format nil "(progn (setq latticewm/tests::*ipc-mark* :started) ~a (loop))" body))

(defun newline-count (string)
  "How many newlines STRING contains.  A one-line answer contains none."
  (count #\Newline string))

(defmacro with-fake-wm-thread ((&key (drain t)) &body body)
  "Run BODY with *WM-THREAD* bound to a thread that is not the caller's.

DRAIN runs the real queue drain, so a form queued by CALL-IN-WM-THREAD-SYNC is
executed on a *different* thread than the one waiting for it — the production
path, and the only one #85 lives on.  DRAIN NIL is a window manager that is
wedged somewhere else and never gets to the queue at all.

Restored on the way out, and the thread is terminated rather than left for the
process to reap, because a leaked fake window manager would be a real one for
every later suite in the run."
  (let ((thread (gensym "FAKE-WM-")))
    `(let* ((,thread (bt:make-thread
                      (lambda ()
                        ,(if drain
                             '(loop (ignore-errors (r::drain-wm-queue)) (sleep 0.01))
                             '(loop (sleep 0.05))))
                      :name "latticewm-test-fake-wm"))
            (r::*wm-thread* ,thread))
       (unwind-protect (progn ,@body)
         (ignore-errors (bt:destroy-thread ,thread))
         (setf r::*wm-thread* nil)))))

;;; ------------------------------------------------------- the plain answers

(test a-form-answers-in-one-line
  "One form in, one line out — including when the value prints with newlines."
  (is (string= "(:ok 3)" (r::evaluate-for-ipc "(+ 1 2)")))
  (is (= 0 (newline-count
            (r::evaluate-for-ipc
             "(format nil \"first~%second~%third\")")))))

(test an-unreadable-form-is-an-error-not-a-hang
  "A READ failure answers.  A control socket whose answer to a bad question is
silence is a socket nobody can debug, which is what this file says of itself."
  (let ((answer (r::evaluate-for-ipc "(+ 1 2")))
    (is (stringp answer))
    (is (plusp (length answer)))
    (is (= 0 (newline-count answer)))
    (is (search "(:error" answer))))

(test a-signalling-form-is-an-error-in-one-line
  "A condition that reports as a paragraph is still one line, because ONE-LINE
is applied to the whole answer after it is printed rather than to pieces."
  (let ((answer (r::evaluate-for-ipc
                 "(error \"first line~%second line~%third line\")")))
    (is (search "(:error" answer))
    (is (search "second line" answer))
    (is (= 0 (newline-count answer)))))

;;; ---------------------------------------- #85: the form that never returns

(test a-form-that-never-returns-is-answered-and-named
  "The form that loops gets an answer, and the answer says *which* failure it
is.  Before the fix it said `the window manager did not answer', which is a
statement about the thread; the thread was fine, the form was the problem, and
the form was still running."
  (let ((r::*ipc-timeout-seconds* 1)
        (*ipc-mark* nil))
    (let ((answer (r::evaluate-for-ipc (marking-form))))
      (is (eq :started *ipc-mark*)
          "the form must have started before it can be abandoned")
      (is (stringp answer))
      (is (= 0 (newline-count answer)))
      (is (search "abandoned" answer))
      (is (not (search "did not answer" answer))))))

(test a-form-that-never-returns-leaves-the-socket-usable
  "THE REGRESSION TEST FOR #85.  Before the fix the abandoned thunk kept
running in the window manager thread — which is the event loop — so every form
after it queued behind a loop that never ended.  One `latticewm --eval (loop)'
turned the control surface into a permanent echo of its own timeout message,
with no recovery short of ending the session.

So the answer is not `a form times out'.  The answer is `a form times out *and
the next form is answered normally*'."
  (let ((r::*ipc-timeout-seconds* 1)
        (*ipc-mark* nil))
    (is (search "abandoned" (r::evaluate-for-ipc (marking-form))))
    (is (eq :started *ipc-mark*))
    (is (string= "(:ok 3)" (r::evaluate-for-ipc "(+ 1 2)")))
    (is (string= "(:ok 3)" (r::evaluate-for-ipc "(+ 1 2)")))))

(test a-form-that-finishes-in-time-is-not-abandoned
  "The budget is the form's, not the caller's patience.  A deadline that fired
at forms which merely waited would fail here."
  (let ((r::*ipc-timeout-seconds* 1))
    (is (string= "(:ok :SLOW-BUT-FINE)"
                 (r::evaluate-for-ipc "(progn (sleep 0.25) :slow-but-fine)")))
    (is (string= "(:ok :FAST)" (r::evaluate-for-ipc ":fast")))))

(test the-same-two-checks-on-a-second-thread
  "The same two facts on the production path: a caller that is not the window
manager thread, a form queued onto one, and the deadline applied there.  The
inline path above runs the form where the caller already is, which is what
happens when a form evaluated over the socket calls back into this — not what
`latticewm --eval' does."
  (with-fake-wm-thread ()
    (let ((r::*ipc-timeout-seconds* 1))
      ;; The global, not a binding: the form runs on the fake window manager
      ;; thread, which does not share this thread's dynamic bindings — and that
      ;; is the point of the fixture rather than an inconvenience of it.
      (setf *ipc-mark* nil)
      (is (search "abandoned" (r::evaluate-for-ipc (marking-form))))
      (is (eq :started *ipc-mark*))
      (is (string= "(:ok 3)" (r::evaluate-for-ipc "(+ 1 2)"))))))

;;; ----------------------------------- the other failure, and why it is distinct

(test a-wedged-thread-is-not-blamed-on-the-form
  ":TIMEOUT and :ABANDONED must never collapse into one message.  They are
different failures with different recoveries: raising a deadline fixes the
second and does nothing for the first.  A test that only checked `the caller
got an answer' would pass on a fix that told a user to raise a deadline while
the whole session was wedged — which is worse than the bug."
  (with-fake-wm-thread (:drain nil)
    (let ((r::*ipc-timeout-seconds* 1))
      (let ((answer (r::evaluate-for-ipc "(+ 1 2)")))
        (is (search "did not answer" answer))
        (is (not (search "abandoned" answer)))))))

;;; ----------------------------- what an abandoned form leaves behind, pushed

(test an-abandoned-form-still-pushes-what-it-had-already-done
  "A half-finished form is a world the screen has not been told about.  The
thunk's UNWIND-PROTECT runs AFTER-COMMAND on the way out, so the emitter is
asked whether the tree still fits even though the form never reached the place
it would have pushed from.  Without that cleanup the tree and the screen
disagree until something else happens to dirty them.

Observed through the undo ring rather than the emitter, because the emitter
needs a compositor: NOTE-LAYOUT-SETTLED records a step when the tree changed,
and this form changes it and then hangs."
  (let ((r::*ipc-timeout-seconds* 1)
        (r::*undo-depth* 10)
        (r::*undo-coalesce-seconds* 0))
    (let ((r::*world* (c:make-world)))
      ;; A structural change rather than a label: NODE-SIGNATURE is the thing
      ;; that decides whether a settle is a step, and it records the tree, not
      ;; every slot on every node.
      (flet ((split-string ()
               "(setf (c:child-at (c:world-root *world*) 0)
                      (c:make-split :horizontal
                                    (list (c:make-leaf) (c:make-leaf))))"))
        ;; The first settle of the session records a baseline and no step.
        (is (search "(:ok" (r::evaluate-for-ipc (split-string))))
        (is (= 0 (length (r:undo-ring))))
        ;; A second change is a step.  A leaf's identity and its window are in
        ;; NODE-SIGNATURE, so filling the second pane is a structural change —
        ;; and, unlike flipping a split's axis, one the signature can see (#98).
        (is (search "(:ok" (r::evaluate-for-ipc
                            "(setf (c:child-at (c:world-root *world*) 0)
                                   (c:make-leaf (make-instance 'c:window
                                                               :app-id \"second\")))")))
        (is (= 1 (length (r:undo-ring))))
        ;; And a change followed by a hang is a step too, because the push on
        ;; the way out of the deadline runs.
        (is (search "abandoned"
                    (r::evaluate-for-ipc
                     "(progn (setf (c:child-at (c:world-root *world*) 0)
                                   (c:make-leaf (make-instance 'c:window
                                                               :app-id \"third\")))
                             (loop))")))
        (is (= 2 (length (r:undo-ring))))))))
