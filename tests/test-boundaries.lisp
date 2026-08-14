;;;; tests/test-boundaries.lisp --- The three error boundaries, checked.
;;;;
;;;; GUARDED, BEST-EFFORT and WITH-ABANDON are the whole of what stands between
;;;; a broken DEFMETHOD written at a live REPL and a frozen desktop, and nothing
;;;; had ever asked them a question.  That is how all three came to be built on
;;;; HANDLER-CASE, which transfers control to the exit point *before* running
;;;; its clause -- so the stack the error happened on is gone by the time
;;;; anything logs it.
;;;;
;;;; The consequence was not subtle and was invisible from inside: the one class
;;;; of error a live-editable window manager exists to let you fix was reported
;;;; as a single line of ~A.  WITH-ABANDON was worse, because its docstring
;;;; promised "log it with a backtrace" and it called LOG-BACKTRACE post-unwind,
;;;; printing its own frames.
;;;;
;;;; A backtrace is exactly the kind of thing that is easy to look at once, say
;;;; "yes, there is a backtrace", and never check again -- so these tests assert
;;;; the property that distinguishes a real one from a useless one: *the name of
;;;; the function that signalled appears in it.*  That is false for HANDLER-CASE
;;;; and true for HANDLER-BIND, and it is the whole difference.

(in-package #:latticewm/tests)

(def-suite boundaries :in model
  :description "The error boundaries: what they swallow, what they log, and
whether the log is any use.")

(in-suite boundaries)

;;; The signalling function is a DEFUN with a findable name rather than a
;;; LAMBDA, because the assertion is that its name reaches the log.
(defun boundary-victim ()
  "Signal, from a named function, so a backtrace can be checked for the name."
  (error "the policy method is broken"))

(defmacro with-captured-log ((var) &body body)
  "Run BODY with the log going to a string; VAR is everything it has said.

VAR IS A SYMBOL MACRO, AND IT USED TO BE A VARIABLE BOUND ONLY AFTER BODY RAN.
Every call site below ends its body with VAR — naming the thing it is about to
assert on — and under the old expansion that trailing form was a reference to
a variable that did not exist yet.  SBCL 2.6 deletes it, because its value is
discarded and the compiler can prove nothing depends on it; SBCL 2.2.9 does
not, and four of the five tests in this file died on `The variable LOG is
unbound' without reaching a single assertion.

So the suite written to check the error boundaries was the suite that could not
run on the oldest compiler this project claims to support, `plain' was the CI
job built to ask exactly that question, and the answer went unread for thirty
runs.  A test helper whose meaning depends on the compiler version is not a
test helper.

Reading VAR drains the stream into an accumulator, so it answers the same
inside BODY and after it, and answers the same twice."
  (let ((stream (gensym "STREAM"))
        (seen (gensym "SEEN"))
        (so-far (gensym "SO-FAR")))
    `(let* ((,stream (make-string-output-stream))
            (,seen (make-array 0 :element-type 'character
                                 :adjustable t :fill-pointer 0))
            (p:*log-stream* ,stream)
            (p:*log-level* :debug)
            (p:*log-to-stderr* nil)
            (p:*log-file* nil)
            (p:*debug-on-error* nil))
       (flet ((,so-far ()
                (loop for character across (get-output-stream-string ,stream)
                      do (vector-push-extend character ,seen))
                (coerce ,seen 'simple-string)))
         (symbol-macrolet ((,var (,so-far)))
           ,@body
           ,var)))))

(test guarded-returns-nil-and-names-the-frame-that-signalled
  "GUARDED swallows, and the backtrace is the error's rather than its own.

The second half is the point.  Under HANDLER-CASE this test passes its first
two assertions and fails the third, because the unwind has already happened
when the handler runs -- which is exactly the state this project shipped in."
  (let (result)
    (let ((text (with-captured-log (log)
                  (setf result (p:guarded "a-context" (boundary-victim)))
                  log)))
      (is (null result) "GUARDED returns NIL")
      (is (search "a-context" text) "the context reaches the log")
      (is (search "the policy method is broken" text)
          "the condition reaches the log")
      (is (search "BOUNDARY-VICTIM" text)
          "the backtrace names the function that signalled, which is the whole
difference between HANDLER-BIND and HANDLER-CASE here"))))

(test best-effort-is-quiet-about-the-compositor
  "BEST-EFFORT logs one line and no backtrace.

A request against a proxy river has already destroyed is an ordinary outcome,
not a defect of ours, and forty frames of ours saying so is noise that trains
a reader to skip the log."
  (let (result)
    (let ((text (with-captured-log (log)
                  (setf result (p:best-effort "wire-context" (boundary-victim)))
                  log)))
      (is (null result))
      (is (search "wire-context" text))
      (is (not (search "BOUNDARY-VICTIM" text))
          "no backtrace at the wire boundary"))))

(test the-two-boundaries-are-distinguishable-in-the-log
  "A reader can tell whose bug it was.

Before the split there was one name for both, so the log printed `window
destroy: ...' beside `layout: ...' with nothing to say which one was somebody's
mistake.  Once a boundary marker means anything it means nothing."
  (let ((policy (with-captured-log (log)
                  (p:guarded "ctx" (boundary-victim)) log))
        (wire (with-captured-log (log)
                (p:best-effort "ctx" (boundary-victim)) log)))
    (is (string/= policy wire)
        "the two boundaries do not produce the same log line")))

(test with-abandon-logs-the-error-s-frames-not-its-own
  "WITH-ABANDON's docstring promised a backtrace and printed the wrong one.

It called LOG-BACKTRACE from a HANDLER-CASE clause -- after the unwind -- so
what reached the log was the frames of the WITH-ABANDON site.  Meanwhile the
only place in the program that captured a true backtrace was the debugger
hook, which runs before unwinding, and WITH-ABANDON handled the error first:
the working path was permanently shadowed by the broken one."
  (let (reached)
    (let ((text (with-captured-log (log)
                  (p:with-abandon (boundary-victim) (setf reached :body-finished))
                  (setf reached (or reached :body-abandoned))
                  log)))
      (is (eq :body-abandoned reached) "the body is abandoned, not resumed")
      (is (search "abandoned" text))
      (is (search "BOUNDARY-VICTIM" text)
          "the frames are the error's"))))

(test debug-on-error-lets-the-condition-through
  "With *DEBUG-ON-ERROR*, all three decline, so a connected SLIME sees it.

There was no such option.  In a program whose entire thesis is that you edit it
while it runs, the whole reported surface of an error in your own code was one
line of text and no route to a debugger."
  (let ((p:*debug-on-error* t)
        (p:*log-stream* (make-string-output-stream))
        (p:*log-to-stderr* nil)
        (p:*log-file* nil))
    (signals error (p:guarded "ctx" (boundary-victim)))
    (signals error (p:best-effort "ctx" (boundary-victim)))
    (signals error (p:with-abandon (boundary-victim)))))

;;; ------------------------------------------------------------------ timers
;;;
;;; The same premise one mechanism over: the event loop has always taken a
;;; timeout and had never been asked for one, so nothing in this program
;;; happened because time passed.  These check the three properties the loop
;;; depends on -- that the wait shortens to the nearest deadline, that a
;;; repeating timer keeps its place while a one-shot takes itself off, and
;;; that a timer which signals is contained rather than taking the desktop.

(test the-wait-is-the-distance-to-the-nearest-deadline
  "TIMER-WAIT is what turned the constant into a question.

Bounded above by *POLL-INTERVAL*, so a timer can only ever make the loop wake
sooner than the backstop; and never zero, because a zero timeout is a spin."
  (let ((r::*timers* (make-hash-table :test #'equal)))
    (is (= r:*poll-interval* (r::timer-wait))
        "with nothing registered the backstop is the whole answer")
    (r:add-timer :far 600 (lambda () nil))
    (is (= r:*poll-interval* (r::timer-wait))
        "and a deadline beyond the backstop does not lengthen the wait")
    (r:add-timer :near 2 (lambda () nil))
    (is (<= (r::timer-wait) 2) "the nearest deadline shortens it")
    (is (plusp (r::timer-wait)) "and never to zero, which would be a spin")))

(test a-repeating-timer-keeps-its-place-and-a-one-shot-takes-itself-off
  (let ((r::*timers* (make-hash-table :test #'equal))
        (repeats 0)
        (onces 0))
    (r:add-timer :repeating 1 (lambda () (incf repeats)))
    (r:add-timer :once 1 (lambda () (incf onces)) :repeat nil)
    ;; Due now rather than in a second: the suite does not have a second.
    (maphash (lambda (name timer)
               (declare (ignore name))
               (setf (r::timer-due timer) 0))
             r::*timers*)
    (is (= 2 (r::run-due-timers)) "both were due")
    (is (= 1 repeats))
    (is (= 1 onces))
    (is (member :repeating (loop for k being the hash-keys of r::*timers* collect k))
        "the repeating one is still registered")
    (is (not (member :once (loop for k being the hash-keys of r::*timers* collect k)))
        "and the one-shot took itself off rather than needing to be removed")))

(test re-adding-a-timer-under-one-name-replaces-it
  "The property that makes loading a configuration file twice safe."
  (let ((r::*timers* (make-hash-table :test #'equal)))
    (r:add-timer :clock 30 (lambda () nil))
    (r:add-timer :clock 30 (lambda () nil))
    (is (= 1 (hash-table-count r::*timers*))
        "one name, one timer, however many times it is registered")
    (r:remove-timer :clock)
    (is (= 0 (hash-table-count r::*timers*)))))

(test a-timer-that-signals-does-not-take-the-loop-with-it
  "It runs inside GUARDED, so a broken clock costs a log line and its own tick."
  (let ((r::*timers* (make-hash-table :test #'equal))
        (after 0)
        (p:*log-to-stderr* nil)
        (p:*log-file* nil))
    (r:add-timer :broken 1 (lambda () (boundary-victim)))
    (r:add-timer :fine 1 (lambda () (incf after)))
    (maphash (lambda (name timer)
               (declare (ignore name))
               (setf (r::timer-due timer) 0))
             r::*timers*)
    (finishes (r::run-due-timers))
    (is (= 1 after) "the timer behind the broken one still ran")
    (is (= 2 (hash-table-count r::*timers*))
        "and the broken one keeps its place rather than being dropped")))
