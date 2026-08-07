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
  "Run BODY with the log going to a string, and bind VAR to what it said."
  `(let* ((stream (make-string-output-stream))
          (p:*log-stream* stream)
          (p:*log-level* :debug)
          (p:*log-to-stderr* nil)
          (p:*log-file* nil)
          (p:*debug-on-error* nil))
     ,@body
     (let ((,var (get-output-stream-string stream)))
       ,var)))

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
