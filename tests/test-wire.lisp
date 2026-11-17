;;;; tests/test-wire.lisp --- What the protocol can carry.
;;;;
;;;; #91: Wayland's `int' is 32 bits and the layout is not.  A bignum reaching
;;;; wayflan's marshaller signals a type error; at the emit boundary that lands
;;;; inside BEST-EFFORT, so the failure is a log line per frame and a window the
;;;; desktop has quietly stopped placing.
;;;;
;;;; Two checks, because the fix has two halves: the value saturates, and the
;;;; call site records what it saturates to (the emitter's diff table is what
;;;; the next frame is judged against, so a clamped send with an unclamped key
;;;; would resend forever).

(in-package #:latticewm/tests)
(in-suite wire)

(test an-int-beyond-the-wire-s-range-saturates
  "The helper itself.  The interesting values are the two edges and what lies
just past them, not a round number in the middle."
  (is (= 0 (w:clamp-int32 0)))
  (is (= 1234 (w:clamp-int32 1234)))
  (is (= -1 (w:clamp-int32 -1)))
  (is (= w::+int32-max+ (w:clamp-int32 w::+int32-max+))
      "the largest int is representable")
  (is (= w::+int32-min+ (w:clamp-int32 w::+int32-min+)))
  (is (= w::+int32-max+ (w:clamp-int32 (1+ w::+int32-max+)))
      "one past the largest saturates")
  (is (= w::+int32-min+ (w:clamp-int32 (1- w::+int32-min+))))
  (is (= w::+int32-max+ (w:clamp-int32 (expt 2 40)))
      "a coordinate from a far-out plane")
  (is (= w::+int32-min+ (w:clamp-int32 (- (expt 2 40))))))

(test an-emitted-position-is-clamped-before-it-is-sent
  "The call site, not only the helper.  The emitter's diff table is what the
next frame compares against, so the recorded position has to be the clamped
one: otherwise a window parked past the edge is resent every frame forever,
which is the log spam this fix exists to remove.

The request itself is not the subject here — a sentinel node makes it fail
inside BEST-EFFORT, the way a compositor that cannot place the window would."
  (let* ((server (make-instance 'r::server))
         (r:*server* server)
         (p:*log-level* nil)
         (window (win "far"))
         (node (c:make-leaf window))
         (placed (c:make-rect (expt 2 40) 7 100 100)))
    (setf (gethash window (r::server-nodes server)) :sentinel)
    (r::emit-window-visible (policy) window node '(0) placed nil)
    (is (equal (list w::+int32-max+ 7) (r::emitted window :position))
        "what the emitter recorded sending is the clamped position")))

(test an-emitted-dimension-is-clamped-before-it-is-sent
  "Same boundary, other request: a very wide cell is geometry too."
  (let* ((server (make-instance 'r::server))
         (r:*server* server)
         (p:*log-level* nil)
         (window (win "wide")))
    (setf (c:window-proxy window) :sentinel
          (r::server-pending-dimensions server)
          (list (list window (expt 2 40) 10)))
    (r::emit-dimension-work)
    (is (equal (list w::+int32-max+ 10) (r::emitted window :dimensions))
        "what the emitter recorded sending is the clamped size")))
