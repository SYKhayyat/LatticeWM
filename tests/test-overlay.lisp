;;;; tests/test-overlay.lisp --- The buffer the compositor is reading.
;;;;
;;;; For the whole life of the project there was one canvas per overlay, no
;;;; wl_buffer.release listener and no wl_surface.frame callback, and the CANVAS
;;;; docstring stated the consequence as though it were a feature: "the
;;;; compositor is looking at the same bytes we are, so a write is visible as
;;;; soon as the surface is committed."  The vendored wayland.xml, in this tree,
;;;; says the opposite in as many words.
;;;;
;;;; None of that is reachable from a suite with no compositor, and that is
;;;; exactly the shape of finding this project keeps making: the instrument
;;;; faces the part that can be constructed.  So what is asserted here is the
;;;; bookkeeping the fix rests on, which *is* constructible — which buffer is
;;;; safe to draw into, what a buffer still holds, and what changed between the
;;;; one on screen and the one about to replace it.  The half that needs a
;;;; compositor is in tools/integration.lisp and is honest about being weaker.

(in-package #:latticewm/tests)
(in-suite pixels)

;;; ------------------------------------------------------------- fixtures

(defmacro with-canvases ((&rest bindings) &body body)
  "Bind each of BINDINGS — (VAR WIDTH HEIGHT &optional SCALE) — to a canvas
backed by a pinned Lisp vector instead of an shm mapping.

Everything below the wl_shm requests is ordinary memory and ordinary
arithmetic, so the drawing and the erasing can be exercised for real and the
pixels read back.  What cannot be exercised is the protocol, which is the point
made at the top of this file."
  (let ((vectors (mapcar (lambda (binding) (gensym (string (first binding))))
                         bindings)))
    `(let ,(mapcar (lambda (vector binding)
                     (destructuring-bind (name w h &optional (scale 1)) binding
                       (declare (ignore name))
                       `(,vector (make-array (* 4 ,w ,h ,scale ,scale)
                                             :element-type '(unsigned-byte 8)
                                             :initial-element 0))))
                   vectors bindings)
       (sb-sys:with-pinned-objects ,vectors
         (let ,(mapcar (lambda (vector binding)
                         (destructuring-bind (name w h &optional (scale 1)) binding
                           `(,name (r::%make-canvas
                                    :width (* ,w ,scale) :height (* ,h ,scale)
                                    :scale ,scale :stride (* 4 ,w ,scale)
                                    :size (length ,vector)
                                    :data (sb-sys:vector-sap ,vector)))))
                       vectors bindings)
           ,@body)))))

(defun pixel-at (canvas x y)
  "The device pixel at (X, Y) of CANVAS, as a 32-bit word."
  (sb-sys:sap-ref-32 (r::canvas-data canvas)
                     (* 4 (+ (* y (r::canvas-width canvas)) x))))

;;; ------------------------------------------------ which buffer may be drawn

(test a-buffer-the-compositor-is-reading-is-never-drawn-into
  "The whole change in one assertion.

FREE-CANVAS is what stands between a redraw and the buffer the compositor is
reading, and it has two ways to say no: the buffer has not been released, or
the buffer is the one the surface currently references.  The second is not
redundant — a compositor may release a buffer it is still displaying, and
drawing into that is the single-buffered bug wearing a pool."
  (let ((a (r::%make-canvas)) (b (r::%make-canvas)))
    (let ((pool (list a b)))
      (is (eq a (r::free-canvas pool nil))
          "with nothing committed and nothing busy, the first will do")
      (setf (r::canvas-busy a) t)
      (is (eq b (r::free-canvas pool nil))
          "a busy buffer is skipped")
      (is (null (r::free-canvas pool b))
          "and so is the one on screen, released or not: ~a" (r::canvas-busy b))
      (setf (r::canvas-busy a) nil)
      (is (eq a (r::free-canvas pool b))
          "the release event is what puts a buffer back into circulation"))))

(test a-resize-retires-every-buffer-and-not-one-of-them
  "Two buffers of different sizes on one surface is a class of bug worth not
having, so the size check is asked of the pool rather than of the canvas that
happens to be current."
  (let ((canvas (r::%make-canvas :width 200 :height 100 :scale 1)))
    (is (r::canvas-fits-p canvas 200 100 1))
    (is (not (r::canvas-fits-p canvas 200 101 1)) "a size change retires it")
    (is (not (r::canvas-fits-p canvas 200 100 2))
        "and so does a scale change, which is the HiDPI half: the same logical
size on a 2x monitor is four times the pixels")
    (let ((doubled (r::%make-canvas :width 400 :height 200 :scale 2)))
      (is (r::canvas-fits-p doubled 200 100 2)
          "the question is asked in logical pixels, which is what a drawer has"))))

;;; ------------------------------------------------- what a buffer still holds

(test a-buffer-is-erased-of-what-it-holds-not-of-what-the-last-frame-drew
  "The bug that arrives with a second buffer, asserted directly.

The incremental drawers clear only the rectangles they drew, which was right
when there was one buffer and the last frame was the only thing on it.  With
two, the buffer handed back is holding the frame from *two* redraws ago, and
clearing last frame's rectangles leaves a stale label sitting under a fresh one
— visible, wrong, and impossible to see in a suite that only counts.

So the record moved off the overlay and onto the canvas.  This alternates two
buffers by hand and reads the pixels back."
  (with-canvases ((a 20 20) (b 20 20))
    (let ((overlay (make-instance 'r:overlay :kind :test)))
      (flet ((frame (canvas rect)
               (setf (r::overlay-canvas overlay) canvas)
               (r::canvas-erase canvas)
               (r:canvas-fill canvas #xffff0000 rect)
               (r:overlay-drew overlay (list rect))))
        ;; Frame 1 into A, frame 2 into B — different rectangles, so a wrong
        ;; erase has somewhere to show up.
        (frame a (c:make-rect 0 0 4 4))
        (frame b (c:make-rect 10 10 4 4))
        (is (= #xffff0000 (pixel-at a 0 0)) "A holds frame 1")
        (is (= #xffff0000 (pixel-at b 10 10)) "B holds frame 2")
        (is (zerop (pixel-at a 10 10)) "and neither holds the other's")
        ;; Frame 3 comes back round to A, whose content is now two frames old.
        (frame a (c:make-rect 10 10 4 4))
        (is (zerop (pixel-at a 0 0))
            "A was erased of what A was holding.  Erasing what the *last* frame
drew would have cleared (10,10) — which frame 3 is about to paint anyway — and
left frame 1's rectangle on the screen underneath it")
        (is (= #xffff0000 (pixel-at a 10 10)) "and frame 3 is on it")
        (is (= #xffff0000 (pixel-at b 10 10))
            "while B is untouched, because it is the compositor's until it is
released")))))

(test a-drawer-that-fills-its-canvas-pays-for-no-clearing-and-damages-all-of-it
  "Saying nothing is the truthful answer for three of the five drawers.

The echo area, the help screen and the drawn map each fill their canvas; the
coordinate overlay and the empty-pane outlines do not.  A drawer of the first
kind never calls OVERLAY-DREW, and what it gets for that is exactly right — no
clearing, because it is about to overwrite every pixel, and full damage, because
it cannot say which of them changed.

The one shape this does not serve is a drawer that alternates.  That is a stated
rule rather than a guarded case: the alternative is clearing 33 megabytes on
every frame of the drawn map to defend against something no drawer does."
  (with-canvases ((canvas 20 20))
    (r:canvas-fill canvas #xff00ff00)
    (is (eq t (r::canvas-drawn canvas))
        "a drawer that said nothing is recorded as having painted all of it")
    (is (eq t (r::canvas-erase canvas))
        "so the next frame skips the memset rather than zeroing pixels the
drawer is about to overwrite")
    (is (= #xff00ff00 (pixel-at canvas 19 19)) "and the frame is still there")
    (is (eq t (r::canvas-drawn canvas))
        "T is also what the erase leaves behind, so a drawer that keeps saying
nothing keeps getting full damage")
    (is (eq t (r:surface-damage (r::canvas-drawn canvas) '()))
        "which is what OVERLAY-COMMIT then reports")))

(test what-a-drawer-recorded-survives-into-the-damage
  "OVERLAY-DREW is one SETF and this is the whole of what it buys.

The record has two readers and they are the two halves of the fix: the next
frame into this buffer erases exactly these rectangles, and the compositor is
told exactly these changed."
  (with-canvases ((canvas 20 20))
    (let ((overlay (make-instance 'r:overlay :kind :test))
          (rects (list (c:make-rect 1 1 2 2) (c:make-rect 8 8 2 2))))
      (setf (r::overlay-canvas overlay) canvas)
      (r:overlay-drew overlay rects)
      (is (eq rects (r::canvas-drawn canvas)))
      (is (equalp rects (r:surface-damage (r::canvas-drawn canvas) '()))
          "and against a blank buffer on screen the damage is just what was drawn"))))

;;; ------------------------------------------------------ what changed on screen

(test the-damage-is-the-two-frames-and-not-the-whole-surface
  "SURFACE-DAMAGE is the ten lines the report asked for.

Both incremental drawers record every rectangle they touch, and both then had
OVERLAY-COMMIT tell the compositor that the entire surface had changed — so the
saved memset bought a full texture upload.  The damage is the union of the two
buffers' records, because everything outside both is transparent in both, so
nothing outside the union can have changed."
  (let ((old (list (c:make-rect 0 0 4 4)))
        (new (list (c:make-rect 10 10 4 4))))
    (is (equal (append new old) (r:surface-damage new old))
        "what appeared and what disappeared, and no more")
    (is (eq t (r:surface-damage t old))
        "a buffer painted whole cannot say where, so the answer is everywhere")
    (is (eq t (r:surface-damage new t))
        "and so is a buffer on screen that was painted whole — which is what a
freshly allocated one reports, so the first commit after a resize damages all
of it")
    (is (null (r:surface-damage '() '()))
        "two blank buffers changed nothing, which is a real answer and not a
reason to upload a screen")))

;;; ------------------------------------------------------ giving the buffer back

(test which-overlays-give-their-buffers-back-is-a-decision-per-kind
  "*OVERLAY-BUFFER-IDLE* was T, argued from arithmetic that was wrong by 8x.

\"A full-screen ARGB buffer is about four megabytes\" is 1920x1080x4 at scale 1.
MAKE-CANVAS multiplies by the output scale before computing the stride, so the
same panel at scale 2 — the machine the sentence was written on — is 33 MB, and
there are two of them now.  That makes releasing far more worthwhile for the
help screen and far less for anything a continuous gesture toggles, which is
why the answer is a list of kinds rather than a yes."
  (is (p:overlay-buffer-idle-p :help)
      "the shipped answer: large, and shown for seconds a week")
  (is (not (p:overlay-buffer-idle-p :lattice/map))
      "and not the one a zoom crossing a threshold toggles, where releasing
means mkstemp, ftruncate, mmap and create_pool per wobble")
  (is (not (p:overlay-buffer-idle-p :echo)))
  (let ((p:*overlay-buffer-idle* t))
    (is (p:overlay-buffer-idle-p :echo) "T is still every overlay, as before")
    (is (p:overlay-buffer-idle-p :something-nobody-has-defined)
        "including an extension's own"))
  (let ((p:*overlay-buffer-idle* nil))
    (is (not (p:overlay-buffer-idle-p :help)) "and NIL is still none of them")))
