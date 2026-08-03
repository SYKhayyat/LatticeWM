;;;; tools/bench.lisp --- How fast is the model, with no compositor involved?
(require :asdf)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "lattice"))
(in-package #:latticewm/user)

(defun make-big-world (n)
  (let ((world (make-world))
        (policy (make-instance 'conventional-policy)))
    (setf *policy* policy)
    (dotimes (i n) (on-window-open policy world
                                   (make-instance 'window :app-id "bench")))
    world))

(defmacro timing (label n &body body)
  `(let ((start (get-internal-real-time)))
     (dotimes (i ,n) ,@body)
     (let ((ms (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second 0.001)))
       (format t "~&~a~40t~8,3f ms total  ~8,4f ms each~%" ,label ms (/ ms ,n)))))

(dolist (n '(10 50 200))
  (format t "~&~%=== ~d windows ===~%" n)
  (let* ((world (make-big-world n))
         (policy *policy*)
         (root (world-root world))
         (rect (make-rect 0 0 1920 1080)))
    (timing "layout-node" 1000 (layout-node policy root rect))
    (let ((rects (latticewm/policy::motion-rects policy root)))
      (timing "find-motion-target (cached rects)" 1000
        (find-motion-target policy root (world-cursor world) :right :rects rects)))
    (timing "move-cursor" 1000 (move-cursor policy world :right))
    (timing "leaf-paths" 1000 (leaf-paths root))
    (timing "repair-path" 1000 (repair-path root (world-cursor world)))))

(format t "~&~%=== lattice, 100 cells, 4 visible ===~%")
(let* ((grid (lattice:make-grid :cols 2 :rows 2))
       (policy (make-instance 'lattice:lattice-policy))
       (rect (make-rect 0 0 1920 1080)))
  (dotimes (x 10)
    (dotimes (y 10)
      (setf (child-at grid (lattice:cell x y))
            (make-leaf (make-instance 'window :app-id "c")))))
  (timing "layout-node (grid)" 1000 (layout-node policy grid rect))
  (timing "container-addresses" 1000 (container-addresses grid)))

;;; ----------------------------------------------------------------- drawing
;;;
;;; THIS SECTION EXISTS BECAUSE EVERYTHING ABOVE IT WAS THE WRONG THING TO
;;; MEASURE.  For seven sessions this file benchmarked the model — the pure,
;;; functional, trivially-benchmarkable half — and reported numbers like
;;; 0.35 ms to lay out two hundred windows, which read as "this program is
;;; fast".  Meanwhile clearing the help overlay cost 40 ms, on a keypress,
;;; and nothing measured it.
;;;
;;; It is the same shape as the finding in src/runtime/server.lisp — "the
;;; tests pass because they construct state rather than receive it" — one
;;; layer over: *the bench measured the model because the model is what is
;;; easy to measure.*  A benchmark that only covers the pure half of a
;;; program is not a benchmark of the program.
;;;
;;; No compositor is needed.  A canvas is an mmap and a few integers; the
;;; wl_shm pool is only how the compositor comes to be looking at the same
;;; bytes, and nothing in the drawing path reads it.  So: mmap anonymously,
;;; draw, and time it.

(defun bench-canvas (w h scale)
  "A canvas backed by ordinary anonymous memory rather than by wl_shm."
  (let* ((width (* scale w)) (height (* scale h))
         (stride (* width 4)) (size (* stride height))
         (data (sb-posix:mmap nil size
                              (logior sb-posix:prot-read sb-posix:prot-write)
                              (logior sb-posix:map-private sb-posix:map-anon)
                              -1 0)))
    (latticewm/runtime::%make-canvas
     :width width :height height :scale scale
     :stride stride :size size :fd -1 :data data)))

(setf *policy* (make-instance 'conventional-policy))

(let ((line "[3/3] (-4 . 0) ~/src/latticewm  emacs  120x40  M-x split-right")
      (row "Super+h        focus-left     move the cursor to the pane on the left")
      (colour (r:argb 0.9 0.9 0.9)))
  ;; The echo area, which is redrawn every time anything at all changes.
  (dolist (spec '((1920 24 1) (3840 48 2)))
    (destructuring-bind (w h scale) spec
      (let ((canvas (bench-canvas w h scale)))
        (format t "~&~%=== echo area ~dx~d at scale ~d ===~%" w h scale)
        (timing "canvas-fill" 200 (r:canvas-fill canvas 0))
        (timing "canvas-text (one status line)" 200
          (r:canvas-text canvas 4 4 line colour))
        (timing "one redraw" 200
          (progn (r:canvas-fill canvas 0)
                 (r:canvas-text canvas 4 4 line colour))))))
  ;; The help overlay, which is a full screen of text on Super+/.
  (dolist (spec '((1920 1080 1) (1920 1080 2)))
    (destructuring-bind (w h scale) spec
      (let ((canvas (bench-canvas w h scale)))
        (format t "~&~%=== help overlay ~dx~d at scale ~d ===~%" w h scale)
        (timing "canvas-fill (whole screen)" 50 (r:canvas-fill canvas 0))
        (timing "60 rows of text" 50
          (dotimes (n 60) (r:canvas-text canvas 20 (+ 10 (* n 17)) row colour)))
        (timing "one full redraw" 50
          (progn (r:canvas-fill canvas 0)
                 (dotimes (n 60)
                   (r:canvas-text canvas 20 (+ 10 (* n 17)) row colour))))))))

(format t "~&~%heap in use: ~,1f MB~%" (/ (sb-kernel:dynamic-usage) 1048576.0))
