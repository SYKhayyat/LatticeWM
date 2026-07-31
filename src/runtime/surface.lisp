;;;; runtime/surface.lisp --- Drawing our own pixels.
;;;;
;;;; The window manager needs surfaces of its own for the echo area, the
;;;; coordinate overlay and eventually the minimap.  River provides exactly the
;;;; mechanism: river_window_manager_v1.get_shell_surface wraps one of our
;;;; wl_surfaces so it can be positioned and ordered like a window, and gets
;;;; its own river_node_v1.
;;;;
;;;; This is the wl_shm path, which README calls "the least-proven part of
;;;; wayflan" and the cut list removed for that reason.  It is here because
;;;; both of the things it buys are, in PLAN.org's own words, the defence
;;;; against the design's biggest stated risk:
;;;;
;;;;   "README calls two-dimensional navigation 'the single biggest design
;;;;   risk' and names three mitigations: the minimap, named cells, and the
;;;;   coordinate overlay.  The cut list removes the first two.  *Removing all
;;;;   three would ship the ZUI get-lost failure mode with nothing standing
;;;;   against it.*"
;;;;
;;;; It turned out to be about ninety lines.  wayflan's fd passing is real
;;;; sendmsg with cmsg header walking, and it worked first time.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------------ shm buffers

(defstruct (canvas (:constructor %make-canvas))
  "A block of shared memory the compositor can read as pixels.

Pixels are ARGB8888, premultiplied, one 32-bit word each, row-major.  DATA is
a foreign pointer; the compositor is looking at the same bytes we are, so a
write is visible as soon as the surface is committed."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (stride 0 :type fixnum)
  (size 0 :type fixnum)
  (fd -1 :type fixnum)
  (data (sb-sys:int-sap 0))
  (pool nil)
  (buffer nil))

(defun make-shm-file (size)
  "An anonymous file of SIZE bytes in the runtime directory.

Unlinked immediately, so it exists only as long as the descriptor does and
cannot be left behind by a crash.  memfd_create would be tidier and needs
CFFI; this needs nothing and works on every kernel."
  (let* ((directory (or (uiop:getenv "XDG_RUNTIME_DIR") "/tmp"))
         (template (format nil "~a/latticewm-XXXXXX" directory)))
    (multiple-value-bind (fd path) (sb-posix:mkstemp template)
      (ignore-errors (sb-posix:unlink path))
      (sb-posix:ftruncate fd size)
      fd)))

(defun make-canvas (shm width height)
  "A WIDTH by HEIGHT canvas, with a wl_buffer the compositor can attach."
  (let* ((width (max 1 width))
         (height (max 1 height))
         (stride (* width 4))
         (size (* stride height))
         (fd (make-shm-file size))
         (data (sb-posix:mmap nil size
                              (logior sb-posix:prot-read sb-posix:prot-write)
                              sb-posix:map-shared fd 0))
         (pool (wl:wl-shm.create-pool shm fd size))
         (buffer (wl:wl-shm-pool.create-buffer pool 0 width height stride
                                               :argb8888)))
    (%make-canvas :width width :height height :stride stride :size size
                  :fd fd :data data :pool pool :buffer buffer)))

(defun destroy-canvas (canvas)
  "Release everything a canvas holds."
  (ignore-errors (wl:wl-buffer.destroy (canvas-buffer canvas)))
  (ignore-errors (wl:wl-shm-pool.destroy (canvas-pool canvas)))
  (ignore-errors (sb-posix:munmap (canvas-data canvas) (canvas-size canvas)))
  (ignore-errors (sb-posix:close (canvas-fd canvas)))
  (setf (canvas-buffer canvas) nil (canvas-pool canvas) nil)
  nil)

;;; ------------------------------------------------------------- drawing

(declaim (inline argb))
(defun argb (r g b &optional (a 1.0))
  "Pack floats in [0,1] into a premultiplied ARGB8888 word.

Premultiplied because that is what wl_shm's argb8888 means, and getting it
wrong shows up as a halo around everything rather than as an error."
  (flet ((byte8 (x) (max 0 (min 255 (round (* 255 (max 0.0 (min 1.0 x))))))))
    (let ((alpha (byte8 a)))
      (logior (ash alpha 24)
              (ash (byte8 (* r a)) 16)
              (ash (byte8 (* g a)) 8)
              (byte8 (* b a))))))

(defun canvas-fill (canvas color &optional rect)
  "Fill RECT — or the whole canvas — with COLOR."
  (let* ((data (canvas-data canvas))
         (width (canvas-width canvas))
         (height (canvas-height canvas))
         (x0 (if rect (max 0 (c:rect-x rect)) 0))
         (y0 (if rect (max 0 (c:rect-y rect)) 0))
         (x1 (if rect (min width (c:rect-right rect)) width))
         (y1 (if rect (min height (c:rect-bottom rect)) height)))
    (loop for y from y0 below y1
          for row = (* y width)
          do (loop for x from x0 below x1
                   do (setf (sb-sys:sap-ref-32 data (* 4 (+ row x))) color)))))

(defun canvas-rect (canvas rect color &key (width 1))
  "Draw RECT's outline, WIDTH pixels thick, in COLOR."
  (let ((x (c:rect-x rect)) (y (c:rect-y rect))
        (w (c:rect-w rect)) (h (c:rect-h rect)))
    (canvas-fill canvas color (c:make-rect x y w width))
    (canvas-fill canvas color (c:make-rect x (- (+ y h) width) w width))
    (canvas-fill canvas color (c:make-rect x y width h))
    (canvas-fill canvas color (c:make-rect (- (+ x w) width) y width h))))

(defun canvas-text (canvas x y string color &key (scale 1) (tracking 0))
  "Draw STRING at (X, Y) — its top-left corner — and return the width used.

Integer scaling only, so the result is crisp at any size instead of blurry at
most of them.  At scale 1 with the shipped font this is simply text."
  (let ((data (canvas-data canvas))
        (cw (canvas-width canvas))
        (ch (canvas-height canvas))
        (pen x))
    (loop for character across string
          do (loop for row from 0 below +font-height+
                   for bits = (glyph-row character row)
                   unless (zerop bits)
                     do (loop for column from 0 below +font-width+
                              when (logbitp (- 7 column) bits)
                                do (loop for dy from 0 below scale
                                         for py = (+ y (* row scale) dy)
                                         when (< -1 py ch)
                                           do (loop for dx from 0 below scale
                                                    for px = (+ pen (* column scale) dx)
                                                    when (< -1 px cw)
                                                      do (setf (sb-sys:sap-ref-32
                                                                data (* 4 (+ (* py cw) px)))
                                                               color)))))
             (incf pen (* scale (+ +font-width+ tracking))))
    (- pen x)))

;;; ------------------------------------------------------- shell surfaces

(defclass overlay ()
  ((surface :initform nil :accessor overlay-surface)
   (shell :initform nil :accessor overlay-shell
          :documentation "river_shell_surface_v1.")
   (node :initform nil :accessor overlay-node)
   (canvas :initform nil :accessor overlay-canvas)
   (rect :initform (c:make-rect 0 0 0 0) :accessor overlay-rect)
   (visible :initform nil :accessor overlay-visible-p)
   (name :initarg :name :initform "overlay" :reader overlay-name))
  (:documentation
   "A surface of our own that river will position like a window.

Kept alive across redraws and resized only when it has to be, because a buffer
is an mmap and a file descriptor and churning them for every frame would be
both slow and a way to run out of descriptors."))

(defvar *overlays* '() "Every overlay, so relayout can place them all.")

(defun ensure-overlay (overlay width height)
  "Make OVERLAY exist and be WIDTH by HEIGHT.  Returns its canvas, or NIL.

Returns NIL when the compositor has not given us the globals we need, which
is not an error: the overlays are decoration, and a window manager that
refused to start because it could not draw a label would be a bad trade."
  (let ((compositor (server-compositor *server*))
        (shm (server-shm *server*))
        (manager (server-manager *server*)))
    (unless (and compositor shm manager) (return-from ensure-overlay nil))
    (unless (overlay-surface overlay)
      (setf (overlay-surface overlay) (wl:wl-compositor.create-surface compositor)
            (overlay-shell overlay) (w:wm-get-shell-surface
                                     manager (overlay-surface overlay))
            (overlay-node overlay) (river:river-shell-surface-v1.get-node
                                    (overlay-shell overlay)))
      (pushnew overlay *overlays*))
    (let ((canvas (overlay-canvas overlay)))
      (when (and canvas (or (/= (canvas-width canvas) width)
                            (/= (canvas-height canvas) height)))
        (destroy-canvas canvas)
        (setf canvas nil))
      (or canvas
          (setf (overlay-canvas overlay) (make-canvas shm width height))))))

(defun overlay-commit (overlay &key (rect (overlay-rect overlay)))
  "Attach the canvas, damage everything, commit, and position the surface."
  (let ((surface (overlay-surface overlay))
        (canvas (overlay-canvas overlay)))
    (when (and surface canvas)
      (setf (overlay-rect overlay) rect)
      (wl:wl-surface.attach surface (canvas-buffer canvas) 0 0)
      (wl:wl-surface.damage-buffer surface 0 0
                                   (canvas-width canvas) (canvas-height canvas))
      (wl:wl-surface.commit surface)
      (when (overlay-node overlay)
        (guarded "overlay position"
          (w:node-set-position (overlay-node overlay)
                               (c:rect-x rect) (c:rect-y rect))
          ;; Overlays are always on top of everything else.  River leaves the
          ;; initial render position undefined, so this is not optional.
          (w:node-place-top (overlay-node overlay))))
      (setf (overlay-visible-p overlay) t))))

(defun overlay-hide (overlay)
  "Stop showing OVERLAY, without destroying its buffer."
  (when (and (overlay-surface overlay) (overlay-visible-p overlay))
    (setf (overlay-visible-p overlay) nil)
    (guarded "overlay hide"
      (wl:wl-surface.attach (overlay-surface overlay) nil 0 0)
      (wl:wl-surface.commit (overlay-surface overlay)))))
