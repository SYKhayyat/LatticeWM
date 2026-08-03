;;;; runtime/surface.lisp --- Drawing our own pixels.
;;;;
;;;; The window manager needs surfaces of its own for the echo area, the
;;;; coordinate overlay and eventually the minimap.  River provides exactly the
;;;; mechanism: river_window_manager_v1.get_shell_surface wraps one of our
;;;; wl_surfaces so it can be positioned and ordered like a window, and gets
;;;; its own river_node_v1.
;;;;
;;;; This is the wl_shm path, which DESIGN calls "the least-proven part of
;;;; wayflan" and the cut list removed for that reason.  It is here because
;;;; both of the things it buys are, in PLAN.org's own words, the defence
;;;; against the design's biggest stated risk:
;;;;
;;;;   "DESIGN calls two-dimensional navigation 'the single biggest design
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
write is visible as soon as the surface is committed.

WIDTH and HEIGHT are *device* pixels.  SCALE is how many of those there are per
logical pixel, and every drawing function below takes logical coordinates and
multiplies.  That is the whole of HiDPI support, and it is done here rather
than at each call site for the reason it always is: there are forty call sites
and one of them would be forgotten.

Before this, everything the window manager drew itself — the echo area, the
empty-pane hint, the help screen, the coordinate overlay, the drawn map — came
out at half size on a 2x display, in the one place the program writes text for
a human to read."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (scale 1 :type fixnum)
  (stride 0 :type fixnum)
  (size 0 :type fixnum)
  (fd -1 :type fixnum)
  (data (sb-sys:int-sap 0))
  (pool nil)
  (buffer nil))

(defun canvas-logical-width (canvas)
  "The canvas's width in logical pixels."
  (floor (canvas-width canvas) (canvas-scale canvas)))

(defun canvas-logical-height (canvas)
  "The canvas's height in logical pixels."
  (floor (canvas-height canvas) (canvas-scale canvas)))

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

(defun make-canvas (shm width height &optional (scale 1))
  "A canvas WIDTH by HEIGHT *logical* pixels, at SCALE device pixels each.

The buffer is allocated at the device size and the surface is told its scale at
commit time, which is how Wayland does HiDPI: the compositor is handed more
pixels for the same area rather than the same pixels stretched."
  (let* ((scale (max 1 scale))
         (width (* scale (max 1 width)))
         (height (* scale (max 1 height)))
         (stride (* width 4))
         (size (* stride height))
         (fd (make-shm-file size))
         (data (sb-posix:mmap nil size
                              (logior sb-posix:prot-read sb-posix:prot-write)
                              sb-posix:map-shared fd 0))
         (pool (wl:wl-shm.create-pool shm fd size))
         (buffer (wl:wl-shm-pool.create-buffer pool 0 width height stride
                                               :argb8888)))
    (%make-canvas :width width :height height :scale scale
                  :stride stride :size size
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

;;; EVERY COORDINATE BELOW IS LOGICAL, and every one is multiplied by the
;;; canvas's scale before it touches a pixel.  Callers draw in the same units
;;; the layout uses and are correct on a HiDPI display without knowing one
;;; exists.

(defun canvas-fill (canvas color &optional rect)
  "Fill RECT — or the whole canvas — with COLOR.  RECT is in logical pixels.

FILL THE FIRST ROW, THEN COPY IT.  Every row of a filled rectangle is the same
bytes as every other row, so only one of them has to be computed a pixel at a
time; the rest are a `memcpy' the C library has spent decades making fast.  On
a 1920x1080 surface at 2x this is the difference between 40.7 ms and 2.7 ms,
and clearing the help overlay is a thing that happens on a keypress.

The declarations are the other half of it and they are not decoration.  This
is the innermost loop in the program — it runs once per pixel per redraw —
and without a declared SAP and declared fixnum bounds SBCL compiles the index
arithmetic as generic arithmetic, which cost 3x on its own before the row copy
was written.

SAFETY 1 RATHER THAN 0, DELIBERATELY.  This function writes into an mmap the
compositor is reading, where an off-by-one is a corrupt screen or a SIGSEGV
rather than a condition anybody can catch.  Safety 0 is a further 30% and it
buys a class of bug this project has no way to find; §honest's whole complaint
is that nothing here verifies what it cannot construct.  Not worth it.

Every product below is wrapped in `the fixnum'.  Two fixnums multiplied are a
bignum as far as the compiler is concerned, so without it SBCL emits generic
arithmetic and twenty-two compilation notes, and this project does not have a
build you scroll past."
  (declare (type (unsigned-byte 32) color))
  (let* ((data (canvas-data canvas))
         (scale (canvas-scale canvas))
         (width (canvas-width canvas))
         (height (canvas-height canvas))
         (x0 (if rect (max 0 (the fixnum (* scale (c:rect-x rect)))) 0))
         (y0 (if rect (max 0 (the fixnum (* scale (c:rect-y rect)))) 0))
         (x1 (if rect (min width (the fixnum (* scale (c:rect-right rect)))) width))
         (y1 (if rect (min height (the fixnum (* scale (c:rect-bottom rect)))) height)))
    (declare (type sb-sys:system-area-pointer data)
             (type fixnum width height x0 y0 x1 y1)
             (optimize (speed 3) (safety 1)))
    (when (and (< x0 x1) (< y0 y1))
      ;; The first row, one pixel at a time.
      (let ((row (the fixnum (* y0 width))))
        (declare (type fixnum row))
        (loop for x of-type fixnum from x0 below x1
              do (setf (sb-sys:sap-ref-32 data (the fixnum (* 4 (+ row x))))
                       color)))
      ;; Every other row is that row again.  Source and destination are
      ;; different rows of the same buffer, so they cannot overlap.
      (let ((source (the fixnum (* 4 (+ (the fixnum (* y0 width)) x0))))
            (bytes (the fixnum (* 4 (- x1 x0)))))
        (declare (type fixnum source bytes))
        (loop for y of-type fixnum from (1+ y0) below y1
              do (sb-kernel:system-area-ub8-copy
                  data source data
                  (the fixnum (* 4 (+ (the fixnum (* y width)) x0)))
                  bytes))))))

(defun canvas-rect (canvas rect color &key (width 1))
  "Draw RECT's outline, WIDTH logical pixels thick, in COLOR."
  (let ((x (c:rect-x rect)) (y (c:rect-y rect))
        (w (c:rect-w rect)) (h (c:rect-h rect)))
    (canvas-fill canvas color (c:make-rect x y w width))
    (canvas-fill canvas color (c:make-rect x (- (+ y h) width) w width))
    (canvas-fill canvas color (c:make-rect x y width h))
    (canvas-fill canvas color (c:make-rect (- (+ x w) width) y width h))))

(defun canvas-text (canvas x y string color
                    &key (scale 1) (tracking 0) (font (current-font)))
  "Draw STRING at (X, Y) — its top-left corner — and return the width used.

X, Y and the returned width are *logical* pixels; SCALE is the integer font
scale on top of the canvas's own device scale, so text on a 2x display at font
scale 1 is drawn with twice the pixels and comes out the same size, crisp.

Integer scaling only, so the result is sharp at any size instead of blurry at
most of them.  At scale 1 with the shipped font this is simply text.

FONT defaults to whatever the policy answers for :DEFAULT.  The cell size is
read from the font rather than from a constant, so a font of another size
draws correctly here without anything else in the runtime being told.

DECLARED FOR THE SAME REASON `canvas-fill' IS, and see its docstring for why
SAFETY stays at 1.  Four nested loops around a `sap-ref-32' is the shape that
punishes generic arithmetic hardest: 1.7x here, on a status line redrawn every
time anything at all changes.  There is no row-copy trick available — a glyph
is different on every row, which is what makes it a glyph."
  (declare (type (unsigned-byte 32) color) (type string string))
  (let* ((data (canvas-data canvas))
         (device (canvas-scale canvas))
         (total (the fixnum (* scale device)))
         (cw (canvas-width canvas))
         (ch (canvas-height canvas))
         (font-height (p:font-height font))
         (font-width (p:font-width font))
         (top-bit (the fixnum (1- (the fixnum (* 8 (p:font-stride font))))))
         (origin-x (the fixnum (* device x)))
         (origin-y (the fixnum (* device y)))
         (pen origin-x))
    (declare (type sb-sys:system-area-pointer data)
             (type fixnum device total cw ch font-height font-width
                   top-bit origin-x origin-y pen scale tracking)
             (optimize (speed 3) (safety 1)))
    (loop for character across string
          do (loop for row of-type fixnum from 0 below font-height
                   ;; A glyph row is (* 8 STRIDE) bits wide and STRIDE is 1 or
                   ;; 2 for every console font there is, so FIXNUM rather than
                   ;; UNSIGNED-BYTE: the latter is unbounded, and an unbounded
                   ;; integer makes the LOGBITP comparison below generic.
                   for bits of-type fixnum = (p:glyph-row font character row)
                   unless (zerop bits)
                     do (loop for column of-type fixnum from 0 below font-width
                              when (logbitp (- top-bit column) bits)
                                do (loop for dy of-type fixnum from 0 below total
                                         for py of-type fixnum
                                           = (+ origin-y (the fixnum (* row total)) dy)
                                         when (< -1 py ch)
                                           do (loop for dx of-type fixnum from 0 below total
                                                    for px of-type fixnum
                                                      = (+ pen
                                                           (the fixnum (* column total))
                                                           dx)
                                                    when (< -1 px cw)
                                                      do (setf (sb-sys:sap-ref-32
                                                                data
                                                                (the fixnum
                                                                     (* 4 (+ (the fixnum
                                                                                  (* py cw))
                                                                             px))))
                                                               color)))))
             (incf pen (the fixnum (* total (+ font-width tracking)))))
    ;; Logical, so a caller's pen arithmetic stays in the units it started in.
    ;; Exact: the advance is a whole multiple of the device scale.
    (floor (- pen origin-x) device)))

;;; ------------------------------------------------------- shell surfaces

(defclass overlay ()
  ((surface :initform nil :accessor overlay-surface)
   (shell :initform nil :accessor overlay-shell
          :documentation "river_shell_surface_v1.")
   (node :initform nil :accessor overlay-node)
   (canvas :initform nil :accessor overlay-canvas)
   (rect :initform (c:make-rect 0 0 0 0) :accessor overlay-rect)
   (visible :initform nil :accessor overlay-visible-p)
   (name :initarg :name :initform "overlay" :reader overlay-name)
   (kind :initarg :kind :initform :overlay :reader overlay-kind
         :documentation
         "What this overlay is for: :ECHO, :CURSOR, :HELP, or an extension's own
keyword.  Half of the registry key.")
   (output :initarg :output :initform nil :reader overlay-output
           :documentation
           "The output this overlay belongs to.  The other half of the key.")
   (props :initform '() :accessor c:props
          :documentation "Extension state, as on a node.  Used by the
incremental redraw to remember what it cleared last time."))
  (:documentation
   "A surface of our own that river will position like a window.

Kept alive across redraws and resized only when it has to be, because a buffer
is an mmap and a file descriptor and churning them for every frame would be
both slow and a way to run out of descriptors.

*ONE PER OUTPUT, ALWAYS.*  Every shipped overlay used to be a single global —
one `*echo-overlay*', one `*cursor-overlay*', one `*help-overlay*' — which
meant the echo area existed on exactly one monitor and stayed there when the
cursor left, the empty-pane outlines were drawn on monitor 1 whichever monitor
the empty panes were on, and a user could not have two of anything.  A surface
is per output in Wayland for the same reason a window is; making the *object*
per output is what lets the drawing code stop pretending otherwise."))

(defvar *overlays* (make-hash-table :test #'equal)
  "(KIND . OUTPUT) -> OVERLAY.  Every overlay in the program, so that a
relayout can place them all and an unplugged monitor can take its own with it.")

(defun overlay-for (kind output)
  "The overlay of KIND belonging to OUTPUT, made on demand.

This is how a drawing routine gets a surface, and it is deliberately the only
way: an overlay held in a global of its own is an overlay that cannot follow
the cursor to the second monitor and cannot be torn down when that monitor is
unplugged.  An extension asks for its own KIND and inherits both behaviours."
  (let ((key (cons kind output)))
    (or (gethash key *overlays*)
        (setf (gethash key *overlays*)
              (make-instance 'overlay
                             :name (string-downcase (string kind))
                             :kind kind :output output)))))

(defun all-overlays (&optional kind)
  "Every overlay, or every overlay of KIND."
  (let ((out '()))
    (maphash (lambda (key overlay)
               (when (or (null kind) (eq kind (car key)))
                 (push overlay out)))
             *overlays*)
    out))

(defun destroy-overlay (overlay)
  "Release everything OVERLAY holds and forget it.

Called when a monitor is unplugged.  Not doing this leaked a shell surface, a
river node, an mmap and a file descriptor per overlay per hotplug — which on a
laptop that is docked and undocked twice a day is a real leak with a slow fuse."
  (when overlay
    (let ((canvas (overlay-canvas overlay)))
      (when canvas (ignore-errors (destroy-canvas canvas))))
    (setf (overlay-canvas overlay) nil)
    (let ((node (overlay-node overlay)))
      (when node (ignore-errors (river:river-node-v1.destroy node))))
    (let ((shell (overlay-shell overlay)))
      (when shell (ignore-errors (river:river-shell-surface-v1.destroy shell))))
    (let ((surface (overlay-surface overlay)))
      (when surface (ignore-errors (wl:wl-surface.destroy surface))))
    (setf (overlay-node overlay) nil
          (overlay-shell overlay) nil
          (overlay-surface overlay) nil
          (overlay-visible-p overlay) nil)
    (remhash (cons (overlay-kind overlay) (overlay-output overlay)) *overlays*))
  nil)

(defun forget-overlays-for-output (output)
  "Destroy every overlay that belonged to OUTPUT.  For hotplug."
  (dolist (overlay (all-overlays))
    (when (eq output (overlay-output overlay))
      (guarded "destroy overlay" (destroy-overlay overlay))))
  nil)

(defun hide-overlays (kind &key except)
  "Hide every overlay of KIND except the one on the EXCEPT output.

What a *single*-output decoration does on a multi-output desktop: the help
screen and the drawn map belong where you are looking, not on every monitor at
once, and the one they were on last has to be told to go away."
  (dolist (overlay (all-overlays kind))
    (unless (eq (overlay-output overlay) except)
      (guarded "hide overlay" (overlay-hide overlay))))
  nil)

(defun overlay-scale (overlay)
  "How many device pixels per logical pixel OVERLAY should be drawn at.

The scale of the output it belongs to, clamped to at least 1.  An overlay with
no output — nothing ships one, but an extension might — draws at 1, which is
correct on every display and merely soft on a scaled one."
  (let ((output (overlay-output overlay)))
    (max 1 (if output (or (c:output-scale output) 1) 1))))

(defun ensure-overlay (overlay width height)
  "Make OVERLAY exist and be WIDTH by HEIGHT *logical* pixels.

Returns its canvas, or NIL when the compositor has not given us the globals we
need — which is not an error: the overlays are decoration, and a window manager
that refused to start because it could not draw a label would be a bad trade.

The canvas is reallocated when the logical size changes *or* when the output's
scale does, so moving a window between a 1x and a 2x monitor redraws at the
right resolution rather than at the last one's."
  (let ((compositor (server-compositor *server*))
        (shm (server-shm *server*))
        (manager (server-manager *server*))
        (scale (overlay-scale overlay)))
    (unless (and compositor shm manager) (return-from ensure-overlay nil))
    (unless (overlay-surface overlay)
      (setf (overlay-surface overlay) (wl:wl-compositor.create-surface compositor)
            (overlay-shell overlay) (w:wm-get-shell-surface
                                     manager (overlay-surface overlay))
            (overlay-node overlay) (river:river-shell-surface-v1.get-node
                                    (overlay-shell overlay))))
    (let ((canvas (overlay-canvas overlay)))
      (when (and canvas (or (/= (canvas-logical-width canvas) (max 1 width))
                            (/= (canvas-logical-height canvas) (max 1 height))
                            (/= (canvas-scale canvas) scale)))
        (destroy-canvas canvas)
        (setf canvas nil))
      (or canvas
          (setf (overlay-canvas overlay) (make-canvas shm width height scale))))))

(defun overlay-commit (overlay &key (rect (overlay-rect overlay)))
  "Attach the canvas, damage everything, commit, and position the surface."
  (let ((surface (overlay-surface overlay))
        (canvas (overlay-canvas overlay)))
    (when (and surface canvas)
      (setf (overlay-rect overlay) rect)
      ;; Tell the compositor how many device pixels per logical one this buffer
      ;; carries, *before* attaching it.  Without this a 2x buffer is drawn at
      ;; twice the size rather than at twice the resolution.
      (when (> (canvas-scale canvas) 1)
        (guarded "set_buffer_scale"
          (wl:wl-surface.set-buffer-scale surface (canvas-scale canvas))))
      (wl:wl-surface.attach surface (canvas-buffer canvas) 0 0)
      ;; Damage in *buffer* coordinates, which are device pixels.
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
  "Stop showing OVERLAY, and release its buffer unless told otherwise."
  (when (and (overlay-surface overlay) (overlay-visible-p overlay))
    (setf (overlay-visible-p overlay) nil)
    (guarded "overlay hide"
      (wl:wl-surface.attach (overlay-surface overlay) nil 0 0)
      (wl:wl-surface.commit (overlay-surface overlay))))
  (when (and p:*overlay-buffer-idle* (overlay-canvas overlay))
    (destroy-canvas (overlay-canvas overlay))
    (setf (overlay-canvas overlay) nil))
  nil)
