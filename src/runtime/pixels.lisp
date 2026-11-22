;;;; runtime/pixels.lisp --- Asking the compositor for a picture of a window.
;;;;
;;;; This program draws only what it draws.  The echo area, the overlays, the
;;;; deck's boxes and titles: all of it is our own wl_shm canvases.  What it has
;;;; never been able to draw is a *window* -- somebody else's surface, whose
;;;; pixels live in the compositor's memory and nowhere else (#130).
;;;;
;;;; The standard extensions for that arrived in wayland-protocols as three
;;;; protocols that chain:
;;;;
;;;;   ext-foreign-toplevel-list-v1          the compositor names its toplevels
;;;;   ext-image-capture-source-v1           a handle becomes a capture source
;;;;   ext-image-copy-capture-v1             copy a source into a buffer we own
;;;;
;;;; and the order matters twice: a session is made FROM a source, and a source
;;;; is made FROM a handle.
;;;;
;;;; ONE COPY PER REQUEST.  This is the whole reason the thing is affordable.
;;;; The older alternative (wlr-screencopy) is a frame per frame; a thumbnail
;;;; that costs a video feed is a thumbnail nobody should ship.  Here the
;;;; compositor copies one frame into a buffer we hand it and otherwise does
;;;; nothing at all, so the cost of a deck of thumbnails is the cost of a
;;;; screenshot, taken when somebody looked.
;;;;
;;;; NOTHING IN HERE RUNS ON ITS OWN.  There is no timer, no refresh loop and no
;;;; cache behind the user's back: CAPTURE-WINDOW is called from a drawing path,
;;;; answers (or answers NIL), and the pixels are as old as the look.  A stale
;;;; picture of a window is the honest picture of a window, because the window
;;;; may well have changed in the meantime and this program is not watching it.
;;;;
;;;; A MISSING GLOBAL IS AN ORDINARY STATE.  Every entry point answers NIL when
;;;; the compositor does not offer these protocols, because "your compositor
;;;; does not support this" is information and a crash is not.  The floor this
;;;; project supports is a river without them.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------------- the handles

;;; The compositor's names for its windows are NOT our names for its windows.
;;; A river handle arrives on river_window_manager_v1 and is ours; an
;;; ext_foreign_toplevel_handle_v1 arrives on a separate list and is a stranger.
;;; The only thing the two have in common is what they say about themselves, so
;;; the match is by app-id and then by title -- and an ambiguous match answers
;;; NIL rather than picking one, because a thumbnail of the wrong window is
;;; worse than no thumbnail at all.

(defstruct toplevel
  "What one EXT_FOREIGN_TOPLEVEL_HANDLE_V1 says about itself."
  handle
  app-id
  title
  identifier
  (closed nil))

(defun toplevel-list (server)
  (server-toplevels server))

(defun forget-toplevel (server handle)
  (setf (server-toplevels server)
        (delete handle (server-toplevels server) :key #'toplevel-handle)))

(defun match-toplevel (server app-id title)
  "The one toplevel whose APP-ID (or, failing that, TITLE) matches, or NIL.

APP-ID first: two windows of one application share it, so a title is only
consulted when the app-id match is ambiguous, and only an unambiguous answer is
returned.  A window with neither is not matched at all."
  (let ((open (remove-if #'toplevel-closed (server-toplevels server))))
    (labels ((only (candidates)
               (and (= 1 (length candidates)) (first candidates))))
      (or (and app-id (only (remove app-id open
                                    :key #'toplevel-app-id :test-not #'string=)))
          (and title (only (remove title open
                                   :key #'toplevel-title :test-not #'string=)))))))

;;; ------------------------------------------------------------- the globals

(defun capture-global-p (interface)
  "T for the three interfaces this file binds, so BIND-GLOBALS stays a list."
  (member interface '("ext_foreign_toplevel_list_v1"
                      "ext_foreign_toplevel_image_capture_source_manager_v1"
                      "ext_image_copy_capture_manager_v1")
          :test #'string=))

(defun bind-capture-globals (server registry name interface version)
  "Bind the capture globals, in the order they depend on each other.

Answers T when INTERFACE was one of them.  Kept beside BIND-ONE-GLOBAL rather
than inside it so the interface names are in one readable list."
  (declare (ignore version))
  (cond
    ((string= interface "ext_foreign_toplevel_list_v1")
     (let ((list (wl:wl-registry.bind registry name
                                      'wp:ext-foreign-toplevel-list-v1 1)))
       (setf (server-toplevel-list server) list)
       (on-events (list "ext_foreign_toplevel_list_v1")
                  (:toplevel (adopt-toplevel server (first arguments)))
                  (:finished (setf (server-toplevel-list server) nil))
                  (t nil))
       t))
    ((string= interface "ext_foreign_toplevel_image_capture_source_manager_v1")
     (setf (server-toplevel-capture-source-manager server)
           (wl:wl-registry.bind
            registry name
            'wp:ext-foreign-toplevel-image-capture-source-manager-v1 1))
     t)
    ((string= interface "ext_image_copy_capture_manager_v1")
     (setf (server-capture-manager server)
           (wl:wl-registry.bind registry name
                                'wp:ext-image-copy-capture-manager-v1 1))
     t)
    (t nil)))

(defun adopt-toplevel (server handle)
  "Start following one foreign toplevel handle."
  (let ((entry (make-toplevel :handle handle)))
    (push entry (server-toplevels server))
    (on-events (handle "ext_foreign_toplevel_handle_v1")
               (:app-id (setf (toplevel-app-id entry) (first arguments)))
               (:title (setf (toplevel-title entry) (first arguments)))
               (:identifier (setf (toplevel-identifier entry) (first arguments)))
               (:done nil)
               (:closed (progn
                          (setf (toplevel-closed entry) t)
                          (forget-toplevel server (toplevel-handle entry))))
               (t nil))
    entry))

(defun capture-available-p (&optional (server *server*))
  "T when this compositor can be asked for a window's pixels."
  (and server
       (server-capture-manager server)
       (server-toplevel-capture-source-manager server)))

;;; ------------------------------------------------------------ the buffer

(defstruct (pixels (:constructor %make-pixels) (:copier nil))
  "A captured frame: WIDTH x HEIGHT pixels in ARGB8888, one word each.

COPIED OUT OF THE SHARED MEMORY on the way in, deliberately.  The mapping is
the compositor's to read as well as ours, and a picture handed to a drawing
routine must not change under it when the next frame lands -- so the frame is
copied once, the mapping is dropped, and what the drawer holds is its own."
  width
  height
  (data nil))

(defun xrgb-to-argb (sap width height)
  "A vector of WIDTH x HEIGHT words from SAP, with the alpha byte made opaque.

A CAPTURE IS XRGB8888, where the top byte is undefined (in practice zero); the
canvas is ARGB8888 premultiplied, where a zero top byte is a fully transparent
pixel.  Copying the words as they arrive draws an invisible thumbnail, which is
the sort of thing that looks like a capture failure and is not one.  A
premultiplied opaque colour and an unpremultiplied one are the same bytes when
alpha is 255, so OR-ing the top byte in is the whole conversion."
  (let ((count (* width height))
        (out (make-array (* width height) :element-type '(unsigned-byte 32))))
    (dotimes (i count)
      (setf (aref out i)
            (logior #xff000000 (ldb (byte 24 0) (sb-sys:sap-ref-32 sap (* 4 i))))))
    out))

;;; ------------------------------------------------------------- the frame

(defun pump-until (display predicate &key (limit 2000))
  "Dispatch events until PREDICATE answers T, or LIMIT events have gone by.

A LIMIT RATHER THAN A BLOCKING WAIT, and it is the whole safety story of this
file: a compositor that never answers must not hang the window manager inside a
drawing routine.  Two thousand events is several seconds of ordinary traffic,
and a deck that cannot get a picture in that time draws its boxes instead."
  (let ((seen 0))
    (loop until (or (funcall predicate) (> seen limit))
          do (dispatch-one-event display)
             (incf seen))
    (funcall predicate)))

(defun capture-window (window &key (server *server*) (display (server-display *server*)))
  "A PIXELS of WINDOW's contents at the moment it was asked, or NIL.

NIL means no picture: the compositor does not offer the protocols, the window
has no foreign toplevel to match, the match was ambiguous, the frame failed, or
the compositor stopped answering.  Every one of those is a reason to draw the
deck's box instead, never a reason to signal -- a thumbnail is decoration and a
decoration that can take the session down is not worth having."
  (when (and server (capture-available-p server) window display)
    (let* ((app-id (c:window-app-id window))
           (title (c:window-title window))
           (toplevel (match-toplevel server app-id title)))
      (when toplevel
        (capture-toplevel server display toplevel)))))

(defun capture-toplevel (server display toplevel)
  "The frame-copy sequence: source, session, size, buffer, frame, copy, read."
  (let ((source nil) (session nil) (frame nil) (pool nil) (buffer nil)
        (data nil) (size 0)
        (width 0) (height 0) (ready nil) (failed nil) (done nil)
        (result nil))
    (unwind-protect
         (progn
           (setf source
                 (wp:ext-foreign-toplevel-image-capture-source-manager-v1.create-source
                  (server-toplevel-capture-source-manager server)
                  (toplevel-handle toplevel)))
           ;; THE OPTIONS ARGUMENT IS A BITFIELD, so wayflan wants a *list* of
           ;; keywords and not the integer 0: `no options' is ().  The one bit
           ;; is :PAINT-CURSORS, which a thumbnail does not want -- the deck
           ;; draws the window, not whatever pointer happened to be over it.
           (setf session (wp:ext-image-copy-capture-manager-v1.create-session
                          (server-capture-manager server) source '()))
           ;; THE SIZE COMES FROM THE COMPOSITOR, not from the window's own
           ;; rectangle: the copy is of what is on screen, and a window that is
           ;; partly off an output still captures at its full size.
           (on-events (session "ext_image_copy_capture_session_v1")
                      (:buffer-size (setf width (first arguments)
                                          height (second arguments)))
                      (:done (setf done t))
                      (:stopped (setf failed t))
                      (t nil))
           (pump-until display (lambda () (or done failed)))
           (when (and done (plusp width) (plusp height))
             (multiple-value-setq (pool buffer data size)
               (make-capture-buffer (server-shm server) width height))
             (when buffer
               (setf frame (wp:ext-image-copy-capture-session-v1.create-frame
                            session))
               (on-events (frame "ext_image_copy_capture_frame_v1")
                          (:ready (setf ready t))
                          (:failed (setf failed t))
                          (t nil))
               (wp:ext-image-copy-capture-frame-v1.attach-buffer frame buffer)
               (wp:ext-image-copy-capture-frame-v1.damage-buffer
                frame 0 0 width height)
               (wp:ext-image-copy-capture-frame-v1.capture frame)
               (pump-until display (lambda () (or ready failed)))
               (when (and ready data)
                 (setf result (%make-pixels :width width :height height
                                            :data (xrgb-to-argb data width height)))
                 (setf data nil))))
           result)
      ;; The order that matters: the frame before the session that owns it, the
      ;; session before the source it was made from.
      (when frame (ignore-errors (wp:ext-image-copy-capture-frame-v1.destroy frame)))
      (when session (ignore-errors (wp:ext-image-copy-capture-session-v1.destroy session)))
      (when data (ignore-errors (sb-posix:munmap data size)))
      (when buffer (ignore-errors (wl:wl-buffer.destroy buffer)))
      (when pool (ignore-errors (wl:wl-shm-pool.destroy pool)))
      (when source (ignore-errors (wp:ext-image-capture-source-v1.destroy source))))))

(defun make-capture-buffer (shm width height)
  "An XRGB8888 wl_buffer of WIDTH x HEIGHT, its pool, its bytes and its size.

A SECOND USER OF THE SHM PATH, not a second implementation of it: the same
memfd, the same mmap, the same create-buffer request the canvases use.  What
differs is the format -- XRGB8888, the one every ext-image-copy-capture
compositor must support -- and that nothing ever draws into it.

Answers the mapping as well as the buffer, because the caller has to read the
pixels out before the memory goes away: the descriptor is closed after the pool
is made (exactly as the canvas path does), so a later reader would have nothing
to open."
  (let* ((stride (* width 4))
         (size (* stride height))
         (fd (make-shm-file size))
         (data nil)
         (pool nil)
         (buffer nil)
         (finished nil))
    (unwind-protect
         (progn
           (setf data (sb-posix:mmap nil size
                                     (logior sb-posix:prot-read sb-posix:prot-write)
                                     sb-posix:map-shared fd 0))
           (setf pool (wl:wl-shm.create-pool shm fd size))
           (setf buffer (wl:wl-shm-pool.create-buffer pool 0 width height
                                                      stride :xrgb8888))
           (ignore-errors (sb-posix:close fd))
           (setf fd -1 finished t)
           (values pool buffer data size))
      (unless finished
        (when buffer (ignore-errors (wl:wl-buffer.destroy buffer)))
        (when pool (ignore-errors (wl:wl-shm-pool.destroy pool)))
        (when data (ignore-errors (sb-posix:munmap data size))))
      (when (and (not finished) (>= fd 0)) (ignore-errors (sb-posix:close fd))))))

(defun pixels-checksum (pixels)
  "A cheap sum over the pixel words, for saying \"a frame arrived, and it is not
an empty one\" without printing eight million bytes."
  (let ((data (pixels-data pixels))
        (sum 0)
        (non-zero 0))
    (loop for word of-type (unsigned-byte 32) across data
          do (incf sum (ldb (byte 24 0) word))
             (when (plusp (ldb (byte 24 0) word)) (incf non-zero)))
    (values (logand #xffffffff sum) non-zero)))

(defun capture-report (&optional (name nil))
  "What this compositor can do about window pixels, as a plist.

The seam the command and the integration harness share: a command prints, a
test reads, and neither has to parse the other's prose."
  (let* ((server *server*)
         (world *world*)
         (toplevels (and server (remove-if #'toplevel-closed
                                           (server-toplevels server))))
         (window (cond ((null world) nil)
                       (name (find name (c:node-windows (c:world-root world))
                                   :key (lambda (w)
                                          (or (c:window-app-id w)
                                              (c:window-title w)))
                                   :test #'string=))
                       (t (and p:*seat* (seat-focused p:*seat*))))))
    (cond
      ((null server) (list :available nil :reason "no session"))
      ((null (server-capture-manager server))
       (list :available nil :reason "no ext-image-copy-capture-v1"
             :toplevels (length toplevels)))
      ((null (server-toplevel-capture-source-manager server))
       (list :available nil
             :reason "no ext-foreign-toplevel-image-capture-source-manager-v1"
             :toplevels (length toplevels)))
      ((null toplevels)
       (list :available nil :reason "no toplevels listed" :toplevels 0))
      ((null window)
       (list :available t :reason "no such window" :toplevels (length toplevels)))
      (t
       (let ((matched (match-toplevel server (c:window-app-id window)
                                      (c:window-title window)))
             (pixels (capture-window window :server server)))
         (list :available t
               :reason (if pixels nil "the frame did not arrive")
               :toplevels (length toplevels)
               :matched (and matched t)
               :app-id (c:window-app-id window)
               :width (and pixels (pixels-width pixels))
               :height (and pixels (pixels-height pixels))
               :checksum (and pixels (nth-value 0 (pixels-checksum pixels)))
               :non-zero (and pixels (nth-value 1 (pixels-checksum pixels)))
               :words (and pixels (length (pixels-data pixels)))))))))

(defcommand capture-probe (&optional (name nil))
  "Say whether this compositor can be asked for a window's pixels, and try once.

NAME is an app-id or a title.  With no name, the window with the keyboard is
used.

This is the diagnostic the protocol work is verified by, and it is deliberately
not a thumbnail: it reports whether the three globals are bound, how many
foreign toplevels the compositor listed, which of them matched, and -- when a
frame arrived -- its size and a checksum of the pixels.  The checksum is the
check that catches \"a frame arrived, and it is a black rectangle\"."
  (:interactive :string)
  (let ((report (capture-report name)))
    (cond
      ((not (getf report :available))
       (notify "no window pixels: ~a (~:d toplevel~:p listed)"
               (getf report :reason) (or (getf report :toplevels) 0)))
      ((getf report :reason)
       (notify "window pixels: ~a (~d toplevel~:p listed)"
               (getf report :reason) (getf report :toplevels 0)))
      (t
       (notify "captured ~a: ~dx~d, checksum ~x, ~d/~d words non-zero"
               (getf report :app-id)
               (getf report :width) (getf report :height)
               (getf report :checksum)
               (getf report :non-zero) (getf report :words))))))
