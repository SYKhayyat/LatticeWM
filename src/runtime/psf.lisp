;;;; runtime/psf.lisp --- Load a console font off the disk.
;;;;
;;;; policy/appearance.lisp made "which font gets drawn where" a decision.
;;;; This is what gives that decision something to choose between.  Reading
;;;; bytes off a disk is mechanism, so it lives here; nothing in this file has
;;;; an opinion about where the font it returns should be used.
;;;;
;;;; PSF is the Linux console font format, which means every distribution
;;;; already ships a few dozen of them under /usr/share/kbd/consolefonts/ and
;;;; the user does not have to find, convert or install anything to change the
;;;; font.  That is the whole reason for picking it over BDF or a TTF stack:
;;;; the fonts are already on the machine.

(in-package #:latticewm/runtime)

(defparameter +psf1-magic+ '(#x36 #x04))
(defparameter +psf2-magic+ '(#x72 #xB5 #x4A #x86))

(defun read-file-bytes (path)
  "PATH as a byte vector, transparently un-gzipping a .gz.

Shelling out to gzip rather than vendoring an inflate: console fonts ship
gzipped almost everywhere, gzip is on every machine that has a Linux console,
and this is a file read at configuration time rather than anything on a hot
path.  A missing gzip produces a condition naming it, which is a better error
than a corrupt-font one."
  (let ((path (probe-file path)))
    (unless path (error "No such font file: ~a" path))
    (if (string-equal "gz" (pathname-type path))
        ;; :EXTERNAL-FORMAT :LATIN-1 rather than a byte element type.  Asking
        ;; RUN-PROGRAM for (:string :element-type (unsigned-byte 8)) reads the
        ;; pipe as *text* anyway and dies on the first octet that is not valid
        ;; in the default encoding — which for a font table is immediate, and
        ;; the failure surfaced as "font did not change" rather than as an
        ;; error, because the caller fell back to the built-in one.  Latin-1 is
        ;; the encoding whose code points are its bytes, so this round-trips
        ;; every octet exactly.
        (let ((out (uiop:run-program (list "gzip" "-dc" (namestring path))
                                     :output :string
                                     :external-format :latin-1
                                     :error-output nil)))
          (map '(simple-array (unsigned-byte 8) (*)) #'char-code out))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (let ((bytes (make-array (file-length in)
                                   :element-type '(unsigned-byte 8))))
            (read-sequence bytes in)
            bytes)))))

(defun %u32 (bytes offset)
  "The little-endian 32-bit word at OFFSET."
  (loop for i from 0 below 4
        sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun %psf-geometry (bytes)
  "(values HEADER-SIZE WIDTH HEIGHT COUNT) for the PSF in BYTES.

PSF1 is a four-byte header with a fixed width of eight; PSF2 says everything
about itself.  Both are handled because /usr/share/kbd/consolefonts/ on a
current distribution still contains a mixture."
  (cond
    ((and (> (length bytes) 4)
          (every #'= +psf1-magic+ (list (aref bytes 0) (aref bytes 1))))
     (let ((charsize (aref bytes 3))
           (mode (aref bytes 2)))
       (values 4 8 charsize (if (logbitp 0 mode) 512 256))))
    ((and (> (length bytes) 32)
          (every #'= +psf2-magic+ (list (aref bytes 0) (aref bytes 1)
                                        (aref bytes 2) (aref bytes 3))))
     (values (%u32 bytes 8) (%u32 bytes 28) (%u32 bytes 24) (%u32 bytes 16)))
    (t (error "Not a PSF font: no PSF1 or PSF2 magic."))))

(defun load-psf (path &optional name)
  "Load the PSF console font at PATH and return it.  Does not register it.

    (p:register-font (load-psf \"/usr/share/kbd/consolefonts/ter-124n.psf.gz\"))
    (setf p:*ui-font* \"ter-124n\")

NAME defaults to the file's own name with its suffixes removed, because that
is what the user will type and what FONT-NAMES will show them.

Any width is accepted.  An earlier version of this capped it at eight pixels,
on the reasoning that the representation is one byte per row and Terminus's
larger sizes are taller rather than wider.  That reasoning was simply wrong,
and running it said so immediately: ter-122b is eleven pixels wide and
ter-d28n is fourteen, so the cap refused every size above the smallest --
which is to say every font somebody changing the font would be reaching
for."
  (let* ((bytes (read-file-bytes path))
         (name (or name (pathname-name (if (string-equal
                                            "gz" (pathname-type path))
                                           (pathname-name path)
                                           path)))))
    (multiple-value-bind (header width height count) (%psf-geometry bytes)
      ;; The geometry fields are read straight off the file, and the allocation
      ;; below is (min 95 count-32) * height * ceil(width/8).  A malformed or
      ;; hostile font with a huge width or height forces a multi-gigabyte
      ;; MAKE-ARRAY -- a denial of service (not an overrun: the glyph copy is
      ;; clamped).  Refuse anything past a sane console font; Terminus, the
      ;; largest anyone reaches for, tops out around 14x32.
      (unless (and (integerp width) (integerp height) (integerp count)
                   (<= 1 width 256) (<= 1 height 256) (<= 0 count 65536))
        (error "~a: implausible PSF geometry (~sx~s, ~s glyphs); refusing to load."
               name width height count))
      (let* ((stride (ceiling width 8))
             (charsize (* height stride))
             (wanted (- 127 32))          ; the printable ASCII this draws
             (available (max 0 (- count 32)))
             (glyphs (make-array (* (min wanted available) charsize)
                                 :element-type '(unsigned-byte 8))))
        (when (< available wanted)
          ;; AVAILABLE, not COUNT: COUNT includes the 32 control-character
          ;; glyphs this never draws, so it overstated the printable coverage.
          (logmsg :warn "~a covers only ~d printable glyph~:p; the rest will be blank"
                  name available))
        ;; Start at code 32.  Everything drawn here is printable ASCII, and
        ;; copying the control-character glyphs would be thirty-two rows of
        ;; nothing sitting in front of every lookup.
        (loop for code from 32 below (min 127 count)
              for source = (+ header (* code charsize))
              for target = (* (- code 32) charsize)
              do (replace glyphs bytes :start1 target
                                       :start2 source
                                       :end2 (min (length bytes)
                                                  (+ source charsize))))
        (p:make-font name width height 32 glyphs)))))

(defcommand (load-font "load-font") (command)
  "Load a PSF console font from a path and use it everywhere.

    M-x load-font  /usr/share/kbd/consolefonts/ter-124n.psf.gz

The whole of what changing the font takes, from a prompt, with no restart:
the overlays are redrawn from the new metrics on the next frame.  For a
font in one place only, write a FONT-FOR method instead."
  (:interactive :shell-command)
  (handler-case
      (let ((font (load-psf (string-trim " " command))))
        (p:register-font font)
        (setf p:*ui-font* (p:font-name font))
        (mark-dirty)
        (notify "font: ~a (~dx~d)" (p:font-name font)
                (p:font-width font) (p:font-height font)))
    (error (condition) (notify "~a" condition))))
