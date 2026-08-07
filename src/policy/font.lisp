;;;; policy/font.lisp --- Fonts as data, and the metrics every widget needs.
;;;;
;;;; SPLIT OUT OF POLICY/APPEARANCE.LISP, which was 832 lines because a metric
;;;; that no longer exists made it so.  An early version of gate 6 measured the
;;;; ratio of lines outside src/ to lines inside it, and appearance.lisp opens
;;;; with thirty-four lines confessing that it exists in that shape to keep the
;;;; ratio honest.  Gate 6 was replaced three commits later by a question about
;;;; *generics answered from outside*, which no arrangement of files can move.
;;;; The file kept the shape the dead metric forced: a font library, a string
;;;; library and the actual appearance policy in one place, none of which is
;;;; about either of the others.  The ratchet left residue after the pawl.
;;;;
;;;; What is here is a font *as data* -- a name, a cell size, a glyph table --
;;;; plus the pure functions that answer how wide and how tall a string will be
;;;; drawn.  Every layout decision made on screen is made out of those two
;;;; questions, which is why they are policy rather than blitter.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; FONTS
;;; ==================================================================
;;;
;;; The window manager shipped with exactly one font, wired in as three
;;; constants and a lookup table, because it draws short strings that are read
;;; at a glance and Pango would have been a C font stack in the way of the one
;;; requirement that matters -- surviving without a maintainer.  That reasoning
;;; still holds for the *default*.  It was never an argument for the font being
;;; unchangeable.
;;;
;;; A FONT here is data: a name, a cell size, and a glyph table.  The generated
;;; Terminus table registers itself as one at load time, LOAD-PSF makes one out
;;; of any console font on disk, and FONT-FOR decides which is used where.  A
;;; user who wants their terminal's font in the echo area writes two lines.

(defstruct (font (:constructor %make-font))
  "A bitmap font: fixed cell, one byte per row, bit 7 leftmost.

Deliberately a struct of plain data rather than a class with behaviour.  A
font is a table somebody may want to load from a file, hand to a method, or
build in a REPL, and none of that wants a protocol."
  (name "unnamed" :type string)
  (width 8 :type fixnum)
  (height 16 :type fixnum)
  (stride 1 :type fixnum)
  (first-code 32 :type fixnum)
  (glyphs (make-array 0 :element-type '(unsigned-byte 8))
          :type (array (unsigned-byte 8) (*))))

(defun make-font (name width height first-code glyphs)
  "A font called NAME whose glyphs are WIDTH by HEIGHT, starting at FIRST-CODE.

STRIDE -- the bytes each row occupies -- is derived rather than passed, because
it is a fact about WIDTH and there is no combination of the two that is
meaningful and disagrees."
  (%make-font :name name :width width :height height
              :stride (ceiling width 8)
              :first-code first-code :glyphs glyphs))

(defvar *fonts* (make-hash-table :test #'equal)
  "Every registered font, by name.  See REGISTER-FONT and FIND-FONT.")

(defvar *default-font* nil
  "The font used when nothing more specific is asked for.

Set by the generated font table at load time.  A defvar rather than a
define-option because it holds a font *object*: the option a user sets is
*UI-FONT*, which holds a name and can therefore appear in a config file and
survive being written down.")

(defun register-font (font)
  "Add FONT to the registry under its own name, replacing any of that name."
  (setf (gethash (font-name font) *fonts*) font))

(defun find-font (name)
  "The registered font called NAME, or NIL.  A font itself passes through,
so anywhere a font name is accepted a font object is too."
  (etypecase name
    (null nil)
    (font name)
    (string (gethash name *fonts*))
    (symbol (gethash (string-downcase (symbol-name name)) *fonts*))))

(defun font-names ()
  "Every registered font name, sorted."
  (sort (loop for name being the hash-keys of *fonts* collect name) #'string<))

(define-option *ui-font* nil
  "The font to draw interface text in, by name, or NIL for the built-in one.

    (setf *ui-font* \"ter-118n\")

Names come from FONT-NAMES.  The shipped Terminus is registered as
\"terminus\".  LOAD-PSF registers any console font on disk under its own file
name, so putting a different one everywhere is two lines in a config file:

    (register-font (load-psf \"/usr/share/kbd/consolefonts/ter-124n.psf.gz\"))
    (setf *ui-font* \"ter-124n\")

Per-role control is one method rather than one option -- see FONT-FOR.")

(defgeneric font-for (policy role)
  (:documentation
   "Which font should ROLE be drawn in?

ROLE is a keyword naming a place text appears: :ECHO for the status line,
:MINIBUFFER for the prompt, :HELP for the keymap and describe screens, :HINT
for the empty-pane hint, :OVERLAY for the coordinate overlay, :MAP for the
drawn map, and :DEFAULT for anything that has not been given a role.

The shipped answer is the same font everywhere -- *UI-FONT* if it names one,
otherwise the built-in Terminus -- because one font everywhere is what a
window manager should look like until somebody decides otherwise.  Deciding
otherwise is one method:

    (defmethod font-for ((policy conventional-policy) (role (eql :map)))
      (find-font \"ter-112n\"))

A role is a keyword rather than a subclass so that an extension can invent
one -- a widget the core has never heard of asks FONT-FOR with its own
keyword and inherits the default answer for free."))

(defmethod font-for ((policy appearance-policy) role)
  (declare (ignore role))
  (or (find-font *ui-font*) *default-font*))

;;; ------------------------------------------------------------- metrics
;;;
;;; Pure functions of a font and a string.  They live here rather than beside
;;; the blitter because "how wide is this text" is the question every layout
;;; decision on screen is made of, and a policy that cannot answer it cannot
;;; decide anything about a widget's shape.

(defun glyph-row (font character row)
  "Row ROW of CHARACTER in FONT, as an integer of (* 8 STRIDE) bits.

The leftmost pixel is the *highest* bit -- bit 7 for an eight-pixel font, bit
15 for a font up to sixteen -- so a caller walks columns with

    (logbitp (- (* 8 (font-stride font)) 1 column) bits)

which is the same expression at every width.  Fonts wider than eight pixels
are not a corner case to tolerate: Terminus ships 8, 10, 11, 12, 14 and 16
pixels wide, and every size above the smallest is one of the wide ones, so a
one-byte-per-row representation would have refused every font somebody with a
high-resolution screen actually wants.

Out of range in either direction is a blank row rather than an error: a font
that does not cover a character should leave a gap, not stop the window
manager from drawing the rest of the line."
  (let* ((code (char-code character))
         (height (font-height font))
         (stride (font-stride font))
         (first-code (font-first-code font))
         (glyphs (font-glyphs font))
         (start (+ (* (- code first-code) height stride) (* row stride))))
    (if (and (<= first-code code) (< -1 row height)
             (<= 0 start) (<= (+ start stride) (length glyphs)))
        (loop with bits = 0
              for i from 0 below stride
              do (setf bits (logior (ash bits 8) (aref glyphs (+ start i))))
              finally (return bits))
        0)))

(defun font-text-width (font string &key (scale 1) (tracking 0))
  "How wide STRING will be drawn in FONT, in pixels."
  (let ((advance (* scale (+ (font-width font) tracking))))
    (max 0 (- (* advance (length string)) (* scale tracking)))))

(defun font-text-height (font &key (scale 1))
  "How tall one line of FONT is, in pixels."
  (* scale (font-height font)))
