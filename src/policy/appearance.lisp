;;;; policy/appearance.lisp --- What the window manager draws, as decisions.
;;;;
;;;; GATE 6, AND WHY THIS FILE EXISTS
;;;;
;;;; Gate 6 reports (model + policy + lattice) against (wire + runtime), on the
;;;; argument in PLAN §extensibility-real: Lisp is not what kept Emacs alive,
;;;; the *ratio* is.  1.3 million lines of Elisp on 400,000 of C means every
;;;; feature is a worked example of how to write a feature.  Vim, Neovim and
;;;; Hyprland all have a scripting language and none of them is Emacs; the
;;;; boundary is not the disease, how little of the system lives above it is.
;;;;
;;;; The number went 1.20 -> 0.82 -> 0.80 as the drawing and interaction
;;;; subsystem was built, and PLAN §log2 left the question open: is gate 6
;;;; measuring the wrong pair, or is the decomposition failing?  §log3 rules:
;;;; *neither*.  The widget layer should become extensible, and the ratio
;;;; should move because code crosses the line — not because the line moves.
;;;;
;;;; Gate 6 counts by directory, which is not a flaw in it.  It is the gate
;;;; insisting the decomposition be real: a DEFINE-OPTION that lives in
;;;; src/runtime/ is still something a user has to read the runtime to find.
;;;;
;;;; So the split here is the Emacs one.  The C core does redisplay primitives;
;;;; Elisp decides what goes on the mode line.  Shared-memory buffers, the fd
;;;; path, glyph blitting and surface lifecycle stay in src/runtime/.  Colours,
;;;; scales, what a status line is made of, how many columns a help screen
;;;; uses, and *which font gets drawn for which role* are decisions, and they
;;;; live here.

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

(defmethod font-for ((policy policy) role)
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

;;; ==================================================================
;;; THE ECHO AREA
;;; ==================================================================

(define-option *echo-area* t
  "Show a status line along the bottom of the screen.

It reports where the cursor is, what is on this workspace, and the last
message.  Turn it off if you want the screen entirely to yourself; you will
lose the only permanent indication of which cell you are in.")

(define-option *echo-height* 24
  "Height of the echo area in pixels.")

(define-option *echo-scale* 1
  "Integer scale factor for echo-area text.

1 is the font's own size — sixteen pixels tall, which is an ordinary status
bar.  2 is for a HiDPI display, where it is again ordinary rather than large.
There is no half step, deliberately: a bitmap font scaled by anything but a
whole number is a smear, and a bitmap font scaled by a whole number is crisp
at any size.")

(define-option *echo-position* :bottom
  "Which edge the echo area sits on: :BOTTOM or :TOP.

Bottom is Emacs's minibuffer and is the default.  Top is worth knowing about
for two situations: running nested inside another desktop whose panel is along
the bottom, and any setup where something else already owns that edge.")

(define-option *echo-background* '(0.09 0.09 0.12 0.92)
  "Echo area background, as (R G B A).  Slightly translucent by default so it
reads as an overlay rather than as a window.")

(define-option *echo-foreground* '(0.75 0.78 0.85 1.0)
  "Echo area text colour.")

(define-option *echo-accent* '(0.40 0.65 1.00 1.0)
  "Colour for the part of the echo area that says where you are.")

(define-option *echo-divider* '(0.28 0.28 0.34 1.0)
  "Colour of the separators between echo-area segments.

Dim on purpose: a separator is punctuation, and punctuation you notice is
punctuation that is too loud.")

(define-option *echo-message-seconds* 6
  "How long a message stays in the echo area before it is dropped.")

;;; ==================================================================
;;; THE MINIBUFFER
;;; ==================================================================

(define-option *minibuffer-prompt-color* '(0.95 0.75 0.35 1.0)
  "Colour of the prompt in the echo area.")

(define-option *minibuffer-completion-color* '(0.50 0.55 0.65 1.0)
  "Colour of the completion hint after what you have typed.")

(define-option *minibuffer-caret-color* '(0.95 0.75 0.35 1.0)
  "Colour of the caret — the bar showing where what you type will go.")

(define-option *minibuffer-candidates-shown* 6
  "How many completion candidates the echo area lists at once.")

(define-option *history-length* 100
  "How many past entries each minibuffer history ring keeps.")

;;; ==================================================================
;;; THE HELP OVERLAY
;;; ==================================================================

(define-option *help-columns* 2
  "How many columns of bindings the help overlay uses.

Two, not three.  Three fits more rows and truncates almost every description,
and a help screen where you can read the keys but not what they do has kept
the wrong half.")

(define-option *help-background* '(0.07 0.07 0.10 0.94)
  "Help overlay background, as (R G B A).")

(define-option *help-key-color* '(0.45 0.70 1.00 1.0)
  "Colour of the key in the help overlay.")

(define-option *help-text-color* '(0.78 0.80 0.86 1.0)
  "Colour of the description in the help overlay.")

(define-option *help-scale* 1
  "Integer scale factor for help-overlay text.")

;;; ==================================================================
;;; EMPTY PANES, AND OVERLAY BUFFERS
;;; ==================================================================

(define-option *show-empty-panes* t
  "Draw an outline around empty panes, and the spawn hints inside the focused
one.

Turning this off gives you a window manager where standing in an empty pane
looks exactly like a broken keyboard.  It is here because it is an option, not
because it is a good idea.")

(define-option *empty-pane-hint* t
  "Show which keys open something, inside the focused empty pane.")

(define-option *empty-outline-color* '(0.30 0.32 0.40 0.55)
  "Outline colour of an unfocused empty pane.")

(define-option *empty-hint-color* '(0.70 0.75 0.86 1.0)
  "Text colour of the hint inside the focused empty pane.")

(define-option *overlay-buffer-idle* t
  "Release an overlay's pixel buffer while it is hidden.

A full-screen ARGB buffer is about four megabytes, and the help screen and the
drawn map are each hidden almost all of the time.  Keeping their buffers costs
eight megabytes of resident memory to save one allocation on a keypress, which
is the wrong way round.

Set to NIL if you would rather have the allocation happen once.")
