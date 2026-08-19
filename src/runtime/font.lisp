;;;; runtime/font.lisp --- What the program does with the glyph table.
;;;;
;;;; The table itself is runtime/font-table.lisp, which is generated in full by
;;;; tools/psf-to-lisp.py.  This file is not, and that split is the point: these
;;;; four functions used to be the tail of the generated file, which meant they
;;;; lived inside a Python string literal in a script run by hand when somebody
;;;; changes fonts.  Nothing indented them, nothing compiled them until they had
;;;; been pasted out, and editing one meant editing a string in another language.
;;;;
;;;; Terminus Font is copyright (c) 2020 Dimitar Toshkov Zhekov and is licensed
;;;; under the SIL Open Font License, Version 1.1.  The full licence text is in
;;;; doc/OFL-TERMINUS.txt and must travel with any copy of the table.

(in-package #:latticewm/runtime)

;;; Hand the table to the policy layer, which is what decides where it gets
;;; used -- see policy/appearance.lisp.  This supplies the *default* font and
;;; has no opinion about anything else.

(p:register-font (p:make-font "terminus" +font-width+ +font-height+
                              +font-first+ +font+))

(setf p:*default-font* (p:find-font "terminus"))

(defun current-font (&optional (role :default))
  "The font ROLE should be drawn in, as the policy decides.

Falls back to the built-in table if a method answers NIL, because a window
manager that draws nothing because a font is missing is worse than one that
draws the wrong font."
  (or (p:font-for (p:current-policy) role)
      p:*default-font*
      ;; Last resort: any registered font at all.  The docstring's promise is
      ;; the wrong font over nothing, and a NIL here reaches the blitter, which
      ;; dereferences it -- so a missing *DEFAULT-FONT* must not be able to
      ;; return NIL as long as a single font is registered.
      (p:find-font (first (p:font-names)))))

(defun glyph-row (character row &optional (font (current-font)))
  "Row ROW of CHARACTER, as a byte whose bit 7 is the leftmost pixel."
  (p:glyph-row font character row))

(defun text-width (string &key (scale 1) (tracking 0) (font (current-font)))
  "How wide STRING will be drawn, in pixels."
  (p:font-text-width font string :scale scale :tracking tracking))

(defun text-height (&key (scale 1) (font (current-font)))
  "How tall one line of text is, in pixels."
  (p:font-text-height font :scale scale))
