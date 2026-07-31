;;;; runtime/font.lisp --- The bitmap font.  GENERATED; see tools/psf-to-lisp.py.
;;;;
;;;; Terminus 8x16, imported from ter-116n.psf.gz.
;;;;
;;;; Terminus Font is copyright (c) 2020 Dimitar Toshkov Zhekov and is licensed
;;;; under the SIL Open Font License, Version 1.1.  The full licence text is in
;;;; doc/OFL-TERMINUS.txt and must travel with any copy of this file.
;;;;
;;;; WHY A BITMAP FONT AT ALL.  The window manager puts text on screen for two
;;;; things: the coordinate a cell is at, and the echo area.  Both are short and
;;;; read at a glance.  Pulling in Pango and Cairo to draw "3,-2" would be a
;;;; dependency this project has to keep alive for as long as it lives, in
;;;; exchange for nothing anybody would notice — and it would put a C font stack
;;;; in the way of the one requirement that matters, which is that this survives
;;;; without a maintainer.
;;;;
;;;; WHY TERMINUS AND NOT A HAND-ROLLED 5x7.  There was a 5x7 here first, and it
;;;; looked exactly like a 5x7 scaled up: blocky, in a way that reads as unfinished
;;;; rather than as retro.  Terminus at 16 pixels is a real console font,
;;;; drawn by someone who knows how, and at scale 1 it is simply *text*.  The
;;;; whole cost is this generated file.
;;;;
;;;; Row-major: one byte per row, bit 7 leftmost.

(in-package #:latticewm/runtime)

(defparameter +font-width+ 8)
(defparameter +font-height+ 16)
(defparameter +font-first+ 32 "The first character the table covers.")

(defparameter +font+
  (make-array (* 95 16) :element-type (quote (unsigned-byte 8))
   :initial-contents
   (list
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ;  
    #x00 #x00 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x00 #x10 #x10 #x00 #x00 #x00 #x00   ; !
    #x00 #x24 #x24 #x24 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; quote
    #x00 #x00 #x24 #x24 #x24 #x7e #x24 #x24 #x7e #x24 #x24 #x24 #x00 #x00 #x00 #x00   ; #
    #x00 #x10 #x10 #x7c #x92 #x90 #x90 #x7c #x12 #x12 #x92 #x7c #x10 #x10 #x00 #x00   ; $
    #x00 #x00 #x64 #x94 #x68 #x08 #x10 #x10 #x20 #x2c #x52 #x4c #x00 #x00 #x00 #x00   ; %
    #x00 #x00 #x18 #x24 #x24 #x18 #x30 #x4a #x44 #x44 #x44 #x3a #x00 #x00 #x00 #x00   ; &
    #x00 #x10 #x10 #x10 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; '
    #x00 #x00 #x08 #x10 #x20 #x20 #x20 #x20 #x20 #x20 #x10 #x08 #x00 #x00 #x00 #x00   ; (
    #x00 #x00 #x20 #x10 #x08 #x08 #x08 #x08 #x08 #x08 #x10 #x20 #x00 #x00 #x00 #x00   ; )
    #x00 #x00 #x00 #x00 #x00 #x24 #x18 #x7e #x18 #x24 #x00 #x00 #x00 #x00 #x00 #x00   ; *
    #x00 #x00 #x00 #x00 #x00 #x10 #x10 #x7c #x10 #x10 #x00 #x00 #x00 #x00 #x00 #x00   ; +
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x10 #x10 #x20 #x00 #x00 #x00   ; ,
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x7e #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; -
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x10 #x10 #x00 #x00 #x00 #x00   ; .
    #x00 #x00 #x04 #x04 #x08 #x08 #x10 #x10 #x20 #x20 #x40 #x40 #x00 #x00 #x00 #x00   ; /
    #x00 #x00 #x3c #x42 #x42 #x46 #x4a #x52 #x62 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; 0
    #x00 #x00 #x08 #x18 #x28 #x08 #x08 #x08 #x08 #x08 #x08 #x3e #x00 #x00 #x00 #x00   ; 1
    #x00 #x00 #x3c #x42 #x42 #x02 #x04 #x08 #x10 #x20 #x40 #x7e #x00 #x00 #x00 #x00   ; 2
    #x00 #x00 #x3c #x42 #x42 #x02 #x1c #x02 #x02 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; 3
    #x00 #x00 #x02 #x06 #x0a #x12 #x22 #x42 #x7e #x02 #x02 #x02 #x00 #x00 #x00 #x00   ; 4
    #x00 #x00 #x7e #x40 #x40 #x40 #x7c #x02 #x02 #x02 #x42 #x3c #x00 #x00 #x00 #x00   ; 5
    #x00 #x00 #x1c #x20 #x40 #x40 #x7c #x42 #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; 6
    #x00 #x00 #x7e #x02 #x02 #x04 #x04 #x08 #x08 #x10 #x10 #x10 #x00 #x00 #x00 #x00   ; 7
    #x00 #x00 #x3c #x42 #x42 #x42 #x3c #x42 #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; 8
    #x00 #x00 #x3c #x42 #x42 #x42 #x42 #x3e #x02 #x02 #x04 #x38 #x00 #x00 #x00 #x00   ; 9
    #x00 #x00 #x00 #x00 #x00 #x10 #x10 #x00 #x00 #x00 #x10 #x10 #x00 #x00 #x00 #x00   ; :
    #x00 #x00 #x00 #x00 #x00 #x10 #x10 #x00 #x00 #x00 #x10 #x10 #x20 #x00 #x00 #x00   ; semicolon
    #x00 #x00 #x00 #x04 #x08 #x10 #x20 #x40 #x20 #x10 #x08 #x04 #x00 #x00 #x00 #x00   ; <
    #x00 #x00 #x00 #x00 #x00 #x7e #x00 #x00 #x7e #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; =
    #x00 #x00 #x00 #x40 #x20 #x10 #x08 #x04 #x08 #x10 #x20 #x40 #x00 #x00 #x00 #x00   ; >
    #x00 #x00 #x3c #x42 #x42 #x42 #x04 #x08 #x08 #x00 #x08 #x08 #x00 #x00 #x00 #x00   ; ?
    #x00 #x00 #x7c #x82 #x9e #xa2 #xa2 #xa2 #xa6 #x9a #x80 #x7e #x00 #x00 #x00 #x00   ; @
    #x00 #x00 #x3c #x42 #x42 #x42 #x42 #x7e #x42 #x42 #x42 #x42 #x00 #x00 #x00 #x00   ; A
    #x00 #x00 #x7c #x42 #x42 #x42 #x7c #x42 #x42 #x42 #x42 #x7c #x00 #x00 #x00 #x00   ; B
    #x00 #x00 #x3c #x42 #x42 #x40 #x40 #x40 #x40 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; C
    #x00 #x00 #x78 #x44 #x42 #x42 #x42 #x42 #x42 #x42 #x44 #x78 #x00 #x00 #x00 #x00   ; D
    #x00 #x00 #x7e #x40 #x40 #x40 #x78 #x40 #x40 #x40 #x40 #x7e #x00 #x00 #x00 #x00   ; E
    #x00 #x00 #x7e #x40 #x40 #x40 #x78 #x40 #x40 #x40 #x40 #x40 #x00 #x00 #x00 #x00   ; F
    #x00 #x00 #x3c #x42 #x42 #x40 #x40 #x4e #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; G
    #x00 #x00 #x42 #x42 #x42 #x42 #x7e #x42 #x42 #x42 #x42 #x42 #x00 #x00 #x00 #x00   ; H
    #x00 #x00 #x38 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x38 #x00 #x00 #x00 #x00   ; I
    #x00 #x00 #x0e #x04 #x04 #x04 #x04 #x04 #x04 #x44 #x44 #x38 #x00 #x00 #x00 #x00   ; J
    #x00 #x00 #x42 #x44 #x48 #x50 #x60 #x60 #x50 #x48 #x44 #x42 #x00 #x00 #x00 #x00   ; K
    #x00 #x00 #x40 #x40 #x40 #x40 #x40 #x40 #x40 #x40 #x40 #x7e #x00 #x00 #x00 #x00   ; L
    #x00 #x00 #x82 #xc6 #xaa #x92 #x92 #x82 #x82 #x82 #x82 #x82 #x00 #x00 #x00 #x00   ; M
    #x00 #x00 #x42 #x42 #x42 #x62 #x52 #x4a #x46 #x42 #x42 #x42 #x00 #x00 #x00 #x00   ; N
    #x00 #x00 #x3c #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; O
    #x00 #x00 #x7c #x42 #x42 #x42 #x42 #x7c #x40 #x40 #x40 #x40 #x00 #x00 #x00 #x00   ; P
    #x00 #x00 #x3c #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x4a #x3c #x02 #x00 #x00 #x00   ; Q
    #x00 #x00 #x7c #x42 #x42 #x42 #x42 #x7c #x50 #x48 #x44 #x42 #x00 #x00 #x00 #x00   ; R
    #x00 #x00 #x3c #x42 #x40 #x40 #x3c #x02 #x02 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; S
    #x00 #x00 #xfe #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x00 #x00 #x00 #x00   ; T
    #x00 #x00 #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; U
    #x00 #x00 #x42 #x42 #x42 #x42 #x42 #x24 #x24 #x24 #x18 #x18 #x00 #x00 #x00 #x00   ; V
    #x00 #x00 #x82 #x82 #x82 #x82 #x82 #x92 #x92 #xaa #xc6 #x82 #x00 #x00 #x00 #x00   ; W
    #x00 #x00 #x42 #x42 #x24 #x24 #x18 #x18 #x24 #x24 #x42 #x42 #x00 #x00 #x00 #x00   ; X
    #x00 #x00 #x82 #x82 #x44 #x44 #x28 #x10 #x10 #x10 #x10 #x10 #x00 #x00 #x00 #x00   ; Y
    #x00 #x00 #x7e #x02 #x02 #x04 #x08 #x10 #x20 #x40 #x40 #x7e #x00 #x00 #x00 #x00   ; Z
    #x00 #x00 #x38 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20 #x38 #x00 #x00 #x00 #x00   ; [
    #x00 #x00 #x40 #x40 #x20 #x20 #x10 #x10 #x08 #x08 #x04 #x04 #x00 #x00 #x00 #x00   ; backslash
    #x00 #x00 #x38 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x38 #x00 #x00 #x00 #x00   ; ]
    #x00 #x10 #x28 #x44 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; ^
    #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x7e #x00 #x00   ; _
    #x10 #x08 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; `
    #x00 #x00 #x00 #x00 #x00 #x3c #x02 #x3e #x42 #x42 #x42 #x3e #x00 #x00 #x00 #x00   ; a
    #x00 #x00 #x40 #x40 #x40 #x7c #x42 #x42 #x42 #x42 #x42 #x7c #x00 #x00 #x00 #x00   ; b
    #x00 #x00 #x00 #x00 #x00 #x3c #x42 #x40 #x40 #x40 #x42 #x3c #x00 #x00 #x00 #x00   ; c
    #x00 #x00 #x02 #x02 #x02 #x3e #x42 #x42 #x42 #x42 #x42 #x3e #x00 #x00 #x00 #x00   ; d
    #x00 #x00 #x00 #x00 #x00 #x3c #x42 #x42 #x7e #x40 #x40 #x3c #x00 #x00 #x00 #x00   ; e
    #x00 #x00 #x0e #x10 #x10 #x7c #x10 #x10 #x10 #x10 #x10 #x10 #x00 #x00 #x00 #x00   ; f
    #x00 #x00 #x00 #x00 #x00 #x3e #x42 #x42 #x42 #x42 #x42 #x3e #x02 #x02 #x3c #x00   ; g
    #x00 #x00 #x40 #x40 #x40 #x7c #x42 #x42 #x42 #x42 #x42 #x42 #x00 #x00 #x00 #x00   ; h
    #x00 #x00 #x10 #x10 #x00 #x30 #x10 #x10 #x10 #x10 #x10 #x38 #x00 #x00 #x00 #x00   ; i
    #x00 #x00 #x04 #x04 #x00 #x0c #x04 #x04 #x04 #x04 #x04 #x04 #x44 #x44 #x38 #x00   ; j
    #x00 #x00 #x40 #x40 #x40 #x42 #x44 #x48 #x70 #x48 #x44 #x42 #x00 #x00 #x00 #x00   ; k
    #x00 #x00 #x30 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x38 #x00 #x00 #x00 #x00   ; l
    #x00 #x00 #x00 #x00 #x00 #xfc #x92 #x92 #x92 #x92 #x92 #x92 #x00 #x00 #x00 #x00   ; m
    #x00 #x00 #x00 #x00 #x00 #x7c #x42 #x42 #x42 #x42 #x42 #x42 #x00 #x00 #x00 #x00   ; n
    #x00 #x00 #x00 #x00 #x00 #x3c #x42 #x42 #x42 #x42 #x42 #x3c #x00 #x00 #x00 #x00   ; o
    #x00 #x00 #x00 #x00 #x00 #x7c #x42 #x42 #x42 #x42 #x42 #x7c #x40 #x40 #x40 #x00   ; p
    #x00 #x00 #x00 #x00 #x00 #x3e #x42 #x42 #x42 #x42 #x42 #x3e #x02 #x02 #x02 #x00   ; q
    #x00 #x00 #x00 #x00 #x00 #x5e #x60 #x40 #x40 #x40 #x40 #x40 #x00 #x00 #x00 #x00   ; r
    #x00 #x00 #x00 #x00 #x00 #x3e #x40 #x40 #x3c #x02 #x02 #x7c #x00 #x00 #x00 #x00   ; s
    #x00 #x00 #x10 #x10 #x10 #x7c #x10 #x10 #x10 #x10 #x10 #x0e #x00 #x00 #x00 #x00   ; t
    #x00 #x00 #x00 #x00 #x00 #x42 #x42 #x42 #x42 #x42 #x42 #x3e #x00 #x00 #x00 #x00   ; u
    #x00 #x00 #x00 #x00 #x00 #x42 #x42 #x42 #x24 #x24 #x18 #x18 #x00 #x00 #x00 #x00   ; v
    #x00 #x00 #x00 #x00 #x00 #x82 #x82 #x92 #x92 #x92 #x92 #x7c #x00 #x00 #x00 #x00   ; w
    #x00 #x00 #x00 #x00 #x00 #x42 #x42 #x24 #x18 #x24 #x42 #x42 #x00 #x00 #x00 #x00   ; x
    #x00 #x00 #x00 #x00 #x00 #x42 #x42 #x42 #x42 #x42 #x42 #x3e #x02 #x02 #x3c #x00   ; y
    #x00 #x00 #x00 #x00 #x00 #x7e #x04 #x08 #x10 #x20 #x40 #x7e #x00 #x00 #x00 #x00   ; z
    #x00 #x00 #x0c #x10 #x10 #x10 #x20 #x10 #x10 #x10 #x10 #x0c #x00 #x00 #x00 #x00   ; {
    #x00 #x00 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x10 #x00 #x00 #x00 #x00   ; |
    #x00 #x00 #x30 #x08 #x08 #x08 #x04 #x08 #x08 #x08 #x08 #x30 #x00 #x00 #x00 #x00   ; }
    #x00 #x62 #x92 #x8c #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00   ; ~
    ))
  "ASCII 32 to 126, 16 row bytes each.")


;;; Everything above is the table.  Everything below hands it to the policy
;;; layer, which is what decides where it gets used -- see
;;; policy/appearance.lisp.  This file supplies the *default* font and has no
;;; opinion about anything else.

(p:register-font (p:make-font "terminus" +font-width+ +font-height+
                              +font-first+ +font+))

(setf p:*default-font* (p:find-font "terminus"))

(defun current-font (&optional (role :default))
  "The font ROLE should be drawn in, as the policy decides.

Falls back to the built-in table if a method answers NIL, because a window
manager that draws nothing because a font is missing is worse than one that
draws the wrong font."
  (or (p:font-for (p:current-policy) role) p:*default-font*))

(defun glyph-row (character row &optional (font (current-font)))
  "Row ROW of CHARACTER, as a byte whose bit 7 is the leftmost pixel."
  (p:glyph-row font character row))

(defun text-width (string &key (scale 1) (tracking 0) (font (current-font)))
  "How wide STRING will be drawn, in pixels."
  (p:font-text-width font string :scale scale :tracking tracking))

(defun text-height (&key (scale 1) (font (current-font)))
  "How tall one line of text is, in pixels."
  (p:font-text-height font :scale scale))
