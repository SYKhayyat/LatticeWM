;;;; examples/04-scrolling-columns.lisp
;;;;
;;;; TIER 3 — a new container kind, from outside.  The hardest thing the
;;;; extension surface claims to support, and the one the lattice is the
;;;; large-scale version of.
;;;;
;;;; This is niri and PaperWM: an infinite horizontal strip of columns, of which
;;;; a window's worth is on screen at a time, scrolling sideways as you move.
;;;; It is the closest living relative of the lattice — DESIGN calls niri "what
;;;; it gets wrong is the sharpest available spec input" — and it is the lattice
;;;; with one axis instead of two.
;;;;
;;;; If you want to understand how to add a layout model to this window manager,
;;;; read extensions/scrolling-columns/strip.lisp rather than lattice/, because
;;;; it is the same shape at a tenth of the size: a container kind, how it is
;;;; drawn, how motion crosses it, where you land arriving at it.
;;;;
;;;; IT MOVED OUT OF THIS DIRECTORY FOR A REASON THE EXAMPLES CANNOT SHOW.
;;;; A container kind has a *state* half as well as a drawing half — copy,
;;;; signature, serialise, deserialise — and the inline example answered the
;;;; first and skipped the second.  That is invisible until the day you press
;;;; undo, or save the session, and the scroll position goes with it.  The
;;;; module answers both, and its tests check that a strip survives a round trip
;;;; through the file format.

(in-package #:latticewm/user)

(load-extension "scrolling-columns")

;;; Then, live or at startup, turn a workspace into a strip.  NOT HERE, in the
;;; body of this file: loading an example must be safe with no window manager
;;; running (gate 6 loads every one of these into a bare image), and `make the
;;; current workspace a strip' needs a current workspace.  At =Super+;=, or from
;;; your own init file, it is one line:
;;;
;;;   (scrolling-columns:scrolling)     two columns visible
;;;   (scrolling-columns:scrolling 3)   three at once

;;; Every window that was in the workspace becomes a column, in the order it was
;;; laid out -- nothing is lost, nothing is reordered.  Moving left or right past
;;; the edge of the viewport scrolls by one rather than refusing, which is what
;;; makes the strip unbounded instead of merely wide.
;;;
;;; The visible count afterwards is the strip's zoom:
;;;
;;;   (setf (scrolling-columns:strip-visible <the strip>) 4)
;;;
;;; and a strip survives undo and save/restore because the module implements
;;; COPY-NODE-SLOTS, NODE-SIGNATURE, SERIALIZE-NODE and DESERIALIZE-NODE.
