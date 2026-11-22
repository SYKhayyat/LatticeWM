;;;; lattice/peek.lisp --- The whole plane, over what you have, for as long as
;;;; you look at it.
;;;;
;;;; THE ISSUE, AND WHAT IT COSTS TO ANSWER IT.  #38 asks for hold-to-peek: see
;;;; the plane, get out, in one gesture.  The compositor half of its proposal is
;;;; `modifiers_watch', which river does have since version 3 and which this
;;;; program still does not send; the risk of wiring it is a feature only a
;;;; machine with the right river can try.
;;;;
;;;; So the ruling here is different and, I think, better: *the peek is the map
;;;; drawn over what you already have, and it ends the moment you press anything
;;;; else.*  Two consequences, both wanted:
;;;;
;;;;   1. NOTHING IS RESIZED, EVER.  Zoom in this window manager is a relayout
;;;;      (there is no scaling primitive in the protocol), so peeking by zooming
;;;;      costs two relayouts of every window on screen -- terminals rewrap,
;;;;      browsers reflow -- twice per glance.  Drawing the map instead costs
;;;;      none: the windows keep their rectangles, the overlay is simply on top
;;;;      of them.
;;;;
;;;;   2. RELEASE NEEDS NO PROTOCOL.  A held modifier is one way to say "for as
;;;;      long as I hold this"; the *next key you press* is another, and it is
;;;;      the stronger promise -- nobody has ever wanted a peek to survive the
;;;;      keystroke that was the reason for it.  It is also testable with no
;;;;      compositor at all, which is the difference between a feature that
;;;;      ships and a feature that is described.
;;;;
;;;; A peek is not a mode because you cannot be in it: the next key takes it
;;;; down and does what that key says.  That is the whole difference from
;;;; GNOME's overview, and it is why this is a verb rather than a state the
;;;; status line has to report.

(in-package #:lattice)

(defvar *peek* nil
  "True while a peek is up.  One flag, because the picture is derived: what to
draw is a function of the grid, not of anything remembered here.")

(defun peek-active-p ()
  "True while the plane is being shown over the session."
  (and *peek* t))

(defun peek-begin ()
  "Show the whole plane over everything.  Changes no window and no viewport."
  (prog1 (setf *peek* t)
    (r:mark-dirty)))

(defun peek-end ()
  "Take the peek down.  Answers T when there was one to take down."
  (when *peek*
    (setf *peek* nil)
    (r:mark-dirty)
    t))

(defun map-shown-p (grid rect)
  "Should the plane be *drawn* over the session rather than rendered?

The ordinary answer is MAP-MODE-P: cells too small to be worth rendering.  A
peek forces the answer regardless of size, which is the point -- the map is what
a glance at the whole plane wants, and it is the only thing on offer that costs
no relayout."
  (or (peek-active-p) (map-mode-p grid rect)))

(defun peek-viewport (grid)
  "The viewport to draw GRID's cells in: the whole plane while peeking.

Deliberately the *grid's own* viewport when nothing is peeking, so the map at
deep zoom keeps drawing exactly the cells on screen -- the peek is a different
picture, not a change to the ordinary one."
  (if (peek-active-p) (plane-viewport grid) (grid-viewport grid)))

(defun plane-viewport (grid)
  "A viewport showing every occupied cell of GRID, or its own when none is.

The peek draws the whole plane even while you are zoomed into one cell, so it
cannot ask the grid where it is looking -- it has to ask where the windows are.
An empty plane keeps the current viewport, because `the whole of nothing' has no
bounding box and the map of an empty plane is the map you were already looking
at."
  (let ((cells (occupied-cells grid)))
    (if (null cells)
        (grid-viewport grid)
        (make-instance
         'viewport
         :origin (cell (reduce #'min cells :key #'cell-x)
                       (reduce #'min cells :key #'cell-y))
         :cols (1+ (- (reduce #'max cells :key #'cell-x)
                      (reduce #'min cells :key #'cell-x)))
         :rows (1+ (- (reduce #'max cells :key #'cell-y)
                      (reduce #'min cells :key #'cell-y)))))))

;;; ============================================================= commands

(r:defcommand peek-on ()
  "Show the whole plane over your windows, without moving or resizing one.

The next key you press takes it down and then does its own job, so this is a
glance rather than a mode: bind it to a comfortable key and use it the way you
would look out of a window."
  ()
  (peek-begin))

(r:defcommand peek-off ()
  "Take the plane back down."
  ()
  (peek-end))

(r:defcommand peek-toggle ()
  "Show the plane, or take it down -- for a key that has only one chord."
  ()
  (if (peek-active-p) (peek-end) (peek-begin)))

;;; =========================================================== the wiring

(defmethod p:on-key :before ((policy lattice-mixin) world key)
  "A peek is over the moment you press anything else.

:BEFORE, so the key that ends the peek still runs: the point of a glance is that
you take it and act, and swallowing the keystroke that was the reason for the
look would be the one way to make this feel like a mode after all."
  (declare (ignore world key))
  (when (peek-active-p) (peek-end)))
