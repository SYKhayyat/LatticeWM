;;;; lattice/pointer.lisp --- Clicking on the map.
;;;;
;;;; #30: the drawn map was a picture you could not act on.  Every square on it
;;;; is a cell with an address, and the address is the thing the map is *for* --
;;;; so the press that starts a drag everywhere else asks the policy first, and
;;;; while the map is up the answer is "this one".
;;;;
;;;; THE ONE HARD PART IS WHICH CELL, and it is decidable with no compositor:
;;;; the map is drawn from CELL-RECTS, so a click is a point looked up in the
;;;; same list of rectangles.  Drawing and hit-testing ask one function, which is
;;;; the same ruling SNAP-RECT made -- a second copy of the arithmetic is a
;;;; second thing to disagree with the picture.
;;;;
;;;; The two presses, and where they come from: Super+click is *move a window*
;;;; everywhere else in this program, so on the map it is "put the cursor in that
;;;; cell" and Super+right-click (resize everywhere else) is "send the cursor's
;;;; window there".  Both are already bound, so a user who has ever dragged a
;;;; window has already discovered the chord.

(in-package #:lattice)

(defun map-area (policy output)
  "The rectangle the map is drawn in on OUTPUT: OUTER-RECT, as DRAW-MAP-ON uses.

Shared rather than repeated, because a hit test against a different rectangle
than the one drawn in is a click that lands on the right cell in a picture
nobody is looking at."
  (p:outer-rect policy output))

(defun map-cell-at (policy grid output x y)
  "The cell address drawn at the point (X, Y) on OUTPUT, or NIL.

Walks the rectangles the map is drawn from -- including the whole-plane viewport
a peek uses -- so the answer is by construction the cell under the pointer
rather than a second calculation that happens to agree."
  (let ((area (map-area policy output)))
    (loop for (address . rect) in (cell-rects policy grid area
                                              :viewport (peek-viewport grid))
          when (c:rect-contains-p rect x y)
            return address)))

(r:defcommand goto-pointer (&optional (output nil))
  "Put the cursor in the cell under the pointer.

The keyboard spelling of clicking on the map, for the days the pointer is
somewhere else."
  ()
  (let* ((policy (p:current-policy))
         (seat (and r:*server* (r:primary-seat)))
         (x (and seat (r:seat-pointer-x seat)))
         (y (and seat (r:seat-pointer-y seat)))
         (output (or output (and x y (r:output-at x y))))
         (grid (and output (current-grid))))
    (let ((address (and grid output x y
                        (map-cell-at policy grid output x y))))
      (cond (address (goto-cell address))
            (t (r:notify "the pointer is not over a cell") nil)))))

;;; The press that claims lives in PAN.LISP, with the drags it shares a method
;;; with: CLOS keeps one method per specializer list, so a claim written here
;;; and a claim written there would be one claim and one silent deletion.
