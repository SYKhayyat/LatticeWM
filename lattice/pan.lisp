;;;; lattice/pan.lisp --- Pulling the plane around with the pointer.
;;;;
;;;; #29 ASKED FOR WHEEL ZOOM AND IT CANNOT BE BUILT: river's seat protocol has
;;;; pointer_position, pointer_enter, pointer_leave and pointer bindings with
;;;; pressed/released -- and no axis event anywhere.  A window manager that is
;;;; not told about the wheel cannot zoom with it, and binding wl_pointer for
;;;; ourselves would mean competing with every client for the scroll everyone
;;;; else wants.  So of the four things the issue asks for, three ship and one is
;;;; recorded as impossible rather than faked:
;;;;
;;;;   wheel zoom          impossible: no axis event in the protocol.
;;;;   zoom at the cursor   GOTO-CELL's mechanism, aimed by ZOOM-AT-POINTER.
;;;;   drag to pan          the empty plane is a handle; pull it.
;;;;   pointer anchoring    the decode #30 built, used for zoom instead of go.
;;;;
;;;; THE HANDLE IS THE REASON THIS IS SMALL.  Panning already existed as a verb
;;;; and as arithmetic; what did not exist is a way to say "this motion, now"
;;;; -- which is what :POINTER-MOVED is.  A drag is three moments (press, move,
;;;; release), so it is three hooks' worth of machinery and no new generic: the
;;;; press is claimed by the same P:POINTER-PRESS that made the map clickable.
;;;;
;;;; AND THE DRAG PANS BY CELLS, DELIBERATELY.  There is no such thing as a
;;;; half-cell: a window either fits a cell or it does not, so a drag that moved
;;;; the camera by pixels would relayout every window on screen sixty times a
;;;; second and land on the same integer cell at the end anyway.  The plane
;;;; moves when the pointer has moved a whole cell, and not before.

(in-package #:lattice)

(defvar *drag-pan* nil
  "The drag in progress, or NIL.

A plist: :GRID, :VIEWPORT, the press point as :X and :Y, the camera's :ORIGIN
when the drag began, and the :PITCH of one cell in pixels.  Recorded rather
than recomputed so that a drag cannot drift while it runs -- the pitch is read
from the layout that was on screen when the hand went down.")

(defun drag-panning-p () (not (null *drag-pan*)))

(defun cell-pitch (policy grid output)
  "How many pixels one cell is worth on OUTPUT, as two values.

The whole area divided by the cells that fit in it, which includes the gaps: a
drag of one pitch is a drag of one cell, gaps and all, and that is the distance
the hand actually travelled across the picture."
  (let* ((area (map-area policy output))
         (viewport (peek-viewport grid)))
    (values (max 1.0 (/ (c:rect-w area) (max 1 (viewport-cols viewport))))
            (max 1.0 (/ (c:rect-h area) (max 1 (viewport-rows viewport)))))))

(defun drag-pan-begin (policy grid output x y)
  "Start pulling GRID around from (X, Y) on OUTPUT."
  (let ((viewport (grid-viewport grid)))
    (multiple-value-bind (pitch-x pitch-y) (cell-pitch policy grid output)
      (setf *drag-pan* (list :grid grid :viewport viewport :x x :y y
                             :origin (viewport-origin viewport)
                             :pitch-x pitch-x :pitch-y pitch-y))
      ;; The cursor is NOT dragged along: the plane moves under a still camera,
      ;; which is what makes panning different from motion.  Saying so here
      ;; because "why did my cursor not follow" is the first question a pan
      ;; raises, and the answer is that following it would be a different verb.
      (r:logmsg :debug "lattice: dragging the plane from ~d,~d" x y))))

(defun drag-pan-update (x y)
  "Move the camera as far as the pointer has been dragged since the press."
  (when *drag-pan*
    (let* ((anchor (getf *drag-pan* :origin))
           (pitch-x (getf *drag-pan* :pitch-x))
           (pitch-y (getf *drag-pan* :pitch-y))
           ;; The pointer is *cumulative* motion, so every step is computed from
           ;; the press rather than from the last frame: a drag that leaves the
           ;; screen and comes back lands where the hand is, not where the
           ;; arithmetic remembers it was.  Same rule the window drags follow.
           (steps-x (round (- x (getf *drag-pan* :x)) pitch-x))
           (steps-y (round (- y (getf *drag-pan* :y)) pitch-y))
           ;; Drag right and the plane comes right, so the camera goes LEFT.
           ;; Drag down and the plane goes down; lattice +Y is up (D2), so the
           ;; camera goes UP.  Two signs, and only one of them is the one a
           ;; reader guesses.
           (target (cell (- (cell-x anchor) steps-x)
                         (+ (cell-y anchor) steps-y)))
           (viewport (getf *drag-pan* :viewport)))
      (unless (and (= (cell-x target) (cell-x (viewport-origin viewport)))
                   (= (cell-y target) (cell-y (viewport-origin viewport))))
        (setf (viewport-origin viewport) target)
        (r:mark-dirty))
      target)))

(defun drag-pan-end ()
  "Let go of the plane."
  (when *drag-pan*
    (setf *drag-pan* nil)
    (r:mark-dirty)))

;;; ------------------------------------------------------------- the hooks

(defun drag-pan-tracking (x y)
  "The :POINTER-MOVED half: nothing at all unless a drag is running.

Unconditional and unthrottled is the right shape for the hook, and this is why:
the check is here, so a session with no drag pays one NIL test per motion event
and never a layout."
  (when (drag-panning-p)
    (r:guarded "drag-pan" (drag-pan-update x y))))

(defun drag-pan-released (kind)
  "The :POINTER-RELEASED half.  KIND is ignored: every button ends every drag."
  (declare (ignore kind))
  (drag-pan-end))

;; Installed at load time rather than from ENABLE, and the reason is ordering:
;; ENABLE lives in COMMANDS.LISP, which is compiled before this file, so a call
;; from there would be a forward reference and a compiler warning that gate 1
;; would (rightly) refuse.  Both hooks do nothing unless a drag is running, and
;; a drag only ever starts from this file, so a session that never enables the
;; lattice pays one NIL test per motion event and nothing else.
(p:add-hook :pointer-moved 'drag-pan-tracking)
(p:add-hook :pointer-released 'drag-pan-released)

;;; ------------------------------------------------------------ the press

(defun a-window-at (world x y)
  "Any window whose rectangle covers (X, Y), floats first, or NIL.

The same order WINDOW-UNDER-POINTER uses, asked of the world rather than the
seat, because a policy method is handed a world and a press and nothing else."
  (or (loop for float in (reverse (c:world-floats world))
            for window = (c:float-window float)
            for rect = (and window (c:window-rect window))
            when (and rect (c:rect-contains-p rect x y)) return window)
      (loop for window in (r:all-windows)
            for rect = (c:window-rect window)
            when (and rect (c:window-live-p window)
                      (not (c:window-minimized-p window))
                      (c:rect-contains-p rect x y))
              return window)))

;;; ------------------------------------------------------------- the verbs

(r:defcommand zoom-at-pointer (direction)
  "Zoom IN or OUT anchored on the cell under the pointer.

The difference from ZOOM-IN and ZOOM-OUT is only where the camera lands, and it
is the difference a map makes: the thing you are pointing at stays where it is
instead of the thing you are standing in.  With no pointer over a cell it falls
back to the cursor, which is what those two commands already do."
  (let* ((policy (p:current-policy))
         (seat (and r:*server* (r:primary-seat)))
         (x (and seat (r:seat-pointer-x seat)))
         (y (and seat (r:seat-pointer-y seat)))
         (output (and x y (r:output-at x y)))
         (grid (and output (current-grid)))
         (address (and grid x y (map-cell-at policy grid output x y))))
    (when grid
      (set-zoom grid (+ (zoom-index (grid-viewport grid))
                        (if (eq direction :out) 1 -1))
                :focus (or address (current-cell)))
      (r:mark-dirty))))

;;; ------------------------------------------------------------- the claiming

(defmethod p:pointer-press ((policy lattice-mixin) (world c:world) x y kind)
  "Claim a press on the map's cells, and a press on the plane's empty parts.

Two claims with one rule between them: what is under the pointer decides.  A
*celled* press on the map is a jump (or a window move); an *empty* press is a
handle on the plane.  What is never claimed is a press over a window, because
that is a window drag and always was -- the map changes what the mouse does to
the plane, not to the windows on it."
  (let* ((output (r:output-at x y))
         (grid (and output (cursor-grid world)))
         (area (and output grid (map-area policy output))))
    (cond
      ((null area) nil)
      ((and (map-shown-p grid area)
            (map-cell-at policy grid output x y))
       (let ((address (map-cell-at policy grid output x y)))
         (ecase kind
           (:move
            (goto-cell address)
            (r:notify "~a" (cell-label grid address)))
           (:resize
            ;; The resize chord is the one that means `move the thing'.
            (let ((window (r:focused-window)))
              (when window
                (move-cell-contents (cell-x address) (cell-y address))
                (r:notify "~a to ~a" (p:name-of-window window)
                          (cell-label grid address)))))
           (t nil))
         t))
      ;; Empty space -- a gap between cells, or past the edge of the plane --
      ;; is a handle, and only the left button and the move chord.
      ((and (eq kind :move) (null (a-window-at world x y)))
       (drag-pan-begin policy grid output x y)
       t)
      (t nil))))
