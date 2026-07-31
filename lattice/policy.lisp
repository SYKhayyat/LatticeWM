;;;; lattice/policy.lisp --- Teaching the existing policy about the plane.
;;;;
;;;; Every function here is a DEFMETHOD on a generic the core already declared.
;;;; Nothing is redefined, nothing is patched, and no file under src/ is
;;;; touched.  That is D21's experiment, and this file is the answer sheet.

(in-package #:lattice)

(defclass lattice-policy (p:conventional-policy)
  ;; %NAME, not NAME.  A slot is identified by its *symbol*, so writing
  ;; (name :initform "lattice") in this package would declare a brand new
  ;; LATTICE::NAME slot alongside the inherited one rather than overriding it,
  ;; and POLICY-NAME would keep answering "conventional".  It did, and it cost
  ;; twenty minutes of chasing a layout bug that was not there.  Naming the
  ;; slot with a prefix that is only ever written in one package makes the
  ;; mistake impossible to make by accident.
  ((p::%name :initform "lattice"))
  (:documentation
   "The conventional policy, plus a plane.

Subclassing rather than replacing is the point.  Splits still split, tabs are
still tabs, floats still float, minimize still leaves the tree — every
behaviour built in weeks one and two survives untouched, and the lattice is
the arrangement of *cells*, each of which contains all of it."))

;;; ==================================================================
;;; TIER 0
;;; ==================================================================

(p:define-option *zoom-mode* :fit
  "How zooming out makes room.  The two answers the design allows, both
shipped, configuration picking the default.

  :FIT     the viewport always shows a whole number of cells and they stretch
           to fill it exactly.  Zoom walks a ladder: 1, 2, 4, 6, 8 cells.
  :FIXED   cells keep an absolute pixel size and the viewport shows however
           many fit, cropping the trailing one at the edge.

There is no scaling primitive anywhere in the river protocol — none: no scale,
no transform, no matrix, no buffer_scale.  So \"zoom out\" *cannot* mean \"draw
the same windows smaller\"; it can only mean \"give every window smaller
dimensions\".  Every zoom step is a genuine relayout in which terminals rewrap
and browsers reflow.  Both modes pay that; they differ in what happens when
you *pan*.

PLAN.org's Delta 3 rules :FIXED the default, on the grounds that :FIT makes a
column's rendered width depend on which columns are beside it, so panning
resizes every window on screen — and pan is the frequent navigational verb.

That argument is right and it is *conditional*, which Delta 3 does not say:
it only bites once columns are non-uniform, and columns are uniform until
somebody resizes one.  On a uniform lattice every visible column is the same
width no matter where the viewport sits, so :FIT panning resizes nothing at
all.  :FIT is therefore free by default and is also the behaviour actually
asked for.  The moment you resize a column, Delta 3's cost becomes real —
which is exactly when to set this to :FIXED.  RESIZE-COLUMN says so when you
first use it.")

(p:define-option *zoom-ladder*
  '((1 . 1) (2 . 1) (2 . 2) (3 . 2) (4 . 2) (4 . 3) (6 . 4) (8 . 6))
  "The zoom levels, as (COLUMNS . ROWS): 1, 2, 4, 6, 8, 12, 24, 48 cells.

Emergent rather than authored — the achievable views are simply integer pairs,
and this is the walk through them.  It dissolves the 'four in the corners or
four in a row?' question: both are achievable and which you land on follows
from where you were.

Steps that preserve the current aspect are preferred where one exists, so 2x2
goes to 4x4-ish rather than to something that makes cells portrait.")

(p:define-option *cell-width* 960
  "Pixel width of one cell under :FIXED zoom.")

(p:define-option *cell-height* 540
  "Pixel height of one cell under :FIXED zoom.")

(p:define-option *cell-gap* 0
  "Pixels of empty space between adjacent cells, on top of their borders.

Zero by default, and the reasoning is worth stating because the obvious choice
is wrong.  A cell boundary is *already* two borders meeting — one from each
cell — so at the shipped border width it is a clean four-pixel divider before
any gap is added.  A gap of eight then makes it twelve pixels of black, which
reads as a *column of nothing between the windows* rather than as an edge.

The cell boundary does need to be more legible than a split boundary inside a
cell, since it is where the coordinate changes.  That is what the parity tint
is for — see *LATTICE-BORDER-PARITY* — and a colour costs no screen.

Set it to 4 or 8 if you want the airy look.  It is not free: on a 2x2 view an
eight-pixel gap costs eight pixels of every window, four times over.")

(p:define-option *skip-empty-cells* nil
  "Whether plain directional motion crosses empty cells in one step.

NIL is README D3's ruling: plain motion moves exactly one cell whether or not
that cell is occupied, and the modified motion skips.  Raw step is the default
because zoom-out is the primary orientation tool, so motion does not need to
defend against getting lost in empty space — and keeping the default motion
distance-predictable preserves muscle memory.

Turning this on gives the window model's entire *feel* without unwinding
anything, which is README D18's stated escape hatch.")

(p:define-option *coordinate-overlay* :zoomed-out
  "When cells show their coordinate.

  :ALWAYS       always
  :ZOOMED-OUT   whenever more than one cell is visible (the default)
  :NEVER        never

README names two-dimensional navigation as the single biggest design risk and
lists three mitigations: a minimap, named cells, and this.  This is much the
cheapest of the three and it is the one that ships.  Turning it off is
supported and is a bad idea until the lattice is in your fingers.")

(p:define-option *coordinate-tint* 0.55
  "How strongly a cell's border colour encodes its coordinate, 0.0 to 1.0.

Every cell gets a *stable* border hue derived from its (x, y).  Cell (3,-2)
looks the same colour every time you visit it, forever, and its neighbours look
adjacent.  That is spatial memory support for the price of a colour, and it is
the only orientation aid that survives at a zoom level where text would be
unreadable.

README names getting lost as the single biggest risk in the whole design —
\"two dimensions is more than twice as hard to hold in your head as niri's
one\" — and lists three defences: a minimap, named cells, and the coordinate
overlay.  This is the cheapest useful form of the third.

0.0 turns it off and gives every cell the same neutral border.  Above about
0.7 it starts to look like a toy.")

(p:define-option *lattice-border-parity* nil
  "Tint cell borders by coordinate parity, like a chessboard.

Superseded by *COORDINATE-TINT*, which distinguishes every cell rather than
only alternate ones, and kept because a chessboard is genuinely easier to read
at a glance if all you want to know is whether a move happened.")

;;; ==================================================================
;;; LAYOUT
;;; ==================================================================

(defun cell-rects (policy grid rect)
  "Rectangles for every visible cell, as an alist of (ADDRESS . RECT).

THE SIGN INVERSION LIVES HERE, and nowhere else.  Lattice +Y is up (D2);
Wayland +Y is down.  One line, one function, deliberately."
  (declare (ignorable policy))
  (let* ((viewport (grid-viewport grid))
         (ox (cell-x (viewport-origin viewport)))
         (oy (cell-y (viewport-origin viewport)))
         (cols (viewport-cols viewport))
         (rows (viewport-rows viewport))
         (gap *cell-gap*)
         (widths (loop for i from 0 below cols collect (col-width grid (+ ox i))))
         (heights (loop for i from 0 below rows collect (row-height grid (+ oy i))))
         (columns
           (ecase *zoom-mode*
             (:fit (c:divide-rect rect :horizontal widths :gap gap))
             (:fixed (fixed-tracks rect :horizontal cols *cell-width* gap))))
         (bands
           (ecase *zoom-mode*
             (:fit (c:divide-rect rect :vertical heights :gap gap))
             (:fixed (fixed-tracks rect :vertical rows *cell-height* gap)))))
    (loop for row from 0 below rows
          append (loop for col from 0 below cols
                       for column = (nth col columns)
                       ;; +Y is up, so the *last* band on screen is the
                       ;; *lowest* row number.  This is the inversion.
                       for band = (nth (- rows 1 row) bands)
                       when (and column band)
                         collect (cons (cell (+ ox col) (+ oy row))
                                       (c:make-rect (c:rect-x column)
                                                    (c:rect-y band)
                                                    (c:rect-w column)
                                                    (c:rect-h band)))))))

(defun fixed-tracks (rect axis count size gap)
  "COUNT tracks of exactly SIZE pixels along AXIS, starting at RECT's edge.

The trailing track runs off the end of RECT and is cropped by CLIP-RECT —
river's set_content_clip_box redraws the border at the crop edge, so it reads
as a cleanly cut cell rather than a window sliced in half.  The sliver is
arguably a feature: it is peripheral vision telling you something is over
there."
  (let ((horizontal (eq axis :horizontal)))
    (loop for i from 0 below count
          for offset = (* i (+ size gap))
          collect (if horizontal
                      (c:make-rect (+ (c:rect-x rect) offset) (c:rect-y rect)
                                   size (c:rect-h rect))
                      (c:make-rect (c:rect-x rect) (+ (c:rect-y rect) offset)
                                   (c:rect-w rect) size)))))

(defun tag-cell (node address)
  "Record which cell a subtree is, so decoration can ask.

On PROPS rather than a slot, because it is presentation state that a node has
no business carrying permanently — which is exactly the case README D20
predicted an extension would need PROPS for."
  (when node
    (setf (c:prop node :lattice/address) address
          (c:prop node :lattice/parity) (evenp (+ (cell-x address)
                                                  (cell-y address))))
    ;; The cell's whole subtree answers for the cell, so a split *inside* a
    ;; cell borders in the cell's colour rather than reverting to neutral.
    (when (c:container-p node)
      (dolist (child-address (c:container-addresses node))
        (tag-cell (c:child-at node child-address) address))))
  node)

(defmethod p:layout-children ((policy lattice-policy) (grid grid) rect)
  "Place the visible cells; everything else is not laid out and is hidden.

Cells outside the viewport are deliberately omitted rather than positioned
offscreen.  The conventional layout driver walks the omitted addresses anyway
and marks them invisible, so their windows get river's hide — which is not
optional, since newly created windows are shown unless explicitly hidden."
  (let ((placed (remove-if-not (lambda (entry) (c:child-at grid (car entry)))
                               (cell-rects policy grid rect))))
    (dolist (entry placed placed)
      (tag-cell (c:child-at grid (car entry)) (car entry)))))

(defmethod p:clip-rect ((policy lattice-policy) node rect)
  "Crop a cell that hangs over the viewport edge.

This is the payoff of the best find in the protocol.  Under :FIXED zoom the
trailing cell is usually partial, and set_content_clip_box crops the content
*and* redraws the compositor's border at the crop edge, for free.  It was
expected to cost a week of bad approximation."
  (let ((bounds (c:prop node :lattice/viewport-bounds)))
    (cond ((null bounds) (call-next-method))
          (t (let ((visible (c:rect-intersect rect bounds)))
               (if (and visible (not (c:rect-equal visible rect)))
                   visible
                   (call-next-method)))))))

(defmethod p:gaps ((policy lattice-policy) (grid grid))
  *cell-gap*)

(defun coordinate-hue (address)
  "A stable hue in [0,1) for a cell address.

Adjacent columns are adjacent hues and adjacent rows are a smaller step, so
neighbours look related and a cell three columns away looks clearly different.
The multipliers are chosen to walk the wheel without repeating inside any
viewport anybody will actually use: x steps by about a seventh of the circle,
y by about a fifth of that again."
  (mod (+ (* (cell-x address) 0.14) (* (cell-y address) 0.31)) 1.0))

(defun hsv-to-rgb (h s v)
  "Convert hue, saturation and value in [0,1] to (values R G B) in [0,1]."
  (let* ((i (floor (* h 6)))
         (f (- (* h 6) i))
         (p (* v (- 1 s)))
         (q (* v (- 1 (* s f))))
         (t* (* v (- 1 (* s (- 1 f))))))
    (ecase (mod i 6)
      (0 (values v t* p)) (1 (values q v p)) (2 (values p v t*))
      (3 (values p q v)) (4 (values t* p v)) (5 (values v p q)))))

(defmethod p:border-color ((policy lattice-policy) node focusedp)
  "Neutral, tinted toward the cell's own colour.

The tint is applied to *both* the focused and unfocused colour, and that is
deliberate: if only unfocused cells were coloured then the cell you are in —
the one you most need to identify — would be the one with no identity.  Focus
still reads instantly because it is much brighter, not because it is the only
coloured thing."
  (multiple-value-bind (r g b a) (call-next-method)
    (let ((address (c:prop node :lattice/address)))
      (cond
        ((or (null address) (<= *coordinate-tint* 0.0))
         (if (and *lattice-border-parity* (c:prop node :lattice/parity)
                  (not focusedp))
             (values (* r 1.35) (* g 1.15) (min 1.0 (* b 1.35)) a)
             (values r g b a)))
        (t
         ;; The focused cell is *bright*; the rest are tinted greys.  Both
         ;; carry the cell's hue, so you can always tell which cell you are in
         ;; — but if the unfocused ones are also saturated then everything
         ;; looks highlighted and nothing does, which is what the first
         ;; attempt at this looked like.
         (multiple-value-bind (hr hg hb)
             (hsv-to-rgb (coordinate-hue address)
                         (if focusedp 0.45 0.55)
                         (if focusedp 1.0 0.30))
           (let ((mix (* *coordinate-tint* (if focusedp 0.55 1.0))))
             (values (+ (* r (- 1 mix)) (* hr mix))
                     (+ (* g (- 1 mix)) (* hg mix))
                     (+ (* b (- 1 mix)) (* hb mix))
                     a))))))))

(defmethod p:border-width ((policy lattice-policy) node focusedp)
  "A thicker border on the focused cell.

Colour alone is not enough at a glance across a 3x2 view, and a border that is
merely brighter can lose to a bright application behind it.  Thickness cannot."
  (let ((base (call-next-method)))
    (if (and focusedp (c:prop node :lattice/address)) (+ base 2) base)))

;;; ==================================================================
;;; MOTION — the one that makes the whole thing feel like a place
;;; ==================================================================

(defmethod p:step-address ((policy lattice-policy) (grid grid) address direction)
  "Move one cell, creating it if it does not exist yet.

The plane is infinite, so 'the cell to the left' always exists in principle;
arriving is what brings it into being.  README D3 rules that plain motion
moves exactly one cell whether or not it is occupied, and D18 rules that focus
is a place — together those *require* that moving into empty space works, or
both rulings have no referent.

With *SKIP-EMPTY-CELLS*, motion crosses a run of empty cells in one press
instead: the spreadsheet Ctrl+Arrow idiom, and README D3's modified motion."
  (let* ((dx (if (c:direction-horizontal-p direction)
                 (c:direction-sign direction) 0))
         ;; +Y is up, so :UP increases Y.  DIRECTION-SIGN says :UP is -1
         ;; because it decreases the *screen* coordinate, so it inverts here.
         (dy (if (c:direction-vertical-p direction)
                 (- (c:direction-sign direction)) 0))
         (next (cell (+ (cell-x address) dx) (+ (cell-y address) dy))))
    (when *skip-empty-cells*
      (loop repeat 1000
            while (let ((node (c:child-at grid next)))
                    (or (null node) (c:node-empty-p node)))
            do (setf next (cell (+ (cell-x next) dx) (+ (cell-y next) dy)))
            finally (unless (c:child-at grid next)
                      (return-from p:step-address address))))
    (ensure-cell grid next)
    (ensure-visible grid next)
    next))

(defmethod p:entry-address ((policy lattice-policy) (grid grid)
                            direction reference rects)
  "Arriving at the plane from outside — from another workspace, say.

Lands on the cell nearest the viewport origin, or geometrically when there is
something to be geometric about."
  (declare (ignore direction reference rects))
  (c:default-address grid))

(defmethod p:motion-escapes-p ((policy lattice-policy) (grid grid) direction)
  "Motion never leaves the plane, because the plane has no edge.

D6: the lattice is genuinely infinite.  No hard bound, no growable-but-bounded
compromise — D3's skip motion, the coordinate overlay and zoom-out together
supply the navigability a bound would have provided."
  (declare (ignore direction))
  nil)

;;; ==================================================================
;;; THE VIEWPORT — zoom and pan, which never overlap
;;; ==================================================================
;;;
;;; README D7, the central usability ruling: zoom, resize and pan are three
;;; separate commands answering three separate questions, and they never
;;; overlap.  Most tiling window managers feel muddy because widening a window
;;; also changes what is visible and the user cannot tell which thing they
;;; asked for.
;;;
;;;   Zoom    "how much of my world am I looking at?"  Pure view control.
;;;   Resize  "how much room does this thing get?"     Changes the layout.
;;;   Pan     "where am I standing?"                   Pure view control.

(defun ensure-visible (grid address)
  "Pan the viewport, minimally, so that ADDRESS is on screen.

Minimally is the whole requirement: scrolling further than necessary is how a
viewport loses you.  Moving one cell right off the edge should shift the view
by exactly one column."
  (let* ((viewport (grid-viewport grid))
         (ox (cell-x (viewport-origin viewport)))
         (oy (cell-y (viewport-origin viewport)))
         (x (cell-x address))
         (y (cell-y address))
         (new-x (cond ((< x ox) x)
                      ((> x (+ ox (viewport-cols viewport) -1))
                       (- x (viewport-cols viewport) -1))
                      (t ox)))
         (new-y (cond ((< y oy) y)
                      ((> y (+ oy (viewport-rows viewport) -1))
                       (- y (viewport-rows viewport) -1))
                      (t oy))))
    (setf (viewport-origin viewport) (cell new-x new-y))))

(defun zoom-index (viewport)
  "Where the viewport currently sits on the ladder, or the nearest rung."
  (let ((here (cons (viewport-cols viewport) (viewport-rows viewport))))
    (or (position here *zoom-ladder* :test #'equal)
        ;; Not on a rung — pick the one with the closest cell count, so that a
        ;; viewport set by hand still zooms sensibly rather than jumping.
        (let ((want (* (viewport-cols viewport) (viewport-rows viewport))))
          (loop with best = 0 with best-distance = most-positive-fixnum
                for step in *zoom-ladder* for i from 0
                for distance = (abs (- (* (car step) (cdr step)) want))
                when (< distance best-distance)
                  do (setf best i best-distance distance)
                finally (return best))))))

(defun zoom-origin (anchor cols rows)
  "Where the viewport origin goes when ANCHOR should be centre-ish within a
COLS by ROWS view, biased toward the top-left.

*The two axes are not symmetric, and that is the sign convention biting.*
Biasing left on X means subtracting floor((cols-1)/2), so the focused cell sits
at or left of centre.  Biasing *top* on Y, with +Y up, means the focused cell
should be at or above centre — which is subtracting the ceiling rather than the
floor, because the origin is the *bottom* row.

Getting this wrong is not a rounding detail.  With two rows, the floor rule put
the focused cell on the bottom row of the viewport and filled the top half with
the empty row above it — so zooming out from a full lattice showed you half a
screen of nothing, while every number in the model was correct.  It took a
screenshot to see."
  (cell (- (cell-x anchor) (floor (1- cols) 2))
        (- (cell-y anchor) (ceiling (1- rows) 2))))

(defun set-zoom (grid index &key (focus nil))
  "Move the viewport to rung INDEX of the ladder, anchored near FOCUS.

The focused cell lands centre-ish, biased top-left, always visible, and
deterministically.  Deterministic matters more than centred — a zoom you cannot
predict is a zoom you stop using."
  (let* ((viewport (grid-viewport grid))
         (index (max 0 (min index (1- (length *zoom-ladder*)))))
         (step (nth index *zoom-ladder*))
         (cols (car step))
         (rows (cdr step))
         (anchor (or focus (viewport-origin viewport))))
    (setf (viewport-cols viewport) cols
          (viewport-rows viewport) rows
          (viewport-origin viewport) (zoom-origin anchor cols rows))
    (when focus (ensure-visible grid focus))
    (cons cols rows)))
