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

NIL is DESIGN D3's ruling: plain motion moves exactly one cell whether or not
that cell is occupied, and the modified motion skips.  Raw step is the default
because zoom-out is the primary orientation tool, so motion does not need to
defend against getting lost in empty space — and keeping the default motion
distance-predictable preserves muscle memory.

Turning this on gives the window model's entire *feel* without unwinding
anything, which is DESIGN D18's stated escape hatch.")

(p:define-option *coordinate-overlay* :zoomed-out
  "When cells show their coordinate.

  :ALWAYS       always
  :ZOOMED-OUT   whenever more than one cell is visible (the default)
  :NEVER        never

DESIGN names two-dimensional navigation as the single biggest design risk and
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

DESIGN names getting lost as the single biggest risk in the whole design —
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

(defun cell-tracks (grid rect axis)
  "The screen rectangles of the visible tracks along AXIS inside RECT.

:HORIZONTAL gives one rectangle per visible column, :VERTICAL one per visible
row, each spanning RECT on the other axis.  CELL-RECTS crosses the two.

THE ONE PLACE THAT KNOWS HOW BIG A CELL IS.  It was two, and the second one was
wrong in a way only :FIXED could see: MAP-MODE-P divided the output width by the
column count, which is the :FIT answer, so at eight columns of a nine-hundred-
pixel :FIXED lattice it computed 240 pixels, believed the cells were too small
to render, and put the drawn map up over cells that were full size.  Both
callers ask this now, so a third zoom mode is one clause here rather than a
clause here and a division somewhere else."
  (let* ((viewport (grid-viewport grid))
         (horizontal (eq axis :horizontal))
         (origin (if horizontal
                     (cell-x (viewport-origin viewport))
                     (cell-y (viewport-origin viewport))))
         (count (if horizontal (viewport-cols viewport) (viewport-rows viewport)))
         (weights (loop for i from 0 below count
                        collect (if horizontal
                                    (col-width grid (+ origin i))
                                    (row-height grid (+ origin i))))))
    (ecase *zoom-mode*
      (:fit (c:divide-rect rect axis weights :gap *cell-gap*))
      (:fixed (fixed-tracks rect axis weights
                            (if horizontal *cell-width* *cell-height*)
                            *cell-gap*)))))

(defun cell-rects (policy grid rect)
  "Rectangles for every visible cell, as an alist of (ADDRESS . RECT).

THE SIGN INVERSION LIVES HERE, and nowhere else.  Lattice +Y is up (D2);
Wayland +Y is down.  One line, one function, deliberately.

A cell whose rectangle has run entirely past the edge of RECT is not a cell that
is partly visible, it is a cell that is not on screen, and it is omitted for the
same reason a cell outside the viewport is: LAYOUT-CHILDREN's omissions are what
get *hidden*, and a window positioned wholly outside the output but never hidden
is a window drawn on top of somebody else's desktop.  This can only happen under
:FIXED, where the tracks do not adapt to the space; :FIT tiles RECT exactly and
nothing is ever dropped."
  (declare (ignorable policy))
  (let* ((viewport (grid-viewport grid))
         (ox (cell-x (viewport-origin viewport)))
         (oy (cell-y (viewport-origin viewport)))
         (cols (viewport-cols viewport))
         (rows (viewport-rows viewport))
         (columns (cell-tracks grid rect :horizontal))
         (bands (cell-tracks grid rect :vertical)))
    (loop for row from 0 below rows
          append (loop for col from 0 below cols
                       for column = (nth col columns)
                       ;; +Y is up, so the *last* band on screen is the
                       ;; *lowest* row number.  This is the inversion.
                       for band = (nth (- rows 1 row) bands)
                       for box = (and column band
                                      (c:make-rect (c:rect-x column)
                                                   (c:rect-y band)
                                                   (c:rect-w column)
                                                   (c:rect-h band)))
                       when (and box (c:rect-intersect box rect))
                         collect (cons (cell (+ ox col) (+ oy row)) box)))))

(defun fixed-tracks (rect axis weights size gap)
  "One track per weight along AXIS, each WEIGHT * SIZE pixels, from RECT's edge.

Absolute rather than proportional, which is the whole of :FIXED: a track's pixel
size depends on its own weight and on nothing else, so panning cannot resize a
window — PLAN's Delta 3, which is the reason this mode exists.

The trailing track runs off the end of RECT and is cropped by CLIP-RECT —
river's set_content_clip_box redraws the border at the crop edge, so it reads
as a cleanly cut cell rather than a window sliced in half.  The sliver is
arguably a feature: it is peripheral vision telling you something is over
there.

THE WEIGHTS USED TO BE DROPPED HERE.  This took a COUNT and gave every track
exactly SIZE, so RESIZE-COLUMN and RESIZE-ROW did nothing whatsoever under
:FIXED — in the one mode RESIZE-COLUMN's own first-use warning tells you to
switch to when :FIT's panning cost becomes intolerable.  The advice sent you to
the mode where the thing you had just done stopped working, silently, and the
test that covered :FIXED asserted a uniform lattice.  D8's ruling is that a
size belongs to a column and spans every row; under :FIXED the weight scales
the absolute size, and EQUALIZE-CELLS puts it back."
  (let ((horizontal (eq axis :horizontal))
        (offset 0))
    (loop for weight in weights
          for extent = (max 1 (round (* weight size)))
          collect (if horizontal
                      (c:make-rect (+ (c:rect-x rect) offset) (c:rect-y rect)
                                   extent (c:rect-h rect))
                      (c:make-rect (c:rect-x rect) (+ (c:rect-y rect) offset)
                                   (c:rect-w rect) extent))
          do (incf offset (+ extent gap)))))

(defun tag-cell (node address bounds)
  "Record which cell a subtree is and what it is allowed to spill over.

On PROPS rather than a slot, because it is presentation state that a node has
no business carrying permanently — which is exactly the case DESIGN D20
predicted an extension would need PROPS for.

BOUNDS IS THE RECTANGLE THE PLANE WAS LAID OUT IN, and it is here because
CLIP-RECT is asked about a *window* and has to answer with the *plane's* edge.
It is rewritten on every relayout — the layout driver calls LAYOUT-CHILDREN
before it descends — so the bound follows the output, the reserved space and the
gaps without anything having to invalidate it, and a cached first answer cannot
outlive the layout that produced it.

WHAT IT DOES NOT COVER, since the whole point of this key is that a silent NIL
cost the method its life: a plane nested inside a *cell of another plane* tags
its own subtree afterwards and so its own rectangle wins, and if that cell is
itself one of the partial ones, the inner plane's overhang is cropped at the
inner rectangle rather than at both.  One extra sliver on a nested plane at the
screen edge, and the honest reason it is left is that fixing it means keeping a
bound from the enclosing pass, which is a cache, and a stale cache here is the
failure that does not announce itself."
  (when node
    (setf (c:prop node :lattice/address) address
          (c:prop node :lattice/parity) (evenp (+ (cell-x address)
                                                  (cell-y address)))
          (c:prop node :lattice/viewport-bounds) bounds)
    ;; The cell's whole subtree answers for the cell, so a split *inside* a
    ;; cell borders in the cell's colour rather than reverting to neutral.
    (when (c:container-p node)
      (dolist (child-address (c:container-addresses node))
        (tag-cell (c:child-at node child-address) address bounds))))
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
      (tag-cell (c:child-at grid (car entry)) (car entry) rect))))

(defmethod p:keys-hint ((policy lattice-policy) world)
  "Add the two facts about cells that nothing else can teach.

The core hint says how to open, split, move and close.  Loading the lattice
changes what \"move\" means -- walking off the edge of a cell carries you into
the next one, which is how a cell comes to exist -- and adds the only way to
*see* that there are cells at all.

Neither is discoverable, and the first one is unusually hard: it works by
*not* existing.  There is no new-cell command, no mode and no boundary to
cross deliberately, which is the design's whole claim -- \"motion runs straight
through the cell boundary as though it were not there\".  Something that works
by not existing cannot be found by looking for it, and the report from the
first person to use it was exactly that: \"how do i make a new window in
lattice?\".

This method is also the smallest honest demonstration of the extension
surface: a loaded extension changing what the status line teaches, in four
lines, with no edit to anything under src/."
  (let ((core (call-next-method)))
    (when core
      (let ((mod (string-capitalize (string p:*modifier*))))
        (format nil "~a  |  past a cell edge = next cell  ~a+- zoom out"
                core mod)))))

(defmethod p:cursor-place-name ((policy lattice-policy) world)
  "The cell coordinate: 3,-2 rather than 0.1.0.

THIS METHOD IS THE ONE THE CORE USED TO WRITE FOR US.  The shipped ECHO-CONTENT
read :LATTICE/ADDRESS off the node itself and destructured the cons inline, in
src/policy/appearance.lisp, so the extension's private property key and its
private representation of an address were both compiled into the core.  It was
never a core *edit* the lattice asked for -- the lattice never overrode
anything here, because it did not have to -- which is exactly why no gate saw
it and why FINDINGS.org's list did not have it.

Four lines, on the outside, where it always belonged.  Everything else the
lattice puts in the status line already worked this way; see KEYS-HINT."
  (let* ((node (c:world-node-at world))
         (address (and node (c:prop node :lattice/address))))
    (if address (cell-string address) (call-next-method))))

(defmethod p:clip-rect ((policy lattice-policy) node rect)
  "Crop a cell that hangs over the viewport edge.

This is the payoff of the best find in the protocol.  Under :FIXED zoom the
trailing cell is usually partial, and set_content_clip_box crops the content
*and* redraws the compositor's border at the crop edge, for free.  It was
expected to cost a week of bad approximation.

AND IT COULD NOT FIRE, from the commit that wrote it until the one that wrote
this paragraph.  :LATTICE/VIEWPORT-BOUNDS occurred exactly twice in the whole
tree — this read, and a docstring — and never once in a SETF, so BOUNDS was
always NIL, the method always fell through to CALL-NEXT-METHOD, and
CALL-NEXT-METHOD is the shipped `nothing overhangs here, clip nothing'.  A
method on the right generic, dispatching on the right class, doing nothing at
all: DESIGN's best find, FINDINGS' list of what the lattice overrides, and two
documents describing the cropped trailing cell as shipped behaviour.  TAG-CELL
records the bounds now, and gate 13 is the check that would have said so — a
property the program reads and nothing writes is the same bug as an option the
program registers and nothing reads, one mechanism over."
  (let ((bounds (c:prop node :lattice/viewport-bounds)))
    (cond ((null bounds) (call-next-method))
          (t (let ((visible (c:rect-intersect rect bounds)))
               (if (and visible (not (c:rect-equal visible rect)))
                   visible
                   (call-next-method)))))))

(defmethod p:gaps ((policy lattice-policy) (grid grid))
  "Pixels between cells, which is *CELL-GAP* and not *GAPS*.

The two are separate knobs on purpose: the gap between panes inside a cell is
about reading two windows side by side, and the gap between cells is the width
of the seam that says *this is a different place*.  Somebody who wants a
seamless plane of gapped windows sets one to zero and the other to eight, and
a single option could not say that.

Narrowed to GRID, so *GAPS* still answers for every split and stack in the
tree — including the ones inside a cell.  `latticewm --extension-surface'
prints this method under *GAPS* as an `overridden for', and prints *CELL-GAP*
under GAPS itself as an `answered from' — the option this method takes away and
the option it puts in its place, each named where somebody would look for it.
The second half of that was missing until gate 17: the surface said what an
override removed and never what it added, so the knob this method offers was
findable only by reading this file."
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
         ;;
         ;; Three states, not two: FOCUSEDP is :CURSOR when a floating window
         ;; has the keyboard and this cell still holds the cursor.  It gets a
         ;; brightness between the two, because it is between the two — you
         ;; need to see which cell the dialog will drop you back into.
         (let ((keyboardp (eq focusedp t))
               (cursorp (eq focusedp :cursor)))
           (multiple-value-bind (hr hg hb)
               (hsv-to-rgb (coordinate-hue address)
                           (cond (keyboardp 0.45) (cursorp 0.50) (t 0.55))
                           (cond (keyboardp 1.00) (cursorp 0.60) (t 0.30)))
             (let ((mix (* *coordinate-tint*
                           (cond (keyboardp 0.55) (cursorp 0.75) (t 1.0)))))
               (values (+ (* r (- 1 mix)) (* hr mix))
                       (+ (* g (- 1 mix)) (* hg mix))
                       (+ (* b (- 1 mix)) (* hb mix))
                       a)))))))))

(defmethod p:border-width ((policy lattice-policy) node focusedp)
  "A thicker border on the cell that has the keyboard.

Colour alone is not enough at a glance across a 3x2 view, and a border that is
merely brighter can lose to a bright application behind it.  Thickness cannot.

(EQ FOCUSEDP T) rather than a truth test: :CURSOR means a float has the
keyboard, and thickening the cell underneath it would claim the keystrokes are
going there."
  (let ((base (call-next-method)))
    (if (and (eq focusedp t) (c:prop node :lattice/address)) (+ base 2) base)))

;;; ==================================================================
;;; MOTION — the one that makes the whole thing feel like a place
;;; ==================================================================

(defmethod p:step-address ((policy lattice-policy) (grid grid) address direction)
  "Move one cell, creating it if it does not exist yet.

The plane is infinite, so 'the cell to the left' always exists in principle;
arriving is what brings it into being.  DESIGN D3 rules that plain motion
moves exactly one cell whether or not it is occupied, and D18 rules that focus
is a place — together those *require* that moving into empty space works, or
both rulings have no referent.

With *SKIP-EMPTY-CELLS*, motion crosses a run of empty cells in one press
instead: the spreadsheet Ctrl+Arrow idiom, and DESIGN D3's modified motion."
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

A *directional* arrival is motion, and motion answers structurally: the cell
nearest the viewport origin.  Motion cannot in fact reach a plane from outside
one, since MOTION-ESCAPES-P traps it, but a container that answers only half
the protocol is a trap of its own.

A *non-directional* arrival is the interesting one, and it is what switching
workspace is: you are stepping from one plane to the plane behind it, and the
question 'where do I land?' has no structural answer — only a design one.
*WORKSPACE-ENTRY* is that decision.

THIS MUTATES, WHICH AN ACCESSOR MAY NOT AND AN ARRIVAL MAY.  Landing on a cell
that does not exist yet creates it, and pans the viewport so you can see it, on
exactly the argument STEP-ADDRESS already makes: the plane is infinite, so the
cell always exists in principle and arriving is what brings it into being.  The
directional branch touches nothing, which is what keeps speculative motion
queries free of side effects."
  (declare (ignore reference rects))
  (if direction
      (c:default-address grid)
      (let ((address (workspace-entry-cell grid)))
        (cond ((null address) (c:default-address grid))
              (t (ensure-cell grid address)
                 (ensure-visible grid address)
                 address)))))

(defmethod p:on-focus-change ((policy lattice-policy) world old new)
  "Remember which cell each plane was left standing in.

Per *plane*, on the plane's own PROPS, rather than one global 'last cell' —
because the fact worth keeping is 'workspace 3 was left at (2,-1)', and a
single global slot answers a question nobody asked.  This is what
*WORKSPACE-ENTRY* :REMEMBERED reads, and it costs one PROP write per focus
change.

CALL-NEXT-METHOD is not optional: the motion layer's method is what maintains
the focus history that :MRU close-focus consults, and shadowing it would break
a feature two layers away with no visible connection to the lattice."
  (let ((chain (c:resolve-chain (c:world-root world) new)))
    (loop for node in chain
          for address in new
          when (typep node 'grid)
            do (setf (c:prop node :lattice/last-cell) address)))
  (call-next-method))

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
;;; DESIGN D7, the central usability ruling: zoom, resize and pan are three
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

;;; ==================================================================
;;; THE Z AXIS — infinitely many planes, one behind another
;;; ==================================================================
;;;
;;; The workspace list has always been infinite: it is a STACK at the root of
;;; the tree, a stack grows, and WORKSPACE 40 on a machine with three makes
;;; forty.  What it was not was *made of planes*.
;;;
;;; ENABLE wraps the workspaces that exist when it runs.  Every workspace made
;;; afterwards came from one of four sites in the core that each built an empty
;;; pane by hand, so workspace 7 was a pane on a machine whose first six were
;;; planes — no error, no log line, and invisible until somebody went there and
;;; found that Super+minus did nothing.  The plane was a wrapper applied once,
;;; not the shape of a workspace.
;;;
;;; MAKE-WORKSPACE is the core's answer, and this section is the lattice's.
;;; With it, "infinite workspaces of lattices one behind another" is not an
;;; arrangement you set up — it is the shape of the thing, permanently, and it
;;; costs one method.

(defvar *default-viewport* (cons 1 1)
  "The zoom ENABLE was called with, as (COLUMNS . ROWS).

Remembered so that a workspace created three hours later is born the same shape
as the ones created at startup.  Not an option: it is a record of what the
configuration already said, and a user who wants to change it has
*NEW-WORKSPACE-ZOOM* to say so with.")

(p:define-option *new-workspace-zoom* :inherit
  "The viewport a newly created plane is born with.

  :INHERIT      the same zoom as the plane you are standing on, falling back to
                whatever ENABLE was called with.  The default, and the one that
                makes the planes read as *stacked*: stepping back a workspace
                changes what you are looking at and not how much of it.
  (COLS . ROWS) exactly that, every time — (1 . 1) for 'every new workspace
                starts zoomed in on one cell'.

Zoom is a view control (D7), so this is a statement about where you arrive, not
about the layout you arrive in.  Nothing is laid out differently for it.")

(p:define-option *new-workspace-origin* :inherit
  "Where a newly created plane's viewport sits.

  :INHERIT   the same origin as the plane you are standing on.  The default.
             Two planes at the same origin with the same zoom are literally one
             behind the other: the same window of coordinates, a different
             plane under it.
  :ORIGIN    always (0,0), so every plane starts at the same well-known place.
  (X . Y)    exactly that cell.

Pairs with *WORKSPACE-ENTRY*: this decides what the new plane shows, that
decides which cell you stand in on it.  :INHERIT and :ALIGNED together are the
stacked-planes reading of a workspace switch; :ORIGIN and :ORIGIN are the
'every workspace is its own world' reading.  Both ship, per DESIGN P1.")

(p:define-option *new-workspace-cells* nil
  "Cells a new plane starts with, as a list of (X . Y) conses.

NIL — the default — means one empty cell at the origin the plane was given,
which is DESIGN D19's starting state repeated per plane.  A list makes every
new workspace start pre-divided:

    (setf *new-workspace-cells* (list (cell 0 0) (cell 1 0)))

Cells, not windows.  A cell is a place; what goes in it is a spawn, and this
option deliberately cannot open anything — that is what the :WORKSPACE-CHANGED
hook is for.")

(p:define-option *workspace-entry* :remembered
  "Which cell you land in when you arrive on a plane from outside it.

  :REMEMBERED  the cell you were last standing in on *that* plane, falling
               back to :ALIGNED on a plane you have never visited.  The
               default, and the one that makes a workspace feel like a room
               you left rather than a room you are shown into.
  :ALIGNED     the same coordinate you are standing on now.  The literal
               reading of 'one behind the other': the planes share a coordinate
               space and switching moves you along Z with X and Y untouched.
  :ORIGIN      always (0,0).
  :OCCUPIED    the first occupied visible cell — the behaviour before this
               option existed, and the only one of the four that never creates
               a cell by arriving.

The first three create the cell they name if it is not there yet, which is
STEP-ADDRESS's rule and not a new one: on an infinite plane every cell exists
in principle, and arriving is what brings it into being.")

(defun cursor-grid (world)
  "The innermost plane the cursor is inside, or NIL.

Ancestor search rather than a fixed depth, so a plane nested inside a split
inside another plane answers correctly — see CURRENT-GRID, which is this with
the world already supplied."
  (when world
    (let* ((root (c:world-root world))
           (chain (c:resolve-chain root (c:world-cursor world))))
      (loop for node in (reverse (or chain (list root)))
            when (typep node 'grid) return node))))

(defun cursor-cell (world)
  "The cell address the cursor is standing in, or NIL."
  (when world
    (let* ((root (c:world-root world))
           (path (c:world-cursor world))
           (chain (c:resolve-chain root path)))
      (loop for node in chain
            for address in path
            when (typep node 'grid) return address))))

(defun workspace-entry-cell (grid)
  "The cell a non-directional arrival at GRID lands on, or NIL for 'ask the
container'.

NIL rather than a coordinate for :OCCUPIED, because 'the first occupied visible
cell' is a question only the grid can answer and re-deriving it here would be a
second implementation of DEFAULT-ADDRESS waiting to disagree with the first."
  (let ((world r:*world*))
    (case *workspace-entry*
      (:remembered (or (c:prop grid :lattice/last-cell)
                       (cursor-cell world)
                       (cell 0 0)))
      (:aligned (or (cursor-cell world) (cell 0 0)))
      (:origin (cell 0 0))
      (:occupied nil)
      ;; An unrecognised value is a typo in a configuration file, and the
      ;; useful response to a typo is the shipped behaviour plus a log line —
      ;; not a workspace switch that signals.
      (t (r:logmsg :warn "lattice: *workspace-entry* is ~s, which is not one of ~
                          :remembered :aligned :origin :occupied; using :remembered"
                   *workspace-entry*)
         (or (c:prop grid :lattice/last-cell) (cursor-cell world) (cell 0 0))))))

(defun new-workspace-viewport (world)
  "The (COLS . ROWS) and origin a plane created now should be born with.

Returns (values COLS ROWS ORIGIN)."
  (let* ((here (cursor-grid world))
         (viewport (and here (grid-viewport here)))
         (zoom (if (and (consp *new-workspace-zoom*)
                        (integerp (car *new-workspace-zoom*))
                        (integerp (cdr *new-workspace-zoom*)))
                   *new-workspace-zoom*
                   (if viewport
                       (cons (viewport-cols viewport) (viewport-rows viewport))
                       *default-viewport*)))
         (origin (cond ((and (consp *new-workspace-origin*)
                             (integerp (car *new-workspace-origin*))
                             (integerp (cdr *new-workspace-origin*)))
                        *new-workspace-origin*)
                       ((eq *new-workspace-origin* :origin) (cell 0 0))
                       (viewport (cell (cell-x (viewport-origin viewport))
                                       (cell-y (viewport-origin viewport))))
                       (t (cell 0 0)))))
    (values (max 1 (car zoom)) (max 1 (cdr zoom)) origin)))

(defmethod p:make-workspace ((policy lattice-policy) world index)
  "A workspace is a plane.  Always, and not only the ones ENABLE happened to see.

This one method is the entire Z axis.  Everything else that 'infinite
workspaces of lattices one behind another' needs was already true: the
workspace list is a stack and grows on demand, a stack shows one child at a
time, SERIALIZE-NODE writes planes and reads them back, and switching is a
verb the core already had.  What was missing was that the *default shape of a
new workspace* was not a policy decision, so it could not be the lattice's.

INDEX is ignored by the shipped answer — every plane is born alike — and is
passed because a method that wants 'workspace 1 is the wide one' should not
have to count the stack to find out which one it is being asked about.

IT TAKES *NEW-WORKSPACE* AWAY AND OWES ONE BACK.  This is the whole of a total
override: it wins wherever the shipped method applied and never calls
CALL-NEXT-METHOD, so the option that method reads decides nothing while the
lattice is enabled — printed by --list-options, documented, and inert.  Gate 15
is the check that says so out loud, and the debt is paid in
*NEW-WORKSPACE-CELLS*: the same decision, at the same tier, for somebody who
will never write a DEFMETHOD."
  (declare (ignore index))
  (multiple-value-bind (cols rows origin) (new-workspace-viewport world)
    ;; The cell list is computed rather than left to MAKE-GRID's default,
    ;; because that default is a cell at (0,0) — the right guess for a plane
    ;; nobody has told anything to, and the wrong one for a plane whose
    ;; viewport starts at (4,-2), which would open showing a hole.
    (let* ((cells (or (remove-if-not #'consp *new-workspace-cells*)
                      (list origin)))
           (grid (make-grid :cols cols :rows rows
                            :cells (loop for address in cells
                                         collect (cons address (c:make-leaf))))))
      (setf (viewport-origin (grid-viewport grid)) origin)
      grid)))

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
