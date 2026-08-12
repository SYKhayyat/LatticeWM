;;;; policy/motion.lisp --- Directional motion, written once for every boundary.
;;;;
;;;; THE ALGORITHM
;;;;
;;;; From the cursor's leaf, walk *up* the ancestor chain.  At each container
;;;; ask STEP-ADDRESS whether it can satisfy the motion.  The first one that
;;;; can, does; then descend into whatever is there, asking ENTRY-ADDRESS which
;;;; child to land on, until a leaf is reached.
;;;;
;;;; Thirty lines.  What they buy:
;;;;
;;;;   * motion between panes of a split;
;;;;   * motion that *leaves* a split when the direction is across its axis —
;;;;     pressing Up inside a row of side-by-side panes goes to whatever is
;;;;     above the row, not to a neighbour in it;
;;;;   * motion that crosses from a split into the lattice cell next door and
;;;;     lands on that cell's *nearest* pane, entering through the edge it
;;;;     crossed, with no mode and no second command.  PLAN.org's Delta 1 is
;;;;     this property, and it falls out rather than being built;
;;;;   * motion that skips a stack rather than surfacing a hidden tab;
;;;;   * motion through container kinds that did not exist when this was
;;;;     written, provided they answer STEP-ADDRESS and ENTRY-ADDRESS.
;;;;
;;;; Nothing here knows what a split, a stack or a grid is.  That is the test
;;;; of whether the container protocol was drawn in the right place.
;;;;
;;;; WHY MOTION IS GEOMETRIC
;;;;
;;;; Consider a row of two columns, each split into a top and a bottom pane.
;;;; You are in the *bottom* left pane and press Right.  Structural entry rules
;;;; land you at the first child of the right column — the *top* pane — and
;;;; pressing Left then returns you to the top left pane, not where you began.
;;;;
;;;; Motion that does not come back is disorienting out of all proportion to
;;;; how small the bug sounds, because it silently destroys the mental model
;;;; that the layout is a place.  So entry across a container's axis is decided
;;;; by *where you are*, not by an index: the child whose rectangle best lines
;;;; up with the one you left.  That is what Emacs's windmove and i3 both do,
;;;; and it is involutive by construction.
;;;;
;;;; The rectangles come from the last layout, which the runtime caches.  When
;;;; there is no cache — during startup, and in unit tests — a layout is
;;;; computed on the spot over a nominal output.  A tree has tens of nodes, so
;;;; that costs nothing worth measuring, and it means motion behaves identically
;;;; whether or not anything has been drawn yet.

(in-package #:latticewm/policy)

(define-option *motion-reference-rect* '(0 0 1920 1080)
  "The nominal output used to compute geometry for motion when no layout has
been drawn yet, as (X Y WIDTH HEIGHT).

Only the aspect ratio matters, and only for deciding which pane lines up with
which across a boundary.  You will not need to change this.")

(defun motion-rects (policy root)
  "A hash of NODE to RECT for everything under ROOT, from a fresh layout."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (placement (layout-node policy root
                                    (apply #'c:make-rect *motion-reference-rect*))
                       table)
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path visible))
        (setf (gethash node table) rect)))))

(defun best-aligned-address (container reference rects axis)
  "Which child of CONTAINER lines up best with REFERENCE along AXIS?

Returns an address, or NIL when no child has a known rectangle.  'Best' is:
the child whose extent along AXIS contains the reference's centre, and failing
that the child whose extent is nearest to it.  Overlap beats proximity, because
a pane you are partly in front of is the one you mean."
  (when (null rects) (return-from best-aligned-address nil))
  (let* ((horizontal (eq axis :horizontal))
         (centre (if horizontal
                     (+ (c:rect-x reference) (floor (c:rect-w reference) 2))
                     (+ (c:rect-y reference) (floor (c:rect-h reference) 2))))
         (best nil)
         (best-distance nil))
    (dolist (address (c:container-addresses container) best)
      (let* ((child (c:child-at container address))
             (rect (and child (gethash child rects))))
        (when rect
          (let* ((low (if horizontal (c:rect-x rect) (c:rect-y rect)))
                 (high (if horizontal (c:rect-right rect) (c:rect-bottom rect)))
                 (distance (cond ((< centre low) (- low centre))
                                 ((>= centre high) (- centre high -1))
                                 (t 0))))
            (when (or (null best-distance) (< distance best-distance))
              (setf best address best-distance distance))))))))

(defun descend-to-leaf (policy node path direction &key reference rects)
  "Descend from NODE to a leaf, entering each container as if travelling DIRECTION.

REFERENCE is the rectangle being left, and RECTS maps nodes to rectangles;
together they let each container answer geometrically rather than by index.
DIRECTION may be NIL for a non-directional arrival — a jump to a name, a typed
coordinate, a click — in which case each container answers with its first
child instead of its edge-adjacent one."
  (loop
    (unless (c:container-p node) (return path))
    (let ((address (entry-address policy node direction reference rects)))
      (when (null address) (return path))
      (let ((child (c:child-at node address)))
        (when (null child) (return path))
        (setf node child
              path (c:path-append path address))))))

(defun find-motion-target (policy root path direction &key (rects nil rects-supplied))
  "The path motion DIRECTION from PATH arrives at, or NIL when it hits an edge.

This is the whole of directional navigation.  Specialize STEP-ADDRESS or
ENTRY-ADDRESS to change *what a container does*; redefine this only to change
the walk itself, which almost nothing wants to.

RECTS maps nodes to rectangles and is what makes entry geometric.  Omit it and
one is computed; pass NIL explicitly to force purely structural entry."
  (let ((chain (c:resolve-chain root path))
        (rects (if rects-supplied rects (motion-rects policy root))))
    (when (null chain) (return-from find-motion-target nil))
    (let ((reference (gethash (car (last chain)) rects)))
      ;; CHAIN is (root … node); walk it from the deepest container upwards.
      (loop for depth from (1- (length chain)) downto 1
            for container = (nth (1- depth) chain)
            for address = (nth (1- depth) path)
            do (unless (c:container-p container) (return nil))
               (let ((next (step-address policy container address direction)))
                 (cond
                   (next
                    (let ((child (c:child-at container next))
                          (base (c:path-append (subseq path 0 (1- depth)) next)))
                      (return-from find-motion-target
                        (if child
                            ;; Enter through the edge we crossed: travelling
                            ;; right means arriving at the *left* of what is
                            ;; there, at the height we were already at.
                            (descend-to-leaf policy child base direction
                                             :reference reference :rects rects)
                            base))))
                   ((not (motion-escapes-p policy container direction))
                    ;; A container that traps motion: stop rather than ascend.
                    (return-from find-motion-target nil)))))
      nil)))

;;; ------------------------------------------------------------- the screens

(defun output-workspace (output)
  "Which workspace OUTPUT is displaying, or NIL.

Read through here rather than by naming the property, so that the one place
this program decides where that fact lives stays one place -- see
OUTPUT-CONTENT, which rules that it lives on the output's props."
  (c:prop output :workspace))

(defun output-showing (world index)
  "The first output displaying workspace INDEX, or NIL."
  (when (integerp index)
    (find index (c:world-outputs world) :key #'output-workspace :test #'eql)))

(defun output-in-direction (world from direction)
  "The output DIRECTION of FROM, or NIL when there is nothing that way.

Nearest first, by the gap along the axis of travel, so a three-monitor row
steps one screen at a time rather than jumping to the far end.  Monitors are
all described in one logical coordinate space -- river reports position as
well as size -- so this is ordinary geometry and needs no per-output
translation."
  (when from
    (let ((horizontal (c:direction-horizontal-p direction))
          (sign (c:direction-sign direction))
          (best nil)
          (best-gap nil))
      (multiple-value-bind (from-x from-y) (c:rect-center (c:output-rect from))
        (dolist (output (c:world-outputs world) best)
          (unless (eq output from)
            (multiple-value-bind (x y) (c:rect-center (c:output-rect output))
              (let ((gap (* sign (if horizontal (- x from-x) (- y from-y)))))
                (when (and (plusp gap) (or (null best-gap) (< gap best-gap)))
                  (setf best output best-gap gap))))))))))

;;; ----------------------------------------------------- where you last were

(define-option *remember-place* t
  "Come back to where you were standing in a workspace, rather than to its top.

Switching to a workspace used to put the cursor at its first leaf, always.  On
one monitor that is a small annoyance you stop noticing; with two it is the
thing that makes crossing between screens not worth doing, because every
crossing costs you your place and there is no key that gives it back.

The memory is a property of the *workspace*, not of the output.  That is the
smaller and the more correct of the two: the cursor is one place in one model
(see SHOW-WORKSPACE-ON, which rules that a command must not move the keyboard
to another monitor), so what a workspace has to remember is where you stood in
it — and a workspace that moves to the other screen takes that with it, which
is what anybody would expect and what a per-output table would get wrong.

The stored path is relative to the workspace node rather than to the root, so
it stays true if the workspace is not where it was in the stack.  A remembered
path that no longer leads anywhere is not a problem to check for: it goes
through REPAIR-PATH like every other stale path in this program, which lands
you at the deepest surviving part of where you were — still closer than the
top.

Set it to NIL to always arrive at the first pane, which is deterministic and
which some people genuinely prefer.")

(defun note-place (world path)
  "Record PATH as where the cursor is standing in its workspace.

Called on arrival rather than on departure, which is what makes it correct
without a transition to detect: every cursor move records the current place
for the workspace the cursor is now in, so the workspace you *left* was
already recorded by the last move you made inside it."
  (let ((stack (c:world-workspaces world)))
    (when (and *remember-place* stack path)
      (let ((node (c:child-at stack (first path))))
        (when node
          (setf (c:prop node :last-place) (rest path)))))))

(defun remembered-place (world index)
  "The path the cursor should arrive at in workspace INDEX.

The remembered place if there is one, and the workspace itself otherwise —
which is what every caller wanted anyway, so nobody has to write the fallback."
  (let* ((stack (c:world-workspaces world))
         (node (and stack (c:child-at stack index)))
         (place (and *remember-place* node (c:prop node :last-place))))
    (if place (cons index place) (list index))))

(defun note-cursor-arrival (policy world old target)
  "Record where the cursor now is, and tell everything that watches it.

THE ONE PLACE A CURSOR MOVE IS ANNOUNCED.  The three functions below each set
the cursor and then repeated the same two announcements, which is the shape
that lets a fourth writer forget one of them — and the memory above would have
been exactly that kind of thing to forget, since it is invisible until somebody
switches workspaces twice.

ON-FOCUS-CHANGE runs inside IGNORE-ERRORS because it is a policy method: a
broken one must not strand the cursor half-moved.  The hook is separately
guarded by RUN-HOOKS."
  (note-place world target)
  (ignore-errors (on-focus-change policy world old target))
  (run-hooks :focus-changed old target)
  target)

(define-option *motion-crosses-outputs* t
  "Walking off the edge of one monitor arrives on the next one.

MOTION IS CONTINUOUS ACROSS EVERY BOUNDARY, and the screen is the last
boundary this program was not crossing.  Motion already leaves a split when
the direction crosses its axis, and crosses into the lattice cell next door
entering through the edge it left by; pressing Right at the right-hand edge of
the left monitor did nothing at all, which made two monitors feel like two
window managers.

It costs no key.  With one monitor there is nothing in any direction and the
behaviour is unchanged to the character, so this is invisible until a second
screen is plugged in -- which is when somebody wants it.

Where you arrive is where you last were on that screen (see *REMEMBER-PLACE*),
not its first pane, because arriving somewhere you did not choose is what made
the explicit commands not worth pressing either.

Set it to NIL for motion that stops at the edge of a workspace, and reach the
other screen with FOCUS-OUTPUT.")

(defun cross-to-output (policy world direction)
  "Move the cursor to the screen DIRECTION of the one it is on.  The new path,
or NIL when there is no screen that way.

The cursor is one place in one model, so `go to the other monitor' means `go
to the workspace that monitor is displaying' and nothing else changes: neither
output is asked to show anything different, and no window moves.  That is the
whole of what SHOW-WORKSPACE-ON's ruling makes this cost.

WHICH SCREEN YOU ARE ON IS ASKED OF THE WORKSPACE, NOT OF THE PIXELS, and that
is the right question here rather than a cheaper one.  The runtime's
CURRENT-OUTPUT answers by intersection area because it has to say which
monitor a pane straddling the seam is mostly on; this is asking `which screen
is my workspace displayed on', which is what crossing between them is defined
in terms of, and it has an exact answer with no geometry in it at all."
  (let* ((from (output-showing world (first (c:world-cursor world))))
         (to (output-in-direction world from direction))
         (index (and to (output-workspace to))))
    (when (integerp index)
      (jump-cursor policy world (remembered-place world index)))))

(defun move-cursor (policy world direction)
  "Move the world's cursor one step DIRECTION.  Returns the new path, or NIL.

Returning NIL means the motion hit the edge of everything -- the tree, and
then the monitors -- and nothing moved.  Callers should treat that as a no-op
and not as an error: bumping into the edge is an ordinary thing to do and must
not produce a beep, a log line, or a condition."
  (let* ((cached (c:prop world :rect-index))
         (target (find-motion-target policy (c:world-root world)
                                     (c:world-cursor world) direction
                                     :rects (or cached
                                                (motion-rects
                                                 policy (c:world-root world))))))
    (cond
      (target
       (let ((old (c:world-cursor world)))
         (setf (c:world-cursor world) target
               ;; Directional motion is a statement about the tree, so it takes
               ;; the keyboard back from any floating window.  Otherwise
               ;; pressing Left while a dialog is up moves the cursor invisibly
               ;; underneath it, which is the worst kind of bug: nothing looks
               ;; wrong until you type.
               (c:world-focused-float world) nil)
         (note-cursor-arrival policy world old target)))
      ;; The tree had nowhere to go.  The screens might.
      (*motion-crosses-outputs* (cross-to-output policy world direction)))))

(defun jump-cursor (policy world path)
  "Put the cursor at PATH, resolving into a leaf if PATH names a container.

Non-directional arrival, so containers answer with their first child rather
than an edge — DESIGN D20's second rule."
  (let* ((root (c:world-root world))
         (node (c:resolve-path root path))
         (target (if node
                     (descend-to-leaf policy node path nil)
                     (c:repair-path root path))))
    (let ((old (c:world-cursor world)))
      (setf (c:world-cursor world) target
            (c:world-focused-float world) nil)
      (note-cursor-arrival policy world old target))))

(defun repair-cursor (policy world &optional (was (c:world-cursor world)))
  "Put the cursor somewhere valid after the tree changed under it.

WAS may be a path that no longer names a leaf — the usual case being that the
node it pointed at just became a split, so the cursor is now pointing at a
container rather than a place.  Every verb that restructures the tree calls
this; it is REPAIR-PATH plus the focus-change hook."
  (let* ((root (c:world-root world))
         (target (c:repair-path root was))
         (old (c:world-cursor world)))
    (setf (c:world-cursor world) target)
    (unless (c:path-equal old target)
      (note-cursor-arrival policy world old target))
    target))
