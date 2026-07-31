;;;; policy/conventional.lisp --- The shipped behaviour.
;;;;
;;;; Every method here is a default you are expected to override.  They are
;;;; written to be *readable as examples* rather than merely correct, because
;;;; the realistic way somebody changes one is by copying it and editing it —
;;;; and because PLAN.org's honest requirement is that a cheap model must be
;;;; able to produce a fourth worked example by pattern-matching on these.
;;;;
;;;; The behaviour it adds up to is conventional on purpose: recursive splits
;;;; like Emacs and hyprland, tabs, workspaces, per-window floating, and
;;;; minimize that takes a window out of the tree.  Not novel, not trying to
;;;; be.  It is the smallest thing that is a usable daily driver, and therefore
;;;; the smallest thing that proves the core works.

(in-package #:latticewm/policy)

(defclass conventional-policy (policy)
  ((%name :initform "conventional"))
  (:documentation
   "Recursive splits, tabs, workspaces, floats, minimize-out-of-tree.

The shipped default, and the base class the lattice extends rather than
replaces — so that everything below stays true unless the lattice explicitly
says otherwise.

NOTE WHAT THIS CLASS DOES *NOT* HAVE: methods.  Every default below is
specialised on POLICY, the base class, and this subclass deliberately adds
nothing.  The reason is the difference between an extension surface that works
and one that only looks like it does.

If the defaults lived here, then a user writing the obvious thing —

    (defmethod gaps ((policy conventional-policy) container) 8)

would *replace* the shipped method rather than extend it, and CALL-NEXT-METHOD
inside it would signal NO-NEXT-METHOD.  Every worked example in this project
uses CALL-NEXT-METHOD, because 'do what you were going to do, plus this' is
overwhelmingly the common case; so the class a user names must be strictly more
specific than the class the defaults are on.

This was found by writing the examples and running them, which is the argument
for shipping examples as tests.  See FINDINGS.org."))

;;; ==================================================================
;;; TIER 0 — the values.  Every P1 fork in the design appears here.
;;; ==================================================================

(define-option *gaps* 0
  "Pixels of empty space between adjacent panes.  0 is a clean grid; 4 to 8
gives the airy look, at the cost of that much screen.")

(define-option *outer-gaps* 0
  "Pixels of empty space between the layout and the edge of the output.")

(define-option *border-width* 2
  "Border thickness in pixels, drawn by the compositor around every window.")

(define-option *focused-border-color* '(0.40 0.65 1.00 1.0)
  "Border colour of the focused pane, as (R G B A) from 0.0 to 1.0.

Do not make this subtle.  Focus may rest on an *empty* pane, which has no
window to look at, and an empty pane that does not obviously hold the cursor
reads as a broken keyboard rather than as a place.")

(define-option *unfocused-border-color* '(0.22 0.22 0.26 1.0)
  "Border colour of every pane that is not focused.")

(define-option *empty-pane-color* '(0.30 0.30 0.36 1.0)
  "Border colour drawn around a focused empty pane.")

(define-option *spawn-mode* :split
  "Where a new window goes.  README D14 rules that all three ship.

  :SPLIT       split the focused pane and put it beside — Emacs, hyprland.
  :FILL-FIRST  drop it in the nearest empty pane if there is one, else split.
  :STACK       add it as a tab on the focused pane.

:SPLIT is the shipped default because it is the behaviour nobody has to be
taught.")

(define-option *split-axis* :longer
  "How a fresh split is cut.

  :LONGER      along whichever axis is longer, so panes tend towards square.
  :HORIZONTAL  always side by side.
  :VERTICAL    always stacked.

:LONGER is the default because fixed-axis splitting produces slivers as soon
as you go three deep, and everybody's first configuration change in the
fixed-axis window managers is to add a 'split the other way' binding.")

(define-option *new-child-side* :after
  "Which side of the existing pane a new sibling lands on: :AFTER or :BEFORE.")

(define-option *collapse-degenerate-splits* t
  "Dissolve a split once it is down to one child.

T is Emacs's behaviour and keeps the tree shallow.  NIL is i3's, where an
emptied container persists and can be filled again.")

(define-option *move-into-occupied* :split
  "What dropping a window onto an occupied pane means: :SPLIT, :SWAP or :STACK.

:SPLIT is shipped because it is the only one that never destroys structure.
SWAP remains a separate verb with its own binding, so nothing is lost.")

(define-option *focus-after-close* :stay
  "Where the cursor goes when the focused window closes.

  :STAY  the deepest surviving place along the old path (the default).
  :MRU   the most recently used window anywhere.
  :NEXT  the next pane in layout order.

:STAY implements the governing property that *nothing moves the viewport
except the user*.  Under :MRU the next window can be anywhere, so closing
something can teleport you across your desktop.")

(define-option *float-dialogs* t
  "Float windows that river reports as having a parent — dialogs, file
pickers and similar.")

(define-option *focus-follows-mouse* nil
  "Move the cursor to whatever pane the pointer is over.")

(define-option *focus-new-windows* t
  "Move the cursor onto a window when it appears.")

(define-option *empty-pane-keys*
  '((#\e . "editor") (#\t . "terminal") (#\b . "browser") (#\f . "files"))
  "README D19's keysym-to-command table for typing in an empty pane.

While the cursor rests on an empty pane, an unbound printable key is looked up
here and the named command is run, so the empty pane is a spawn menu with no
menu.  It has to be a table rather than a single default because the keystroke
is consumed as a trigger and cannot be replayed into the application that
opens — nothing in either river protocol synthesises input — so under a single
default, typing `ls` at an empty pane opens a terminal showing `s`.")

(define-option *float-fraction* 3/5
  "Fraction of the output a floated window takes when it has no opinion.")

(define-option *smart-gaps* t
  "Drop gaps and borders entirely when a workspace holds exactly one window.")

;;; ==================================================================
;;; LAYOUT
;;; ==================================================================

(defmethod gaps ((policy policy) container)
  (declare (ignore container))
  *gaps*)

(defmethod gaps ((policy policy) (container c:stack))
  "Tabs share one rectangle, so a gap between them would be a gap between a
thing and itself."
  0)

(defmethod layout-children ((policy policy) (split c:split) rect)
  "Divide RECT along the split's axis in proportion to its weights.

The whole of the split layout model is this one call.  Weights are relative,
so a resize is a transfer between two adjacent weights and needs to know
nothing about pixels — which is why resizing behaves identically at every zoom
level and on every monitor."
  (let ((addresses (c:container-addresses split)))
    (mapcar #'cons
            addresses
            (c:divide-rect rect (c:split-axis split) (c:weights split)
                           :gap (gaps policy split)))))

(defmethod layout-children ((policy policy) (container c:container) rect)
  "The fallback for a container kind this policy has never heard of.

Give the whole rectangle to DEFAULT-ADDRESS's child and hide the rest — that
is, behave like a stack, which is the only choice that is correct for *any*
container: it shows something, it shows only one thing, and it cannot overlap.

CORE EDIT, added while building the lattice.  It is one of exactly two the
experiment required, and it is recorded in FINDINGS.org.  The finding is real:
without it, LAYOUT-CHILDREN was a *partial* function on the container protocol,
so a policy meeting a container type it did not know signalled NO-APPLICABLE-
METHOD rather than degrading.  A protocol whose operations are partial is not
an extension point, it is a set of special cases with a docstring — an
extension author would have had to know, from nowhere, that adding a container
kind obliges them to also teach every *other* policy about it."
  (let ((address (c:default-address container)))
    (when (c:child-at container address)
      (list (cons address rect)))))

(defmethod layout-children ((policy policy) (stack c:stack) rect)
  "The selected child gets everything; the rest are not laid out at all, and
are therefore hidden."
  (let ((address (stack-visible-address policy stack)))
    (when (c:child-at stack address)
      (list (cons address rect)))))

(defmethod stack-visible-address ((policy policy) (stack c:stack))
  (c:stack-selected stack))

(defmethod layout-node ((policy policy) node rect)
  "Walk NODE, assigning rectangles, and return every placement.

Returns a list of (NODE PATH RECT VISIBLE-P), parents before children.  Note
that it descends into children the layout *omitted* as well, marking them
invisible: a stack's unselected tabs and a lattice's offscreen cells still
hold windows, and river shows a window unless it is explicitly hidden.  Missing
this is how offscreen windows end up drawn on top of your desktop."
  (let ((out '()))
    (labels ((walk (node path rect visible)
               (push (list node path rect visible) out)
               (when (c:container-p node)
                 (let* ((placed (if visible
                                    (layout-children policy node rect)
                                    '()))
                        (seen '()))
                   (dolist (entry placed)
                     (destructuring-bind (address . child-rect) entry
                       (push address seen)
                       (let ((child (c:child-at node address)))
                         (when child
                           (walk child (c:path-append path address)
                                 child-rect
                                 (and visible (visible-p policy child)))))))
                   ;; Everything the layout did not place is hidden, but must
                   ;; still be walked so its windows get hidden too.
                   (dolist (address (c:container-addresses node))
                     (unless (member address seen
                                     :test (lambda (a b)
                                             (c:address-equal node a b)))
                       (let ((child (c:child-at node address)))
                         (when child
                           (walk child (c:path-append path address)
                                 (c:make-rect 0 0 0 0) nil)))))))))
      (walk node '() rect t))
    (nreverse out)))

(defmethod visible-p ((policy policy) node)
  (declare (ignore node))
  t)

(defmethod window-dimensions ((policy policy) (leaf c:leaf) rect)
  "Propose the pane's size, less the border on each side.

River draws borders *around* the content rectangle, so a window given the full
pane would overflow it by twice the border width."
  (let ((inset (* 2 (border-width policy leaf nil))))
    (values (max 1 (- (c:rect-w rect) inset))
            (max 1 (- (c:rect-h rect) inset)))))

(defmethod gravity ((policy policy) (leaf c:leaf) rect width height)
  "Centre a window that came back smaller than the pane it was given.

This is the *involuntary* case — a terminal that quantised to its cell size, a
dialog that refuses to grow.  The deliberate case, where a user wants a window
to take less than its pane, is served by splitting against an empty pane
instead (README D17), which is why one sane answer suffices here rather than a
gravity policy language."
  (let ((w (min width (c:rect-w rect)))
        (h (min height (c:rect-h rect))))
    (c:make-rect (+ (c:rect-x rect) (floor (- (c:rect-w rect) w) 2))
                 (+ (c:rect-y rect) (floor (- (c:rect-h rect) h) 2))
                 w h)))

(defmethod border-width ((policy policy) node focusedp)
  (declare (ignore node focusedp))
  *border-width*)

(defmethod border-color ((policy policy) node focusedp)
  (let ((color (cond ((and focusedp (typep node 'c:leaf) (c:leaf-empty-p node))
                      *empty-pane-color*)
                     (focusedp *focused-border-color*)
                     (t *unfocused-border-color*))))
    (values-list color)))

(defmethod clip-rect ((policy policy) node rect)
  "Nothing overhangs in the conventional layer, so nothing is clipped.

The lattice overrides this, and it is where river's set_content_clip_box earns
its keep: a cell half-scrolled off the viewport edge is cropped and its border
is redrawn at the crop edge, so it reads as a cleanly cut cell rather than a
window sliced in half."
  (declare (ignore node rect))
  nil)

(defmethod outer-rect ((policy policy) (output c:output))
  (c:rect-inset (c:output-rect output) *outer-gaps*))

(defmethod render-order ((policy policy) placements)
  "Tiled nodes in layout order, then floats, then anything marked as overlay.

River says the initial position of a node in the render list is *undefined*,
so every node must be ordered explicitly or overlapping windows flicker."
  (stable-sort (copy-list placements) #'<
               :key (lambda (placement)
                      (let ((node (first placement)))
                        (cond ((c:prop node :overlay) 2)
                              ((and (typep node 'c:leaf)
                                    (c:leaf-window node)
                                    (c:window-floating-p (c:leaf-window node)))
                               1)
                              (t 0))))))

;;; ==================================================================
;;; MOTION
;;; ==================================================================

(defmethod step-address ((policy policy) (split c:split)
                         address direction)
  "Within a split, motion works only along the split's own axis.

Moving up inside a row of side-by-side panes is not a smaller motion — it is a
motion this container cannot answer, so it returns NIL and the caller ascends
to ask the parent.  That is what makes a vertical move inside a horizontal
split leave the split, which is what every user expects and what most tiling
window managers get wrong on the first try."
  (when (eq (c:direction-axis direction) (c:split-axis split))
    (let ((next (+ address (c:direction-sign direction))))
      (when (and (<= 0 next) (< next (c:container-count split)))
        next))))

(defmethod step-address ((policy policy) (stack c:stack)
                         address direction)
  "A stack answers no spatial direction.

You do not arrive in another workspace, or another tab, by pressing Left.
Switching is its own verb, and that is what keeps a stack legible."
  (declare (ignore address direction))
  nil)

(defmethod entry-address ((policy policy) (split c:split)
                          direction reference rects)
  "Enter through the edge you crossed, at the height you were already at.

Two rules, and they answer different questions:

  * *Along* the split's own axis — travelling right into a horizontal split —
    entry is by edge: you arrive at the leftmost child.  README D20.  This is
    what guarantees every pane is reachable by directional motion, and it is
    why last-focus memory is not used here: with memory, a cell whose right
    pane was last focused would swallow a rightward move and the left pane
    could never be reached at all.

  * *Across* the axis — travelling right into a vertical split — entry is
    geometric: you arrive at the child that lines up with the pane you left.
    This is what makes Right-then-Left return you exactly where you started."
  (let ((n (c:container-count split)))
    (cond ((zerop n) nil)
          ((and direction (eq (c:direction-axis direction) (c:split-axis split)))
           (if (plusp (c:direction-sign direction)) 0 (1- n)))
          ((and direction reference rects)
           (or (best-aligned-address split reference rects (c:split-axis split))
               0))
          (t 0))))

(defmethod entry-address ((policy policy) (stack c:stack)
                          direction reference rects)
  "Entering a stack always lands on the visible child.  Directional motion
does not reveal a hidden tab."
  (declare (ignore direction reference rects))
  (stack-visible-address policy stack))

(defmethod motion-escapes-p ((policy policy) container direction)
  (declare (ignore container direction))
  t)

(defmethod focus-after-remove ((policy policy) world removed-path
                               suggested)
  (declare (ignore removed-path))
  (ecase *focus-after-close*
    (:stay suggested)
    (:next (or (c:next-leaf-path (c:world-root world) suggested) suggested))
    (:mru (or (mru-path world) suggested))))

(defun mru-path (world)
  "The path of the most recently focused surviving window, or NIL.

The recency list lives on the world's PROPS rather than in a slot, because it
is exactly the kind of state an *option* wants and the core does not."
  (loop for window in (c:prop world :focus-history)
        for leaf = (and (c:window-live-p window)
                        (c:leaf-holding (c:world-root world) window))
        when leaf return (c:node-path-to (c:world-root world) leaf)))

(defmethod on-focus-change ((policy policy) world old new)
  "Record the window we are leaving, so that :MRU has something to consult."
  (declare (ignore old))
  (let ((window (c:world-window-at world new)))
    (when window
      (setf (c:prop world :focus-history)
            (cons window (remove window (c:prop world :focus-history)
                                 :count 1)))))
  nil)

(defmethod pointer-focus ((policy policy) world x y)
  "The deepest visible leaf whose rectangle contains the pointer."
  (let ((best nil))
    (dolist (placement (c:prop world :last-placements) best)
      (destructuring-bind (node path rect visible) placement
        (when (and visible (typep node 'c:leaf) (c:rect-contains-p rect x y))
          (setf best path))))))

;;; ==================================================================
;;; STRUCTURE
;;; ==================================================================

(defmethod split-axis-for ((policy policy) node rect)
  "Cut along the longer side, so panes tend towards square."
  (declare (ignore node))
  (ecase *split-axis*
    (:longer (if (>= (c:rect-w rect) (c:rect-h rect)) :horizontal :vertical))
    (:horizontal :horizontal)
    (:vertical :vertical)))

(defmethod new-child-side ((policy policy) node direction)
  (declare (ignore node))
  (cond ((null direction) *new-child-side*)
        ((member direction '(:right :down)) :after)
        (t :before)))

(defmethod should-collapse-p ((policy policy) (split c:split))
  *collapse-degenerate-splits*)

(defmethod should-collapse-p ((policy policy) (stack c:stack))
  "A one-workspace workspace list is a thing you are about to add to, not
debris."
  nil)

(defmethod should-collapse-p ((policy policy) container)
  (declare (ignore container))
  t)

(defmethod move-into-occupied ((policy policy) world from to)
  (declare (ignore world from to))
  *move-into-occupied*)

(defmethod insertion-weight ((policy policy) (split c:split) address)
  (declare (ignore address))
  (let ((ws (c:weights split)))
    (if ws (/ (reduce #'+ ws) (length ws)) 1)))

(defmethod spawn-target ((policy policy) world window)
  "Split the focused pane, unless it is empty, in which case fill it.

Filling an empty pane rather than splitting it is not a special case bolted
on: an empty pane exists *because the user made a place for something*, so
putting the next thing there is the only reading that respects the gesture."
  (declare (ignore window))
  (let* ((path (c:world-cursor world))
         (leaf (c:world-leaf-at world path)))
    (cond
      ((and leaf (c:leaf-empty-p leaf)) (values path :fill))
      ((eq *spawn-mode* :stack) (values path :stack))
      ((eq *spawn-mode* :fill-first)
       ;; LEAF-PATHS of the workspace are relative to the workspace, so they
       ;; must be resolved against it and only then rebased onto the world.
       ;; Resolving a workspace-relative path against the world root is the
       ;; kind of mistake that silently does nothing, which is worse than
       ;; crashing.
       (let* ((workspace (c:current-workspace world))
              (empty (find-if (lambda (candidate)
                                (let ((node (c:resolve-path workspace candidate)))
                                  (and (typep node 'c:leaf)
                                       (c:leaf-empty-p node))))
                              (c:leaf-paths workspace))))
         (if empty
             (values (append (c:workspace-path world) empty) :fill)
             (values path :split))))
      (t (values path :split)))))

;;; ==================================================================
;;; WINDOW LIFECYCLE
;;; ==================================================================

(defmethod should-float-p ((policy policy) (window c:window))
  "Anything with a parent floats; everything else tiles.

River's spec says a window with a parent 'might be a dialog, file picker, or
similar', and that one signal covers the overwhelming majority of windows that
should not be tiled — without an application blacklist that goes stale.  A
fixed-size hint is the second signal, and it catches the rest."
  (or (and *float-dialogs* (c:window-parent-window window) t)
      (multiple-value-bind (w h) (c:window-preferred-size window)
        (and w h t))))

(defmethod default-float-rect ((policy policy) (window c:window)
                               (output c:output))
  "Honour the window's own preferred size when it pinned one, centred;
otherwise take a fraction of the output."
  (let ((area (c:output-rect output)))
    (multiple-value-bind (want-w want-h) (c:window-preferred-size window)
      (let ((w (or want-w (round (* (c:rect-w area) *float-fraction*))))
            (h (or want-h (round (* (c:rect-h area) *float-fraction*)))))
        (c:make-rect (+ (c:rect-x area) (floor (- (c:rect-w area) w) 2))
                     (+ (c:rect-y area) (floor (- (c:rect-h area) h) 2))
                     w h)))))

(defmethod window-capabilities ((policy policy) (window c:window))
  "Declare all four.  We honour all four, and fullscreen is free.

Returns a list of keywords drawn from :WINDOW-MENU, :MAXIMIZE, :FULLSCREEN and
:MINIMIZE — river's bitfield, in the keyword-list form the bindings use.
River draws client-side-decoration titlebar buttons from this, so declaring a
capability you do not honour produces a button that does nothing."
  (declare (ignore window))
  (list :window-menu :maximize :fullscreen :minimize))

(defmethod decoration-mode ((policy policy) (window c:window))
  "Server-side, which under river means our borders and no client titlebar."
  (declare (ignore window))
  :ssd)

(defmethod window-rule-for ((policy policy) (window c:window))
  (declare (ignore window))
  nil)

(defmethod container-label ((policy policy) container)
  (c:node-label container))

(defmethod on-key ((policy policy) world key)
  (declare (ignore world key))
  nil)
