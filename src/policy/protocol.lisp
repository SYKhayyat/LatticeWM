;;;; policy/protocol.lisp --- THE EXTENSION SURFACE.
;;;;
;;;; This file contains generic functions and their docstrings.  It contains no
;;;; methods, and that rule is enforced by a build gate.  The shipped behaviour
;;;; is in conventional.lisp, as methods on CONVENTIONAL-POLICY.
;;;;
;;;; WHY THE FILE IS SHAPED THIS WAY
;;;;
;;;; DESIGN.org's §extensibility makes an admission that governs everything
;;;; here: "CLOS means there is no hook set to guess wrong" is only true if the
;;;; code is written as fine-grained generics.  A two-hundred-line RELAYOUT
;;;; that inlines every decision has nothing to specialize; the only available
;;;; move is to redefine all two hundred lines, which requires understanding
;;;; all two hundred lines.  *Extensibility is a property of the decomposition,
;;;; not of the language.*  Lisp raises the ceiling and does nothing for the
;;;; floor.
;;;;
;;;; So this file is the deliverable, not the documentation of it.  Reading it
;;;; end to end should tell you everything the window manager can be talked
;;;; out of.
;;;;
;;;; HOW TO USE IT
;;;;
;;;; Every generic below is specialized on a POLICY as its first argument.  To
;;;; change one behaviour, subclass the policy you are using and add a method:
;;;;
;;;;     (defclass my-policy (conventional-policy) ())
;;;;     (setf *policy* (make-instance 'my-policy))
;;;;
;;;;     (defmethod should-float-p ((p my-policy) win)
;;;;       (or (call-next-method)
;;;;           (member (window-app-id win) '("pavucontrol") :test #'equal)))
;;;;
;;;; or, if you do not want a second policy class, specialize on the shipped
;;;; one directly — it is your window manager:
;;;;
;;;;     (defmethod gaps ((p conventional-policy) container) 8)
;;;;
;;;; Both take effect the moment you evaluate them.  No restart, no rebuild.
;;;;
;;;; ON SIZE
;;;;
;;;; PLAN.org sets the target: "If this list reaches thirty, the decomposition
;;;; has gone wrong in the direction of ceremony.  If it drops below ten, it
;;;; has gone wrong in the direction of a monolith."  The count is reported by
;;;; the build.  Adding a generic here is cheap; adding one that nobody would
;;;; ever specialize is not free, because it costs a reader's attention.

(in-package #:latticewm/policy)

;;; ------------------------------------------------------ policy objects

(defgeneric policy-name (policy)
  (:documentation "A short human-readable name for POLICY, shown in status output."))

;;; ==================================================================
;;; THE SIX PROTOCOLS
;;; ==================================================================
;;;
;;; POLICY USED TO BE ONE INTERFACE WITH FIFTY-THREE GENERICS ON IT, and that
;;; was the clearest interface-segregation failure in the program.  A user who
;;; wanted only a custom *layout* still had to be a POLICY and nominally
;;; implement everything — motion, appearance, spawning, key handling — and
;;; every generic added to POLICY widened the surface every existing extension
;;; was answering for.
;;;
;;; The container protocol shows this project already knew how to do it: nine
;;; focused generics, implementable independently, which is exactly why GRID
;;; works.  This is the same treatment applied to the other side.
;;;
;;; Nothing breaks and nothing moves.  POLICY inherits all six, so a method on
;;; POLICY or on CONVENTIONAL-POLICY still specialises correctly and still
;;; reaches the shipped default with CALL-NEXT-METHOD.  What changes is that
;;; the *defaults* now live on the narrow class, so a mixin implementing one
;;; protocol is a real thing you can write:
;;;
;;;     (defclass tall (layout-policy) ())
;;;     (defmethod layout-children ((p tall) (c split) rect) ...)
;;;     (defclass mine (tall conventional-policy) ())
;;;     (setf *policy* (make-instance 'mine))
;;;
;;; `tall' answers for layout and for nothing else, which is both what it does
;;; and now what it says.

(defclass layout-policy ()
  ()
  (:documentation
   "How a rectangle is divided, and what the result looks like on screen.

LAYOUT-CHILDREN, LAYOUT-NODE, WINDOW-DIMENSIONS, GRAVITY, GAPS, VISIBLE-P,
RENDER-ORDER, CLIP-RECT, OUTPUT-CONTENT, RESERVED-SPACE, OUTER-RECT.

The one protocol most people who write an extension are actually after.  Every
layout model this window manager will ever have is a set of methods on these."))

(defclass appearance-policy ()
  ()
  (:documentation
   "What the window manager draws for itself: colours, fonts, and text.

BORDER-WIDTH, BORDER-COLOR, ECHO-CONTENT, KEYS-HINT, FONT-FOR, and the five
generics that turn the live keymap and command registry into rows of text.

Separate from layout because they change for entirely different reasons: a
theme is not a tiling model, and somebody writing one should not have to
implement the other."))

(defclass motion-policy ()
  ()
  (:documentation
   "Where the cursor goes.

STEP-ADDRESS, ENTRY-ADDRESS, MOTION-ESCAPES-P, FOCUS-AFTER-REMOVE,
ON-FOCUS-CHANGE, POINTER-FOCUS.

The smallest and most self-contained of the six, and the one whose defaults are
most often exactly right — which is why it is worth being able to inherit them
without inheriting anything else."))

(defclass structure-policy ()
  ()
  (:documentation
   "Where things go when the tree changes: spawning, splitting, joining, sizing.

SPAWN-TARGET, SPLIT-AXIS-FOR, NEW-CHILD-SIDE, SHOULD-COLLAPSE-P,
MOVE-INTO-OCCUPIED, INSERTION-WEIGHT, CONTAINER-AXIS, EQUALIZE-CONTAINER,
RESIZE-CONTAINER, TAB-SIBLINGS, JOIN-EXISTING-SPLIT-P, STACK-VISIBLE-ADDRESS,
CONTAINER-LABEL."))

(defclass lifecycle-policy ()
  ()
  (:documentation
   "What happens when a window arrives, leaves, minimizes or floats.

ON-WINDOW-OPEN, ON-WINDOW-CLOSE, SHOULD-FLOAT-P, DEFAULT-FLOAT-RECT,
ON-MINIMIZE, ON-RESTORE, WINDOW-CAPABILITIES, DECORATION-MODE,
WINDOW-RULE-FOR."))

(defclass input-policy ()
  ()
  (:documentation
   "What a key, a click or a drag means.

ON-KEY, KEY-UNBOUND, POINTER-DRAG-RECT, POINTER-RESIZE-EDGES,
SHIFTED-CHARACTER, COMMAND-REPEATABLE-P, COMPLETE-CANDIDATES,
ARGUMENT-TYPE-FOR."))

(defclass policy (layout-policy appearance-policy motion-policy
                  structure-policy lifecycle-policy input-policy)
  ((%name :initarg :name :initform "policy" :accessor policy-name)
   (props :initform '() :accessor policy-props
          :documentation "Extension state, as on a node.  See CORE:PROPS."))
  (:documentation
   "The object every decision dispatches on.

A policy is a *layout model plus a set of behavioural choices*, and it is an
object rather than a pile of special variables so that two of them can exist
at once — which is what lets the lattice ship as a subclass rather than as a
patch.  It carries almost no state: the tree holds the state, the policy holds
the opinions.

It implements all six protocols above, which is what makes `a policy' one thing
you can hold rather than six you have to assemble — and what keeps every
existing method, worked example and extension specialising on POLICY exactly as
correct as it was."))

(defvar *policy* nil
  "The policy in force.  Bound to a CONVENTIONAL-POLICY at startup.

Rebinding it swaps the entire layout model live:

    (setf *policy* (make-instance 'lattice-policy))
    (relayout)")

(defun current-policy ()
  "The policy in force, which is *POLICY* — or a conventional one, made here.

The fallback is not laziness about startup order.  Everything that asks a
policy a question now includes things that run with no compositor attached at
all: `latticewm --list-commands', a unit test, a REPL in an editor at three in
the morning.  Answering NIL there produces a no-applicable-method error whose
text mentions neither the policy nor the fact that none was ever made, and the
right answer is available and obvious."
  (or *policy* (setf *policy* (make-instance 'conventional-policy))))

;;; Tier-0 options — the registry, DEFINE-OPTION and friends — live in
;;; policy/options.lisp, which loads before policy/log.lisp because logging is
;;; itself configurable and logging must exist before anything can signal.

;;; ==================================================================
;;; LAYOUT — how a rectangle is divided
;;; ==================================================================

(defgeneric layout-children (policy container rect)
  (:documentation
   "Divide RECT among CONTAINER's children.

Returns a list of (ADDRESS . RECT), one entry per child that should be
*rendered*.  Children omitted from the list are not drawn — that is how a
STACK shows one tab, and how the lattice hides cells outside the viewport.

This is the single most important generic in the file.  Every layout model
this window manager will ever have is a method on it:

  * the shipped SPLIT method divides by weight,
  * the shipped STACK method gives the whole rectangle to one child,
  * the lattice's GRID method places cells on a coordinate plane and crops to
    a viewport.

It is pure.  It must not touch the compositor, must not mutate CONTAINER, and
must be safe to call repeatedly — the emitter calls it on every relayout and
diffs the result."))

(defgeneric layout-node (policy node rect)
  (:documentation
   "Recursively lay NODE out inside RECT.

Returns a list of (NODE PATH RECT VISIBLE-P) for every node beneath and
including NODE, deepest last, which is everything the placement emitter needs.
Specialize this only to change the *recursion* — to add a decoration band
above every container, say, or to stop descending past a certain depth for a
drawn overview map.  To change how one container divides its space, specialize
LAYOUT-CHILDREN instead; it is far cheaper."))

(defgeneric window-dimensions (policy leaf rect)
  (:documentation
   "The size to propose for LEAF's window, given the RECT it has been assigned.

Returns (values WIDTH HEIGHT).  River's propose_dimensions is advisory — the
spec calls out terminal emulators that quantise to their cell size — so the
real size arrives later in a dimensions event and may not be what was asked
for.  GRAVITY decides where the shortfall goes."))

(defgeneric gravity (policy leaf rect width height)
  (:documentation
   "Where to place a WIDTH by HEIGHT window inside the larger RECT it was given.

Returns a RECT.  This is the involuntary case — a client that refused the
proposed size.  The deliberate case, where a user wants a window to occupy
less than its pane, is handled by splitting against an empty pane instead
(DESIGN D17), which is why this only needs one sane answer rather than a
policy language."))

(defgeneric gaps (policy container)
  (:documentation
   "Pixels of empty space to leave between CONTAINER's adjacent children.

Specialize on the container to vary it: zero inside a stack, larger between
lattice cells than between splits within a cell."))

(defgeneric border-width (policy node focusedp)
  (:documentation
   "Border thickness in pixels for NODE, drawn by the compositor.

FOCUSEDP takes the same three values BORDER-COLOR's does."))

(defgeneric border-color (policy node focusedp)
  (:documentation
   "The border colour for NODE, as (values R G B A), each 0.0 to 1.0.

*COLOURS ON THIS SURFACE ARE STRAIGHT ALPHA.*  You write the colour you mean —
half-transparent blue is (0.4 0.65 1.0 0.5) — and the wire layer premultiplies
at the boundary, because river's set_borders takes premultiplied RGBA and
nobody wants to write colours that way.  The same rule holds for every colour
option in the system, so one value can be used for a border and for the echo
area and produce the same pixel in both.  See W:COLOR-COMPONENT, which is the
one place the multiplication happens.

FOCUSEDP is three-valued: T when NODE has the keyboard, :CURSOR when it holds
the cursor while a floating window has the keyboard, and NIL otherwise.
:CURSOR is truthy, so a method written as though this were a boolean is still
correct — it simply cannot tell the middle state apart.

Note this is also the only decoration an *empty focused pane* can have, and an
empty pane that does not obviously have the cursor reads as a broken keyboard
rather than as a place — DESIGN D18 lists that as an accepted cost with the
cursor being unmissable as the mitigation.  Do not make the focused colour
subtle."))

(defgeneric visible-p (policy node)
  (:documentation
   "Should NODE be shown at all?

Nodes that answer NIL are sent river's hide request and cost nothing further.
Newly created windows are shown unless explicitly hidden, so anything offscreen
must be hidden deliberately."))

(defgeneric render-order (policy placements)
  (:documentation
   "Order PLACEMENTS from bottom to top for river's place_above chain.

River says the initial position of a node in the render list is *undefined*,
so every node must be ordered explicitly or overlapping windows flicker
between frames.  The default is tiled nodes in layout order, then floats, then
overlays."))

(defgeneric clip-rect (policy node rect)
  (:documentation
   "The region of NODE's content that should be visible, or NIL for all of it.

Returns a RECT in the same space as RECT.  This is river's
set_content_clip_box, and it is the best thing in the protocol: a cell
half-scrolled off the viewport edge is cropped *and its border is redrawn at
the crop edge*, so it reads as a cleanly cut cell rather than a window sliced
in half.  Free, from the compositor.

Returns NIL in the conventional layer, where nothing ever overhangs.  The
lattice is what makes it earn its keep."))

(defgeneric output-content (policy world output)
  (:documentation
   "Which part of the model does OUTPUT show?

Returns (values NODE PATH): the subtree to lay out on that output, and its
path from the root so that placements can be addressed globally.

PLAN.org's fiat rules multi-monitor as \"one model, one viewport per output\",
and this generic is that ruling.  The shipped answer gives each output its own
*workspace* — output 0 shows whichever workspace it was last switched to,
output 1 its own — which is i3's and sway's behaviour and the one nobody has to
be taught.

Returning the same node for two outputs is a mistake with a specific and
confusing symptom, so it is worth naming: every window gets placed twice, the
second placement wins, and the whole layout silently collapses onto the last
output while the model insists everything is fine.  That was the first
implementation."))

(defgeneric echo-content (policy world)
  (:documentation
   "What the echo area says, as a list of (TEXT . KIND).

KIND is :ACCENT for the part that says where you are and :NORMAL for the rest.
Segments are drawn left to right with separators between them; empty ones are
dropped, so returning \"\" is how a segment says nothing this time.

This is a policy decision and not a status-bar feature: what a window manager
should tell you about depends entirely on what its layout model *is*.  The
shipped answer names the workspace, the place, what is in it, and the counts.
The lattice's answer would sensibly include the viewport; a policy with no
workspaces should not mention them."))

(defgeneric reserved-space (policy output)
  (:documentation
   "Pixels to keep clear on each edge of OUTPUT, as (TOP RIGHT BOTTOM LEFT).

This is how a status bar, a dock or the echo area takes its strip without
every layout having to know it exists.  The shipped answer reserves the echo
area's height at the bottom and nothing else.

Return all zeroes to let windows use the whole output — the echo area is drawn
above them, so it still works, it just overlaps."))

(defgeneric outer-rect (policy output)
  (:documentation
   "The rectangle the layout tree may use on OUTPUT.

Subtract space here for a status bar or a reserved strip; everything
downstream honours it without knowing why."))

;;; ==================================================================
;;; MOTION AND FOCUS
;;; ==================================================================

(defgeneric step-address (policy container address direction)
  (:documentation
   "Moving DIRECTION from ADDRESS inside CONTAINER, which address do we reach?

Returns an address, or NIL meaning 'motion cannot be satisfied here' — at
which point the caller ascends to CONTAINER's own parent and asks it the same
question.  That one rule is what makes motion continuous across every kind of
boundary: move left from the leftmost pane of a split and you leave the split;
if the split sits in a lattice cell you arrive in the cell to its left, and
you enter *through its right edge*, landing on its rightmost pane.  No mode,
no second command, no discontinuity in the user's model.

A STACK returns NIL for every spatial direction, deliberately: you do not
arrive in another workspace by pressing Left."))

(defgeneric entry-address (policy container direction reference rects)
  (:documentation
   "Arriving at CONTAINER while travelling DIRECTION, which child do we land on?

DIRECTION is NIL for a non-directional arrival — a jump to a name, a typed
coordinate, a click.  REFERENCE is the rectangle being left, or NIL, and RECTS
maps nodes to their rectangles, or NIL; together they let a container answer
*geometrically* — with the child that lines up with where you already are —
rather than with a fixed index.  A method that does not care may ignore both.

The shipped rule is DESIGN D20's: directional motion enters through the edge
it crossed and lands on the child adjacent to that edge, and non-directional
jumps land on the first child.  There is deliberately *no memory* of what was
focused there before, and the reason is not laziness.  Suppose a cell holds a
terminal left and an editor right and the editor was last focused.  With
memory, pressing Right from the cell to its left drops you on the editor;
press Right again and you leave.  *The terminal cannot be reached by rightward
motion at all.*  Geometric entry is also involutive: Right then Left returns
you exactly where you started.

Along the container's own axis, entry is by *edge* and geometry does not enter
into it: that is what guarantees every pane is reachable.  Across the axis,
entry is geometric, so that Right-then-Left returns you exactly where you
started.  Getting the second one wrong is the difference between a layout that
feels like a place and one that feels like a menu.

Memory is genuinely nicer for named jumps, and adding it is a method on this
generic plus a value on the container's PROPS — which is what PROPS is for."))

(defgeneric motion-escapes-p (policy container direction)
  (:documentation
   "May motion leave CONTAINER travelling DIRECTION, or is it a wall?

Answering NIL makes a container trap the cursor, which is how a fullscreen
group or a modal region is built.  The default is T everywhere: the tree is
one continuous space."))

(defgeneric focus-after-remove (policy world removed-path suggested)
  (:documentation
   "Where should the cursor go after the node at REMOVED-PATH was taken out?

SUGGESTED is what CORE:REPAIR-PATH proposed — the deepest surviving ancestor's
first leaf — and returning it is the shipped behaviour.  It implements DESIGN
D18's governing property: *nothing ever moves the viewport except the user*.
Under a most-recently-used rule the next window can be anywhere on the plane,
so closing something can teleport you across your desktop.

Returning something else is a one-method change, and MRU-after-close is one of
the shipped worked examples precisely because it is the most commonly wanted
one."))

(defgeneric focus-target (policy world)
  (:documentation
   "Which window should hold the *keyboard*, or NIL for none.

DESIGN D18, AND IT IS THE ONE IDEA THE README ASKS YOU TO READ FIRST: focus is
a *place*, and Wayland keyboard focus is derived from where the cursor rests
rather than being the primary fact.  When the place holds a window, that window
gets the keyboard; when it is a deliberately empty pane, NIL is the honest
answer and the compositor is told to clear focus — because leaving the last
window focused while the highlight is somewhere else means your keystrokes go
somewhere other than where you are looking.

THIS WAS A COND IN AN EVENT HANDLER UNTIL IT WAS A GENERIC.  The single idea
that distinguishes this window manager from i3 was the one decision no method
could reach: runtime/windows.lisp derived it inline, so a policy could change
where the *cursor* went, what it *looked* like, and what happened *after* it
moved — and could not change what focus meant.  The thesis of the project is
that the decisions live above the line; this one is the thesis's own example,
and it was below it.

The shipped rule is C:WORLD-FOCUS-WINDOW: a focused float wins, because a float
is on top and is what the user is looking at; otherwise the cursor's window,
which may be NIL.  Two obvious other rules, both now one method:

  click-to-focus     ignore the cursor, answer the last window clicked
  sloppy-focus       answer the cursor's window but never NIL, so an empty
                     pane leaves the keyboard where it was

Returns a C:WINDOW or NIL.  It is asked on every manage sequence, so it must be
cheap and must not signal — errors are caught and logged, and the fallback is
to leave focus alone rather than to clear it.

Note what this does *not* decide: a layer surface holding exclusive keyboard
focus — a screen locker — is a protocol fact rather than a policy one, and the
runtime honours it whatever this answers.  A policy that could focus a window
through a lock screen would be a policy that could unlock the screen."))

(defgeneric on-focus-change (policy world old-path new-path)
  (:documentation
   "Called after the cursor moves from OLD-PATH to NEW-PATH.

For side effects: raising a float, warping the pointer, updating a bar.
Return value ignored.  Errors here are caught and logged rather than allowed
to abort the motion."))

(defgeneric pointer-focus (policy world x y)
  (:documentation
   "Which path, if any, does the pointer at (X, Y) name?

Returns a path or NIL.  Used by focus-follows-mouse over *empty* panes, which
river cannot report because an empty pane holds no window for the pointer to
enter.  Where there is a window, river's own pointer_enter is authoritative and
is used instead — it knows about borders and decoration input regions, which a
hit test against layout rectangles does not."))

(defgeneric pointer-drag-rect (policy world kind start dx dy edges)
  (:documentation
   "Where a dragged window goes, given the rectangle it started from.

KIND is :MOVE or :RESIZE.  START is the window's rectangle when the drag began,
DX and DY are the pointer's *cumulative* motion since then, and EDGES is the
set of edges being dragged for a resize — a list drawn from :TOP, :BOTTOM,
:LEFT and :RIGHT.  Returns a RECT.

WORLD is handed in because snapping needs to know where the output edges are,
and a policy method may not go looking for the runtime's globals to find out.

*Cumulative, not incremental*, and every method has to be written that way.
River sends the total motion since the operation started, so the correct
computation is always START plus the total — never `the current rectangle plus
the last delta', which accumulates rounding and drifts badly whenever the
pointer leaves the screen and comes back.

Specialize this for snapping, for edge resistance, for a grid, or to make a
resize keep its aspect ratio."))

(defgeneric pointer-resize-edges (policy window x y)
  (:documentation
   "Which edges of WINDOW a resize started at (X, Y) should move.

Returns a list drawn from :TOP, :BOTTOM, :LEFT and :RIGHT, and never both of an
opposing pair.  The shipped rule is the quadrant the pointer is in, which is
what makes a Super+right-drag near the top-left corner move that corner rather
than the bottom-right one — the thing every floating window manager does and
nobody has ever had to be taught."))

;;; ==================================================================
;;; STRUCTURE — where things go
;;; ==================================================================

(defgeneric make-workspace (policy world index)
  (:documentation
   "The node a workspace that has just come into existence contains.

INDEX is the position it will occupy, counting from zero.  WORLD is the world
it is being added to, so a method may look at what is already there — the
workspace beside it, its viewport, the outputs.

Called from every one of the four places the workspace list grows: switching to
a workspace that is not there yet, NEW-WORKSPACE, sending a window past the end
of the list, and a monitor appearing with no workspace of its own.  There is no
fifth, and a new one must call this rather than build a node.

*This generic is what makes \"infinite workspaces of lattices one behind
another\" true rather than nearly true.*  The list has always grown on demand —
a stack grows, so infinity costs nothing — but each of those four sites built
the same empty pane by hand.  So a lattice installed at startup covered the
workspaces that existed at startup and nothing after it: workspace 7 was a
plain pane on a machine whose first six were planes, silently, and only
somebody who went there could find out.  The plane was not the default shape of
a workspace; it was a wrapper applied once.

The shipped answer is one empty pane, which is DESIGN D19's starting state: a
place with nothing in it, where typing a key spawns something.  *NEW-WORKSPACE*
changes that without a method.  The lattice answers with a fresh plane, and
that single method is the whole of the Z axis."))

(defgeneric spawn-target (policy world window)
  (:documentation
   "Where should a newly appeared WINDOW be placed?

Returns (values PATH DISPOSITION), where DISPOSITION is

  :SPLIT   split the node at PATH, putting the new window beside it,
  :FILL    the node at PATH is an empty pane; fill it,
  :STACK   stack the new window onto the node at PATH as a tab,
  :FLOAT   do not tile it at all.

DESIGN D14 rules that all three placement modes ship and configuration picks
the default; this generic plus the *SPAWN-MODE* option is that ruling.  The
shipped default is 'split the focused pane', which is Emacs's and hyprland's
behaviour and the one nobody has to be taught."))

(defgeneric split-axis-for (policy node rect)
  (:documentation
   "Which way should a fresh split at NODE be cut?

The shipped rule is dynamic: cut along whichever axis is longer, so panes tend
towards square instead of towards slivers.  A fixed :HORIZONTAL is one method
away and is what people who want dwindle-style predictability ask for."))

(defgeneric new-child-side (policy node direction)
  (:documentation
   ":BEFORE or :AFTER — which side of NODE does a new sibling land on?

DIRECTION is the direction the user asked for, or NIL for a plain spawn."))

(defgeneric should-collapse-p (policy container)
  (:documentation
   "Should CONTAINER be dissolved now that it has one child left?

The default is yes for splits and no for stacks: a one-pane split is
pointless, whereas a one-workspace workspace list is a thing you are about to
add to.  Answering NIL for splits gives i3's behaviour, where containers
persist and can be filled again."))

(defgeneric move-into-occupied (policy world from to)
  (:documentation
   "What does moving the subtree at FROM onto an occupied TO mean?

Returns :SPLIT, :SWAP or :STACK.  The shipped answer is :SPLIT — DESIGN's open
question notes that if move-onto-occupied swapped, the separate swap verb
would be half redundant, and splitting is the only choice that never destroys
structure.  Both other verbs remain separately bound."))

(defgeneric insertion-weight (policy split address)
  (:documentation
   "The weight a child newly inserted at ADDRESS of SPLIT should receive.

The default is the mean of the existing weights, so inserting into an even
split keeps it even and inserting into a lopsided one does not hand the
newcomer a surprising share."))

(defgeneric container-axis (policy container)
  (:documentation
   "The axis CONTAINER divides space along: :HORIZONTAL, :VERTICAL, or NIL.

NIL is the honest answer for anything that does not divide space — a stack
gives every child the whole rectangle, a leaf has no children — and it is also
the answer for NIL itself, so a caller walking up past the root need not guard.

This generic exists because four verbs used to ask (TYPEP CONTAINER 'SPLIT)
directly, and a TYPEP is not an extension point.  A container kind from
outside the core that divides space in exactly the way a split does was
invisible to RESIZE, EQUALIZE, EQUALIZE-ALL and TAB, and no method anywhere
could say otherwise.  Answer this and all four work."))

(defgeneric resize-container (policy container address amount)
  (:documentation
   "Grow CONTAINER's child at ADDRESS by AMOUNT of the container's total.

AMOUNT is a signed fraction: 1/20 means one twentieth wider, -1/20 one
twentieth narrower, and the space comes from — or goes to — the adjacent child,
so no other divider moves.  True when something changed.

The unit is *the container's total*, not a pixel count and not a weight, and
that is the whole reason this is a generic rather than a call to ADJUST-WEIGHT.
Weights are a SPLIT's representation; a container kind that divides space some
other way — a strip with fixed column widths, a grid with track sizes — has no
weights to adjust and was silently unresizable.  The shipped method translates
the fraction into a weight transfer; a kind from outside translates it into
whatever it keeps."))

(defgeneric equalize-container (policy container)
  (:documentation
   "Give every child of CONTAINER an equal share of it.  True if anything moved.

The shipped answer evens out a split's weights and does nothing to anything
else, because a stack's children already have equal shares — every one of them
the whole rectangle — and there is nothing to even out."))

(defgeneric tab-siblings (policy container address)
  (:documentation
   "Which two children of CONTAINER should TAB fold into one stack?

Returns (VALUES KEEP REMOVE): the address the new stack takes, and the address
it empties.  NIL means there is no pair to fold here, and TAB does nothing.

The shipped rule is 'this child and the next one, or the previous one when
this is the last', which is what makes TAB do the obvious thing at either end
of a row rather than refusing at one of them."))

(defgeneric join-existing-split-p (policy container axis)
  (:documentation
   "Should a fresh split along AXIS join CONTAINER rather than nest inside it?

This is what stops three windows placed side by side from becoming a lopsided
nest of two-child splits — press the same key three times and you get one
split of four, the way Emacs and i3 behave.

The shipped rule is yes exactly when CONTAINER already divides along AXIS.
Answering NIL always gives hyprland's dwindle, where every split nests inside
the last; DESIGN's note that binary remains reachable as policy is this
method plus SPLIT-AXIS-FOR."))


(defgeneric binding-description (policy target)
  (:documentation
   "A short description of what a key does, for a help or which-key screen.

TARGET is what the key is bound to: a command form, a keymap for a chord, or
NIL.  The shipped answer prefers the command's own docstring over its name,
because the name is usually the least informative thing available, and
substitutes the binding's actual arguments into it -- so a key bound to
\"focus :left\" reads \"Move the cursor one pane left\" rather than \"...one
pane DIRECTION\"."))

(defgeneric help-entries (policy keymap)
  (:documentation
   "Every binding in KEYMAP as (KEYS . DESCRIPTION), sorted for reading.

The shipped answer merges bindings that do the same thing onto one row -- the
keymap binds both the vi letters and the arrows deliberately, and listing each
twice would double the screen while saying nothing -- and sorts by *what the
key does* rather than by the key, so the four directions of one verb end up
together and the screen reads as a set of verbs rather than as an alphabet.

Both of those are taste.  Somebody who wants a plain alphabetical reference
writes one method."))

(defgeneric keys-running (policy name)
  (:documentation
   "Every key bound to the command called NAME, as a printable string, or NIL.

Emacs's `where-is', folded into describe-command because \"what does this do\"
and \"how do I do it without typing its name\" are asked at the same moment."))

(defgeneric command-help-rows (policy command)
  (:documentation
   "COMMAND's documentation, arguments and keys, as rows for the help overlay.

This is what Super+? c draws.  The shipped answer is the signature, the whole
docstring wrapped, each argument with its type, and where it is bound."))

(defgeneric welcome-rows (policy)
  (:documentation
   "The handful of keys a new user is shown on the very first start.

Deliberately about a dozen entries, ending with how to quit.  The full keymap
is one key away and is a *reference*; this is the smaller thing a reference
cannot be.

Both halves are derived rather than written down -- the key names from
*MODIFIER*, the descriptions from each command's own docstring -- so a
rebinding moves the welcome screen with it."))

;;; ==================================================================
;;; WINDOW LIFECYCLE
;;; ==================================================================

(defgeneric on-window-open (policy world window)
  (:documentation
   "A window has appeared.  Place it, and decide whether it takes focus.

Returns the path it was placed at, or NIL if the policy declined to tile it
(having floated it, or parked it).  This is the top of the placement path;
SPAWN-TARGET is the part you usually want to specialize instead."))

(defgeneric on-window-close (policy world window path)
  (:documentation
   "WINDOW has gone away and was at PATH.  Repair the tree.

The shipped behaviour is DESIGN D17's CLOSE: the pane goes with the window and
its sibling grows to fill the space.  The other half of D17 — CLEAR, which
empties the pane but leaves it standing — is a separate command rather than a
mode, and configuration decides which one holds the primary binding."))

(defgeneric should-float-p (policy window)
  (:documentation
   "Should WINDOW float rather than tile?

The shipped rule is: anything with a parent floats — river reports that via
river_window_v1.parent and the spec says it 'might be a dialog, file picker,
or similar' — and everything else tiles.  Adding app-id rules is the canonical
first extension anybody writes, which is why it is a shipped worked example."))

(defgeneric default-float-rect (policy window output)
  (:documentation
   "The rectangle a newly floated WINDOW should occupy on OUTPUT.

The default honours the window's own preferred size when it has one, centred,
and otherwise takes a fraction of the output."))

(defgeneric on-minimize (policy world window)
  (:documentation
   "WINDOW asked to be minimized, or the user asked for it.

River is explicit that this is entirely ours to define — the window manager is
'free to ignore this request, hide the window, or do whatever else it
chooses'.  The shipped behaviour meets the stated requirement: *minimized
windows leave the tiling tree entirely* and the remaining windows retile
without them.  It is not 'hide it somewhere', it is 'take it out of the
layout'.  They go to a flat scratchpad list, remembering the path they came
from."))

(defgeneric on-restore (policy world window)
  (:documentation
   "Bring WINDOW back from the scratchpad.

The shipped rule returns it to the slot it was minimized from if that slot
still exists, and to the cursor's pane otherwise."))

(defgeneric window-capabilities (policy window)
  (:documentation
   "The capability bitfield to declare for WINDOW.

River draws client-side-decoration titlebar buttons based on what we declare,
so this is directly user-visible.  The shipped answer declares all four —
window_menu, maximize, fullscreen, minimize — because we honour all four and
fullscreen in particular is free."))

(defgeneric decoration-mode (policy window)
  (:documentation
   ":CSD or :SSD — who draws WINDOW's decorations?

The default asks for server-side, which under river means our borders and no
client titlebar, and falls back to whatever the client insists on."))

(defgeneric window-rule-for (policy window)
  (:documentation
   "A property list of overrides for WINDOW, or NIL.

Consulted once when the window appears.  Recognised keys: :FLOAT, :WORKSPACE,
:PATH, :FOCUS, :FULLSCREEN, :BORDER-COLOR.  This is the declarative escape
hatch for people who do not want to write methods; the method is still there
underneath for people who do."))

;;; ==================================================================
;;; INPUT
;;; ==================================================================

(defgeneric on-key (policy world key)
  (:documentation
   "A bound key fired.  Returns non-NIL when the policy handled it itself.

The default returns NIL, letting the command bound to KEY run.  Specializing
this is how a modal layer — a resize mode, a vi-style submap — intercepts
everything without unbinding anything."))

(defgeneric capture-keys (policy)
  (:documentation
   "Every key the window manager may read *directly*, as (KEYSYM . MODIFIERS).

River delivers keys to the focused window and gives the window manager only
what it asked for, so this list is the whole of what can ever be read at a
prompt, in an empty pane, or as the second key of a chord.  Bound once at
startup and enabled only while something is reading — a key not on this list
is not merely unbound, it is *unreadable*, and no keymap entry can rescue it.

THIS WAS A DEFPARAMETER, AND THAT IS THE FINDING RATHER THAN THE FIX.  The set
of keys an Emacs-shaped window manager may ever read was fixed at compile time,
in src/runtime/, in a program whose entire premise is that this class of thing
is a decision.  A modal editing layer — the single most obvious thing this
program's users will ask for — could bind F1 through F12 in a keymap and find
that river never delivered one of them, with nothing anywhere to say why.
Nobody decided that; it is a DEFPARAMETER nobody revisited.

The shipped answer is printable ASCII bare and shifted, the keys that move and
delete, and the readline chords.  Extend rather than replace:

    (defmethod capture-keys ((policy my-policy))
      (append (call-next-method)
              (loop for keysym from #xffbe to #xffc9 collect (cons keysym '()))))

Answering with *more* is free — a binding that is never enabled costs one
object on each side.  Answering with *less* is how you make a prompt unable to
read a character, so subtract only what you are sure of.

Consulted on every manage sequence and diffed, so a method that answers
differently after the fact takes effect at the next one; keys are added
incrementally and never removed, which is the same rule REGISTER-BINDINGS uses
for the keymap and for the same reason: a river_xkb_binding_v1 the compositor
has already been told about is cheaper to leave disabled than to churn."))

(defgeneric shifted-character (policy character)
  (:documentation
   "What CHARACTER becomes with Shift held.

*THIS EXISTS BECAUSE RIVER SENDS THE UNSHIFTED KEYSYM.*  What arrives for
Shift+9 on a US keyboard is keysym `9' with Shift in the modifier set, not
`parenleft' — so the shifted glyph cannot be derived, it has to be declared.
Typing (+ 1 2) into M-: produced `9= 1 20' until this existed.

A GENERIC, and that matters more than it looks.  The shipped table is US
layout and *there is no way for it not to be*: river does the xkb work and does
not tell us the shifted keysym.  So on a German, French or Dvorak keyboard the
shipped answer is simply wrong — and before this was a generic, a user on one
of those had no supported way to fix it beyond replacing a global table by
hand.  Now there are three ways, in increasing order of effort: set
*KEYBOARD-LAYOUT* to one of the shipped tables, add a table with
REGISTER-SHIFT-MAP, or write this method.

Falls back to CHAR-UPCASE, which is right for every alphabet SBCL knows and
harmless for anything a table does not mention."))

(defgeneric command-repeatable-p (policy command arguments)
  (:documentation
   "Should REPEAT be able to run COMMAND with ARGUMENTS again?

The shipped answer consults *NOT-REPEATABLE*, a list of names — which is where
this behaviour used to live *entirely*, as a global an extension had to PUSH
onto rather than answer.  Two things were wrong with that: an extension adding
a non-repeatable command had to mutate somebody else's variable, and the list
form cannot express `not repeatable under these circumstances', which is the
interesting case.  A command that is repeatable when it acted and not when it
declined is one method here and is inexpressible in a denylist."))

(defgeneric key-unbound (policy world keysym)
  (:documentation
   "An unbound key was pressed while the cursor rests on an empty pane.

DESIGN D19: this is what gives the empty pane something to *be*.  The keypress
is run through a keysym-to-command table — `e` opens an editor there, `t` a
terminal, `b` a browser — so that an empty pane is a spawn menu with no menu.

Note the design constraint that forces a table rather than a single default:
the keystroke is consumed as a trigger and *cannot be replayed* into the
application that opens, because nothing in either river protocol synthesises
input.  Under a single-default policy, typing `ls` at an empty pane opens a
terminal showing `s`.  Under the table the keypress was a choice rather than
content, and nothing is lost."))

;;; ------------------------------------------------------- the hardware
;;;
;;; River hands the window manager the machine's input devices and expects it
;;; to configure them: there is nothing else on the system that will.  Applying
;;; a setting is mechanism and lives in runtime/input.lisp; *which* setting
;;; applies to *which* device is the decision, and it is one people have very
;;; specific opinions about — natural scrolling on the touchpad but not on the
;;; mouse is the canonical example and no number of global flags expresses it.

(defgeneric input-settings (policy device)
  (:documentation
   "Every setting to apply to DEVICE, as a plist.

Called when a device appears and again whenever the configuration is reloaded.
The result is diffed against what the device reports is already in force, so
returning the same plist twice sends nothing.

The recognised properties, each named after its tier-0 option:

  :TAP-TO-CLICK :TAP-AND-DRAG :DRAG-LOCK :NATURAL-SCROLL :LEFT-HANDED
  :MIDDLE-EMULATION :DISABLE-WHILE-TYPING :CLICK-METHOD :SCROLL-METHOD
  :SCROLL-BUTTON :ACCEL-PROFILE :ACCEL-SPEED     — libinput
  :SCROLL-FACTOR :REPEAT-RATE :REPEAT-DELAY      — river, any device

A property the device cannot support is skipped rather than sent; a property
absent from the plist, or present with the value NIL, is left alone rather than
reset.  That last rule is what makes the obvious extension do what it reads as
doing:

    (defmethod input-settings ((p conventional-policy) device)
      (append (when (search \"Wacom\" (or (input-device-name device) \"\"))
                '(:accel-profile :flat))
              (call-next-method)))

The shipped method reads the tier-0 options and then lays *INPUT-RULES* over
them, so most people never write one.  Write one when the decision is a
*program* rather than a table."))

(defgeneric keyboard-layout-for (policy device)
  (:documentation
   "The keyboard layout DEVICE should use, as (values LAYOUT VARIANT OPTIONS
MODEL RULES), or NIL for `leave whatever the compositor started with'.

Separate from INPUT-SETTINGS because a keymap is not a setting: it has to be
compiled, the compilation can fail with a message worth showing, and the result
is shared between every keyboard that asked for the same one.

Separate from the options because the second keyboard — the one in the dock,
with a different physical layout — is precisely the case a single global cannot
express, and precisely the case that matters to the person who has one:

    (defmethod keyboard-layout-for ((p conventional-policy) device)
      (if (search \"Dock\" (or (input-device-name device) \"\"))
          (values \"de\" \"nodeadkeys\" nil nil nil)
          (call-next-method)))"))

;;; ==================================================================
;;; READING FROM THE USER
;;; ==================================================================

(defgeneric complete-candidates (policy input candidates)
  (:documentation
   "Which of CANDIDATES does INPUT match, best first?

Emacs calls this a completion style and ships six of them, which is the correct
number to ship when the answer is a matter of taste and everybody's taste
differs.  The shipped default is the one nobody has to be taught: what you
typed as a prefix first, then as a substring, then as a subsequence — so
`wsp' finds `send-to-workspace' and `close' finds both `close' and
`close-float', with the exact prefix on top where the eye already is.

Pure string work, specialized on the policy for one reason: it is the single
most personal decision in the whole minibuffer, and somebody will want flex,
or initials-only, or plain strict prefixes.  Returns a fresh list."))

(defgeneric argument-type-for (policy command parameter)
  (:documentation
   "What kind of value does PARAMETER of COMMAND hold?  A keyword, or NIL.

This is what lets M-x prompt for a command's arguments instead of refusing to
run it.  The default is a naming convention rather than a declaration: a
parameter called DIRECTION holds a direction, one called NUMBER holds a number,
one called NAME holds a name.  That is worth stating plainly because it looks
like a hack and is not — the parameter names were already chosen to read well
in a docstring, so the convention costs nothing and covers almost every command
in the system without a single annotation.

Where the convention is wrong, DEFCOMMAND's (:interactive ...) clause overrides
it per command, and specializing this covers a whole naming scheme at once.
NIL means the parameter cannot be read from a human, which is what stops M-x
offering to prompt for a tree path."))

;;; ==================================================================
;;; STACKS, WORKSPACES, NAMES
;;; ==================================================================

(defgeneric stack-visible-address (policy stack)
  (:documentation
   "Which child of STACK is currently shown?

The default is its SELECTED slot.  Overriding this is how you get a stack that
shows two children side by side at wide sizes and one at narrow — a
responsive tab bar, in nine lines."))

(defgeneric container-role (policy world container)
  (:documentation
   "What CONTAINER *is* to the user right now: :WORKSPACES, :TABS, or NIL.

STACK IS ONE OBJECT WITH THREE NAMES, and collapsing them is defensible — it is
why 'move this window to workspace 3' and 'move this window to tab 3' needed no
second implementation.  What does not collapse is the *vocabulary*: the echo
area rendered [2/5] for a workspace list while the identical object was a tab
strip one level down, and a user who had not read the design document was
looking at the same three characters meaning two different things.

So the model stays collapsed and the words stop being.  The shipped rule is
positional and is the one nobody has to be taught: the alternatives container
at the *root* is the workspace list, and any other is a tab strip.  A policy
whose root is not a workspace list answers NIL and the vocabulary goes quiet
rather than lying.

Returns a keyword; CONTAINER-ROLE-NAME turns it into a word."))

(defgeneric container-label (policy container)
  (:documentation
   "A short human-readable name for CONTAINER, or NIL.

Used by the status output, by jump-to-name, and by the lattice's coordinate
overlay.  Defaults to the node's LABEL slot when one was set."))

;;; ==================================================================
;;; INTROSPECTION — the surface describes itself
;;; ==================================================================

(defparameter +policy-protocols+
  '(policy layout-policy appearance-policy motion-policy
    structure-policy lifecycle-policy input-policy)
  "Every class an extension-surface generic may dispatch its first argument on.

The list exists so that POLICY-GENERIC-P can ask about the whole *lineage*
rather than about one class.  Adding a seventh protocol means adding it here
and to POLICY's superclasses, and nothing else changes.")

(defun policy-lineage-p (class)
  "True when CLASS is somewhere on the policy lineage.

Either direction counts, and that is the point.  A method specialised on
CONVENTIONAL-POLICY is *below* POLICY; a default specialised on LAYOUT-POLICY
is *above* it.  Both are extension-surface methods, and a test written only one
way round loses one of them — which, when the defaults moved onto the six
protocol classes, would have emptied the generated extension-surface document
and made gate 2 pass over nothing at all."
  (and (classp class)
       (some (lambda (name)
               (let ((protocol (find-class name nil)))
                 (and protocol
                      (or (subtypep class protocol)
                          (subtypep protocol class)))))
             +policy-protocols+)))

(defun policy-generic-p (symbol)
  "True when SYMBOL names an extension-surface generic.

The test is structural rather than a maintained list: a generic function
exported from this package with a method whose first required argument is
specialized somewhere on the policy lineage.  A list would drift; this cannot."
  (and (symbolp symbol)
       (fboundp symbol)
       (typep (fdefinition symbol) 'generic-function)
       (eq (nth-value 1 (find-symbol (symbol-name symbol) '#:latticewm/policy))
           :external)
       (let ((gf (fdefinition symbol)))
         (some (lambda (method)
                 (policy-lineage-p (first (closer-mop:method-specializers method))))
               (closer-mop:generic-function-methods gf)))))

(defun classp (x)
  "True when X is a class object, as opposed to an EQL specializer."
  (typep x 'class))

(defun policy-generics ()
  "Every extension-surface generic, sorted by name.

This is what the generated extension-surface document walks, and what the
SWANK bridge answers 'what can I change?' with.

This walks POLICY-GENERIC-P rather than every exported generic function, and
the difference is not cosmetic.  It used to accept anything in this package
that happened to be a GENERIC-FUNCTION, which was indistinguishable from the
structural test for as long as the only generics here were the surface — and
stopped being so the moment a class with slot readers moved in.  Ten CLOS
slot accessors became extension-surface entries, gate 2 demanded docstrings
for them, and the printed contract at the top of the surface document —
\"every generic below takes a POLICY as its first argument\" — was false.

POLICY-GENERIC-P had been written, documented and left unused.  Its docstring
describes exactly this test."
  (let ((out '()))
    (do-external-symbols (symbol '#:latticewm/policy)
      (when (policy-generic-p symbol)
        (pushnew symbol out)))
    (sort out #'string< :key #'symbol-name)))
