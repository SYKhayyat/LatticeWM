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

(defclass policy ()
  ((%name :initarg :name :initform "policy" :accessor policy-name)
   (props :initform '() :accessor policy-props
          :documentation "Extension state, as on a node.  See CORE:PROPS."))
  (:documentation
   "The object every decision dispatches on.

A policy is a *layout model plus a set of behavioural choices*, and it is an
object rather than a pile of special variables so that two of them can exist
at once — which is what lets the lattice ship as a subclass rather than as a
patch.  It carries almost no state: the tree holds the state, the policy holds
the opinions."))

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

;;; --------------------------------------------------------- tier-0 options

(defvar *options* (make-hash-table :test #'eq)
  "NAME -> (list VARIABLE DEFAULT DOCUMENTATION).  See DEFINE-OPTION.")

(defmacro define-option (name default &body (documentation))
  "Declare a tier-0 configuration value: a variable a user edits, nothing more.

    (define-option *gaps* 0
      \"Pixels of empty space left between adjacent panes.\")

This is a DEFPARAMETER plus a registration, so that the extension-surface
document can list every knob without anyone maintaining a second list of them.
DESIGN.org's tier table calls tier 0 'edit a DEFPARAMETER, no restart, and the
only tier available to a non-programmer' — so every P1 fork in the design must
appear here rather than as a branch buried in a method."
  (check-type documentation string)
  (let ((key (intern (string-trim "*" (symbol-name name)) :keyword)))
    `(progn
       (defparameter ,name ,default ,documentation)
       (setf (gethash ,key *options*) (list ',name ,default ,documentation))
       ',name)))

(defun option (name)
  "The current value of tier-0 option NAME, a keyword."
  (let ((entry (gethash name *options*)))
    (when entry (symbol-value (first entry)))))

(defun (setf option) (value name)
  (let ((entry (gethash name *options*)))
    (unless entry (error "No such option: ~s" name))
    (setf (symbol-value (first entry)) value)))

(defun option-default (name)
  "The value option NAME shipped with."
  (second (gethash name *options*)))

(defun option-documentation (name)
  "The docstring of option NAME."
  (third (gethash name *options*)))

(defun option-boundp (name)
  "True when NAME names a registered option."
  (nth-value 1 (gethash name *options*)))

(defun all-options ()
  "Every registered tier-0 option, as (KEYWORD VARIABLE VALUE DEFAULT DOC),
sorted by name."
  (let ((out '()))
    (maphash (lambda (key entry)
               (destructuring-bind (variable default documentation) entry
                 (push (list key variable (symbol-value variable)
                             default documentation)
                       out)))
             *options*)
    (sort out #'string< :key (lambda (row) (symbol-name (first row))))))

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
   "Border thickness in pixels for NODE, drawn by the compositor."))

(defgeneric border-color (policy node focusedp)
  (:documentation
   "The border colour for NODE, as (values R G B A), each 0.0 to 1.0.

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

(defgeneric on-focus-change (policy world old-path new-path)
  (:documentation
   "Called after the cursor moves from OLD-PATH to NEW-PATH.

For side effects: raising a float, warping the pointer, updating a bar.
Return value ignored.  Errors here are caught and logged rather than allowed
to abort the motion."))

(defgeneric pointer-focus (policy world x y)
  (:documentation
   "Which path, if any, does the pointer at (X, Y) name?

Returns a path or NIL.  Used by focus-follows-mouse, which is off by default
and is one of the shipped worked examples."))

;;; ==================================================================
;;; STRUCTURE — where things go
;;; ==================================================================

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

(defgeneric container-label (policy container)
  (:documentation
   "A short human-readable name for CONTAINER, or NIL.

Used by the status output, by jump-to-name, and by the lattice's coordinate
overlay.  Defaults to the node's LABEL slot when one was set."))

;;; ==================================================================
;;; INTROSPECTION — the surface describes itself
;;; ==================================================================

(defun policy-generic-p (symbol)
  "True when SYMBOL names an extension-surface generic.

The test is structural rather than a maintained list: a generic function
exported from this package whose first required argument is specialized on
POLICY somewhere.  A list would drift; this cannot."
  (and (symbolp symbol)
       (fboundp symbol)
       (typep (fdefinition symbol) 'generic-function)
       (eq (nth-value 1 (find-symbol (symbol-name symbol) '#:latticewm/policy))
           :external)
       (let ((gf (fdefinition symbol)))
         (some (lambda (method)
                 (let ((first (first (closer-mop:method-specializers method))))
                   (and (classp first)
                        (subtypep first (find-class 'policy)))))
               (closer-mop:generic-function-methods gf)))))

(defun classp (x)
  "True when X is a class object, as opposed to an EQL specializer."
  (typep x 'class))

(defun policy-generics ()
  "Every extension-surface generic, sorted by name.

This is what the generated extension-surface document walks, and what the
SWANK bridge answers 'what can I change?' with."
  (let ((out '()))
    (do-external-symbols (symbol '#:latticewm/policy)
      (when (and (fboundp symbol)
                 (typep (fdefinition symbol) 'generic-function))
        (pushnew symbol out)))
    (sort out #'string< :key #'symbol-name)))
