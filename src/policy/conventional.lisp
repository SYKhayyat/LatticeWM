;;;; policy/conventional.lisp --- The shipped policy, and its tier-0 values.
;;;;
;;;; The behaviour this file names is conventional on purpose: recursive splits
;;;; like Emacs and hyprland, tabs, workspaces, per-window floating, and
;;;; minimize that takes a window out of the tree.  Not novel, not trying to
;;;; be.  It is the smallest thing that is a usable daily driver, and therefore
;;;; the smallest thing that proves the core works.
;;;;
;;;; WHAT IS HERE IS THE CLASS AND THE VALUES.  The *methods* are in six files
;;;; beside this one, one per protocol, because that is what they turned out to
;;;; be once POLICY was split into protocols a user can implement a slice of:
;;;;
;;;;   policy/layout.lisp              LAYOUT-POLICY
;;;;   policy/defaults-motion.lisp     MOTION-POLICY, and the pointer
;;;;   policy/structure.lisp           STRUCTURE-POLICY
;;;;   policy/lifecycle.lisp           LIFECYCLE-POLICY
;;;;   policy/input.lisp               INPUT-POLICY, the reading half
;;;;   policy/appearance.lisp          APPEARANCE-POLICY
;;;;
;;;; Six files and six protocols, and that is not a coincidence any more: the
;;;; lifecycle answers were in two files until they were merged, sorted by
;;;; subject under a rule that could not be applied from outside.  Gate 18
;;;; holds the one file-naming rule the tree does keep -- DEFAULTS- names the
;;;; answers half of an algorithm/answers pair, and the algorithm half defines
;;;; no methods.
;;;;
;;;; Every one of them is a default you are expected to override, and they are
;;;; written to be *readable as examples* rather than merely correct — because
;;;; the realistic way somebody changes one is by copying it and editing it,
;;;; and because PLAN.org's honest requirement is that a cheap model must be
;;;; able to produce a fourth worked example by pattern-matching on these.
;;;;
;;;; The values stay together, here, and that is also deliberate.  They are
;;;; what a person edits on their first day, `latticewm --list-options' prints
;;;; them, and a reader looking for `the knobs' should find them in one place
;;;; rather than in seven.

(in-package #:latticewm/policy)

(defclass conventional-policy (policy)
  ((%name :initform "conventional"))
  (:documentation
   "Recursive splits, tabs, workspaces, floats, minimize-out-of-tree.

The shipped default, and the base class the lattice extends rather than
replaces — so that everything below stays true unless the lattice explicitly
says otherwise.

NOTE WHAT THIS CLASS DOES *NOT* HAVE: methods.  Every default below is
specialised on one of the six protocol classes POLICY inherits —
LAYOUT-POLICY, APPEARANCE-POLICY, MOTION-POLICY, STRUCTURE-POLICY,
LIFECYCLE-POLICY, INPUT-POLICY — and this subclass deliberately adds nothing.
The reason is the difference between an extension surface that works and one
that only looks like it does.

If the defaults lived here, then a user writing the obvious thing —

    (defmethod gaps ((policy conventional-policy) container) 8)

would *replace* the shipped method rather than extend it, and CALL-NEXT-METHOD
inside it would signal NO-NEXT-METHOD.  Every worked example in this project
uses CALL-NEXT-METHOD, because 'do what you were going to do, plus this' is
overwhelmingly the common case; so the class a user names must be strictly more
specific than the class the defaults are on.

This was found by writing the examples and running them, which is the argument
for shipping examples as tests.  See FINDINGS.org.

The defaults sit on the *narrow* protocols rather than on POLICY for a second
reason, added later: it makes a mixin that answers for one protocol and no
others a real thing you can write.  See the header of policy/protocol.lisp."))

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

(define-option *cursor-border-color* '(0.28 0.42 0.62 1.0)
  "Border colour of the pane holding the cursor while a *float* has the keyboard.

Between the focused and the unfocused colour, deliberately, because that is
exactly what the state is.  Focus is a place and a floating window is not in
the tree, so while a dialog has the keyboard the cursor is still standing
somewhere — and you need to be able to see where, because that is where
dismissing the dialog puts you back.

Set it equal to *UNFOCUSED-BORDER-COLOR* if you would rather the tree went
completely quiet while a float is up.")

(define-option *empty-pane-color* '(0.42 0.56 0.78 1.0)
  "Border colour drawn around a focused empty pane.

Deliberately bright.  DESIGN D18 accepts, as a cost of focus being a place,
that \"you can stand somewhere that typing does not reach\", and names the
mitigation: \"the cursor being unmissable — that is not optional polish, it is
what stops an empty pane reading as a broken keyboard.\"  A subtle colour here
is not a taste choice, it is the failure the ruling warned about.")

(define-option *spawn-mode* :split
  "Where a new window goes.  DESIGN D14 rules that all three ship.

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
  "DESIGN D19's keysym-to-command table for typing in an empty pane.

While the cursor rests on an empty pane, an unbound printable key is looked up
here and the named command is run, so the empty pane is a spawn menu with no
menu.  It has to be a table rather than a single default because the keystroke
is consumed as a trigger and cannot be replayed into the application that
opens — nothing in either river protocol synthesises input — so under a single
default, typing `ls` at an empty pane opens a terminal showing `s`.")

(define-option *float-fraction* 3/5
  "Fraction of the output a floated window takes when it has no opinion.")

(define-option *smart-gaps* t
  "Drop the screen-edge gap and the border when one pane is alone on a screen.

*One pane, not one window*, and the difference is three cases.  An empty pane
beside a window is a second pane and keeps both borders, because DESIGN D18
makes focus a place and an unmarked empty pane reads as a broken keyboard.
Three tabs on a workspace are one pane, because that is what is on the screen.
A floating window is not a pane at all, so it neither counts nor changes.

Inner gaps need no case: DIVIDE-RECT spends GAP once per *boundary between*
children, so a single pane already spends none.

Set to NIL to keep the gap and the border however few windows are up.  A
window rule that sets :BORDER-WIDTH still wins, on the window it names.")
