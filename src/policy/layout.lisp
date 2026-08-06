;;;; policy/layout.lisp --- The shipped answers for LAYOUT-POLICY.
;;;;
;;;; How a rectangle is divided, and what the result looks like.  Every method
;;;; here is a default you are expected to override, and they are written to be
;;;; *readable as examples* rather than merely correct — because the realistic
;;;; way somebody changes one is by copying it and editing it.
;;;;
;;;; Specialized on LAYOUT-POLICY rather than on POLICY, which is what makes a
;;;; mixin answering for layout and nothing else a real thing you can write:
;;;;
;;;;     (defclass tall (layout-policy) ())
;;;;     (defmethod layout-children ((p tall) (c split) rect) ...)
;;;;     (defclass mine (tall conventional-policy) ())
;;;;
;;;; See the header of policy/protocol.lisp for the other five.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; SMART GAPS — one pane has nothing to be separated from
;;; ==================================================================
;;;
;;; *SMART-GAPS* WAS REGISTERED, EXPORTED, DOCUMENTED AND READ BY NOTHING.
;;; It shipped from the commit that added it until the one that added this
;;; paragraph: a DEFINE-OPTION, an export, and a paragraph in the generated
;;; extension surface saying it worked.  `latticewm --list-options' printed it.
;;; The config man page listed it.  Ten gates, the unit suite and the
;;; integration suite all passed, because every one of them asks the registry
;;; what it contains and none of them asked the *program* whether it looks.
;;; Gate 11 is the answer to that and it is the more important half of this
;;; change; this is the half that makes the document true.
;;;
;;; AND THE DOCSTRING DID NOT SURVIVE BEING IMPLEMENTED, which is the finding
;;; underneath the finding.  It said "when a workspace holds exactly one
;;; window", and an option nobody implements is a design claim nobody checks
;;; against the rest of the design.  Three cases break it:
;;;
;;;   * AN EMPTY PANE IS A PANE.  A split holding one window and one empty
;;;     pane holds exactly one window, and dropping the border there is
;;;     precisely the failure D18 names -- focus is a *place*, an empty pane
;;;     has no window to hang a border on, and an unmarked one "reads as a
;;;     broken keyboard rather than as a place".  The one case the literal
;;;     wording most obviously covers is the one case it must not.
;;;
;;;   * TABS HOLD MORE WINDOWS THAN THEY SHOW.  A workspace that is a stack of
;;;     three shows one.  Gaps and borders are about what is on the screen, so
;;;     three tabs are one pane, and this is also what sway's smart_gaps does.
;;;
;;;   * FLOATS ARE NOT TILED.  A float is not in the tree, has its own border,
;;;     and is not what a gap between panes separates.
;;;
;;; So the rule is what the eye can check: the workspace puts exactly one pane
;;; on the screen and that pane holds a window.  Everything else keeps its
;;; gaps and its borders.

(defvar *solo-windows* '()
  "(OUTPUT . WINDOW) for each output showing a single window and nothing else.

Rebuilt by the runtime at the head of every relayout, before the layout that
reads it, and *left standing* afterwards — the same shape as the world's
:LAST-PLACEMENTS and :RECT-INDEX, and for the same reason.  It is not scratch
state for the duration of a call; it is what the last layout decided, and
anything that asks OUTER-RECT or BORDER-WIDTH afterwards has to get the answer
that is on the screen.

BOUND FOR THE DURATION WAS THE FIRST VERSION AND IT WAS WRONG.  Under it these
two generics answered one thing inside a relayout and another outside one, so
`(outer-rect (current-policy) (current-output))' at a REPL described a screen
nobody was looking at.  The integration suite caught it in the section written
to prove the option works, which is the second time that file has found the
difference between the model and the screen inside a change that had already
passed the unit suite.

Empty until a relayout has happened, so a unit test and a freshly built world
see the plain behaviour: this is a property of a *screen*, and until there is
one there is nothing to be alone on.")

(defun solo-window (policy node rect)
  "The window NODE shows, when NODE shows exactly one pane and it holds one.

RECT is the rectangle NODE is about to be laid out in.  It is needed because
`which children get placed' is a layout decision — a stack shows its
selection, a grid shows the cells inside its viewport — and asking
LAYOUT-CHILDREN is how this finds out without keeping a second copy of that
decision.  A second copy is what would rot: the lattice would go on placing
cells this function had never heard of.

NIL when *SMART-GAPS* is off, which is the one place that option is read."
  (when *smart-gaps*
    (let ((panes '()))
      (labels ((walk (node)
                 ;; Two panes is already the answer, so stop: a workspace of
                 ;; forty windows costs the same as a workspace of two.
                 (when (< (length panes) 2)
                   (if (c:container-p node)
                       (dolist (entry (layout-children policy node rect))
                         (let ((child (c:child-at node (car entry))))
                           (when (and child (visible-p policy child))
                             (walk child))))
                       (push node panes)))))
        (walk node))
      (and (= 1 (length panes))
           (typep (first panes) 'c:leaf)
           (c:leaf-window (first panes))))))

(defun output-solo-window (output)
  "The window alone on OUTPUT this relayout, or NIL."
  (cdr (assoc output *solo-windows* :test #'eq)))

(defun solo-node-p (node)
  "True when NODE is the leaf holding a window that is alone on its screen."
  (let ((window (and (typep node 'c:leaf) (c:leaf-window node))))
    (and window (rassoc window *solo-windows* :test #'eq) t)))

;;; ==================================================================
;;; LAYOUT
;;; ==================================================================

(defmethod gaps ((policy layout-policy) container)
  "Pixels between adjacent panes.

NO SMART-GAPS CASE HERE, and it is not an omission.  DIVIDE-RECT spends
GAP * (n - 1) pixels, so a container with one child already spends none; a
solo workspace has nothing for an inner gap to sit between.  The two places
the option can be seen are the inset from the screen edge and the border, and
those are where it is read."
  (declare (ignore container))
  *gaps*)

(defmethod gaps ((policy layout-policy) (container c:stack))
  "Tabs share one rectangle, so a gap between them would be a gap between a
thing and itself."
  0)

(defmethod layout-children ((policy layout-policy) (split c:split) rect)
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

(defmethod layout-children ((policy layout-policy) (container c:container) rect)
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

(defmethod layout-children ((policy layout-policy) (stack c:stack) rect)
  "The selected child gets everything; the rest are not laid out at all, and
are therefore hidden."
  (let ((address (stack-visible-address policy stack)))
    (when (c:child-at stack address)
      (list (cons address rect)))))

(defmethod stack-visible-address ((policy structure-policy) (stack c:stack))
  (c:container-selection stack))

(defmethod layout-node ((policy layout-policy) node rect)
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

(defmethod visible-p ((policy layout-policy) node)
  (declare (ignore node))
  t)

(defmethod window-dimensions ((policy layout-policy) (leaf c:leaf) rect)
  "Propose the pane's size, less the border on each side.

River draws borders *around* the content rectangle, so a window given the full
pane would overflow it by twice the border width."
  (let ((inset (* 2 (border-width policy leaf nil))))
    (values (max 1 (- (c:rect-w rect) inset))
            (max 1 (- (c:rect-h rect) inset)))))

(defmethod gravity ((policy layout-policy) (leaf c:leaf) rect width height)
  "Centre a window that came back smaller than the pane it was given.

This is the *involuntary* case — a terminal that quantised to its cell size, a
dialog that refuses to grow.  The deliberate case, where a user wants a window
to take less than its pane, is served by splitting against an empty pane
instead (DESIGN D17), which is why one sane answer suffices here rather than a
gravity policy language."
  (let ((w (min width (c:rect-w rect)))
        (h (min height (c:rect-h rect))))
    (c:make-rect (+ (c:rect-x rect) (floor (- (c:rect-w rect) w) 2))
                 (+ (c:rect-y rect) (floor (- (c:rect-h rect) h) 2))
                 w h)))

(defun node-window-prop (node key)
  "The PROP KEY of the window NODE holds, or NIL.

The bridge between a window rule — which is about a *window* — and the
appearance generics, which are handed a *node*.  A rule that sets a colour for
firefox has to be findable from whichever leaf firefox is in at the time, and
the leaf changes every time the window moves."
  (let ((window (and (typep node 'c:leaf) (c:leaf-window node))))
    (and window (c:prop window key))))

(defmethod border-width ((policy appearance-policy) node focusedp)
  "Border thickness in pixels, or 0 for the one window alone on its screen.

A window rule's width still wins over the smart-gaps zero: somebody who marked
a window out asked for that width on that window, and a rule is a narrower
statement than a global."
  (declare (ignore focusedp))
  (or (node-window-prop node :border-width)
      (if (solo-node-p node) 0 *border-width*)))

(defmethod border-color ((policy appearance-policy) node focusedp)
  "Four straight-alpha floats.  The wire layer premultiplies; see W:COLOR-COMPONENT.

FOCUSEDP is T, :CURSOR or NIL — see RUNTIME:LEAF-FOCUS-STATE.  :CURSOR is
truthy, so testing it as a boolean is still correct; testing it as a keyword is
how you get the third colour."
  (let ((color (cond ((and focusedp (c:empty-pane-p node)) *empty-pane-color*)
                     ;; A rule's colour wins, and only for the focused state:
                     ;; a window somebody marked out is worth marking out when
                     ;; you are in it, and colouring it when you are not would
                     ;; make every unfocused pane compete for attention.
                     ((and focusedp (node-window-prop node :border-color)))
                     ((eq focusedp :cursor) *cursor-border-color*)
                     (focusedp *focused-border-color*)
                     (t *unfocused-border-color*))))
    (values-list color)))

(defmethod clip-rect ((policy layout-policy) node rect)
  "Nothing overhangs in the conventional layer, so nothing is clipped.

The lattice overrides this, and it is where river's set_content_clip_box earns
its keep: a cell half-scrolled off the viewport edge is cropped and its border
is redrawn at the crop edge, so it reads as a cleanly cut cell rather than a
window sliced in half."
  (declare (ignore node rect))
  nil)

(defmethod output-content ((policy layout-policy) world (output c:output))
  "Each output shows its own workspace.

The index lives on the output's PROPS rather than in a slot, because which
workspace a monitor is showing is exactly the kind of state that should not
require a core class to grow a field — and because an extension that wants a
different rule needs somewhere to put its own answer."
  (let ((stack (c:world-workspaces world)))
    (if (null stack)
        (values (c:world-root world) '())
        (let* ((index (or (c:prop output :workspace)
                          (setf (c:prop output :workspace)
                                (default-workspace-for world output))))
               (index (max 0 (min index (1- (c:container-count stack))))))
          (values (c:child-at stack index) (list index))))))

(defun default-workspace-for (world output)
  "The workspace an output shows before anybody has said otherwise.

Its own position in the output list, so a fresh two-monitor session shows
workspaces 1 and 2 rather than the same workspace twice.

The clamp is against the *current* number of workspaces, so plugging in a
second monitor before there is a second workspace shows workspace 1 on both
until one exists.  ENSURE-WORKSPACES-FOR-OUTPUTS makes sure one does."
  (let ((position (position output (c:world-outputs world)))
        (stack (c:world-workspaces world)))
    (if (null stack)
        0
        (min (or position 0) (max 0 (1- (c:container-count stack)))))))

(defun ensure-workspaces-for-outputs (world)
  "Grow the workspace list so every output can have one of its own.

Called when an output appears.  Without it, a second monitor arrives, finds
only one workspace, and mirrors the first — which looks like a bug in the
layout rather than an absence of workspaces."
  (let ((stack (c:world-workspaces world))
        (wanted (length (c:world-outputs world))))
    (when stack
      (loop while (< (c:container-count stack) wanted)
            do (let ((index (c:container-count stack)))
                 ;; MAKE-WORKSPACE, not MAKE-LEAF: a monitor plugged in after
                 ;; startup must get the same kind of workspace as the ones
                 ;; made at startup, or the second screen shows a pane where
                 ;; the first shows a plane.
                 (c:insert-child stack index
                                 (or (guarded "make-workspace"
                                       (make-workspace (current-policy) world index))
                                     (c:make-leaf)))))
      (loop for output in (c:world-outputs world)
            for index from 0
            do (unless (c:prop output :workspace)
                 (setf (c:prop output :workspace)
                       (min index (1- (c:container-count stack))))))
      stack)))

(defmethod outer-rect ((policy layout-policy) (output c:output))
  "The rectangle the tree may use, less the gaps and anything reserved.

RESERVED-SPACE is where a status bar takes its strip out, so that windows are
laid out around it rather than under it.  Everything downstream honours the
result without knowing why it is that shape.

The outer gap goes when one window is alone on the screen — see *SMART-GAPS*.
The reserved space does not: a status bar asked for its strip because it is
drawing in it, and the number of windows underneath is not its business."
  (let ((rect (c:rect-inset (c:output-rect output)
                            (if (output-solo-window output) 0 *outer-gaps*))))
    (destructuring-bind (top right bottom left) (reserved-space policy output)
      (c:make-rect (+ (c:rect-x rect) left)
                   (+ (c:rect-y rect) top)
                   (max 1 (- (c:rect-w rect) left right))
                   (max 1 (- (c:rect-h rect) top bottom))))))

(defmethod render-order ((policy layout-policy) placements)
  "Tiled nodes in layout order, then floats, then anything marked as overlay.

River says the initial position of a node in the render list is *undefined*,
so every node must be ordered explicitly or overlapping windows flicker.

The float clause used to be unreachable.  Floats are not in the tree, so they
were never in the PLACEMENTS this was handed — the runtime appended them after
the fact, above everything, and this method could only ever see tiles.  It sees
the whole render list now, which is what makes `then floats' a decision this
takes rather than a sentence describing what the caller does next."
  (stable-sort (copy-list placements) #'<
               :key (lambda (placement)
                      (let ((node (first placement)))
                        (cond ((c:prop node :overlay) 2)
                              ((and (typep node 'c:leaf)
                                    (c:leaf-window node)
                                    (c:window-floating-p (c:leaf-window node)))
                               1)
                              (t 0))))))