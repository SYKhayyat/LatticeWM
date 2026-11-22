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
                   ;;
                   ;; SEEN-P RATHER THAN MEMBER WITH A :TEST LAMBDA.  The lambda
                   ;; closed over NODE, which is invariant across the loop, so it
                   ;; was a fresh closure per address — on the walk that runs for
                   ;; every container in the world on every relayout, and over a
                   ;; list that for a lattice grid is every cell ever navigated
                   ;; through.
                   (flet ((seen-p (address)
                            (dolist (other seen nil)
                              (when (c:address-equal node address other)
                                (return t)))))
                     (dolist (address (c:container-addresses node))
                       (unless (seen-p address)
                         (let ((child (c:child-at node address)))
                           (when child
                             (walk child (c:path-append path address)
                                   (c:make-rect 0 0 0 0) nil))))))))))
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

(defmethod border-state ((policy appearance-policy) node focusedp)
  "The four shipped states, and the order they are asked in.

:EMPTY beats :FOCUSED because an empty focused pane is the one case where the
border is the *only* decoration there is — there is no window in it to look at
— so which pane you are in has to be readable from the border alone."
  (cond ((and focusedp (c:empty-pane-p node)) :empty)
        ((eq focusedp :cursor) :cursor)
        (focusedp :focused)
        (t :unfocused)))

(defmethod border-color-for ((policy appearance-policy) state)
  "The colour of a state nothing else answered for.

Not an error: an extension that invents a state and forgets its colour should
draw a plain border rather than stop the frame, and this is also the method
that answers for :UNFOCUSED, which is every border on the screen but one."
  (declare (ignore state))
  (values-list *unfocused-border-color*))

(defmethod border-color-for ((policy appearance-policy) (state (eql :focused)))
  (values-list *focused-border-color*))

(defmethod border-color-for ((policy appearance-policy) (state (eql :cursor)))
  (values-list *cursor-border-color*))

(defmethod border-color-for ((policy appearance-policy) (state (eql :empty)))
  (values-list *empty-pane-color*))

(defmethod border-color ((policy appearance-policy) node focusedp)
  "Four straight-alpha floats.  The wire layer premultiplies; see W:COLOR-COMPONENT.

FOCUSEDP is T, :CURSOR or NIL — see RUNTIME:LEAF-FOCUS-STATE.  :CURSOR is
truthy, so testing it as a boolean is still correct; testing it as a keyword is
how you get the third colour.

THE COMPOSITION OF TWO GENERICS AND NOTHING ELSE, which is the whole of the
change: what state this border is in, and what colour that state is drawn in.
It used to be one COND with all five branches in it, so a policy wanting a
fourth state — urgent, tagged, recording — copied the branch, and every
extension that did was a fork of a decision that runs per window per frame."
  (let ((rule (and focusedp (node-window-prop node :border-color))))
    ;; A rule's colour wins, and only for the focused state: a window somebody
    ;; marked out is worth marking out when you are in it, and colouring it
    ;; when you are not would make every unfocused pane compete for attention.
    ;; It cannot collide with :EMPTY — an empty pane holds no window, so no
    ;; rule has ever been applied to it.
    (if rule
        (values-list rule)
        (border-color-for policy (border-state policy node focusedp)))))

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
different rule needs somewhere to put its own answer.

THE CLAMP ON THE LAST LINE IS A GUARD AND NOT A DECISION, and for a while it
was the only thing standing between two monitors and a collapsed layout — see
ENSURE-WORKSPACES-FOR-OUTPUTS, which now runs before every layout so that an
index out of range cannot arrive here at all.  It stays because an extension
may write this property itself and a `nil' answer here is a black screen."
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

(defun free-workspace-index (taken count &optional preferred)
  "The workspace an output can have, given the ones TAKEN by other outputs.

PREFERRED when it is in range and nobody has it, otherwise the lowest index
nobody has.  COUNT is guaranteed to exceed the number of outputs by the time
this is called, so there is always one — the last clause is a guard against
being called out of order rather than a case that happens."
  (if (and (integerp preferred) (< -1 preferred count)
           (not (member preferred taken)))
      preferred
      (or (loop for index from 0 below count
                unless (member index taken) return index)
          0)))

(defun held-by-a-stranger-p (others output index)
  "Does any output other than OUTPUT hold INDEX, and is it not in OUTPUT's group?

THE SHARING EXCEPTION, AND NOTHING MORE.  Two outputs on one workspace is still
the collapse #96's own code calls it — every window placed twice, the second
placement winning — unless the two of them are one shared group, in which case
exactly one of them lays the plane out across both and the other draws nothing.
The distinction is made here, at the invariant, rather than in the sharing code,
because this is the only place that knows which outputs are showing what: a
second opinion would be a second chance to disagree."
  (some (lambda (other)
          (and (not (eq other output))
               (eql index (c:prop other :workspace))
               (not (c:share-siblings-p output other))))
        others))

(defun ensure-workspaces-for-outputs (world)
  "Every output shows a workspace, and no two outputs show the same one.

THIS IS AN INVARIANT AND NOT A STEP IN A PROCEDURE, which is the whole of what
was wrong with it.  It used to be called from exactly one place — the moment an
output appeared — and its docstring described that moment rather than the
property.  Restoring a saved layout afterwards replaces the workspace stack
with the saved one, and nothing asked the question a second time: a layout
saved on one monitor and reloaded on two left one workspace for two outputs,
OUTPUT-CONTENT clamped both of them to the last valid index, and the two
outputs returned the identical node.  Every window is then placed twice, the
second placement wins, and the second monitor is black while the model insists
everything is fine.  Both mechanisms were correct and their *order* was not,
which is why every gate and both suites passed — there is no state in which
either one is wrong on its own.

So it is asserted before every layout now (see RELAYOUT) as well as when an
output arrives, and it is idempotent and cheap: when the counts already agree
it is a LENGTH, a comparison and one pass over the outputs.

Four things, in this order:

  * grow the stack until there are at least as many workspaces as outputs.
    MAKE-WORKSPACE, not MAKE-LEAF: a monitor plugged in after startup must get
    the same kind of workspace as the ones made at startup, or the second
    screen shows a pane where the first shows a plane;
  * give a workspace to any output that has not got one;
  * keep one shared group on one workspace, by the leader's answer, because a
    plane that two screens disagree about is a screen drawing nothing (#126);
  * and move an output off a workspace another output is already showing, or
    off an index that is no longer in range.  That third clause is the one the
    original lacked, and it is what makes this a repair rather than a
    precaution — the state it fixes is reachable from a restore, from an
    unplugged monitor, and from an extension that writes the property itself."
  (let ((stack (c:world-workspaces world))
        (outputs (c:world-outputs world)))
    (when stack
      (loop while (< (c:container-count stack) (length outputs))
            do (let ((index (c:container-count stack)))
                 (c:insert-child stack index
                                 (or (guarded "make-workspace"
                                       (make-workspace (current-policy) world index))
                                     (c:make-leaf)))))
      (let ((count (c:container-count stack))
            (taken '())
            (settled '()))
        (loop for output in outputs
              for position from 0
              for index = (c:prop output :workspace)
              do (unless (and (integerp index) (< -1 index count)
                              (not (held-by-a-stranger-p settled output index)))
                   (setf index (free-workspace-index taken count position)
                         (c:prop output :workspace) index))
                 (push index taken)
                 (push output settled))
        ;; A GROUP SHOWS ONE PLANE (#126).  The loop above is about screens
        ;; that are *not* sharing landing on one workspace; this is the
        ;; opposite failure -- siblings that have drifted onto two -- and it is
        ;; the same repair with the leader's answer winning.  SHOW-WORKSPACE-ON
        ;; moves a group together, so what reaches here is what did not go
        ;; through it: a state file, an extension writing the property, or a
        ;; bug.  A follower's answer was never its own, so the leader's wins.
        (dolist (output outputs)
          (let ((group (c:output-share-group output)))
            (when (and group (not (eq output (first group))))
              (setf (c:prop output :workspace)
                    (c:prop (first group) :workspace))))))
      stack)))

;;; ------------------------------------------------------------ where a window sits
;;;
;;; #14.  Snapping is not a second layout engine: it is a *place*, computed from
;;; the same rectangle the tiled layout uses, and a window put there is floating.
;;; That is the whole design, and it is why SNAP-RECT is a pure function of the
;;; policy, the output and a keyword -- the command, a window rule and a test all
;;; ask the same question and none of them has its own copy of the arithmetic.

(defparameter +snap-places+
  '(:left :right :top :bottom
    :top-left :top-right :bottom-left :bottom-right
    :maximize :center)
  "Every place SNAP-RECT understands, in the order an inspector should list them.

NINE REGIONS AND A CENTRE, which is the set Aero Snap and every tiling
compositor's `drag to the edge' gesture agrees on.  They are deliberately not
arbitrary fractions: a place has to be reachable by dragging a window at it, and
`the left third' is not something a pointer can mean unambiguously.")

(defun snap-place-p (place)
  (and place (member place +snap-places+) t))

(defun snap-rect (policy output place)
  "The rectangle PLACE means on OUTPUT, inside the space windows may use.

THE SAME SPACE THE LAYOUT USES -- OUTER-RECT, so the gaps, the panels reserved
by a layer surface and (#96) a shared group's union are all already accounted
for.  A snapped window that overlapped a status bar, or ignored the gap it was
given, would be a second opinion about where the screen is.

:MAXIMIZE is not fullscreen.  Fullscreen is river's, it hides the bars and
ignores the layout; this fills the space a window is allowed to occupy, which is
what `maximize' means everywhere else.

:CENTER is a window of half the space, centred in it.  Centring the size a window
already has would need the window, and then this is not a place -- it is a
command that moves something.  The predictable answer is worth more here than
the specific one, and a window you wanted at its own size has not been resized
by snapping it there."
  (let* ((base (outer-rect policy output))
         (x (c:rect-x base))
         (y (c:rect-y base))
         (w (c:rect-w base))
         (h (c:rect-h base))
         (half-w (floor w 2))
         (half-h (floor h 2)))
    (ecase place
      (:maximize base)
      (:left     (c:make-rect x y half-w h))
      (:right    (c:make-rect (+ x half-w) y (- w half-w) h))
      (:top      (c:make-rect x y w half-h))
      (:bottom   (c:make-rect x (+ y half-h) w (- h half-h)))
      (:top-left     (c:make-rect x y half-w half-h))
      (:top-right    (c:make-rect (+ x half-w) y (- w half-w) half-h))
      (:bottom-left  (c:make-rect x (+ y half-h) half-w (- h half-h)))
      (:bottom-right (c:make-rect (+ x half-w) (+ y half-h)
                                  (- w half-w) (- h half-h)))
      (:center   (c:make-rect (+ x (floor (- w half-w) 2))
                              (+ y (floor (- h half-h) 2))
                              half-w half-h)))))

(defmethod outer-rect ((policy layout-policy) (output c:output))
  "The rectangle the tree may use, less the gaps and anything reserved.

RESERVED-SPACE is where a status bar takes its strip out, so that windows are
laid out around it rather than under it.  Everything downstream honours the
result without knowing why it is that shape.

The outer gap goes when one window is alone on the screen — see *SMART-GAPS*.
The reserved space does not: a status bar asked for its strip because it is
drawing in it, and the number of windows underneath is not its business.

THE RECTANGLE ASKED ABOUT IS THE SHARED ONE WHEN THERE IS ONE (#96).  An output
presenting a workspace shared with its neighbours presents the union of the
group, so the gaps go round the whole arrangement rather than round each
monitor in it: a shared plane with a gap down the seam would be two planes with
a border between them, which is the thing sharing exists to remove.  The
reserved space is the leader's own, because that is the output the surfaces
were drawn for — one status strip per group, not one per monitor."
  (let ((rect (c:rect-inset (or (c:output-share-rect output)
                                (c:output-rect output))
                            (if (output-solo-window output) 0 *outer-gaps*))))
    (destructuring-bind (top right bottom left) (reserved-space policy output)
      (c:make-rect (+ (c:rect-x rect) left)
                   (+ (c:rect-y rect) top)
                   (max 1 (- (c:rect-w rect) left right))
                   (max 1 (- (c:rect-h rect) top bottom))))))

(defmethod render-order ((policy layout-policy) placements)
  "Tiled nodes in layout order, then floats.

River says the initial position of a node in the render list is *undefined*,
so every node must be ordered explicitly or overlapping windows flicker.

The float clause used to be unreachable.  Floats are not in the tree, so they
were never in the PLACEMENTS this was handed — the runtime appended them after
the fact, above everything, and this method could only ever see tiles.  It sees
the whole render list now, which is what makes `then floats' a decision this
takes rather than a sentence describing what the caller does next.

AND THERE WAS A THIRD TIER THAT NOTHING COULD ENTER.  This said `then anything
marked as overlay' and sorted a node carrying an :OVERLAY property above the
floats.  Nothing in the program has ever written that property — the echo area,
the help page, the cursor and the lattice's own two overlays are *surfaces*,
made by runtime/surface.lisp, and they are not nodes and are not in a render
list.  A documented tier reachable only by an extension nobody told about it,
which is the shape gate 13 now fails the build on.  Deleted rather than wired
up, because the tier a policy actually wants is this method: RENDER-ORDER is
the documented extension point, an override of it is obeyed for the whole list
(see RENDER-ORDER-DECIDES-THE-WHOLE-LIST-AND-NOT-HALF-OF-IT), and a property
key standing in for a generic is the mistake *SMART-GAPS* made with an option."
  ;; TWO BUCKETS, WALKED ONCE.  This was a STABLE-SORT over a COPY-LIST with a
  ;; key that answers 0 or 1 — a comparison sort, per relayout, to do a
  ;; partition, and the copy exists only because STABLE-SORT is destructive.
  ;; One pass and two NREVERSEs is the same answer with the same stability and
  ;; without the O(n log n) or the second list.
  ;;
  ;; AND PINNED FLOATS SORT ABOVE THE REST (#13).  Three buckets rather than
  ;; two: a pinned window is an ordinary float that stays on top of its
  ;; neighbours -- the thing you pin is the reference you are working against,
  ;; so the alternative (pinning means nothing) makes the attribute a lie.  The
  ;; order *within* each group is the world's, which is recency: raising a
  ;; window is moving it in WORLD-FLOATS, and this does not overrule that.
  (let ((tiled '()) (floating '()) (pinned '()))
    (dolist (placement placements)
      (let* ((node (first placement))
             (window (and (typep node 'c:leaf) (c:leaf-window node))))
        (cond
          ((and window (c:window-pinned-p window)) (push placement pinned))
          ((and window (c:window-floating-p window)) (push placement floating))
          (t (push placement tiled)))))
    (nconc (nreverse tiled) (nreverse floating) (nreverse pinned))))

;;; --------------------------------------------------- where a step ends (#1)

(define-option *workspace-nav* :wrap
  "What the last workspace is: a wall (:wrap) or the edge of the map (:grow).

  :wrap  the next workspace after the last one is the first, as it always was
  :grow  the next workspace after the last one is a new one

THE TWO ANSWERS ARE BOTH RIGHT AND THEY ARE ABOUT DIFFERENT DESKS.  Wrapping is
what a fixed set of numbered workspaces wants -- i3 and the tiling tradition, and
before this option the only answer this program had.  Growing is what infinite
workspaces are for, and #1 exists because somebody switched to the end of their
stack, pressed the key again, and found themselves back at the beginning, which
reads as `the desktop is smaller than I thought' rather than as a decision.

What grows is the *far end only*: stepping back from the first workspace wraps,
because there is nothing behind it and a workspace numbered zero would undo the
one thing every workspace number in this program agrees about.

The numbered verb is untouched.  `M-x workspace 40' has always created what it
needed, and the stack grows; this option only decides what happens when a
*relative* step runs off the end.")

(defun grow-workspace-nav-p ()
  "True when a relative step off the far end grows instead of wrapping.

A guard rather than a raw read, because *WORKSPACE-NAV* is a variable a config
file sets: a typo -- :grows, \"grow\" -- must leave the desktop behaving as it
did rather than stopping every relative step dead."
  (eq *workspace-nav* :grow))
