;;;; runtime/emit.lisp --- Turning a layout into protocol calls.
;;;;
;;;; This is the only file that knows both what the model means and what the
;;;; wire wants.  Policy says where things go; this decides which protocol
;;;; sequence each call belongs in, and never lets that distinction leak
;;;; upwards.
;;;;
;;;; THE SHAPE OF THE PROBLEM
;;;;
;;;; A layout produces two kinds of instruction at once, and river will not
;;;; accept them at the same time:
;;;;
;;;;   propose_dimensions, focus, capabilities   MANAGE sequence only
;;;;   set_position, borders, hide/show, order   rendering state
;;;;
;;;; So the layout is computed once, split into two piles, and each pile is
;;;; drained when its sequence comes round.  The manage pile is held on the
;;;; server object between sequences.
;;;;
;;;; EVERYTHING IS DIFFED.  River processes every request we send before it can
;;;; act on further input, and the spec has an `unresponsive' error it will use
;;;; if we are slow.  Re-sending a hundred unchanged positions on every
;;;; keystroke is a real cost for no benefit, so each property remembers what
;;;; was last sent for it.

(in-package #:latticewm/runtime)

(defun emitted (window property)
  "The last value sent for PROPERTY of WINDOW."
  (gethash (cons window property) (server-emitted *server*) :none))

(defun (setf emitted) (value window property)
  (setf (gethash (cons window property) (server-emitted *server*)) value))

(defmacro when-changed ((window property value) &body body)
  "Run BODY only if VALUE differs from what was last sent for PROPERTY."
  (let ((v (gensym "VALUE")) (w (gensym "WINDOW")) (p (gensym "PROPERTY")))
    `(let ((,v ,value) (,w ,window) (,p ,property))
       (unless (equal ,v (emitted ,w ,p))
         (setf (emitted ,w ,p) ,v)
         ,@body))))

(defun forget-window-state (window)
  "Drop the diff cache for WINDOW.

Called when a window goes away, and when we deliberately want the next
relayout to re-send everything — after a hot reconnect, say."
  (let ((table (server-emitted *server*)))
    (loop for key being the hash-keys of table
          when (eq (car key) window) do (remhash key table))))

;;; ------------------------------------------------------------ the layout

(defun compute-layout ()
  "Lay the world out over the outputs and return the placements.

Multi-monitor is one model with one viewport per output (PLAN §fiat), so this
lays the same tree out on each output and lets the policy decide what each one
shows.  With a single output — which is every laptop — it is one call."
  (let* ((policy (p:current-policy))
         (outputs (all-outputs))
         (root (c:world-root *world*)))
    (cond
      ;; No outputs yet: lay out over a nominal rectangle so the tree is in a
      ;; consistent state.  Nothing is drawn, because nothing is connected.
      ((null outputs)
       (guarded "layout" (p:layout-node policy root (c:make-rect 0 0 1920 1080))))
      (t
       (let ((placed '()))
         (dolist (output outputs (nreverse placed))
           (multiple-value-bind (node prefix)
               (guarded "output-content" (p:output-content policy *world* output))
             (when node
               ;; Paths come back relative to NODE; rebase them onto the root so
               ;; that a placement is addressable globally and the cursor can be
               ;; compared against it.
               (dolist (placement (guarded "layout"
                                    (p:layout-node policy node
                                                   (p:outer-rect policy output))))
                 (destructuring-bind (child path rect visible) placement
                   (push (list child (append prefix path) rect visible)
                         placed)))))))))))

(defun solo-windows (policy)
  "(OUTPUT . WINDOW) for every output whose workspace shows one window alone.

Computed once, before the layout, because the answer is needed *by* the layout
— OUTER-RECT drops the screen-edge gap and WINDOW-DIMENSIONS sizes the window
against a border that BORDER-WIDTH is about to say is zero — and again during
the emit, where the border is actually sent.  Two computations would be two
chances to disagree, and the disagreement's shape is a window sized for a
border it does not get.

The probe rectangle is the output's own, before the gap and the reserved space
come out of it.  It is a rectangle only so that LAYOUT-CHILDREN can be asked
which children it would place; a few pixels either way cannot change a count
that has to be exactly one."
  (let ((out '()))
    (dolist (output (all-outputs) (nreverse out))
      (let ((node (guarded "output-content" (p:output-content policy *world* output))))
        (when node
          (let ((window (guarded "solo-window"
                          (p:solo-window policy node (c:output-rect output)))))
            (when window (push (cons output window) out))))))))

(defun draw-overlays ()
  "Draw everything we render ourselves, and place it above the windows.

Runs at the end of a render sequence: an overlay is a wl_surface of ours
positioned by a river node, so it is rendering state like any other."
  (dolist (output (all-outputs))
    (guarded "echo area" (draw-echo-area *world* output)))
  (guarded "overlays" (run-hooks :draw-overlays)))

(defun index-placements (placements)
  "A node-to-rect hash, cached on the world for motion and pointer hit tests."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (placement placements table)
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path visible))
        (setf (gethash node table) rect)))))

(defun relayout (&key (force nil))
  "Recompute the layout and emit everything that changed.

Called after every command that could have moved something.  It is safe to
call more than once — the diff makes a redundant call nearly free — so callers
should err towards calling it rather than reasoning about whether they must."
  (unless (and *server* *world*)
    (return-from relayout nil))
  ;; Called from outside any protocol sequence — which is what happens when you
  ;; evaluate (relayout) at a SWANK REPL, and is the single most likely thing
  ;; for anyone extending this to type.  Emitting here would violate the
  ;; sequence discipline on every request; instead, ask river for a manage
  ;; sequence and do the work when it arrives.
  ;;
  ;; This is what makes (setf *gaps* 8) (relayout) work from a REPL, which is
  ;; the whole live-development story and would otherwise have been a trap with
  ;; a good error message in it.
  (when (null w:*sequence*)
    (when force (clrhash (server-emitted *server*)))
    (mark-dirty)
    (request-manage)
    (return-from relayout :deferred))
  (when force
    (clrhash (server-emitted *server*)))
  ;; SET BEFORE THE LAYOUT AND LEFT STANDING, exactly like the two PROPs below.
  ;; BORDER-WIDTH is asked once by WINDOW-DIMENSIONS on the way down and once by
  ;; EMIT-BORDERS on the way out and the two have to agree; so does anything
  ;; that asks afterwards, which is why this is not a binding that unwinds.
  ;; It has to precede COMPUTE-LAYOUT because the layout is one of its readers.
  ;; See P:*SOLO-WINDOWS*.
  (setf p:*solo-windows* (solo-windows (p:current-policy)))
  (let* ((policy (p:current-policy))
         (placements (compute-layout)))
    (setf (c:prop *world* :last-placements) placements
          (c:prop *world* :rect-index) (index-placements placements))
    ;; Split the work.  The manage pile waits for the next manage sequence.
    (setf (server-pending-dimensions *server*)
          (collect-dimension-work policy placements))
    ;; RENDER-ORDER IS ASKED ONCE, DOWN IN EMIT-RENDER-ORDER, ABOUT THE WHOLE
    ;; RENDER LIST.  It used to be asked here, about the tree placements alone,
    ;; and the floats were appended unconditionally afterwards — so a policy
    ;; that put a float below a tiled window was overruled by the caller and
    ;; looked like a broken generic.  Emitting position and borders in render
    ;; order bought nothing; only the place_above chain cares.
    (emit-rendering-state policy placements)
    (draw-overlays)
    (run-hooks :layout-changed)
    placements))

(defun collect-dimension-work (policy placements)
  "The window-management half of the layout: what size each window should be.

Returned as a list of (WINDOW WIDTH HEIGHT) rather than sent, because
propose_dimensions is manage-sequence-only and a relayout can happen at any
time."
  (let ((out '()))
    (dolist (placement placements (nreverse out))
      (destructuring-bind (node path rect visible) placement
        (declare (ignore path))
        (when (and visible (typep node 'c:leaf) (c:leaf-window node))
          (let ((window (c:leaf-window node)))
            (when (and (c:window-live-p window)
                       (not (c:window-minimized-p window))
                       (not (c:window-fullscreen-p window)))
              (multiple-value-bind (width height)
                  (guarded "window-dimensions"
                    (p:window-dimensions policy node rect))
                (when (and width height)
                  (push (list window (max 1 width) (max 1 height)) out))))))))))

(defun emit-window-management-state ()
  "Reconcile every window's management state with the model.  Manage only.

WHY THIS EXISTS RATHER THAN EACH COMMAND SENDING ITS OWN REQUESTS.  Floating,
minimizing and fullscreening all change window-management state, which is legal
only inside a manage sequence.  A command invoked from a *key binding* is
already inside one; the same command invoked from a REPL is not.  So a
command that sent its own requests worked when typed and failed when evaluated
— which is precisely backwards, since the REPL is the development interface.

Instead the commands change the model and this reconciles it, from the one
place that is always in the right sequence.  It is the same rule the file
header states: policy says where things go, the runtime decides which sequence
that turns into.  Diffed, so the common case sends nothing."
  (dolist (window (all-windows))
    (let ((proxy (c:window-proxy window)))
      (when (and proxy (c:window-live-p window))
        ;; Tiled edges: a floating window is adjacent to nothing, so clients
        ;; stop suppressing their rounded corners and shadows.
        (when-changed (window :tiled (not (c:window-floating-p window)))
          (guarded "set_tiled"
            (w:window-set-tiled proxy (if (c:window-floating-p window)
                                          w:+edges-none+
                                          w:+edges-all+))))
        ;; Fullscreen.  Cheap in both directions: river ignores clip boxes and
        ;; draws no borders while fullscreen, so there is no relayout either
        ;; way.
        (when-changed (window :fullscreen (c:window-fullscreen-p window))
          (guarded "fullscreen"
            (if (c:window-fullscreen-p window)
                (let ((output (current-output)))
                  (when (and output (c:output-proxy output))
                    (w:window-fullscreen proxy (c:output-proxy output))
                    (w:window-inform-fullscreen proxy)))
                (progn (w:window-exit-fullscreen proxy)
                       (w:window-inform-not-fullscreen proxy)))))))))

(defun close-window-later (window)
  "Ask river to close WINDOW at the next manage sequence.

Not now, because now is almost never legal.  river_window_v1.close is
window-management state, and every caller is a command running from a key
binding -- which is outside any sequence.  Sending it there is a
SEQUENCE-VIOLATION, which GUARDED turns into a log line and a window that does
not close.

That is what Super+q did on every machine from the first day: seven refusals
in the log of the first bare-metal session that pressed it, and nothing on
screen to say why."
  (when (and *server* window (c:window-proxy window))
    (pushnew window (server-pending-closes *server*))
    (request-manage)
    t))

(defun emit-pending-closes ()
  "Drain the windows waiting to be closed.  Manage sequence only."
  (let ((work (server-pending-closes *server*)))
    (setf (server-pending-closes *server*) '())
    (dolist (window work)
      (when (c:window-proxy window)
        (guarded "close" (w:window-close (c:window-proxy window)))))))

(defun emit-dimension-work ()
  "Drain the pending propose_dimensions work.  Manage sequence only."
  (let ((work (server-pending-dimensions *server*)))
    (setf (server-pending-dimensions *server*) '())
    (dolist (entry work)
      (destructuring-bind (window width height) entry
        (when (c:window-proxy window)
          (when-changed (window :dimensions (list width height))
            (guarded "propose_dimensions"
              (w:window-propose-dimensions (c:window-proxy window)
                                           width height))))))))

;;; ------------------------------------------------------- rendering state

(defun emit-rendering-state (policy placements)
  "Position, order, borders, visibility and clipping for everything.

Note the ordering pass at the end: river says the initial position of a node in
the render list is *undefined*, so a node nobody ordered can be drawn anywhere
in the stack.  Every visible node has to be ordered explicitly, every time its
set changes, or overlapping windows flicker between frames."
  (let ((cursor (c:world-cursor *world*))
        (shown '()))
    (dolist (placement placements)
      (destructuring-bind (node path rect visible) placement
        (when (typep node 'c:leaf)
          (let ((window (c:leaf-window node)))
            (when (and window (c:window-proxy window) (c:window-live-p window))
              (cond
                ((and visible (not (c:window-minimized-p window)))
                 (push placement shown)
                 (emit-window-visible policy window node path rect cursor))
                (t
                 (when-changed (window :shown nil)
                   (guarded "hide" (w:window-hide (c:window-proxy window)))))))))))
    ;; The floats come back as placements rather than being appended by the
    ;; caller, so that RENDER-ORDER is handed the whole render list and its
    ;; answer is the whole answer.
    (let ((render-list (append (nreverse shown) (emit-floats policy))))
      (hide-unplaced-windows render-list)
      (emit-render-order policy render-list))))

(defun hide-unplaced-windows (render-list)
  "Hide every live window the layout did not place at all.

WITHOUT THIS, `NOT PLACED' AND `PLACED AND INVISIBLE' MEAN DIFFERENT THINGS TO
RIVER, AND ONLY ONE OF THEM IS HANDLED ABOVE.  The loop in EMIT-RENDERING-STATE
walks *placements*, so it can only hide a window the layout mentioned and marked
invisible.  A window the layout never mentions is not hidden by it — it is
simply not spoken about, and river goes on drawing it exactly where it was.

Two ordinary things produce a window the layout never mentions, and both were
broken:

  * *Minimize.*  The shipped ON-MINIMIZE takes the window out of the tree
    entirely, which is the stated requirement — `minimized windows leave the
    tiling tree and the remaining windows retile without them'.  So the next
    layout has nothing to say about it.  MINIMIZE-WINDOW's own comment claimed
    `the emitter hides everything it did not place', which is what this function
    is named after and what was not true until it existed.

  * *Every workspace you are not on.*  OUTPUT-CONTENT returns the workspace the
    output is showing, so COMPUTE-LAYOUT lays out that subtree and no other.
    The windows on workspace 1 are not invisible placements when you are on
    workspace 2; they are absent.

Both are invisible to a unit test, because a unit test asks the model what it
thinks rather than asking river what it was told, and the model was right both
times.  The integration suite found them by reading the diff table — which is
the record of what actually went out on the wire — after a real minimize and a
real workspace switch.

*UNPLACED WINDOWS ARE EXEMPT.*  A window river has announced but which has not
reached PLACE-UNPLACED-WINDOWS yet is not a window that should be hidden; it is
a window nobody has decided about, and hiding it would make every new window
flash.  Diffed like everything else here, so the common case — nothing to hide —
is one walk of a list that is almost always short."
  (let ((placed (make-hash-table :test #'eq)))
    (dolist (placement render-list)
      (let ((node (first placement)))
        (when (typep node 'c:leaf)
          (let ((window (c:leaf-window node)))
            (when window (setf (gethash window placed) t))))))
    (dolist (window (all-windows))
      (let ((proxy (c:window-proxy window)))
        (when (and proxy
                   (c:window-live-p window)
                   (not (gethash window placed))
                   (not (member window *unplaced*)))
          (when-changed (window :shown nil)
            (guarded "hide" (w:window-hide proxy))))))))

(defun leaf-focus-state (path cursor)
  "What kind of focus the pane at PATH has: T, :CURSOR, or NIL.

Three states rather than two, and the third one is what a focused float costs.
Focus is a *place* (D18) and a float is deliberately not in the tree, so while a
float has the keyboard the cursor is still somewhere — and drawing that pane as
though it had the keyboard is a lie, while drawing it as though it were any
other pane loses the only indication of where dismissing the float returns you.

  T        this pane holds the cursor and the keyboard
  :CURSOR  this pane holds the cursor; a float has the keyboard
  NIL      neither

:CURSOR is truthy, so a policy method written against the old two-state
argument keeps behaving exactly as it did.  A method that wants the third
colour tests for the keyword."
  (cond ((not (c:path-equal path cursor)) nil)
        ((c:world-focused-float *world*) :cursor)
        (t t)))

(defun emit-window-visible (policy window node path rect cursor)
  "Show WINDOW at RECT, with the border and clip its policy asks for."
  (let* ((proxy (c:window-proxy window))
         (focusedp (leaf-focus-state path cursor))
         (placed (place-rect policy node rect window)))
    (setf (c:window-rect window) placed)
    (when-changed (window :shown t)
      (guarded "show" (w:window-show proxy)))
    (let ((river-node (window-river-node window)))
      (when river-node
        (when-changed (window :position (list (c:rect-x placed) (c:rect-y placed)))
          (guarded "set_position"
            (w:node-set-position river-node (c:rect-x placed) (c:rect-y placed))))))
    ;; Borders.  Also the only decoration a focused *empty* pane could have —
    ;; but an empty pane has no window, so the cursor there is drawn by the
    ;; overlay instead; see runtime/cursor.lisp.
    (emit-borders policy window node focusedp)
    ;; Content clipping: the viewport edge, rendered by the compositor.
    (let ((clip (guarded "clip-rect" (p:clip-rect policy node placed))))
      (when-changed (window :clip (and clip (list (c:rect-x clip) (c:rect-y clip)
                                                  (c:rect-w clip) (c:rect-h clip))))
        (guarded "set_content_clip_box"
          (if clip
              (w:window-set-content-clip-box
               proxy (- (c:rect-x clip) (c:rect-x placed))
               (- (c:rect-y clip) (c:rect-y placed))
               (c:rect-w clip) (c:rect-h clip))
              ;; A clip box covering everything is how you turn clipping off.
              (w:window-set-content-clip-box proxy 0 0 (c:rect-w placed)
                                             (c:rect-h placed))))))))

(defun emit-borders (policy window node focusedp)
  "Ask the policy for WINDOW's border and send it, diffed, premultiplied.

ONE FUNCTION, CALLED FROM BOTH PLACES, and that is the point.  The tiled path
and the floating path each had their own copy of this, they disagreed about
which predicate meant `focused', and one of them did not premultiply.  Two
copies of a decision are two chances to get it wrong and one of them will."
  (let ((proxy (c:window-proxy window)))
    (when proxy
      (let ((width (guarded "border-width" (p:border-width policy node focusedp))))
        (multiple-value-bind (r g b a)
            (guarded "border-color" (p:border-color policy node focusedp))
          (when (and width r)
            (when-changed (window :borders (list width r g b a))
              (multiple-value-bind (wire-r wire-g wire-b wire-a)
                  (w:premultiplied-rgba r g b (or a 1.0))
                (guarded "set_borders"
                  (w:window-set-borders proxy w:+edges-all+ width
                                        wire-r wire-g wire-b wire-a))))))))))

(defun window-focused-p (window)
  "True when WINDOW is the one the keyboard is talking to.

THE ONE PREDICATE, because there were two and they disagreed exactly where it
mattered.  CURRENT-WINDOW is *the cursor's* window; FOCUSED-WINDOW is the one
with keyboard focus; they differ precisely when a floating window is focused —
and the float-drawing path asked CURRENT-WINDOW, so a focused float was drawn
with the unfocused border.  The single case the distinction exists for was the
single case that got it wrong, for the second time.

Every call site that means \"is this the window the user is typing into\" asks
this and nothing else."
  (and window (eq window (focused-window))))

(defun place-rect (policy node rect window)
  "Where WINDOW actually goes inside the RECT it was assigned.

River's propose_dimensions is advisory, and the spec calls out terminal
emulators that quantise to their cell size, so a window frequently comes back
smaller than its pane.  GRAVITY decides where the shortfall goes; the shipped
answer is to centre it."
  (let ((width (c:window-width window))
        (height (c:window-height window)))
    (if (and (plusp width) (plusp height)
             (or (< width (c:rect-w rect)) (< height (c:rect-h rect))))
        (or (guarded "gravity" (p:gravity policy node rect width height)) rect)
        rect)))

(defun emit-floats (policy)
  "Position the floating windows, and return them as placements.

They are not in the tree, so they are not in the layout; they are placed from
their own rectangles, and an *anchored* float is offset by wherever its anchor
node ended up — which is what makes 'a floating window inside a window' travel
with the window it belongs to.

RETURNING PLACEMENTS IS WHAT MAKES P:RENDER-ORDER MEAN ANYTHING FOR FLOATS.
This used to return nothing and the caller appended the floats to the end of
the render list itself, above every tiled window, unconditionally — so the one
generic whose whole job is to order the render list was consulted about the
tiled half and overruled about the other.  A policy that wanted a float below a
tiled window wrote a method, watched nothing happen, and had every reason to
think the generic was broken rather than the caller.  The float's node is
FLOAT-LEAF, the same node every appearance generic is already handed for it, so
a RENDER-ORDER method sees floats and tiles in one vocabulary."
  (let ((out '()))
    (dolist (float (c:world-floats *world*) (nreverse out))
      (let* ((window (c:float-window float))
             (proxy (and window (c:window-proxy window))))
        (when (and proxy (c:window-live-p window))
          (if (c:window-minimized-p window)
              (when-changed (window :shown nil)
                (guarded "hide" (w:window-hide proxy)))
              (let* ((anchor (c:float-anchor float))
                     (base (and anchor (gethash anchor (c:prop *world* :rect-index))))
                     (rect (c:float-rect float))
                     (placed (if base
                                 (c:make-rect (+ (c:rect-x base) (c:rect-x rect))
                                              (+ (c:rect-y base) (c:rect-y rect))
                                              (c:rect-w rect) (c:rect-h rect))
                                 rect)))
                (setf (c:window-rect window) placed)
                (when-changed (window :shown t)
                  (guarded "show" (w:window-show proxy)))
                ;; Diffed like everything around it.  This was the one emission
                ;; in the file that was not, so every float re-proposed identical
                ;; dimensions on every relayout — and river processes every
                ;; request we send before it can answer input, which is the whole
                ;; reason the diff table exists.
                (when-changed (window :float-dimensions
                                      (list (c:rect-w placed) (c:rect-h placed)))
                  (push (list window (c:rect-w placed) (c:rect-h placed))
                        (server-pending-dimensions *server*)))
                (let ((river-node (window-river-node window)))
                  (when river-node
                    (when-changed (window :position (list (c:rect-x placed)
                                                          (c:rect-y placed)))
                      (guarded "set_position"
                        (w:node-set-position river-node (c:rect-x placed)
                                             (c:rect-y placed))))))
                ;; The float's own leaf, kept on the float record rather than
                ;; made fresh each time: BORDER-COLOR is handed a node, and a
                ;; node that is a different object on every relayout cannot carry
                ;; a prop, cannot be compared, and cannot be the thing a
                ;; window-rule hung a colour on.
                (emit-borders policy window (float-leaf float)
                              (window-focused-p window))
                ;; The same shape a tree placement has: (NODE PATH RECT VISIBLE).
                ;; PATH is NIL because a float is deliberately not in the tree,
                ;; which is exactly what a RENDER-ORDER method wanting to know
                ;; `is this a float' can test.
                (push (list (float-leaf float) nil placed t) out))))))))

(defun float-leaf (float)
  "The LEAF standing for FLOAT, made once and kept.

A floating window is deliberately not in the tree, but every appearance generic
takes a *node* — so the float needs one to be asked about.  Making a throwaway
(C:MAKE-LEAF WINDOW) at each call site meant the node identity changed on every
relayout, which quietly cost the float every per-node facility the rest of the
system has: props, labels, and any rule that identified it."
  (or (c:float-node float)
      (setf (c:float-node float) (c:make-leaf (c:float-window float)))))

(defun emit-render-order (policy placements)
  "Order every visible window, bottom to top, however P:RENDER-ORDER says.

River leaves the initial render position undefined, so this cannot be skipped
for windows we have not moved.  It can, however, be skipped when the *sequence*
has not changed, which is the common case.

PLACEMENTS IS THE WHOLE RENDER LIST — tiled placements and floats together —
and the generic is asked once, about all of it.  It used to be asked in
RELAYOUT about the tree alone, with the floats appended here afterwards, which
made P:RENDER-ORDER's own documented contract (`tiled nodes in layout order,
then floats, then overlays') something the caller enforced rather than
something the method decided.  The shipped method still says exactly that, so
nothing moves on screen; what changed is that a method saying otherwise is now
obeyed."
  (let* ((ordered (or (guarded "render-order" (p:render-order policy placements))
                      placements))
         (order (loop for placement in ordered
                      for node = (first placement)
                      for window = (and (typep node 'c:leaf) (c:leaf-window node))
                      when (and window (c:window-live-p window)
                                (not (c:window-minimized-p window)))
                        collect window)))
    ;; EQUAL on a list of objects compares them with EQL, which is identity —
    ;; exactly the comparison wanted, and it makes the common case (nothing
    ;; changed order) one list walk rather than N protocol messages.
    (unless (equal order (c:prop *world* :render-signature))
      (setf (c:prop *world* :render-signature) (copy-list order))
      (let ((previous nil))
        (dolist (window order)
          (let ((node (window-river-node window)))
            (when node
              (guarded "render order"
                (if previous
                    (w:node-place-above node previous)
                    (w:node-place-bottom node)))
              (setf previous node))))))))
