;;;; model/world.lisp --- The whole managed state, in one object.
;;;;
;;;; A WORLD is the layout tree, the cursor, the outputs, the floats and the
;;;; scratchpad.  It is a single object rather than a pile of globals so that
;;;; it can be copied for a test, serialised for persistence, and — the reason
;;;; that actually matters — so that a REPL can hold two of them and compare.
;;;;
;;;; The shipped shape of the tree is
;;;;
;;;;     world root  =  STACK                 ; the workspace list
;;;;                    ├── workspace 0       ; a SPLIT, a LEAF, or (with the
;;;;                    ├── workspace 1       ;   lattice loaded) a GRID
;;;;                    └── workspace 2
;;;;
;;;; and nothing depends on that shape.  Workspaces are a stack because a stack
;;;; is "an ordered set of alternatives of which one is current", which is what
;;;; a workspace list is; making them the same object means every verb that
;;;; moves a subtree into a container already moves a window to a workspace.
;;;; "Infinite workspaces of lattices one behind another" is this stack with
;;;; grids in it.
;;;;
;;;; The stack is infinite for free — a stack grows, so workspace 40 on a
;;;; machine with three makes forty.  What is *in* it is a policy decision and
;;;; is asked of P:MAKE-WORKSPACE, at every one of the four sites that grow the
;;;; list.  Nothing here may build a workspace by hand: the sentence above is
;;;; only true while the grids keep arriving, and they arrive from there.

(in-package #:latticewm/core)

;;; ---------------------------------------------------------------- outputs

(defclass output ()
  ((proxy :initarg :proxy :initform nil :accessor output-proxy)
   (name :initarg :name :initform nil :accessor output-name)
   (rect :initarg :rect :initform (make-rect 0 0 1920 1080) :accessor output-rect
         :documentation
         "Position and size in the compositor's logical coordinate space.
River reports both, so a multi-monitor arrangement is described entirely in
one space and needs no per-output translation.")
   (scale :initarg :scale :initform 1 :accessor output-scale)
   (capture-sessions :initform nil :accessor output-capture-sessions
                     :documentation
                     "How many screen capture sessions are recording this
whole monitor, or NIL for `river has not said'.  The same three-valued rule as
WINDOW-CAPTURE-SESSIONS, for the same reason; OUTPUT-CAPTURED-P is what to
read.")
   (props :initform '() :accessor props))
  (:documentation "A monitor, as river describes it."))

(defun output-captured-p (output)
  "Is something recording this whole monitor right now?"
  (let ((count (output-capture-sessions output)))
    (and count (plusp count))))

;;; --------------------------------------------------------- window state
;;;
;;; #25.  "What state is this window in?" had five answers in five places: three
;;; booleans on the window, tree membership in the world, and the scratchpad.
;;; The query is now one function, and what it returns is derived rather than
;;; stored -- which is the finding, not a shortcut.

(defun window-state (world window)
  "How WINDOW is currently shown, as a list of keywords, least surprising first.

The vocabulary, and why each entry exists:

  (:DEAD)                    river has told us the window is gone.
  (:MINIMIZED ...)           out of the layout and out of sight; the
                             scratchpad holds it.  May be followed by
                             :FLOATING, because a floated window that is put
                             away comes back floating -- the membership is
                             kept for exactly that reason.
  (:TILED)                   a leaf of the tree: the layout places it.
  (:FLOATING)                on the world's float list, outside the tree.
  (:UNPLACED)                alive, in no tree, not floating, not minimized.
                             THIS ONE SHOULD NOT EXIST, and naming it is half
                             the reason to have this function: it is the state
                             #6 calls invisible, it is what a window is between
                             `river says it exists' and `somebody placed it.',
                             and until now nothing could say whether a window
                             was in it.  A caller that finds it has found a
                             bug, not a state.
  ... :FULLSCREEN             appended to any of the above but :DEAD.

*FULLSCREEN IS ORTHOGONAL TO TILED AND FLOATING*, and that is the second thing
this function records.  River renders a fullscreen window alone on its output
and ignores the layout, so a fullscreen window is still either a leaf of the
tree or a float underneath -- minimizing it and restoring it, or unfloating it,
has a correct answer that depends on which.  An enum with one slot per state
would have to throw that away, which is the reason the model does not have one.

*MINIMIZED IS EXCLUSIVE*, and the difference between the two rules is the whole
design: a window cannot be minimized and tiled at once (it is not in the tree),
but it can be minimized and floating, because putting a float away and getting
it back should not turn it into a tile."
  (cond
    ((not (window-live-p window)) '(:dead))
    ((window-minimized-p window)
     (append '(:minimized)
             (when (window-floating-p window) '(:floating))
             (when (window-fullscreen-p window) '(:fullscreen))))
    (t
     (append
      (cond
        ((window-floating-p window) '(:floating))
        ((leaf-holding (world-root world) window) '(:tiled))
        (t '(:unplaced)))
      (when (window-fullscreen-p window) '(:fullscreen))))))

(defun window-state-name (state)
  "STATE as one readable string, for an inspector to print."
  (if (null state)
      "unknown"
      (format nil "~{~(~a~)~^ ~}" state)))

;;; ------------------------------------------------------ shared outputs
;;;
;;; #96: one plane across several monitors.  The word "share" needs a
;;; definition before the code, because the obvious reading is not
;;; expressible: a Wayland surface is one window with one geometry, so two
;;; outputs cannot display *the same window* twice -- that would need two
;;; surfaces for one client.  What a window manager CAN do is present one
;;; workspace across the union of the monitors in a group, so a cell, a split
;;; or a window straddles the seam.  That is what SHARE-WORKSPACE makes, and
;;; MIRROR (the other half of #96) is filed separately rather than faked: see
;;; the issue for the two candidate readings and why neither is this.

(defun output-share-group (output)
  "The outputs OUTPUT shares one workspace with, leader first, or NIL.

The list is the *group*, stored on every member so that no table has to be
kept in step with the outputs themselves: an output that is detached takes its
answer with it, and a group whose leader is gone is repaired by the member
that is still present.  Every member holds the same list, and identity -- not
equality -- is what decides membership, because two outputs are not
interchangeable just because they describe the same rectangle."
  (prop output :share-group))

(defun (setf output-share-group) (group output)
  (setf (prop output :share-group) group))

(defun output-share-rect (output)
  "The rectangle OUTPUT presents, which is its group's union when it is shared.

NIL for a follower: a follower draws nothing, because the leader laid the
shared plane out over the group's whole area.  The two facts are kept apart on
purpose -- GROUP says `we are sharing', RECT says `this is the area I draw' --
so that a policy can tell `not shared' from `shared and not the one drawing'."
  (prop output :share-rect))

(defun (setf output-share-rect) (rect output)
  (setf (prop output :share-rect) rect))

(defun share-follower-p (output)
  "Is OUTPUT in a shared group but not the member drawing it?"
  (let ((group (output-share-group output)))
    (and group (not (eq output (first group))) t)))

(defun share-siblings-p (a b)
  "Do A and B belong to one shared group?  NIL when neither is shared."
  (let ((group (output-share-group a)))
    (and group (eq group (output-share-group b)) t)))

(defun refresh-share-rects (world)
  "Recompute every group's rectangle from where its monitors actually are.

Called before each layout, because an output can be moved, resized or
unplugged between frames and the union is a fact about the *current*
arrangement.  Cheap: a pass over the outputs and a reduction per group, which
is the same order as the workspace invariant RELAYOUT already asserts."
  (dolist (output (world-outputs world) (world-outputs world))
    (let ((group (output-share-group output)))
      (setf (output-share-rect output)
            (and group
                 (eq output (first group))
                 (rect-union (mapcar (lambda (member)
                                       (output-rect member))
                                     group)))))))

(defmethod print-object ((o output) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (format stream "~a ~dx~d+~d+~d" (or (output-name o) "?")
            (rect-w (output-rect o)) (rect-h (output-rect o))
            (rect-x (output-rect o)) (rect-y (output-rect o)))))

;;; ----------------------------------------------------------------- floats

(defparameter *default-float-width* 640
  "The width a floating window gets when nothing better is known.

Nothing better is known in exactly two places: a FLOATING-WINDOW constructed
without a rect -- a test, or a policy that answered NIL -- and a session with
no output to centre a float on.  Both used to spell 640x480 inline, in two
different files and with two different origins.

This lives in the model rather than beside `*float-fraction*' in the policy
because it is the model's answer to the question 'what is a rect at all', and
the first of those two places has no policy to ask.  It is emphatically *not*
the answer to 'how big should a float be': that is the policy's
DEFAULT-FLOAT-RECT, which takes a fraction of the output and asks the window
first.")

(defparameter *default-float-height* 480
  "The height that goes with *DEFAULT-FLOAT-WIDTH*.  See it for the argument.")

(defun make-default-float-rect (&optional (x 0) (y 0))
  "A fresh default float rectangle at X,Y.

Named MAKE- and not DEFAULT- because POLICY already has a generic called
DEFAULT-FLOAT-RECT (the one that asks the output), and two names that differ
only by package would collide in the very package extensions are read in.

Fresh, not a shared object: callers mutate the rect they were handed (a move,
a resize), and one shared rectangle would make every default-sized float on
screen move together."
  (make-rect x y *default-float-width* *default-float-height*))

(defclass floating-window ()
  ((window :initarg :window :accessor float-window)
   (rect :initarg :rect :initform (make-default-float-rect) :accessor float-rect)
   (anchor :initarg :anchor :initform nil :accessor float-anchor
           :documentation
           "The node this float is pinned to, or NIL for one pinned to the
output.  An anchored float travels with its node — it moves when the node
moves, hides when the node hides, and is clipped by the node's clip box.  That
is what \"a floating window inside a window\" means here, and it costs one
slot.")
   (node :initform nil :accessor float-node
         :documentation
         "The LEAF that stands for this float when a policy is asked about it.

A floating window is deliberately not in the tree, and every appearance generic
— BORDER-COLOR, BORDER-WIDTH, CLIP-RECT — takes a *node*.  So a float needs one
to be asked about, and it has to be the *same* node every time: the emitter used
to make a throwaway leaf per relayout, which meant the float silently had no
identity at all.  It could not carry a PROP, could not be compared, and could
not be the thing a window rule hung a colour on.

Made on demand by RUNTIME:FLOAT-LEAF and kept for the life of the float.")
   (props :initform '() :accessor props))
  (:documentation
   "A window that is positioned by hand rather than tiled.

Floating is per window and chosen, never inferred and forced: the requirement
is that windows can be *picked out* to float rather than floating being a
property the window manager decides for you.  SHOULD-FLOAT-P only supplies the
initial guess."))

;;; ------------------------------------------------------------------ world

(defclass world ()
  ((root :initarg :root :accessor world-root
         :documentation "The layout tree.  Shipped as a STACK of workspaces.")
   (cursors :initform (make-hash-table :test #'eq) :accessor world-cursors
            :documentation
            "SEAT KEY -> focus path (DESIGN D18).  A place, not a window.

DESIGN says *one cursor per seat*, and this used to be one slot: a second
keyboard is a second person, at a second place, and one slot meant the second
one could only stand where the first was standing.  The key is a seat object,
or NIL for the world's own cursor — the one every caller that does not name a
seat gets, which is the default seat and, on the one-seat machines this is
normally run on, the only one.  NIL is the default rather than an absence so
that every existing caller keeps meaning exactly what it meant.")
   (outputs :initform '() :accessor world-outputs
            :documentation "List of OUTPUT, in the order river reported them.")
   (inputs :initform '() :accessor world-inputs
           :documentation
           "List of INPUT-DEVICE, in the order river reported them.

Here rather than on the server for the same reason the outputs are: policy has
to be able to ask what is plugged in — a rule that applies to every touchpad
has to be able to find the touchpads — and policy may not depend on the
runtime.")
   (floats :initform '() :accessor world-floats
           :documentation "List of FLOATING-WINDOW, bottom to top.")
   (focused-floats :initform (make-hash-table :test #'eq)
                   :accessor world-focused-floats
                   :documentation
                   "SEAT KEY -> the floating window that has that keyboard, or absent.

The same per-seat rule as CURSORS, for the same reason: two people at one
machine can each be in a dialog, and one slot could only say that both were in
the same one.  Focus is a *place* in the tree (D18), and a float is
deliberately not in the tree — so without a float slot there is no way to
express \"the keyboard is talking to the floating window\", and a floated window
could never be typed into.  That is not a corner case: it is every dialog,
every file picker, and the first thing anybody notices.

The cursor keeps pointing wherever it was, so dismissing the float returns you
exactly where you were without anything having to remember it.")
   (scratchpad :initform '() :accessor world-scratchpad
               :documentation
               "Minimized windows, most recent first.  They are genuinely out
of the tree — the remaining windows retile without them — which is the stated
requirement: minimize is not \"hide it somewhere\", it is \"take it out of the
layout\".")
   (props :initform '() :accessor props))
  (:documentation
   "Everything the window manager is managing."))

(defun make-world (&key root (cursor nil cursor-supplied))
  "A world holding ROOT, defaulting to a single empty workspace.

The default is deliberately the smallest thing that is already a usable window
manager: one workspace holding one empty pane.  Starting up puts the cursor in
that pane, where — per DESIGN D19 — typing a key spawns something."
  (let* ((root (or root (make-stack (list (make-leaf)) 0)))
         (world (make-instance 'world :root root)))
    (setf (world-cursor world)
          (if cursor-supplied cursor (first-leaf-path root)))
    world))

;;; ------------------------------------------------------------ the seat's place
;;;
;;; THE ACCESSORS ARE FUNCTIONS WITH AN OPTIONAL SEAT, and that is what made
;;; this change small.  `(world-cursor world)' still means what it always meant
;;; — the world's own cursor, which belongs to the default seat — so the
;;; emitter, the history, the saved state and every extension compiled against
;;; the old slot keep working unchanged, and single-seat behaviour is identical
;;; by construction rather than by testing.  Naming a seat gets that seat's
;;; place instead.
;;;
;;; A setf place rather than a plain reader, because `(setf (world-cursor world
;;; seat) path)' is how the policy moves a *particular* person: SETF passes the
;;; extra arguments through to the function below, so the form reads like the
;;; slot it replaced.

(defun world-cursor (world &optional seat)
  "The focus path of SEAT, or the world's own cursor when SEAT is NIL (DESIGN D18)."
  (values (gethash seat (world-cursors world) '())))

(defun (setf world-cursor) (path world &optional seat)
  "Put SEAT's cursor at PATH."
  (setf (gethash seat (world-cursors world)) path))

(defun world-focused-float (world &optional seat)
  "The floating window holding SEAT's keyboard, or NIL."
  (values (gethash seat (world-focused-floats world))))

(defun (setf world-focused-float) (float world &optional seat)
  "Give SEAT's keyboard to FLOAT, or to nothing when FLOAT is NIL."
  (let ((table (world-focused-floats world)))
    (if float
        (setf (gethash seat table) float)
        (remhash seat table))))

(defun world-forget-seat (world seat)
  "Drop every per-seat fact about SEAT: its place and its focused float.

For a seat that has gone away.  Left behind, a departed seat's cursor sits in
the table for ever, and a seat object that is garbage is a hash key nothing can
reach — two small leaks the removal path used to have no reason to care about."
  (remhash seat (world-cursors world))
  (remhash seat (world-focused-floats world))
  nil)

(defun world-window-count (world)
  "Every window WORLD holds a place for, counted once.

TILED, PUT AWAY, OR FLOATING -- the question a person reads a printed world to
answer.  Counting the tree alone said a world of two tiled windows and a
floating dialog held two windows, which is the same under-report the deck (#138)
and the map (#139) had, in the place most likely to be read: the object printed
in a REPL or a log.

The same answer as the runtime's WORLD-WINDOWS, and deliberately a separate
function rather than a call to it: the model may not depend on the runtime, and
this is the model's own fact about itself."
  (let ((seen (make-hash-table :test #'eq))
        (count 0))
    (flet ((note (window)
             (when (and window (not (gethash window seen)))
               (setf (gethash window seen) t)
               (incf count))))
      (dolist (window (node-windows (world-root world))) (note window))
      (dolist (window (world-scratchpad world)) (note window))
      (dolist (float (world-floats world)) (note (float-window float))))
    count))

(defmethod print-object ((w world) stream)
  (print-unreadable-object (w stream :type t :identity nil)
    (format stream "~d window~:p, cursor ~s"
            (world-window-count w) (world-cursor w))))

;;; ------------------------------------------------------- world accessors

(defun world-node-at (world &optional (path (world-cursor world)))
  "The node at PATH, defaulting to the cursor's."
  (resolve-path (world-root world) path))

(defun world-leaf-at (world &optional (path (world-cursor world)))
  "The leaf at PATH, or NIL when PATH does not name a leaf."
  (let ((node (world-node-at world path)))
    (when (typep node 'leaf) node)))

(defun world-window-at (world &optional (path (world-cursor world)))
  "The window held at PATH, or NIL — including when the pane is deliberately
empty, which is an ordinary state and not an error."
  (let ((leaf (world-leaf-at world path)))
    (when leaf (leaf-window leaf))))

(defun world-focus-window (world &optional seat)
  "The window that should have SEAT's keyboard, or NIL.

A focused float wins over the cursor, because a float is on top and is what the
user is looking at.  Otherwise it is the cursor's window, which may be NIL when
the cursor rests on an empty pane — and NIL is the honest answer there, not a
reason to leave focus on the last window."
  (let ((float (world-focused-float world seat)))
    (if (and float (window-live-p (float-window float))
             (member float (world-floats world)))
        (float-window float)
        (world-window-at world (world-cursor world seat)))))

(defun world-workspaces (world)
  "The workspace container, if the root is one, else NIL.

Nothing in the core requires the root to be a stack of workspaces; this is a
convenience for the commands that assume the shipped shape, and every one of
them tolerates NIL.

The test is CONTAINER-ALTERNATIVES-P rather than (TYPEP ROOT 'STACK), because
what makes something a workspace list is that it holds alternatives of which
one is current — not that it is the particular class the core ships for that.
An extension whose root container answers the alternatives protocol gets the
whole workspace vocabulary for free, which is the point of having one."
  (let ((root (world-root world)))
    (when (and (container-p root) (container-alternatives-p root)) root)))

(defun workspace-path (world)
  "The path of the current workspace — the first element of the cursor, as a
one-element path — or the empty path when the root is not a workspace stack."
  (let ((workspaces (world-workspaces world)))
    (if workspaces
        (list (or (first (world-cursor world))
                  (container-selection workspaces)))
        '())))

(defun current-workspace (world)
  "The node of the workspace the cursor is in, or the root."
  (or (resolve-path (world-root world) (workspace-path world))
      (world-root world)))

(defun float-of-window (world window)
  "The FLOATING-WINDOW record for WINDOW, or NIL."
  (find window (world-floats world) :key #'float-window))

(defun world-captures (world)
  "Everything river says is being recorded right now: OUTPUTs first, then
WINDOWs, each with at least one screen capture session.

*Every* window the model holds, not the ones on screen.  A window keeps being
recorded while it is minimized, on another workspace, or in a lattice cell four
screens away — that is the whole point of a per-window capture session — so a
listing that walked only the tree would go quiet at exactly the moment the
answer stops being obvious from looking at the screen."
  (append (remove-if-not #'output-captured-p (world-outputs world))
          (remove-duplicates
           (remove-if-not #'window-captured-p
                          (append (node-windows (world-root world))
                                  (mapcar #'float-window (world-floats world))
                                  (world-scratchpad world)))
           :from-end t)))
