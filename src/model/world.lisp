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
   (props :initform '() :accessor props))
  (:documentation "A monitor, as river describes it."))

(defmethod print-object ((o output) stream)
  (print-unreadable-object (o stream :type t :identity nil)
    (format stream "~a ~dx~d+~d+~d" (or (output-name o) "?")
            (rect-w (output-rect o)) (rect-h (output-rect o))
            (rect-x (output-rect o)) (rect-y (output-rect o)))))

;;; ----------------------------------------------------------------- floats

(defclass floating-window ()
  ((window :initarg :window :accessor float-window)
   (rect :initarg :rect :initform (make-rect 0 0 640 480) :accessor float-rect)
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
   (cursor :initarg :cursor :initform '() :accessor world-cursor
           :documentation
           "The focus path (DESIGN D18).  A place, not a window.  Wayland
keyboard focus is derived from it on every relayout and never stored.")
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
   (focused-float :initform nil :accessor world-focused-float
                  :documentation
                  "The floating window that has keyboard focus, or NIL.

Focus is a *place* in the tree (D18), and a float is deliberately not in the
tree — so without this slot there is no way to express \"the keyboard is
talking to the floating window\", and a floated window could never be typed
into.  That is not a corner case: it is every dialog, every file picker, and
the first thing anybody notices.

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

(defmethod print-object ((w world) stream)
  (print-unreadable-object (w stream :type t :identity nil)
    (format stream "~d window~:p, cursor ~s"
            (length (node-windows (world-root w))) (world-cursor w))))

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

(defun world-focus-window (world)
  "The window that should have keyboard focus, or NIL.

A focused float wins over the cursor, because a float is on top and is what the
user is looking at.  Otherwise it is the cursor's window, which may be NIL when
the cursor rests on an empty pane — and NIL is the honest answer there, not a
reason to leave focus on the last window."
  (let ((float (world-focused-float world)))
    (if (and float (window-live-p (float-window float))
             (member float (world-floats world)))
        (float-window float)
        (world-window-at world))))

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
