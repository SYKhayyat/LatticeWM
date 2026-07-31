;;;; runtime/server.lisp --- The live connection, and the global state.
;;;;
;;;; Everything mutable that is not the layout tree lives here, in one object,
;;;; for the same reason the WORLD is one object: so that it can be inspected
;;;; from a REPL as a value rather than reconstructed from a dozen specials.

(in-package #:latticewm/runtime)

(defvar *world* nil
  "The WORLD being managed.  See LATTICEWM/CORE:WORLD.

At a REPL this is the thing to look at:

    (c:world-cursor *world*)
    (c:node-windows (c:world-root *world*))")

(defvar *server* nil
  "The live SERVER, or NIL when not connected.")

(defvar *wm-thread* nil
  "The thread that owns the compositor socket, so other threads can wake it.

Declared here, before anything that consults it, because the whole point is
that code all over the runtime can ask \"am I allowed to write to the socket
from here?\" — and the answer must never depend on load order.")

(defclass server ()
  ((display :initarg :display :accessor server-display
            :documentation "The wl_display.")
   (registry :initform nil :accessor server-registry)
   (manager :initform nil :accessor server-manager
            :documentation "river_window_manager_v1 — the whole protocol.")
   (bindings :initform nil :accessor server-bindings
             :documentation "river_xkb_bindings_v1, or NIL if unavailable.")
   (layer-shell :initform nil :accessor server-layer-shell)
   (seats :initform '() :accessor server-seats
          :documentation "List of SEAT.")
   (windows :initform (make-hash-table :test #'eq) :accessor server-windows
            :documentation "river_window_v1 proxy -> our WINDOW.")
   (nodes :initform (make-hash-table :test #'eq) :accessor server-nodes
          :documentation "our WINDOW -> its river_node_v1.")
   (outputs :initform (make-hash-table :test #'eq) :accessor server-outputs
            :documentation "river_output_v1 proxy -> our OUTPUT.")
   (emitted :initform (make-hash-table :test #'equal) :accessor server-emitted
            :documentation
            "The last value we sent for each (WINDOW . PROPERTY), so that a
relayout only sends what actually changed.  River processes every request we
send before it can answer input, so re-sending a hundred identical positions on
every keystroke is not free.")
   (running :initform nil :accessor server-running)
   (dirty :initform t :accessor server-dirty
          :documentation "Whether the layout needs recomputing.")
   (bindings-dirty :initform nil :accessor server-bindings-dirty
                   :documentation
                   "Whether keybindings need (re)registering at the next manage
sequence.  Enabling a binding is window-management state, so it cannot happen
when the keymap changes — only when river next lets us.")
   (pending-dimensions :initform '() :accessor server-pending-dimensions
                       :documentation
                       "Window-management work computed during layout and
waiting for the next manage sequence, since propose_dimensions is illegal in a
render sequence.")
   (version :initform 0 :accessor server-version
            :documentation "The river_window_manager_v1 version river offered."))
  (:documentation
   "The compositor connection and everything derived from it."))

(defclass seat ()
  ((proxy :initarg :proxy :accessor seat-proxy)
   (bindings-seat :initform nil :accessor seat-bindings-seat
                  :documentation "river_xkb_bindings_seat_v1.")
   (bound-keys :initform (make-hash-table :test #'equal) :accessor seat-bound-keys
               :documentation "(KEYSYM . MODIFIERS) -> river_xkb_binding_v1.")
   (modifiers :initform 0 :accessor seat-modifiers)
   (pointer-x :initform 0 :accessor seat-pointer-x)
   (pointer-y :initform 0 :accessor seat-pointer-y)
   (focused :initform nil :accessor seat-focused
            :documentation "The window we last gave keyboard focus to.")
   (props :initform '() :accessor c:props))
  (:documentation "One seat: a keyboard, a pointer, and a keyboard focus."))

(defun primary-seat ()
  "The first seat, which on every ordinary machine is the only one."
  (first (server-seats *server*)))

;;; ------------------------------------------------------------ registries

(defun window-of-proxy (proxy)
  "Our WINDOW for a river_window_v1 PROXY, or NIL.

NIL is an ordinary answer, not an error: events for a window can still arrive
after we have forgotten it."
  (and *server* (gethash proxy (server-windows *server*))))

(defun window-river-node (window)
  "The river_node_v1 for WINDOW, created on demand.

River gives every window exactly one node and errors with `node_exists' if you
ask twice, so this must be the only place that asks."
  (let ((existing (gethash window (server-nodes *server*))))
    (or existing
        (when (c:window-proxy window)
          (setf (gethash window (server-nodes *server*))
                (w:window-get-node (c:window-proxy window)))))))

(defun all-windows ()
  "Every window we are managing, in no particular order."
  (when *server*
    (loop for window being the hash-values of (server-windows *server*)
          collect window)))

(defun all-outputs ()
  "Every output, in the order river reported them."
  (if *world* (c:world-outputs *world*) '()))

;;; --------------------------------------------------------- the cursor's view

(defun current-path ()
  "The cursor's path."
  (and *world* (c:world-cursor *world*)))

(defun current-node (&optional (path (current-path)))
  "The node the cursor is on."
  (and *world* (c:resolve-path (c:world-root *world*) path)))

(defun current-leaf (&optional (path (current-path)))
  "The leaf the cursor is on, or NIL."
  (and *world* (c:world-leaf-at *world* path)))

(defun current-window (&optional (path (current-path)))
  "The window the cursor is on, or NIL — including when the pane is
deliberately empty, which is an ordinary state and not an error."
  (and *world* (c:world-window-at *world* path)))

(defun focused-window ()
  "The window that currently has keyboard focus, float or tiled.

Distinct from CURRENT-WINDOW, which is *the cursor's* window.  They differ
exactly when a floating window is focused, and every command that acts on \"the
window\" — close, minimize, fullscreen, float — means this one.  Getting that
wrong is not subtle in use: you press close, and the window behind the dialog
you were looking at disappears."
  (and *world* (c:world-focus-window *world*)))

(defun current-output ()
  "The output the cursor is on, or the first one.

Multi-monitor is one model with one viewport per output (PLAN §fiat), so this
is a question about where the cursor is rather than about which tree is
active."
  (or (loop for output in (all-outputs)
            for rect = (c:output-rect output)
            for window = (current-window)
            for wr = (and window (c:window-rect window))
            when (and wr (c:rect-intersect rect wr)) return output)
      (first (all-outputs))))
