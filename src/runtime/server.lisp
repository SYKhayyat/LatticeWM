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

(defvar *restart-on-exit* nil
  "Set by RESTART-WM, read by MAIN once the compositor connection is closed.

A flag rather than a call, because the successor must not exist while we still
hold river's window-manager object: river hands window management to one client
at a time, so a process started from inside the event loop would race the one
starting it and lose.  MAIN spawns it after START has returned, by which point
wl_display_disconnect has run and river has seen us go.")

(defvar *wm-thread* nil
  "The thread that owns the compositor socket, so other threads can wake it.

Declared here, before anything that consults it, because the whole point is
that code all over the runtime can ask \"am I allowed to write to the socket
from here?\" — and the answer must never depend on load order.")

(defvar *unannounced-outputs* '()
  "Monitors that have appeared but have not yet been announced to :OUTPUT-ADDED.

The same deferral as *UNPLACED*, for the same reason and one step further out.
river_output_v1 arrives carrying nothing: the position, the size and the
wl_output that supplies the name and the scale are all separate events, so the
moment the object exists is the moment least is known about it — and a hook
documented \"for anything that needs a surface or a process per screen\" was
being handed a nameless monitor of no size.

Drained in the manage sequence, beside the windows.  An output unplugged before
that happens is taken off this list rather than announced and then removed,
which is a sequence no hook should have to be written against.")

(defvar *unplaced* '()
  "Windows that have appeared but have not yet been placed.

Drained at the start of each manage sequence; see runtime/windows.lisp's header
for why placement is deferred rather than done on arrival.

Declared here rather than there for the same reason *WM-THREAD* is: the
emitter loads first and has to ask \"has anybody decided where this goes yet?\"
before it may hide a window it did not place, and the answer must not depend on
load order.")


;;; ------------------------------------------------- declared event handlers
;;;
;;; A handler for an event that does not exist is invisible to everything.
;;; ATTACH-OUTPUT listened for a :NAME event from river_output_v1 for the whole
;;; life of the project; the interface has no such event, so the clause could
;;; never fire, every output was anonymous, and the per-output workspace memory
;;; was keyed on NIL.  Gate 1 sees a well-formed CASE clause.  Gate 5 counts
;;; codegen against the XML but says nothing about which events we *listen*
;;; for.  The tests pass because they construct state rather than receive it.
;;;
;;; The fix is to make the handler say which interface it is for, so a gate can
;;; check the two against each other.  ON-EVENTS is that, and it costs one line
;;; per handler.

(defvar *handled-events* (make-hash-table :test #'equal)
  "Interface name -> the list of event keywords some handler CASEs on.")

(defun declare-handled-events (interface events)
  "Record that a handler for INTERFACE handles EVENTS.  Gate 8 checks them."
  (setf (gethash interface *handled-events*)
        (union events (gethash interface *handled-events*)))
  interface)

(defun all-handled-events ()
  "Every (INTERFACE . EVENTS) pair, sorted, for the gate."
  (let ((out '()))
    (maphash (lambda (interface events) (push (cons interface events) out))
             *handled-events*)
    (sort out #'string< :key #'car)))

(defmacro on-events ((proxy interface) &body clauses)
  "Attach an event handler to PROXY, declaring it handles INTERFACE's events.

The declaration is derived from the CASE clauses themselves rather than
written beside them, so the two cannot drift -- which is the whole point, as
drift is exactly what a hand-maintained list of handled events would produce."
  ;; LOAD-TIME-VALUE, not a plain call: the macro is used inside function
  ;; bodies, so a plain call would only register when a proxy actually
  ;; attaches -- which never happens in the image a build gate runs in.  The
  ;; gate would then check nothing and say every one of nothing was fine.
  `(progn
     (load-time-value
      (declare-handled-events ,interface
                              ',(remove t (mapcar #'first clauses)))
      t)
     (push (lambda (event &rest arguments)
             (declare (ignorable arguments))
             (with-abandon (case event ,@clauses)))
           (wl:wl-proxy-hooks ,proxy))))

(defclass server ()
  ((display :initarg :display :accessor server-display
            :documentation "The wl_display.")
   (registry :initform nil :accessor server-registry)
   (manager :initform nil :accessor server-manager
            :documentation "river_window_manager_v1 — the whole protocol.")
   (bindings :initform nil :accessor server-bindings
             :documentation "river_xkb_bindings_v1, or NIL if unavailable.")
   (layer-shell :initform nil :accessor server-layer-shell)
   (input-manager :initform nil :accessor server-input-manager
                  :documentation "river_input_manager_v1, or NIL.")
   (libinput-config :initform nil :accessor server-libinput-config
                    :documentation "river_libinput_config_v1, or NIL.")
   (xkb-config :initform nil :accessor server-xkb-config
               :documentation "river_xkb_config_v1, or NIL.")
   (device-index :initform (make-hash-table :test #'eq) :accessor server-device-index
                 :documentation
                 "Any of a device's three proxies -> our INPUT-DEVICE.

One table rather than three because the join is by object identity and the
question is always the same one: whose event is this?  A touchpad arrives as a
river_input_device_v1, a river_libinput_device_v1 and — if it were a keyboard —
a river_xkb_keyboard_v1, on three separate globals in any order, and every one
of them ends up in here pointing at the same device.")
   (inputs-dirty :initform nil :accessor server-inputs-dirty
                 :documentation
                 "Whether some device's configuration needs (re)applying.

Set by every event that could change the answer and drained once per pass of
the event loop, rather than acted on where it is set.  A device announces
itself with a burst of twenty-odd events — its name, its kind, what it
supports, what is currently in force — and configuring it after the first of
them would mean deciding what a touchpad supports before it had said.")
   (wl-outputs :initform (make-hash-table :test #'eql) :accessor server-wl-outputs
               :documentation
               "wl_output global id -> the bound wl_output proxy.

Kept because a tablet or a touchscreen is mapped to an output by handing river
the wl_output itself, and river_output_v1 is not one.")
   (compositor :initform nil :accessor server-compositor
               :documentation "wl_compositor, for surfaces of our own.")
   (shm :initform nil :accessor server-shm
        :documentation "wl_shm, for the pixels we draw ourselves.")
   (pending-manage-work :initform '() :accessor server-pending-manage-work
                        :documentation
                        "Thunks waiting for a manage sequence, newest first.
See DEFER-TO-MANAGE, which is the general case of the pile above it.")
   (seats :initform '() :accessor server-seats
          :documentation "List of SEAT.")
   (windows :initform (make-hash-table :test #'eq) :accessor server-windows
            :documentation "river_window_v1 proxy -> our WINDOW.")
   (nodes :initform (make-hash-table :test #'eq) :accessor server-nodes
          :documentation "our WINDOW -> its river_node_v1.")
   (outputs :initform (make-hash-table :test #'eq) :accessor server-outputs
            :documentation "river_output_v1 proxy -> our OUTPUT.")
   (output-names :initform (make-hash-table :test #'eql)
                 :accessor server-output-names
                 :documentation
                 "wl_output global id -> its name, like \"eDP-1\".

river_output_v1 has no name event -- it has `wl_output', carrying the numeric
id of the wl_output global -- so the human-readable name has to be fetched
from wl_output itself, which has carried one since version 4.  This is the
table those two halves meet in, because the two events race and either can
arrive first.")
   (output-scales :initform (make-hash-table :test #'eql)
                  :accessor server-output-scales
                  :documentation
                  "wl_output global id -> its integer scale factor.

The same join as OUTPUT-NAMES and for the same reason: river_output_v1 reports
position and dimensions in the compositor's *logical* coordinate space and says
nothing at all about scale, so a HiDPI display is indistinguishable from a
low-resolution one as far as the window management protocol is concerned.  That
is right for laying windows out — logical pixels are what a layout wants — and
wrong for everything the window manager draws *itself*, which is drawn in
device pixels into a buffer of its own.

Without this, the echo area on a 2x display was half the size it should have
been, in the one place where a window manager writes text for a human to read.")
   (emitted :initform (make-hash-table :test #'eq) :accessor server-emitted
            :documentation
            "WINDOW to a table of the last value we sent for each property, so
that a relayout only sends what actually changed.  River processes every
request we send before it can answer input, so re-sending a hundred identical
positions on every keystroke is not free.

Two levels rather than one table keyed on the pair, because the pair had to be
consed to *ask* — see EMITTED.")
   (running :initform nil :accessor server-running)
   (dirty :initform t :accessor server-dirty
          :documentation "Whether the layout needs recomputing.")
   (manage-requested :initform nil :accessor server-manage-requested
                     :documentation
                     "Whether a manage_dirty we sent has not yet been answered.

Set by REQUEST-MANAGE and cleared by RUN-MANAGE-SEQUENCE, which is the
:MANAGE-START handler — so it is true exactly over the window between asking
for a sequence and being given one, and a second ask inside that window is a
round trip that buys nothing.  There is no third state: the flag is written
only on the compositor thread.")
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
   (pending-closes :initform '() :accessor server-pending-closes
                   :documentation
                   "Windows asked to close, waiting for the next manage
sequence.  river_window_v1.close is window-management state and a command runs
from a key binding, which is *outside* any sequence -- so closing a window
straight from the verb reached the sequence discipline and was refused, every
time, on every machine.")
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
   (pointer-x :initform 0 :accessor seat-pointer-x)
   (pointer-y :initform 0 :accessor seat-pointer-y)
   (pointer-window :initform nil :accessor seat-pointer-window
                   :documentation
                   "The window the pointer is currently over, or NIL.

River tells us this directly with pointer_enter and pointer_leave, and its
notion of a window's *area* includes the borders it draws and the input regions
of decoration surfaces — none of which our own hit test against the layout
rectangles knew about.  So this is both cheaper and more correct than asking
where the pointer is and looking it up.")
   (focused :initform nil :accessor seat-focused
            :documentation "The window we last gave keyboard focus to.")
   (layer-focus :initform nil :accessor seat-layer-focus
                :documentation
                "Whether a layer surface holds the keyboard: :EXCLUSIVE,
:NON-EXCLUSIVE, or NIL.

:EXCLUSIVE is a screen locker, and the protocol is explicit that every focus
request we make is ignored until it clears.  Sending them anyway is how a
window manager ends up fighting a locker for the keyboard on every manage
sequence — and the window manager loses, quietly, forever.")
   (pointer-op :initform nil :accessor seat-pointer-op
               :documentation
               "The interactive pointer operation in progress, or NIL.

A POINTER-OP struct while a drag is happening: what is being dragged, whether
it is a move or a resize, and where it started.  River sends cumulative deltas
from the start of the operation rather than per-motion deltas, so the *start*
is the thing that has to be remembered.")
   (props :initform '() :accessor c:props))
  (:documentation "One seat: a keyboard, a pointer, and a keyboard focus."))

(defun primary-seat ()
  "The first seat, which on every ordinary machine is the only one."
  (first (server-seats *server*)))

(defun warp-pointer (x y &optional (seat (primary-seat)))
  "Move the pointer to X, Y in output-layout coordinates.

Wraps one protocol request, and exists because the alternative is that an
extension reaches into LATTICEWM/WIRE for it.  That is the seam the package
comment draws — policy and user code talk to the runtime, the runtime talks to
the protocol — and the first worked example that crossed it did so because
this function was missing rather than because the rule was wrong."
  (when seat
    (best-effort "pointer_warp" (w:seat-pointer-warp (seat-proxy seat) x y))))

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
  "Every window we are managing, in no particular order.

A FRESH LIST EVERY TIME, which is right for the callers that hold onto it or
sort it and wrong for the three that walk it and drop it — the two per relayout
and the one per pointer motion.  Those use DO-WINDOWS."
  (when *server*
    (loop for window being the hash-values of (server-windows *server*)
          collect window)))

(defmacro do-windows ((window &optional result) &body body)
  "Run BODY with WINDOW bound to each managed window, then return RESULT.

DOLIST over ALL-WINDOWS with the list left out.  RETURN exits it, as it does a
DOLIST, which is what the pointer hit test wants.

Walking every window is a per-relayout and per-pointer-event act, and the list
ALL-WINDOWS conses to allow it is dropped on the floor by every caller here —
in the file whose neighbour argues that a GC pause during a keystroke is input
latency, directly and visibly."
  (let ((key (gensym "KEY")) (table (gensym "TABLE")))
    `(block nil
       (let ((,table (and *server* (server-windows *server*))))
         (when ,table
           (maphash (lambda (,key ,window)
                      (declare (ignore ,key))
                      (tagbody ,@body))
                    ,table)))
       ,result)))

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

(defun node-rect-now (node)
  "The rectangle NODE was last laid out at, or NIL.

Read from the index the emitter caches on the world, so this is a hash lookup
rather than a layout.  NIL before the first relayout, which every caller has to
tolerate anyway."
  (let ((index (and *world* (c:prop *world* :rect-index))))
    (and index node (gethash node index))))

(defun output-at (x y)
  "The output containing the point (X, Y), or NIL."
  (loop for output in (all-outputs)
        when (c:rect-contains-p (c:output-rect output) x y) return output))

(defun output-for-rect (rect)
  "The output RECT mostly lies on, or NIL when it lies on none.

*Mostly*, by intersection area, rather than `the first one that overlaps'.  A
pane straddling the seam between two monitors overlaps both, and which of them
it is *on* is the one showing more of it — which is also what a person would
say if you asked them."
  (when rect
    (let ((best nil) (best-area 0))
      (dolist (output (all-outputs) best)
        (let ((overlap (c:rect-intersect (c:output-rect output) rect)))
          (when overlap
            (let ((area (* (c:rect-w overlap) (c:rect-h overlap))))
              (when (> area best-area)
                (setf best output best-area area)))))))))

(defun output-showing-workspace (index)
  "The first output whose displayed workspace is INDEX, or NIL."
  (when (integerp index)
    (loop for output in (all-outputs)
          when (eql index (c:prop output :workspace)) return output)))

(defun current-output ()
  "The output the cursor is on, or the best guess available.

Multi-monitor is one model with one viewport per output (PLAN §fiat), so this
is a question about where the cursor is rather than about which tree is active.

FROM THE CURSOR'S *RECTANGLE*, NOT ITS WINDOW, and that distinction was a real
two-monitor bug.  The old version intersected each output with the *cursor's
window's* rect — so when the cursor rested on a deliberately empty pane, which
is an ordinary first-class state in this model and not an edge case, there was
no window, nothing matched, and it fell through to the first output.  On a
two-monitor setup every empty pane was reported as being on monitor 1.  A pane
has a rectangle whether or not anything is in it, which is the whole point of
D17, so asking the pane is both correct and shorter.

Three answers in decreasing order of confidence, because each one can be
unavailable at a different moment:

  1. the pane's own rectangle, once a layout has happened;
  2. the output displaying the cursor's workspace, before one has;
  3. the first output, which is right on every single-monitor machine."
  (let* ((path (current-path))
         (node (current-node path)))
    (or (output-for-rect (node-rect-now node))
        (output-showing-workspace (first path))
        (first (all-outputs)))))

(defun output-of-window (window)
  "The output WINDOW is mostly on, or NIL."
  (and window (output-for-rect (c:window-rect window))))
