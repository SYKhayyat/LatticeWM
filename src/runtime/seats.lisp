;;;; runtime/seats.lisp --- Seats: keyboards, bindings, and reading a key.
;;;;
;;;; A seat is a keyboard, a pointer and a keyboard focus.  On every ordinary
;;;; machine there is exactly one, and the code is written for several anyway
;;;; because the protocol is and the cost is a LOOP.
;;;;
;;;; RIVER DOES THE XKB WORK AND WE DO NOT.  river_seat_v1 has no keyboard
;;;; binding mechanism at all; the separate river-xkb-bindings-v1 protocol
;;;; takes a keysym and a modifier bitfield and sends `pressed'.  So there is
;;;; no libxkbcommon here, no keymap file descriptor, no xkb state machine and
;;;; no wl_keyboard plumbing — an entire subsystem every other window manager
;;;; has to carry, deleted by a protocol decision somebody else made well.
;;;;
;;;; WHAT REMAINS IS HARDER THAN IT LOOKS: *reading text*.  River delivers keys
;;;; to the focused window and gives the window manager only what it asked for,
;;;; so reading a line means binding every key you might read and enabling
;;;; those bindings for exactly as long as you are reading.  Three things ever
;;;; want that — a prompt, an empty pane, and a half-entered chord — and they
;;;; share one mechanism because they are one question: *should the next
;;;; keypress belong to the window manager rather than to a window?*  Having
;;;; three answers to one question is how they come to disagree.

(in-package #:latticewm/runtime)


(defun attach-seat (proxy)
  "Register a seat, and give it its keyboard bindings."
  (let ((seat (make-instance 'seat :proxy proxy)))
    (push seat (server-seats *server*))
    (on-events (proxy "river_seat_v1")
                (:pointer-position
                 (setf (seat-pointer-x seat) (first arguments)
                       (seat-pointer-y seat) (second arguments))
                 ;; The spec is explicit that a pointer move alone must not
                 ;; start a manage sequence, so focus-follows-mouse cannot be
                 ;; driven from here without asking for one.
                 (when p:*focus-follows-mouse* (pointer-moved seat)))
                ;; River tells us which window the pointer is over, including
                ;; the borders it draws and the input regions of decoration
                ;; surfaces -- none of which a hit test against our own layout
                ;; rectangles knows about.  So this is both cheaper and more
                ;; correct than the geometry we were doing ourselves.
                (:pointer-enter
                 (on-pointer-enter seat (window-of-proxy (first arguments))))
                (:pointer-leave (on-pointer-leave seat))
                ;; Not a button event: river's own rationale is that a window
                ;; manager needs to know when to focus and when to raise, and
                ;; that exposing every pointer event to answer it would be
                ;; mechanism where policy belongs.  So this one clause is
                ;; click-to-focus for the mouse, for touch and for a stylus.
                (:window-interaction
                 (on-window-interaction seat (window-of-proxy (first arguments))))
                ;; Cumulative motion since the drag began -- see POINTER-OP.
                (:op-delta
                 (apply-pointer-delta seat (first arguments) (second arguments)))
                (:op-release (end-pointer-op seat))
                ;; There was a (:MODIFIERS ...) clause here.  river_seat_v1
                ;; has no such event -- the real one is modifiers_update on
                ;; river_xkb_bindings_seat_v1, and it needs a modifiers_watch
                ;; request to opt in, which nothing sends.  So it never fired,
                ;; SEAT-MODIFIERS was never written, and nothing read it
                ;; either.  Removed rather than wired up: inventing the feature
                ;; would be answering a gate instead of listening to it.
                ;;
                ;; A SEAT CAN GO AWAY, and until this clause existed nothing
                ;; noticed.  The dead SEAT stayed on SERVER-SEATS, PRIMARY-SEAT
                ;; kept returning it, and every focus request, pointer warp and
                ;; keybinding enable after that went to a destroyed object --
                ;; which is not a silent failure but a *protocol error*, and a
                ;; protocol error is the connection closing.  One seat is the
                ;; normal case; several is what a multi-seat machine and every
                ;; remote-desktop client create and destroy at will.
                (:removed (detach-seat seat proxy))
                (t nil))
    (attach-layer-shell-seat seat)
    (when (server-bindings *server*)
      (setf (seat-bindings-seat seat)
            (w:bindings-get-seat (server-bindings *server*) proxy))
      (on-events ((seat-bindings-seat seat) "river_xkb_bindings_seat_v1")
        (:ate-unbound-key (handle-unbound-key (first arguments)))
        (t nil))
      ;; Registration itself is fine here, but ENABLE is window-management
      ;; state and is therefore manage-sequence-only.  A seat arrives during
      ;; the initial roundtrip, when no sequence is in progress, so the work is
      ;; deferred to the first manage sequence.
      ;;
      ;; This is the sequence discipline doing exactly what it was built for:
      ;; it caught the violation at the point of use, before any bytes reached
      ;; the wire, instead of letting river kill the connection with
      ;; sequence_order and leaving a hang to debug.
      (setf (server-bindings-dirty *server*) t))
    ;; Pointer bindings are on river_seat_v1 itself rather than on the xkb
    ;; bindings protocol, so they exist even where keybindings do not.
    (setf (server-bindings-dirty *server*) t)
    (logmsg :info "seat appeared")
    seat))

(defun detach-seat (seat proxy)
  "A seat went away.  Take it out of everything and release what it held.

The bindings are the part worth naming: every key in the keymap and every one
of the two-hundred-odd capture keys is a river_xkb_binding_v1 belonging to this
seat, and a seat that comes and goes without them being destroyed leaks a few
hundred objects per cycle on the compositor's side as well as ours.

The protocol asks us to destroy the object after `removed', which is what frees
its half."
  (loop for binding being the hash-values of (seat-bound-keys seat)
        do (best-effort "binding destroy" (river:river-xkb-binding-v1.destroy binding)))
  (clrhash (seat-bound-keys seat))
  (loop for (nil . binding) in (c:prop seat :capture-bindings)
        do (best-effort "capture binding destroy"
             (river:river-xkb-binding-v1.destroy binding)))
  (setf (c:prop seat :capture-bindings) nil
        (c:prop seat :capture-armed) nil
        ;; The remembered answer goes with the bindings it produced.  Leaving
        ;; it behind would mean ENSURE-CAPTURE-BINDINGS saw `the policy says
        ;; what it said last time' on a seat that now has no bindings at all,
        ;; and skipped making any -- a seat that came back with a dead prompt.
        (c:prop seat :capture-wanted) nil)
  (let ((layer (c:prop seat :layer-shell)))
    (when layer
      (best-effort "layer shell seat destroy"
        (river:river-layer-shell-seat-v1.destroy layer))
      (setf (c:prop seat :layer-shell) nil)))
  (let ((bindings-seat (seat-bindings-seat seat)))
    (when bindings-seat
      (best-effort "bindings seat destroy"
        (river:river-xkb-bindings-seat-v1.destroy bindings-seat))
      (setf (seat-bindings-seat seat) nil)))
  (setf (server-seats *server*) (remove seat (server-seats *server*)))
  (best-effort "seat destroy" (river:river-seat-v1.destroy proxy))
  (setf (seat-proxy seat) nil)
  ;; Whatever is left needs its bindings re-established, because they were
  ;; per seat and this was not necessarily the only one.
  (setf (server-bindings-dirty *server*) t)
  (logmsg :info "seat removed")
  nil)

(defun pointer-moved (seat)
  "Focus follows the pointer, if that is turned on."
  (let* ((policy (p:current-policy))
         (path (guarded "pointer-focus"
                 (p:pointer-focus policy *world*
                                  (seat-pointer-x seat) (seat-pointer-y seat)))))
    (when (and path (not (c:path-equal path (current-path))))
      (p:jump-cursor policy *world* path)
      (mark-dirty))))

;;; --------------------------------------------------------- keybindings

(defun register-bindings (seat)
  "Ask river for every key the keymap binds, including chord prefixes.

Must be called inside a manage sequence: river_xkb_binding_v1.enable is
window-management state."
  (let ((bindings (server-bindings *server*)))
    (unless bindings (return-from register-bindings nil))
    (dolist (entry (p:bindable-keys))
      (let ((key (car entry)))
        (unless (gethash key (seat-bound-keys seat))
          (let ((binding (best-effort "get_xkb_binding"
                           (w:bindings-get-xkb-binding
                            bindings (seat-proxy seat) (car key) (cdr key)))))
            (when binding
              (setf (gethash key (seat-bound-keys seat)) binding)
              (push (let ((key key))
                      (progn
                        (load-time-value
                         (declare-handled-events "river_xkb_binding_v1"
                                                 '(:pressed))
                         t)
                        (lambda (event &rest arguments)
                          (declare (ignore arguments))
                          (with-abandon
                            (case event
                              (:pressed (when (handle-key key) (after-command)))
                              (t nil))))))
                    (wl:wl-proxy-hooks binding))
              (best-effort "enable binding" (w:binding-enable binding)))))))
    (logmsg :info "~d key~:p bound" (hash-table-count (seat-bound-keys seat)))))

(defun rebind-keys ()
  "Re-register bindings after the keymap changed at a REPL.

This is what makes a keymap edit take effect without a restart, and it is why
DEFINE-KEY does not need to know about the compositor.  The work is deferred
to the next manage sequence, because that is the only place it is legal — so
this is safe to call from anywhere, including a SWANK thread."
  (when *server*
    (setf (server-bindings-dirty *server*) t)
    (request-manage)))

(defun cursor-on-empty-pane-p ()
  "True when the cursor rests on a deliberately empty pane and no float has
the keyboard."
  (let ((leaf (current-leaf)))
    (and leaf (c:leaf-empty-p leaf) (null (c:world-focused-float *world*)))))

;;; +CAPTURE-KEYS+ WAS HERE, AS A DEFPARAMETER, AND THAT WAS THE BUG.
;;;
;;; It listed every key the window manager may ever read directly, which — since
;;; river delivers keys to the focused *window* and hands us only what we asked
;;; for — is the whole of what a prompt, an empty pane or the second key of a
;;; chord can ever see.  Fixed at compile time, in the runtime, in a program
;;; whose premise is that this class of thing is a decision.  A modal editing
;;; layer could bind a function key in a keymap and simply never receive it.
;;;
;;; It is P:CAPTURE-KEYS now: a policy generic whose default method is the same
;;; list, in src/policy/input.lisp.  What is left here is the part that is
;;; genuinely the runtime's — turning an answer into river_xkb_binding_v1
;;; objects, once, in a manage sequence, without churning the ones that already
;;; exist.

(defun capture-key-list ()
  "What the policy says the window manager may read, as (KEYSYM . MODIFIERS).

An empty answer — which is what GUARDED yields when a method signals — adds
nothing and removes nothing, so a policy that breaks after startup leaves the
prompt exactly as readable as it was.  A policy that breaks *before* the first
manage sequence gets no capture keys and a logged error, which is the same
outcome the old DEFPARAMETER would have had if the file failed to load."
  (or (guarded "capture-keys" (p:capture-keys (p:current-policy))) '()))

(defun capture-wanted-p ()
  "Is anything currently waiting to read a key directly?

Two things ever are: a prompt in the echo area, and DESIGN D19's empty pane.
*Three* things are, since a submap joined them.  They share the machinery
because they are the same question — *should the next keypress belong to the
window manager rather than to a window?* — and having three answers to one
question is how they end up disagreeing.

The submap was the one that got this wrong.  Its keys were registered with
river permanently instead of only while it was pending, so river ate them
always and the window manager acted on them never."
  (or (reading-p) (cursor-on-empty-pane-p) *pending-keymap*))

(defun ensure-capture-bindings (seat)
  "Make sure every key the policy asks for has a binding on SEAT.

INCREMENTAL, AND THAT IS THE WHOLE DIFFERENCE FROM WHAT THIS USED TO BE.  It
created the bindings once, from a constant, and never looked again — so a
CAPTURE-KEYS method evaluated at a REPL, or a policy installed by a
configuration file that loads after the first manage sequence, would have been
read and discarded.  Now it is the same rule REGISTER-BINDINGS uses for the
keymap: add what is missing, leave what is there.

Never removes.  A river_xkb_binding_v1 the compositor already knows about costs
one disabled object, and destroying bindings on every manage sequence to chase
a policy that might answer differently is churn in the one code path that runs
before the compositor can process input.

A binding created while capture is armed is enabled immediately.  Without that,
a key added at a REPL would not work until something else toggled the armed
state — which is the sort of `works on the second try' that costs an afternoon."
  (let* ((bindings (server-bindings *server*))
         (armed (c:prop seat :capture-armed))
         (wanted (capture-key-list)))
    ;; Diffed like everything else on this path.  ARM-CAPTURE runs on every
    ;; manage sequence and the answer is two hundred keys, so the common case —
    ;; the policy said the same thing it said last time — has to be one list
    ;; comparison rather than two hundred ASSOCs over a two-hundred-element
    ;; alist.  Comparing the answer is also what makes `never removes' safe to
    ;; state: nothing here can churn when nothing changed.
    (when (and bindings (not (equal wanted (c:prop seat :capture-wanted))))
      (setf (c:prop seat :capture-wanted) (copy-list wanted))
      (dolist (key wanted)
        (destructuring-bind (keysym . modifiers) key
          (unless (assoc key (c:prop seat :capture-bindings) :test #'equal)
            (let ((binding (best-effort "get_xkb_binding"
                             (w:bindings-get-xkb-binding
                              bindings (seat-proxy seat) keysym modifiers))))
              (when binding
                (push (let ((keysym keysym) (modifiers modifiers))
                        (lambda (event &rest arguments)
                          (declare (ignore arguments))
                          (with-abandon
                            (when (eq event :pressed)
                              (handle-captured-key keysym modifiers)))))
                      (wl:wl-proxy-hooks binding))
                (push (cons (cons keysym modifiers) binding)
                      (c:prop seat :capture-bindings))
                (when armed
                  (best-effort "capture binding" (w:binding-enable binding))))))))))
  (c:prop seat :capture-bindings))

(defun handle-captured-key (keysym modifiers)
  "A key arrived because we had asked for it.  Decide what it meant."
  (let ((character (when (<= #x20 keysym #x7e)
                     (let ((base (code-char keysym)))
                       ;; River sends the *unshifted* keysym with Shift in the
                       ;; modifier set, so the shifted glyph is ours to work
                       ;; out.  See *SHIFT-MAP*.
                       (if (member :shift modifiers)
                           (p:shifted-character (p:current-policy) base)
                           base)))))
    (cond
      ((reading-p) (prompt-key keysym modifiers character))
      ;; A chord is waiting for its second key.  HANDLE-KEY already knows what
      ;; to do with one; it just needs to be given the key, which is the whole
      ;; of what ate_unbound_key cannot tell us.
      (*pending-keymap* (handle-key (cons keysym modifiers)))
      ;; A chord in an empty pane is not a request to open an editor.  The
      ;; empty pane's table is single printable keys, and letting Ctrl through
      ;; would make C-c there mean whatever `c' means.
      ((and (cursor-on-empty-pane-p) (null modifiers))
       (spawn-for-empty-pane character))
      (t nil))))

(defun arm-capture ()
  "Enable or disable the capture bindings to match what is being read.

Manage sequence only: enable and disable are window-management state.  Diffed,
because this runs on every manage sequence and there are ninety-eight of them."
  (let* ((seat (primary-seat))
         (bindings-seat (and seat (seat-bindings-seat seat))))
    (when bindings-seat
      (when *pending-keymap*
        (best-effort "ensure_next_key_eaten"
          (w:bindings-seat-ensure-next-key-eaten bindings-seat)))
      (let ((wanted (capture-wanted-p)))
        (ensure-capture-bindings seat)
        (unless (eq wanted (c:prop seat :capture-armed))
          (setf (c:prop seat :capture-armed) wanted)
          (loop for (nil . binding) in (c:prop seat :capture-bindings)
                do (best-effort "capture binding"
                     (if wanted (w:binding-enable binding)
                         (w:binding-disable binding)))))))))

(defun spawn-for-empty-pane (character)
  "Run the command CHARACTER names, if the cursor is still on an empty pane.

DESIGN D19: while the cursor rests on an empty pane, an unbound printable key
is looked up in a table -- e opens an editor, t a terminal -- so the empty pane
is a spawn menu with no menu.

THE DESIGN GUESSED THE MECHANISM WRONG AND THE ANSWER IS BETTER THAN ITS
FALLBACK.  D19 rests on ate_unbound_key and asks whether it carries the keysym,
ruling that if it does not, only a single-default mode is possible and the table
dies.  Measured against the shipped protocol, ate_unbound_key has *no arguments
at all*.  But binding the keys ourselves and enabling them conditionally gets
the keysym, keeps the table, and never intercepts a key that would have gone to
an application -- because the binding is not enabled then."
  (when (and character (cursor-on-empty-pane-p))
    (let ((command (guarded "key-unbound"
                     (p:key-unbound (p:current-policy) *world* character))))
      (when command
        (logmsg :debug "empty pane: ~a -> ~a" character command)
        (run-command command)
        (after-command)))))
