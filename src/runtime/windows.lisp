;;;; runtime/windows.lisp --- Window lifecycle, driven by river's events.
;;;;
;;;; Every window we will ever manage arrives as a `new_id' inside a `window'
;;;; event on river_window_manager_v1, which is why server-created new_id
;;;; handling was the first thing verified in the binding library.
;;;;
;;;; The order of the events at creation is the thing to hold on to: river
;;;; sends `window', then a burst of property events (app_id, title,
;;;; identifier, parent, dimensions_hint), then `manage_start'.  So we must not
;;;; decide where a window goes the moment it appears — at that instant we know
;;;; nothing about it, and SHOULD-FLOAT-P would be answering about a blank.
;;;; Placement is deferred to the first manage sequence, by which time
;;;; everything has arrived.

(in-package #:latticewm/runtime)

(defvar *unplaced* '()
  "Windows that have appeared but have not yet been placed.

Drained at the start of each manage sequence.  See the file header for why
this exists rather than placing on arrival.")

(defun attach-window (proxy)
  "Register a newly announced river_window_v1 and start listening to it."
  (let ((window (make-instance 'c:window :proxy proxy)))
    (setf (gethash proxy (server-windows *server*)) window)
    (push (evlambda-for-window window) (wl:wl-proxy-hooks proxy))
    (push window *unplaced*)
    (logmsg :debug "window appeared: ~s" window)
    window))

(defun evlambda-for-window (window)
  "The event handler for one window.

Everything here is bookkeeping: record what river tells us and mark the layout
dirty.  No placement decisions are made in an event handler, because an event
handler runs at an arbitrary point in the protocol and has no idea which
sequence it is in."
  ;; Declared rather than wrapped in ON-EVENTS: this one *returns* the handler
  ;; instead of pushing it, so the macro's shape does not fit.  The declaration
  ;; is what gate 8 checks, and it has to be kept beside the CASE by hand --
  ;; which is exactly the drift the macro exists to prevent, so it is the one
  ;; place in the system where that risk is real.
  (load-time-value
   (declare-handled-events "river_window_v1"
    '(:app-id :title :identifier :parent :dimensions :dimensions-hint :decoration-hint :closed :fullscreen-requested :exit-fullscreen-requested :minimize-requested :maximize-requested :unmaximize-requested :pointer-move-requested :pointer-resize-requested :show-window-menu-requested :unreliable-pid :presentation-hint))
   t)
  (lambda (event &rest arguments)
    (with-abandon
      (case event
        (:app-id (setf (c:window-app-id window) (first arguments)))
        (:title (setf (c:window-title window) (first arguments)))
        (:identifier (setf (c:window-identifier window) (first arguments)))
        (:parent
         (setf (c:window-parent-window window)
               (and (first arguments) (window-of-proxy (first arguments)))))
        (:dimensions
         (setf (c:window-width window) (first arguments)
               (c:window-height window) (second arguments))
         ;; The size we asked for is advisory and this is the answer.  Redo the
         ;; placement so gravity can centre a window that came back small.
         (mark-dirty))
        (:dimensions-hint
         (destructuring-bind (min-w min-h max-w max-h) arguments
           (setf (c:window-min-width window) min-w
                 (c:window-min-height window) min-h
                 (c:window-max-width window) max-w
                 (c:window-max-height window) max-h)))
        (:decoration-hint (setf (c:window-decoration-hint window)
                                (first arguments)))
        (:closed (detach-window window))
        (:fullscreen-requested (request-fullscreen window t))
        (:exit-fullscreen-requested (request-fullscreen window nil))
        (:minimize-requested (minimize-window window))
        (:maximize-requested (guarded "inform_maximized"
                               (w:window-inform-maximized (c:window-proxy window))))
        (:unmaximize-requested
         (guarded "inform_unmaximized"
           (w:window-inform-unmaximized (c:window-proxy window))))
        ;; Somebody grabbed the window's own titlebar or corner.  Clients that
        ;; draw their own decorations ask for this through xdg-shell and river
        ;; forwards it; ignoring it makes a GTK titlebar inert, which reads as
        ;; the window manager being broken rather than as a policy.
        (:pointer-move-requested
         (on-client-move-request window (seat-of-proxy (first arguments))))
        (:pointer-resize-requested
         (on-client-resize-request window (seat-of-proxy (first arguments))
                                   (second arguments)))
        ;; A right-click on a client-side titlebar.  There is no window menu to
        ;; put up -- every operation it would offer is a command with a key --
        ;; so the honest answer is to say what the keys are, in the one place
        ;; the program can talk.
        (:show-window-menu-requested
         (notify "window menu: ~a float  ~a fullscreen  ~a close"
                 (or (p:keys-running (p:current-policy) "toggle-float") "?")
                 (or (p:keys-running (p:current-policy) "toggle-fullscreen") "?")
                 (or (p:keys-running (p:current-policy) "close") "?")))
        ;; Advisory and unreliable by its own name, and useful anyway: it is
        ;; the only way to tell two windows of the same application apart when
        ;; writing a rule.
        (:unreliable-pid (setf (c:prop window :pid) (first arguments)))
        (:presentation-hint (setf (c:prop window :presentation-hint)
                                  (first arguments)))
        (t (logmsg :debug "window event ~s ~s" event arguments))))))

(defun detach-window (window)
  "River says the window is gone.  Take it out of everything."
  (setf (c:window-live-p window) nil)
  (let ((proxy (c:window-proxy window)))
    (when proxy (remhash proxy (server-windows *server*))))
  (remhash window (server-nodes *server*))
  (forget-window-state window)
  (setf *unplaced* (remove window *unplaced*))
  ;; Out of the float list and the scratchpad, wherever it was.
  (setf (c:world-floats *world*)
        (remove window (c:world-floats *world*) :key #'c:float-window)
        (c:world-scratchpad *world*)
        (remove window (c:world-scratchpad *world*)))
  ;; And out of the tree, through the policy, so that focus repair and the
  ;; collapse rules are the ones the user configured.
  (let* ((root (c:world-root *world*))
         (leaf (c:leaf-holding root window))
         (path (and leaf (c:node-path-to root leaf))))
    (when path
      (guarded "on-window-close"
        (p:on-window-close (p:current-policy) *world* window path))))
  (setf (c:window-proxy window) nil)
  (run-hooks :window-closed window)
  (mark-dirty)
  (logmsg :debug "window closed: ~s" window))

(defun place-unplaced-windows ()
  "Place every window that has appeared since the last manage sequence.

Called from inside a manage sequence, which is where the window-management
requests this makes — set_capabilities, use_ssd, propose_dimensions — are
legal."
  (let ((pending (nreverse *unplaced*))
        (policy (p:current-policy)))
    (setf *unplaced* '())
    (dolist (window pending)
      (when (c:window-live-p window)
        (declare-window-capabilities policy window)
        (let ((path (guarded "on-window-open"
                      (p:on-window-open policy *world* window))))
          (when (and (null path) (c:window-floating-p window))
            (float-window-now policy window))
          (run-hooks :window-opened window)
          (logmsg :info "placed ~s at ~s" window path))))
    (when pending (mark-dirty))))

(defun declare-window-capabilities (policy window)
  "Tell river which window-menu buttons to draw, and who decorates.

River draws client-side-decoration titlebar buttons based on what we declare,
so getting this wrong shows up as a titlebar with a maximize button that does
nothing."
  (let ((proxy (c:window-proxy window)))
    (when proxy
      (guarded "set_capabilities"
        (w:window-set-capabilities proxy (p:window-capabilities policy window)))
      (guarded "decoration"
        (if (eq (p:decoration-mode policy window) :ssd)
            (w:window-use-ssd proxy)
            (w:window-use-csd proxy)))
      ;; set_tiled is reconciled by EMIT-WINDOW-MANAGEMENT-STATE, so that there
      ;; is exactly one place that decides it.
      nil)))

;;; ------------------------------------------------------------- floating

(defun float-window-now (policy window)
  "Give WINDOW a floating rectangle and put it on the float list."
  (let* ((output (or (current-output) (first (all-outputs))))
         (rect (if output
                   (guarded "default-float-rect"
                     (p:default-float-rect policy window output))
                   (c:make-rect 100 100 640 480))))
    (setf (c:window-floating-p window) t)
    (let ((float (make-instance 'c:floating-window
                                :window window
                                :rect (or rect (c:make-rect 100 100 640 480)))))
      (push float (c:world-floats *world*))
      ;; Focus follows the window you just floated.  Without this, floating
      ;; something takes the keyboard away from it — which is exactly backwards,
      ;; since you floated it in order to use it.
      (setf (c:world-focused-float *world*) float))
    ;; set_tiled is window-management state and is reconciled by the emitter in
    ;; the next manage sequence.  Sending it here would work from a key binding
    ;; and fail from a REPL.
    (mark-dirty)
    window))

(p:define-option *unfloat-returns-home* t
  "Put an unfloated window back where it was floated from, if that place survives.

T is the symmetry with minimize and restore, and the argument ON-RESTORE's
docstring makes applies verbatim: \"landing back where you were is the
difference between minimize being useful and being a way to lose a window\".
Floating something to look at it and then putting it back is the same gesture,
and it had the opposite behaviour — unfloat dropped the window at the cursor,
wherever that happened to be by then.

NIL restores the old behaviour: an unfloated window lands at the cursor, which
is what you want if you use floating as a way of *moving* windows around.")

(defun unfloat-window (window)
  "Put a floating window back into the tree — where it came from, or at the cursor."
  (let ((float (c:float-of-window *world* window)))
    (when (eq float (c:world-focused-float *world*))
      (setf (c:world-focused-float *world*) nil)))
  (setf (c:world-floats *world*)
        (remove window (c:world-floats *world*) :key #'c:float-window)
        (c:window-floating-p window) nil)
  (let* ((policy (p:current-policy))
         (leaf (c:make-leaf window))
         (home (and *unfloat-returns-home* (c:prop window :float-home-path)))
         (target (and home (c:resolve-path (c:world-root *world*) home)))
         (landed
           (cond
             ;; The pane it was floated out of is still there and still empty:
             ;; it is still yours.
             ((c:empty-pane-p target)
              (setf (c:world-root *world*)
                    (c:tree-replace-at (c:world-root *world*) home leaf))
              home)
             (t
              (let ((here (current-leaf)))
                (p:place-node policy *world* leaf (current-path)
                              (if (and here (c:leaf-empty-p here))
                                  :fill :split)))))))
    (setf (c:prop window :float-home-path) nil)
    (mark-dirty)
    (p:jump-cursor policy *world* landed)
    landed))

;;; --------------------------------------------------- fullscreen, minimize

(defun request-fullscreen (window on)
  "Honour a fullscreen request.  Cheap, in both directions.

River makes a fullscreen window the only thing rendered on its output; clip
boxes are ignored and borders are not drawn.  So entering and leaving cost no
relayout at all, which is why we honour it unconditionally rather than making
it a policy question."
  ;; Model only.  EMIT-WINDOW-MANAGEMENT-STATE reconciles it in the next manage
  ;; sequence, which is the only place the requests are legal.
  (setf (c:window-fullscreen-p window) on)
  (mark-dirty)
  (request-manage)
  window)

(defun minimize-window (window)
  "Take WINDOW out of the tree and onto the scratchpad."
  (guarded "on-minimize" (p:on-minimize (p:current-policy) *world* window))
  ;; No explicit hide: the window is out of the tree, so the next layout does
  ;; not place it, and the emitter hides everything it did not place.  Hiding
  ;; here as well would be a rendering request outside a render sequence.
  (let ((float (c:float-of-window *world* window)))
    (when (eq float (c:world-focused-float *world*))
      (setf (c:world-focused-float *world*) nil)))
  (mark-dirty)
  (request-manage)
  window)

(defun restore-window (window)
  "Bring WINDOW back from the scratchpad."
  (guarded "on-restore" (p:on-restore (p:current-policy) *world* window))
  (mark-dirty)
  (request-manage)
  window)

;;; ------------------------------------------------------------ keyboard focus

(defun apply-keyboard-focus ()
  "Derive Wayland keyboard focus from the cursor.  DESIGN D18.

Focus is a *place*.  When the place holds a window that window gets focus;
when it is an empty pane there is nothing for Wayland focus to be, and
clear_focus is the honest answer rather than leaving the last window focused
while the cursor is somewhere else — which would mean your keystrokes go
somewhere other than where the highlight is.

WHAT COUNTS AS THE PLACE IS P:FOCUS-TARGET, and that is the point of the one
line that changed here.  The rule itself was a C:WORLD-FOCUS-WINDOW call in the
middle of this function, so the single idea the README leads with was the single
decision no policy could reach: click-to-focus and sloppy-focus were both
unwritable, in a program whose thesis is that they are methods.  This function
keeps what is genuinely the runtime's: the lock-screen case, the diff, and the
choice of which protocol request says `nothing'."
  (let* ((seat (primary-seat))
         ;; Wrapped in a list, deliberately.  A policy answering NIL means `an
         ;; empty pane, clear the keyboard' and is D18's whole point; GUARDED
         ;; also answers NIL when a method signalled.  Clearing the keyboard
         ;; because somebody's FOCUS-TARGET has a typo in it is the wrong
         ;; failure — the right one is to leave focus exactly where it was.
         (answer (guarded "focus-target"
                   (list (p:focus-target (p:current-policy) *world*))))
         (window (if answer (first answer) (and seat (seat-focused seat)))))
    (when seat
      (cond
        ;; A layer surface has taken exclusive focus — a screen locker.  The
        ;; protocol says every focus request we make is ignored until it
        ;; clears, so sending them anyway is a window manager fighting a locker
        ;; for the keyboard on every manage sequence and losing, quietly,
        ;; forever.  Forgetting what we last focused is deliberate: when the
        ;; lock clears we want to re-send focus rather than believe a cached
        ;; value the compositor never honoured.
        ((layer-shell-holds-keyboard-p seat)
         (setf (seat-focused seat) nil))
        ((eq window (seat-focused seat)) nil)
        (t
         (setf (seat-focused seat) window)
         (guarded "focus"
           (if (and window (c:window-proxy window) (c:window-live-p window))
               (w:seat-focus-window (seat-proxy seat) (c:window-proxy window))
               (w:seat-clear-focus (seat-proxy seat)))))))))
