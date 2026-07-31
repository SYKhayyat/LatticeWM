;;;; tools/hardware-check.lisp --- Record a whole session, so nobody has to
;;;; remember what they did.
;;;;
;;;; Load it from ~/.config/latticewm/init.lisp -- there is deliberately no
;;;; --config flag, so this is a LOAD line rather than an argument:
;;;;
;;;;   (load "/home/you/LatticeWM/tools/hardware-check.lisp")
;;;;
;;;; Then use the window manager normally and quit.  Everything lands in
;;;; ~/latticewm-hardware.txt as it happens.
;;;;
;;;; WHY IT RECORDS SO MUCH.  The first sessions were debugged by the user
;;;; describing what they saw -- "the first super return did not open term",
;;;; "i could never type", "closing did not work".  Every one of those was
;;;; accurate and every one cost a round trip, because the report held the
;;;; answers to three prepared questions and nothing else.  The failures were
;;;; always in the part nobody had thought to instrument.
;;;;
;;;; So this records the session rather than a checklist: every key that fired
;;;; and what it was bound to, every window, every focus move, the machine it
;;;; ran on, and the state at the end.  Written line by line as things happen,
;;;; because the most interesting way for a session to end is badly.
;;;;
;;;; It records rather than concludes.  Every line is what was observed.

(in-package #:latticewm/user)

(defparameter *report* (merge-pathnames "latticewm-hardware.txt"
                                        (user-homedir-pathname)))

(defvar *started* (get-internal-real-time))

(defun note (format &rest arguments)
  "Append one observation, timestamped, immediately and unbuffered.

Unbuffered because a report lost precisely when something goes wrong is worse
than no report.  Timestamped because \"the *first* Super+Return did nothing\"
is a claim about order, and order is the one thing a pile of lines cannot
recover afterwards."
  (with-open-file (out *report* :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
    (format out "~6,1f  " (/ (float (- (get-internal-real-time) *started*))
                             internal-time-units-per-second))
    (apply #'format out format arguments)
    (terpri out)))

;;; ------------------------------------------------------------ every key

(defmethod on-key ((policy conventional-policy) world key)
  "Record every bound key, and what the keymap says it does.

This is the line that was missing for three sessions.  ON-KEY sees the key
*after* river matched a binding, so the record says both what was pressed and
what it resolved to -- which is how \"the first Super+Return did not open
term\" becomes either `KEY Super+Return -> (\"terminal\")' or, as it turned
out, the same line with a note that an overlay swallowed it."
  (declare (ignore world))
  (note "KEY     ~a~@[  -> ~s~]~@[  ~a~]"
        (key-to-string key)
        (lookup-key *keymap* key)
        (cond (*pending-keymap* "(second key of a chord)")
              ((reading-p) "(at a prompt)")
              (*help-visible* "(an overlay was up; this key also dismissed it)")))
  nil)

(defmethod key-unbound ((policy conventional-policy) world keysym)
  "An unbound key reached us, which only happens on an empty pane.

KEYSYM arrives as a *character* for anything printable -- HANDLE-UNBOUND-KEY
passes (or character keysym) -- and the first version of this called
KEYSYM-NAME on it, which wants an integer.  The method signalled, GUARDED
swallowed it, and the empty pane stopped spawning: pressing `t' did nothing,
and the instrumentation was the reason."
  (note "UNBOUND ~s on an empty pane -- ate_unbound_key works" keysym)
  (call-next-method))

;;; --------------------------------------------------------- what happened

(defun note-window-opened (window)
  (note "WINDOW  opened ~s at ~s~@[  cell ~s~]"
        (or (window-app-id window) "?")
        (world-cursor *world*)
        (let ((node (world-node-at *world*)))
          (and node (prop node :lattice/address)))))

(defun note-window-closed (window)
  (note "WINDOW  closed ~s -- ~d left"
        (or (window-app-id window) "?") (length (all-windows))))

(defun note-focus (old new)
  ;; Focus repair re-announces the path it was already on; recording those
  ;; buries the moves that matter.
  (unless (equal old new)
    (note "FOCUS   ~s -> ~s~@[  cell ~s~]~:[~;  (empty pane)~]"
        old new
        (let ((node (world-node-at *world*)))
          (and node (prop node :lattice/address)))
        (latticewm/runtime::cursor-on-empty-pane-p))))

(defun note-workspace (index) (note "WKSPACE now ~a" index))

(add-hook :window-opened 'note-window-opened)
(add-hook :window-closed 'note-window-closed)
(add-hook :focus-changed 'note-focus)
(add-hook :workspace-changed 'note-workspace)

;;; ------------------------------------------------- the machine it ran on

(defun note-environment ()
  (note "~%======== LatticeWM session ========")
  (note "ENV     display=~a  session=~a"
        (or (uiop:getenv "WAYLAND_DISPLAY") "?")
        (or (uiop:getenv "XDG_SESSION_TYPE") "?"))
  (note "ENV     font=~a ~dx~d  lattice=~:[absent~;loaded~]"
        (font-name *default-font*)
        (font-width *default-font*) (font-height *default-font*)
        (find-package "LATTICE"))
  (note "ENV     ~d keys registered, ~d commands, ~d generics"
        (length (bindable-keys)) (length (all-commands))
        (length (policy-generics)))
  ;; The keys most likely to be the question, resolved rather than assumed --
  ;; this is what would have shown, instantly, that the lattice had taken
  ;; Super+/ away from `help'.
  (dolist (spec '("Super+Return" "Super+d" "Super+q" "Super+slash"
                  "Super+minus" "Shift+Super+question" "Super+semicolon"))
    (note "BINDING ~22a -> ~s" spec
          (ignore-errors (lookup-key *keymap* (kbd spec))))))

(defun note-outputs ()
  (dolist (output (all-outputs))
    (let ((r (output-rect output)))
      (note "OUTPUT  ~a  ~dx~d at (~d,~d)  workspace=~a"
            (or (output-name output) "(name not arrived yet)")
            (rect-w r) (rect-h r) (rect-x r) (rect-y r)
            (prop output :workspace)))))

(defun note-state (why)
  (note "STATE   ~a: ~d window~:p, cursor ~s~@[, cell ~s~]"
        why (length (all-windows)) (world-cursor *world*)
        (let ((node (world-node-at *world*)))
          (and node (prop node :lattice/address))))
  (let ((v (current-viewport))) (when v (note "STATE   viewport ~a" v)))
  (note "STATE   the status line reads: ~{~a~^ | ~}"
        (remove "" (mapcar #'car (ignore-errors
                                  (echo-content (current-policy) *world*)))
                :test #'equal)))


;;; ------------------------------------------------------- the viewport

(defvar *last-viewport* nil)

(defun current-viewport ()
  "The lattice viewport as a string, or NIL when the lattice is not loaded."
  (ignore-errors
   (let* ((workspace (world-node-at *world* '(0)))
          (slot (find-symbol "VIEWPORT" "LATTICE")))
     (when (and workspace slot (slot-exists-p workspace slot))
       (princ-to-string (slot-value workspace slot))))))

(defun note-viewport ()
  "Record the viewport, but only when it has actually changed.

THIS WAS THE LINE MISSING FROM THE LAST RECORDING.  Twelve presses of Super+-
appear in it as twelve identical KEY lines and nothing else, so the report
could not answer the only question that mattered -- whether zooming did
anything.  It did: the viewport goes 1x1 to 6x4 over six presses.  A key that
works and leaves no trace is indistinguishable, in a log, from one that does
not."
  (let ((now (current-viewport)))
    (when (and now (not (equal now *last-viewport*)))
      (setf *last-viewport* now)
      (note "ZOOM    ~a" now))))

(add-hook :layout-changed 'note-viewport)

;;; --------------------------------------------------------------- wiring

;; NOT :startup.  ADD-HOOK puts new functions at the *front*, and this file is
;; loaded after the config's own hooks, so a :startup recording would run
;; before them -- before the lattice installs its keys.  The first version did
;; exactly that and reported "Super+minus -> NIL" and 87 keys, both of which
;; were true at the moment it looked and useless by the time anything happened.
;; The first layout is after every startup hook has run.

(defvar *outputs-noted* nil)

(defun note-outputs-once ()
  ;; :LAYOUT-CHANGED, not :RELAYOUT.  The latter is not a hook this system
  ;; runs -- the first draft of this file hung its recording on it and would
  ;; have reported nothing at all.  ADD-HOOK says so out loud now.
  (unless *outputs-noted*
    (setf *outputs-noted* t)
    (ignore-errors (note-environment))
    (ignore-errors (note-outputs))
    (ignore-errors (note-state "first layout"))))

(add-hook :layout-changed 'note-outputs-once)

(defun note-shutdown ()
  (ignore-errors (note-outputs))
  (ignore-errors (note-state "at exit"))
  (note "======== end ========~%"))

(add-hook :shutdown 'note-shutdown)

(defcommand hardware-report ()
  "Write everything known right now, and say where the file is.

Bound to Super+F12.  You should not need it -- the session records itself and
the shutdown hook writes the ending -- but it is here for the case where the
window manager does not get to exit cleanly."
  (ignore-errors (note-environment))
  (ignore-errors (note-outputs))
  (ignore-errors (note-state "asked"))
  (notify "wrote ~a" *report*))

(define-key *keymap* "Super+F12" '("hardware-report"))
