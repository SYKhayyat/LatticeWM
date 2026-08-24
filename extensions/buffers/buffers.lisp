;;;; buffers/buffers.lisp --- The registry, and the two verbs.
;;;;
;;;; THE REGISTRY IS A HASH ON THE WORLD, not a global: it belongs to a
;;;; session the way the tree does, and a saved layout that carried names
;;;; would want them beside everything else.  The reverse direction -- which
;;;; name does this window have -- is a prop on the window.
;;;;
;;;; THREE DECISIONS WERE ARGUED BEFORE THIS WAS BUILT, and the answers are
;;;; baked in as the doc/EXTENSION-IDEAS.org entry recorded them:
;;;;
;;;;   * Focus after a recall is configurable (*FOCUS-FOLLOWS-RECALL*, default
;;;;     follow), because both answers are defensible and P1 says ship both.
;;;;   * Whether a switch is an undo step is configurable
;;;;     (*UNDO-INCLUDES-SWAPS*, default no): a switch changes what you are
;;;;     LOOKING at more often than it changes an arrangement you want to walk
;;;;     back through.  When NO, the switch resets undo's baseline instead of
;;;;     recording -- RESET-UNDO-BASLINE exists for exactly this shape of
;;;;     deliberate change, and using it means there is no inhibit flag to
;;;;     leave set if something signals halfway.
;;;;   * Two panes never show one window.  A live Wayland window has exactly
;;;;     one rectangle; cloning is not representable.  Asking for a buffer
;;;;     that is already visible jumps to the pane showing it.

(in-package #:buffers)

;;; ------------------------------------------------------------- the options

(p:define-option *focus-follows-recall* t
  "Whether the cursor follows a buffer recalled into the current pane.

T (the default): recalling a named window moves the cursor onto it, so the
keyboard arrives with the window.  NIL: the cursor stays where it was, which
suits a script or a socket call that rearranges panes while you are working in
another one.")

(p:define-option *undo-includes-swaps* nil
  "Whether switching buffers records an undo step.

NIL (the default): a switch is a change of view, not an edit to walk back
through -- undo restores arrangements and leaves your choice of view alone.
T: every switch lands on the undo ring like any other tree change.  The cost
of T is a ring full of near-identical trees; the cost of NIL is that undo
cannot put a window back where a switch took it from.")

;;; ------------------------------------------------------------ the registry

(defun registry (&optional (world r:*world*))
  "The session's name-to-window table, created on first use."
  (or (c:prop world :buffers/registry)
      (setf (c:prop world :buffers/registry)
            (make-hash-table :test #'equal))))

(defun buffer-name (window &optional (world r:*world*))
  "The name WINDOW carries, or NIL."
  (c:prop window :buffers/name))

(defun buffer-names (&optional (world r:*world*))
  "Every name in the registry, sorted."
  (sort (loop for name being the hash-keys of (registry world) collect name)
        #'string<))

(defun window-named (name &optional (world r:*world*))
  "The window NAME refers to, or NIL."
  (gethash name (registry world)))

(defun forget-window (window &optional (world r:*world*))
  "Drop WINDOW's entry, wherever it is.  Called when a named window closes."
  (let ((name (buffer-name window world)))
    (when name
      (remhash name (registry world))
      (setf (c:prop window :buffers/name) nil)))
  (values))

(defun auto-name (window world)
  "A name for WINDOW nobody supplied: its app-id, made unique by numbering."
  (let ((base (or (c:window-app-id window) "window")))
    (loop for n from 0
          for candidate = (if (zerop n) base (format nil "~a<~d>" base n))
          unless (gethash candidate (registry world)) return candidate)))

(defun note-window-closed (window)
  "Drop WINDOW's registry entry.  On :WINDOW-CLOSED."
  (forget-window window))

;;; Registered once at load; ADD-HOOK replaces rather than accumulates, so a
;;; configuration file loading the module twice leaves one entry.
(r:add-hook :window-closed 'note-window-closed)

;;; ------------------------------------------------------- reachability

(defun install-vocabulary (&optional (package '#:latticewm/user))
  "Make this module's names visible in PACKAGE, the one configuration files
and the REPL are read in.  The lattice's own pattern, verbatim in spirit:
USE-PACKAGE at load time rather than a hand-maintained re-export list, with
conflicts resolved in favour of the name that was already there, because a
name the user defined is theirs."
  (let ((target (find-package package))
        (source (find-package '#:buffers)))
    (when (and target source)
      (let ((taken '()))
        (do-external-symbols (symbol source)
          (multiple-value-bind (theirs status)
              (find-symbol (symbol-name symbol) target)
            (when (and status (not (eq theirs symbol)))
              (push theirs taken))))
        (when taken
          (shadowing-import taken target)
          (r:logmsg :warn "buffers: ~d name~:p in ~a already meant something ~
else and still do: ~{~a~^ ~}"
                    (length taken) (package-name target)
                    (sort (mapcar #'symbol-name taken) #'string<)))
        (use-package source target)))
    (values)))

(install-vocabulary)



;;; ---------------------------------------------------------------- verbs

(defun name-window (window &optional name)
  "Name WINDOW -- from the registry's point of view -- and return the name."
  (forget-window window)
  (let ((final (or name (auto-name window r:*world*))))
    (setf (gethash final (registry r:*world*)) window
          (c:prop window :buffers/name) final)
    final))

(r:defcommand name-buffer (&optional name)
  "Name the focused window so any pane can show it by name.

With no NAME supplied, one is derived from the app-id and made unique."
  (:interactive :buffer-name)
  ;; The command macro wraps the body, so RETURN-FROM cannot reach this
  ;; function's block; a COND says the same thing and stays inside it.
  (let ((win (r:current-window)))
    (cond
      ((null win) (r:notify "nothing under the cursor to name"))
      (t (let ((final (name-window win name)))
           (r:notify "buffer ~a" final)
           final)))))

(r:defcommand buffers ()
  "Echo every named buffer, sorted."
  (if (buffer-names)
      (r:notify "~{~a~^  ~}" (buffer-names))
      (r:notify "no buffers named yet")))

;;; The prompt completes over the live registry: :CANDIDATES is re-evaluated
;;; each time the prompt comes up, so names added since load are offered too.
(p:define-argument-type :buffer "buffer: "
  :documentation "A named buffer, with completion over the registry."
  :candidates (buffer-names))

(p:define-argument-type :buffer-name "name: "
  :documentation "A name for the focused window."
  )

(defun leaf-holding-path (world window)
  "The path of the leaf currently showing WINDOW, or NIL."
  (let ((leaf (c:leaf-holding (c:world-root world) window)))
    (and leaf (c:node-path-to (c:world-root world) leaf))))

(r:defcommand switch-to-buffer (name)
  "Show the buffer NAME in the current pane.

Already on screen: jump to the pane showing it -- one window, one rectangle,
so showing it twice is not representable.  Put away: recalled here, and the
window that was here goes to the scratchpad with its home set to this pane, so
restoring IT later brings it back HERE."
  (:interactive :buffer)
  (let ((win (window-named name)))
    (cond
      ((null win)
       (r:notify "no buffer named ~a" name))
      ((c:window-minimized-p win)
       (recall-into-current-pane win))
      (t
       (let ((path (leaf-holding-path r:*world* win)))
         (cond
           ((null path)
            ;; In neither the tree nor the scratchpad: a floating window.
            (recall-or-focus-float win))
           ((equal path (c:world-cursor r:*world*))
            ;; Already here; nothing to do, and nothing to record.
            )
           (t
            ;; On screen elsewhere: jump.
            (jump-to path)))))))
  name)

(defun jump-to (path)
  "Move the cursor to PATH.  A jump is a focus change only, and the undo
machinery already ignores those."
  (setf (c:world-cursor r:*world*) path)
  (r:mark-dirty))

(defun recall-into-current-pane (win)
  "Bring WIN back from the scratchpad into the pane under the cursor.

The window that was there goes to the scratchpad with ITS home set here, which
is what makes switch-and-switch-back return both windows to where each was.
Undo records the step or not, per *UNDO-INCLUDES-SWAPS*."
  (let* ((cursor-before (c:world-cursor r:*world*))
         (displaced (r:current-window)))
    ;; Take the displaced window out through the ordinary door -- ON-MINIMIZE
    ;; remembers where it was, which is this pane.
    (when displaced
      (r:minimize-window displaced))
    (r:restore-window win)
    ;; RESTORE lands at the cursor's pane now that it is empty, but say where
    ;; we want the cursor afterwards rather than trusting the landing.
    (cond
      (*focus-follows-recall*
       (let ((path (leaf-holding-path r:*world* win)))
         (when path (jump-to path))))
      (t
       (jump-to cursor-before)))
    (if *undo-includes-swaps*
        ;; Let the door's settle point record the swap like any other change.
        (r:note-layout-settled "switched buffers")
        ;; A deliberate change of view: the new arrangement becomes what undo
        ;; considers unchanged, and no step is recorded.
        (r:reset-undo-baseline "switched buffers"))))

(defun recall-or-focus-float (win)
  "WIN is neither tiled nor minimized: it is floating.  Focus it there."
  (declare (ignore win))
  (r:notify "that buffer is floating; use its window directly"))
