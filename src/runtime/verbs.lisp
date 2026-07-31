;;;; runtime/verbs.lisp --- The commands.
;;;;
;;;; README's P1 says that where a fork is situational rather than principled,
;;;; both options ship and configuration picks the default.  It also states the
;;;; cost honestly: "P1 is only affordable if the verbs are genuinely
;;;; orthogonal (verb × direction × scope, composed) rather than enumerated.
;;;; *If a P1 ruling ever requires writing two unrelated implementations rather
;;;; than one primitive with a parameter, that is the signal that the
;;;; abstraction is wrong.*"
;;;;
;;;; So this file is deliberately short, and every verb takes its variation as
;;;; an argument.  There are four spatial verbs — focus, move, swap, pull —
;;;; each taking a direction; a resize verb taking a direction and an amount;
;;;; and the structural verbs.  The keymap crosses them.  Nothing here is
;;;; written four times, once per direction.
;;;;
;;;; If you find yourself adding `focus-left', `focus-right', `focus-up' and
;;;; `focus-down' as separate commands, stop: that is the signal P1 names.

(in-package #:latticewm/runtime)

(defmacro with-relayout (&body body)
  "Run BODY and make sure its consequences reach the screen."
  `(prog1 (progn ,@body)
     (mark-dirty)))

(defun policy () (p:current-policy))

;;; ============================================================== motion

(defcommand focus (direction)
  "Move the cursor one pane DIRECTION — :LEFT, :RIGHT, :UP or :DOWN.

Motion is continuous across every boundary: within a split, out of a split
when the direction crosses its axis, and — once the lattice is loaded — into
the cell next door, entering through the edge you crossed.  Bumping the edge of
the world does nothing, and that is not an error."
  (with-relayout (p:move-cursor (policy) *world* direction)))

(defcommand focus-next ()
  "Move the cursor to the next pane in layout order, wrapping."
  (with-relayout
    (let ((next (c:next-leaf-path (c:world-root *world*) (current-path))))
      (when next (p:jump-cursor (policy) *world* next)))))

(defcommand focus-previous ()
  "Move the cursor to the previous pane in layout order, wrapping."
  (with-relayout
    (let ((previous (c:previous-leaf-path (c:world-root *world*) (current-path))))
      (when previous (p:jump-cursor (policy) *world* previous)))))

(defcommand focus-path (path)
  "Move the cursor to PATH, a list of addresses from the root."
  (with-relayout (p:jump-cursor (policy) *world* path)))

;;; ============================================================ structure

(defcommand (split-pane "split") (&optional axis (side :after))
  "Split the focused pane, leaving an *empty* pane beside it.

AXIS is :HORIZONTAL, :VERTICAL, or NIL to let the policy choose — which by
default cuts along the longer side so panes tend towards square.

This is README D17's resize idiom, and the empty pane it makes is a
first-class object rather than a gap waiting to be filled: you split a pane and
leave one side empty, and the window occupies the rest.  The cursor moves into
the new empty pane, where — per D19 — typing a key spawns something."
  (with-relayout
    (let* ((path (current-path))
           (node (current-node))
           (axis (or axis (p:split-axis-for (policy) node
                                            (p:node-rect *world* node)))))
      (multiple-value-bind (root landed)
          (c:tree-split-at (c:world-root *world*) path (c:make-leaf)
                           :axis axis :side side)
        (setf (c:world-root *world*) root)
        (p:jump-cursor (policy) *world* landed)))))

(defcommand (close-window "close") ()
  "Close the focused window, and take its pane with it.

The sibling grows to fill the space.  README D17's CLOSE; see CLEAR for the
other half."
  (with-relayout
    (let ((window (focused-window)))
      (if window
          (guarded "close" (w:window-close (c:window-proxy window)))
          ;; An empty pane has nothing to close, so closing it means removing
          ;; the pane — which is what the user meant.
          (remove-pane)))))

(defcommand clear ()
  "Empty the focused pane but leave it standing.

You are now *in* an empty pane, which still occupies its space.  README D17's
CLEAR: a separate verb from CLOSE rather than a mode, because they are
different intentions and a mode would make you remember which one you were in.

The window itself is closed; what survives is the place."
  (with-relayout
    (let ((leaf (current-leaf)))
      (when leaf
        (let ((window (c:leaf-window leaf)))
          (setf (c:leaf-window leaf) nil)
          (when window
            (guarded "close" (w:window-close (c:window-proxy window)))))))))

(defcommand remove-pane ()
  "Remove the focused pane entirely, whether or not it holds anything."
  (with-relayout
    (let ((path (current-path)))
      (when path
        (multiple-value-bind (removed root landed)
            (c:tree-remove-at (c:world-root *world*) path
                              :simplify (lambda (node)
                                          (p:should-collapse-p (policy) node)))
          (declare (ignore removed))
          (setf (c:world-root *world*) root)
          (p:jump-cursor (policy) *world* landed))))))

(defcommand move (direction &key (join nil))
  "Move the focused pane DIRECTION, onto whatever is there.

JOIN says what landing on something occupied means — :SPLIT, :SWAP or :STACK —
and defaults to the policy's answer, which ships as :SPLIT.  Landing on an
*empty* pane always fills it, whatever JOIN says, because an empty pane is a
place someone made for something.

This is the verb that satisfies 'you can move a window to be a split window in
another window': the destination does not have to be adjacent in the tree, only
adjacent on screen."
  (with-relayout
    (let* ((from (current-path))
           (to (p:find-motion-target (policy) (c:world-root *world*) from direction))
           (join (or join (p:move-into-occupied (policy) *world* from to))))
      (when (and to (not (c:path-equal from to)))
        (multiple-value-bind (root landed)
            (c:tree-move (c:world-root *world*) from to
                         :axis (c:direction-axis direction)
                         :side (if (member direction '(:right :down)) :after :before)
                         :join join)
          (setf (c:world-root *world*) root)
          (p:jump-cursor (policy) *world* landed))))))

(defcommand swap (direction)
  "Exchange the focused pane with the one DIRECTION of it.

Neither changes shape and nothing else moves — the cheap, predictable way to
rearrange.  The cursor follows the pane you were in, so swapping twice returns
you exactly where you were."
  (with-relayout
    (let* ((from (current-path))
           (to (p:find-motion-target (policy) (c:world-root *world*) from direction)))
      (when (and to (not (c:path-equal from to)))
        (let ((node (current-node)))
          (multiple-value-bind (root)
              (c:tree-swap (c:world-root *world*) from to)
            (setf (c:world-root *world*) root)
            (p:jump-cursor (policy) *world*
                           (or (c:node-path-to root node) to))))))))

(defcommand pull (direction)
  "Bring the pane DIRECTION of here *into* the focused pane, as a split.

The inverse of MOVE, and the reason it exists separately: pulling does not
require navigating to the thing first.  You stay where you are and the window
comes to you."
  (with-relayout
    (let* ((here (current-path))
           (there (p:find-motion-target (policy) (c:world-root *world*)
                                        here direction)))
      (when (and there (not (c:path-equal here there)))
        (multiple-value-bind (root landed)
            (c:tree-move (c:world-root *world*) there here
                         :axis (c:direction-axis direction)
                         :side (if (member direction '(:right :down)) :before :after)
                         :join :split)
          (setf (c:world-root *world*) root)
          (p:jump-cursor (policy) *world* landed))))))

;;; ================================================================ resize

(defcommand (resize-pane "resize") (direction &optional (amount 1/20))
  "Grow the focused pane DIRECTION by AMOUNT of its container's total.

Resizing is a *transfer* between two adjacent weights rather than an
assignment, so dragging one divider never disturbs the divider beyond it —
which is the single most common complaint about tiling resize.

Because weights are relative, this behaves identically at every zoom level and
on every monitor, and needs to know nothing about pixels."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path))
           (axis (c:direction-axis direction))
           (sign (c:direction-sign direction)))
      ;; Find the nearest ancestor split that runs along this axis: pressing
      ;; "wider" inside a vertical split means widening the column that split
      ;; is in, which is what the user meant and what they would have had to
      ;; navigate out to do by hand.
      (loop for depth from (length path) downto 1
            for container = (c:resolve-path root (subseq path 0 (1- depth)))
            for address = (nth (1- depth) path)
            when (and (typep container 'c:split)
                      (eq (c:split-axis container) axis))
              do (let* ((last (1- (c:container-count container)))
                        ;; At the far edge there is no neighbour on that side,
                        ;; so the transfer has to go the other way to mean
                        ;; anything.
                        (delta (if (and (= address last) (plusp sign))
                                   (- amount)
                                   (* sign amount))))
                   (c:adjust-weight container address (* delta 4)))
                 (return t)))))

(defcommand equalize ()
  "Give every pane in the focused container an equal share.

Applies to the nearest enclosing split.  With a prefix of nothing else, this is
the command people reach for after ten minutes of resizing."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path))
           (parent (c:resolve-path root (c:parent-path path))))
      (when (typep parent 'c:split)
        (setf (c:weights parent)
              (make-list (c:container-count parent) :initial-element 1))))))

(defcommand equalize-all ()
  "Give every pane in the whole workspace an equal share."
  (with-relayout
    (c:map-nodes (lambda (node)
                   (when (typep node 'c:split)
                     (setf (c:weights node)
                           (make-list (c:container-count node) :initial-element 1))))
                 (c:current-workspace *world*))))

;;; ============================================================== tabbing

(defcommand (tab-pane "tab") ()
  "Turn the focused pane and its next sibling into tabs.

A stack is what tabs are — an ordered set of alternatives of which one is
current — so this is the same object a workspace list is, and every verb that
works on one works on the other."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path))
           (parent (c:resolve-path root (c:parent-path path)))
           (address (c:path-last path)))
      (when (and (typep parent 'c:split) (> (c:container-count parent) 1))
        (let* ((other (if (< (1+ address) (c:container-count parent))
                          (1+ address) (1- address)))
               (a (c:child-at parent (min address other)))
               (b (c:child-at parent (max address other)))
               (stack (c:make-stack (list a b) 0)))
          (c:remove-child parent (max address other))
          (setf (c:child-at parent (min address other)) stack)
          (p:jump-cursor (policy) *world*
                         (c:node-path-to root stack)))))))

(defcommand tab-next (&optional (step 1))
  "Show the next tab of the nearest enclosing stack.

Also how you switch workspaces, because a workspace list *is* a stack — see
NEXT-WORKSPACE, which is this command aimed at the root."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path)))
      (loop for depth from (length path) downto 1
            for container = (c:resolve-path root (subseq path 0 (1- depth)))
            when (typep container 'c:stack)
              do (let ((n (c:container-count container)))
                   (when (plusp n)
                     (setf (c:stack-selected container)
                           (mod (+ (c:stack-selected container) step) n))
                     (p:jump-cursor (policy) *world*
                                    (c:repair-path root
                                                   (subseq path 0 (1- depth))))))
                 (return t)))))

(defcommand tab-previous ()
  "Show the previous tab of the nearest enclosing stack."
  (tab-next -1))

(defcommand untab ()
  "Dissolve the nearest enclosing stack back into a split."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path)))
      (loop for depth from (length path) downto 1
            for container = (c:resolve-path root (subseq path 0 (1- depth)))
            when (typep container 'c:stack)
              do (let ((split (c:make-split :horizontal (c:children container))))
                   (setf (c:world-root *world*)
                         (c:tree-replace-at root (subseq path 0 (1- depth)) split))
                   (p:jump-cursor (policy) *world*
                                  (c:repair-path (c:world-root *world*) path)))
                 (return t)))))

;;; =========================================================== workspaces

(defun workspace-stack ()
  "The workspace container, or NIL if the root is not one."
  (c:world-workspaces *world*))

(defcommand workspace (index)
  "Switch to workspace INDEX, creating it and any before it if needed.

Workspaces are a stack at the root of the tree, so this is the same operation
as switching a tab — and 'infinite workspaces' costs nothing, because a stack
grows."
  (with-relayout
    (let ((stack (workspace-stack)))
      (when stack
        (loop while (<= (c:container-count stack) index)
              do (c:insert-child stack (c:container-count stack) (c:make-leaf)))
        (setf (c:stack-selected stack) index)
        (p:jump-cursor (policy) *world* (list index))
        (run-hooks :workspace-changed index)))))

(defcommand next-workspace (&optional (step 1))
  "Switch to the next workspace, wrapping."
  (with-relayout
    (let ((stack (workspace-stack)))
      (when stack
        (workspace (mod (+ (c:stack-selected stack) step)
                        (max 1 (c:container-count stack))))))))

(defcommand previous-workspace ()
  "Switch to the previous workspace, wrapping."
  (next-workspace -1))

(defcommand new-workspace ()
  "Add a workspace after the current one and switch to it."
  (with-relayout
    (let ((stack (workspace-stack)))
      (when stack
        (let ((index (1+ (c:stack-selected stack))))
          (c:insert-child stack index (c:make-leaf))
          (setf (c:stack-selected stack) index)
          (p:jump-cursor (policy) *world* (list index))
          (run-hooks :workspace-changed index))))))

(defcommand send-to-workspace (index &key (follow nil))
  "Move the focused pane to workspace INDEX.

This is TREE-TRANSPLANT and nothing else — because a workspace is a container
and a pane is a subtree, 'send to workspace' needed no code of its own."
  (with-relayout
    (let ((stack (workspace-stack))
          (from (current-path)))
      (when (and stack (> (length from) 1))
        (loop while (<= (c:container-count stack) index)
              do (c:insert-child stack (c:container-count stack) (c:make-leaf)))
        (let* ((target (c:child-at stack index))
               (node (current-node)))
          (multiple-value-bind (root landed)
              (if (and (typep target 'c:leaf) (c:leaf-empty-p target))
                  (multiple-value-bind (r) (c:tree-remove-at (c:world-root *world*)
                                                             from :simplify nil)
                    (declare (ignore r))
                    (c:tree-replace-at (c:world-root *world*) (list index) node))
                  (c:tree-move (c:world-root *world*) from
                               (c:node-path-to (c:world-root *world*) target)))
            (setf (c:world-root *world*) root)
            (if follow
                (progn (setf (c:stack-selected stack) index)
                       (p:jump-cursor (policy) *world* landed))
                (p:repair-cursor (policy) *world* from))))))))

;;; ============================================== floating, fullscreen, etc.

(defcommand toggle-float ()
  "Float the focused window, or put a floating one back into the tree.

Floating is per window and chosen, never inferred and forced: SHOULD-FLOAT-P
only supplies the initial guess, and this is how you overrule it."
  (with-relayout
    (let ((window (focused-window)))
      (cond
        ((null window) nil)
        ((c:window-floating-p window) (unfloat-window window))
        (t
         (let* ((root (c:world-root *world*))
                (leaf (c:leaf-holding root window))
                (path (and leaf (c:node-path-to root leaf))))
           (when path
             (setf (c:window-home-path window) path)
             (multiple-value-bind (removed new-root landed)
                 (c:tree-remove-at root path :focus-path (current-path))
               (declare (ignore removed))
               (setf (c:world-root *world*) new-root
                     (c:world-cursor *world*) (c:repair-path new-root landed)))
             (float-window-now (policy) window))))))))

(defcommand toggle-fullscreen ()
  "Make the focused window the only thing on its output, or stop.

Cheap in both directions: river ignores clip boxes and does not draw borders
while fullscreen, so entering and leaving cost no relayout."
  (let ((window (focused-window)))
    (when window (request-fullscreen window (not (c:window-fullscreen-p window))))))

(defcommand minimize ()
  "Take the focused window out of the tiling tree entirely.

The remaining windows retile without it.  Minimize is not 'hide it somewhere',
it is 'take it out of the layout' — and where it went is the scratchpad, which
RESTORE-LAST brings it back from."
  (let ((window (focused-window)))
    (when window (minimize-window window))))

(defcommand restore-last ()
  "Bring back the most recently minimized window.

To the slot it was minimized from if that slot still exists, and to the cursor
otherwise."
  (let ((window (first (c:world-scratchpad *world*))))
    (when window (restore-window window))))

;;; ================================================================ system

(defcommand spawn (&rest command)
  "Run COMMAND as a detached process.

    (spawn \"foot\")
    (spawn \"firefox\" \"--new-window\")

The child is detached and its output goes nowhere, so a program that writes to
stderr cannot fill a pipe nobody is reading and block."
  (guarded "spawn"
    (sb-ext:run-program (first command) (rest command)
                        :search t :wait nil
                        :output nil :error nil :input nil))
  (logmsg :info "spawned ~{~a~^ ~}" command)
  nil)

(defcommand terminal ()
  "Open a terminal.  Bound to `t' in an empty pane by default."
  (spawn *terminal*))

(defcommand editor ()
  "Open an editor.  Bound to `e' in an empty pane by default."
  (spawn *editor*))

(defcommand browser ()
  "Open a web browser.  Bound to `b' in an empty pane by default."
  (spawn *browser*))

(defcommand files ()
  "Open a file manager.  Bound to `f' in an empty pane by default."
  (spawn *file-manager*))

(defcommand reload-config ()
  "Re-read the configuration file.

Note that this only re-runs the file; anything you redefined at a REPL and did
not write down is unaffected, and anything the file *removed* since last time
is not undone.  Live redefinition is not transactional, and pretending it is
would be worse than saying so."
  (load-config)
  (rebind-keys)
  (relayout :force t))

(defcommand quit ()
  "Exit the session.  This logs you out."
  (run-hooks :shutdown)
  (save-state)
  (when (server-manager *server*)
    (guarded "exit_session" (w:wm-exit-session (server-manager *server*))))
  (setf (server-running *server*) nil))

(defcommand restart-wm ()
  "Stop the window manager without ending the session.

River keeps running, and so does every application; the window manager is an
ordinary Wayland client.  Relaunch it and the layout comes back, because
persistence is keyed on river's stable window identifiers."
  (run-hooks :shutdown)
  (save-state)
  (setf (server-running *server*) nil))

(defcommand describe-key (spec)
  "Print what SPEC is bound to."
  (let* ((key (kbd spec))
         (target (lookup-key *keymap* key)))
    (format t "~&~a: ~:[unbound~;~:*~s~]~%" (key-to-string key) target)
    target))

;;; ==================================================================
;;; FLOATS THAT BELONG TO A PANE
;;; ==================================================================

(defcommand anchor-float (&optional (path (current-path)))
  "Pin the focused floating window to the pane at PATH.

An anchored float travels with the pane it belongs to: it moves when the pane
moves, hides when the pane hides, and is clipped by the pane's clip box.  That
is what \"a floating window inside a window\" means here — a picture-in-picture,
a preview, a terminal that follows its editor across the plane.

Costs one slot on the float.  The alternative — floats pinned to the output —
is still the default, because most floats are dialogs and a dialog belongs to
the screen rather than to a pane."
  (with-relayout
    (let* ((window (focused-window))
           (float (c:float-of-window *world* window)))
      (cond
        ((null float)
         (logmsg :warn "anchor-float: the focused window is not floating")
         nil)
        (t
         (let ((anchor (c:resolve-path (c:world-root *world*) path)))
           (setf (c:float-anchor float) anchor)
           ;; Re-express the float's rectangle relative to its new anchor, so
           ;; that anchoring does not make it jump.
           (let ((base (and anchor (gethash anchor (c:prop *world* :rect-index))))
                 (rect (c:float-rect float)))
             (when base
               (setf (c:float-rect float)
                     (c:make-rect (- (c:rect-x rect) (c:rect-x base))
                                  (- (c:rect-y rect) (c:rect-y base))
                                  (c:rect-w rect) (c:rect-h rect)))))
           (logmsg :info "float anchored to ~s" path)
           anchor))))))

(defcommand unanchor-float ()
  "Pin the focused floating window to the output instead of to a pane."
  (with-relayout
    (let* ((window (focused-window))
           (float (c:float-of-window *world* window)))
      (when (and float (c:float-anchor float))
        (let ((base (gethash (c:float-anchor float) (c:prop *world* :rect-index)))
              (rect (c:float-rect float)))
          (when base
            (setf (c:float-rect float)
                  (c:make-rect (+ (c:rect-x rect) (c:rect-x base))
                               (+ (c:rect-y rect) (c:rect-y base))
                               (c:rect-w rect) (c:rect-h rect)))))
        (setf (c:float-anchor float) nil)
        t))))

(defcommand move-float (direction &optional (pixels 40))
  "Nudge the focused floating window DIRECTION.

Floats are positioned by hand, which is the point of floating them; this is
the keyboard half of that."
  (with-relayout
    (let* ((window (focused-window))
           (float (c:float-of-window *world* window)))
      (when float
        (let ((rect (c:float-rect float))
              (dx (if (c:direction-horizontal-p direction)
                      (* pixels (c:direction-sign direction)) 0))
              (dy (if (c:direction-vertical-p direction)
                      (* pixels (c:direction-sign direction)) 0)))
          (setf (c:float-rect float)
                (c:make-rect (+ (c:rect-x rect) dx) (+ (c:rect-y rect) dy)
                             (c:rect-w rect) (c:rect-h rect))))))))

(defcommand resize-float (direction &optional (pixels 40))
  "Grow or shrink the focused floating window DIRECTION."
  (with-relayout
    (let* ((window (focused-window))
           (float (c:float-of-window *world* window)))
      (when float
        (let* ((rect (c:float-rect float))
               (sign (c:direction-sign direction))
               (dw (if (c:direction-horizontal-p direction) (* pixels sign) 0))
               (dh (if (c:direction-vertical-p direction) (* pixels sign) 0)))
          (setf (c:float-rect float)
                (c:make-rect (c:rect-x rect) (c:rect-y rect)
                             (max 100 (+ (c:rect-w rect) dw))
                             (max 60 (+ (c:rect-h rect) dh)))))))))

(defcommand focus-float (&optional (step 1))
  "Move keyboard focus to the next floating window, or back to the tree.

Cycling past the last float returns focus to the cursor, so one key both enters
and leaves the float layer and there is no mode to be stuck in.

This command exists because focus is a *place in the tree* (D18) and a float is
deliberately not in the tree.  That is the right model — it is what makes
'move one cell left whether or not anything is there' mean something — but it
leaves floats with no way to be focused at all, and a floating window you
cannot type into is not a floating window.  WORLD-FOCUSED-FLOAT is the one slot
that fixes it, and this is the verb that sets it."
  (with-relayout
    (let* ((floats (remove-if-not (lambda (f) (c:window-live-p (c:float-window f)))
                                  (c:world-floats *world*)))
           (current (c:world-focused-float *world*))
           (position (position current floats)))
      (setf (c:world-focused-float *world*)
            (cond ((null floats) nil)
                  ((null position) (first floats))
                  (t (let ((next (+ position step)))
                       (when (< -1 next (length floats)) (nth next floats))))))
      (request-manage)
      (c:world-focused-float *world*))))

(defcommand focus-tiled ()
  "Take keyboard focus off any floating window and give it back to the cursor."
  (with-relayout
    (setf (c:world-focused-float *world*) nil)
    (request-manage)))

(defcommand raise-float ()
  "Put the focused floating window on top of the other floats."
  (with-relayout
    (let ((float (c:world-focused-float *world*)))
      (when float
        (setf (c:world-floats *world*)
              (append (remove float (c:world-floats *world*)) (list float)))))))

(defcommand close-float ()
  "Close the focused floating window."
  (let ((float (c:world-focused-float *world*)))
    (when float
      (let ((window (c:float-window float)))
        (when (c:window-proxy window)
          (guarded "close" (w:window-close (c:window-proxy window))))))))
