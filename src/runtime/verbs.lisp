;;;; runtime/verbs.lisp --- The commands.
;;;;
;;;; DESIGN's P1 says that where a fork is situational rather than principled,
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

;;; Undo is *not* wrapped around each verb here.  It is installed once, around
;;; RUN-COMMAND, in runtime/history.lisp -- so it covers every command in the
;;; system including the ones a user writes, and no verb has to remember to opt
;;; in.  A verb that changes nothing records nothing, because the check is on
;;; the tree's signature rather than on which function was called.

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

This is DESIGN D17's resize idiom, and the empty pane it makes is a
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
                           :axis axis :side side
                           :join-p (p:split-join-predicate (policy)))
        (setf (c:world-root *world*) root)
        (p:jump-cursor (policy) *world* landed)))))

(defcommand (close-window "close") ()
  "Close the focused window, and take its pane with it.

The sibling grows to fill the space.  DESIGN D17's CLOSE; see CLEAR for the
other half."
  (with-relayout
    (let ((window (focused-window)))
      (if window
          (close-window-later window)
          ;; An empty pane has nothing to close, so closing it means removing
          ;; the pane — which is what the user meant.
          (remove-pane)))))

(defcommand clear ()
  "Empty the focused pane but leave it standing.

You are now *in* an empty pane, which still occupies its space.  DESIGN D17's
CLEAR: a separate verb from CLOSE rather than a mode, because they are
different intentions and a mode would make you remember which one you were in.

The window itself is closed; what survives is the place."
  (with-relayout
    (let ((leaf (current-leaf)))
      (when leaf
        (let ((window (c:leaf-window leaf)))
          (setf (c:leaf-window leaf) nil)
          (when window
            (close-window-later window)))))))

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

(defcommand (resize-pane "resize") (direction &optional (amount p:*resize-amount*))
  "Grow the focused pane DIRECTION by AMOUNT of its container's total.

AMOUNT is a *fraction of the container*: 1/20 makes the pane one twentieth of
the container wider and its neighbour one twentieth narrower.  That is what the
sentence above always claimed and what the code now does — it used to multiply
by an undocumented constant 4, which happened to be right only when the
container held exactly four children, and named no unit anywhere.

Resizing is a *transfer* between two adjacent children rather than an
assignment, so dragging one divider never disturbs the divider beyond it —
which is the single most common complaint about tiling resize.

Because the shares are relative, this behaves identically at every zoom level
and on every monitor, and needs to know nothing about pixels."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path))
           (axis (c:direction-axis direction))
           (sign (c:direction-sign direction)))
      ;; Find the nearest ancestor that divides space along this axis: pressing
      ;; "wider" inside a vertical split means widening the column that split
      ;; is in, which is what the user meant and what they would have had to
      ;; navigate out to do by hand.
      (loop for depth from (length path) downto 1
            for container = (c:resolve-path root (subseq path 0 (1- depth)))
            for address = (nth (1- depth) path)
            when (eq (p:container-axis (policy) container) axis)
              do (let* ((last (1- (c:container-count container)))
                        ;; At the far edge there is no neighbour on that side,
                        ;; so the transfer has to go the other way to mean
                        ;; anything.
                        (delta (if (and (eql address last) (plusp sign))
                                   (- amount)
                                   (* sign amount))))
                   (p:resize-container (policy) container address delta))
                 (return t)))))

(defcommand equalize ()
  "Give every pane in the focused container an equal share.

Applies to the nearest enclosing split.  With a prefix of nothing else, this is
the command people reach for after ten minutes of resizing."
  (with-relayout
    (let* ((root (c:world-root *world*))
           (path (current-path))
           (parent (c:resolve-path root (c:parent-path path))))
      (p:equalize-container (policy) parent))))

(defcommand equalize-all ()
  "Give every pane in the whole workspace an equal share."
  (with-relayout
    (let ((policy (policy)))
      (c:map-nodes (lambda (node) (p:equalize-container policy node))
                   (c:current-workspace *world*)))))

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
      (multiple-value-bind (keep remove)
          (p:tab-siblings (policy) parent address)
        (when keep
          (let* ((a (c:child-at parent keep))
                 (b (c:child-at parent remove))
                 (stack (c:make-stack (list a b) 0)))
            (c:remove-child parent remove)
            (setf (c:child-at parent keep) stack)
            (p:jump-cursor (policy) *world*
                           (c:node-path-to root stack))))))))

(defun enclosing-alternatives (&optional (path (current-path)))
  "The nearest container at or above PATH that holds alternatives, and its path.

Returns (values CONTAINER PATH) or NIL.  `Alternatives' rather than `stack'
because that is the property the verbs need — a set of children of which one is
current — and a container kind from outside the core that has it should get
tabbing and workspace switching without the core having heard of it.  Asking
(TYPEP CONTAINER 'C:STACK) here made all four of those verbs blind to any such
kind, and no method anywhere could say otherwise."
  (let ((root (c:world-root *world*)))
    (loop for depth from (length path) downto 0
          for prefix = (subseq path 0 depth)
          for container = (c:resolve-path root prefix)
          when (and (c:container-p container)
                    (c:container-alternatives-p container))
            do (return (values container prefix)))))

(defcommand tab-next (&optional (step 1))
  "Show the next tab of the nearest enclosing stack.

Also how you switch workspaces, because a workspace list *is* a stack — see
NEXT-WORKSPACE, which is this command aimed at the root."
  (with-relayout
    (multiple-value-bind (container prefix) (enclosing-alternatives)
      (when container
        (let ((n (c:container-count container)))
          (when (plusp n)
            (setf (c:container-selection container)
                  (mod (+ (or (c:container-selection container) 0) step) n))
            (p:jump-cursor (policy) *world*
                           (c:repair-path (c:world-root *world*) prefix))
            t))))))

(defcommand tab-previous ()
  "Show the previous tab of the nearest enclosing stack."
  (tab-next -1))

(defcommand untab ()
  "Dissolve the nearest enclosing stack back into a split.

Every child becomes a pane side by side, which is the inverse of TAB: the
alternatives stop being alternatives and all become visible at once."
  (with-relayout
    (multiple-value-bind (container prefix) (enclosing-alternatives)
      (when container
        (let* ((children (loop for address in (c:container-addresses container)
                               for child = (c:child-at container address)
                               when child collect child))
               (split (c:make-split :horizontal children)))
          (when children
            (setf (c:world-root *world*)
                  (c:tree-replace-at (c:world-root *world*) prefix split))
            (p:jump-cursor (policy) *world*
                           (c:repair-path (c:world-root *world*) (current-path)))
            t))))))

;;; =========================================================== workspaces

(defun workspace-stack ()
  "The workspace container, or NIL if the root is not one."
  (c:world-workspaces *world*))

(defun fresh-workspace (index)
  "What a workspace at INDEX is made of, as the policy answers it.

Never (C:MAKE-LEAF) at a call site.  A workspace's contents are a policy
decision — it is how the lattice makes every workspace a plane — and four
sites each building their own empty pane is how three of them come to disagree
with the fourth.  See P:MAKE-WORKSPACE."
  (or (guarded "make-workspace" (p:make-workspace (policy) *world* index))
      (c:make-leaf)))

(defun show-workspace (stack index)
  "Put workspace INDEX on the screen and stand the cursor in it.

THE OUTPUT AND THE STACK ARE TWO DIFFERENT FACTS AND BOTH HAVE TO MOVE.  The
stack knows which of its children is *selected*; the output knows which one it
is *displaying*; and P:OUTPUT-CONTENT — the thing that decides what gets laid
out at all — reads the output's answer and not the stack's.  On one monitor
they look like the same fact right up until they disagree.

NEW-WORKSPACE made them disagree.  It set the selection and jumped the cursor
and never touched the output, so `add a workspace and switch to it' added one,
moved the cursor into it, and left the screen showing the workspace you came
from — with the windows still on it, the status line confidently reporting the
new number, and every key doing something invisible.  That is the same failure
RESTORE-OUTPUT-WORKSPACES has a paragraph about, reached by a different road,
and it survived because the model was right the whole time.

So both commands go through here.

AND THROUGH SHOW-WORKSPACE-ON, which is the half that only matters on two
monitors: asking for a workspace the other screen is already showing used to
put both screens on it, and two outputs showing one workspace is a collapsed
layout and a blank second monitor.  They trade there."
  (let ((output (current-output)))
    (show-workspace-on output index)
    (setf (c:container-selection stack) index)
    (p:jump-cursor (policy) *world* (list index))
    (run-hooks :workspace-changed index)
    (notify "~a ~d" (p:world-role-name *world* stack) (1+ index))
    index))

(defun grow-workspaces-to (stack index)
  "Make sure workspace INDEX exists, creating every workspace up to it.

This is where 'infinite workspaces' is actually implemented, and it is four
lines because a stack grows: asking for workspace 40 on a machine with three
makes 40, and every one of them is whatever the policy says a workspace is.

Returns the node now at INDEX."
  (loop while (<= (c:container-count stack) index)
        do (let ((next (c:container-count stack)))
             (c:insert-child stack next (fresh-workspace next))))
  (c:child-at stack index))

(defcommand workspace (number)
  "Switch to workspace NUMBER, creating it and any before it if needed.

*Workspaces are numbered from one*, because that is what is written on the key
you press to reach them and what everybody says out loud.  The index into the
stack is one less, and that conversion happens here rather than leaking into
the keymap, the help screen and every conversation about the thing.

Workspaces are a stack at the root of the tree, so this is the same operation
as switching a tab — and 'infinite workspaces' costs nothing, because a stack
grows."
  (with-relayout
    (let ((stack (workspace-stack))
          (index (max 0 (1- number))))
      (when stack
        (grow-workspaces-to stack index)
        ;; The *output* changes workspace, not the world.  With one monitor
        ;; these are the same statement; with two they are emphatically not,
        ;; and treating them as the same is how a second monitor ends up
        ;; mirroring the first.  See SHOW-WORKSPACE.
        (show-workspace stack index)))))

(defcommand next-workspace (&optional (step 1))
  "Switch to the next workspace, wrapping."
  (with-relayout
    (let ((stack (workspace-stack)))
      (when stack
        (workspace (1+ (mod (+ (c:container-selection stack) step)
                            (max 1 (c:container-count stack)))))))))

(defcommand previous-workspace ()
  "Switch to the previous workspace, wrapping."
  (next-workspace -1))

(defcommand new-workspace ()
  "Add a workspace after the current one and switch to it."
  (with-relayout
    (let ((stack (workspace-stack)))
      (when stack
        (let ((index (1+ (c:container-selection stack))))
          (c:insert-child stack index (fresh-workspace index))
          ;; SHOW-WORKSPACE rather than the four lines it replaced, because one
          ;; of the four was missing here and the screen stayed where it was.
          (show-workspace stack index))))))

(defcommand send-to-workspace (number &key (follow nil))
  "Move the focused pane to workspace NUMBER, counting from one.

This is TREE-MOVE and nothing else — because a workspace is a container and a
pane is a subtree, 'send to workspace' needed no code of its own.

IT LANDS ON A *PLACE INSIDE* THE WORKSPACE, NOT ON THE WORKSPACE.  The
difference is invisible while a workspace is a single pane and destructive the
moment it is anything else.  It used to address the workspace node, which was
right for the empty leaf the core ships and wrong for every richer shape: send
a window to a lattice workspace and the whole *plane* became one half of a
split, viewport and all, with the window as the other half.  A plane is not a
pane and must not be treated as one by anything that can also be handed a pane.

DESCEND-TO-LEAF with no direction is the question actually being asked — 'where
inside this thing does a non-directional arrival go?' — and it is answered by
ENTRY-ADDRESS, per container kind.  A leaf answers with itself, so the core
behaviour is unchanged to the character.  A grid answers with a cell, which is
how a window sent to another plane arrives in a cell of it rather than beside
it.  Neither this command nor the core knows which happened.

THE EMPTY-WORKSPACE PATH USED TO DUPLICATE THE WINDOW, and it is worth writing
down because the shape of the mistake recurs.  Every surgery function in
model/ is *pure*: it returns (values REMOVED NEW-ROOT NEW-PATH) and mutates
nothing above the subtree it was handed.  The old code bound only the first
value, ignored it, discarded the new root entirely, and then called
TREE-REPLACE-AT on the *original* root — which still contained the node at
FROM.  So the removal never happened and the window ended up in both
workspaces, live in two places at once.

Callers that ignore a surgery function's second value have a bug they have not
noticed yet; this one had it for the life of the command."
  (with-relayout
    (let ((stack (workspace-stack))
          (from (current-path))
          (index (max 0 (1- number))))
      (when (and stack (> (length from) 1))
        (let* ((workspace (grow-workspaces-to stack index))
               ;; The path is rebuilt from the *node* rather than written as
               ;; (LIST INDEX), because the workspace stack is only the root by
               ;; convention and a configuration that nests it would make the
               ;; literal wrong in a way nothing would report.
               (to (or (guarded "descend-to-leaf"
                         (p:descend-to-leaf
                          (policy) workspace
                          (c:node-path-to (c:world-root *world*) workspace) nil))
                       (list index))))
          (multiple-value-bind (root landed)
              ;; One call for both cases: TREE-MOVE already rules that an empty
              ;; pane simply becomes the thing moved, so 'the target is empty'
              ;; needs no branch here and never needed one.
              (c:tree-move (c:world-root *world*) from to
                           :join (p:move-into-occupied (policy) *world* from to))
            (setf (c:world-root *world*) root)
            (cond
              (follow
               (setf (c:container-selection stack) index)
               (p:jump-cursor (policy) *world* landed))
              (t
               (p:repair-cursor (policy) *world* from)
               (notify "sent to ~a ~d" (p:world-role-name *world* stack) (1+ index))))
            landed))))))

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
             ;; ON ITS OWN SLOT, NOT ON HOME-PATH.  This used to write
             ;; C:WINDOW-HOME-PATH, which is where ON-MINIMIZE records where a
             ;; window came from and what ON-RESTORE reads.  So minimizing a
             ;; window, then floating and unfloating anything at all, then
             ;; restoring, put the restored window somewhere it had never been
             ;; — one verb quietly overwriting another's memory.
             (setf (c:prop window :float-home-path) path)
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

From M-x this is the run-a-program prompt, and what you type is split on
spaces the way a shell would split it.

The child is detached and its output goes nowhere, so a program that writes to
stderr cannot fill a pipe nobody is reading and block."
  (:interactive :shell-command)
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
  (run-shutdown-once)
  (when (server-manager *server*)
    (guarded "exit_session" (w:wm-exit-session (server-manager *server*))))
  (setf (server-running *server*) nil))

(defcommand restart-wm ()
  "Stop the window manager without ending the session.

River keeps running, and so does every application; the window manager is an
ordinary Wayland client.  Relaunch it and the layout comes back, because
persistence is keyed on river's stable window identifiers."
  (run-shutdown-once)
  (setf (server-running *server*) nil))

(defcommand captures ()
  "Say what is being recorded right now, and by how many sessions.

The status line carries a standing REC while anything is being captured, which
answers `is something recording?'.  This answers the next question — *what* —
and it is worth a command of its own because the answer includes windows you
cannot see: a minimized window, or one four cells away, goes on being recorded
exactly as it was, which is the whole reason river reports the count per window
as well as per screen."
  (let ((captures (c:world-captures *world*)))
    (notify "~:[nothing is being recorded~;recording: ~:*~{~a~^, ~}~]"
            (mapcar (lambda (subject)
                      (let ((count (capture-sessions-of subject)))
                        (format nil "~a~@[ (~d)~]"
                                (capture-subject-name subject)
                                (and (> count 1) count))))
                    captures))
    captures))

(defcommand describe-key (spec)
  "Say what SPEC is bound to, and what that does.  Emacs's C-h k.

SPEC is written the way a binding is: Super+Return, Ctrl+Super+h, C-x."
  (:interactive :key)
  (let* ((key (handler-case (kbd spec) (error (condition)
                                         (notify "~a" condition)
                                         (return-from describe-key nil))))
         (target (lookup-key *keymap* key)))
    (notify "~a: ~a" (key-to-string key)
            (if target (p:binding-description (policy) target) "unbound"))
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
          (close-window-later window))))))
