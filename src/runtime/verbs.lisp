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

;;; --------------------------------------------------- by name
;;;
;;; THE DOOR THAT WAS MISSING.  There were four ways to reach a window by name
;;; and every one of them needed something else established first:
;;; SWITCH-TO-BUFFER wants a name you gave the window, JUMP-TO-TAG a tag you
;;; set, TAB-ADD pulls the window *in* rather than focusing it where it is,
;;; and the keyboard overview finds windows by rearranging the tree.  None of
;;; them answers "the window whose title that is", asked about a window you
;;; have not labelled and cannot see.
;;;
;;; FOCUS-EXISTING-WINDOW lives here rather than in tags.lisp, where it was
;;; born, because it is not about tags: two callers in that file and this one
;;; use it, and this file loads first, so a call to it from here would be the
;;; forward reference gate 1 refuses.

(defun focus-existing-window (window)
  "Put the cursor on WINDOW wherever it is, switching workspace if needed.

The workspace switch is the part that matters and the part that is easy to
forget: a window on another workspace can be focused perfectly correctly and
still be on no screen, which looks exactly like the command having done
nothing.

A float takes the keyboard where it stands.  That is not a shortcut: focus is
a *place in the tree* (D18) and a float is deliberately not in the tree, so
there is no cell to move the cursor to — WORLD-FOCUSED-FLOAT is the slot that
says the keyboard is somewhere else."
  (let ((float (c:float-of-window *world* window)))
    (cond
      (float
       (setf (c:world-focused-float *world* p:*seat*) float)
       (mark-dirty)
       (request-manage))
      (t
       (let* ((root (c:world-root *world*))
              (leaf (c:leaf-holding root window))
              (path (and leaf (c:node-path-to root leaf))))
         (when path
           (let ((stack (workspace-stack)))
             (when (and stack (first path))
               (setf (c:container-selection stack) (first path))
               ;; Through SHOW-WORKSPACE-ON, so that following a window to a
               ;; workspace the *other* monitor is showing trades for it rather
               ;; than putting both monitors on one workspace.
               (show-workspace-on (current-output) (first path))))
           (p:jump-cursor (policy) *world* path)
           (mark-dirty)
           (request-manage)
           path))))))

(defun world-windows (&optional (world *world*))
  "Every window WORLD holds a place for: tiled, put away, or floating.

ASKED OF THE WORLD AND NOT OF THE COMPOSITOR, which is the difference between
a picker that can be trusted and one that cannot.  ALL-WINDOWS answers with
river's list, and that list holds a window which has arrived and has not been
placed yet -- a window with no path, so a picker offering it is offering an
entry that does nothing when you choose it.  The world's answer is the windows
this program has a place for, each exactly once, in a stable order; it is also
the same answer with no compositor attached, which is what lets this be tested."
  (let ((seen (make-hash-table :test #'eq))
        (found '()))
    (flet ((note (window)
             (when (and window (not (gethash window seen)))
               (setf (gethash window seen) t)
               (push window found))))
      (dolist (window (c:node-windows (c:world-root world))) (note window))
      (dolist (window (c:world-scratchpad world)) (note window))
      (dolist (float (c:world-floats world)) (note (c:float-window float))))
    (nreverse found)))

(defun window-place-name (window &optional (world *world*))
  "Where WINDOW is, in two words, for a picker that has to tell two apart.

The words are the ones the rest of the program says: the workspace's role name
and its number counting from one, `floating', or `scratchpad'.

ASKED OF THE WORLD RATHER THAN OF THE WINDOW'S FLAGS, for the same reason
WORLD-WINDOWS is: WINDOW-MINIMIZED-P is a disposition something sets, and the
scratchpad is where the window is.  They agree in every path the program has,
and where they would disagree the place is the answer the picker needs."
  (cond
    ((member window (c:world-scratchpad world)) "scratchpad")
    ((c:float-of-window world window) "floating")
    (t
     (let* ((root (c:world-root world))
            (leaf (c:leaf-holding root window))
            (path (and leaf (c:node-path-to root leaf)))
            (stack (c:world-workspaces world)))
       (if (and stack path (integerp (first path)))
           (format nil "~a ~d" (p:world-role-name world stack) (1+ (first path)))
           "tiled")))))

(defun window-picker-entries (&optional (windows (world-windows)))
  "WINDOWS paired with the label a picker offers for each: (LABEL . WINDOW).

THE BARE NAME WHERE IT IS A NAME, AND THE PLACE WHERE IT IS NOT.  A window's
name is not an address -- two windows may share one and nothing breaks, which
is the test P:NAME-OF-WINDOW's docstring sets -- so a picker built on the bare
name would offer `firefox' twice and make Return a coin flip.  Where a name is
unique it is offered alone, because that is the whole point of asking by name;
where it is not, each candidate is spelled with where it is, and numbered if
two windows share a place as well.

The list is sorted by label rather than left in traversal order: a prompt you
scan is read alphabetically, and a completion that reorders itself when a
window opens somewhere else is worse than one that does not."
  (let ((shared (make-hash-table :test #'equal))
        (used (make-hash-table :test #'equal))
        (entries '()))
    (dolist (window windows)
      (incf (gethash (p:name-of-window window) shared 0)))
    (dolist (window windows)
      (let* ((name (p:name-of-window window))
             (base (if (> (gethash name shared) 1)
                       (format nil "~a (~a)" name (window-place-name window))
                       name))
             (count (incf (gethash base used 0)))
             (label (if (= count 1) base (format nil "~a <~d>" base count))))
        (push (cons label window) entries)))
    (sort entries #'string< :key #'car)))

(defun window-by-picker-name (text &optional (windows (world-windows)))
  "The window a picker's TEXT names, or NIL when it names none of them.

Three readings, in order, because a prompt is typed at as often as it is
completed: an exact label, then a bare name only one window carries, then a
prefix only one label matches.  Ambiguity answers NIL rather than guessing --
focusing the wrong window is a visible act, and the picker's job is to say
*which one*."
  (let ((entries (window-picker-entries windows))
        (text (string-trim " " text)))
    (flet ((only (candidates)
             (when (and candidates (null (rest candidates))) (first candidates))))
      (or
       ;; The label the prompt showed, which is also what completing a prefix
       ;; leaves in the input.
       (cdr (assoc text entries :test #'string=))
       ;; A bare name: the completion prompt only ever shows the bare name when
       ;; it is unambiguous, so typing it must mean the same thing.
       (cdr (only (remove-if-not (lambda (entry)
                                   (string= text (p:name-of-window (cdr entry))))
                                 entries)))
       ;; A prefix of one label, so that the first letters of a disambiguated
       ;; entry are enough to reach it.
       (cdr (only (remove-if-not
                   (lambda (entry)
                     (let ((label (car entry)))
                       (and (plusp (length text))
                            (<= (length text) (length label))
                            (string-equal text label :end2 (length text)))))
                   entries)))))))

(define-argument-type :window "window: "
  :documentation "Any window of this session, by name, with completion over all of them."
  :candidates (mapcar #'car (window-picker-entries))
  :parse (lambda (text)
           (or (window-by-picker-name text)
               (error "no window matches ~a" text))))

(defcommand focus-window-by-name (window)
  "Focus the window you name, wherever it is.

Every window this session has is offered with completion -- on any workspace,
inside a lattice cell, put away on the scratchpad, or floating over the top --
and the one you choose is the one you are typing into, with nothing
rearranged.  That is the gap this fills: the other by-name doors need a name
or a tag you set first, and this one needs only what is written on the window.

A window that was put away is restored first.  A window on the scratchpad is
on no screen, so `focus it where it is' would otherwise be a command that
looks like it did nothing; RESTORE puts it back in the slot it left, and the
cursor follows it there."
  (:interactive :window)
  (cond
    ((null window) (notify "no window matches") nil)
    (t
     (when (c:window-minimized-p window) (restore-window window))
     (focus-existing-window window)
     ;; The place is part of the answer, not decoration: the cursor can cross a
     ;; workspace without the window changing at all, and a notification that
     ;; only repeated the name you just chose would leave you unsure it had.
     (notify "~a (~a)" (p:name-of-window window) (window-place-name window))
     window)))
;;; ============================================================ structure

;;; ------------------------------------------------------------- by name
;;;
;;; #128.  Tags gave windows names and FOCUS-WINDOW-BY-NAME made a name a way to
;;; reach one.  This makes a name an *argument* to everything else: `close the
;;; thing I tagged browser', from a key binding, without caring which window
;;; happens to have the keyboard.

(defcommand (with-window "with") (window command)
  "Run COMMAND with the window you name as the one it acts on.

Every verb in this system asks FOCUSED-WINDOW who it is working on, so naming a
window here is enough to aim any of them at it -- including verbs an extension
adds, which need to know nothing about this.  COMMAND is then asked for its own
arguments as usual, which is why this is a wrapper and not a `&key (window ...)'
on five hundred commands.

The window is not *focused*: nothing is told that it has the keyboard, and after
the command runs the keyboard is wherever it was.  That distinction matters for
anything that reads the world rather than FOCUSED-WINDOW -- the echo area, the
pointer, the compositor -- and it is why the override is a dynamic binding
rather than a change to the focus model."
  (:interactive :window :command)
  (let ((inner (find-command command)))
    (cond
      ((null window) (notify "no window matches"))
      ((null inner) (notify "no such command: ~a" command))
      ((eq inner (find-command "with"))
       (notify "with-window cannot run itself"))
      (t
       (let ((arguments (command-arguments inner)))
         (if (and arguments (not (command-interactive-p inner)))
             (notify "~a takes ~{~(~a~)~^ ~} -- run it from a REPL"
                     (command-name inner) (command-lambda-list inner))
             (with-focus-on window
               (if (null arguments)
                   (progn (run-command (command-name inner)) (after-command))
                   (read-arguments
                    inner arguments
                    (lambda (values)
                      (apply #'run-command (command-name inner) values)
                      (after-command)))))))))))

;;; ------------------------------------------------------------ focus history
;;;
;;; #17.  The recency list already existed -- :MRU consults it when the window
;;; you close is the focused one -- so what was missing was the walk.  These are
;;; the four commands, and they are thin on purpose: the list, the cursor and
;;; the stepping rules live in the policy (P:FOCUS-HISTORY-STEP), because they
;;; are about focus and not about keys.

(defun focus-history-jump (window)
  "Go to WINDOW and say where it is, or say there was nothing to go to."
  (if (null window)
      (notify "no window that way in the focus history")
      (progn
        (when (c:window-minimized-p window) (restore-window window))
        (focus-existing-window window)
        (notify "~a (~a)" (p:name-of-window window) (window-place-name window)))))

(defcommand focus-back (&optional (step 1))
  "Focus the window you were in before this one.  Give it a number to walk back
further; each one is a window you have actually been in, most recent first.

Windows that have closed or been put away since are stepped over, so this does
not land on nothing, and walking back does not reset the walk -- only an
unexpected focus change does.  FLIP: FOCUS-FORWARD comes back the way you went."
  (focus-history-jump (p:focus-history-step *world* step)))

(defcommand focus-forward (&optional (step 1))
  "Focus the window you walked back away from.  The other half of FOCUS-BACK."
  (focus-history-jump (p:focus-history-step *world* (- step))))

(defcommand focus-history ()
  "List every window focus has been through, most recent first, marking where
the walk stands and which entries are no longer on a screen."
  (let ((rows (loop for (window . reachable) in (p:focus-history-entries *world*)
                    for index from 0
                    collect (cons (if (= index (p:focus-history-cursor *world*))
                                      (format nil "~d*" index)
                                      (format nil "~d" index))
                                  (format nil "~a~@[ (~a)~]~:[ -- gone~;~]"
                                          (p:name-of-window window)
                                          (window-place-name window)
                                          reachable)))))
    (if (null rows)
        (notify "no focus history yet")
        (show-help-page (format nil "focus history -- ~d" (length rows)) rows))))

(defcommand clear-focus-history ()
  "Forget every window but the one you are in now."
  (let ((left (p:forget-focus-history *world*)))
    (notify "focus history cleared~@[ (~d left)~]" (and left (length left)))))

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
layout and a blank second monitor.  They trade there.

AND YOU ARRIVE WHERE YOU LEFT OFF.  This jumped to (LIST INDEX), which
descends to the workspace's *first* leaf — so every visit to a workspace put
you at its top left, whatever you had been doing there.  P:REMEMBERED-PLACE
answers with the place if there is one and with the workspace itself if there
is not, which is what this line meant both times."
  (let ((output (current-output)))
    (show-workspace-on output index)
    (setf (c:container-selection stack) index)
    (p:jump-cursor (policy) *world* (p:remembered-place *world* index))
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

;;; ------------------------------------------------------------- the screens
;;;
;;; EIGHTY COMMANDS AND NOT ONE OF THEM NAMED AN OUTPUT.  Per-output workspaces
;;; have worked for a while — each monitor displays its own, they trade rather
;;; than collide, and the arrangement survives an undock — and there was no
;;; verb that took you to the other screen or sent anything there.  The only
;;; door was `workspace N', which puts N on the screen you are already looking
;;; at and trades away whatever was there: a real workflow, and the only one.
;;;
;;; Both of these are compositions and neither has an algorithm in it.  The
;;; cursor is one place in one model (SHOW-WORKSPACE-ON's ruling), so `focus
;;; the other screen' is a jump into the workspace that screen displays, and
;;; `send this there' is SEND-TO-WORKSPACE with the number looked up rather
;;; than typed.  That is the whole of what the ruling costs, and it is why
;;; neither of these needed the model to grow a field.

(defun resolve-output (which)
  "The output WHICH names: a direction, a number counting from one, or itself.

Directions are relative to the screen the cursor is on and are the same four
words every other verb in this file takes, so `the next screen right' and `the
next pane right' are one vocabulary rather than two."
  (etypecase which
    (c:output which)
    (keyword (p:output-in-direction
              *world* (p:output-showing *world* (first (current-path))) which))
    (integer (nth (1- which) (all-outputs)))))

(defcommand focus-output (which)
  "Move the cursor to another screen — a direction, or a number from one.

Nothing on either screen changes: no window moves and neither monitor is asked
to display anything different.  You arrive where you last were on that screen
rather than at its first pane, which is *REMEMBER-PLACE* and is the difference
between a command worth pressing and one you try once.

You will not often need this, and that is deliberate: plain motion crosses the
screen boundary on its own when you walk off the edge — see
*MOTION-CROSSES-OUTPUTS*.  This is the version for when you are in the middle
of a screen and do not want to walk."
  (:interactive :direction)
  (with-relayout
    (let* ((output (resolve-output which))
           (index (and output (p:output-workspace output))))
      (when (integerp index)
        (p:jump-cursor (policy) *world* (p:remembered-place *world* index))))))

(defcommand send-to-output (which &key (follow nil))
  "Move the focused pane to another screen — a direction, or a number from one.

SEND-TO-WORKSPACE with the number looked up instead of typed, so everything it
argues about landing on a *place inside* the workspace rather than on the
workspace holds here without being restated.  FOLLOW takes you with it."
  (:interactive :direction)
  (let* ((output (resolve-output which))
         (index (and output (p:output-workspace output))))
    (when (integerp index)
      (send-to-workspace (1+ index) :follow follow))))

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
  (send-path-to-workspace (current-path) number :follow follow))

(defun send-path-to-workspace (from number &key follow)
  "Move the pane at path FROM to workspace NUMBER.  The landed path, or NIL.

FROM rather than the cursor, so that BATCH-SEND-TO-WORKSPACE can move a window
that is not the focused one without walking the cursor to it first: that would
fire :FOCUS-CHANGED once per window and leave the user standing three
workspaces away from where they were."
  (with-relayout
    (let ((stack (workspace-stack))
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

;;; ------------------------------------------------------- selection

(defvar *selection* '()
  "Windows marked for a batch verb, in the order they were marked.

A list rather than a flag on the window: marking is a thing you did *for this
operation*, not a property of the window, and a window that carried a
SELECTED-P bit would need somebody to remember to clear it on close, on
restore, on session load, and after the batch that used it.")

(defun selected-windows ()
  "The marked windows that still exist, oldest mark first.

Windows are dropped from the selection as soon as the compositor says they
are gone (WINDOW-LIVE-P), so a batch verb can never act on a dead proxy — the
same reasoning as the spawn latch dropping a stale path: the world moves
between the mark and the act."
  (setf *selection* (remove-if-not #'c:window-live-p *selection*)))

(defun mark-selected (window)
  "Add WINDOW to the selection if it is not there, remove it if it is.

Returns T when it is now selected.  Toggling rather than adding is what makes
one binding do both jobs — the mistake people make with a selection is not
selecting too little, it is being unable to take one back."
  (let ((selected (not (member window (selected-windows)))))
    (setf *selection*
          (if selected
              (append *selection* (list window))
              (remove window *selection*)))
    selected))

(defun clear-selected ()
  "Forget the whole selection."
  (setf *selection* '()))

(defcommand select-window ()
  "Mark the focused window for a batch verb, or unmark it.

Selection is deliberately container-agnostic: a window marked on one
workspace, in a lattice cell, or in another output's tree is still marked,
and the batch verbs act on it wherever it is.  Nothing about the selection is
visible on screen yet; LIST-SELECTED is how you ask."
  (let ((window (focused-window)))
    (when window
      (let ((selected (mark-selected window)))
        (notify "~a ~a (~d selected)"
                (if selected "selected" "deselected")
                (or (c:window-app-id window) "window")
                (length (selected-windows)))
        selected))))

(defcommand clear-selection ()
  "Unmark every window."
  (let ((count (length (selected-windows))))
    (clear-selected)
    (notify "cleared ~d selected window~:p" count)
    count))

(defcommand list-selected ()
  "Show the marked windows, in the order they were marked."
  (let ((windows (selected-windows)))
    (notify "~d selected: ~{~a~^, ~}" (length windows)
            (mapcar (lambda (window)
                      (or (c:window-app-id window) "window"))
                    windows))
    windows))

(defcommand batch-close ()
  "Ask every selected window to close, then forget the selection."
  (let ((closed (act-on-windows (selected-windows) #'close-window-later)))
    (clear-selected)
    (notify "closing ~d selected window~:p" (length closed))
    (length closed)))

(defcommand batch-minimize ()
  "Take every selected window out of the tree, then forget the selection.

Each one lands on the scratchpad exactly as MINIMIZE would have put it there,
so RESTORE and RESTORE-LAST bring them back."
  (let ((went (act-on-windows (selected-windows) #'minimize-window)))
    (clear-selected)
    (notify "minimized ~d selected window~:p" (length went))
    (length went)))

(defcommand batch-send-to-workspace (number)
  "Move every selected pane to workspace NUMBER, counting from one.

Sent from each pane's own path rather than by walking the cursor to it, so
the :focus-changed hook fires once for the batch rather than once per window.
The cursor is put back on the *leaf you were standing in* afterwards: the
single-window verb leaves the cursor at the repaired source, which for a batch
would walk you through every pane it moved.  If the pane you were standing in
was itself marked, the cursor follows it — you sent the place you were in, and
standing anywhere else would be a lie."
  (let ((windows (selected-windows))
        ;; THE WINDOW, NOT THE LEAF.  Tree surgery rebuilds the nodes it walks,
        ;; so a leaf object captured before a move is no longer in the tree and
        ;; NODE-PATH-TO answers NIL for it -- which reads as "the place you were
        ;; standing is gone" when you are standing in it.
        (start (current-window))
        (sent 0))
    (dolist (window windows)
      (let* ((root (c:world-root *world*))
             (leaf (c:leaf-holding root window))
             (from (and leaf (c:node-path-to root leaf))))
        (when (send-path-to-workspace from number)
          (incf sent))))
    (let* ((root (c:world-root *world*))
           (leaf (and start (c:leaf-holding root start))))
      (p:jump-cursor (policy) *world*
                     (or (and leaf (c:node-path-to root leaf))
                         (c:repair-path root (c:world-cursor *world*)))))
    (clear-selected)
    (notify "sent ~d selected window~:p to workspace ~d" sent number)
    sent))

;;; ============================================== floating, fullscreen, etc.

(defcommand toggle-float ()
  "Float the focused window, or put a floating one back into the tree.

Floating is per window and chosen, never inferred and forced: SHOULD-FLOAT-P
only supplies the initial guess, and this is how you overrule it."
  ;; The surgery lives in FLOAT-AWAY-FROM-TREE, beside the other window-state
  ;; changes, so that TRANSITION-WINDOW (#25) floats a window the same way this
  ;; key does rather than growing a second copy of the same four lines.
  (with-relayout
    (let ((window (focused-window)))
      (cond
        ((null window) nil)
        ((c:window-floating-p window) (unfloat-window window))
        (t (float-away-from-tree (policy) window))))))

(defcommand toggle-fullscreen ()
  "Make the focused window the only thing on its output, or stop.

Cheap in both directions: river ignores clip boxes and does not draw borders
while fullscreen, so entering and leaving cost no relayout."
  (let ((window (focused-window)))
    (when window
      (transition-window window (if (c:window-fullscreen-p window)
                                    :normal :fullscreen)))))

(defcommand minimize ()
  "Take the focused window out of the tiling tree entirely.

The remaining windows retile without it.  Minimize is not 'hide it somewhere',
it is 'take it out of the layout' — and where it went is the scratchpad, which
RESTORE-LAST brings it back from."
  (let ((window (focused-window)))
    (when window (transition-window window :minimized))))

(defcommand restore-last ()
  "Bring back the most recently minimized window.

To the slot it was minimized from if that slot still exists, and to the cursor
otherwise."
  (let ((window (first (c:world-scratchpad *world*))))
    (when window (transition-window window :visible))))

;;; -------------------------------------------------------- bulk verbs

(defun current-workspace-windows ()
  "Every tiled window on the workspace the cursor is in, or NIL.

Floats belong to the output rather than to a workspace — a dialog does not
live on a plane — so a bulk verb that cleaned the screen with them would be
closing windows that were never on it.  They are deliberately left alone; a
float is dismissed with CLOSE-FLOAT."
  (let ((workspace (c:current-workspace *world*)))
    (and workspace (c:node-windows workspace))))

(defun act-on-windows (windows action &key except)
  "Call ACTION on each of WINDOWS but EXCEPT, returning those it acted on.

THE LOOP SIX VERBS WERE SPELLING OUT, in the file whose header names this
exact mistake -- 'if you find yourself adding focus-left, focus-right,
focus-up and focus-down as separate commands, stop'.  The six bulk verbs
below differed by two tokens -- CLOSE-WINDOW-LATER or MINIMIZE-WINDOW, and
whether a window came from the workspace or from the selection -- and a
behavioural ruling (bulk verbs leave floats alone; count only the ones that
actually moved) had to be reasoned about and applied six times.

ACTION answers truthy when it acted, and that answer is what the count is:
CLOSE-WINDOW-LATER refuses a window with no proxy and MINIMIZE-WINDOW returns
the window it moved, so the two verbs disagree about failure in a way the
caller now does not have to know about."
  (let ((acted '()))
    (dolist (window windows (nreverse acted))
      (unless (eq window except)
        (when (funcall action window) (push window acted))))))

(defun act-on-current-workspace (action &key except)
  "ACT-ON-WINDOWS over this workspace's tiled windows.  See it for the shape."
  (act-on-windows (current-workspace-windows) action :except except))

(defcommand close-others ()
  "Close every window on the current workspace except the focused one.

'Clean the screen but keep my editor.'  The survivor is untouched — its pane,
its place in the tree — and every other window on this workspace is asked to
close through the ordinary path, so the applications get to save and the
:WINDOW-CLOSED hook fires.  Returns how many went."
  (let ((closed (act-on-current-workspace #'close-window-later
                                          :except (focused-window))))
    (notify "closing ~d other window~:p" (length closed))
    (length closed)))

(defcommand close-workspace ()
  "Close every window on the current workspace, leaving an empty pane.

A workspace you are done with, in one key.  Closing every pane leaves the
workspace a single empty pane — DESIGN D17/D19's shape, so that typing there
still spawns something — and the applications are asked to close through the
ordinary path rather than being torn down.  Returns how many went."
  (let ((closed (act-on-current-workspace #'close-window-later)))
    (notify "closing ~d window~:p on this workspace" (length closed))
    (length closed)))

(defcommand minimize-others ()
  "Minimize every window on the current workspace except the focused one.

The model, not the wire: each one is taken out of the tree onto the
scratchpad by MINIMIZE-WINDOW immediately, so the rest of the workspace
retiles now and RESTORE-LAST brings each back.  Returns how many went."
  (let ((went (act-on-current-workspace #'minimize-window
                                        :except (focused-window))))
    (notify "minimized ~d other window~:p" (length went))
    (length went)))

(defcommand minimize-workspace ()
  "Minimize every window on the current workspace.

The whole workspace to the scratchpad at once — 'put this session down'.
Returns how many went."
  (let ((went (act-on-current-workspace #'minimize-window)))
    (notify "minimized ~d window~:p" (length went))
    (length went)))

;;; ================================================================ system

(defvar *spawn-directory* nil
  "The directory children of SPAWN start in, or NIL for wherever the
compositor itself was started.

A dynamic variable rather than an argument to SPAWN because the code that
knows the directory -- a project workspace, a file manager integration -- is
rarely the code that spells out the command line, and because every path into
a program (SPAWN itself, TERMINAL, EDITOR, a key on an empty pane) funnels
through this one command.  Bound around the call by whoever knows; left alone
by everybody else.")

(defcommand spawn (&rest command)
  "Run COMMAND as a detached process.

    (spawn \"foot\")
    (spawn \"firefox\" \"--new-window\")

From M-x this is the run-a-program prompt, and what you type is split on
spaces the way a shell would split it.

The child is detached and its output goes nowhere, so a program that writes to
stderr cannot fill a pipe nobody is reading and block.

When *SPAWN-DIRECTORY* is bound, the child starts there.  The directory is
changed and put back around RUN-PROGRAM because neither it nor anything under
it takes a working directory; the window manager is single-threaded where
commands run, so the window in which another thread could observe the moved
directory does not exist."
  (:interactive :shell-command)
  (best-effort "spawn"
    (if *spawn-directory*
        (let ((previous (sb-posix:getcwd)))
          (sb-posix:chdir (namestring *spawn-directory*))
          (unwind-protect
               (sb-ext:run-program (first command) (rest command)
                                   :search t :wait nil
                                   :output nil :error nil :input nil)
            (sb-posix:chdir previous)))
        (sb-ext:run-program (first command) (rest command)
                            :search t :wait nil
                            :output nil :error nil :input nil)))
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

;;; ------------------------------------------------------- targeted spawn

(defcommand target-here (&optional (count 1))
  "The next COUNT windows open in the focused pane, wherever you go meanwhile.

Spawn placement reads the cursor, so opening something 'here' is the default
and there was no way to say it about a place you were not standing in.  This
is the latch: set it on a pane, then walk anywhere and open the thing, and it
lands here.  One-shot, because a latch that stays armed is a mode, and the
mode people actually want is the cursor.

A pane that holds a window is split to receive the next one, and an empty pane
is filled — the same rules an ordinary spawn follows at the cursor."
  (:interactive :number)
  (let ((path (current-path)))
    (cond
      ((null path) (notify "there is no pane to target"))
      (t
       (p:set-spawn-target path :count count)
       (notify "the next ~d window~:p will open here" (max 1 count))
       path))))

(defcommand clear-target ()
  "Drop a TARGET-HERE latch.  New windows go where placement says again."
  (p:clear-spawn-target)
  (notify "no targeted spawn")
  nil)

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
    (best-effort "exit_session" (w:wm-exit-session (server-manager *server*))))
  (setf (server-running *server*) nil))

(defcommand restart-wm ()
  "Restart the window manager without ending the session.

River keeps running, and so does every application; the window manager is an
ordinary Wayland client.  This saves the layout, disconnects, and starts the
same binary again with the same arguments — the layout comes back, because
persistence is keyed on river's stable window identifiers.

IT DID NOT RESTART, AND THAT IS THE WHOLE OF THIS DOCSTRING'S HISTORY.  It
cleared the running flag and exited, under a docstring that said \"Relaunch it
and the layout comes back\" — addressed to somebody who no longer had a keymap,
because the keymap belongs to the window manager that just left.  install.sh
writes `exec river -c latticewm', which runs the client once and does not
respawn it, so the user was left with their windows, no bindings, and recovery
by way of a TTY.  It is bound to Shift+Super+r by default.

The relaunch happens in MAIN, after START has returned and the Wayland
connection is closed, and that ordering is the point rather than tidiness:
river hands window management to one client at a time, so a successor spawned
from here — inside the event loop, with our manager object still bound — would
race us for it and lose."
  (setf *restart-on-exit* t)
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
  "Grow or shrink the focused floating window DIRECTION.

Shrinking stops at *FLOAT-MINIMUM-SIZE*, because a float smaller than that is
still focused and still framed and cannot be seen or grabbed back."
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
                             (max (car p:*float-minimum-size*)
                                  (+ (c:rect-w rect) dw))
                             (max (cdr p:*float-minimum-size*)
                                  (+ (c:rect-h rect) dh)))))))))

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
           (current (c:world-focused-float *world* p:*seat*))
           (position (position current floats)))
      (setf (c:world-focused-float *world* p:*seat*)
            (cond ((null floats) nil)
                  ((null position) (first floats))
                  (t (let ((next (+ position step)))
                       (when (< -1 next (length floats)) (nth next floats))))))
      (request-manage)
      (c:world-focused-float *world* p:*seat*))))

(defcommand focus-tiled ()
  "Take keyboard focus off any floating window and give it back to the cursor."
  (with-relayout
    (setf (c:world-focused-float *world* p:*seat*) nil)
    (request-manage)))

(defcommand raise-float ()
  "Put the focused floating window on top of the other floats."
  (with-relayout
    (let ((float (c:world-focused-float *world* p:*seat*)))
      (when float
        (setf (c:world-floats *world*)
              (append (remove float (c:world-floats *world*)) (list float)))))))

(defcommand close-float ()
  "Close the focused floating window."
  (let ((float (c:world-focused-float *world* p:*seat*)))
    (when float
      (let ((window (c:float-window float)))
        (when (c:window-proxy window)
          (close-window-later window))))))
