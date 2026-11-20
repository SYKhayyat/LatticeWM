;;;; tabs/tabs.lisp --- One pane, many windows, shown one at a time.

(in-package #:tabs)

;;; ================================================================ state

(defvar *enabled* nil "True while closed windows are pruned from groups.")

(defvar *live-windows* nil
  "The windows TAB-ADD may pull, or NIL to ask the runtime.

A function of no arguments.  Bound by tests, which have no compositor to
ask; the honest default is every window the session manages.")

(defvar *undo-includes-tab-switches* nil
  "Whether cycling tabs records an undo step.

NIL -- the default -- treats a tab switch as a change of VIEW, like the
buffers module treats a buffer switch: the arrangement after the switch
becomes what undo considers unchanged, and no step is recorded.  T makes
every switch walkable with undo, which sounds right and is exhausting in
practice: ten switches are nine do-nothing steps between two real ones.")

(defun enabled-p () "True while groups are pruned on close." *enabled*)

;;; ============================================================ the groups
;;;
;;; A group lives ON ITS LEAF as a prop: an ordered list of windows.  The
;;; visible member is whatever the leaf currently holds.  Keying by the node
;;; itself means the group follows the pane through tree edits around it,
;;; and dies with it when the leaf goes away.

(defun group-of (leaf)
  "The ordered window list of LEAF's tab group, or NIL."
  (c:prop leaf :tabs))

(defun (setf group-of) (windows leaf)
  (setf (c:prop leaf :tabs) windows))

(defun visible-window (leaf)
  "The window LEAF is showing right now."
  (c:leaf-window leaf))

(defun prune-group (leaf)
  "Drop closed windows from LEAF's group; keep the visible one honest.

A window that has gone cannot be a tab any more.  If the VISIBLE member is
the one that went, promote the next survivor -- or leave the leaf empty if
there are none, at which point the group is dissolved too: one window does
not need tabs."
  (let* ((group (group-of leaf))
         (live (remove-if-not #'c:window-live-p group)))
    (when (< (length live) (length group))
      (let ((visible (visible-window leaf))
            (replacement (first live)))
        (unless (member visible live)
          (setf replacement (find-if #'c:window-live-p live))
          (when replacement
            (setf (c:leaf-window leaf) replacement))))
      (setf (group-of leaf) live)
      (when (null live)
        (setf (c:prop leaf :tabs) nil)))))

(defun note-window-closed (window)
  "Prune WINDOW from every group in the world.

WINDOW itself is not consulted: the walk visits every group anyway, and a
dead window in any of them is dead all the same.  The parameter stays --
the hook passes it, and the signature documents what the event offers.

The named function behind :window-closed, because ADD-HOOK keeps what it is
given and a fresh closure per call would accumulate rather than replace."
  (declare (ignore window))
  (labels ((walk (node)
             (when (and (typep node 'c:leaf) (group-of node))
               (prune-group node))
             (when (c:container-p node)
               (dolist (address (c:container-addresses node))
                 (let ((child (c:child-at node address)))
                   (when child
                     (walk child)))))))
    (walk (c:world-root r:*world*))))

;;; ============================================================== plumbing

(defun pull-window-out-of-tree (window)
  "Remove WINDOW's leaf from wherever it is tiled, leaving other windows
standing.  Returns T if it was tiled."
  (let* ((root (c:world-root r:*world*))
         (leaf (c:leaf-holding root window))
         (path (and leaf (c:node-path-to root leaf))))
    (when path
      (multiple-value-bind (removed new-root landed)
          (c:tree-remove-at root path
                            :simplify (lambda (node)
                                        (p:should-collapse-p
                                         (p:current-policy) node)))
        (declare (ignore removed landed))
        (setf (c:world-root r:*world*) new-root)
        t))))

(defun take-from-scratchpad (window)
  "If WINDOW is parked on the scratchpad, take it off.  Returns T if it was."
  (let* ((world r:*world*)
         (pad (c:world-scratchpad world)))
    (when (member window pad)
      (setf (c:world-scratchpad world) (remove window pad))
      t)))

(defun activate-in (leaf window)
  "Show WINDOW in LEAF and make it the keyboard's place."
  (setf (c:leaf-window leaf) window)
  (let ((path (c:node-path-to (c:world-root r:*world*) leaf)))
    (when path
      (p:jump-cursor (p:current-policy) r:*world* path))))

(defun settle-switch ()
  "Record or forgive the layout change a tab switch just made."
  (r:mark-dirty)
  (if *undo-includes-tab-switches*
      (r:note-layout-settled "switched tabs")
      (r:reset-undo-baseline "switched tabs")))

;;; ================================================================ commands

(r:defcommand tab-here ()
  "Make the focused pane hold tabs.

Its window becomes the group's first member.  A pane that already holds a
group is confirmed, not duplicated."
  (let ((leaf (r:current-leaf)))
    (cond
      ((null leaf) (r:notify "no pane under the cursor") nil)
      (t
       (unless (group-of leaf)
         (setf (group-of leaf)
               (list (visible-window leaf))))
       (r:notify "~d tab~:p" (length (group-of leaf)))
       t))))

(p:define-argument-type :live-app-id "tab-add: "
  :documentation "A live window, by application id, to pull into the group."
  :candidates (lambda ()
                (sort (delete-duplicates
                       (mapcar #'c:window-app-id (r:all-windows))
                       :test #'equal)
                      #'string<)))

(r:defcommand tab-add (&optional app-id)
  "Pull a live window into the focused pane's tab group, creating the group
if there is none.  The pulled window becomes the visible one.

The window is found by application id among everything the session manages;
it leaves wherever it was -- another workspace, the scratchpad -- the way
SEND-TO-WORKSPACE would have moved it, minus the workspace."
  (:interactive :live-app-id)
  (let ((leaf (r:current-leaf)))
    (cond
      ((null leaf) (r:notify "no pane under the cursor") nil)
      ((null app-id) (r:notify "which window?") nil)
      (t
       (let* ((windows (funcall (or *live-windows* #'r:all-windows)))
             (target (find app-id windows
                           :key #'c:window-app-id :test #'equal)))
         (cond
           ((null target)
            (r:notify "no live window ~a" app-id)
            nil)
           ;; Already here and visible: nothing to do.
           ((eq target (visible-window leaf))
            (r:notify "already showing") app-id)
           (t
            ;; Out of wherever it was...
            (pull-window-out-of-tree target)
            (take-from-scratchpad target)
            ;; ...into the group, becoming visible.
            (let ((group (or (group-of leaf)
                             (list (visible-window leaf)))))
              (setf (group-of leaf)
                    (append group (list target)))
              (activate-in leaf target)
              (settle-switch)
              (r:logmsg :info "tabbed ~a into ~s" app-id
                        (ignore-errors
                         (c:node-path-to (c:world-root r:*world*) leaf)))
              app-id))))))))

;;; ------------------------------------------------------------ #36: order, names
;;;
;;; The group is an ordered list on the leaf, so reordering is moving a window in
;;; that list and nothing else -- no z-index, no second structure.  The visible
;;; member is the leaf's window, so a reorder that kept the selection pointing at
;;; the same window is the one thing that must not happen by accident: TAB-MOVE
;;; moves the window the user is looking at, which is what "bring this tab
;;; forward" means.

(defun a-group-with-order (leaf)
  "LEAF's group, pruned, or NIL when it is not a group worth reordering.

PRUNED FIRST, because a group holding two dead windows and one live one is not
a two-member group, and MOD against a stale length is how a reorder lands
somewhere nobody asked for."
  (when leaf
    (prune-group leaf)
    (let ((group (group-of leaf)))
      (and group (> (length group) 1) group))))

(p:define-argument-type :tab-direction "tab direction: "
  :documentation "Forward (towards the end of the group) or backward."
  :candidates '("forward" "backward")
  :parse (lambda (text)
           (let ((word (string-downcase (string-trim " " text))))
             (cond ((string= word "forward") :forward)
                   ((string= word "backward") :backward)
                   (t (error "~a is not forward or backward" text))))))

(r:defcommand tab-move (direction)
  "Move the visible tab one place BACKWARD or FORWARD in its group, wrapping.

The window you are looking at is the one that moves, and it stays visible:
reordering tabs is arranging them, not choosing one, and a reorder that also
switched what you were looking at would make the key unusable for its purpose."
  (:interactive :tab-direction)
  (let ((leaf (r:current-leaf)))
    (let ((group (a-group-with-order leaf)))
      (cond
        ((null group) (r:notify "this pane holds no tabs to reorder") nil)
        (t
         (let* ((visible (visible-window leaf))
                (here (position visible group))
                (there (ecase direction
                         (:forward (mod (1+ here) (length group)))
                         (:backward (mod (1- here) (length group)))))
                ;; A MOVE, not a swap: exchanging the two places would drag the
                ;; neighbour the other way, which is not what "move this tab
                ;; forward" says and is why the wrapping case looked wrong.
                (rest (remove visible group)))
           (setf (group-of leaf)
                 (append (subseq rest 0 there) (list visible) (subseq rest there)))
           (settle-switch)
           (r:mark-dirty)
           (r:notify "tab ~d of ~d" (1+ there) (length group))
           visible))))))

(p:define-argument-type :tab-position "tab number: "
  :documentation "A place in the group: 1 is the first tab."
  :parse (lambda (text)
           (let ((n (parse-integer (string-trim " " text) :junk-allowed t)))
             (if (and n (plusp n))
                 n
                 (error "~a is not a tab number" text)))))

(r:defcommand tab-move-to (index)
  "Put the visible tab at position INDEX in its group, counting from 1.

No wrapping: a number out of range is a mistake, not a request to go round, and
moving to position 40 in a group of three should say so rather than landing
somewhere plausible."
  (:interactive :tab-position)
  (let ((leaf (r:current-leaf)))
    (let ((group (a-group-with-order leaf)))
      (cond
        ((null group) (r:notify "this pane holds no tabs to reorder") nil)
        ((not (<= 1 index (length group)))
         (r:notify "no tab ~d in a group of ~d" index (length group)) nil)
        (t
         (let* ((visible (visible-window leaf))
                (rest (remove visible group))
                (place (min (1- index) (length rest))))
           (setf (group-of leaf)
                 (append (subseq rest 0 place) (list visible) (subseq rest place)))
           (settle-switch)
           (r:mark-dirty)
           (r:notify "tab ~d of ~d" index (length group))
           visible))))))

(p:define-argument-type :tab-name "tab name: "
  :documentation "What this tab should be called.  Empty clears the name.")

(r:defcommand tab-rename (name)
  "Call the visible tab NAME, or clear the name with an empty one.

THE NAME STICKS TO THE WINDOW, not to the pane, because that is what it is
describing: UNTAB and TAB-ADD move a window between panes, and a name that
stayed behind would end up labelling whatever moved in instead.  It is the
window's label -- the same prop a window rule's :LABEL sets -- so a rename is
visible everywhere a label is, and a rule can name a window for you."
  (:interactive :tab-name)
  (let ((leaf (r:current-leaf)))
    (let ((visible (and leaf (visible-window leaf))))
      (cond
        ((null visible) (r:notify "no tab to rename") nil)
        (t
         (let ((name (and name (string-trim " " name))))
           (if (or (null name) (zerop (length name)))
               (progn (setf (c:prop visible :label) nil)
                      (r:notify "tab name cleared"))
               (progn (setf (c:prop visible :label) name)
                      (r:notify "tab named ~a" name)))
           (r:mark-dirty)
           (r:request-manage)
           name))))))

(r:defcommand tab-send (app-id)
  "Move the visible tab into the group holding APP-ID, creating it if it is
not a group yet.

The other half of TAB-ADD: that one pulls a window in from anywhere, this one
pushes a tab out to a *named* destination, which is the sentence people
actually say -- `this terminal belongs over in that group'.  The destination
is named by a window rather than by coordinates so it does not depend on the
layout, and the group being left dissolves if this was its last member: one
window does not need tabs."
  (:interactive :live-app-id)
  (let* ((leaf (r:current-leaf))
         (group (and leaf (group-of leaf)))
         (visible (and leaf (visible-window leaf))))
    (cond
      ((null group) (r:notify "this pane holds no tabs to send") nil)
      ((null app-id) (r:notify "which window marks the destination?") nil)
      (t
       (let* ((windows (funcall (or *live-windows* #'r:all-windows)))
              (target (find app-id windows
                            :key #'c:window-app-id :test #'equal)))
         (cond
           ((null target) (r:notify "no live window ~a" app-id) nil)
           ((eq target visible) (r:notify "that is this tab") nil)
           (t
            (let ((target-leaf (leaf-holding-tab (c:world-root r:*world*) target)))
              (cond
                ((null target-leaf)
                 (r:notify "~a is not in the layout" app-id) nil)
                ((eq target-leaf leaf)
                 (r:notify "~a is already in this group" app-id) nil)
                (t
                 ;; out of this group, and this group keeps its selection
                 (let ((rest (remove visible group)))
                   (setf (group-of leaf) rest)
                   (when rest (activate-in leaf (first rest))))
                 ;; into the destination's, becoming its visible member
                 (unless (group-of target-leaf)
                   (setf (group-of target-leaf) (list target)))
                 (setf (group-of target-leaf)
                       (append (group-of target-leaf) (list visible)))
                 (activate-in target-leaf visible)
                 ;; THE CURSOR COMES BACK to the pane you sent it from, which
                 ;; is where you are: a tab sent elsewhere is a window put
                 ;; away, not a journey, and the pane you were working in
                 ;; should not lose the keyboard for having given something up.
                 (let ((home (c:node-path-to (c:world-root r:*world*) leaf)))
                   (when home
                     (p:jump-cursor (p:current-policy) r:*world* home)))
                 (settle-switch)
                 (r:mark-dirty)
                 (r:notify "sent ~a to the group at ~a"
                           (or (c:prop visible :label)
                               (c:window-app-id visible))
                           app-id)
                 visible)))))))))) 

(defun leaf-holding-tab (root window)
  "The leaf whose visible window, or whose hidden group member, is WINDOW.

C:LEAF-HOLDING cannot answer this: it looks at what each leaf is *showing*, and
a hidden tab is exactly a window a group holds without showing.  Sending a tab
to its own group, or to a group that is currently showing something else, both
go through here."
  (labels ((walk (node)
             (cond
               ((typep node 'c:leaf)
                (and (or (eq (c:leaf-window node) window)
                         (member window (group-of node)))
                     node))
               ((c:container-p node)
                (some (lambda (address) (walk (c:child-at node address)))
                      (c:container-addresses node))))))
    (walk root)))

(defun cycle-tab (leaf step)
  "Move STEP places through LEAF's group, wrapping."
  (let* ((n (progn (prune-group leaf)
                   (length (group-of leaf))))
         (group (group-of leaf)))
    (when (> n 1)
      (let* ((visible (position (visible-window leaf) group))
             (next (mod (+ (or visible 0) step) n)))
        (activate-in leaf (nth next group))
        (settle-switch)
        (1+ next)))))

(r:defcommand tab-next ()
  "Show the next window in the focused pane's tab group, wrapping."
  (let ((leaf (r:current-leaf)))
    (if (and leaf (group-of leaf))
        (progn (cycle-tab leaf 1) t)
        (progn (r:notify "this pane holds no tabs") nil))))

(r:defcommand tab-prev ()
  "Show the previous window in the focused pane's tab group, wrapping."
  (let ((leaf (r:current-leaf)))
    (if (and leaf (group-of leaf))
        (progn (cycle-tab leaf -1) t)
        (progn (r:notify "this pane holds no tabs") nil))))

(r:defcommand untab ()
  "Pop the visible window out of its group and back into the tree as an
ordinary pane beside this one.

The last member of a group dissolves the group: one window does not need
tabs."
  (let ((leaf (r:current-leaf)))
    (let ((group (and leaf (group-of leaf))))
      (cond
        ((null group) (r:notify "this pane holds no tabs") nil)
        (t
         (let* ((visible (visible-window leaf))
                (rest (remove visible group)))
           ;; Out of the group...
           (setf (group-of leaf) rest)
           ;; ...and back into the tree beside us.
           (let* ((path (c:node-path-to (c:world-root r:*world*) leaf))
                  ;; A depth-1 path means our leaf IS the workspace's whole
                  ;; content: the parent is the stack itself, and wrapping
                  ;; both panes in a split is the only honest move.
                  (parent (if (= (length path) 1)
                              (c:world-root r:*world*)
                              (c:resolve-path (c:world-root r:*world*)
                                              (butlast path))))
                  (index (if (= (length path) 1)
                             0
                             (first (last path)))))
             (cond
               ((= (length path) 1)
                (let ((ours (c:child-at parent index)))
                  (c:remove-child parent index)
                  (c:insert-child
                   parent index
                   (c:make-split :horizontal
                                 (list ours (c:make-leaf visible))
                                 nil))))
               (t
                (c:insert-child parent (1+ index)
                                (c:make-leaf visible)))))
           (activate-in leaf (first rest))
           (settle-switch)
           (r:mark-dirty)
           visible))))))

;;; =============================================================== plumbing

(defun enable ()
  "Start pruning groups when windows close.  Idempotent by name."
  (p:add-hook :window-closed 'note-window-closed)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop pruning.  Groups survive; a closed window inside one simply stays
in the list until something next prunes it."
  (p:remove-hook :window-closed 'note-window-closed)
  (setf *enabled* nil)
  nil)
