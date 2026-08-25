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
