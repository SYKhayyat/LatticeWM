;;;; model/path.lisp --- Paths into the tree, and the focus-repair rule.
;;;;
;;;; A PATH is a list of addresses read from the root downwards.  (0 2 1) means
;;;; the root's child 0, that node's child 2, that node's child 1.  The empty
;;;; path is the root itself.
;;;;
;;;; Focus is a path, not a window (DESIGN D18).  It may rest on an empty leaf,
;;;; and that is the point: "move one cell left, whether or not anything is
;;;; there" and "you are now standing in an empty pane" are both commands with
;;;; a referent only under this model.  Wayland keyboard focus is *derived*
;;;; from the path and never stored — occupied leaf gets focus_window, empty
;;;; leaf gets clear_focus.
;;;;
;;;; The cursor points into a tree that every verb mutates.  DESIGN D18 names
;;;; the consequence and the fix: "After any tree surgery, focus lands on the
;;;; deepest surviving node along its previous path, then on that node's first
;;;; leaf.  This must be a single function that every verb calls.  Improvised
;;;; per-verb repair is how you get fifteen subtly different behaviours and a
;;;; bug class where focus points at a node that no longer exists."
;;;;
;;;; That function is REPAIR-PATH, and everything in model/surgery.lisp routes
;;;; through it.  Nothing else in the codebase is allowed to invent its own.

(in-package #:latticewm/core)

;;; ---------------------------------------------------------- path basics

(defun path-equal (a b)
  "True when paths A and B are the same."
  (equal a b))

(defun parent-path (path)
  "PATH with its last address dropped, i.e. the path of its parent."
  (butlast path))

(defun path-last (path)
  "The final address of PATH — the address it occupies in its parent."
  (car (last path)))

(defun path-append (path address)
  "PATH extended by one more ADDRESS."
  (append path (list address)))

(defun resolve-path (root path)
  "The node at PATH under ROOT, or NIL when PATH does not lead anywhere.

Returning NIL rather than signalling is deliberate: a stale path is an
ordinary event in a live window manager, and callers repair rather than
crash."
  (let ((node root))
    (dolist (address path node)
      (unless (container-p node) (return nil))
      (setf node (child-at node address))
      (unless node (return nil)))))

(defun resolve-chain (root path)
  "The list of nodes from ROOT to PATH inclusive, or NIL when PATH is invalid.

Motion needs the ancestor chain in order to walk back up out of a container
when it cannot move within one, and paths carry no parent pointers by design
— parent pointers are a bug farm under tree surgery, since every reparenting
has to remember to fix them and one missed case is an unfindable cycle."
  (let ((node root) (chain (list root)))
    (dolist (address path (nreverse chain))
      (unless (container-p node) (return nil))
      (setf node (child-at node address))
      (unless node (return nil))
      (push node chain))))

(defun node-contains-p (root target)
  "True when TARGET is ROOT or lies beneath it.

Distinct from NODE-PATH-TO because the path to ROOT itself is the empty list,
which is indistinguishable from \"not found\" — so a survival check written on
NODE-PATH-TO reports the root as missing.  Anything asking *whether* a node is
still in the tree must use this."
  (or (eq root target)
      (and (node-path-to root target) t)))

(defun path-valid-p (root path)
  "True when PATH leads to an existing node under ROOT."
  (and (resolve-path root path) t))

(defun node-path-to (root target)
  "The path from ROOT to TARGET (compared with EQ), or NIL when absent.

This is how surgery keeps focus on the node the user was looking at: resolve
the focused node *before* mutating, then find where it ended up afterwards.
It is O(tree), and the tree has tens of nodes, so the simplicity is free."
  (labels ((walk (node path)
             (when (eq node target) (return-from node-path-to (nreverse path)))
             (when (container-p node)
               (dolist (address (container-addresses node))
                 (let ((kid (child-at node address)))
                   (when kid (walk kid (cons address path))))))))
    (walk root '())
    nil))

;;; ------------------------------------------------------------ leaf paths

(defun first-leaf-path (root &optional prefix)
  "The path to the first leaf at or under ROOT, prefixed by PREFIX.

'First' is each container's DEFAULT-ADDRESS, which for a split is its leftmost
or topmost child and for a stack is the *selected* one — focus repair must
never land on a hidden tab, because a cursor you cannot see is indistinguishable
from a broken keyboard."
  (cond ((not (container-p root)) prefix)
        (t (let ((address (default-address root)))
             (if (null address)
                 prefix
                 (let ((kid (child-at root address)))
                   (if kid
                       (first-leaf-path kid (path-append prefix address))
                       prefix)))))))

(defun last-leaf-path (root &optional prefix)
  "The path to the last leaf at or under ROOT, prefixed by PREFIX."
  (cond ((not (container-p root)) prefix)
        (t (let ((addresses (container-addresses root)))
             (if (null addresses)
                 prefix
                 (let* ((address (car (last addresses)))
                        (kid (child-at root address)))
                   (if kid
                       (last-leaf-path kid (path-append prefix address))
                       prefix)))))))

(defun leaf-paths (root &optional prefix)
  "Every leaf path at or under ROOT, in layout order.

Used by cycle-focus, by the ephemeral jump labels, and by anything that wants
to enumerate places rather than windows."
  (if (container-p root)
      (loop for address in (container-addresses root)
            for kid = (child-at root address)
            when kid
              append (leaf-paths kid (path-append prefix address)))
      (list prefix)))

(defun next-leaf-path (root path &key (wrap t))
  "The leaf path after PATH in layout order, or NIL.

With WRAP, the last leaf's successor is the first leaf."
  (let* ((all (leaf-paths root))
         (position (position path all :test #'path-equal)))
    (cond ((null position) (first all))
          ((< (1+ position) (length all)) (nth (1+ position) all))
          (wrap (first all))
          (t nil))))

(defun previous-leaf-path (root path &key (wrap t))
  "The leaf path before PATH in layout order, or NIL."
  (let* ((all (leaf-paths root))
         (position (position path all :test #'path-equal)))
    (cond ((null position) (car (last all)))
          ((plusp position) (nth (1- position) all))
          (wrap (car (last all)))
          (t nil))))

;;; ------------------------------------------------------- THE repair rule

(defun repair-path (root path)
  "The nearest still-valid place to PATH under ROOT.  DESIGN D18's one rule.

Walk PATH as far as it still leads somewhere; from the deepest surviving node,
descend to its first leaf.  The result is always a valid leaf path as long as
ROOT exists, so callers never have to test it.

Every verb in the window manager calls this — directly, or through the surgery
functions in model/surgery.lisp, which call it for you.  Do not write another
one.  The reason is not tidiness: fifteen improvised repairs produce fifteen
subtly different behaviours after a close, and the resulting 'where did my
focus go' bugs are close to undebuggable because each one is local and
plausible."
  (let ((node root)
        (survived '()))
    (dolist (address path)
      (unless (container-p node) (return))
      (let ((kid (child-at node address)))
        (unless kid (return))
        (push address survived)
        (setf node kid)))
    (first-leaf-path node (nreverse survived))))
