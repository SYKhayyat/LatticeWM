;;;; model/node.lisp --- Nodes, containers, and the container protocol.
;;;;
;;;; THE ONE IDEA IN THIS FILE
;;;;
;;;; A split boundary and a cell boundary are the same thing seen twice.  A
;;;; split separates two subtrees reached by an index; a lattice cell separates
;;;; two subtrees reached by a coordinate.  Nested versus addressed.  Nothing
;;;; else differs.
;;;;
;;;; So there is exactly one abstraction — a CONTAINER holds children at
;;;; ADDRESSES — and every structural operation in the window manager is
;;;; written once, against it:
;;;;
;;;;   * motion (including motion that crosses from a split into the cell next
;;;;     door, with no mode and no separate command),
;;;;   * layout,
;;;;   * insert / remove / move / swap,
;;;;   * focus repair.
;;;;
;;;; Three container kinds ship in the core:
;;;;
;;;;   SPLIT   ordered, n-ary, weighted.  Emacs windows, i3 containers,
;;;;           hyprland's dwindle.  Addresses are integers.
;;;;   STACK   ordered, only one child visible.  This is *simultaneously* tabs,
;;;;           workspaces, and the "lattices one behind another" Z axis — they
;;;;           are the same object, so every verb works on all three for free.
;;;;   LEAF    holds a window, or deliberately nothing (README D17: the empty
;;;;           pane is first-class).
;;;;
;;;; A fourth — GRID, the sparse coordinate-addressed lattice with a viewport —
;;;; is deliberately *not* here.  It ships in the separate `lattice' system and
;;;; must be expressible with zero edits to this file.  That is README D21's
;;;; experiment, and this file's decomposition is what it tests.
;;;;
;;;; Why n-ary splits rather than binary: binary trees make three side-by-side
;;;; windows an asymmetric nest, so "equalize" becomes ambiguous and moving one
;;;; window right crosses two boundaries.  Emacs and i3 are effectively n-ary;
;;;; hyprland's dwindle is binary and it is the thing people complain about.
;;;; Binary remains reachable as policy — see SPLIT-AXIS-FOR and SPAWN-TARGET.

(in-package #:latticewm/core)

;;; ------------------------------------------------------------------ nodes

(defvar *node-counter* 0
  "Source of NODE-ID values.  Ids are unique within one session and are used
for persistence and for talking about a node from a REPL.")

(defclass node ()
  ((id :initform (incf *node-counter*) :reader node-id
       :documentation "Unique within the session.  Stable across tree surgery.")
   (props :initform '() :accessor props
          :documentation
          "An extension property list.  README D20: every user-visible object
carries one from day one, so that an extension can hang per-node state without
editing a DEFCLASS it does not own.  This is the Emacs answer — symbol plists,
text properties, buffer-local variables — and without it 'extensible in Lisp'
is true of behaviour and false of state for anyone who is not the core
author.")
   (label :initform nil :accessor node-label
          :documentation "An optional human-given name, for jump-to-name."))
  (:documentation
   "Base class of everything in the layout tree.

Subclass it freely.  Nothing in the core tests for exactly LEAF, SPLIT or
STACK where a generic would do; the container protocol below is the contract,
and a new container kind that answers it is a first-class citizen."))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t :identity nil)
    (format stream "~d~@[ ~s~]" (node-id n) (node-label n))))

(defun prop (node key &optional default)
  "The extension property KEY of NODE, or DEFAULT.

    (setf (prop node :my-extension/pinned) t)"
  (getf (props node) key default))

(defun (setf prop) (value node key &optional default)
  (declare (ignore default))
  (setf (getf (props node) key) value))

;;; ------------------------------------------------------------------ leaves

(defclass leaf (node)
  ((window :initarg :window :initform nil :accessor leaf-window
           :documentation
           "The window this leaf holds, or NIL for a deliberately empty pane."))
  (:documentation
   "A place in the layout that holds one window, or nothing.

An *empty* leaf is not a degenerate case to be cleaned up — it is how the user
places slack (README D17).  You split a pane and leave one side empty, and the
window occupies the rest.  Focus may rest on an empty leaf; that is README
D18's whole point, and it is why CLEAR and CLOSE are two different verbs."))

(defun make-leaf (&optional window)
  "A leaf holding WINDOW, or an empty pane when WINDOW is NIL."
  (make-instance 'leaf :window window))

(defun leaf-empty-p (leaf)
  "True when LEAF holds no window."
  (null (leaf-window leaf)))

;;; -------------------------------------------------------- the container protocol

(defclass container (node) ()
  (:documentation
   "Abstract base of everything that holds children at addresses.

A container is defined entirely by the generic functions below.  It has no
CHILDREN slot, deliberately: SPLIT keeps an ordered list, STACK keeps an
ordered list plus a selection, and the lattice's GRID keeps a sparse hash
keyed by coordinate.  Sharing a slot would have forced all three into one
representation and made the lattice impossible to add from outside."))

(defgeneric container-addresses (container)
  (:documentation
   "The addresses of CONTAINER's children, as a fresh list, in layout order.

Layout order is the order the addresses are laid out left-to-right or
top-to-bottom, and it is the order LAYOUT-CHILDREN and RENDER-ORDER walk.
For a SPLIT it is 0, 1, 2, …; for the lattice's GRID it is the visible cell
coordinates in reading order."))

(defgeneric child-at (container address)
  (:documentation
   "The child of CONTAINER at ADDRESS, or NIL when there is none.

Returning NIL rather than signalling is deliberate: the lattice is sparse and
asking about an empty coordinate is an ordinary question, not an error."))

(defgeneric (setf child-at) (node container address)
  (:documentation
   "Put NODE at ADDRESS in CONTAINER, replacing whatever was there.

For a dense container ADDRESS must already exist; use INSERT-CHILD to grow
one.  For a sparse container this creates the address."))

(defgeneric insert-child (container address node)
  (:documentation
   "Add NODE to CONTAINER at ADDRESS, growing the container.

For a dense container (SPLIT, STACK) ADDRESS is the index NODE will occupy and
existing children from that index on shift up by one.  For a sparse container
it is simply the address to fill.  Returns CONTAINER."))

(defgeneric remove-child (container address)
  (:documentation
   "Take the child at ADDRESS out of CONTAINER and return it, or NIL.

For a dense container the remaining children close up and their addresses
shift down; any path held elsewhere that pointed past ADDRESS is now stale.
That is why every surgery function returns a repaired path rather than
expecting callers to fix it themselves — see README D18's focus-repair rule."))

(defgeneric address-equal (container a b)
  (:documentation
   "True when addresses A and B name the same slot of CONTAINER.

A method is needed because addresses are integers in a SPLIT and coordinate
lists in a GRID, and EQL is wrong for the second."))

(defgeneric container-count (container)
  (:documentation "How many children CONTAINER currently holds."))

(defgeneric simplify-node (node)
  (:documentation
   "Return the node that should stand in NODE's place after surgery.

Three possible answers:

  NODE itself   nothing to do — the overwhelmingly common case;
  another node  NODE dissolves into it.  A split down to one child is not
                wrong, merely pointless, and leaving such nodes behind is how
                a tree becomes nine levels deep for three windows;
  NIL           NODE should be *removed from its parent entirely*, which then
                gets the same question asked of it.

The NIL answer is what makes a whole nest unwind in one step: close the only
window inside three levels of nesting and every level above it evaporates,
rather than leaving a ladder of empty containers or — worse — an empty pane
the user never asked for.  D17's empty pane is deliberate, and debris must not
be able to impersonate it.

Called by the surgery functions with :SIMPLIFY T, which is the default.  Pass
:SIMPLIFY NIL when you are mid-operation and about to add a sibling back."))

(defgeneric default-address (container)
  (:documentation
   "The address a structural descent into CONTAINER should take.

*Structural*, not policy: this is what REPAIR-PATH and FIRST-LEAF-PATH use to
find a valid place, so it must always answer with something real and must not
consult a policy that might not be loaded.  A STACK answers with its selected
child, because landing focus repair on a hidden tab would be a bug that looks
like a haunting.

Policy's ENTRY-ADDRESS is the richer question — it knows the direction you
arrived from — and it is what ordinary navigation uses."))

(defun container-p (x)
  "True when X is a container."
  (typep x 'container))

;;; Generic fallbacks that hold for every container.

(defmethod address-equal ((c container) a b)
  "The default is EQUAL, which is right for integers and for coordinate lists."
  (equal a b))

(defmethod container-count ((c container))
  (length (container-addresses c)))

(defmethod simplify-node ((n node))
  "Most nodes stand for themselves."
  n)

(defmethod default-address ((c container))
  (first (container-addresses c)))

;;; ------------------------------------------------- sequential containers

(defclass sequential-container (container)
  ((children :initarg :children :initform '() :accessor children
             :documentation "An ordered list of child nodes.  Addresses are
indices into it."))
  (:documentation
   "A container whose children are an ordered list addressed by integer index.

SPLIT and STACK share it.  Everything specific to either lives in the
subclass; the addressing, insertion and removal are here and written once."))

(defmethod container-addresses ((c sequential-container))
  (loop for i from 0 below (length (children c)) collect i))

(defmethod child-at ((c sequential-container) address)
  (when (and (integerp address) (<= 0 address) (< address (length (children c))))
    (nth address (children c))))

(defmethod (setf child-at) (node (c sequential-container) address)
  (unless (and (integerp address) (<= 0 address) (< address (length (children c))))
    (error "Address ~s is outside ~s, which holds ~d children."
           address c (length (children c))))
  (setf (nth address (children c)) node))

(defmethod container-count ((c sequential-container))
  (length (children c)))

(defmethod insert-child ((c sequential-container) address node)
  (let ((i (max 0 (min (or address (length (children c))) (length (children c))))))
    (setf (children c)
          (append (subseq (children c) 0 i) (list node) (subseq (children c) i))))
  c)

(defmethod remove-child ((c sequential-container) address)
  (let ((kid (child-at c address)))
    (when kid
      (setf (children c)
            (append (subseq (children c) 0 address)
                    (subseq (children c) (1+ address)))))
    kid))

;;; ------------------------------------------------------------------ split

(defclass split (sequential-container)
  ((axis :initarg :axis :initform :horizontal :accessor split-axis
         :documentation
         ":HORIZONTAL lays children out side by side, :VERTICAL stacks them.")
   (weights :initarg :weights :initform '() :accessor weights
            :documentation
            "One positive number per child, parallel to CHILDREN.  Only the
ratios matter; DIVIDE-RECT normalises.  Resizing a divider is adjusting two
adjacent weights, which is why resize does not need to know pixel sizes and
therefore works identically at every zoom level."))
  (:documentation
   "An ordered, n-ary, weighted split.  Emacs windows and i3 containers.

Recursive without limit: any child may itself be a split, a stack, a leaf, or
a container kind an extension invented."))

(defun make-split (axis children &optional weights)
  "A split along AXIS holding CHILDREN, with WEIGHTS defaulting to equal shares."
  (let ((n (length children)))
    (make-instance 'split :axis axis :children (copy-list children)
                          :weights (if weights
                                       (copy-list weights)
                                       (make-list n :initial-element 1)))))

(defmethod insert-child :after ((c split) address node)
  (declare (ignore node))
  ;; Keep WEIGHTS parallel to CHILDREN.  A new child takes the average of the
  ;; existing weights, so inserting into an evenly-split container keeps it
  ;; even and inserting into a lopsided one does not hand the newcomer a
  ;; surprising share.
  (let* ((existing (weights c))
         (share (if existing
                    (/ (reduce #'+ existing) (length existing))
                    1))
         (i (max 0 (min (or address (length existing)) (length existing)))))
    (setf (weights c)
          (append (subseq existing 0 i) (list share) (subseq existing i)))))

(defmethod remove-child :around ((c split) address)
  (let ((kid (call-next-method)))
    (when kid
      (let ((ws (weights c)))
        (when (and (integerp address) (< address (length ws)))
          (setf (weights c)
                (append (subseq ws 0 address) (subseq ws (1+ address)))))))
    kid))

(defmethod simplify-node ((s split))
  "A split with one child is that child; a split with none should be removed.

Answering NIL rather than an empty leaf is what stops a closed window leaving
a pane behind.  An empty pane is something the user asked for (D17); one that
appears because a container ran out of children is debris wearing the same
costume, and it would teach people to distrust the real ones."
  (case (length (children s))
    (0 nil)
    (1 (first (children s)))
    (t s)))

(defun weight-at (split address)
  "SPLIT's weight for the child at ADDRESS, or 1 when there is none."
  (or (nth address (weights split)) 1))

(defun set-weight (split address value)
  "Set SPLIT's weight at ADDRESS to VALUE, which must be positive."
  (when (and (integerp address) (< -1 address (length (weights split))))
    (setf (nth address (weights split)) (max 1/1000 value)))
  value)

(defun normalized-weights (split)
  "SPLIT's weights scaled to sum to 1."
  (let* ((ws (weights split))
         (total (reduce #'+ ws :initial-value 0)))
    (if (plusp total)
        (mapcar (lambda (w) (/ w total)) ws)
        (make-list (length ws) :initial-element (/ 1 (max 1 (length ws)))))))

(defun adjust-weight (split address delta)
  "Move DELTA of SPLIT's total weight into the child at ADDRESS from the next one.

This is the resize primitive.  It is expressed as a transfer rather than an
assignment so that the other children keep the sizes they had — dragging one
divider must not disturb the divider beyond it, which is the single most
common complaint about tiling resize."
  (let* ((ws (weights split))
         (n (length ws)))
    (when (and (integerp address) (< -1 address n))
      (let* ((neighbour (if (< (1+ address) n) (1+ address) (1- address))))
        (when (<= 0 neighbour)
          (let* ((total (+ (nth address ws) (nth neighbour ws)))
                 (want (max 1/1000 (min (- total 1/1000)
                                        (+ (nth address ws) delta)))))
            (setf (nth address ws) want
                  (nth neighbour ws) (- total want))))))
    (weights split)))

;;; ------------------------------------------------------------------ stack

(defclass stack (sequential-container)
  ((selected :initarg :selected :initform 0 :accessor stack-selected
             :documentation "Index of the child that is currently shown."))
  (:documentation
   "An ordered container that shows one child at a time.

This one class is tabs, workspaces, and the Z axis of lattices stacked one
behind another.  They are genuinely the same object — a list of alternatives
of which one is current — and collapsing them means every verb that works on
a container works on all three without being written three times.  'Move this
window to workspace 3' and 'move this window to tab 3' are one command.

Directional motion deliberately does *not* enter a stack's hidden children:
STEP-ADDRESS returns NIL for spatial directions, so motion ascends past it.
You do not arrive in another workspace by pressing Left.  Switching is its own
verb, which is what makes a stack legible."))

(defun make-stack (children &optional (selected 0))
  "A stack over CHILDREN showing the one at index SELECTED."
  (make-instance 'stack :children (copy-list children) :selected selected))

(defmethod remove-child :around ((s stack) address)
  (let ((kid (call-next-method)))
    (when kid
      ;; Keep the selection pointing at the same child where possible, and
      ;; never off the end.
      (let ((n (length (children s))))
        (cond ((zerop n) (setf (stack-selected s) 0))
              ((> (stack-selected s) address)
               (setf (stack-selected s) (1- (stack-selected s))))
              (t (setf (stack-selected s) (min (stack-selected s) (1- n)))))))
    kid))

(defmethod insert-child :after ((s stack) address node)
  (declare (ignore node))
  (when (and (integerp address) (<= address (stack-selected s)))
    (incf (stack-selected s))))

(defmethod simplify-node ((s stack))
  "A stack survives, always, and regains an empty child if it loses its last.

Unlike a split, a one-element stack is meaningful: it is a workspace list with
one workspace, or a tab bar with one tab, and collapsing it would delete the
user's ability to add a second.  And a workspace list with *no* workspaces is
not a simpler state, it is a broken one — so closing the last window on the
last workspace leaves you standing in an empty pane, which is exactly where
you started and exactly where typing a key spawns something (D19)."
  (when (zerop (length (children s)))
    (setf (children s) (list (make-leaf))
          (stack-selected s) 0))
  s)

(defmethod default-address ((s stack))
  "The selected child.  Focus repair must never land on a hidden tab."
  (stack-selected s))

;;; -------------------------------------------------------------- traversal

(defun map-nodes (function root &key (order :pre))
  "Call FUNCTION on ROOT and every node beneath it.

ORDER is :PRE (parents before children, the default) or :POST.  Traversal
visits *every* child of every container, including a stack's hidden ones —
this is structural traversal, not rendering."
  (labels ((walk (n)
             (when (eq order :pre) (funcall function n))
             (when (container-p n)
               (dolist (addr (container-addresses n))
                 (let ((kid (child-at n addr)))
                   (when kid (walk kid)))))
             (when (eq order :post) (funcall function n))))
    (walk root))
  root)

(defun find-node-if (predicate root)
  "The first node at or under ROOT satisfying PREDICATE, or NIL."
  (map-nodes (lambda (n) (when (funcall predicate n)
                           (return-from find-node-if n)))
             root)
  nil)

(defun node-leaves (root)
  "Every leaf at or under ROOT, in traversal order."
  (let ((out '()))
    (map-nodes (lambda (n) (when (typep n 'leaf) (push n out))) root)
    (nreverse out)))

(defun node-windows (root)
  "Every window held under ROOT, in traversal order, skipping empty panes."
  (remove nil (mapcar #'leaf-window (node-leaves root))))

(defun leaf-holding (root window)
  "The leaf under ROOT whose window is WINDOW, or NIL."
  (find-node-if (lambda (n) (and (typep n 'leaf) (eq (leaf-window n) window)))
                root))

(defun node-empty-p (node)
  "True when NODE holds no windows at all.

An empty *leaf* is a deliberate object (D17); an empty *container* is usually
debris, and this is how the collapse rules find it."
  (etypecase node
    (leaf (leaf-empty-p node))
    (container (every (lambda (addr)
                        (let ((kid (child-at node addr)))
                          (or (null kid) (node-empty-p kid))))
                      (container-addresses node)))))

(defun copy-node (node)
  "A structural copy of NODE and everything under it.

Windows are shared, not duplicated — there is only ever one of those.  PROPS
are copied one level deep, so an extension's plist survives but a mutable
value inside it is shared."
  (let ((new (make-instance (class-of node))))
    (setf (props new) (copy-list (props node))
          (node-label new) (node-label node))
    (typecase node
      (leaf (setf (leaf-window new) (leaf-window node)))
      (split (setf (split-axis new) (split-axis node)
                   (weights new) (copy-list (weights node))
                   (children new) (mapcar #'copy-node (children node))))
      (stack (setf (stack-selected new) (stack-selected node)
                   (children new) (mapcar #'copy-node (children node))))
      (sequential-container
       (setf (children new) (mapcar #'copy-node (children node)))))
    new))
