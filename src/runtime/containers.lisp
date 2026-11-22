;;;; runtime/containers.lisp --- Changing what a container is, without moving
;;;;                                   anything out of it.
;;;;
;;;; THE PROBLEM THIS SOLVES IS THE SECOND HALF OF "EVERY PARADIGM".  A split, a
;;;; stack, a grid and a strip are four ways of arranging the same children, and
;;;; until now each one converted THE WORKSPACE and nothing else -- two commands,
;;;; two copies of the same surgery, and no way back.  So a person could not put a
;;;; grid in one half of a split, could not turn a grid into plain tiling again
;;;; without retyping the windows, and could not try a layout and change their
;;;; mind.
;;;;
;;;; A RESTYLE is a container swapping its KIND while keeping its children, their
;;;; order, and the cursor's place among them.  Nothing enters or leaves the
;;;; tree: one node is replaced by one node holding the same list.  That is what
;;;; makes paradigms combinable rather than exclusive -- the arrangement of a
;;;; container is a property of the container, and every container in the tree
;;;; can have its own.
;;;;
;;;; WHY A REGISTRY AND NOT A CASE.  The kinds a program can arrange are exactly
;;;; the container classes the extensions define, so a COND over the three we
;;;; ship would make every new kind a patch to this file -- the thing extensions
;;;; exist not to need.  A kind is registered by name with a constructor, and
;;;; RESTYLE asks the registry.  The core registers the two everybody has.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------------- the registry

(defvar *container-kinds* (make-hash-table :test #'equal)
  "Keyword name -> (CONSTRUCTOR . DESCRIPTION), for every restylable kind.

The constructor takes (CHILDREN &KEY ARGUMENT) and answers a container node
holding CHILDREN in order, or it may answer NIL when the request makes no
sense -- a grid asked for zero columns, say.  ARGUMENT is the optional word the
user typed after the kind, already parsed, or NIL.")

(defun define-container-kind (name constructor &key description)
  "Teach RESTYLE that a container of kind NAME is made by CONSTRUCTOR.

NAME is a keyword or a designator for one: (DEFINE-CONTAINER-KIND :GRID
#'MAKE-GRID).  Registering a name twice replaces it, because loading an
extension twice is something people do all evening and the second load must
not be an error.

This is the seam an extension uses to make its own arrangement switchable --
not a hook and not a generic, because the answer is a *constructor*, and a
constructor is a value."
  (let ((key (intern (string-upcase (string name)) :keyword)))
    (setf (gethash key *container-kinds*)
          (cons constructor (or description "")))
    key))

(defun container-kind-names ()
  "Every restylable kind, as keywords, sorted."
  (sort (loop for key being the hash-keys of *container-kinds* collect key)
        #'string< :key #'symbol-name))

(defun container-kind-description (name)
  "What kind NAME arranges, or NIL when it is not registered."
  (let ((entry (gethash (intern (string-upcase (string name)) :keyword)
                        *container-kinds*)))
    (and entry (cdr entry))))

(defun container-kind-registered-p (name)
  "True when RESTYLE knows a kind called NAME."
  (nth-value 1 (gethash (intern (string-upcase (string name)) :keyword)
                        *container-kinds*)))

;;; ------------------------------------------------------- the kinds we ship

(defun build-splits (children &optional (axis :horizontal) )
  "Splits over CHILDREN, alternating the axis as it nests.

Binary at the top and recursive below, because a split's weights decide share
and a binary tree is what the resize verbs assume: an n-ary split of eight
children can be resized into eight unequal slots and no command makes sense of
that.  The last two children are the base case, so four children come out as
two columns of two rows, which is the shape a person expects from \"plain
tiling\"."
  (cond
    ((null children) (c:make-stack '()))
    ((null (rest children)) (first children))
    (t (let ((n (length children))
             (other (if (eq axis :horizontal) :vertical :horizontal)))
         (if (>= n 4)
             ;; Half and half, so the recursion stays balanced.
             (let* ((half (ceiling n 2))
                    (left (subseq children 0 half))
                    (right (subseq children half)))
               (c:make-split axis
                             (list (build-splits left other)
                                   (build-splits right other))))
             (c:make-split axis (copy-list children)))))))


(define-container-kind
 :stack
 (lambda (children &key argument)
   (declare (ignore argument))
   ;; A stack shows one child at a time; the first one is the honest choice for
   ;; a conversion, because it is the child that was leftmost before.
   (c:make-stack children 0))
 :description "one child on screen at a time")

(define-container-kind
 :split
 (lambda (children &key argument)
   ;; The axis alternates by depth as the tree is built below, and an explicit
   ;; word beats the alternation: (RESTYLE SPLIT :VERTICAL).
   (let ((axis (and (member argument '(:horizontal :vertical) :test #'eq)
                    argument)))
     (build-splits children axis)))
 :description "every child on screen, tiled")

(define-container-kind
 :tabs
 (lambda (children &key argument)
   (declare (ignore argument))
   (c:make-stack children 0))
 :description "the same as stack: a stack is what tabs are made of")

;;; ------------------------------------------------------ the container itself

(defun cursor-container (path)
  "The deepest container on PATH -- the one the cursor is *in*.

The cursor itself is usually a leaf, and a leaf has no arrangement: what can be
restyled is the container holding it.  Answers NIL for an empty path or a path
that names a container directly, in which case that container is the answer."
  (when (and *world* path)
    (let ((chain (c:resolve-chain (c:world-root *world*) path)))
      (or (find-if #'c:container-p (reverse chain))
          ;; A path of length zero: the cursor is a container (the workspace
          ;; stack).  Restyling the root of the world is not a thing anybody
          ;; means, so answer NIL and let the command say so.
          nil))))

(defun restyle-container (container kind &key argument)
  "Make CONTAINER's children a container of KIND, in the same order, in place.

Returns (VALUES NEW-CONTAINER PATH) or NIL when KIND is not a kind this program
can make.  The children are moved, not copied: this is a swap of the box they
are in, and nothing about a child -- its window, its own children, its
identity -- changes."
  (let ((entry (gethash (intern (string-upcase (string kind)) :keyword)
                        *container-kinds*)))
    (when (and entry *world* container (c:container-p container))
      (let* ((root (c:world-root *world*))
             (path (c:node-path-to root container))
             (children (loop for address in (c:container-addresses container)
                             for child = (c:child-at container address)
                             when child collect child))
             (replacement (funcall (car entry) children :argument argument)))
        (when (and path replacement)
          ;; THE CURSOR'S OWN PATH, kept and repaired rather than reset to the
          ;; container: the children are the same list in the same order, so the
          ;; place the cursor was pointing at is still a place -- and a restyle
          ;; that moved the focus to the container would be a layout command
          ;; that loses your window.
          (let ((was (c:world-cursor *world*)))
            (setf (c:world-root *world*)
                  (c:tree-replace-at root path replacement))
            (setf (c:world-cursor *world*)
                  (c:repair-path (c:world-root *world*) (or was path))))
          (values replacement path))))))

;;; --------------------------------------------------------------- the verbs

(defun kind-argument (text)
  "The optional word in TEXT, as a keyword, or NIL.

  \"grid 3\"      -> 3        (a number the kind may read as its own)
  \"split vertical\" -> :VERTICAL
  \"stack\"       -> NIL

Kept here rather than in the prompt so that every kind gets the same reading of
what a trailing word means, and so a kind never has to parse anything."
  (let* ((space (position #\Space text))
         (word (and space (string-trim " " (subseq text (1+ space))))))
    (when (and word (plusp (length word)))
      (or (ignore-errors (parse-integer word :junk-allowed nil))
          (intern (string-upcase word) :keyword)))))

(defun kind-name (text)
  "The kind TEXT names, as a keyword: \"SPLIT vertical\" -> :SPLIT."
  (let* ((space (position #\Space text))
         (name (if space (subseq text 0 space) text)))
    (intern (string-upcase (string-trim " " name)) :keyword)))

(defcommand restyle (kind)
  "Change the arrangement of the container the cursor is in, keeping its windows.

    M-x restyle split     every window on screen, tiled
    M-x restyle stack     one window at a time
    M-x restyle grid 3    three columns, if the grid module is loaded

Any container can be restyled, not only the workspace: split a window to the
right, go there, and `restyle grid' arranges that half as a grid while the other
half keeps whatever it had.  Nothing enters or leaves the tree -- the children
become the children of a container of another kind -- so the windows, their
order and your place among them are the same afterwards.

The kinds are whatever is registered: the core ships SPLIT, STACK and TABS, and
a module that defines a container kind registers its own.  M-x container-kinds
prints the list."
  (:interactive :string)
  (let* ((text kind)
         (kind (kind-name text))
         (argument (kind-argument text))
         (container (and (container-kind-registered-p kind)
                         (cursor-container (current-path)))))
    (cond
      ((not (container-kind-registered-p kind))
       (notify "no container kind called ~(~a~); M-x container-kinds lists them"
               kind))
      ((null container)
       (notify "nothing here to restyle"))
      (t
       (let ((replacement (restyle-container container kind :argument argument)))
         (if (null replacement)
             (notify "~(~a~) could not be made here" kind)
             (progn
               (mark-dirty)
               (relayout :force t)
               (notify "~(~a~): ~d child~:p" kind
                       (c:container-count replacement)))))))))

(defcommand container-kinds ()
  "List the container kinds M-x restyle can make."
  ()
  (show-help-page
   (format nil "container kinds -- ~d" (length (container-kind-names)))
   (loop for kind in (container-kind-names)
         collect (cons (format nil "~(~a~)" kind)
                       (container-kind-description kind)))))

(defun container-class-name (container)
  "The class of CONTAINER, as a keyword: SPLIT, STACK, GRID-CONTAINER, STRIP.

The class name rather than a registered kind name, because the two need not be
the same string -- an extension's class is its own business -- and a person
reading ~(~a~) is not misled the way a guessed short name would mislead them."
  (intern (string-upcase (string (class-name (class-of container)))) :keyword))

(defcommand my-container ()
  "Say what container the cursor is in, and of what kind.

Named MY-CONTAINER and not CONTAINER because the model already exports
CONTAINER, and a command and a model type may not share a name -- the build
refuses the export, which is the good version of that failure."
  ()
  (let ((container (cursor-container (current-path))))
    (cond
      ((null container) (notify "the cursor is not in a container"))
      (t (notify "~(~a~) with ~d child~:p"
                 (container-class-name container)
                 (c:container-count container))))))
