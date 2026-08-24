;;;; master-stack/master-stack.lisp --- The layout, and the composition.
;;;;
;;;; The example's two methods are unchanged in substance: LAYOUT-CHILDREN
;;;; imposes master-and-stack on every SPLIT, STEP-ADDRESS makes motion follow
;;;; the drawing rather than the split's stored axis.  What changed is where
;;;; they live: on a MIXIN over POLICY, composed over whatever is installed,
;;;; instead of on a subclass of CONVENTIONAL-POLICY that replaces it.
;;;;
;;;; WHY THE CHANGE.  (setf *policy* (make-instance 'my-policy)) discards the
;;;; policy the user -- or the lattice, or a previous extension -- had
;;;; installed, silently, and the only clue is somebody else's behaviour
;;;; stopping.  EXTENDING.org's contract for a module with state is explicit:
;;;; mixin first in the superclass list, CHANGE-CLASS rather than
;;;; MAKE-INSTANCE, composed class interned under a derived name so enabling
;;;; twice finds it, disable back to the saved class rather than a fresh
;;;; default.

(in-package #:master-stack)

;;; ------------------------------------------------------------- the mixin

(defclass master-stack-mixin (p:policy)
  ((ratio :initarg :ratio :initform 3/5 :accessor master-ratio
          :documentation "Fraction of the split's width the master column gets.")
   (masters :initarg :masters :initform 1 :accessor master-count
            :documentation "How many panes share the master column.")
   (previous-class :initform nil :accessor %previous-class
                   :documentation "The class that was installed before ENABLE.
Rides on the mixin itself rather than in a global, because it goes away with
the composition it belongs to."))
  (:documentation
   "One large pane on the left, the rest stacked beside it."))

(defun enabled-p ()
  "True when the current policy answers as master-and-stack."
  (typep (p:current-policy) 'master-stack-mixin))

;;; ------------------------------------------------------------ the methods

(defmethod p:layout-children ((policy master-stack-mixin) (split c:split) rect)
  "Ignore the split's own axis and weights; impose master-and-stack instead.

The split is untouched -- this decides only where its children are drawn.
Nested splits get the same treatment recursively, which is what makes the
result read as dwm all the way down rather than dwm with one odd corner."
  (let* ((addresses (c:container-addresses split))
         (n (length addresses)))
    (cond
      ((zerop n) '())
      ;; One child takes everything; the general case would give it the
      ;; master fraction and leave the rest of the screen blank.
      ((= n 1) (list (cons (first addresses) rect)))
      (t
       (let* ((masters (min (master-count policy) (1- n)))
              (gap (p:gaps policy split))
              (split-x (+ (c:rect-x rect)
                          (round (* (c:rect-w rect) (master-ratio policy)))))
              (master-rect (c:make-rect (c:rect-x rect) (c:rect-y rect)
                                      (- split-x (c:rect-x rect) (round gap 2))
                                      (c:rect-h rect)))
              (stack-rect (c:make-rect (+ split-x (round gap 2)) (c:rect-y rect)
                                     (- (c:rect-right rect) split-x (round gap 2))
                                     (c:rect-h rect))))
         (append
          (mapcar #'cons (subseq addresses 0 masters)
                  (c:divide-rect master-rect :vertical
                               (make-list masters :initial-element 1) :gap gap))
          (mapcar #'cons (subseq addresses masters)
                  (c:divide-rect stack-rect :vertical
                               (make-list (- n masters) :initial-element 1)
                               :gap gap))))))))

(defmethod p:step-address ((policy master-stack-mixin) (split c:split)
                           address direction)
  "Motion follows the DRAWN arrangement, not the split's stored axis.

Left and Right cross between master and stack; Up and Down move within one.
Getting this wrong is the difference between a layout that works and one that
looks right and navigates wrongly."
  (let ((n (c:container-count split))
        (masters (min (master-count policy)
                      (max 0 (1- (c:container-count split))))))
    (if (< n 2)
        nil
        (case direction
          (:right (when (< address masters) masters))
          (:left (when (>= address masters) (1- masters)))
          (:down (let ((next (1+ address)))
                   (when (and (< next n)
                              (eq (< address masters) (< next masters)))
                     next)))
          (:up (let ((previous (1- address)))
                 (when (and (>= previous 0)
                            (eq (< address masters) (< previous masters)))
                   previous)))))))

;;; ------------------------------------------------------------ the commands

(r:defcommand master-wider (&optional (amount 1/20))
  "Give the master column more of the width."
  (when (enabled-p)
    (setf (master-ratio (p:current-policy))
          (max 1/10 (min 9/10 (+ (master-ratio (p:current-policy)) amount))))
    (r:relayout)))

(r:defcommand master-narrower (&optional (amount 1/20))
  "Give the master column less of the width."
  (master-wider (- amount)))

(r:defcommand more-masters (&optional (by 1))
  "Move a pane from the stack into the master column."
  (when (enabled-p)
    (setf (master-count (p:current-policy))
          (max 1 (+ (master-count (p:current-policy)) by)))
    (r:relayout)))

(r:defcommand fewer-masters (&optional (by 1))
  "Move a pane from the master column into the stack."
  (more-masters (- by)))

;;; ------------------------------------------------------- enable / disable

(defun composed-class-name (base)
  "The name under which the composition of MASTER-STACK-MIXIN over BASE lives.

Interned under a derived name so enabling twice finds the same class instead
of minting a second and emptying every generic function's dispatch cache --
which CLOS permits and no window manager survives."
  (intern (format nil "MASTER-STACK-OVER-~a" (class-name base))
          '#:master-stack))

(defun enable ()
  "Compose master-and-stack over the currently installed policy, live.

CHANGE-CLASS, not MAKE-INSTANCE, so a policy carrying state comes through the
switch still carrying it; the mixin first in the superclass list, so these
methods win and CALL-NEXT-METHOD reaches whatever was underneath.  Enabling
twice is a value check, not an error and not an accumulation."
  (let* ((policy (p:current-policy))
         (base (class-of policy)))
    (unless (typep policy 'master-stack-mixin)
      (change-class policy
                    (closer-mop:ensure-class
                     (composed-class-name base)
                     :direct-superclasses (list (find-class 'master-stack-mixin)
                                                base)))
      (setf (%previous-class policy) base))
    (values)))

(defun disable ()
  "Change back to whatever class was installed before ENABLE, live.

The saved class, not a fresh CONVENTIONAL-POLICY -- which would be a
different object with none of anybody's state on it."
  (let ((policy (p:current-policy)))
    (when (typep policy 'master-stack-mixin)
      (change-class policy (or (%previous-class policy)
                               (find-class 'p:conventional-policy)))))
  (values))
