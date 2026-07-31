;;;; examples/03-master-stack.lisp
;;;;
;;;; TIER 3 — a whole layout model, in one method.
;;;;
;;;; The master-and-stack layout: one large pane on the left holding the window
;;;; you are working in, and everything else in a column down the right.  It is
;;;; dwm's layout, xmonad's default, and the thing most people mean by "tiling
;;;; window manager".  LatticeWM does not ship it, because splits are more
;;;; general and master-stack is a *restriction* of them.
;;;;
;;;; Here it is anyway, as a policy, in about fifteen lines of actual code.
;;;;
;;;; WHY THIS IS THE INTERESTING EXAMPLE.  It is not a tweak — it changes what
;;;; the window manager *is*.  And it does so without touching the tree: the
;;;; splits are still there, still nested, still hold exactly what they held.
;;;; Only the answer to "how does this container divide its rectangle" changed.
;;;; Switch back with (setf *policy* (make-instance 'conventional-policy)) and
;;;; every window is where you left it.
;;;;
;;;; That separation — structure in the tree, arrangement in the policy — is
;;;; what makes a new layout cost one method instead of a fork.

(in-package #:latticewm/user)

(defclass master-stack-policy (conventional-policy)
  ((ratio :initarg :ratio :initform 3/5 :accessor master-ratio
          :documentation "Fraction of the width the master pane gets.")
   (masters :initarg :masters :initform 1 :accessor master-count
            :documentation "How many panes share the master column."))
  (:documentation
   "One large pane on the left, the rest stacked on the right."))

(defmethod layout-children ((policy master-stack-policy) (split split) rect)
  "Ignore the split's own axis and weights; impose master-and-stack instead.

The split is untouched — this only decides where its children are drawn.  Note
that nested splits get the same treatment recursively, which is what makes the
result look like dwm rather than like dwm-with-one-odd-corner."
  (let* ((addresses (container-addresses split))
         (n (length addresses)))
    (cond
      ((zerop n) '())
      ;; One child: it takes everything.  Falling through to the general case
      ;; would give it the master fraction and leave the rest blank.
      ((= n 1) (list (cons (first addresses) rect)))
      (t
       (let* ((masters (min (master-count policy) (1- n)))
              (gap (gaps policy split))
              (split-x (+ (rect-x rect)
                          (round (* (rect-w rect) (master-ratio policy)))))
              (master-rect (make-rect (rect-x rect) (rect-y rect)
                                      (- split-x (rect-x rect) (round gap 2))
                                      (rect-h rect)))
              (stack-rect (make-rect (+ split-x (round gap 2)) (rect-y rect)
                                     (- (rect-right rect) split-x (round gap 2))
                                     (rect-h rect))))
         (append
          (mapcar #'cons (subseq addresses 0 masters)
                  (divide-rect master-rect :vertical
                               (make-list masters :initial-element 1) :gap gap))
          (mapcar #'cons (subseq addresses masters)
                  (divide-rect stack-rect :vertical
                               (make-list (- n masters) :initial-element 1)
                               :gap gap))))))))

(defmethod step-address ((policy master-stack-policy) (split split)
                         address direction)
  "Motion has to follow the *drawn* arrangement, not the split's stored axis.

This is the part that is easy to forget and that makes the difference between a
layout that works and one that looks right and navigates wrongly.  The children
are drawn as a column beside a column, so Left and Right cross between master
and stack, and Up and Down move within one."
  (let ((n (container-count split))
        (masters (min (master-count policy) (max 0 (1- (container-count split))))))
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

;;; Verbs for the two knobs.  Both are ordinary commands, so both can be bound.

(defcommand master-wider (&optional (amount 1/20))
  "Give the master column more of the width."
  (when (typep *policy* 'master-stack-policy)
    (setf (master-ratio *policy*)
          (max 1/10 (min 9/10 (+ (master-ratio *policy*) amount))))
    (relayout)))

(defcommand master-narrower (&optional (amount 1/20))
  "Give the master column less of the width."
  (master-wider (- amount)))

(defcommand more-masters (&optional (by 1))
  "Move a window from the stack into the master column."
  (when (typep *policy* 'master-stack-policy)
    (setf (master-count *policy*) (max 1 (+ (master-count *policy*) by)))
    (relayout)))

(defcommand master-stack ()
  "Switch to the master-and-stack layout.  Nothing in the tree changes."
  (setf *policy* (make-instance 'master-stack-policy))
  (relayout :force t))

(defcommand conventional ()
  "Switch back to recursive splits.  Every window is where you left it."
  (setf *policy* (make-instance 'conventional-policy))
  (relayout :force t))

;; (define-key *keymap* "Super+F1" '("conventional"))
;; (define-key *keymap* "Super+F2" '("master-stack"))
;; (define-key *keymap* "Super+comma" '("master-narrower"))
;; (define-key *keymap* "Super+period" '("master-wider"))
