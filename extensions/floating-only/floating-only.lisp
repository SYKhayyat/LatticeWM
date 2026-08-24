;;;; floating-only/floating-only.lisp --- Where does a new window go?  Nowhere.

(in-package #:floating-only)

;;; ================================================================ state

(defvar *enabled* nil "True while the installed policy floats everything.")

(defvar *previous-class* nil
  "The policy class that was installed when ENABLE ran, so DISABLE can put
back exactly what was there -- not merely a conventional policy.")

(defun enabled-p () "True while everything floats." *enabled*)

;;; ================================================================ policy

(defclass floating-only-policy (p:conventional-policy)
  ()
  (:documentation
   "The no-layout policy.

Every question placement asks, this class answers the same way: nothing
tiles.  It says so through SHOULD-FLOAT-P -- the shipped escape hatch a
declarative :float rule uses for one application at a time, answered here
for every window unconditionally -- rather than by reimplementing
placement, because ON-WINDOW-OPEN already knows what to do with a floated
window (never enter the tree) and reimplementing THAT would be the mistake
the surface exists to make unnecessary."))

(defmethod p:should-float-p ((policy floating-only-policy) (window c:window))
  (declare (ignore window))
  ;; Yes.  That is the whole policy.
  t)

;;; ============================================================== plumbing

(defun enable ()
  "Install the floating-only policy over whatever is installed.

Uses CHANGE-CLASS rather than replacing *POLICY*, so anything the previous
instance was carrying -- a master-stack mixin, say -- stays carried; the
class it had is remembered for DISABLE."
  (let ((policy (p:current-policy)))
    (unless (typep policy 'floating-only-policy)
      (setf *previous-class* (class-of policy))
      (change-class policy 'floating-only-policy)))
  (setf *enabled* t)
  nil)

(defun disable ()
  "Put back the policy class that was installed before.  Idempotent."
  (when (and *enabled*
             (typep (p:current-policy) 'floating-only-policy)
             *previous-class*)
    (change-class (p:current-policy) *previous-class*)
    (setf *previous-class* nil))
  (setf *enabled* nil)
  nil)
