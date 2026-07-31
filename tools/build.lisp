;;;; tools/build.lisp --- Load the system with every warning made loud.
;;;;
;;;; Gate 1 of PLAN.org: zero compiler warnings.  The spike verified that this
;;;; catches wrong arity, wrong-typed constants, typo'd accessors and typo'd
;;;; calls at compile time — the largest slice of what a type system was going
;;;; to catch, for the price of a DECLAIM.  It will not catch a wrong-but-well-
;;;; typed argument.  Cheap enough that not doing it would be indefensible.
;;;;
;;;; Redefinition warnings are filtered, and only those.  Compiling a file and
;;;; then loading its fasl in the same image necessarily defines every macro
;;;; twice; that is an artifact of the build harness, not of the code, and
;;;; treating it as a failure would train everybody to ignore the gate.
;;;;
;;;;   sbcl --non-interactive --load tools/build.lisp [system]

(require :asdf)
(require :sb-introspect)

(defparameter *system* (or (second sb-ext:*posix-argv*) "latticewm"))
(defparameter *real* '())
(defparameter *redefinitions* 0)

(defun record (condition)
  (if (typep condition 'sb-kernel:redefinition-warning)
      (incf *redefinitions*)
      (pushnew (princ-to-string condition) *real* :test #'string=)))

(handler-bind ((warning (lambda (c) (record c) (muffle-warning c))))
  (handler-case (asdf:load-system *system*)
    (error (e)
      (format t "~&~%======== BUILD FAILED ========~%~a~%" e)
      (sb-ext:quit :unix-status 1))))

(format t "~&~%======== GATE 1: compiler warnings ========~%")
(if *real*
    (progn
      (format t "~d warning~:p:~%" (length *real*))
      (dolist (w (reverse *real*)) (format t "  ~a~%" w))
      (format t "~%FAIL~%")
      (sb-ext:quit :unix-status 1))
    (format t "clean (~d benign redefinition~:p from compile-then-load)~%PASS~%"
            *redefinitions*))
