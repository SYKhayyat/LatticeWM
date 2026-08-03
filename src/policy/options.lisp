;;;; policy/options.lisp --- Tier-0 configuration values, and the registry.
;;;;
;;;; DESIGN.org's tier table calls tier 0 "edit a DEFPARAMETER, no restart, and
;;;; the only tier available to a non-programmer".  Every P1 fork in the design
;;;; has to appear here as a value rather than as a branch buried in a method,
;;;; and the registry is what makes `latticewm --list-options' able to print
;;;; every one of them without anybody maintaining a second list.
;;;;
;;;; IT IS THE FIRST FILE IN THE POLICY LAYER, and that is not alphabetical
;;;; accident.  Logging is configurable — where the log goes, how big it gets
;;;; before it rotates — and logging is also the very first thing that has to
;;;; exist, because GUARDED is the boundary every policy method is called
;;;; behind.  So the registry has to precede even the log, or log.lisp would
;;;; have to declare its values as bare DEFVARs that no user could discover.

(in-package #:latticewm/policy)

(defvar *options* (make-hash-table :test #'eq)
  "NAME -> (list VARIABLE DEFAULT DOCUMENTATION).  See DEFINE-OPTION.")

(defmacro define-option (name default &body (documentation))
  "Declare a tier-0 configuration value: a variable a user edits, nothing more.

    (define-option *gaps* 0
      \"Pixels of empty space left between adjacent panes.\")

This is a DEFPARAMETER plus a registration, so that the extension-surface
document can list every knob without anyone maintaining a second list of them."
  (check-type documentation string)
  (let ((key (intern (string-trim "*" (symbol-name name)) :keyword)))
    `(progn
       (defparameter ,name ,default ,documentation)
       (setf (gethash ,key *options*) (list ',name ,default ,documentation))
       ',name)))

(defun option (name)
  "The current value of tier-0 option NAME, a keyword."
  (let ((entry (gethash name *options*)))
    (when entry (symbol-value (first entry)))))

(defun (setf option) (value name)
  (let ((entry (gethash name *options*)))
    (unless entry (error "No such option: ~s" name))
    (setf (symbol-value (first entry)) value)))

(defun option-variable (name)
  "The symbol option NAME is stored in, or NIL."
  (first (gethash name *options*)))

(defun option-default (name)
  "The value option NAME shipped with."
  (second (gethash name *options*)))

(defun option-documentation (name)
  "The docstring of option NAME."
  (third (gethash name *options*)))

(defun option-boundp (name)
  "True when NAME names a registered option."
  (nth-value 1 (gethash name *options*)))

(defun all-options ()
  "Every registered tier-0 option, as (KEYWORD VARIABLE VALUE DEFAULT DOC),
sorted by name."
  (let ((out '()))
    (maphash (lambda (key entry)
               (destructuring-bind (variable default documentation) entry
                 (push (list key variable (symbol-value variable)
                             default documentation)
                       out)))
             *options*)
    (sort out #'string< :key (lambda (row) (symbol-name (first row))))))
