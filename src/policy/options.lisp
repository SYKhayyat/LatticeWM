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

;;; ------------------------------------------------- who actually looks at it
;;;
;;; AN OPTION IS A PROMISE, AND NOTHING HERE COULD CHECK THE PROGRAM KEEPS IT.
;;; DEFINE-OPTION registers a name, a default and a docstring; the code that
;;; reads the value is somewhere else entirely, and until this function existed
;;; nothing connected the two.  So `latticewm --list-options', the generated
;;; extension surface and the config man page could all print a knob that was
;;; wired to nothing, in a build where every gate passed — and did, for
;;; *SMART-GAPS*, for the whole life of the option.
;;;
;;; This is the same species as the bug PLAN §log3 is proudest of finding:
;;; twenty-four options that could not be *set* from a configuration file.  The
;;; test written in response checks symbol identity between the registry and
;;; the user package, which is exactly the right shape — check the relationship
;;; between two independently maintained artifacts, because that is the thing
;;; no human ever verifies.  It just checked the wrong end.  Being settable and
;;; being read are two different promises and an option makes both.
;;;
;;; ASK THE COMPILER, NOT THE TEXT.  A grep for the name would count the
;;; DEFPARAMETER, the export, the docstring that mentions it and the man page,
;;; and would miss nothing only by accident.  SBCL already recorded who
;;; references what while it was compiling the system, which is a fact about
;;; the *program* rather than about its spelling: it sees a read inside a macro
;;; expansion and does not see the name in a comment.

(defun option-readers (name)
  "Every function and method in the loaded image that reads option NAME.

Function names, so a method comes back as SBCL writes it —
(SB-PCL::FAST-METHOD GAPS (LAYOUT-POLICY T)) — which OPTION-READER-NAME turns
into something to say out loud.

Empty for an option nothing looks at.  Gate 11 fails on that, and it is worth
being clear about why it is a gate and not a warning: an option that is read
nowhere is indistinguishable, from outside, from one that is read everywhere.
The user sets it, nothing happens, and the documentation agrees with them."
  (let ((variable (option-variable name)))
    (when variable
      (remove-duplicates (mapcar #'first (sb-introspect:who-references variable))
                         :test #'equal))))

(defun option-reader-name (reader)
  "READER as a string, with a method written the way you would specialize it.

The method case is the one that matters.  When the only thing reading an
option is the shipped method of a generic, then *that generic is the real
extension point* and the option is its default — so a policy that overrides
the method stops reading the option, and setting it does nothing.  That is the
whole answer to `is *GAPS* or GAPS in charge', and it is a fact about the
image rather than a sentence somebody has to remember to keep true."
  (if (and (consp reader)
           (symbolp (first reader))
           (search "METHOD" (symbol-name (first reader))))
      (format nil "~(~a ~a~)" (second reader) (or (third reader) '()))
      (format nil "~(~a~)" reader)))

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
