;;;; policy/input.lisp --- The shipped answers for the *reading* half of
;;;; INPUT-POLICY, and the reservation hook every widget accumulates into.
;;;;
;;;; Completion, argument types, and how much of an output is left for the
;;;; layout once the echo area and any third-party panels have taken theirs.
;;;;
;;;; The completion style is here rather than in the minibuffer for a stated
;;;; reason: it is the single most personal decision in the whole prompt, and
;;;; the shipped answer is a default rather than a law.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; READING FROM THE USER
;;; ==================================================================

(define-option *argument-naming-convention*
  '((:direction     "direction")
    (:axis          "axis")
    (:side          "side")
    (:number        "number" "count" "steps" "step" "by" "index"
                    "cols" "rows" "cells" "columns" "visible" "x" "y")
    (:fraction      "amount" "fraction" "weight")
    (:pixels        "pixels" "distance")
    (:shell-command "command" "program")
    (:key           "spec" "key")
    (:name          "name" "label")
    (:string        "text" "string" "title")
    (:sexp          "form" "expression"))
  "Which kind of value a command's parameter holds, keyed by its name.

A convention rather than a declaration, so that a command written the obvious
way is interactively callable without its author doing anything.  Every
parameter name in the shipped commands is here, which is not a coincidence:
they were named to read well in a docstring, and a name that reads well in a
docstring is a name that says what kind of thing it is.

Where a name is genuinely ambiguous — SPAWN's COMMAND is a shell command line
and DESCRIBE-COMMAND's NAME is one of ours — DEFCOMMAND's (:interactive ...)
clause says so at the command instead of bending the table.

PATH is deliberately absent.  A tree path is a list of integers that nobody
should be asked to type, and leaving it out of the table is what makes M-x
say so rather than putting up a prompt that cannot be answered.")

(defmethod argument-type-for ((policy input-policy) command parameter)
  "The shipped naming convention.  See *ARGUMENT-NAMING-CONVENTION*."
  (declare (ignore command))
  (let ((name (string-downcase (string parameter))))
    (loop for (type . names) in *argument-naming-convention*
          when (member name names :test #'string=)
            return type)))

(defun subsequence-match-p (needle haystack)
  "True when NEEDLE's characters appear in HAYSTACK in order, gaps allowed.

The `fzf' match, and the reason `swsp' finds `send-to-workspace'.  Deliberately
last of the three tests in COMPLETE-CANDIDATES: on its own it matches nearly
everything, and a completion list that matches nearly everything has told you
nothing."
  (let ((position 0))
    (every (lambda (character)
             (let ((found (position character haystack :start position
                                                       :test #'char-equal)))
               (when found (setf position (1+ found)))))
           needle)))

(defun rank-candidates (candidates)
  "Shortest first, then alphabetical.

Length before alphabet because the shorter of two matches is nearly always the
more general one — `close' before `close-float' — and the general one is what
somebody typing four letters meant."
  (sort candidates (lambda (a b)
                     (if (= (length a) (length b))
                         (string< a b)
                         (< (length a) (length b))))))

(defmethod complete-candidates ((policy input-policy) input candidates)
  "Prefix, then substring, then subsequence.  See the generic's docstring."
  (if (zerop (length input))
      (rank-candidates (copy-list candidates))
      (let ((prefix '()) (substring '()) (fuzzy '()))
        (dolist (candidate candidates)
          (let ((position (search input candidate :test #'char-equal)))
            (cond ((eql position 0) (push candidate prefix))
                  (position (push candidate substring))
                  ((subsequence-match-p input candidate) (push candidate fuzzy)))))
        (nconc (rank-candidates prefix)
               (rank-candidates substring)
               (rank-candidates fuzzy)))))

(defun run-reserve-hooks (output)
  "The total reservation for OUTPUT, as (TOP RIGHT BOTTOM LEFT).

Reservations *accumulate*, which is a hook's shape and not a method's: two
status bars should each get their strip and the second must not have to know
about the first.  So this is the one place that reads what RUN-HOOKS returns.

It goes through the declared hook mechanism rather than through a special
variable of its own, which it used to.  Two ways to hook is one way too many —
gate 7 could see the declared one and not the other, and the extension guide
documented only the half a user could find."
  (let ((total (list 0 0 0 0)))
    (dolist (edges (run-hooks :reserve-space output) total)
      (when (and (listp edges) (= 4 (length edges))
                 (every #'realp edges))
        (setf total (mapcar #'+ total edges))))))

(defmethod reserved-space ((policy layout-policy) (output c:output))
  "Whatever the reserve hooks ask for, and nothing else.

Consults the runtime through a hook rather than calling it directly, because
policy may not depend on the runtime — the echo area is a runtime thing, a
third-party panel's exclusive zone is a compositor thing, and this is the seam
between all three."
  (run-reserve-hooks output))

;;; ------------------------------------------------------- keys, as policy

(defmethod on-key ((policy input-policy) world key)
  "The shipped answer is `I did not handle it', so the binding runs.

Specializing this is how a modal layer -- a resize mode, a vi-style submap --
intercepts everything without unbinding anything."
  (declare (ignore world key))
  nil)
