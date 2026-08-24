;;;; transient-rules/transient-rules.lisp --- The queue, and one method.
;;;;
;;;; Consulted at the same door permanent rules are: WINDOW-RULE-FOR, once,
;;;; when the window appears.  An entry that matches is consumed -- removed
;;;; from the queue BEFORE placement runs, so a rule cannot fire twice even if
;;;; placement signals and something retries.

(in-package #:transient-rules)

(defvar *queue* '()
  "Waiting transient rules, most recently added first.

Each entry is (MATCH . OVERRIDES): MATCH is an app-id string or a function of
the window; OVERRIDES is the plist WINDOW-RULE-FOR documents.  First match in
the list wins and is consumed.")

(defun add-rule (match &rest overrides)
  "Queue a rule: the NEXT window MATCH accepts gets OVERRIDES, once.

MATCH is an app-id string or a function of the window."
  (push (cons match overrides) *queue*)
  (length *queue*))

(defun pending-rules ()
  "The queue, oldest first -- what you would read to see what is armed."
  (reverse *queue*))

(defun clear-rules ()
  "Disarm everything waiting."
  (setf *queue* '())
  (values))

(defun matches-p (match window)
  (etypecase match
    (string (equal match (c:window-app-id window)))
    (function (funcall match window))))

(defmethod p:window-rule-for ((policy p:conventional-policy) (win c:window))
  "Consult the queue before the shipped answer; consume on match.

A PRIMARY method, deliberately not an :AROUND: window-rules already hangs an
:AROUND on exactly these specializers, and two methods with identical
qualifiers and specializers do not compose -- the later definition REPLACES
the earlier one, quietly, leaving whichever module loaded second in charge of
the door. A primary on CONVENTIONAL-POLICY is strictly more specific than the
shipped answer on LIFECYCLE-POLICY and coexists with every :AROUND above it,
which is the whole composition story.

Consumed BEFORE CALL-NEXT-METHOD runs, so a signal partway through placement
cannot re-arm a rule that has already had its one shot."
  (let ((entry (find-if (lambda (e) (matches-p (car e) win)) *queue*)))
    (when entry
      (setf *queue* (remove entry *queue* :test #'eq))
      (return-from p:window-rule-for (cdr entry))))
  (call-next-method))

;;; ------------------------------------------------------------- commands

(r:defcommand float-next (match)
  "The next window whose app-id is MATCH floats, once."
  (:interactive :string)
  (add-rule match :float t))

(r:defcommand workspace-next (match workspace)
  "The next window whose app-id is MATCH goes to WORKSPACE, once."
  (:interactive :string :number)
  (add-rule match :workspace workspace :focus nil))

(r:defcommand clear-transient-rules ()
  "Disarm every waiting transient rule."
  (clear-rules)
  (r:notify "transient rules cleared"))

;;; Make the commands typable from init.lisp and M-x without prefixes: the
;;; lattice's reachability pattern.
(defun install-vocabulary (&optional (package '#:latticewm/user))
  (let ((target (find-package package))
        (source (find-package '#:transient-rules)))
    (when (and target source)
      (let ((taken '()))
        (do-external-symbols (symbol source)
          (multiple-value-bind (theirs status)
              (find-symbol (symbol-name symbol) target)
            (when (and status (not (eq theirs symbol)))
              (push theirs taken))))
        (when taken (shadowing-import taken target))
        (use-package source target)))
    (values)))

(install-vocabulary)
