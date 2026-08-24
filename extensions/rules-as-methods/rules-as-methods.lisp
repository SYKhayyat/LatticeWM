;;;; rules-as-methods/rules-as-methods.lisp --- The table becomes the language.

(in-package #:rules-as-methods)

;;; ================================================================ state

(defvar *enabled* nil "True while the bridge hook is installed.")

(defun enabled-p () "True when rules are being consulted." *enabled*)

;;; ============================================================ the generic

(defgeneric rule-overrides (app-id)
  (:documentation
   "The overrides for the application named APP-ID, as a plist, or NIL.

Methods on this generic ARE the rules: eql-specialize the APP-ID argument on
the application's id string.  CLOS keeps them, orders them by specificity,
lets you remove one with REMOVE-METHOD, and lets a REPL redefine one live --
which is the entire argument for this over a table of pairs."))

(defmethod rule-overrides ((app-id string))
  "No method says otherwise: NIL, and placement does what it always does."
  (declare (ignore app-id))
  nil)

;;; ================================================================ macros

(defmacro define-app-id-rule (app-id &body overrides)
  "Define a rule for the application whose id is APP-ID, as a method.

    (define-app-id-rule \"pavucontrol\" :float t)
    (define-app-id-rule \"thunderbird\" :workspace 3 :focus nil)

The expansion is an ordinary DEFMETHOD eql-specialized on APP-ID, so
redefining replaces live, and every tool that works on methods works on
your rules.  OVERRIDES is whatever the shipped rule language understands."
  `(defmethod rule-overrides ((app-id (eql ,app-id)))
     (declare (ignore app-id))
     (list ,@overrides)))

(defun all-rules ()
  "Every rule currently defined, as (APP-ID OVERRIDES), sorted by app-id.

Read off the generic's methods -- the list and the truth cannot drift apart,
because they are the same object."
  (let ((out '()))
    (dolist (method (sb-mop:generic-function-methods #'rule-overrides)
                    (sort out #'string< :key #'first))
      (let* ((specializers (sb-mop:method-specializers method))
             (first (first specializers)))
        (when (typep first 'sb-mop:eql-specializer)
          (let ((app-id (sb-mop:eql-specializer-object first)))
            (when (stringp app-id)
              (push (list app-id
                          (copy-list
                           (ignore-errors
                            (funcall method app-id))))
                    out))))))))

(defun remove-app-id-rule (app-id)
  "Remove the rule for APP-ID, if there is one.  Returns the app-id or NIL."
  (dolist (method (sb-mop:generic-function-methods #'rule-overrides) nil)
    (let* ((specializers (sb-mop:method-specializers method))
           (first (first specializers)))
      (when (and (typep first 'sb-mop:eql-specializer)
                 (equal (sb-mop:eql-specializer-object first) app-id)
                 ;; The default method has a CLASS specializer; only remove
                 ;; what an EQL specializer names.
                 (not (rest specializers)))
        (remove-method #'rule-overrides method)
        (return app-id)))))

;;; =============================================================== bridge

(defun rule-hook-bridge (window)
  "Answer :WINDOW-RULE out of the methods, or NIL.

One function, one hook: the module's whole presence in placement is this
consultation, and everything else is Lisp-level structure over it."
  (let ((app-id (c:window-app-id window)))
    (when app-id
      (rule-overrides app-id))))

(defun enable ()
  "Start answering placement questions from the methods.  Idempotent."
  (p:add-hook :window-rule 'rule-hook-bridge)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop.  The methods stay defined -- DISABLE stops consulting them, which
is not the same as forgetting them."
  (p:remove-hook :window-rule 'rule-hook-bridge)
  (setf *enabled* nil)
  nil)
