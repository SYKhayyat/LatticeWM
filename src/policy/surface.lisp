;;;; policy/surface.lisp --- The extension surface, described from the image.
;;;;
;;;; PLAN.org, gate 2, and it explains why this is generated rather than
;;;; written:
;;;;
;;;;   "The extension-surface document generated *from the live image*, not
;;;;   from the source: each policy generic, its lambda list, its docstring,
;;;;   and every method with its specializers. […] Pull this forward to Week 1
;;;;   and make it a CI gate.  It is the cheapest available forcing function
;;;;   against this document's own worst fear — an extension surface that rots
;;;;   because documenting it is a separate chore from writing it.  Generated
;;;;   from the image, it is not a separate chore, and it cannot silently drift
;;;;   out of date."
;;;;
;;;; It also does something a written document cannot: it lists the methods
;;;; *you* have added, so a user with a configuration file can ask the running
;;;; window manager what they have changed.

(in-package #:latticewm/policy)

(defun specializer-name (specializer)
  "A readable name for a method specializer."
  (typecase specializer
    (closer-mop:eql-specializer
     (format nil "(eql ~s)" (closer-mop:eql-specializer-object specializer)))
    (class (string-downcase (class-name specializer)))
    (t (princ-to-string specializer))))

(defun method-description (method)
  "A method as (SPECIALIZERS QUALIFIERS SOURCE-FILE)."
  (list (mapcar #'specializer-name (closer-mop:method-specializers method))
        (method-qualifiers method)
        (ignore-errors
         (let ((source (sb-introspect:find-definition-source method)))
           (when source
             (let ((path (sb-introspect:definition-source-pathname source)))
               (when path (file-namestring path))))))))

(defun generic-description (symbol)
  "One extension-surface generic, as a plist."
  (let ((gf (fdefinition symbol)))
    (list :name symbol
          :lambda-list (sb-introspect:function-lambda-list symbol)
          :documentation (documentation symbol 'function)
          :methods (mapcar #'method-description
                           (closer-mop:generic-function-methods gf)))))

(defun extension-surface ()
  "The whole extension surface, as data.

This is what the SWANK bridge answers 'what can I change?' with, and it is
deliberately machine-readable first and pretty second — PLAN.org's framing is
that the realistic post-expiry maintainer is a cheap model plus whatever
contributors the release attracts, and a model would rather have a plist."
  (list :generics (mapcar #'generic-description (policy-generics))
        :options (all-options)))

(defun undocumented-generics ()
  "Every extension-surface generic with no docstring.  Gate 2 fails on any."
  (remove-if (lambda (symbol) (documentation symbol 'function))
             (policy-generics)))

(defun print-extension-surface (&optional (stream *standard-output*))
  "Print the extension surface for a human.

Prints UNDOCUMENTED <-- flag me for anything missing a docstring, which is the
build gate's failure output as well as a nudge to whoever is reading."
  (let ((surface (extension-surface)))
    (format stream "~&~76,,,'=<~>~%~
                    LatticeWM extension surface~%~
                    ~d generic~:p, ~d option~:p.  ~
                    Generated from the running image.~%~
                    ~76,,,'=<~>~2%"
            (length (getf surface :generics)) (length (getf surface :options)))
    (format stream "~
Every generic below takes a POLICY as its first argument.  To change one,~%~
write a method; it takes effect the moment you evaluate it.~2%~
    (defmethod gaps ((policy conventional-policy) container) 8)~2%")
    (dolist (entry (getf surface :generics))
      (format stream "~&~76,,,'-<~>~%~(~a~) ~(~s~)~%~76,,,'-<~>~%"
              (getf entry :name) (getf entry :lambda-list))
      (let ((documentation (getf entry :documentation)))
        (if documentation
            (format stream "~a~%" documentation)
            (format stream "UNDOCUMENTED <-- flag me~%")))
      (format stream "~%  methods:~%")
      (dolist (method (getf entry :methods))
        (destructuring-bind (specializers qualifiers source) method
          (format stream "    (~{~a~^ ~})~@[ ~{~a~^ ~}~]~@[~40t; ~a~]~%"
                  specializers qualifiers source)))
      (terpri stream))
    (format stream "~&~76,,,'=<~>~%OPTIONS~%~76,,,'=<~>~2%")
    (dolist (row (getf surface :options))
      (destructuring-bind (key variable value default documentation) row
        (declare (ignore key))
        (format stream "~(~a~)~%  now: ~s~@[   (default ~s)~]~%  ~a~2%"
                variable value (unless (equal value default) default)
                documentation)))
    (values)))
