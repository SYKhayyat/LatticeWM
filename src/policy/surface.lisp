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

;;;; SPECIALIZER-NAME, METHOD-DESCRIPTION and GENERIC-DESCRIPTION used to live
;;;; here.  They describe a CLOS generic and know nothing about policy, and the
;;;; container protocol needs the identical description in the identical format
;;;; — so they are in model/surface.lisp now and both surfaces call them.  Two
;;;; printers for one format are two chances to disagree about it, and a
;;;; generated document whose whole value is that it cannot drift is a poor
;;;; place to keep a second copy of the formatter.

(in-package #:latticewm/policy)

(defun extension-surface ()
  "The whole extension surface, as data.

This is what the SWANK bridge answers 'what can I change?' with, and it is
deliberately machine-readable first and pretty second — PLAN.org's framing is
that the realistic post-expiry maintainer is a cheap model plus whatever
contributors the release attracts, and a model would rather have a plist."
  (list :generics (mapcar #'c:generic-description (policy-generics))
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
Every generic below dispatches on a policy as its first argument.  To change~%~
one, write a method; it takes effect the moment you evaluate it.~2%~
    (defmethod gaps ((policy conventional-policy) container) 8)~2%~
Specialize on CONVENTIONAL-POLICY, not on the class the shipped method uses.~%~
The defaults sit on the six protocols POLICY implements -- LAYOUT-POLICY,~%~
APPEARANCE-POLICY, MOTION-POLICY, STRUCTURE-POLICY, LIFECYCLE-POLICY and~%~
INPUT-POLICY -- so that a mixin answering for one of them is a real thing you~%~
can write.  Yours has to be strictly more specific than theirs, or it~%~
replaces the shipped answer instead of extending it and CALL-NEXT-METHOD~%~
signals NO-NEXT-METHOD.~2%~
The `methods' list under each generic includes yours, which is how you check~%~
that your configuration file was actually loaded.~2%~
This is one of two surfaces.  The other is the *container* protocol -- what a~%~
new container kind answers rather than what a new policy answers -- and it is~%~
printed by `latticewm --container-surface'.~2%")
    (c:print-generic-descriptions (getf surface :generics) stream)
    (format stream "~&~76,,,'=<~>~%OPTIONS~%~76,,,'=<~>~2%")
    (dolist (row (getf surface :options))
      (destructuring-bind (key variable value default documentation) row
        (declare (ignore key))
        (format stream "~(~a~)~%  now: ~s~@[   (default ~s)~]~%  ~a~2%"
                variable value (unless (equal value default) default)
                documentation)))
    (values)))
