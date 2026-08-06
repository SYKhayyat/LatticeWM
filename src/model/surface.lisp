;;;; model/surface.lisp --- Protocols described from the image, and the
;;;; container protocol in particular.
;;;;
;;;; THE HALF OF THE EXTENSION SURFACE THAT HAD NO DOCUMENT.
;;;;
;;;; policy/surface.lisp generates the policy protocol from the running image,
;;;; for the reason PLAN.org gives: an extension surface that is documented by
;;;; hand rots, and one generated from the image cannot.  That argument is not
;;;; about policy.  It is about extension surfaces, and this project has two.
;;;;
;;;; The container protocol — the generics a *new container kind* answers, as
;;;; against the generics a new *policy* answers — had no membership test, no
;;;; generated document and no documentation gate.  The consequences were
;;;; exactly what the absence of a check predicts:
;;;;
;;;;   * REPLACE-CHILD was exported from LATTICEWM/CORE, listed in
;;;;     package.lisp under the comment `the container protocol', and defined
;;;;     nowhere at all.  A DEFGENERIC that does not exist, on the advertised
;;;;     surface, for the whole life of the project.
;;;;   * SERIALIZE-NODE and DESERIALIZE-NODE were declared in
;;;;     runtime/state.lisp.  They are as much a part of the contract as
;;;;     CHILD-AT — a kind that skips them loses the user's layout on every
;;;;     restart, silently — and no document anywhere named them.
;;;;   * Nothing told a stranger how many members there are.  A grid written
;;;;     against model/node.lisp answered six of them, loaded fine, and broke
;;;;     on copy, on save and on focus repair.  That is not hypothetical; it
;;;;     is what happened, with the source open.
;;;;
;;;; So: the same machinery, asked of the other protocol.  The description
;;;; helpers below are shared with policy/surface.lisp rather than copied,
;;;; because two printers for one format are two chances to disagree about it
;;;; and the whole value of a generated document is that it cannot.

(in-package #:latticewm/core)

;;; ------------------------------------------------------ describing generics

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

(defun generic-description (name)
  "One protocol generic, as a plist.

NAME is a function name, so it may be a symbol or (SETF SYMBOL) — the container
protocol has two of the latter and a describer that only accepted symbols would
have quietly dropped the half of CHILD-AT that a new kind must implement."
  (let ((gf (fdefinition name)))
    (list :name name
          :lambda-list (closer-mop:generic-function-lambda-list gf)
          :documentation (documentation name 'function)
          :methods (mapcar #'method-description
                           (closer-mop:generic-function-methods gf)))))

(defun print-generic-descriptions (entries &optional (stream *standard-output*))
  "Print the generics of ENTRIES, one block each.  Both surfaces use this."
  (dolist (entry entries)
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
  (values))

;;; -------------------------------------------------- the container protocol

(defparameter +container-protocol-extras+ '(deserialize-node)
  "Container-protocol generics that do not dispatch on a node.

One, and it is unavoidable: DESERIALIZE-NODE dispatches on the *tag*
SERIALIZE-NODE wrote, because reading a node back has to work before the node
exists.  It is half of the persistence contract and a kind that implements only
the other half loses the user's layout, so it is on the surface — and a purely
structural test cannot see it.  Named here, once, immediately beside the test
that misses it, rather than left off the document that exists to be complete.")

(defun accessor-generic-p (gf)
  "True when GF is nothing but a DEFCLASS slot accessor.

Slot accessors are *state*, and the surface is *decisions*.  This is the exact
distinction POLICY-GENERICS records learning the hard way: ten CLOS accessors
became extension-surface entries, gate 2 demanded docstrings for them, and the
document's own printed contract became false.  The container protocol has more
accessors than the policy one — CHILDREN, WEIGHTS, SPLIT-AXIS, STACK-SELECTED,
PROPS, NODE-LABEL, LEAF-WINDOW are all readers or writers on node subclasses —
so the same mistake was one structural test away from being made twice.

A generic with even one hand-written method is not an accessor: something
decided to intervene, and that is a decision."
  (let ((methods (closer-mop:generic-function-methods gf)))
    (and methods
         (every (lambda (method)
                  (or (typep method 'closer-mop:standard-reader-method)
                      (typep method 'closer-mop:standard-writer-method)))
                methods))))

(defun node-lineage-p (specializer)
  "True when SPECIALIZER is a class at or under NODE.

Only downwards, unlike POLICY-LINEAGE-P.  The policy protocol has six protocol
classes *above* POLICY that its defaults sit on, so a test written one way round
loses them; the container protocol's root is NODE and there is nothing above it
but STANDARD-OBJECT and T.  Accepting T would make every unspecialized argument
of every exported generic in this package a container-protocol member."
  (and (typep specializer 'class)
       (subtypep specializer (find-class 'node))))

(defun container-protocol-p (name)
  "True when NAME is a member of the container protocol.

Structural, for the same reason POLICY-GENERIC-P is: a generic function
exported from this package with at least one method specialized somewhere on
the node lineage, in any argument position — (SETF CHILD-AT) takes the node
first and the container second, and a test that only looked at argument one
would have dropped it.  Slot accessors are excluded; see ACCESSOR-GENERIC-P.

A maintained list would drift.  This cannot, and the one member it structurally
cannot see is named in +CONTAINER-PROTOCOL-EXTRAS+ rather than left out."
  (let ((symbol (if (consp name) (second name) name)))
    (and (symbolp symbol)
         (eq (nth-value 1 (find-symbol (symbol-name symbol) '#:latticewm/core))
             :external)
         (fboundp name)
         (typep (fdefinition name) 'generic-function)
         (let ((gf (fdefinition name)))
           (and (not (accessor-generic-p gf))
                (or (member symbol +container-protocol-extras+)
                    (some (lambda (method)
                            (some #'node-lineage-p
                                  (closer-mop:method-specializers method)))
                          (closer-mop:generic-function-methods gf))))))))

(defun container-protocol-generics ()
  "Every container-protocol generic, sorted by name.

This is what a new container kind has to answer, and it is what the generated
container-surface document walks.  Both readers and writers: (SETF CHILD-AT)
and (SETF CONTAINER-SELECTION) are members in their own right and a kind that
implements the reader alone is half-written."
  (let ((out '()))
    (do-external-symbols (symbol '#:latticewm/core)
      (dolist (name (list symbol (list 'setf symbol)))
        (when (container-protocol-p name)
          (pushnew name out :test #'equal))))
    ;; By name, then reader before writer.  Sorting on the symbol name alone
    ;; leaves CHILD-AT and (SETF CHILD-AT) tied, and a generated document whose
    ;; entries swap order between builds produces a diff that says nothing —
    ;; which is the precise failure a generated document exists to avoid.
    (sort out (lambda (a b)
                (let ((na (symbol-name (if (consp a) (second a) a)))
                      (nb (symbol-name (if (consp b) (second b) b))))
                  (if (string= na nb)
                      (and (not (consp a)) (consp b))
                      (string< na nb)))))))

(defun undocumented-container-generics ()
  "Every container-protocol generic with no docstring.  Gate 2 fails on any."
  (remove-if (lambda (name) (documentation name 'function))
             (container-protocol-generics)))

(defun container-surface ()
  "The container protocol, as data.

Machine-readable first and pretty second, for the reason EXTENSION-SURFACE
gives: the realistic post-expiry maintainer is a cheap model, and a model would
rather have a plist."
  (list :generics (mapcar #'generic-description (container-protocol-generics))))

(defun print-container-surface (&optional (stream *standard-output*))
  "Print the container protocol for a human.

The counterpart of PRINT-EXTENSION-SURFACE, and the preamble is doing the same
job its preamble does: saying what the reader is looking at and what the
smallest correct answer to it is."
  (let ((entries (getf (container-surface) :generics)))
    (format stream "~&~76,,,'=<~>~%~
                    LatticeWM container protocol~%~
                    ~d generic~:p.  Generated from the running image.~%~
                    ~76,,,'=<~>~2%"
            (length entries))
    (format stream "~
A CONTAINER holds children at ADDRESSES.  That is the whole abstraction, and~%~
every structural operation in the window manager -- motion, layout, insert,~%~
remove, move, swap, focus repair, copy, undo, persistence -- is written once~%~
against the generics below.  Answer them and a container kind you invented is~%~
a first-class citizen of a program that has never heard of it.~2%~
    (defclass my-grid (c:container) ((cells :initform (make-hash-table ...))))~%~
    (defmethod c:container-addresses ((c my-grid)) ...)~2%~
THE LIST IS THE POINT.  Six of these are obvious and the rest are not, and a~%~
kind that answers only the obvious six loads without complaint and then loses~%~
data.  The ones that are missed, in the order they were actually missed:~2%~
  COPY-NODE-SLOTS      undo and layout snapshots silently drop your children~%~
  SERIALIZE-NODE       every restart loses the arrangement, and the windows~%~
  DESERIALIZE-NODE     its other half; both or neither~%~
  NODE-SIGNATURE       undo skips past the operation the user wanted back~%~
  DEFAULT-ADDRESS      focus repair lands somewhere that is not on screen~%~
  ADDRESS-EQUAL        EQL is wrong the moment an address is not an integer~2%~
Defaults exist for most of them on CONTAINER, so the minimum is smaller than~%~
this list -- but the minimum is not what a kind with state of its own needs.~%~
The `methods' list under each generic includes yours, which is how you check~%~
that your kind is actually answering rather than inheriting.~2%")
    (print-generic-descriptions entries stream)
    (values)))
