;;;; tests/suite.lisp --- The test harness.
;;;;
;;;; A window manager is mostly untestable without a compositor.  The model is
;;;; the exception, and it is also the part where a bug is worst — a focus
;;;; repair that is subtly wrong produces "where did my cursor go" reports that
;;;; are close to undebuggable, because each instance is local and plausible.
;;;;
;;;; So: everything in src/model/ and src/policy/ is tested here, with no
;;;; compositor, no protocol, and no globals.

(defpackage #:latticewm/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:export #:run-all #:model #:geometry #:tree #:motion #:lifecycle #:surface
           #:minibuffer))

(in-package #:latticewm/tests)

(def-suite model :description "Everything testable without a compositor.")

(def-suite geometry :in model)
(def-suite tree :in model)
(def-suite motion :in model)
(def-suite lifecycle :in model)
(def-suite surface :in model)
(def-suite minibuffer :in model
  :description "Reading a line, which needs no compositor either — the prompt
is a string, a point and a table of what keys mean.")

(defun run-all ()
  "Run every suite and return T when they all pass.

Called by `make test' and by ASDF's TEST-OP."
  (let ((results (append (run 'model)
                         (run (find-symbol "EXAMPLES" "LATTICEWM/TESTS/EXAMPLES")))))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun policy ()
  "A fresh conventional policy, so tests never share option state."
  (make-instance 'p:conventional-policy))

(defun leaves (n)
  "N distinct empty leaves, so a test can tell them apart by identity."
  (loop repeat n collect (c:make-leaf)))

(defun win (&optional (app-id "test"))
  "A window object with no proxy — the model never dereferences one."
  (make-instance 'c:window :app-id app-id))

(defun leaf-with (app-id)
  "A leaf holding a fresh window tagged APP-ID."
  (c:make-leaf (win app-id)))

(defun byte-vector (length &rest bytes)
  "A glyph table of LENGTH bytes, starting with BYTES and zero after that."
  (let ((v (make-array length :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (replace v (coerce bytes 'vector))
    v))

(defun app-at (root path)
  "The app-id of the window at PATH under ROOT, or NIL.

Tests assert on this rather than on node identity, because it says what the
user would see."
  (let ((node (c:resolve-path root path)))
    (when (typep node 'c:leaf)
      (let ((w (c:leaf-window node)))
        (when w (c:window-app-id w))))))

(defun shape (node)
  "A readable s-expression of NODE's structure, for whole-tree assertions.

  (:h (:leaf \"a\") (:v (:leaf \"b\") (:leaf nil)))

Comparing shapes catches structural regressions that per-path assertions
miss — a split that should have collapsed and did not, for instance."
  (typecase node
    (c:leaf (list :leaf (let ((w (c:leaf-window node)))
                          (and w (c:window-app-id w)))))
    (c:split (list* (if (eq (c:split-axis node) :horizontal) :h :v)
                    (mapcar #'shape (c:children node))))
    (c:stack (list* :stack (c:stack-selected node)
                    (mapcar #'shape (c:children node))))
    (t (list :node (type-of node)))))
