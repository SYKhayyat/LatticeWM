;;;; tests/test-floating-only.lisp --- Where does a new window go?  Nowhere.

(defpackage #:floating-only/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:fo #:floating-only))
  (:export #:run-all))

(in-package #:floating-only/tests)

(def-suite floating-only :description "No layout, as a policy.")
(in-suite floating-only)

(t*:register-extension-suite "FLOATING-ONLY/TESTS" "FLOATING-ONLY")

(defun run-all ()
  "Run the FLOATING-ONLY suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'floating-only)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-policy (&body body)
  `(let ((fo::*enabled* nil)
         (fo::*previous-class* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ================================================================ tests

(test should-float-p-answers-yes-unconditionally
  "The whole policy is one method.  Every window floats."
  (with-policy
    (change-class p:*policy* 'fo:floating-only-policy)
    (dolist (app-id '("firefox" "foot" ""))
      (is-true (p:should-float-p p:*policy*
                                 (make-instance 'c:window :app-id app-id))
               "~a floats" app-id))))

(test placement-never-enters-the-tree
  "\"No layout\" expressed as a policy: open windows the ordinary way and
the tree never grows -- no panes, no splits, nothing tiled."
  (with-policy
    (change-class p:*policy* 'fo:floating-only-policy)
    (dotimes (i 3)
      (p:on-window-open p:*policy* r:*world*
                        (make-instance 'c:window :app-id (format nil "a~d" i))))
    ;; Every window floated, so every window is out of the tree.
    (is (= 0 (length (c:node-windows (c:world-root r:*world*)))))
    ;; The workspace list did not grow either: floating does not tile.
    (is (= 1 (c:container-count (c:world-workspaces r:*world*))))))

(test enable-and-disable-roundtrip-the-class
  "ENABLE changes the installed policy's class and remembers what it was;
DISABLE puts back exactly that class, not merely a conventional policy."
  (with-policy
    (let ((original (class-of p:*policy*)))
      (fo:enable)
      (is-true (typep p:*policy* 'fo:floating-only-policy))
      (is (fo:enabled-p))
      (fo:disable)
      (is (eq original (class-of p:*policy*))
          "the original class came back")
      (is-false (fo:enabled-p)))))

(test enable-is-idempotent-and-does-not-clobber-the-memory
  "Enabling twice must not overwrite *PREVIOUS-CLASS* with the
floating-only class itself -- or DISABLE would restore a floating policy."
  (with-policy
    (let ((original (class-of p:*policy*)))
      (fo:enable)
      (fo:enable)
      (fo:disable)
      (is (eq original (class-of p:*policy*))
          "double-enable still restores the truth"))))

(test disable-without-enable-is-a-no-op
  "Nothing to put back, nothing done."
  (with-policy
    (let ((before (class-of p:*policy*)))
      (fo:disable)
      (is (eq before (class-of p:*policy*))))))
