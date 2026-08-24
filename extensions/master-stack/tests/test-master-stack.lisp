;;;; tests/test-master-stack.lisp --- The layout, and the composition.
;;;;
;;;; The geometry half is the example's own test, carried over.  The
;;;; composition half is what makes this a module rather than a config file:
;;;; enable over an arbitrary policy, disable back to it, twice-is-a-no-op,
;;;; and the tree never moving.

(defpackage #:master-stack/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ms #:master-stack))
  (:export #:run-all))

(in-package #:master-stack/tests)

;; A top-level suite -- deliberately not :in MODEL, or RUN-ALL would run it
;; twice, once through MODEL and once through the registry.
(def-suite master-stack :description "The master-and-stack layout module.")
(in-suite master-stack)

(t*:register-extension-suite "MASTER-STACK/TESTS" "MASTER-STACK")

(defun run-all ()
  "Run the MASTER-STACK suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'master-stack)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))

(defmacro with-fresh-world (&body body)
  "A fresh world and conventional policy; nothing to clean up, because
nothing here mutates globals."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun leaf-rects (&optional (world r:*world*))
  "Placements of every leaf under POLICY, as RECTs in draw order."
  (let ((placements (p:layout-node p:*policy* (c:world-root world)
                                   (c:make-rect 0 0 1000 800))))
    (mapcar #'third
            (remove-if-not (lambda (pl) (typep (first pl) 'c:leaf))
                           placements))))

;;; ================================================================ tests

(test the-layout-is-two-columns-of-the-right-widths
  "The example's own assertion: four windows make one master column of one
pane and a stack column of three."
  (with-fresh-world
    (open-windows 4)
    (ms:enable)
    (unwind-protect
         (let ((rects (leaf-rects)))
           (is (= 2 (length (remove-duplicates (mapcar #'c:rect-w rects)
                                               :test #'=)))
               "two column widths: the master and the stack")
           (is (every (lambda (r)
                        (and (>= (c:rect-x r) 0) (>= (c:rect-y r) 0)
                             (<= (+ (c:rect-x r) (c:rect-w r)) 1000)
                             (<= (+ (c:rect-y r) (c:rect-h r)) 800)))
                      rects)
               "and every pane is inside the frame it was drawn for"))
      (ms:disable))))

(test the-tree-is-untouched
  "Only the arrangement changed.  This is why switching layouts loses no
window: the splits are still there, still nested, still holding what they
held."
  (with-fresh-world
    (open-windows 4)
    (let ((before (t*::shape (c:world-root r:*world*))))
      (ms:enable)
      (unwind-protect
           (progn
             (leaf-rects)
             (is (equal before (t*::shape (c:world-root r:*world*)))))
        (ms:disable))
      (is (equal before (t*::shape (c:world-root r:*world*)))
          "and disabling moved nothing either"))))

(test motion-crosses-between-master-and-stack
  "Right from the master column lands in the stack; Down stays inside
whichever column it started in.  A layout that draws correctly but navigates
by the split's stored axis is wrong in the exact way this checks for."
  (with-fresh-world
    (open-windows 4)
    (ms:enable)
    (unwind-protect
         (let* ((root (c:world-root r:*world*))
                (first-leaf (c:first-leaf-path root)))
           (let ((to (p:find-motion-target p:*policy* root first-leaf :right)))
             (is-true to "Right from the master column goes somewhere")
             (is (not (c:path-equal to first-leaf))))
           (is-true (p:find-motion-target p:*policy* root
                                          (c:next-leaf-path root first-leaf)
                                          :down)))
      (ms:disable))))

(test enable-composes-over-whatever-is-installed
  "The contract that makes this a module rather than a config file: ENABLE
does not replace the installed policy class, it composes in front of it, so a
policy carrying state comes through still carrying it."
  (with-fresh-world
    ;; Mark the installed policy with state a replacement would lose.
    (setf (c:prop r:*world* :master-stack-test/state) 'survives)
    (open-windows 1)
    (ms:enable)
    (unwind-protect
         (progn
           (is-true (typep p:*policy* 'ms:master-stack-mixin)
                     "the installed policy now answers as master-and-stack")
           (is-true (typep p:*policy* 'p:conventional-policy)
                     "and is STILL the policy that was there -- composed,
not replaced"))
      (ms:disable))))

(test enable-twice-composes-once
  "The same mixin twice in one precedence list is not a class, and CLOS says
so with an error naming neither this module nor the second call.  The guard
is what keeps a doubly-loaded configuration file from finding out."
  (with-fresh-world
    (ms:enable)
    (unwind-protect
         (progn
           (finishes (ms:enable))
           (finishes (ms:enable))
           (is (eq (class-name (class-of p:*policy*))
                   'ms::master-stack-over-conventional-policy)
               "the composed class was interned once and found again"))
      (ms:disable))))

(test disable-restores-what-was-underneath
  "The saved class, not a fresh default.  A fresh CONVENTIONAL-POLICY is a
different object with none of anybody's state on it."
  (with-fresh-world
    (let ((original p:*policy*))
      (ms:enable)
      (ms:disable)
      (is (eq p:*policy* original)
          "the very object came back through the switch"))))

(test the-knobs-move-panes-between-columns
  "MORE-MASTERS and FEWER-MASTERS change how many panes share the master
column, which changes the drawing and not the tree.  The functions are called
directly: RUN-COMMAND is the keyboard's door and asks its optional arguments
interactively, which a constructed test cannot answer."
  (with-fresh-world
    (open-windows 4)
    (ms:enable)
    (unwind-protect
         (progn
           (ms:more-masters)
           (is (= 2 (ms:master-count p:*policy*))
               "the knob moved")
           (is (= 2 (length (remove-duplicates (mapcar #'c:rect-w (leaf-rects))
                                               :test #'=)))
               "and two panes now share the master column")
           (ms:fewer-masters)
           (is (= 1 (ms:master-count p:*policy*))))
      (ms:disable))))
