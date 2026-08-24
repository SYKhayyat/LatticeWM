;;;; tests/test-scrolling-columns.lisp --- The strip, without a screen.
;;;;
;;;; The example's own test carried over, plus the half the module adds:
;;;; scroll state surviving undo's snapshot machinery and a serialize/
;;;; deserialize round trip.

(defpackage #:scrolling-columns/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:sc #:scrolling-columns))
  (:export #:run-all))

(in-package #:scrolling-columns/tests)

(def-suite scrolling-columns :description "The scrolling strip of columns.")
(in-suite scrolling-columns)

(t*:register-extension-suite "SCROLLING-COLUMNS/TESTS" "SCROLLING-COLUMNS")

(defun run-all ()
  "Run the SCROLLING-COLUMNS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'scrolling-columns)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))

(defmacro with-strip ((&key (windows 5) (visible 2)) &body body)
  "A fresh world whose workspace has been turned into a strip."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     (open-windows ,windows)
     (sc:scrolling ,visible)
     ,@body))

(defun leaf-rects (&optional (world r:*world*))
  "Visible leaf placements, as (PATH RECT VISIBLE-P) triples' rects."
  (let ((placements (p:layout-node p:*policy* (c:world-root world)
                                   (c:make-rect 0 0 1000 800))))
    (mapcar #'rest
            (remove-if-not (lambda (pl)
                             (and (typep (first pl) 'c:leaf) (fourth pl)))
                           placements))))

;;; ================================================================ tests

(test every-window-became-a-column
  (with-strip (:windows 5)
    (let ((strip (c:resolve-path (c:world-root r:*world*) '(0))))
      (is (typep strip 'sc:strip))
      (is (= 5 (c:container-count strip))
          "every window became a column, in order"))))

(test only-the-visible-window-is-drawn
  (with-strip (:windows 5 :visible 2)
    (is (= 2 (length (leaf-rects)))
        "two columns on screen at a time")))

(test moving-past-the-edge-scrolls-rather-than-refusing
  "The whole feature.  Moving right from the rightmost VISIBLE column scrolls
the strip by one; the motion itself succeeds instead of refusing at an edge
that is only the edge of the viewport, not of the strip."
  (with-strip (:windows 5 :visible 2)
    (let ((root (c:world-root r:*world*))
          (offset-before (sc:strip-offset
                          (c:resolve-path (c:world-root r:*world*) '(0)))))
      ;; Column 1 is the last visible one; moving right from it must scroll.
      (p:find-motion-target p:*policy* root '(0 1) :right)
      (is (< offset-before
             (sc:strip-offset (c:resolve-path root '(0))))
          "the strip scrolled to follow"))))

(test scrolling-records-an-undo-step
  "The container-protocol member the example was silent about: NODE-SIGNATURE
includes the scroll state, so a scroll records a snapshot and undo can bring
it back.  Without it, undo skips straight past the operation the user wanted
back -- which is the documented cost of skipping the method."
  (with-strip (:windows 5 :visible 2)
    (let* ((strip (c:resolve-path (c:world-root r:*world*) '(0)))
           (before (c:node-signature strip)))
      (setf (sc:strip-offset strip) 2)
      (is (not (equal before (c:node-signature strip)))
          "a scroll changes the signature")
      (setf (sc:strip-offset strip) 0)
      (is (equal before (c:node-signature strip))
          "and two equal strips compare equal"))))

(test copy-carries-the-scroll-state
  "COPY-NODE-SLOTS: undo restores where you were looking, not only what was
there."
  (with-strip (:windows 5 :visible 2)
    (let* ((strip (c:resolve-path (c:world-root r:*world*) '(0))))
      (setf (sc:strip-offset strip) 3)
      (let ((copy (c:copy-node strip)))
        (is (= 3 (sc:strip-offset copy))
            "the copy carries the changed offset")))))

(test the-strip-survives-a-serialize-round-trip
  "SERIALIZE-NODE and DESERIALIZE-NODE: a saved arrangement comes back with
its columns and its scroll position."
  (with-strip (:windows 5 :visible 2)
    (let* ((strip (c:resolve-path (c:world-root r:*world*) '(0)))
           (form (r:serialize-node strip)))
      (is (eq :scrolling-columns/strip (first form))
          "under the module's own tag")
      (let ((back (r:deserialize-node (first form) (rest form)
                                      (make-hash-table :test #'equal))))
        (is (= 5 (c:container-count back)))
        (is (= (sc:strip-offset strip) (sc:strip-offset back)))
        (is (= (sc:strip-visible strip) (sc:strip-visible back)))))))

(test an-empty-deserialized-strip-is-still-valid
  "SIMPLIFY-NODE at the end of deserialization: whatever came out of the
file, the result is a valid strip -- at least one column."
  (let ((back (r:deserialize-node :scrolling-columns/strip
                                  (list :visible 3)
                                  (make-hash-table :test #'equal))))
    (is (typep back 'sc:strip))
    (is (plusp (c:container-count back))
        "an empty file still yields a usable strip")))
