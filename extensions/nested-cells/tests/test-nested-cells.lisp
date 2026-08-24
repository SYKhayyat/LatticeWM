;;;; tests/test-nested-cells.lisp --- A pane that owns a process.

(defpackage #:nested-cells/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:nc #:nested-cells))
  (:export #:run-all))

(in-package #:nested-cells/tests)

(def-suite nested-cells :description "Supervised child compositors.")
(in-suite nested-cells)

(t*:register-extension-suite "NESTED-CELLS/TESTS" "NESTED-CELLS")

(defun run-all ()
  "Run the NESTED-CELLS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'nested-cells)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-cells (&body body)
  `(let ((nc::*cells* '())
         (nc::*enabled* nil))
     ,@body))

(defun wait-unless (predicate)
  "Poll PREDICATE for up to ~5s; SPAWN detaches, so children need a moment."
  (loop repeat 50
        unless (funcall predicate) do (sleep 0.1)
        finally (return (funcall predicate))))

;;; ================================================================ tests

(test open-and-close-a-cell
  "The whole lifecycle: open starts a live child, close stops it and
forgets the cell."
  (with-cells
    (let ((marker (merge-pathnames "cell-was-here" (uiop:temporary-directory))))
      (ignore-errors (delete-file marker))
      ;; A child whose only act is to leave a trace, so aliveness is
      ;; observable without pretending the test has a Wayland client.
      (nc:open-cell "probe"
                    (list "sh" "-c"
                          (format nil "touch ~a; sleep 30" marker)))
      (unwind-protect
           (progn
             (is-true (wait-unless
                       (lambda () (probe-file marker)))
                      "the child ran")
             (is-true (nc:cell-alive-p "probe") "still alive after 30ms")
             (is (equal "probe" (nc:close-cell "probe"))))
        ;; If the assertion above failed, do not leak a sleeper.
        (ignore-errors (nc:close-cell "probe")))
      (is-false (nc:cell-alive-p "probe"))
      (is (= 0 (length nc::*cells*))))))

(test close-of-unknown-name-declines
  "Closing a cell nobody opened says nothing happened."
  (with-cells
    (is-false (nc:close-cell "never-opened"))))

(test reopen-stops-the-old-child
  "One name, one process: opening an existing name stops the previous
child rather than orphaning it."
  (with-cells
    (nc:open-cell "probe" '("sleep" "30"))
    (let ((old-pid (getf (cdr (assoc "probe" nc::*cells* :test #'equal))
                         :pid)))
      (nc:open-cell "probe" '("sleep" "30"))
      (is (= 1 (length nc::*cells*)) "one cell after re-open")
      (is (/= old-pid (getf (cdr (assoc "probe" nc::*cells*
                                        :test #'equal))
                            :pid))
          "and it is a different process")
      ;; Cleanup.
      (nc:close-cell "probe"))))

(test check-reports-and-forgets-dead-children
  "CHECK-CELLS forgets a cell whose child exited on its own -- there is
nothing left to supervise -- while live cells survive the sweep."
  (with-cells
    (nc:open-cell "dead" '("true"))
    (nc:open-cell "alive" '("sleep" "30"))
    ;; Give the true-child a beat to exit.
    (wait-unless (lambda () (not (nc:cell-alive-p "dead"))))
    ;; Sweep twice: once to notice the death, once to prove it stuck.
    (nc:check-cells)
    (nc:check-cells)
    (is-false (assoc "dead" nc::*cells* :test #'equal) "dead cell forgotten")
    (is-true (assoc "alive" nc::*cells* :test #'equal) "live cell kept")
    (nc:close-cell "alive")))

(test all-cell-names-sorts
  "Cell names come out sorted for completion candidates."
  (with-cells
    (nc:open-cell "zeta" '("true"))
    (nc:open-cell "alpha" '("true"))
    (is (equal '("alpha" "zeta") (nc:all-cell-names)))))
