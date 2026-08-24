;;;; tools/test.lisp --- Run the whole suite and exit non-zero on failure.
;;;;
;;;; THE LATTICE IS IN THIS RUN, and it was not.  `make test' loaded
;;;; "latticewm/tests" alone, so the flagship tier-3 extension — 1500 lines,
;;;; the entire proof that the extension story works — had no test executed by
;;;; the command everybody runs.  The headline check count covered the core
;;;; and said so nowhere.
(require :asdf)

(defun extension-asds ()
  "Every extension .asd under extensions/, with its directory."
  (sort (loop for asd in (directory (merge-pathnames "extensions/*/*.asd"
                                                     *default-pathname-defaults*))
              collect (cons (pathname-name asd) (directory-namestring asd)))
        #'string< :key #'car))

;; The same registration tools/build.lisp does, repeated here because `make
;; test' does not run `make build' first on every path to this file.
(dolist (entry (extension-asds))
  (pushnew (cdr entry) asdf:*central-registry* :test #'equal))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm/tests")
  ;; The lattice suite depends on the core suite's fixtures, so it loads
  ;; second.  Absent lattice.asd this degrades to the core alone rather than
  ;; failing, because the core running without the lattice is gate 4's whole
  ;; point and a test harness that contradicted it would be lying.
  (if (asdf:find-system "lattice/tests" nil)
      (asdf:load-system "lattice/tests")
      (format t "~&note: lattice/tests not found; running the core suite only~%"))
  ;; And every promoted extension's suite, for the reason the lattice is here:
  ;; an extension whose tests nothing loads is an extension whose tests
  ;; nobody runs.  Each registers itself with RUN-ALL when it loads, so this
  ;; file needs no list of names to go stale.
  (dolist (entry (extension-asds))
    (let ((tests (concatenate 'string (car entry) "/tests")))
      (if (asdf:find-system tests nil)
          (asdf:load-system tests)
          (format t "~&note: ~a not found; its suite will not run~%" tests)))))

(multiple-value-bind (ok n) (funcall (read-from-string "latticewm/tests:run-all"))
  (format t "~&~%======== TESTS ========~%~d check~:p, ~:[FAIL~;PASS~]~%" n ok)
  (sb-ext:quit :unix-status (if ok 0 1)))
