;;;; tools/test.lisp --- Run the whole suite and exit non-zero on failure.
;;;;
;;;; THE LATTICE IS IN THIS RUN, and it was not.  `make test' loaded
;;;; "latticewm/tests" alone, so the flagship tier-3 extension — 1500 lines,
;;;; the entire proof that the extension story works — had no test executed by
;;;; the command everybody runs.  The headline check count covered the core
;;;; and said so nowhere.
(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm/tests")
  ;; The lattice suite depends on the core suite's fixtures, so it loads
  ;; second.  Absent lattice.asd this degrades to the core alone rather than
  ;; failing, because the core running without the lattice is gate 4's whole
  ;; point and a test harness that contradicted it would be lying.
  (if (asdf:find-system "lattice/tests" nil)
      (asdf:load-system "lattice/tests")
      (format t "~&note: lattice/tests not found; running the core suite only~%")))

(multiple-value-bind (ok n) (funcall (read-from-string "latticewm/tests:run-all"))
  (format t "~&~%======== TESTS ========~%~d check~:p, ~:[FAIL~;PASS~]~%" n ok)
  (sb-ext:quit :unix-status (if ok 0 1)))
