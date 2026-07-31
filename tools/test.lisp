;;;; tools/test.lisp --- Run the unit suite and exit non-zero on failure.
(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm/tests"))
(multiple-value-bind (ok n) (funcall (read-from-string "latticewm/tests:run-all"))
  (format t "~&~%======== TESTS ========~%~d check~:p, ~:[FAIL~;PASS~]~%" n ok)
  (sb-ext:quit :unix-status (if ok 0 1)))
