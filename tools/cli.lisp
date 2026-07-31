;;;; tools/cli.lisp --- Run the command line without dumping an image.
(require :asdf)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "latticewm"))
(setf sb-ext:*posix-argv* (cons "latticewm" (rest (rest sb-ext:*posix-argv*))))
(funcall (read-from-string "latticewm/runtime:main"))
