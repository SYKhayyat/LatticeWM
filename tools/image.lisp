;;;; tools/image.lisp --- Dump ./latticewm.
;;;;
;;;; SAVE-LISP-AND-DIE does not cost live redefinition: the dumped core keeps
;;;; the compiler, so SWANK connects to the shipped binary and DEFMETHOD still
;;;; works at runtime.  This is StumpWM's shipping model.  ASDF is left in so
;;;; that user extensions can be ASDF:LOAD-SYSTEM-ed from ~/.config/latticewm/.
(require :asdf)
(require :sb-introspect)
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm")
  (asdf:load-system "swank"))
(sb-ext:save-lisp-and-die
 "latticewm"
 :executable t
 :compression nil
 :save-runtime-options t
 :toplevel (lambda ()
             (funcall (read-from-string "latticewm/runtime:main"))
             0))
