;;;; tools/build.lisp --- Load the system with every warning made loud.
;;;;
;;;; Gate 1 of PLAN.org: zero compiler warnings.  The spike verified that this
;;;; catches wrong arity, wrong-typed constants, typo'd accessors and typo'd
;;;; calls at compile time — the largest slice of what a type system was going
;;;; to catch, for the price of a DECLAIM.  It will not catch a wrong-but-well-
;;;; typed argument.  Cheap enough that not doing it would be indefensible.
;;;;
;;;; Redefinition warnings are filtered, and only those.  Compiling a file and
;;;; then loading its fasl in the same image necessarily defines every macro
;;;; twice; that is an artifact of the build harness, not of the code, and
;;;; treating it as a failure would train everybody to ignore the gate.
;;;;
;;;;   sbcl --non-interactive --load tools/build.lisp [system]

(require :asdf)
(require :sb-introspect)

(defparameter *systems*
  (or (rest (rest sb-ext:*posix-argv*)) '("latticewm" "lattice"))
  "Every system gate 1 compiles.

THE LATTICE WAS NOT ON THIS LIST AND SHOULD ALWAYS HAVE BEEN.  Gate 3 checks
that it touches no core, textually.  Gate 4 loads the core *without* it, on
purpose.  Nothing anywhere compiled it -- so a rename in the core broke
lattice/map.lisp, and seven gates and 779 checks passed over a shipped feature
that would not load.  It was found by a user's config file failing at startup,
which is the worst place to find it.")
(defparameter *loose-files*
  '("tools/hardware-check.lisp")
  "Files a *user* is told to load, which belong to no system.

AND THEREFORE FILES NOTHING COMPILED, which is the same hole the lattice was in
and it cost the same coin.  tools/hardware-check.lisp called CURRENT-VIEWPORT
seventeen lines above the DEFUN that provides it, so every session it recorded
opened with `undefined function: LATTICEWM/USER::CURRENT-VIEWPORT' — printed by
a diagnostic tool, into the diagnosis.  Eighteen gates and seventeen hundred
checks had no opinion, because ASDF is what gate 1 asks and this file is in no
system.

COMPILED AND NOT LOADED, deliberately.  Loading it installs an ON-KEY method
and four hooks into the image doing the checking, and a gate that changes the
program it is inspecting is worse than the warning it is looking for.  Every
undefined reference in the file is visible from the compile alone.")

(defparameter *real* '())
(defparameter *redefinitions* 0)

(defparameter *root*
  (truename (or (uiop:getenv "LATTICEWM_ROOT") *default-pathname-defaults*))
  "The project.  Warnings from anywhere else are somebody else's build.")

(defun ours-p ()
  "True when the file being compiled right now is one of ours.

A dependency's warnings are not a gate this project can pass.  Four of them
arrive from PLUMP-DOM the moment the fasl cache is cold, and counting them
meant `make clean && make' could never be green — which is how the gate came
to be run almost exclusively against cached fasls, and how it missed real
warnings in files that did not happen to recompile.  Judging only our own
files is what makes a clean build the normal way to run it.

Note the .deps exclusion.  bootstrap.sh installs quicklisp *inside* the
project, so `under the project root' is not on its own the right test — it
would take every dependency back."
  (let ((file (or *compile-file-truename* *load-truename*)))
    (and file
         (let ((name (namestring file)))
           (and (eql 0 (search (namestring *root*) name))
                (not (search "/.deps/" name)))))))

(defun record (condition)
  (cond
    ((typep condition 'sb-kernel:redefinition-warning) (incf *redefinitions*))
    ((not (ours-p)) nil)
    (t (pushnew (princ-to-string condition) *real* :test #'string=))))

;;; UNDEFINED FUNCTIONS ARE NOT SIGNALLED, THEY ARE PRINTED.
;;;
;;; This gate's header claims it catches "typo'd calls at compile time".  It
;;; did not, and the hole was found the hard way: a function moved from
;;; LATTICEWM/RUNTIME to LATTICEWM/POLICY, one caller was left behind
;;; referring to a symbol in the package it had left, and the gate passed.
;;; Pressing a chord prefix would have errored.
;;;
;;; The reason is that SBCL collects undefined references per compilation unit
;;; and *prints a summary* when the unit exits, rather than signalling a
;;; condition a HANDLER-BIND can see.  ASDF wraps the whole build in one unit,
;;; so every one of them went to a stderr nobody was reading.
;;;
;;; So: capture what the unit prints, and read it.  The binding has to outlive
;;; the WITH-COMPILATION-UNIT, because the summary is emitted as it exits.
(defparameter *unit-output* (make-string-output-stream))

(let ((*error-output* (make-broadcast-stream *error-output* *unit-output*)))
  (with-compilation-unit (:override t)
    (handler-bind ((warning (lambda (c) (record c) (muffle-warning c))))
      (handler-case
          (progn
            (dolist (system *systems*) (asdf:load-system system))
            ;; After the systems, because these files use their packages.
            (dolist (path *loose-files*)
              (let ((file (merge-pathnames path *root*)))
                (if (probe-file file)
                    (uiop:with-temporary-file (:pathname fasl :type "fasl")
                      (compile-file file :output-file fasl
                                         :verbose nil :print nil))
                    (push (format nil "~a is listed in *LOOSE-FILES* and is not ~
                                       there" path)
                          *real*)))))
        (error (e)
          (format t "~&~%======== BUILD FAILED ========~%~a~%" e)
          (sb-ext:quit :unix-status 1))))))

(defparameter *undefined*
  (let ((text (get-output-stream-string *unit-output*)))
    (when (search "undefined" text :test #'char-equal) text)))

(format t "~&~%======== GATE 1: compiler warnings ========~%")
(cond
  (*real*
   (format t "~d warning~:p:~%" (length *real*))
   (dolist (w (reverse *real*)) (format t "  ~a~%" w))
   (format t "~%FAIL~%")
   (sb-ext:quit :unix-status 1))
  (*undefined*
   (format t "undefined references at the end of the compilation unit:~%~a~%"
           *undefined*)
   (format t "~%FAIL~%")
   (sb-ext:quit :unix-status 1))
  (t
   (format t "clean (~d benign redefinition~:p from compile-then-load, ~
              no undefined references)~%PASS~%"
           *redefinitions*)))
