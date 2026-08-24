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

(defun extension-systems ()
  "Every extension under extensions/, as (NAME DIRECTORY) pairs.

PROMOTION MADE THIS A GATE-1 CONCERN RATHER THAN A COURTESY.  EXTENSION-IDEAS.org
promotes the worked examples to modules \"held to gate 3 and gate 4\", and the
lattice's own history says what holding a module to those gates means in
practice: nothing anywhere *compiled* the lattice until it was put on this
list, and a rename in the core broke it while every gate passed.  An extension
that ships beside the core compiles beside the core.

Globbed rather than listed, for the same reason EXAMPLE-FILES below is: an
extension added tomorrow is covered on the day it lands.  One .asd per
extension directory, which defines both the extension and its /tests system --
the shape lattice.asd uses -- so this list needs no second entry for tests,
which gate 1 has never compiled and does not start compiling here."
  (sort (loop for asd in (directory (merge-pathnames "extensions/*/*.asd" *root*))
              collect (cons (pathname-name asd) (directory-namestring asd)))
        #'string< :key #'car))

(defun register-extension-systems ()
  "Put every extension directory on ASDF's central registry, once."
  (dolist (entry (extension-systems))
    (let ((dir (cdr entry)))
      (unless (member dir asdf:*central-registry* :test #'equal)
        (push dir asdf:*central-registry*)))))

(register-extension-systems)

(defparameter *systems*
  (or (rest (rest sb-ext:*posix-argv*))
      (append '("latticewm" "lattice")
              (mapcar #'car (extension-systems))))
  "Every system gate 1 compiles.

THE LATTICE WAS NOT ON THIS LIST AND SHOULD ALWAYS HAVE BEEN.  Gate 3 checks
that it touches no core, textually.  Gate 4 loads the core *without* it, on
purpose.  Nothing anywhere compiled it -- so a rename in the core broke
lattice/map.lisp, and seven gates and 779 checks passed over a shipped feature
that would not load.  It was found by a user's config file failing at startup,
which is the worst place to find it.

The extensions under extensions/ are appended by EXTENSION-SYSTEMS above, for
the same reason: promotion to a module that gate 3 holds to the boundary rules
is not worth anything if the boundary check is the only check it gets.")
(defparameter *loose-files*
  '("tools/hardware-check.lisp" "tools/bench.lisp")
  "Files a *user* is told to load, which belong to no system.

TOOLS/BENCH.LISP WAS THE THIRD INSTANCE OF THIS BUG AND IT WAS FOUND WHILE THE
COMMIT FIXING THE SECOND ONE WAS STILL THE TIP.  `make bench' is referenced by
the Makefile and by nothing else -- no gate, no CI job, no document -- and the
file was in no system, so nothing compiled it and a rename in the runtime would
have been discovered by whoever next wanted a number.  Its own header records
that it spent seven sessions measuring the wrong half of the program, which is
the kind of file that most needs somebody watching it.

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
undefined reference in the file is visible from the compile alone.

THE WORKED EXAMPLES ARE THE SAME CASE AND ARE ADDED BY EXAMPLE-FILES BELOW.")

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

(defun example-files ()
  "Every worked example, which belongs to no system either.

THE FOURTH INSTANCE OF THE HOLE *LOOSE-FILES* EXISTS FOR, and the one with the
largest audience: examples/ is what EXTENDING.org sends a stranger to read
first.  Gate 1 compiled latticewm and lattice and nothing else, so its own
banner — `zero compiler warnings' — was true of the systems and was silently
not a claim about the four files a new contributor opens before either of them.
The build printed `The variable COLUMNS is defined but never used' from
examples/05-status-line.lisp on every run, underneath a gate saying there were
none.  A gate with an unstated scope is a gate people stop reading, and the
distance between `zero warnings' and `zero warnings in the two directories we
happened to list' is not visible from the banner.

Gate 6 *loads* these, which is a stronger check in one direction — a rename in
the core that breaks one of them fails the build — and no check at all in this
one, because it muffles warnings to keep its own output readable.  Compiling
here and loading there is the pair, and neither is redundant.

Globbed rather than listed, so an example added tomorrow is covered on the day
it lands rather than on the day somebody remembers this list.  Sorted, because
the order files compile in is the order their warnings are reported in, and a
gate whose output reorders itself per machine is a gate nobody can diff."
  (sort (mapcar (lambda (path) (enough-namestring path *root*))
                (directory (merge-pathnames "examples/*.lisp" *root*)))
        #'string<))

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
            (dolist (path (append *loose-files* (example-files)))
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
