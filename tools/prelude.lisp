;;;; tools/prelude.lisp --- Find the dependencies, however they got here.
;;;;
;;;; Loaded before every other tool, and it exists because there are three
;;;; legitimate ways for a Lisp library to be on a Linux machine and the build
;;;; must not care which one was used:
;;;;
;;;;   1. Already visible to ASDF.  A nix shell, a distro package, an
;;;;      ~/.sbclrc that loads a personal quicklisp — nothing to do.
;;;;   2. Under ./.deps/quicklisp, put there by ./bootstrap.sh.
;;;;   3. Nowhere.  Then say so, in one sentence, naming the fix.
;;;;
;;;; The third case is the whole point.  A build that fails with
;;;; "Component :ALEXANDRIA not found" has told the truth and helped nobody;
;;;; every project this size that people actually install has a line that says
;;;; run this script instead.

(require :asdf)

(defparameter *root*
  (or (uiop:getenv-pathname "LATTICEWM_ROOT" :ensure-directory t)
      (uiop:getcwd))
  "The project directory.  The Makefile runs from it; a REPL might not.")

(defparameter *systems*
  '("wayflan-client" "alexandria" "closer-mop" "bordeaux-threads")
  "Everything latticewm.asd depends on that is not in SBCL itself.

fiveam and swank are deliberately absent: the test harness and the image
dumper ask for those themselves, and a plain `make build' should not need
either.")

;; The project itself, plus anything unpacked under .deps/, findable by ASDF.
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,*root*)
   ,@(let ((deps (merge-pathnames ".deps/" *root*)))
       (when (probe-file deps) `((:tree ,deps))))
   :inherit-configuration))

(defun quicklisp-setup ()
  "The setup file of a quicklisp under .deps/, if bootstrap.sh made one."
  (probe-file (merge-pathnames ".deps/quicklisp/setup.lisp" *root*)))

(defun missing-systems ()
  "Which of *SYSTEMS* ASDF cannot currently find."
  (remove-if (lambda (name)
               (handler-case (asdf:find-system name nil)
                 (error () nil)))
             *systems*))

(let ((missing (missing-systems)))
  (when missing
    ;; Only load quicklisp if something is actually absent.  Loading it
    ;; otherwise would put its dist ahead of a distro's own packages for no
    ;; reason, and quietly changing which copy of a library a build used is a
    ;; bad habit for a program that has to be reproducible.
    (let ((setup (quicklisp-setup)))
      (cond
        (setup
         (handler-bind ((warning #'muffle-warning))
           (load setup)
           (funcall (read-from-string "ql:quickload") missing :silent t)))
        (t
         (format *error-output* "~&~%~
LatticeWM cannot find: ~{~a~^, ~}~%~%~
Run ./bootstrap.sh once -- it fetches them into ./.deps/ and touches~%~
nothing else on the machine.  On NixOS, use nix-shell instead.~%~%"
                 missing)
         (finish-output *error-output*)
         (sb-ext:quit :unix-status 1))))))
