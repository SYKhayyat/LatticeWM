;;;; tools/run-gates.lisp --- Run the gates, one form at a time, contained.
;;;;
;;;; TOOLS/GATES.LISP IS TWENTY-ONE BARE TOP-LEVEL FORMS AND `--load' IS NOT A
;;;; TEST RUNNER.  A gate that signals -- a file that moved, a symbol that was
;;;; renamed, an XML the reader chokes on -- unwinds the whole load, and what
;;;; you get is a backtrace and no information at all about the gates that
;;;; never ran.  The one that broke is usually the one you already know about;
;;;; the ones behind it are the news.
;;;;
;;;; THE PROJECT HAD ALREADY WRITTEN THIS DOWN AND HAD NOT APPLIED IT HERE.
;;;; tools/integration.lisp:131 -- "Run one section, and do not let it take the
;;;; rest of the run with it" -- is that file's answer to having once been a
;;;; single 250-line LET that stopped growing at eighteen checks.  Gates 13 and
;;;; 16 wrap their own file reads on the same argument, spelled out: "a file
;;;; this cannot read is a gate that cannot run, not a gate that passes."  The
;;;; reasoning reached two gates and not the file holding twenty-one of them.
;;;;
;;;; WHY A DRIVER AND NOT A MACRO AROUND EACH GATE.  A gate is not one form:
;;;; gate 12 is four checks and a dozen definitions, gate 5 is a table and a
;;;; loop.  Wrapping each gate in a macro would put every DEFUN and
;;;; DEFPARAMETER in the file inside a LAMBDA, which is legal and is a lie
;;;; about what they are.  Reading the file and evaluating its forms is what
;;;; `load' already does; all this adds is a handler between them, and the gate
;;;; a failure belongs to is whichever one last printed a banner.
;;;;
;;;; A CONTAINED ERROR IS A FAILURE AND NOT A SKIP.  It goes through the same
;;;; FAIL the gates use, so the verdict names it, the exit status is 1, and
;;;; nothing about a broken gate looks like a passing one.

(require :asdf)

(defparameter *gates-file*
  (merge-pathnames "tools/gates.lisp"
                   (let ((root (sb-ext:posix-getenv "LATTICEWM_ROOT")))
                     (if (and root (plusp (length root)))
                         (pathname (concatenate 'string root "/"))
                         *default-pathname-defaults*))))

(let ((*package* (find-package :cl-user)))
  (with-open-file (in *gates-file* :external-format :utf-8)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          do (handler-case (eval form)
               (error (condition)
                 ;; FAIL and *CURRENT-GATE* are defined by the first few forms
                 ;; of the file being read.  If one of those is what signalled
                 ;; there is no reporting machinery yet, and saying so plainly
                 ;; beats calling a function that does not exist.
                 (let ((fail (find-symbol "FAIL" :cl-user))
                       (gate (find-symbol "*CURRENT-GATE*" :cl-user)))
                   (if (and fail (fboundp fail) gate (boundp gate))
                       (funcall fail (symbol-value gate)
                                "the gate itself signalled: ~a" condition)
                       (progn
                         (format t "~&the gate harness itself signalled ~
before it could report: ~a~%" condition)
                         (finish-output)
                         (sb-ext:quit :unix-status 1)))))))))

;; Only reached when tools/gates.lisp did not run its own verdict -- which it
;; does, and which quits.  Getting here means the file was truncated.
(format t "~&tools/gates.lisp ended without a verdict; ~
the file is not the whole file~%")
(sb-ext:quit :unix-status 1)
