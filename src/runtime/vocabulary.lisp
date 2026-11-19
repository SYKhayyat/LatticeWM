;;;; vocabulary.lisp --- make an extension's names typable from init.lisp
;;;
;;;; THE PATTERN THIS FILE EXISTS TO STOP BEING COPIED.  An extension's
;;;; commands are interned in the extension's own package (DEFCOMMAND interns
;;;; where the file was compiled), while configuration files and the REPL are
;;;; read in LATTICEWM/USER -- so a loaded extension whose commands you cannot
;;;; type reads as a broken extension while working perfectly.  The lattice
;;;; solved that with USE-PACKAGE at load time and a careful answer to symbol
;;;; conflicts; buffers and transient-rules then solved it again, nearly
;;;; verbatim, and the copies had already drifted: only the lattice's version
;;;; logged which names had been kept, and only it returned the package it
;;;; had installed into.
;;;
;;;; One implementation on the surface, called by every extension at load
;;;; time.  A second extension should get the ruling for free, not a
;;;; paragraph to re-read.

(in-package #:latticewm/runtime)

(defun package-named (designator)
  "The package DESIGNATOR names, or NIL if there is none.

A package is itself; a string or a symbol is looked up by name -- as a string,
because SBCL's FIND-PACKAGE signals PACKAGE-DOES-NOT-EXIST for an *uninterned*
symbol, and `#:buffers' is exactly what an extension naturally writes in a
call.  A lookup documented to answer NIL for \"no such package\" should not be
the thing that takes the session down at load time."
  (etypecase designator
    (package designator)
    ((or string symbol) (find-package (string designator)))))

(defun install-vocabulary (source &key (package '#:latticewm/user) label)
  "Make every external name of package SOURCE visible in PACKAGE.

PACKAGE defaults to LATTICEWM/USER, the package configuration files and the
REPL are read in.  Returns the target package, or NIL if either package is
missing -- an extension that installs its vocabulary before its own package
exists is a load-order bug, and it gets a log line rather than a condition.

USE-PACKAGE AT LOAD TIME, rather than a hand-maintained re-export list, so a
command added tomorrow is reachable tomorrow.  Conflicts are resolved in
favour of the name that was already there, because a configuration file that
has already defined its own ZOOM-OUT should keep it: it is theirs, and
refusing to load over it would be the wrong way round.

THE CONFLICTS ARE RESOLVED BEFORE THE USE-PACKAGE RATHER THAN DURING IT.  This
used to be an IGNORE-ERRORS around USE-PACKAGE under a HANDLER-BIND hunting
for CL:CONTINUE or SB-IMPL::TAKE-NEW, and when neither restart was found --
which is not hypothetical, it is what a symbol conflict between two *used*
packages gives you -- the error escaped to the IGNORE-ERRORS, the whole
USE-PACKAGE was abandoned partway, and how many names had been imported first
was unspecified.  The extension was then half installed behind one :WARN line,
which is precisely the failure the paragraph above exists to prevent.  Every
name that is already spoken for is SHADOWING-IMPORTed first -- which pins the
user's meaning whether theirs is present or inherited -- so the USE-PACKAGE
that follows has nothing left to signal about, and a collision costs one name
and one log line.

LABEL names the source in that log line, and defaults to the source package's
own name, so an extension does not have to pass anything to be legible.  It is
computed after the lookup rather than as a keyword default, because the default
would have been PACKAGE-NAME of a designator that may name nothing at all --
which signals exactly when the log line it feeds is most needed."
  (let* ((target (package-named package))
         (from (package-named source))
         (label (or label (and from (package-name from)) (string source))))
    (unless (and target from)
      (logmsg :warn "~a: no package ~a; vocabulary not installed"
              label (if target (string source) (string package)))
      (return-from install-vocabulary nil))
    (let ((taken '()))
      (do-external-symbols (symbol from)
        (multiple-value-bind (theirs status)
            (find-symbol (symbol-name symbol) target)
          (when (and status (not (eq theirs symbol)))
            (push theirs taken))))
      (when taken
        ;; Theirs, kept: SHADOWING-IMPORT of a symbol already present is how
        ;; you say "this name is decided" without changing what it means.
        (shadowing-import taken target)
        (logmsg :warn "~a: ~d name~:p in ~a already meant something else ~
and still do: ~{~a~^ ~}"
                label (length taken) (package-name target)
                (sort (mapcar #'symbol-name taken) #'string<)))
      (use-package from target))
    target))
