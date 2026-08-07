;;;; policy/options.lisp --- Tier-0 configuration values, and the registry.
;;;;
;;;; DESIGN.org's tier table calls tier 0 "edit a DEFPARAMETER, no restart, and
;;;; the only tier available to a non-programmer".  Every P1 fork in the design
;;;; has to appear here as a value rather than as a branch buried in a method,
;;;; and the registry is what makes `latticewm --list-options' able to print
;;;; every one of them without anybody maintaining a second list.
;;;;
;;;; IT IS THE FIRST FILE IN THE POLICY LAYER, and that is not alphabetical
;;;; accident.  Logging is configurable — where the log goes, how big it gets
;;;; before it rotates — and logging is also the very first thing that has to
;;;; exist, because GUARDED is the boundary every policy method is called
;;;; behind.  So the registry has to precede even the log, or log.lisp would
;;;; have to declare its values as bare DEFVARs that no user could discover.

(in-package #:latticewm/policy)

(defvar *options* (make-hash-table :test #'eq)
  "NAME -> (list VARIABLE DEFAULT DOCUMENTATION).  See DEFINE-OPTION.")

(defmacro define-option (name default &body (documentation))
  "Declare a tier-0 configuration value: a variable a user edits, nothing more.

    (define-option *gaps* 0
      \"Pixels of empty space left between adjacent panes.\")

This is a DEFPARAMETER plus a registration, so that the extension-surface
document can list every knob without anyone maintaining a second list of them."
  (check-type documentation string)
  (let ((key (intern (string-trim "*" (symbol-name name)) :keyword)))
    `(progn
       (defparameter ,name ,default ,documentation)
       (setf (gethash ,key *options*) (list ',name ,default ,documentation))
       ',name)))

(defun option (name)
  "The current value of tier-0 option NAME, a keyword."
  (let ((entry (gethash name *options*)))
    (when entry (symbol-value (first entry)))))

(defun (setf option) (value name)
  (let ((entry (gethash name *options*)))
    (unless entry (error "No such option: ~s" name))
    (setf (symbol-value (first entry)) value)))

(defun option-variable (name)
  "The symbol option NAME is stored in, or NIL."
  (first (gethash name *options*)))

(defun option-default (name)
  "The value option NAME shipped with."
  (second (gethash name *options*)))

(defun option-documentation (name)
  "The docstring of option NAME."
  (third (gethash name *options*)))

(defun option-boundp (name)
  "True when NAME names a registered option."
  (nth-value 1 (gethash name *options*)))

;;; ------------------------------------------------- who actually looks at it
;;;
;;; AN OPTION IS A PROMISE, AND NOTHING HERE COULD CHECK THE PROGRAM KEEPS IT.
;;; DEFINE-OPTION registers a name, a default and a docstring; the code that
;;; reads the value is somewhere else entirely, and until this function existed
;;; nothing connected the two.  So `latticewm --list-options', the generated
;;; extension surface and the config man page could all print a knob that was
;;; wired to nothing, in a build where every gate passed — and did, for
;;; *SMART-GAPS*, for the whole life of the option.
;;;
;;; This is the same species as the bug PLAN §log3 is proudest of finding:
;;; twenty-four options that could not be *set* from a configuration file.  The
;;; test written in response checks symbol identity between the registry and
;;; the user package, which is exactly the right shape — check the relationship
;;; between two independently maintained artifacts, because that is the thing
;;; no human ever verifies.  It just checked the wrong end.  Being settable and
;;; being read are two different promises and an option makes both.
;;;
;;; ASK THE COMPILER, NOT THE TEXT.  A grep for the name would count the
;;; DEFPARAMETER, the export, the docstring that mentions it and the man page,
;;; and would miss nothing only by accident.  SBCL already recorded who
;;; references what while it was compiling the system, which is a fact about
;;; the *program* rather than about its spelling: it sees a read inside a macro
;;; expansion and does not see the name in a comment.

(defun shorten-source-paths (form)
  "FORM with every filesystem path in it reduced to a bare file name.

SBCL names an anonymous function after the file it was compiled in, so a
DEFINE-KEY closure that reads *TERMINAL* comes back as

  (LAMBDA () :IN \"/home/shaul/src/latticewm/src/runtime/config.lisp\")

and doc/EXTENSION-SURFACE.txt -- generated, committed, and the document this
corpus calls the un-driftable reference -- carried the author's home directory
and build tree on four lines.  It is the same defect as OPTIONS.txt's
\"/home/shaul/.config/latticewm/init.lisp\" and it is worse, because it is not
merely embarrassing: two people regenerating the surface produce different
files, so `make surface && git diff --exit-code doc/' -- the check that turns
\"generated, so it cannot drift\" from a claim about the *generator* into a
claim about the *repository* -- could never have been green.

The bare name is enough.  A reader wants to know which file, and there is
exactly one config.lisp."
  (cond ((and (stringp form) (find #\/ form))
         (let ((slash (position #\/ form :from-end t)))
           (subseq form (1+ slash))))
        ((consp form) (mapcar #'shorten-source-paths form))
        (t form)))

(defun option-readers (name)
  "Every function and method in the loaded image that reads option NAME.

Function names, so a method comes back as SBCL writes it —
(SB-PCL::FAST-METHOD GAPS (LAYOUT-POLICY T)) — which OPTION-READER-NAME turns
into something to say out loud.

SORTED, AND THAT IS NOT TIDINESS.  SB-INTROSPECT:WHO-REFERENCES answers in
whatever order the cross-reference database holds, which is a function of
compilation order and is not stable across machines — so the same source
produced `read by: draw-echo-area-on, echo-reserved-edges' here and the two
names the other way round there.  A generated document that reorders itself
per machine cannot be diffed, and `make surface && git diff --exit-code doc/'
is the whole of what makes \"generated, so it cannot drift\" a fact about this
repository rather than about its generator.  Sorted on the *shortened* name,
because the unshortened one begins with an absolute path.

Empty for an option nothing looks at.  Gate 11 fails on that, and it is worth
being clear about why it is a gate and not a warning: an option that is read
nowhere is indistinguishable, from outside, from one that is read everywhere.
The user sets it, nothing happens, and the documentation agrees with them."
  (let ((variable (option-variable name)))
    (when variable
      (sort (remove-duplicates
             (mapcar #'first (sb-introspect:who-references variable))
             :test #'equal)
            #'string<
            :key (lambda (reader)
                   (princ-to-string (shorten-source-paths reader)))))))

(defun option-reader-name (reader)
  "READER as a string, with a method written the way you would specialize it.

The method case is the one that matters.  When the only thing reading an
option is the shipped method of a generic, then *that generic is the real
extension point* and the option is its default — so a policy that overrides
the method stops reading the option, and setting it does nothing.  That is the
whole answer to `is *GAPS* or GAPS in charge', and it is a fact about the
image rather than a sentence somebody has to remember to keep true."
  (if (and (consp reader)
           (symbolp (first reader))
           (search "METHOD" (symbol-name (first reader))))
      (format nil "~(~a ~a~)" (second reader) (or (third reader) '()))
      (format nil "~(~a~)" (shorten-source-paths reader))))

;;; ------------------------------------------- and who takes the answer away
;;;
;;; THE SECOND HALF OF THE SENTENCE, WHICH WAS STILL PROSE.  OPTION-READERS
;;; answers "who looks at this?" and the generated surface prints it, so the
;;; first half of the rule -- the generic is the extension point, the option is
;;; what its shipped method returns -- is a fact about the image.  The rest of
;;; that same sentence is "a policy that overrides the method stops reading the
;;; option", and *nothing could see an override*.
;;;
;;; It is not hypothetical.  Load the lattice and MAKE-WORKSPACE is answered by
;;; a method that never reaches the shipped one, so *NEW-WORKSPACE* -- a
;;; registered, documented, settable option that `--list-options' prints and
;;; gate 11 certifies as read -- decides nothing at all.  That is true, it is
;;; deliberate, and the only statement of it in the tree was one paragraph at
;;; the bottom of the option's own docstring, which is the artifact this
;;; project has already watched rot four times.
;;;
;;; So ask the image the whole question.  An override is a fact about the
;;; generic function's method list: a primary method whose specializers are at
;;; least as narrow as the reader's everywhere and narrower somewhere.  Two
;;; kinds, and the difference is the one a user cares about:
;;;
;;;   TOTAL     narrower only in the policy argument, so it applies wherever
;;;             the reader did.  Set the option under that policy and nothing
;;;             happens unless the method calls CALL-NEXT-METHOD.
;;;   NARROWED  narrower in some other argument as well -- GAPS for a STACK,
;;;             GAPS for a lattice GRID.  The option still reaches everywhere
;;;             that argument is not one of those.
;;;
;;; Whether the override composes cannot be asked of the image: SBCL keeps no
;;; CALL-NEXT-METHOD flag on a method and the compiled body has no constant to
;;; find.  It can be asked of the source, so gate 15 asks it there and requires
;;; that a total override which does not compose ship an option of its own.
;;; This function stops at what the image knows, and the document says exactly
;;; that much and no more.

(defun option-reader-method (reader)
  "The method object READER names, or NIL when READER is a plain function."
  (when (and (consp reader)
             (symbolp (first reader))
             (search "METHOD" (symbol-name (first reader)))
             (fboundp (second reader)))
    (let ((gf (fdefinition (second reader))))
      (when (typep gf 'generic-function)
        (find-if (lambda (method)
                   (let ((specializers (closer-mop:method-specializers method)))
                     (and (= (length specializers) (length (third reader)))
                          (every #'specializer-named-p specializers (third reader)))))
                 (closer-mop:generic-function-methods gf))))))

(defun specializer-named-p (specializer name)
  "True when NAME is how SBCL writes SPECIALIZER in a method's function name."
  (typecase specializer
    (closer-mop:eql-specializer
     (equal name (list 'eql (closer-mop:eql-specializer-object specializer))))
    (class (eq (class-name specializer) name))
    (t (equal specializer name))))

(defun specializer-narrower-p (narrow wide)
  "True when NARROW admits no argument WIDE refuses.  NIL for unrelated pairs."
  (or (eq narrow wide)
      (and (typep narrow 'closer-mop:eql-specializer)
           (typep wide 'class)
           (typep (closer-mop:eql-specializer-object narrow) wide))
      (and (typep narrow 'class) (typep wide 'class)
           (subtypep narrow wide))))

(defun overrides-p (candidate method)
  "True when CANDIDATE wins over METHOD wherever both are applicable.

Primary methods only.  A :BEFORE or :AROUND method is not an override -- it
runs beside the answer rather than instead of it, and an :AROUND that declines
to call the next method is a policy deciding to be a firewall, which is a
different act and one no user mistakes for a setting that did not take."
  (let ((narrow (closer-mop:method-specializers candidate))
        (wide (closer-mop:method-specializers method)))
    (and (null (method-qualifiers candidate))
         (= (length narrow) (length wide))
         (every #'specializer-narrower-p narrow wide)
         (notevery #'eq narrow wide))))

(defun total-override-p (candidate method)
  "True when CANDIDATE narrows METHOD in the policy argument and nowhere else."
  (let ((narrow (rest (closer-mop:method-specializers candidate)))
        (wide (rest (closer-mop:method-specializers method))))
    (every #'eq narrow wide)))

(defun option-shadows (name)
  "Every method that takes precedence over a method reading option NAME.

Rows of (GENERIC METHOD TOTALP), in the order the option's readers and each
generic's method list give them.  Empty for an option read by ordinary
functions, which nothing can override.

This is the answer to `I set it and my policy ignored it'.  It is also why the
generated surface prints two labels rather than one: a total override takes
the option away wherever that policy is in charge, and a narrowed one takes it
away only for the argument classes it names."
  (let* ((readers (option-readers name))
         (methods (remove nil (mapcar #'option-reader-method readers)))
         (out '()))
    (loop for reader in readers
          for method = (option-reader-method reader)
          when method
            do (dolist (candidate (closer-mop:generic-function-methods
                                   (fdefinition (second reader))))
                 ;; A method that reads this same option is not taking it away,
                 ;; however specific it is -- it is the shipped answer said
                 ;; again for a narrower case, and the user's setting still
                 ;; decides.  Only a method that reads something *else*, or
                 ;; nothing, can leave them with a knob that does not turn.
                 (when (and (not (member candidate methods))
                            (overrides-p candidate method))
                   (pushnew (list (second reader) candidate
                                  (total-override-p candidate method))
                            out :test #'equal))))
    (nreverse out)))

(defun option-shadow-name (shadow)
  "SHADOW as a string, written the way you would specialize it."
  (destructuring-bind (generic method totalp) shadow
    (declare (ignore totalp))
    (format nil "~(~a (~{~a~^ ~})~)" generic
            (mapcar #'c:specializer-name (closer-mop:method-specializers method)))))

(defun all-options ()
  "Every registered tier-0 option, as (KEYWORD VARIABLE VALUE DEFAULT DOC),
sorted by name."
  (let ((out '()))
    (maphash (lambda (key entry)
               (destructuring-bind (variable default documentation) entry
                 (push (list key variable (symbol-value variable)
                             default documentation)
                       out)))
             *options*)
    (sort out #'string< :key (lambda (row) (symbol-name (first row))))))

;;; ------------------------------------------ and the same fact, read backwards
;;;
;;; THE RELATIONSHIP WAS DATA IN ONE DIRECTION ONLY.  OPTION-READERS says which
;;; methods look at an option and OPTION-SHADOWS says which methods take that
;;; answer away, and the generated surface prints both under every option.  So
;;; a reader who arrives at the question from the option list is told the whole
;;; rule.  Nothing told the reader who arrives from the other end, and the
;;; other end is the *front* of the same document: the generics section is what
;;; gate 2 guards, what every docstring in this package points at, and what
;;; `write a method' means.  Twenty-five of the sixty-nine generics have a
;;; shipped answer that is a value a user can set without writing one, and the
;;; surface said so under none of them.
;;;
;;; FIVE OF THEM SHARE THE OPTION'S NAME, which is where it stops being a
;;; missing cross-reference and starts being the thing lamdan/ complained
;;; about: *GAPS* and GAPS, *BORDER-WIDTH* and BORDER-WIDTH, *KEYS-HINT* and
;;; KEYS-HINT, *MOVE-INTO-OCCUPIED* and MOVE-INTO-OCCUPIED, *NEW-CHILD-SIDE*
;;; and NEW-CHILD-SIDE.  Its reading was that two mechanisms answer one
;;; question and you get whichever you found first, and its remedy was to
;;; delete the options.  The remedy is wrong now and the observation is not.
;;; This project has since ruled the other way -- an option is not a second
;;; answer, it is what a generic's shipped method returns, and gate 15 requires
;;; a policy that overrides the method wholesale to offer an option of its own
;;; -- so those five pairs are the rule's own worked examples rather than a
;;; hedge against it.  What was true is that a reader could not see that from
;;; the generic, and a shared name is a claim about the program that nothing
;;; checked.  Gate 17 checks it; this is what it asks.

(defun options-by-generic ()
  "Every policy generic whose methods read options, mapped to which.

GENERIC -> a sorted list of variables.  Methods only, deliberately: the rule
this answers is about a *shipped method's* answer, and an option read by an
ordinary function is not a generic's default -- nothing can override it and
there is no `write a method instead' to point the reader at.

Built in one pass because OPTION-READERS costs a cross-reference walk per
option, and asking it once per generic instead would be sixty-nine times over
the same ground for the same table."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (row (all-options))
      (dolist (reader (option-readers (first row)))
        (when (option-reader-method reader)
          (pushnew (second row) (gethash (second reader) table)))))
    ;; SETF GETHASH on a key already present is defined during MAPHASH; adding
    ;; or removing one is not, and this does neither.
    (maphash (lambda (generic options)
               (setf (gethash generic table)
                     (sort options #'string< :key #'symbol-name)))
             table)
    table))

(defun option-print-name (variable)
  "VARIABLE spelled the way a configuration file would have to spell it.

NOT PRINC, WHICH IS WHAT THE OPTION LIST USES AND GETS AWAY WITH.  Every option
in this package is inherited by LATTICEWM/USER, so PRINC's unqualified name is
right for all of them by luck rather than by construction.  An *extension's*
option is not this package's: load the lattice and GAPS is answered from
LATTICE:*CELL-GAP* as well as *GAPS*, and this is a document whose whole claim
is that it describes the image you are running, so it has to survive one with
an extension in it.

SO ASK THE QUESTION AS THE READER HAS IT, which is not `where does this symbol
live' but `what would I have to type'.  Binding *PACKAGE* to LATTICEWM/USER and
printing readably answers exactly that: a prefix appears when one is needed and
does not when it is not, and which case an extension falls into is the
extension's own decision rather than a guess made here.  The lattice
USE-PACKAGEs itself into LATTICEWM/USER on load so that (zoom-out) works at a
REPL, so *CELL-GAP* prints bare and is correct bare; an extension that does not
install its vocabulary prints qualified and is correct qualified.  The test is
the round trip, not the spelling -- see
THE-RELATIONSHIP-READS-THE-SAME-WAY-ROUND-FROM-THE-GENERIC."
  (let ((*package* (or (find-package '#:latticewm/user) *package*)))
    (string-downcase (prin1-to-string variable))))
