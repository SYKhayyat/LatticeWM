;;;; policy/surface.lisp --- The extension surface, described from the image.
;;;;
;;;; PLAN.org, gate 2, and it explains why this is generated rather than
;;;; written:
;;;;
;;;;   "The extension-surface document generated *from the live image*, not
;;;;   from the source: each policy generic, its lambda list, its docstring,
;;;;   and every method with its specializers. […] Pull this forward to Week 1
;;;;   and make it a CI gate.  It is the cheapest available forcing function
;;;;   against this document's own worst fear — an extension surface that rots
;;;;   because documenting it is a separate chore from writing it.  Generated
;;;;   from the image, it is not a separate chore, and it cannot silently drift
;;;;   out of date."
;;;;
;;;; It also does something a written document cannot: it lists the methods
;;;; *you* have added, so a user with a configuration file can ask the running
;;;; window manager what they have changed.

;;;; SPECIALIZER-NAME, METHOD-DESCRIPTION and GENERIC-DESCRIPTION used to live
;;;; here.  They describe a CLOS generic and know nothing about policy, and the
;;;; container protocol needs the identical description in the identical format
;;;; — so they are in model/surface.lisp now and both surfaces call them.  Two
;;;; printers for one format are two chances to disagree about it, and a
;;;; generated document whose whole value is that it cannot drift is a poor
;;;; place to keep a second copy of the formatter.

(in-package #:latticewm/policy)

(defun extension-surface ()
  "The whole extension surface, as data.

This is what the SWANK bridge answers 'what can I change?' with, and it is
deliberately machine-readable first and pretty second — PLAN.org's framing is
that the realistic post-expiry maintainer is a cheap model plus whatever
contributors the release attracts, and a model would rather have a plist."
  (let ((by-generic (options-by-generic)))
    (list :generics (mapcar (lambda (name)
                              ;; :OPTIONS is this surface's addition to the
                              ;; shared description.  The container protocol has
                              ;; no options and its entries carry none, which is
                              ;; why the printer treats the key as optional
                              ;; rather than the two surfaces keeping two
                              ;; printers -- see model/surface.lisp.
                              (append (c:generic-description name)
                                      (list :options
                                            (mapcar #'option-print-name
                                                    (gethash name by-generic)))))
                            (policy-generics))
        :options (mapcar (lambda (row)
                           (let ((shadows (option-shadows (first row))))
                             (append row
                                     (list (option-readers (first row))
                                           (mapcar #'option-shadow-name
                                                   (remove-if-not #'third shadows))
                                           (mapcar #'option-shadow-name
                                                   (remove-if #'third shadows))))))
                         (all-options)))))

(defparameter *protocol-order*
  '((layout-policy     . "where things go")
    (appearance-policy . "what they look like")
    (motion-policy     . "how you move between them")
    (structure-policy  . "what changes the shape of the tree")
    (lifecycle-policy  . "what happens when a window arrives or leaves")
    (input-policy      . "keys, the pointer, and the devices"))
  "The six protocols POLICY implements, in reading order, with a gloss each.

READING ORDER AND NOT ALPHABETICAL.  This governs how the extension surface is
printed, and the sequence is the one a person meets the system in: where a
window goes, then what it looks like, then how you get to it.  A flat
alphabetical list of sixty-nine generics is a reference and not a document, and
the whole argument of this file is that a stranger reads the surface rather
than the source.")

(defun generic-protocol (name)
  "Which of the six protocol classes answers NAME, or NIL.

Asked of the method list rather than of a table, so a generic whose shipped
answer moves between protocols is described where it now lives and nobody has
to remember to move a row.  NIL means the shipped answer is on POLICY itself,
which is a real and deliberate place for one -- see the group heading."
  (let ((gf (and (fboundp name) (fdefinition name))))
    (when (typep gf 'generic-function)
      (loop for method in (closer-mop:generic-function-methods gf)
            for specializer = (first (closer-mop:method-specializers method))
            when (and (typep specializer 'class)
                      (assoc (class-name specializer) *protocol-order*))
              return (class-name specializer)))))

(defun group-by-protocol (entries)
  "ENTRIES partitioned into (PROTOCOL GLOSS . ENTRIES), in *PROTOCOL-ORDER*.

Deterministic in both directions: the groups come out in the order of the
parameter and the entries keep the order they arrived in, because this output
is a committed file and gate 12 holds the committed file to what the image
prints.  A generic answered on POLICY itself, or by nothing yet, lands in the
last group rather than being dropped -- a surface that silently omits a member
is the failure every check in this project is written against."
  (let ((groups (mapcar (lambda (row) (list (car row) (cdr row))) *protocol-order*))
        (rest (list nil "answered on POLICY itself, or not yet answered at all")))
    (dolist (entry entries)
      (let* ((protocol (generic-protocol (getf entry :name)))
             (group (or (assoc protocol groups) rest)))
        (setf (cddr group) (append (cddr group) (list entry)))))
    (remove-if (lambda (group) (null (cddr group)))
               (append groups (list rest)))))

(defun print-hook-surface (&optional (stream *standard-output*))
  "Print the hooks for a human.

THE THIRD SURFACE, AND THE ONE THAT HAD NO DOCUMENT.  The policy generics are
printed from the image and gated; the container protocol is printed from the
image and gated; the options are printed from the image, gated, and carry the
list of what reads them.  The hooks were a sentence in doc/EXTENDING.org
naming thirteen of them, hand-typed, four short — :LAYOUT-RESTORED, which is
the hook the flagship extension itself uses, among the missing — followed by
\"(all-hooks) is authoritative\", which is a true statement that only helps
somebody already in a REPL.

Two comments in the source said this document existed.  DEFHOOK's docstring
warned that an undeclared hook \"does not appear in the extension surface
document\", and gate 7's header said :FOCUS-CHANGED was \"listed in the
generated extension surface\".  Neither was true of any hook; nothing printed
them anywhere."
  (let ((hooks (all-hooks)))
    (format stream "~&~76,,,'=<~>~%~
                    LatticeWM hooks~%~
                    ~d hook~:p.  Generated from the running image.~%~
                    ~76,,,'=<~>~2%"
            (length hooks))
    (format stream "~
A HOOK NOTICES THAT SOMETHING HAPPENED.  Every function attached to it runs,~%~
the return value is normally ignored, and nobody is in charge.  A *generic*~%~
decides what happens instead: one answer wins, CALL-NEXT-METHOD composes them,~%~
and the return value is the point.  If you find yourself wanting a hook's~%~
answer, you wanted a method -- `latticewm --extension-surface' lists those.~2%~
    (defun note-the-window (window) (logmsg :info \"~~a\" window))~%~
    (add-hook :window-opened 'note-the-window)~2%~
*Pass a symbol, not #'a-function.*  The list holds what it is given and a~%~
function object is a snapshot, so redefining the function afterwards leaves~%~
the hook calling the old one and re-evaluating the ADD-HOOK adds a second~%~
entry.  A symbol is looked up when the hook runs and replaces rather than~%~
accumulates.  Both of those bit this project.~2%~
THE ARGUMENT LIST BESIDE EACH NAME IS CHECKED, in both directions.  ADD-HOOK~%~
complains when the function you attach cannot be called with them, and a~%~
compiler macro fails the build on a RUN-HOOKS that passes the wrong number --~%~
because a hook function of the wrong arity signals inside the guard that keeps~%~
one broken hook from stopping the others, so what reaches you is silence.~2%~
EVERY HOOK BELOW IS ATTACHED TO AND WATCHED FIRING by the test suite or the~%~
integration run; gate 14 fails the build on one that is not, because a seam~%~
nobody has ever pulled is a seam whose arguments and timing are guesses.~2%~
That is a claim about the *suites*, and it used to sit above a body in which~%~
sixteen of eighteen entries printed `attached: 0' -- because `attached' counts~%~
what is on each hook *in this image*, and the image a build tool generates a~%~
document from loads no tests.  Both statements were true and a reader had no~%~
way to know it.  An `attached:' line now appears only when the number is not~%~
zero, so in a committed copy of this file there are none, and at a REPL there~%~
is one for every hook your configuration file touched -- which is what the~%~
number was ever good for.~2%")
    (dolist (row hooks)
      (destructuring-bind (name documentation attached arguments) row
        ;; The argument names alone, not ~S of the list: these are symbols
        ;; interned in LATTICEWM/POLICY, and a reader who has to skip
        ;; `latticewm/policy::' in front of every one of them learns nothing
        ;; from it.  The two protocol surfaces print specializers, where the
        ;; package is the fact; here it is noise.
        (format stream "~&~76,,,'-<~>~%~s (~{~(~a~)~^ ~})~%~76,,,'-<~>~%~
                        ~a~%~%"
                name arguments documentation)
        ;; Only when it is not zero.  See the preamble: this is a per-image
        ;; measurement, and the one image where it is guaranteed meaningless
        ;; is the one whose output gets committed.
        (unless (zerop attached)
          (format stream "  attached: ~d~%" attached))
        (format stream "~%")))
    (values)))

(defun undocumented-generics ()
  "Every extension-surface generic with no docstring.  Gate 2 fails on any."
  (remove-if (lambda (symbol) (documentation symbol 'function))
             (policy-generics)))

(defun print-extension-surface (&optional (stream *standard-output*))
  "Print the extension surface for a human.

Prints UNDOCUMENTED <-- flag me for anything missing a docstring, which is the
build gate's failure output as well as a nudge to whoever is reading."
  (let ((surface (extension-surface)))
    (format stream "~&~76,,,'=<~>~%~
                    LatticeWM extension surface~%~
                    ~d generic~:p, ~d option~:p.  ~
                    Generated from the running image.~%~
                    ~76,,,'=<~>~2%"
            (length (getf surface :generics)) (length (getf surface :options)))
    (format stream "~
Every generic below dispatches on a policy as its first argument.  To change~%~
one, write a method; it takes effect the moment you evaluate it.~2%~
    (defmethod gaps ((policy conventional-policy) container) 8)~2%~
Specialize on CONVENTIONAL-POLICY, not on the class the shipped method uses.~%~
The defaults sit on the six protocols POLICY implements -- LAYOUT-POLICY,~%~
APPEARANCE-POLICY, MOTION-POLICY, STRUCTURE-POLICY, LIFECYCLE-POLICY and~%~
INPUT-POLICY -- so that a mixin answering for one of them is a real thing you~%~
can write.  Yours has to be strictly more specific than theirs, or it~%~
replaces the shipped answer instead of extending it and CALL-NEXT-METHOD~%~
signals NO-NEXT-METHOD.~2%~
The `methods' list under each generic includes yours, which is how you check~%~
that your configuration file was actually loaded.~2%~
`answered from' NAMES THE OPTIONS THE SHIPPED METHOD READS, and it is there to~%~
save you writing a method you did not need.  When a generic has that line, the~%~
decision is already tier-0: set the variable in your config file and the~%~
shipped answer changes, with no DEFMETHOD anywhere.  Write the method when you~%~
want a *different decision* -- one that depends on the container, the window or~%~
the time of day -- and be aware that your method then stops reading the option,~%~
because it is the method that was reading it.  Both halves of that sentence are~%~
printed: the option's own entry below lists what reads it and what overrides~%~
it, and this line is the same fact from the other end.~2%~
Where a generic and an option share a name -- GAPS and *GAPS* -- they are that~%~
one relationship and not two competing ones, and gate 17 fails the build if~%~
they ever stop being: an option named after a generic has to be read by a~%~
method on it.~2%~
This is one of two surfaces.  The other is the *container* protocol -- what a~%~
new container kind answers rather than what a new policy answers -- and it is~%~
printed by `latticewm --container-surface'.~2%")
    ;; GROUPED BY PROTOCOL, and it used to be one flat run of sixty-nine.
    ;; Every paragraph above tells the reader to specialize on
    ;; CONVENTIONAL-POLICY *because* the defaults sit on six protocol classes,
    ;; and then the list they scrolled through gave no sign of which generic
    ;; belonged to which -- so the one structural fact the preamble spends a
    ;; paragraph on was invisible in the part anybody actually reads.
    (dolist (group (group-by-protocol (getf surface :generics)))
      (destructuring-bind (protocol gloss . entries) group
        (format stream "~&~76,,,'=<~>~%~:[POLICY~;~:*~a~] -- ~a~%~
                        ~d generic~:p~%~76,,,'=<~>~2%"
                (and protocol (string protocol)) gloss (length entries))
        (c:print-generic-descriptions entries stream)))
    (format stream "~&~76,,,'=<~>~%OPTIONS~%~76,,,'=<~>~2%")
    (format stream "~
`read by' is every function and method in this image that looks at the value,~%~
asked of the compiler rather than of the source text.  It is here to answer~%~
the question the two mechanisms make unavoidable: *I set it and nothing~%~
happened*.~2%~
When the only reader is a method -- `gaps (layout-policy t)' -- then the~%~
generic is the extension point and the option is what its shipped method~%~
returns.  A policy that overrides that method stops reading the option, and~%~
setting it does nothing.  Change the option to change the shipped answer;~%~
write the method to change the decision.~2%~
`overridden by' is that second sentence made visible: a method on the same~%~
generic that wins wherever the reader applied.  Under it the option decides~%~
nothing, unless the method calls CALL-NEXT-METHOD -- and one that does not is~%~
required by gate 15 to offer an option of its own instead, so there is always~%~
a knob, even when it is not this one.  `overridden for' is the same thing~%~
narrowed to the argument classes it names: GAPS answered separately for a~%~
STACK does not stop *GAPS* reaching every other container.~2%~
Both lists are this image's method list, so a policy you loaded appears in~%~
them.  That is the point: the answer to `I set it and my policy ignored it'~%~
is a fact about the program you are running rather than a sentence somebody~%~
has to remember to keep true.~2%~
An option with no readers at all is a documented value wired to nothing.~%~
There are none: gate 11 fails the build on the first one.~2%")
    (dolist (row (getf surface :options))
      (destructuring-bind (key variable value default documentation readers
                           overridden-by overridden-for)
          row
        (declare (ignore key))
        (format stream "~(~a~)~%  now: ~s~@[   (default ~s)~]~%~
                        ~@[  read by: ~{~a~^, ~}~%~]~
                        ~@[  overridden by: ~{~a~^, ~}~%~]~
                        ~@[  overridden for: ~{~a~^, ~}~%~]  ~a~2%"
                variable value (unless (equal value default) default)
                (mapcar #'option-reader-name readers)
                overridden-by overridden-for
                documentation)))
    (values)))
