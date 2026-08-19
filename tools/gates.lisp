;;;; tools/gates.lisp --- the build gates.  PLAN.org asked for six; there
;;;; are nineteen.  Nine of the thirteen that were added were added because
;;;; they had already found something; gates 15, 17 and 18 are three of the
;;;; other four, all three passed the day they were written, and each says so
;;;; where it stands rather than here.  Gate 19 is the fourth and it failed:
;;;; flake.nix declared a licence the LICENSE file contradicts.
;;;;
;;;; AND GATE 19 IS ALSO WHY THE SENTENCE ABOVE STAYS TRUE.  "Eighteen gates
;;;; run on every build" was written in five places and read by nothing, so
;;;; every gate added after it made five sentences false in silence.  Gate 19
;;;; counts the banners in this file and holds the documents to the number.
;;;;
;;;; "All six run on every commit from day one.  They are cheap and they are
;;;; the only automated defence the project has."
;;;;
;;;; Gate 1 lives in tools/build.lisp because it has to run *during* the load.
;;;; The other seventeen run here.
;;;;
;;;; Twelve of them ask the program a question.  Gate 12 asks the *documents*
;;;; one, which is the half of this project the other twelve cannot see; gate
;;;; 13 asks the *source*, because the one thing a keyword property key cannot
;;;; be asked about is what the compiled image thinks of it; gate 14 asks
;;;; the *test suites*, because the one thing neither the image nor the source
;;;; can say about an extension point is whether anybody has ever used it;
;;;; gate 15 asks the image and then the source, in that order, because whether
;;;; a method overrides another is a fact about the method list and whether it
;;;; composes with the one it overrides is a fact only the text has; and gate 16
;;;; asks all four at once -- the image for what a published name denotes, and
;;;; the source, the suites and the documents for whether anything at all wants
;;;; it -- because a name nobody reaches is not visible from any one of them.
;;;;
;;;; Gate 17 is the smallest question in the file and the one with the widest
;;;; blast radius if it is ever wrong: when an option and a generic share a
;;;; name, are they one relationship or two things that happen to be spelled
;;;; alike?  Nothing had ever asked, and a reader has no way to.
;;;;
;;;; Gate 18 is the only one that asks about the *file list* rather than about
;;;; anything in the program: a DEFAULTS- prefix says the file beside it is an
;;;; algorithm that defines no methods, which was true of one pair and
;;;; decoration on the other.  It reads source text for gate 13's reason.

(require :asdf)
(require :sb-introspect)

(defparameter *failures* '())

(defparameter *current-gate* 0
  "Which gate the form now being evaluated belongs to.

READ BY TOOLS/RUN-GATES.LISP AND BY NOTHING IN THIS FILE, which is the shape
this file needs and did not have: the gates are bare top-level forms, so a gate
that signals rather than failing takes the whole run with it and every gate
after it goes unreported.  tools/integration.lisp wrote that lesson down --
`Run one section, and do not let it take the rest of the run with it' -- after
being one 250-line LET that stopped growing at eighteen checks.  Gates 13 and
16 wrap their own file reads for exactly the same reason, in exactly those
words: a file this cannot read is a gate that cannot run, not a gate that
passes.  The reasoning was applied to two gates and not to the file that holds
them.")

(defun fail (gate format &rest arguments)
  (push (format nil "gate ~a: ~?" gate format arguments) *failures*))

(defun banner (gate title)
  (setf *current-gate* gate)
  (format t "~&~%---- gate ~a: ~a ~a~%" gate title
          (make-string (max 0 (- 50 (length title))) :initial-element #\-)))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm"))

(defmacro sym (name) `(read-from-string ,name))
(defun call (name &rest arguments) (apply (read-from-string name) arguments))

(defparameter *root*
  (truename (or (uiop:getenv "LATTICEWM_ROOT") *default-pathname-defaults*))
  "The project.  Gates that ask a *file* a question resolve it against this.")

(defun under (directory path)
  "True when PATH is a file inside DIRECTORY of the project.

Asked of a truename against the project root rather than by searching the
namestring for a substring, because a checkout in ~/src/latticewm/ would
otherwise answer yes to everything."
  (let ((prefix (namestring (merge-pathnames directory *root*)))
        (name (namestring (or (ignore-errors (truename path))
                              (merge-pathnames path *root*)))))
    (eql 0 (search prefix name))))

(defun relative (path)
  "PATH written from the project root, so lattice/policy.lisp is not policy.lisp."
  (let ((root (namestring *root*))
        (name (namestring (or (ignore-errors (truename path))
                              (merge-pathnames path *root*)))))
    (if (eql 0 (search root name)) (subseq name (length root)) name)))

(defun code-of (path)
  "PATH's text with comment and string contents blanked out.

A GATE MUST NOT BE SATISFIABLE BY BEING TOLD IT PASSES.  Gate 3's first
version searched the lattice's *source* for \"src/\" and flagged four
docstrings that said the lattice touches no file under src/.  Every check below
that greps for a token has that failure mode, and the only fix that stays fixed
is to stop greps seeing prose: a docstring explaining a rule reads as an empty
string, so it can neither satisfy a check nor trip one.

Line structure is preserved so the caller can still report a line number, and
so a multi-line docstring cannot hide a real occurrence on a later line."
  (let ((text (with-open-file (in path)
                (let ((buffer (make-string (file-length in))))
                  (subseq buffer 0 (read-sequence buffer in))))))
    (let* ((end (length text))
           (out (copy-seq text))
           (index 0))
      (flet ((blank (from to)
               (loop for i from from below to
                     unless (char= #\Newline (char text i))
                       do (setf (char out i) #\Space))))
        (loop while (< index end)
              do (let ((c (char text index)))
                   (cond
                     ;; A single escape.  #\; is a character literal and #\"
                     ;; opens nothing; skipping the escaped character is the
                     ;; whole of what is needed to get both right.
                     ((char= c #\\) (incf index 2))
                     ((char= c #\;)
                      (let ((stop (or (position #\Newline text :start index) end)))
                        (blank index stop)
                        (setf index stop)))
                     ((char= c #\")
                      (let ((stop (loop with i = (1+ index)
                                        while (< i end)
                                        do (cond ((char= (char text i) #\\) (incf i 2))
                                                 ((char= (char text i) #\") (return i))
                                                 (t (incf i)))
                                        finally (return end))))
                        (blank (1+ index) stop)
                        (setf index (min end (1+ stop)))))
                     (t (incf index))))))
      out)))

(defun code-lines (path)
  "PATH as (line-number . code) pairs, comments and string bodies removed."
  (let ((code (code-of path)))
    (loop with number = 1
          for start = 0 then (1+ stop)
          for stop = (position #\Newline code :start start)
          collect (cons number (subseq code start (or stop (length code))))
          do (incf number)
          while stop)))

;;; ---------------------------------------------------------------- gate 2

(banner 2 "every generic on either surface, and every command, is documented")
;; BOTH SURFACES.  This gate asked about the policy protocol only, for the
;; whole life of the project, because the policy protocol was the only one that
;; could describe itself — and the container protocol's undocumented member was
;; not a missing docstring but a missing *definition*: REPLACE-CHILD was
;; exported under the comment `the container protocol' in package.lisp and
;; defined nowhere at all.  A gate that walks the live generics cannot be told
;; that lie, which is the whole argument for asking the image rather than the
;; source.
(let ((generics (call "latticewm/policy:undocumented-generics"))
      (containers (call "latticewm/core:undocumented-container-generics"))
      (commands (call "latticewm/runtime::undocumented-commands")))
  (dolist (g generics) (format t "  UNDOCUMENTED <-- flag me: ~(~a~)~%" g))
  (dolist (g containers)
    (format t "  UNDOCUMENTED <-- flag me: container protocol ~(~a~)~%" g))
  (dolist (c commands)
    (format t "  UNDOCUMENTED <-- flag me: command ~a~%"
            (call "latticewm/runtime:command-name" c)))
  (if (or generics containers commands)
      (fail 2 "~d undocumented policy generic~:p, ~d undocumented container ~
               protocol generic~:p, ~d undocumented command~:p"
            (length generics) (length containers) (length commands))
      (format t "  all ~d policy generics, ~d container protocol generics ~
                 and ~d commands documented~%"
              (length (call "latticewm/policy:policy-generics"))
              (length (call "latticewm/core:container-protocol-generics"))
              (length (call "latticewm/runtime:all-commands")))))

;;; ---------------------------------------------------------------- gate 3

(banner 3 "the lattice touches no core, and the core names no lattice")
;; THE SECOND HALF OF THAT TITLE IS NEW, AND IT IS THE DIRECTION THAT BROKE.
;;
;; For the whole life of the project this gate asked one question — does the
;; extension reach into the core? — and the answer was always no.  Nothing
;; asked the mirror question, and the mirror question had been failing since
;; the echo area was written: the *shipped default* of ECHO-CONTENT, in
;; src/policy/appearance.lisp, read :LATTICE/ADDRESS off the focused node and
;; destructured the extension's private cons representation inline.
;;
;; It is worth being exact about why nothing saw it.  It is not a core edit
;; the lattice asked for, so it never appeared in FINDINGS.org's list.  The
;; lattice does not touch it, so this gate passed.  It is documented and
;; dispatches on a policy class, so gate 2 and the extension surface were
;; happy.  And it *works* — with the lattice loaded the status line shows the
;; coordinate, which is what everybody wanted, achieved by the core doing the
;; extension's job for it.  The generic that existed to prove the boundary was
;; crossed by its own default implementation, in the same file.
;;
;; A boundary is two rules, and a project that checks one of them has a
;; boundary on one side.  CURSOR-PLACE-NAME is the repair; this is the check
;; that keeps it repaired.
(defparameter *extension-namespaces* '(":lattice/")
  "Namespaces that belong to an extension and may not appear in src/.

One entry today.  A second extension adds a second string, and that is the
whole maintenance burden -- the alternative, deriving the list from what is
loaded, cannot work here precisely because nothing under src/ is allowed to
know the extension exists.")
;; GATE 3 USED TO BE A REPORT WEARING A GATE'S UNIFORM.  It computed two
;; numbers, printed them, and called FAIL under no circumstance whatsoever —
;; so "ALL GATES PASS" read as eight assertions when it was six assertions and
;; two measurements, and its own parenthetical admitted as much.
;;
;; It has thresholds now, and they are the two failures it was written to
;; describe: a lattice that has grown into a second core, and a lattice that
;; passes the letter by pushing everything through PROPS and &rest arguments
;; until it has almost no methods at all.  Both are stated as numbers because
;; both are only visible as numbers.
(defparameter *lattice-line-budget* 1500
  "Past this many *code* lines, the lattice is a second window manager.

CODE LINES, WHICH IT WAS NOT, AND THAT WAS OLD GATE 6'S DISEASE IN THE FILE
THAT EXECUTED IT.  This counted every line the file had -- 2,142 of them, of
which 293 were blank and 265 were comment.  So a quarter of what the budget
measured was how much the author explains himself, and the cheapest way to buy
headroom under it was to delete an explanation.  A build gate that pays for
deleted comments is the same species as a ratio that pays for moved files, and
it survived the sweep that killed the ratio because that sweep was aimed at a
gate's *name*.

The number moved with the rule, keeping about the headroom the old one had --
a fifth over where the tree sits, which is room to add a container kind and
not room to add a window manager.  CODE-OF blanks comments *and docstrings*,
so this is also immune to the failure gate 3's first version had: an
explanation of the rule cannot spend the budget it explains, and a file cannot
buy headroom by saying less.")
(defparameter *lattice-method-floor* 12
  "Below this, it is not extending through the protocol -- it is going round it.")
(if (probe-file "lattice.asd")
    (let* ((files (directory "lattice/*.lisp"))
           (blank-p (lambda (pair)
                      (string= "" (string-trim '(#\Space #\Tab) (cdr pair)))))
           (lines (reduce #'+ files :key
                          (lambda (path)
                            (count-if-not blank-p (code-lines path)))))
           (methods (reduce #'+ files :key
                            (lambda (path)
                              ;; Over code as well: a docstring that quotes
                              ;; "(defmethod " is prose, and counting it would
                              ;; let the floor be answered by talking about
                              ;; methods instead of writing them.
                              (count-if (lambda (pair)
                                          (search "(defmethod " (cdr pair)))
                                        (code-lines path)))))
           (core-edits
             ;; The letter of the gate, checked where it is actually
             ;; expressible: the lattice's own system definition must not
             ;; claim a component under src/.  Scanning the *source* for the
             ;; string "src/" is what a first version did, and it flagged four
             ;; docstrings explaining that the lattice touches no file under
             ;; src/ — a gate that fails on being told it passes.
             (with-open-file (in "lattice.asd")
               (loop for line = (read-line in nil) while line
                     when (and (search "\"src" line) (search ":file" line))
                       collect line)))
           (reach-ins
             ;; The mirror, over code only -- CODE-OF blanks comments and
             ;; string bodies, so this docstring and the ones in
             ;; appearance.lisp and lattice/policy.lisp that explain the rule
             ;; are invisible to it.  That is not a nicety: the explanation of
             ;; why :LATTICE/ADDRESS must not be in src/ has to be allowed to
             ;; say ":LATTICE/ADDRESS".
             (loop for path in (directory "src/**/*.lisp")
                   append (loop for (number . code) in (code-lines path)
                                when (some (lambda (namespace)
                                             (search namespace code
                                                     :test #'char-equal))
                                           *extension-namespaces*)
                                  collect (format nil "~a:~d"
                                                  (relative path) number)))))
      (format t "  lattice/: ~d code lines (budget ~d), ~d defmethods (floor ~d)~%"
              lines *lattice-line-budget* methods *lattice-method-floor*)
      (when core-edits
        (fail 3 "lattice.asd claims a component under src/: ~{~a~^ ~}"
              core-edits))
      (when reach-ins
        (fail 3 "src/ names an extension's private namespace at ~{~a~^, ~}.~%~
                 ~4tThe core doing an extension's job for it is still a core~%~
                 ~4tedit, and it is the kind that never appears on the list~%~
                 ~4tbecause the extension never had to ask.  Give the~%~
                 ~4tdecision a generic and let the extension answer it --~%~
                 ~4tCURSOR-PLACE-NAME is the worked example."
              reach-ins))
      (when (> lines *lattice-line-budget*)
        (fail 3 "the lattice is ~d code lines against a budget of ~d -- ~
                 an extension this size is a second core, and the thesis is ~
                 that a container kind costs a few hundred lines.~%~
                 ~4tComments and blank lines are not counted, so deleting an~%~
                 ~4texplanation does not buy any of this back."
              lines *lattice-line-budget*))
      (when (< methods *lattice-method-floor*)
        (fail 3 "the lattice has ~d defmethods against a floor of ~d -- ~
                 an extension that answers almost no generics is going round ~
                 the protocol rather than through it, which passes the letter ~
                 of this gate and fails its point"
              methods *lattice-method-floor*))
      (unless (or core-edits reach-ins (> lines *lattice-line-budget*)
                  (< methods *lattice-method-floor*))
        (format t "  no core edits, no reach-ins from src/, ~
                   and the numbers are in range~%")))
    (fail 3 "lattice.asd is missing -- the experiment is not being run"))

;;; ---------------------------------------------------------------- gate 4

(banner 4 "the core runs without the lattice")
(let ((world (call "latticewm/core:make-world"))
      (policy (make-instance (sym "latticewm/policy:conventional-policy"))))
  (handler-case
      (progn
        (call "latticewm/policy:on-window-open" policy world
              (make-instance (sym "latticewm/core:window") :app-id "gate"))
        (call "latticewm/policy:on-window-open" policy world
              (make-instance (sym "latticewm/core:window") :app-id "gate2"))
        (let ((placements (call "latticewm/policy:layout-node" policy
                                (call "latticewm/core:world-root" world)
                                (call "latticewm/core:make-rect" 0 0 1920 1080))))
          (format t "  placed 2 windows, laid out ~d node~:p, no lattice loaded~%"
                  (length placements))))
    (error (condition) (fail 4 "~a" condition))))

;;; ---------------------------------------------------------------- gate 5

(banner 5 "codegen counts and bound versions match the pinned XML")

(defun declared-interface-version (path interface)
  "The version attribute INTERFACE declares in the protocol XML at PATH."
  (with-open-file (in path)
    (let ((text (make-string (file-length in))))
      (read-sequence text in)
      (let ((head (search (format nil "<interface name=\"~a\"" interface) text)))
        (when head
          (let* ((at (search "version=\"" text :start2 head))
                 (start (+ at 9))
                 (end (position #\" text :start start)))
            (parse-integer (subseq text start end))))))))

;; RE-VENDORED FROM RIVER 0.4.6, WHICH IS WHY FIVE OF THESE SIX ROWS MOVED.
;; The event counts went up and nothing else did, in every file: river 0.4.6
;; added `capture_sessions' to river_window_v1 and river_output_v1, and a
;; `done' event to each of the three input protocols' device interfaces.  No
;; request, interface or enum changed anywhere.  That is the shape of an
;; additive protocol release, and it is the whole reason re-vendoring was the
;; cheaper of the two ways out of the version mismatch — see PINNED.
(let ((expected '(("river-window-management-v1" 8 57 44 10)
                  ("river-xkb-bindings-v1" 3 11 5 1)
                  ("river-layer-shell-v1" 3 6 4 1)
                  ;; The three input protocols.  They were vendored here and
                  ;; never compiled, which is why this row is worth having: an
                  ;; XML in the tree that no component names is invisible to
                  ;; every gate, and these three were the difference between a
                  ;; machine that can be configured and one that cannot.
                  ("river-input-management-v1" 2 10 6 3)
                  ("river-libinput-config-v1" 4 27 61 23)
                  ("river-xkb-config-v1" 3 12 12 3)))
      (total-requests 0))
  (dolist (row expected)
    (destructuring-bind (name interfaces requests events enums) row
      (let ((path (format nil "src/protocol/~a.xml" name))
            (counts (list 0 0 0 0)))
        (with-open-file (in path)
          (loop for line = (read-line in nil) while line
                do (when (search "<interface " line) (incf (first counts)))
                   (when (search "<request " line) (incf (second counts)))
                   (when (search "<event " line) (incf (third counts)))
                   (when (search "<enum " line) (incf (fourth counts)))))
        (incf total-requests (second counts))
        (if (equal counts (list interfaces requests events enums))
            (format t "  ~a~40t~{~d~^/~}~%" name counts)
            (fail 5 "~a is ~{~d~^/~}, pinned as ~{~d~^/~}"
                  name counts (list interfaces requests events enums))))))
  ;; And every one of them has a checked wrapper, so a protocol change cannot
  ;; leave a request unguarded until the day somebody first calls it.
  (let ((wrapped (length (call "latticewm/wire:all-wrapped-requests"))))
    (format t "  wrapped requests~40t~d~%" wrapped)
    (when (< wrapped total-requests)
      (fail 5 "~d requests in the XML but only ~d wrappers"
            total-requests wrapped)))
  ;; AND THE VERSION WE BIND IS THE VERSION THE XML DECLARES, which the counts
  ;; above cannot see.  Re-vendoring is a file copy *and* a constant, and the
  ;; copy alone leaves a build that compiles, passes every count in this gate,
  ;; and then refuses to start against the very river it was just vendored
  ;; from -- diagnosable only by running it, which is the one thing a gate is
  ;; here to spare you.  It is four steps in INSTALL.org; this is the line
  ;; that stops it being done in three.
  ;;
  ;; ALL SIX PROTOCOLS, WHICH USED TO BE TWO.  The other four had no constant
  ;; to check because they had no constant at all: they bound at whatever
  ;; version river advertised, so there was nothing for a gate to disagree
  ;; with.  That is the newer-river half of the same bug this gate was written
  ;; for -- a build promising to decode events its bindings have never seen --
  ;; and it is only checkable now that each one has a ceiling to check against.
  (dolist (row '(("river-window-management-v1" "river_window_manager_v1"
                  "latticewm/runtime::+window-management-version+")
                 ("river-xkb-bindings-v1" "river_xkb_bindings_v1"
                  "latticewm/runtime::+xkb-bindings-version+")
                 ("river-layer-shell-v1" "river_layer_shell_v1"
                  "latticewm/runtime::+layer-shell-version+")
                 ("river-input-management-v1" "river_input_manager_v1"
                  "latticewm/runtime::+input-manager-version+")
                 ("river-libinput-config-v1" "river_libinput_config_v1"
                  "latticewm/runtime::+libinput-config-version+")
                 ("river-xkb-config-v1" "river_xkb_config_v1"
                  "latticewm/runtime::+xkb-config-version+")))
    (destructuring-bind (file interface variable) row
      (let ((declared (declared-interface-version
                       (format nil "src/protocol/~a.xml" file) interface))
            (bound (symbol-value (sym variable))))
        (if (eql declared bound)
            (format t "  ~a~40tceiling v~d~%" interface bound)
            (fail 5 "~a declares version ~a in the vendored XML, but ~a is ~a"
                  interface declared variable bound)))))
  ;; AND EVERY FLOOR IS UNDER ITS OWN CEILING.  A floor above the ceiling is a
  ;; program that refuses every river in existence, including the one it was
  ;; vendored from, and it is a one-character typo away at all times: the two
  ;; constants sit next to each other and rise on different occasions -- the
  ;; ceiling whenever we re-vendor, the floor only when we start sending a
  ;; request that is newer than the old one.  Nothing else notices, because
  ;; the failure needs a running compositor to show itself.
  (dolist (row '(("river_window_manager_v1"
                  "latticewm/runtime::+window-management-floor+"
                  "latticewm/runtime::+window-management-version+")
                 ("river_xkb_bindings_v1"
                  "latticewm/runtime::+xkb-bindings-floor+"
                  "latticewm/runtime::+xkb-bindings-version+")))
    (destructuring-bind (interface floor-var ceiling-var) row
      (let ((floor (symbol-value (sym floor-var)))
            (ceiling (symbol-value (sym ceiling-var))))
        (if (<= floor ceiling)
            (format t "  ~a~40taccepts v~d-v~d~%" interface floor ceiling)
            (fail 5 "~a floor is ~d but its ceiling is ~d, so no river can ~
                     ever satisfy it" interface floor ceiling))))))

;;; ---------------------------------------------------------------- gate 6

(banner 6 "how much of the behaviour is answered from outside src/")
;; THIS GATE USED TO BE THE RUNTIME-TO-POLICY LINE RATIO, AND THE RATIO HAD TO
;; GO.  It is worth spending the space, because deleting a gate is the thing
;; this file is least willing to do and the argument had better be good.
;;
;; The ratio counted non-blank non-comment lines: src/wire/ + src/runtime/
;; against src/model/ + src/policy/ + lattice/, floored at 0.80, sitting at
;; 0.99.  It stood for PLAN §extensibility-real — Lisp is not what kept Emacs
;; alive, the *ratio* is; 1.3 million lines of Elisp on 400,000 of C, so every
;; feature is a worked example of how to write a feature.  That argument is
;; still correct.  It is the measurement that stopped tracking it.
;;
;; Three tells, and any one of them would have been enough:
;;
;;   * IT PASSED ON ACCOUNTING.  Moving policy/appearance.lisp and
;;     policy/keys.lisp back to src/runtime/ takes it to 0.76, below the floor.
;;     Dropping lattice/ — an *optional extension*, in the numerator, that gate
;;     4 exists to prove the core does not need — takes it to 0.77.  Both, and
;;     it is 0.56.  The margin over the floor was two files and an extension.
;;
;;   * THE FLOOR WAS SET AFTER THE POSITION WAS ARRANGED.  0.80 was written in
;;     the same session the number was walked from 0.82 to 1.17 by relocation,
;;     so "deliberately well below where the project sits" described a floor
;;     placed under a number that had just been pushed up.
;;
;;   * THE MOVES WERE MOVES.  Across the four commits that recovered it, two
;;     reproduce 92-100% of the relocated lines verbatim and add no dispatch
;;     point at all; the commit whose message says the recovery was "on a real
;;     change rather than an accounting one" also changed the counting rule,
;;     and the rule was worth +0.035 of a +0.039 recovery.  Ninety percent of
;;     the change the message denied being accounting was accounting.
;;
;; None of that is dishonesty.  It is what a proxy does once it is cheaper to
;; satisfy than the property behind it, and a build gate is exactly the place
;; where that gets rewarded every time.  When the metric and the property
;; disagree, the tree gets rearranged to fit the metric — and that is a gate
;; actively shaping where new code is filed, which is worse than no gate.
;;
;; SO ASK THE QUESTION THE RATIO WAS A PROXY FOR.  The Emacs claim is not about
;; where files sit.  It is that most of the system's *behaviour* is expressible
;; in the extension language, from outside, by somebody who cannot edit the
;; core.  Gate 3 already contains this check in embryo — it counts the
;; lattice's DEFMETHODs against a floor, precisely because an extension that
;; answers almost no generics is going round the protocol.  Generalise it to
;; the whole surface and it becomes the measurement:
;;
;;   how many policy generics have a specialising method defined outside src/,
;;   and how many such methods are there?
;;
;; No file can be moved to change either number.  A method outside src/ is a
;; method outside src/, and the only way to raise these is to write one.
;;
;; The number is much less flattering than 0.99, and that is the point:
;; 15 of 69 generics have ever been specialised by anything but their own
;; shipped default.  Fifty-four carry a docstring and a gate-2 obligation
;; against a maybe.  That is the real state of the extension surface, and it is
;; now the number printed on every build instead of a comfortable ratio.
;;
;; AND THEN IT IS PRINTED AGAIN WITH THE EXTENSION TAKEN OUT, BECAUSE 15 IS
;; MEASURING n = 1.  The lattice supplies 13 of the 22 methods and 12 of the 15
;; generics; everything else outside src/ -- the four worked examples -- supplies
;; 7 generics and 9 methods.  So most of the evidence that "the behaviour of
;; this system is answerable from outside the core" is one extension, written by
;; the same hand as the protocol it is testing, in the same fortnight.  A single
;; union figure reads as breadth and is not.  Both numbers are printed; the
;; second one is the uncomfortable one and is the one to argue with.
;;
;; It is worth saying what this measurement can and cannot support, because
;; lattice.asd says "the purpose is falsifiability" and this is not that.  What
;; the evidence carries is smaller and still worth having: *the container
;; protocol plus the layout and motion generics are sufficient for a second
;; tree-shaped, rectangle-subdividing layout model, including a sparse,
;; unbounded, two-dimensional, persistent one.*  Falsifiability needs a second
;; party who cannot edit the core and is not the author -- and under
;; FINDINGS.org's own rule ("if step 4 turns out to require core surgery, that
;; is the finding") the core was edited six times and each edit was recorded as
;; output, which is excellent hygiene and the opposite of a falsification test.
;; No gate can fix that.  What a gate can do is stop the number flattering
;; itself, which is what the second line is for.
;;
;; WHY THIS RUNS AFTER GATE 4 AND NOT BEFORE.  It has to load the lattice and
;; the worked examples, which are the only things outside src/ that specialise
;; anything.  Gate 4's whole job is proving the core comes up with the lattice
;; *absent*, so it has to have already run in a clean image.  The order is
;; load-bearing; do not move this above it.
;;
;; LOADING THE EXAMPLES IS ITSELF A CHECK, and it is one nothing else performs.
;; Gate 1 compiles latticewm and lattice; the four files under examples/ are
;; compiled by nothing, and they are what EXTENDING.org tells a stranger to
;; read first.  A rename in the core that breaks one of them is exactly the bug
;; that broke lattice/map.lisp when the lattice was not on gate 1's list, found
;; by a user's config file failing at startup.
(defparameter *outside-generic-floor* 12
  "Policy generics with at least one specialising method *in lattice/*.

SET AT THE NUMBER, NOT COMFORTABLY BELOW IT, and that is a deliberate break
with how the ratio's floor was set.  A floor with slack under it is an
invitation to spend the slack; this one is a ratchet.  It is safe to be a
ratchet because the number cannot fall by accident -- there is no rearrangement
of the tree that lowers it, only the deletion of a method somebody wrote on the
outside.  Lowering it is therefore a decision, which is what a threshold is
for.

FROM lattice/ ALONE, AND IT USED TO BE FROM EVERYTHING OUTSIDE src/, WHICH
FROZE THE TUTORIAL.  The union of lattice/ and examples/ was 15 generics and
22 methods and the floors were set at exactly 15 and 22, with zero slack on
either -- so deleting any one worked example failed the build.  Delete 01 and
the count is 14; delete 03 and the method count is 20.  An example you cannot
delete is not an example, it is load-bearing infrastructure with a docstring,
and the ratchet was defending the teaching material's line count.  The load of
examples/ above stays, because *that* is the check which has actually caught
something -- a rename in the core that breaks a file EXTENDING.org sends a
stranger to read first.  What is floored is the extension, which is the thing
the claim is about.

PLAN.org §generics wrote down the shape and never got it: \"If this list
reaches thirty, the decomposition has gone wrong in the direction of ceremony.\"
The list is at sixty-nine.  A threshold nobody is ever made to argue with is a
decoration, and the way to stop that is to leave no slack to spend quietly.")

(defparameter *outside-method-floor* 13
  "Methods on policy generics defined in lattice/.

The second number because the first one alone can be gamed the way gate 3's
line budget can: one token method per generic answers the count and demonstrates
nothing.  Together they say breadth *and* depth.")

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "lattice"))

(let ((examples (sort (directory "examples/*.lisp") #'string< :key #'namestring))
      (broken '()))
  (dolist (path examples)
    (handler-case (handler-bind ((warning #'muffle-warning)) (load path))
      (error (condition)
        (push (format nil "~a: ~a" (relative path) condition) broken))))
  (when broken
    (fail 6 "a worked example no longer loads: ~{~%    ~a~}~%    ~
             examples/ is what EXTENDING.org sends a stranger to read, and ~
             nothing~%    else in the build compiles it."
          (reverse broken)))
  (let* ((generics (call "latticewm/policy:policy-generics"))
         (outside '()))
    (dolist (name generics)
      (let ((files '()))
        (loop for method in (closer-mop:generic-function-methods
                             (fdefinition name))
              for source = (ignore-errors
                            (sb-introspect:find-definition-source method))
              for file = (and source
                              (sb-introspect:definition-source-pathname source))
              ;; A method whose source SBCL cannot name counts as inside, which
              ;; is the conservative direction: it can only make this gate
              ;; harder to pass.
              when (and file (not (under "src/" file)))
                do (pushnew (cons (relative file) (and (under "lattice/" file) t))
                            files :test #'equal))
        (when files (push (cons name files) outside))))
    (flet ((generics-of (test)
             (count-if (lambda (entry) (some test (cdr entry))) outside))
           (methods-of (test)
             (reduce #'+ outside
                     :key (lambda (entry) (count-if test (cdr entry))))))
      (let ((count (generics-of (constantly t)))
            (methods (methods-of (constantly t)))
            (lattice-count (generics-of #'cdr))
            (lattice-methods (methods-of #'cdr))
            (other-count (generics-of (lambda (f) (not (cdr f)))))
            (other-methods (methods-of (lambda (f) (not (cdr f))))))
        (dolist (entry (sort outside #'string< :key (lambda (e) (symbol-name (car e)))))
          (format t "    ~(~a~)~24t~{~a~^  ~}~%"
                  (car entry) (mapcar #'car (cdr entry))))
        (format t "  specialised from outside src/~46t~d of ~d~%"
                count (length generics))
        (format t "  methods answering them from outside src/~46t~d~%" methods)
        (format t "  answered only by their own shipped default~46t~d~%"
                (- (length generics) count))
        ;; THE SAME NUMBER WITH THE EXTENSION TAKEN OUT, WHICH IS THE NUMBER
        ;; ANYONE ACTUALLY CARES ABOUT.  The claim under test is that the
        ;; behaviour of this system is answerable from outside the core.  Most
        ;; of the evidence for it is one extension, written by the same hand as
        ;; the protocol it is testing, in the same fortnight -- so the headline
        ;; figure is measuring n = 1 and reads as breadth.  Printed rather than
        ;; floored: the honest number is the one you have to look at, and a
        ;; floor under it would be a floor under the tutorial.
        (format t "  of those, from lattice/~46t~d generics, ~d methods  ~
                   (floors ~d, ~d)~%"
                lattice-count lattice-methods
                *outside-generic-floor* *outside-method-floor*)
        (format t "  of those, from anything else~46t~d generics, ~d methods~%"
                other-count other-methods)
        (when (< lattice-count *outside-generic-floor*)
          (fail 6 "the lattice specialises ~d policy generic~:p, against a ~
                   floor of ~d.~%~
                   ~4tPLAN.org §extensibility-real: the boundary is not the~%~
                   ~4tdisease, how little of the system lives above it is.~%~
                   ~4tSomething that used to be answerable from outside is not."
                lattice-count *outside-generic-floor*))
        (when (< lattice-methods *outside-method-floor*)
          (fail 6 "the lattice defines ~d method~:p on policy generics, ~
                   against a floor of ~d.~%~
                   ~4tThe surface is being demonstrated in fewer places than~%~
                   ~4tit was.  A generic nobody has answered from outside is~%~
                   ~4ta docstring, not an extension point."
                lattice-methods *outside-method-floor*))))))

;;; ---------------------------------------------------------------- gate 7

(banner 7 "every declared hook is run, and every run hook is declared")
;; THE BUG THIS EXISTS FOR IS THE ONE NOTHING ELSE CAN SEE.
;;
;; :FOCUS-CHANGED was declared, documented "For status bars", and named in the
;; extension guide -- and no line anywhere ran it.  A status bar attached to it
;; would simply never have updated, and every check this project has would have
;; kept passing: gate 2 sees a documented hook and the tests never fire it.
;;
;; The mirror image is just as quiet.  A RUN-HOOKS on a name nobody declared is
;; a seam nobody can find, because the generated hook document is built from
;; the declarations.
;;
;; WHAT THIS GATE CANNOT SEE is the third state, which is the one the hooks
;; were actually in: declared, run, and nobody listening.  Both sets it
;; compares are names, and both were written by the same hand on the same
;; afternoon.  Gate 14 is that question.
;;
;; Both directions are one grep against the source, which is the whole reason
;; to have the gate: it is cheap enough that not having it was never a
;; decision, only an oversight.
(flet ((hook-keys (pattern)
         ;; Scan the source rather than the image.  RUN-HOOKS calls are not
         ;; reachable by introspection -- they are call sites, not data.
         (let ((found '()))
           (dolist (path (append (directory "src/**/*.lisp")
                                 (directory "lattice/*.lisp"))
                         found)
             (with-open-file (in path)
               (loop for line = (read-line in nil) while line
                     do (let ((at 0))
                          (loop for hit = (search pattern line :start2 at)
                                while hit
                                do (let* ((start (+ hit (length pattern)))
                                          (end (or (position-if-not
                                                    (lambda (c)
                                                      (or (alpha-char-p c)
                                                          (char= c #\-)))
                                                    line :start start)
                                                   (length line))))
                                     (when (> end start)
                                       (pushnew (subseq line start end) found
                                                :test #'string=))
                                     (setf at (max end (1+ hit)))))))))))
       (declared ()
         (mapcar (lambda (row) (string-downcase (symbol-name (first row))))
                 (call "latticewm/policy:all-hooks"))))
  (let* ((run (hook-keys "run-hooks :"))
         (declared (declared))
         (dead (set-difference declared run :test #'string=))
         (undeclared (set-difference run declared :test #'string=)))
    (format t "  ~d declared, ~d run~%" (length declared) (length run))
    (when dead
      (fail 7 "declared but never run: ~{:~a~^ ~} -- ~
               nothing will ever call a function added to ~:[them~;it~]"
            dead (= 1 (length dead))))
    (when undeclared
      (fail 7 "run but never declared: ~{:~a~^ ~} -- ~
               a seam nobody can find, because the surface lists declarations"
            undeclared))
    (unless (or dead undeclared)
      (format t "  every hook is both declared and run~%"))))

;;; ---------------------------------------------------------------- gate 8

(banner 8 "every event we handle exists on the interface we handle it for")
;; ATTACH-OUTPUT listened for a :NAME event from river_output_v1 for the whole
;; life of the project.  The interface has four events and that is not one of
;; them, so the clause could never fire -- every output was anonymous, and the
;; per-output workspace memory is keyed on the name, so a shipped feature did
;; nothing on every machine.
;;
;; Nothing could see it.  Gate 1 sees a well-formed CASE clause.  Gate 5 counts
;; codegen against the XML but says nothing about which events we *listen* for.
;; The tests pass because they construct state rather than receive it.  It was
;; found by reading the protocol by hand, months later, chasing something else.
;;
;; ON-EVENTS makes each handler name its interface, and derives the event list
;; from the CASE clauses themselves so the two cannot drift.  This checks that
;; list against the vendored XML -- the same XML gate 5 pins.
(flet ((events-of (interface)
         ;; Keywordised the way wayflan does it: underscores to dashes.
         (let ((found '()))
           (dolist (file (directory "src/protocol/*.xml") found)
             (with-open-file (in file)
               (let ((text (make-string (file-length in))))
                 (read-sequence text in)
                 (let* ((head (search (format nil "<interface name=\"~a\"" interface)
                                      text)))
                   (when head
                     (let ((tail (search "</interface>" text :start2 head)))
                       (loop with at = head
                             for hit = (search "<event name=\"" text
                                               :start2 at :end2 tail)
                             while hit
                             do (let* ((start (+ hit 13))
                                       (end (position #\" text :start start)))
                                  (push (intern (string-upcase
                                                 (substitute #\- #\_
                                                             (subseq text start end)))
                                                :keyword)
                                        found)
                                  (setf at end))))))))))))
  (let ((checked 0) (bad '()))
    (dolist (entry (call "latticewm/runtime:all-handled-events"))
      (destructuring-bind (interface . handled) entry
        (let ((real (events-of interface)))
          (cond
            ((null real)
             (push (format nil "~a is not an interface in the pinned XML" interface)
                   bad))
            (t
             (dolist (event handled)
               (incf checked)
               (unless (member event real)
                 (push (format nil "~a has no ~s event (it has ~{~s~^ ~})"
                               interface event real)
                       bad))))))))
    (format t "  ~d event handler~:p across ~d interface~:p~%"
            checked (length (call "latticewm/runtime:all-handled-events")))
    (if bad
        (fail 8 "~{~%    ~a~}" (reverse bad))
        (format t "  every one of them exists~%"))))

;;; ---------------------------------------------------------------- gate 9

(banner 9 "the image, the one installer and the sample config agree")
;; THE SHIPPED CONFIGURATION USED TO LOAD ONLY ON THE AUTHOR'S MACHINE.
;;
;; SAMPLE-CONFIG offered to load the lattice; the lattice is an ASDF system;
;; ASDF finds a system by finding its .asd on a path.  tools/image.lisp did not
;; build the lattice into the image and install.sh did not install lattice.asd
;; or lattice/ anywhere — so on every machine that installed from a package,
;; the default configuration failed at load, and the failure routed straight
;; into the startup path.
;;
;; Nothing could see it, because during development ASDF finds lattice.asd in
;; the build tree, which is still there.  That is the whole shape of the bug:
;; it is invisible to everything except a machine that does not have the source.
;;
;; AND THEN THIS GATE READ ONE INSTALLER AND THERE WERE TWO.  flake.nix had an
;; `installPhase' with its own hand-maintained list of what ships, and that list
;; did not include lattice.asd — so the bug above, the one this gate is named
;; after, was live on the nix path for the whole life of the gate.  It was live
;; on the *worse* path: a store path is what reaches somebody who never ran
;; `make', which is exactly the machine "does not have the source" describes.
;; The two lists disagreed about the lattice sources, the man pages, the
;; launcher, where doc/ lands, the protocol XML, which .org files count as
;; documentation, both licences, and a SWANK client that existed only on the nix
;; side and could not work there.  flake.nix enumerates them; this is the only
;; other place that says so, and it says so without a count, because a number
;; repeated in two files is the FINDINGS §census failure one level up.
;;
;; A gate that names an artifact is a gate that can be satisfied by adding a
;; row, which is the mistake gate 5's comment records making.  So the check here
;; is not "does flake.nix also install lattice.asd" — it is *THERE IS ONE
;; INSTALLER*: flake.nix runs install.sh and installs nothing by hand.  Under
;; that, a new artifact is added in one place or it is added nowhere, and the
;; question of whether the two paths agree stops being a question.
;;
;; It is still text matching, which is crude, and it is still the only check
;; that can be made without actually installing to a clean prefix.  Nothing
;; installs to a clean prefix here — the image does not exist yet when the gates
;; run.  tools/install-check.sh is where that happens; `make install-check'
;; runs it, CI runs it after the image, and `nix build' now runs the installer
;; itself, in a sandbox, on every build.
(defun nix-phase (name)
  "The body of flake.nix's NAME phase, as lines, or NIL.

Nix has no reader here and does not need one: a phase is a '' ... '' block
introduced by a line naming it, which is enough to say what is inside it.

TWO GATES READ A PHASE and this used to be written out inside one of them.
Gate 9 asks whether installPhase delegates to install.sh; gate 21 asks whether
buildPhase delegates to the Makefile, and it exists because nothing did --
buildPhase was five hand-typed `sbcl --load' lines under a comment claiming
they were the steps `make check' runs."
  (when (probe-file "flake.nix")
    (with-open-file (in "flake.nix")
      (loop with inside = nil
            with marker = (format nil "~a = ''" name)
            for line = (read-line in nil) while line
            when (and inside (search "'';" line)) do (loop-finish)
              when inside collect line into body
            when (search marker line) do (setf inside t)
            finally (return body)))))

(flet ((contains (path text)
         (and (probe-file path)
              (with-open-file (in path)
                (loop for line = (read-line in nil) while line
                      thereis (search text line)))))
       (install-phase () (nix-phase "installPhase")))
  (let ((problems '())
        (phase (install-phase)))
    (unless (contains "tools/image.lisp" "(asdf:load-system \"lattice\")")
      (push "tools/image.lisp does not build the lattice into the image" problems))
    (unless (contains "install.sh" "lattice.asd")
      (push "install.sh does not install lattice.asd" problems))
    (unless (contains "install.sh" "EXTENDING.org")
      (push "install.sh does not install the extension guide" problems))
    ;; THE FONT'S LICENCE IS NOT A DOCUMENT, IT IS A CONDITION.  font.lisp is a
    ;; generated table of Terminus glyphs and says in its own header that the
    ;; OFL text must travel with any copy of it.  The binary contains the table,
    ;; so every install is a copy — and neither install path shipped the licence
    ;; on purpose: nix caught it through a doc/*.txt glob and install.sh had a
    ;; hand-written list that did not name it.  Our own LICENSE was shipped by
    ;; neither, because it has no extension and both paths matched on one.
    ;; Each message is built with FORMAT NIL rather than written as a string
    ;; with ~ continuations in it, because these are interpolated into FAIL's
    ;; report with ~A: a directive inside one of them is text by then, and the
    ;; message arrives with a literal tilde in the middle of a sentence.
    (unless (contains "install.sh" "OFL-TERMINUS.txt")
      (push (format nil "install.sh does not install doc/OFL-TERMINUS.txt; ~
src/runtime/font.lisp says the OFL text must travel with any copy of the font ~
table, and the binary is a copy of it")
            problems))
    (unless (contains "install.sh" "\"$root/LICENSE\"")
      (push "install.sh does not install LICENSE" problems))
    (unless (contains "install.sh" "src/protocol")
      (push (format nil "install.sh does not install the pinned protocol, so ~
a version mismatch cannot be diagnosed from an installed copy")
            problems))
    ;; And the structural half: one installer, not two.
    ;;
    ;; ASKED ONLY WHEN THERE IS A FLAKE.  §packaging's test is that deleting
    ;; flake.nix leaves a project that still builds, so a gate that fails on its
    ;; absence would be a gate asserting the dependency the file exists to deny.
    (cond ((not (probe-file "flake.nix")))
          ((null phase)
           (push (format nil "flake.nix has no installPhase; if it has stopped ~
installing anything, this gate no longer knows what a packaged install contains")
                 problems))
          (t
           (unless (some (lambda (line) (search "install.sh" line)) phase)
             (push (format nil "flake.nix's installPhase does not run ~
install.sh, so there are two installers again and only one of them is checked ~
anywhere")
                   problems))
           (let ((handrolled
                   (remove-if-not
                    (lambda (line)
                      (some (lambda (verb) (search verb line))
                            '("install -D" "install -m" "cp " "cat >" "ln -s")))
                    phase)))
             (when handrolled
               (push (format nil "flake.nix's installPhase installs by hand ~
as well as running install.sh, which is how the two lists diverged before:~
~{~%      ~a~}"
                             (mapcar (lambda (line) (string-trim " " line))
                                     handrolled))
                     problems)))))
    ;; The last three are asked of the *live image* rather than of the text,
    ;; which is both more direct and immune to a docstring that happens to
    ;; mention the thing being checked for.
    (let ((sample (call "latticewm/runtime:sample-config"))
          (directories (mapcar #'namestring
                               (call "latticewm/runtime:data-directories"))))
      (when (search "asdf:load-system" sample)
        (push (format nil "the sample config calls asdf:load-system directly; ~
it should call LOAD-EXTENSION, which knows where an installed system lives")
              problems))
      (unless (search "load-extension" sample)
        (push (format nil "the sample config never mentions LOAD-EXTENSION, so ~
nothing tells a user how to load one")
              problems))
      (unless (some (lambda (path) (search "share/latticewm/" path)) directories)
        (push (format nil "DATA-DIRECTORIES does not look under ~
share/latticewm/, which is where install.sh puts it")
              problems)))
    (if problems
        (fail 9 "~{~%    ~a~}" (reverse problems))
        (format t "  image builds it, one installer ships it, the runtime ~
finds it~%"))))

;;; --------------------------------------------------------------- gate 10

(banner 10 "the core dispatches through the container protocol")
;; THE RULE THIS ENFORCES, stated as the project's own: *when an extension
;; point exists, nothing may implement its behaviour except through it.*
;;
;; COPY-NODE was a DEFUN dispatching on concrete container classes by TYPECASE,
;; sitting in the middle of a layer whose entire advertised contract is that
;; container kinds are open.  A kind that subclassed CONTAINER directly — which
;; the lattice's GRID does — matched no clause, so the copy came back with no
;; children at all.  Silently.  SERIALIZE-NODE had the identical shape and the
;; identical consequence: the whole lattice was dropped on every restart.
;;
;; Both are generics now.  This is what stops the third one being written.
;;
;; LEAF IS DELIBERATELY EXEMPT.  Asking whether a node is a leaf is not
;; container-kind dispatch — it is asking whether this is a *place that holds a
;; window*, which is the terminal of the tree and the one thing every kind
;; agrees about.  The banned set is the containers: the moment src/ names one
;; of them by class, some other kind is being silently left out.
(let ((kinds '("'split" "'stack" "'sequential-container" "'c:split" "'c:stack"
               "'c:sequential-container" "'grid"))
      (dispatch '("typecase" "etypecase" "ctypecase"))
      (exempt '("node.lisp"))
      (found '()))
  (dolist (path (directory "src/**/*.lisp"))
    (let ((name (file-namestring path)))
      (unless (member name exempt :test #'string=)
        (with-open-file (in path)
          (loop for line = (read-line in nil)
                for number from 1
                while line
                do (let ((trimmed (string-left-trim " " line)))
                     (unless (and (plusp (length trimmed))
                                  (char= #\; (char trimmed 0)))
                       ;; A TYPECASE family form naming a container kind.
                       (when (and (some (lambda (form) (search form line)) dispatch)
                                  (some (lambda (kind) (search kind line)) kinds))
                         (push (format nil "~a:~d dispatches on a container kind"
                                       name number)
                               found))
                       ;; Or a TYPEP against one, which is the same mistake
                       ;; written smaller.
                       (when (and (search "(typep " line)
                                  (some (lambda (kind) (search kind line)) kinds))
                         (push (format nil "~a:~d tests for a container kind ~
                                            by class"
                                       name number)
                               found)))))))))
  (if found
      (fail 10 "~{~%    ~a~}~%    ~
                Ask the protocol instead: CONTAINER-ALTERNATIVES-P, ~
                CONTAINER-SPLITS-ALONG-P,~%    CONTAINER-SELECTION, or a ~
                generic of your own with a method per kind."
            (reverse found))
      (format t "  no container kind is named by class outside model/node.lisp~%")))

;;; --------------------------------------------------------------- gate 11

(banner 11 "every registered option is read by something")
;; THE BUG THIS EXISTS FOR SHIPPED FOR THE WHOLE LIFE OF THE OPTION.
;;
;; *SMART-GAPS* was a DEFINE-OPTION, an export, and a docstring promising to
;; "drop gaps and borders entirely when a workspace holds exactly one window".
;; `latticewm --list-options' printed it.  doc/EXTENSION-SURFACE.txt described
;; it as working.  doc/latticewm-config.5 listed it beside *GAPS*.  Nothing in
;; the program read it.  Ten gates, 1469 unit checks and 165 integration checks
;; passed on every one of those builds.
;;
;; NOT ONE OF THOSE INSTRUMENTS WAS POINTED THE WRONG WAY.  Gate 2 asks whether
;; every generic and command is documented.  Gate 9 asks whether the image, the
;; installer and the sample config agree about the option *list*.
;; TESTS/TEST-SURFACE's EVERY-OPTION-IS-REACHABLE-FROM-A-CONFIG-FILE asks
;; whether each option's symbol is the one a config file would set — the check
;; that found twenty-four unsettable options, and the best check in the suite.
;; Every one of them asks the registry a question about itself.  Being
;; registered, being documented, being installable and being settable are four
;; promises an option makes, and it makes a fifth: that somebody looks.
;;
;; THE CHECK IS THE SAME SHAPE AS THE ONE THAT FOUND THE TWENTY-FOUR, which is
;; why it is worth writing down as a shape rather than as a rule: compare two
;; artifacts that are maintained independently, because that is the comparison
;; no human ever performs.  There the two were the registry and the user
;; package.  Here they are the registry and the compiled program.
;;
;; ASK THE COMPILER, NOT THE TEXT.  Every other file-reading check in here
;; greps, and greps are why CODE-OF exists.  A grep for an option's name counts
;; its own DEFPARAMETER, its export, four docstrings that mention it and the man
;; page, so it would have to be taught to ignore all of those and would still
;; be wrong about a read inside a macro expansion.  SBCL recorded who
;; references what while it was compiling the system.  That is a fact about the
;; program, it costs a hash lookup, and it cannot be satisfied by prose.
;;
;; IT RUNS AFTER GATE 6 because gate 6 has by then loaded the lattice and the
;; four worked examples, and an option the lattice alone reads is read.
(let* ((rows (call "latticewm/policy:all-options"))
       (unread '()))
  (dolist (row rows)
    (let ((readers (call "latticewm/policy:option-readers" (first row))))
      (when (null readers) (push (second row) unread))))
  (format t "  registered options~46t~d~%" (length rows))
  (format t "  read by nothing~46t~d~%" (length unread))
  (when unread
    (fail 11 "~d registered option~:p nothing in the program reads:~{~%    ~(~a~)~}~%~
              ~4tIt is printed by --list-options, described in the generated~%~
              ~4textension surface and settable from a config file, and it~%~
              ~4tdoes nothing.  Read it, or delete it -- a knob wired to~%~
              ~4tnothing is worse than a missing feature, because the~%~
              ~4tdocumentation agrees with the user that it should have worked."
          (length unread) (sort (mapcar (lambda (s) (string-downcase (symbol-name s)))
                                        unread)
                                #'string<))))

;;; --------------------------------------------------------------- gate 12

(banner 12 "the prose says things about the program, and something asks")
;; THE GATE THE PROJECT WROTE DOWN AS MISSING AND THEN DID NOT WRITE.
;;
;; FINDINGS.org §census, on discovering that the lattice's line count appeared
;; in four places and three of them disagreed: "Nothing checks that a sentence
;; in an .org file is still true about the program, which is the gate this
;; project does not have."  This is that gate.
;;
;; THE ROT IS NOT HYPOTHETICAL AND IT IS NOT COSMETIC.  Every instrument this
;; project owns points at the code.  Gate 2 asks whether a generic has a
;; docstring; gate 7 asks whether a hook is run; gate 8 asks whether an event
;; exists; gate 11 asks whether an option is read.  Not one of them can see a
;; paragraph.  So:
;;
;;   * doc/EXTENDING.org opened its development loop with "Start the window
;;     manager.  SWANK is listening on port 4005."  ASSESSMENT U1 turned SWANK
;;     off by default -- correctly; it was unauthenticated arbitrary code
;;     execution started before the user had done anything -- and the first
;;     instruction in the extension guide became false.  Eleven gates and two
;;     test suites passed on every build afterwards.
;;
;;   * README's Status section said HiDPI scaling, the layer-shell path and
;;     pointer drag-resize were "all three exercised by the test suite".  None
;;     of the three had a single test reference.
;;
;;   * PLAN §generics said "Thirteen.  If this list reaches thirty, the
;;     decomposition has gone wrong in the direction of ceremony."  The list
;;     was at sixty-five and the sentence had never been revised, so the
;;     project's own written threshold was blown through by a factor of two
;;     with nothing to notice.
;;
;;   * *SMART-GAPS* was documented as working in three places while nothing
;;     read it.  Gate 11 now catches the option; nothing caught the sentences.
;;
;; A PROJECT WHOSE THESIS IS THAT THE REASONING OUTLIVES THE AUTHOR CANNOT
;; TREAT ITS PROSE AS UNTESTED CODE.  PLAN §extensibility-real is explicit that
;; the whole bet routes through "there is a documented generic for the thing
;; you want to change" -- a stranger who cannot trust the document is a
;; stranger reading the source, which is the situation the document exists to
;; prevent.
;;
;; FOUR CHECKS, AND THEY ARE DELIBERATELY OF DIFFERENT KINDS.
;;
;; (a) EVERY PROSE FILE IS CLASSIFIED.  A document is either *current* -- it
;;     describes the program as it is, and a false sentence in it is a bug --
;;     or *historical* -- a dated record, correct when written, frozen on
;;     purpose because editing it into agreement with whatever shipped
;;     destroys its only value.  DESIGN.org says this of itself: "DESIGN.org is
;;     the design.  It is frozen."  The distinction is real and it has to be
;;     written down somewhere a check can read, because the difference between
;;     "stale" and "a record" is otherwise a matter of who is asking.
;;
;;     THE LIST IS CHECKED AGAINST THE DIRECTORY, NOT CONSULTED.  A file in
;;     neither list fails.  This is gate 5's lesson taken the other way round:
;;     gate 5 exists because an unlisted protocol XML is invisible, and the fix
;;     for that was three more hardcoded rows.  Enumerating and demanding
;;     classification means a new document cannot arrive unchecked, which is
;;     the failure mode a list of filenames has.
;;
;;     THE GENERATED DOCUMENTS ARE ENUMERATED THE SAME WAY, and they were not.
;;     *GENERATED-DOCUMENTS* was a DEFPARAMETER with a five-line docstring and
;;     one reader in the whole tree -- its own definition -- which is the very
;;     defect gate 11 abolished for options, grown inside the gate file.  It
;;     was also wrong: it named five files and `make surface' writes six, so
;;     doc/HOOKS.txt was generated, installed, advertised in README, and listed
;;     nowhere.  doc/*.txt is now enumerated against it, with a second list for
;;     the one .txt in that directory that is somebody else's licence text and
;;     not ours to check.
;;
;; (b) EVERY OPTION THE CURRENT DOCUMENTS NAME IS REGISTERED.  Automatic, and
;;     it needs no cooperation from whoever wrote the sentence.  An earmuffed
;;     name in code markup or a source block is a claim that the program has
;;     that knob; if it does not, the sentence is *SMART-GAPS* again with the
;;     mistake one level up.  Restricted to code markup because org's bold
;;     syntax is also asterisks, so scanning raw text finds *and*, *before* and
;;     *not* -- a check with a false-positive rate is a check that gets
;;     weakened, and this one has none.
;;
;;     THE MAN PAGES ARE PROSE TOO AND THIS COULD NOT READ THEM.  Both of them
;;     are on *CURRENT-DOCUMENTS*, and MARKED-TOKENS wants a *pair* of ~ or =
;;     on a line, which roff never writes -- so doc/latticewm-config.5 named
;;     forty earmuffed options, including *SMART-GAPS* twice, and the check
;;     that exists because of *SMART-GAPS* validated none of them.  EARMUFFED-P
;;     then refused hyphens escaped as \- , which is how roff writes every
;;     hyphen, so *swank\-port* would have failed even with a delimiter.  The
;;     four hand-written `.\" CLAIM:' lines in that file are the author
;;     plugging by hand a hole he had no way to see was structural.  Roff has
;;     no asterisk markup, so a .1 or .5 line is read whole, as a source block
;;     is, after the escapes are undone.
;;
;; (c) EVERY OPTION A DOCSTRING NAMES IS REGISTERED.  The load-bearing prose in
;;     this project is not all in .org files.  A docstring is the thing a
;;     stranger reads at the REPL and the thing `--extension-surface' prints,
;;     and until now no instrument could see one: gate 2 asks whether a
;;     docstring exists, never what it says.
;;
;;     THE DISCRIMINATOR IS CASE, and it is the same trick as (b) rather than a
;;     new one.  Prose in this tree emphasises with lowercase asterisks -- *not*
;;     have, the *accumulating* hooks -- and names code in capitals.  Requiring
;;     capitals is exactly org's ~...~ distinction spelled in the convention
;;     docstrings already use, and it takes the false-positive rate to nothing:
;;     of 131 earmuffed mentions in 2078 docstrings, three were shouted
;;     emphasis and were rewritten rather than exempted, because a name in
;;     capitals inside an already-shouting sentence was unreadable anyway.
;;
;; (d) #+CLAIM: MAKES A SENTENCE EXECUTABLE.  The general mechanism, because
;;     (b) and (c) can only cover the claims that happen to be spelled as
;;     names.  A line of the form
;;
;;         #+CLAIM: (= 65 (length (policy-generics)))
;;
;;     is an org keyword, so it does not render, and it is read and evaluated
;;     against the loaded image.  NIL fails the gate, naming the file, the
;;     line, the form and the sentence above it.
;;
;;     CLAIMS ARE READ IN LATTICEWM/USER -- the package a config file is read
;;     in, and the one `latticewm --eval' evaluates into.  A claim can
;;     therefore say only what a user can say, which is the right constraint:
;;     it keeps the claims inside the surface the documents are describing, and
;;     it means any of them can be pasted into a running session and watched.
;;
;;     IT ALSO MEANS A CLAIM CANNOT READ A FILE, which is why FINDINGS.org's
;;     line counts are not pinned here and gate 3 counts them instead.  A check
;;     that could read the source could be satisfied by the source.
;;
;;     A CLAIM MUST CONSULT THE PROGRAM.  `#+CLAIM: t' would otherwise be a
;;     gate that can be told it passes, which is the failure CODE-OF exists to
;;     prevent one file over.  So the form must reference at least one symbol
;;     whose home package is part of the program; a form built from CL symbols
;;     alone is itself a failure.
;;
;; WHAT THIS DOES NOT DO, SAID PLAINLY.  It cannot find a false sentence nobody
;; annotated, and there is no honest way to make it.  What it can do is make a
;; sentence pinnable at the cost of one line, catch the naming rot
;; automatically, and refuse to let a new document arrive unclassified.  The
;; claim count is printed and has no floor, because prose is deleted along with
;; the thing it described and a floor would make that a fight; the three
;; automatic checks are what stop this becoming a no-op if the claims went
;; away.
;;
;; A CLAIM CAN CHANGE THE IMAGE, AND SEVEN GATES READ THE IMAGE AFTER THIS ONE.
;; The paragraph that used to stand here said this gate "runs after every gate
;; that asks the image a question", which was a sentence about the intended
;; ordering rather than about the ordering -- gates 14, 15 and 17 all come
;; afterwards and all ask.  Gate 14 counts the functions attached to each hook
;; *in this image*; gate 17 counts the methods on a generic; a claim is an
;; arbitrary form EVALed in LATTICEWM/USER, and `#+CLAIM: (progn (defmethod
;; ...) t)' is a legal claim that would move both.  Nothing would have said so.
;;
;; So the image is fingerprinted before each claim and again after it, and a
;; claim that moved it fails naming itself.  The fingerprint is of the shape
;; the later gates read -- every option and its value, every hook and how many
;; functions are on it, the command count, and the method count of every
;; generic on both surfaces.  This does not *prevent* a claim from mutating,
;; which nothing in this language can; it means a claim that does cannot do it
;; quietly, which is the whole difference between a rule and a gate.

(defparameter *current-documents*
  '("README.org" "INSTALL.org" "doc/FINDINGS.org"
    "doc/EXTENDING.org" "doc/ONBOARDING.org"
    "doc/latticewm.1" "doc/latticewm-config.5")
  "Documents that describe the program as it is.  A false sentence here is a bug.

FINDINGS.org is on this list and DESIGN.org is not, which is the distinction
worth explaining.  FINDINGS is a report on a system that still exists and its
numbers are still being read as current -- its own census block was wrong for
three commits and said so afterwards.  DESIGN is a record of what was decided
before the code existed, and half its value is showing where that was wrong.")

(defparameter *historical-documents*
  '("doc/DESIGN.org" "doc/PLAN.org" "doc/ASSESSMENT.org" "doc/SPIKE-WEEK0.org")
  "Dated records.  Correct when written, frozen on purpose, not to be edited
into agreement with whatever shipped.

PLAN.org is the awkward one: its front half is a live specification and its
back half is a session log, and the two want opposite treatment.  It is
historical here so that the automatic checks do not run over the log, and its
live sentences carry #+CLAIM: lines instead -- which work in any file.")

(defparameter *generated-documents* '("doc/EXTENSION-SURFACE.txt" "doc/CONTAINER-SURFACE.txt"
                                      "doc/HOOKS.txt"
                                      "doc/COMMANDS.txt" "doc/OPTIONS.txt" "doc/KEYS.txt")
  "Written by `make surface' from the running image.  Not prose, cannot drift,
and doc/latticewm-config.5 says why that is the reference and it is the map.

DOC/HOOKS.TXT WAS MISSING FROM THIS LIST for as long as the list existed, and
the list had no reader, so nothing could tell.  Six files are generated and
five were named.  Gate 12(a) now enumerates doc/*.txt against this, which is
what makes the omission cost something.")

(defparameter *verbatim-documents* '("doc/OFL-TERMINUS.txt")
  "Text in doc/ that is somebody else's and is shipped unaltered.

The Terminus font's licence.  It is not prose about this program, it is not
generated from it, and editing it would be the one thing its licence forbids --
so it is classified rather than checked, on the same principle as the
historical documents: say what a file is, and then the enumeration in (a) can
be total.")

(defun document-lines (path)
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil nil) while line collect line)))

(defun marked-tokens (line)
  "Every token between a pair of ~ or a pair of = on LINE.

Org's code markup, and the reason this gate reads it rather than the raw text:
*gaps* in a paragraph is bold, and *gaps* in ~...~ is a variable."
  (let ((out '()))
    (dolist (delimiter '(#\~ #\=) out)
      (let ((start 0))
        (loop for open = (position delimiter line :start start)
              while open
              for close = (position delimiter line :start (1+ open))
              while close
              do (push (subseq line (1+ open) close) out)
                 (setf start (1+ close)))))))

(defun word-tokens (text)
  "TEXT split into words, where a word is anything a Lisp name may be made of."
  (let ((out '())
        (word (make-string-output-stream)))
    (loop for c across text
          do (if (or (alphanumericp c) (find c "*-/+<>?!"))
                 (write-char c word)
                 (let ((w (get-output-stream-string word)))
                   (when (plusp (length w)) (push w out))))
          finally (let ((w (get-output-stream-string word)))
                    (when (plusp (length w)) (push w out))))
    out))

(defun source-tokens (line)
  "Words on LINE with any trailing comment removed.

Inside a Lisp source block the asterisk is unambiguous, so the whole line is
read -- but a `;' comment in an example is prose again and org bold reappears
in it, which is exactly where *our* came from."
  (word-tokens (subseq line 0 (or (position #\; line) (length line)))))

(defparameter *roff-escapes*
  '(("\\-" . "-") ("\\&" . "") ("\\fB" . "") ("\\fI" . "") ("\\fR" . "")
    ("\\fP" . "") ("\\(lq" . "") ("\\(rq" . "") ("\\(em" . "") ("\\(en" . ""))
  "Roff escapes that appear inside a name or hard against one.

\\- IS THE ONE THAT MATTERS.  Roff writes every hyphen that way, so the man
pages spell *swank-port* as *swank\\-port* -- and EARMUFFED-P accepts letters,
digits and hyphens, which that is not.  The rest are here because a font change
or a quote can sit flush against a name with no space between.")

(defun roff-tokens (line)
  "Words on a roff LINE, escapes undone.  NIL for a roff comment.

READ WHOLE, AS A SOURCE BLOCK IS, and for the same reason: roff has no
asterisk markup, so an asterisk on one of these lines is never emphasis.  The
comment lines are skipped because that is where the `.\\\" CLAIM:' lines live
and (d) reads those; a name in a comment is a note to the author, not a
sentence to a user."
  (let ((trimmed (string-left-trim " " line)))
    (unless (eql 0 (search ".\\\"" trimmed))
      (let ((text trimmed))
        (dolist (escape *roff-escapes*)
          (let ((from 0))
            (loop for at = (search (car escape) text :start2 from)
                  while at
                  do (setf text (concatenate 'string (subseq text 0 at)
                                             (cdr escape)
                                             (subseq text (+ at (length (car escape)))))
                           from (+ at (length (cdr escape)))))))
        (word-tokens text)))))

(defun roff-p (path)
  (member (pathname-type path) '("1" "5") :test #'equal))

(defun earmuffed-p (token)
  (and (> (length token) 2)
       (char= #\* (char token 0))
       (char= #\* (char token (1- (length token))))
       (every (lambda (c) (or (alpha-char-p c) (digit-char-p c) (char= c #\-)))
              (subseq token 1 (1- (length token))))))

(defun named-options (path)
  "Every *earmuffed* name PATH claims the program has, as (NAME . LINE)."
  (let ((out '()) (in-source nil) (roff (roff-p path)))
    (loop for line in (document-lines path)
          for number from 1
          for lowered = (string-downcase line)
          do (cond ((and (not roff)
                         (eql 0 (search "#+begin_src" (string-left-trim " " lowered))))
                    (setf in-source t))
                   ((and (not roff)
                         (eql 0 (search "#+end_src" (string-left-trim " " lowered))))
                    (setf in-source nil))
                   (t (dolist (token (cond (roff (roff-tokens line))
                                           (in-source (source-tokens line))
                                           (t (marked-tokens line))))
                        (when (earmuffed-p token)
                          (pushnew (cons (string-downcase token) number) out
                                   :test #'string= :key #'car))))))
    (nreverse out)))

(defparameter *lattice-option-names*
  (let ((out '()))
    (dolist (path (sort (mapcar #'namestring
                                (directory (merge-pathnames "lattice/*.lisp" *root*)))
                        #'string<)
                  (nreverse out))
      (let ((text (code-of path))
            (marker "define-option")
            (start 0))
        (loop for at = (search marker text :start2 start :test #'char-equal)
              while at
              do (setf start (+ at (length marker)))
                 (let* ((from (position #\* text :start start))
                        (to (and from (position-if-not
                                       (lambda (c)
                                         (or (alphanumericp c) (find c "*-")))
                                       text :start from))))
                   (when (and from to (< from (+ start 4)))
                     (pushnew (string-downcase (subseq text from to)) out
                              :test #'string=)))))))
  "Every option the lattice registers, read out of lattice/*.lisp as text.

THE GATE IMAGE DOES NOT LOAD THE LATTICE and must not: gate 4 exists to prove
the core runs without it, in this image, and it would stop proving anything the
moment this file loaded it for convenience.  So OPTION-BOUNDP cannot answer for
*NEW-WORKSPACE-CELLS* -- which doc/latticewm-config.5 names, correctly, in the
paragraph explaining that enabling the lattice takes *NEW-WORKSPACE* away and
offers that in its place.  Exactly the trade gate 15 is about.

Text and not READ, because reading the file needs the LATTICE package and the
package-local nicknames in it, and neither exists here.  CODE-OF has already
blanked the comments and the strings, so a name in a docstring cannot get in.")

(defun option-named-p (token)
  "True when TOKEN, an earmuffed name from a document, is a name the program has.

Three ways to be one, and the second two are why this is a function rather than
a line: a registered tier-0 option; a special that is exported and bound but is
not a knob, which is what *KEYMAP* and *POLICY* are and they are named all over
the extension guide; or an option the lattice registers, which this image
cannot ask about and reads out of the source instead."
  (let ((symbol (find-symbol (string-upcase token) '#:latticewm/user))
        (key (intern (string-upcase (string-trim "*" token)) :keyword)))
    (or (call "latticewm/policy:option-boundp" key)
        (and symbol (boundp symbol)
             (member (symbol-package symbol)
                     (remove nil (mapcar #'find-package
                                         '("LATTICEWM/CORE" "LATTICEWM/POLICY"
                                           "LATTICEWM/RUNTIME"))))
             t)
        (and (member (string-downcase token) *lattice-option-names* :test #'string=)
             t))))

(defun variable-named-p (token)
  "True when TOKEN names a special that exists, exported or not.

THE WIDER TEST, AND IT IS FOR DOCSTRINGS ONLY.  A manual page naming an
internal special would be promising the reader a knob the program does not
offer, which is why OPTION-NAMED-P insists on the published surface.  A
docstring is addressed to whoever is reading the file it is in:
*RENDER-LEGAL-IN-MANAGE* is not exported and never will be, and WIRE's own
prose naming it is correct rather than a broken promise.  What is still caught
is the only thing that was ever wrong with *SMART-GAPS* -- a name for something
that is not there."
  (let ((packages (remove nil (mapcar #'find-package
                                      '("LATTICEWM/CORE" "LATTICEWM/POLICY"
                                        "LATTICEWM/RUNTIME" "LATTICEWM/WIRE"
                                        ;; The four worked examples are loaded
                                        ;; into this image by gate 6 and are
                                        ;; read in LATTICEWM/USER, so example
                                        ;; 02's *RULES* -- which its own method
                                        ;; docstring names, correctly -- lives
                                        ;; there and nowhere else.
                                        "LATTICEWM/USER"
                                        "COMMON-LISP" "SB-EXT")))))
    (loop for package in packages
          for symbol = (find-symbol (string-upcase token) package)
          thereis (and symbol (boundp symbol) t))))

(defun claim-prefix (line)
  "The claim text on LINE, or NIL.  `#+CLAIM:' in org, `.\\\" CLAIM:' in roff."
  (let ((trimmed (string-left-trim " " line)))
    (dolist (marker '("#+CLAIM:" ".\\\" CLAIM:"))
      (when (eql 0 (search marker trimmed :test #'char-equal))
        (return (subseq trimmed (length marker)))))))

(defun claims-in (path)
  "Every claim in PATH, as (LINE FORM TEXT PROSE).

Consecutive claim lines are one run, and every complete form the run holds is
one claim -- so a form may be spread over several lines when it is a list of
things being asserted at once, and two short claims may sit on two lines
without becoming one.  The line reported is the line the form itself starts
on, not the line the run does, because a failure has to name somewhere to
stand."
  (let ((lines (document-lines path))
        (out '()))
    (loop with index = 0
          while (< index (length lines))
          do (let ((text (claim-prefix (nth index lines))))
               (if (null text)
                   (incf index)
                   (let ((start index) (buffer text))
                     (loop while (and (< (1+ index) (length lines))
                                      (claim-prefix (nth (1+ index) lines)))
                           do (incf index)
                              (setf buffer (concatenate 'string buffer
                                                        (string #\Newline)
                                                        (claim-prefix (nth index lines)))))
                     (let ((prose (loop for back downfrom (1- start) to 0
                                        for candidate = (string-trim " " (nth back lines))
                                        unless (or (zerop (length candidate))
                                                   (claim-prefix candidate))
                                          return candidate
                                        finally (return "")))
                           (position 0))
                       (loop
                         ;; Past the whitespace first, so the line reported is
                         ;; the line the form is written on rather than the
                         ;; line the one before it ended on.
                         (setf position (or (position-if-not
                                             (lambda (c) (member c '(#\Space #\Newline #\Tab)))
                                             buffer :start position)
                                            (length buffer)))
                         (multiple-value-bind (form next)
                             (handler-case
                                 (let ((*package* (find-package '#:latticewm/user))
                                       (*read-eval* nil))
                                   (read-from-string buffer nil :end-of-claims
                                                     :start position))
                               (error (condition)
                                 (values (list :unreadable (princ-to-string condition))
                                         (length buffer))))
                           (when (eq form :end-of-claims) (return))
                           (push (list (+ 1 start (count #\Newline buffer :end position))
                                       form
                                       (string-trim '(#\Space #\Newline #\Tab)
                                                    (subseq buffer position next))
                                       prose)
                                 out)
                           (setf position next))))
                     (incf index)))))
    (nreverse out)))

(defparameter *program-packages*
  (remove nil (mapcar #'find-package
                      '("LATTICEWM/CORE" "LATTICEWM/POLICY" "LATTICEWM/RUNTIME"
                        "LATTICEWM/WIRE")))
  "The packages whose docstrings are this program's prose.

LATTICEWM/USER is not one of them.  It uses the four and defines almost
nothing, so walking it walks CL and SBCL as well -- and SBCL's own docstrings
name SBCL's own specials, which are neither our sentences to check nor our
options to register.")

(defun program-docstrings ()
  "Every docstring the program ships, as (WHERE . TEXT).

Symbols, and then the methods, because a method's docstring is not reachable
from its symbol: a generic with eleven methods has eleven more paragraphs of
prose than DOCUMENTATION on the name returns, and gate 6 shows that most of the
interesting ones are methods."
  (let ((seen (make-hash-table :test #'eq)) (out '()))
    (dolist (package *program-packages* out)
      (do-symbols (symbol package)
        (when (and (eq (symbol-package symbol) package)
                   (not (gethash symbol seen)))
          (setf (gethash symbol seen) t)
          (let ((where (format nil "~(~a~):~(~a~)"
                               (package-name package) (symbol-name symbol))))
            (dolist (kind '(variable function type structure setf))
              (let ((text (ignore-errors (documentation symbol kind))))
                (when text
                  (push (cons (format nil "~a (~(~a~))" where kind) text) out))))
            (let ((function (and (fboundp symbol) (ignore-errors (fdefinition symbol)))))
              (when (typep function 'generic-function)
                (dolist (method (sb-mop:generic-function-methods function))
                  (let ((text (documentation method t)))
                    (when text
                      (push (cons (format nil "~a (a method)" where) text) out))))))))))))

(defun code-name-p (token)
  "True when TOKEN is an earmuffed name written as a name rather than as stress.

CASE IS THE MARKUP A DOCSTRING HAS.  Org draws this distinction with ~...~ and
gate 12(b) leans on it; a docstring has no delimiters, and inventing a
convention for one would be a check nobody could satisfy without being told.
But the convention is already there and universal in this tree: code is written
in capitals and emphasis is written in lowercase.  *SMART-GAPS* is a name and
the *accumulating* hooks are a phrase, and no reader has ever had trouble
telling them apart."
  (and (earmuffed-p token)
       (string= token (string-upcase token))
       (some #'alpha-char-p token)))

(defun image-fingerprint ()
  "What the gates after this one will find when they ask the image.

Every option and its value, every hook and how many functions are attached,
how many commands there are, and how many methods are on each generic of both
surfaces.  Compared with EQUAL, so an option whose value is a structure is
compared by identity -- which is the right test, because the thing being
detected is a claim that reached in and put something else there."
  (flet ((methods-of (name)
           (let ((function (and (fboundp name) (fdefinition name))))
             (cons name (if (typep function 'generic-function)
                            (length (sb-mop:generic-function-methods function))
                            0)))))
    (list (mapcar (lambda (row) (list (first row) (third row)))
                  (call "latticewm/policy:all-options"))
          (mapcar (lambda (row) (list (first row) (third row)))
                  (call "latticewm/policy:all-hooks"))
          (length (call "latticewm/runtime:all-commands"))
          (mapcar #'methods-of (call "latticewm/policy:policy-generics"))
          (mapcar #'methods-of (call "latticewm/core:container-protocol-generics")))))

(defun fingerprint-difference (before after)
  "Which part of the image a claim moved, in words, or NIL when it moved none."
  (let ((names '("an option's value" "a hook's attachments" "the command count"
                 "a policy generic's methods" "a container generic's methods")))
    (loop for was in before for is in after for name in names
          unless (equal was is) collect name)))

(defun claim-vocabulary-complaint (form)
  "NIL when FORM is written in the program's vocabulary, or why it is not.

WITHOUT THIS, `#+CLAIM: t' IS A PASSING CLAIM.  A gate that can be told it
passes is the failure CODE-OF exists to prevent for the greps, and a claim
built out of CL symbols alone is the same mistake in a different alphabet.

NAMING SOMETHING IS THE TEST; the orphans are only how the failure is phrased.
A symbol LATTICEWM/USER had to intern in order to read the form is a symbol no
latticewm package exports, which is what a claim looks like the day the
function it asks about is renamed -- so when a claim reaches nothing at all,
saying which names went nowhere is more use than saying it reached nothing.
They cannot be the test themselves: a LAMBDA's own parameter is interned the
same way, and a gate that rejects LAMBDA is a gate nobody can write a claim
with."
  (let ((packages (remove nil (mapcar #'find-package
                                      '("LATTICEWM/CORE" "LATTICEWM/POLICY"
                                        "LATTICEWM/RUNTIME" "LATTICEWM/WIRE"))))
        (user (find-package '#:latticewm/user))
        (found nil)
        (orphans '()))
    (labels ((walk (node)
               (cond ((symbolp node)
                      (cond ((member (symbol-package node) packages) (setf found t))
                            ((eq (symbol-package node) user) (pushnew node orphans))))
                     ((consp node) (walk (car node)) (walk (cdr node))))))
      (walk form))
    (unless found
      (if orphans
          (format nil "reaches nothing the program owns; ~{~(~a~)~^, ~} ~
                       ~[~;is~:;are~] in no latticewm package -- renamed, or gone?"
                  (reverse orphans) (length orphans))
          "names nothing the program owns, so it cannot be false"))))

;; (a) every prose file is classified.
(let* ((tracked (sort (mapcar #'relative
                              (append (directory (merge-pathnames "*.org" *root*))
                                      (directory (merge-pathnames "doc/*.org" *root*))
                                      (directory (merge-pathnames "doc/*.1" *root*))
                                      (directory (merge-pathnames "doc/*.5" *root*))))
                      #'string<))
       (known (append *current-documents* *historical-documents*))
       (unclassified (remove-if (lambda (name) (member name known :test #'string=)) tracked))
       (missing (remove-if (lambda (name) (member name tracked :test #'string=)) known)))
  (format t "  current~46t~d~%  historical~46t~d~%"
          (length *current-documents*) (length *historical-documents*))
  (when unclassified
    (fail 12 "~d prose file~:p in neither list:~{~%    ~a~}~%~
              ~4tSay which it is in tools/gates.lisp gate 12.  A document that~%~
              ~4tdescribes the program as it is goes in *CURRENT-DOCUMENTS* and~%~
              ~4tis checked; a dated record goes in *HISTORICAL-DOCUMENTS* and~%~
              ~4tis left alone.  Arriving unclassified is the one thing it~%~
              ~4tcannot do, because that is how a document goes unread."
          (length unclassified) unclassified))
  (when missing
    (fail 12 "~d classified document~:p that is not there:~{~%    ~a~}"
          (length missing) missing)))

;; (a) again, for the generated half.
(let* ((tracked (sort (mapcar #'relative (directory (merge-pathnames "doc/*.txt" *root*)))
                      #'string<))
       (known (append *generated-documents* *verbatim-documents*))
       (unclassified (remove-if (lambda (name) (member name known :test #'string=)) tracked))
       (missing (remove-if (lambda (name) (member name tracked :test #'string=)) known)))
  (format t "  generated~46t~d~%  verbatim~46t~d~%"
          (length *generated-documents*) (length *verbatim-documents*))
  (when unclassified
    (fail 12 "~d file~:p in doc/ that nothing says the origin of:~{~%    ~a~}~%~
              ~4tA .txt in doc/ is either written by `make surface' from the~%~
              ~4trunning image, in which case it goes in *GENERATED-DOCUMENTS*~%~
              ~4tand `make surface' has to write it, or it is somebody else's~%~
              ~4ttext shipped unaltered, in which case it goes in~%~
              ~4t*VERBATIM-DOCUMENTS*.  Anything else is a hand-written~%~
              ~4tdocument in the directory reserved for ones that cannot drift."
          (length unclassified) unclassified))
  (when missing
    (fail 12 "~d generated document~:p that is not there:~{~%    ~a~}~%~
              ~4tThe list said `make surface' writes it and the directory says~%~
              ~4tit does not.  One of the two is wrong, and the one that is~%~
              ~4twrong is whichever one nobody reads."
          (length missing) missing)))

;; (b) every option the current documents name is registered.
(let ((wrong '()) (named 0) (roff 0))
  (dolist (name *current-documents*)
    (dolist (row (named-options (merge-pathnames name *root*)))
      (incf named)
      (when (roff-p name) (incf roff))
      (unless (option-named-p (car row))
        (push (format nil "~a:~d  ~a" name (cdr row) (car row)) wrong))))
  (format t "  options named by the current documents~46t~d~%~
             ~4tof those, in the two man pages~46t~d~%" named roff)
  (when wrong
    (fail 12 "~d name~:p the documents say the program has and it does not:~
              ~{~%    ~a~}~%~
              ~4tEither the option was renamed or deleted and the sentence was~%~
              ~4tleft behind, or it never existed.  Both are the same thing to~%~
              ~4ta reader who types it into a config file and gets nothing."
          (length wrong) (reverse wrong))))

;; (c) every option a docstring names is registered.
(let ((wrong '()) (named 0) (strings (program-docstrings)))
  (dolist (row strings)
    (dolist (token (remove-if-not #'code-name-p (word-tokens (cdr row))))
      (incf named)
      (unless (or (option-named-p token) (variable-named-p token))
        (pushnew (format nil "~a~%        in ~a" (string-downcase token) (car row))
                 wrong :test #'string=))))
  (format t "  docstrings read~46t~d~%  options named by a docstring~46t~d~%"
          (length strings) named)
  (when wrong
    (fail 12 "~d name~:p a docstring says the program has and it does not:~
              ~{~%    ~a~}~%~
              ~4tA docstring is what a stranger reads at the REPL and what~%~
              ~4t`--extension-surface' prints, so a knob named in one is the~%~
              ~4tsame promise a manual page makes.  If the asterisks were~%~
              ~4temphasis rather than a name, write the word without them:~%~
              ~4tshouting a name inside an already-shouting sentence reads as~%~
              ~4ta name to everybody including this gate."
          (length wrong) (reverse wrong))))

;; (d) every #+CLAIM: is true, and none of them moved the image.
;;
;; The shipped keymap is installed first, because the key tables are the most
;; read prose in the project and a claim about a binding needs the bindings to
;; exist.  START never runs in this image, so nothing else has done it.  It is
;; done before the first fingerprint is taken, since it is this gate's own
;; deliberate mutation and not a claim's.
(call "latticewm/runtime::install-default-keymap")
(let ((checked 0) (broken '()) (moved 0) (fingerprint (image-fingerprint)))
  (dolist (name (append *current-documents* *historical-documents*))
    (dolist (claim (claims-in (merge-pathnames name *root*)))
      (destructuring-bind (line form text prose) claim
        (incf checked)
        (flet ((broke (why)
                 (push (format nil "~a:~d~%      claim  ~a~%      ~a~%      guarding  ~a"
                               name line text why
                               (if (> (length prose) 64)
                                   (concatenate 'string (subseq prose 0 61) "...")
                                   prose))
                       broken)))
          (cond ((and (consp form) (eq :unreadable (car form)))
                 (broke (format nil "does not read: ~a" (second form))))
                ((claim-vocabulary-complaint form)
                 (broke (claim-vocabulary-complaint form)))
                (t
                 (multiple-value-bind (value condition)
                     (handler-case (values (eval form) nil)
                       (error (c) (values nil c)))
                   (cond (condition (broke (format nil "signalled: ~a" condition)))
                         ((null value) (broke "is false")))
                   ;; Whether it passed or failed.  A claim that answers the
                   ;; question correctly and changes the image on its way past
                   ;; is the worse of the two, because nothing downstream would
                   ;; report anything at all.
                   (let* ((now (image-fingerprint))
                          (differences (fingerprint-difference fingerprint now)))
                     (when differences
                       (incf moved)
                       (setf fingerprint now)
                       (broke (format nil "moved the image: ~{~a~^, ~}~%      ~
                                           ~4tA claim is read to be asked, not to be run.  ~
                                           Gates 14, 15~%      ~4tand 17 ask this image ~
                                           afterwards and would have~%      ~4tanswered ~
                                           from what this left behind."
                                      differences)))))))))))
  (format t "  #+CLAIM: sentences checked~46t~d~%~
             ~4tof those, that changed the image~46t~d~%" checked moved)
  (if broken
      (fail 12 "~d claim~:p the program contradicts:~{~%    ~a~}~%~
                ~4tThe sentence and the program disagree.  Fix whichever is~%~
                ~4twrong -- and if it is the sentence, that is the whole~%~
                ~4treason this gate exists."
            (length broken) (reverse broken))
      (format t "  every claim the prose pins is still true~%")))

;;; --------------------------------------------------------------- gate 13

(banner 13 "every extension property is written and read")
;; THE SAME BUG AS *SMART-GAPS*, ONE MECHANISM OVER, AND IT HAD SHIPPED TWICE.
;;
;; PROPS is the third way to keep state in this program.  An option is
;; registered and gate 11 asks whether anything reads it; a hook is declared and
;; gate 7 asks whether anything runs it; a property key is neither registered nor
;; declared -- it is a keyword somebody types in two places, and until this gate
;; nothing compared the two.  Both halves of the tree had a key where the second
;; place was missing:
;;
;;   * :LATTICE/VIEWPORT-BOUNDS.  Read by LATTICE's CLIP-RECT, written by
;;     nothing, so the method returned CALL-NEXT-METHOD's `clip nothing' every
;;     time it was called.  That method is river's set_content_clip_box, which
;;     DESIGN calls the best find in the protocol; it is the cropped trailing
;;     cell that :FIXED zoom is *for*; it is a row in FINDINGS' list of the
;;     generics the lattice overrides.  It could not fire.
;;
;;   * :OVERLAY.  Read by the core's RENDER-ORDER as a third render tier above
;;     the floats, written by nothing, because everything anybody would call an
;;     overlay here is a *surface* rather than a node and is not in a render
;;     list at all.  A documented tier with no way in.
;;
;; Neither is a typo and that is the point: both are a keyword that was read
;; where it should also have been written, which no compiler and no test notices
;; because PROP's contract is that an absent key is NIL.  The failure mode is
;; always the same shape -- a method or a clause that runs, does nothing, and
;; agrees with the documentation.
;;
;; ASK THE READER, NOT THE TEXT.  Gate 11 asks SBCL's cross-reference data,
;; which cannot be done here: keywords are constants, not variables, and
;; WHO-REFERENCES answers NIL for every one of them.  So this reads the source --
;; but with READ rather than SEARCH, which is the whole difference.  A key is
;; found by *symbol identity* against LATTICEWM/CORE:PROP, so c:prop, r::prop and
;; a package-local nickname nobody has invented yet are all the same function; a
;; docstring that mentions :LATTICE/VIEWPORT-BOUNDS is a string and not a form,
;; so it cannot make the gate pass; and the SETF pairs are separated from the
;; reads structurally rather than by looking for "(setf " to the left.
;;
;; ONE DIRECTION IS A FAILURE AND THE OTHER IS A REPORT, and the asymmetry is
;; the argument, not a compromise.  A key that is *read* and never written is
;; provably dead code: PROP answers NIL, the branch behind it never runs, and
;; nothing ever signals.  There is no arrangement of the program in which that is
;; what somebody meant.  A key that is *written* and never read is a different
;; animal, because PROPS is D20's published interface for exactly this -- state
;; hung on an object the writer does not own, for a reader the core does not know
;; about.  :PID is the fact river's unreliable_pid event carries; a status bar
;; that wants it reads it from outside.  Failing on that would be a gate ordering
;; the program to grow a consumer, which is how a check starts writing the
;; design.  So it is printed, every build, in full: visible enough that a key
;; nobody can name a reader for gets noticed, and honest about which of the two
;; questions it is answering.
;;
;; WHAT IT CANNOT SEE, SAID PLAINLY.  A key computed at runtime -- (prop node
;; key) where KEY is a variable -- is invisible to a scan of literals.  One
;; function in the tree does that, NODE-WINDOW-PROP, and it is named in
;; *PROP-ACCESSORS* below rather than special-cased: an accessor that forwards a
;; key has to be listed or the reads through it are unseen, which is the same
;; bargain gate 12 makes about classifying a document.  Being absent from the
;; list cannot make the gate quieter, only louder -- an unlisted forwarder's keys
;; look unread, and unread is the half that fails.
;;
;; TESTS AND TOOLS ARE NOT THE PROGRAM.  The scan is src/, lattice/ and
;; examples/: the core, the one shipped extension and the four worked examples.
;; A key written only by a test is not written, which is the same ruling gate 11
;; makes about options and for the same reason -- the suite is allowed to reach
;; into places the program does not, and a check that counted it would be
;; satisfiable from the file that is supposed to be doing the checking.
(defparameter *prop-writers*
  '((setf . :pairs) (psetf . :pairs)
    (push . :second) (pushnew . :second)
    (pop . :first) (incf . :first) (decf . :first) (remf . :first))
  "Operators that write through a place, and which argument the place is.

:PAIRS is every other argument, SETF-style.  The rest take exactly one place,
PUSH and PUSHNEW second and the others first.

A place is counted as a write and *not* as a read, including for the
read-modify-writes: (PUSH x (PROP node :k)) does consult the old list, but a key
that is only ever pushed onto is not a key anything in the program looks at, and
calling that a read would make the report below meaningless.")

(defparameter *prop-accessors*
  (list (cons (sym "latticewm/core:prop") 2)
        (cons (sym "latticewm/policy:node-window-prop") 2))
  "Every function that takes a property key, and where the key sits in the call.

The number is the position in the form, counting the operator as zero -- so 2 is
the second argument, which is where both of these take it.

PROP is the accessor; NODE-WINDOW-PROP forwards to it, and it is the bridge a
window rule's colour crosses to reach an appearance generic that was handed a
*node* rather than a window.  Its reads are real reads and this is where the gate
is told so.")

(defun prop-key-of (form)
  "The literal property key FORM accesses, or NIL if it is not a literal access."
  (let ((position (and (consp form) (symbolp (car form))
                       (cdr (assoc (car form) *prop-accessors*)))))
    (when position
      (let ((argument (nth position form)))
        (and (keywordp argument) argument)))))

(defun prop-write-places (form)
  "The subforms of FORM that are in a write position."
  (let ((rule (and (consp form) (symbolp (car form))
                   (cdr (assoc (car form) *prop-writers*)))))
    (case rule
      (:pairs (loop for tail on (cdr form) by #'cddr collect (car tail)))
      (:second (and (consp (cdr form)) (consp (cddr form)) (list (third form))))
      (:first (and (consp (cdr form)) (list (second form))))
      (t '()))))

(defun prop-sites (path)
  "Two lists: the property keys PATH reads, and the ones it writes.

*PACKAGE* follows PATH's own IN-PACKAGE forms, which is what makes reading the
file the right instrument: every abbreviation the file uses for the core --
c:prop here, prop inside latticewm/core itself -- resolves to the same symbol
without this gate having to know the nicknames exist.

Two passes over the same conses, because a write position has to be *subtracted*
from the reads: the place in (SETF (PROP node :k) v) is a PROP form like any
other, and the only thing that distinguishes it is the form it is nested in.  The
first pass remembers the place cells themselves, and the second skips them by
identity."
  (let ((reads '()) (writes '()) (places '()))
    (labels ((collect-writes (form)
               (when (consp form)
                 (dolist (place (prop-write-places form))
                   (let ((key (prop-key-of place)))
                     (when key
                       (pushnew key writes)
                       (push place places))))
                 (collect-writes (car form))
                 (collect-writes (cdr form))))
             (collect-reads (form)
               (when (consp form)
                 (let ((key (prop-key-of form)))
                   (when (and key (not (member form places :test #'eq)))
                     (pushnew key reads)))
                 (collect-reads (car form))
                 (collect-reads (cdr form)))))
      (with-open-file (in path)
        (let ((*package* *package*)
              (*read-eval* nil))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (when (and (consp form) (symbolp (car form))
                              (string= (symbol-name (car form)) "IN-PACKAGE"))
                     (let ((package (find-package (second form))))
                       (when package (setf *package* package))))
                   (collect-writes form)
                   (collect-reads form)))))
    (values reads writes)))

(let ((reads (make-hash-table))
      (writes (make-hash-table))
      (files (append (directory "src/**/*.lisp")
                     (directory "lattice/*.lisp")
                     (directory "examples/*.lisp"))))
  (dolist (path files)
    (handler-case
        (multiple-value-bind (read-keys written-keys) (prop-sites path)
          (dolist (key read-keys) (pushnew (relative path) (gethash key reads)
                                           :test #'string=))
          (dolist (key written-keys) (pushnew (relative path) (gethash key writes)
                                              :test #'string=)))
      (error (condition)
        ;; A file this cannot read is a gate that cannot run, not a gate that
        ;; passes.  Reader conditionals and every abbreviation in the tree read
        ;; fine; anything that does not is worth hearing about immediately.
        (fail 13 "~a does not read: ~a" (relative path) condition))))
  (let ((unwritten '()) (write-only '()) (keys '()))
    (maphash (lambda (key files) (declare (ignore files)) (pushnew key keys)) reads)
    (maphash (lambda (key files) (declare (ignore files)) (pushnew key keys)) writes)
    (dolist (key keys)
      (unless (gethash key writes)
        (push (format nil "~(~a~) -- read in ~{~a~^, ~}" key (gethash key reads))
              unwritten))
      (unless (gethash key reads)
        (push (format nil "~(~a~) -- written in ~{~a~^, ~}" key (gethash key writes))
              write-only)))
    (format t "  files scanned~46t~d~%  property keys~46t~d~%~
               ~2twritten, and read by nothing here~46t~d~%~
               ~2tread, and written by nothing at all~46t~d~%"
            (length files) (length keys) (length write-only) (length unwritten))
    ;; The report half.  Printed in full rather than counted, because the whole
    ;; use of it is that a reader can look at a name and ask who wants it.
    (dolist (row (sort write-only #'string<))
      (format t "    published, nothing here reads it: ~a~%" row))
    (when unwritten
      (fail 13 "~d propert~:@p the program reads and nothing writes:~{~%    ~a~}~%~
                ~4tThe read is always NIL, so whatever depends on it never~%~
                ~4thappens -- and it never *fails* either, because an absent~%~
                ~4tkey is a legal answer.  Write it, or delete the read and~%~
                ~4tthe behaviour it was standing in for.  A method whose whole~%~
                ~4tbody is CALL-NEXT-METHOD is a method the documentation~%~
                ~4tdescribes and the program does not have."
            (length unwritten) (sort unwritten #'string<)))
    (unless unwritten
      (format t "  every key the program reads is written by something~%"))))

;;; ---------------------------------------------------------------- gate 14

(banner 14 "every declared hook is attached to by something that runs it")
;; FOURTEEN OF THE SEVENTEEN HOOKS HAD NEVER BEEN ATTACHED TO BY ANYTHING.
;; Not by the lattice, not by the four worked examples, not by a test.  They
;; were declared, documented, run from the right line of the right file, and no
;; function had ever been on the other end of one.
;;
;; Gate 7 is the check that was supposed to see this and structurally cannot.
;; It compares two sets of *names* -- declared here, run there -- and both sets
;; are written by the same person in the same afternoon.  What it establishes
;; is that a hook exists.  What nobody had established is that it works, and a
;; hook's contract is not its name: it is the arguments the functions are
;; called with and the moment they are called.  Neither had ever been executed.
;;
;; THREE OF THEM WERE WRONG, and each is the kind of wrong only a consumer
;; finds.  :STARTUP ran before the compositor connection existed, which its own
;; docstring denied.  :OUTPUT-ADDED ran before the monitor had a name, a size
;; or a scale, which is everything "a surface or a process per screen" needs.
;; :FOCUS-CHANGED, documented "run after the cursor moves", was skipped by two
;; of the five places that move the cursor -- one of them being closing a
;; window, which is the commonest focus change there is.  All three passed
;; gates 1, 2, 7 and 12, the whole unit suite and the whole integration run,
;; because nothing was listening.
;;
;; SO: SOMETHING MUST BE LISTENING, and the three places that can listen are
;; the three tiers of thing a hook needs in order to fire at all.
;;
;;   tests/                     what can be established by constructing state
;;   tools/integration.lisp     what is only true once a compositor agreed
;;   tools/hardware-check.lisp  what needs a real machine: a keyboard being
;;                              unplugged, an xkb layout toggle, a monitor
;;                              going away
;;
;; The third tier is not a loophole, it is the honest answer for the four hooks
;; a headless backend cannot fire -- WLR_LIBINPUT_NO_DEVICES=1 means no input
;; device is ever announced, so :INPUT-ADDED cannot be reached by the harness
;; that reaches everything else.  Attaching them in the session recorder is
;; also the right thing for that tool independently: which devices were found
;; and when a layout changed are exactly what a hardware report is for.
;;
;; THIS IS THE STATIC HALF AND IT SAYS SO.  What it establishes is that every
;; hook is *named in the code* of a suite -- not in a comment or a docstring,
;; since it reads through CODE-OF, so the paragraph you are reading cannot
;; satisfy it, but a name is still only a name.  It is here to catch hook
;; nineteen being declared and nobody touching the suites, which is exactly how
;; the first eighteen got where they were.
;;
;; The dynamic half is in tools/integration.lisp, where it has to be: a
;; recorder sits on every declared hook for the whole run, +WATCHED-HOOKS+ is
;; checked against ALL-HOOKS so the table cannot fall behind the program, every
;; firing is checked against the declared argument count, and a hook that
;; neither fired nor is named as one a headless backend cannot fire is a
;; failure there.  Neither half is sufficient and both are cheap.
(flet ((named-hooks (path)
         "Every literal keyword in PATH's code, lowercased and deduplicated."
         (let ((found '()))
           (dolist (line (code-lines path) found)
             (let ((code (cdr line)) (at 0))
               (loop for hit = (position #\: code :start at)
                     while hit
                     do (let ((stop (or (position-if-not
                                         (lambda (c)
                                           (or (alphanumericp c)
                                               (member c '(#\- #\/))))
                                         code :start (1+ hit))
                                        (length code))))
                          ;; A package marker, not a keyword: p:add-hook has a
                          ;; name to the left of the colon and :focus-changed
                          ;; does not.
                          (when (and (> stop (1+ hit))
                                     (or (zerop hit)
                                         (not (or (alphanumericp (char code (1- hit)))
                                                  (char= #\: (char code (1- hit)))))))
                            (pushnew (string-downcase (subseq code (1+ hit) stop))
                                     found :test #'string=))
                          (setf at (max (1+ hit) stop)))))))))
  (let* ((sources (append (sort (directory (merge-pathnames "tests/*.lisp" *root*))
                                #'string< :key #'namestring)
                          (list (merge-pathnames "tools/integration.lisp" *root*)
                                (merge-pathnames "tools/hardware-check.lisp" *root*))))
         (exercised (make-hash-table :test #'equal))
         (declared (call "latticewm/policy:all-hooks")))
    (dolist (path sources)
      (when (probe-file path)
        (dolist (name (named-hooks path))
          (pushnew (relative path) (gethash name exercised) :test #'string=))))
    (let ((unexercised '()))
      (dolist (row declared)
        (let ((name (string-downcase (symbol-name (first row)))))
          (unless (gethash name exercised)
            (push name unexercised))))
      (format t "  ~d declared, ~d exercised by a suite, ~d with a function ~
                 attached in this image~%"
              (length declared)
              (- (length declared) (length unexercised))
              (count-if #'plusp declared :key #'third))
      (if unexercised
          (fail 14 "declared, and named by no suite that runs it: ~{:~a~^ ~}~%~
                    ~4tA hook nobody has ever pulled has an argument list and~%~
                    ~4ta firing moment that are guesses.  Three of the first~%~
                    ~4tfourteen to be checked were wrong.  Attach it in tests/~%~
                    ~4tif constructing state can reach it, in~%~
                    ~4ttools/integration.lisp if it needs a real compositor,~%~
                    ~4tand in tools/hardware-check.lisp if it needs real~%~
                    ~4thardware -- and assert on what it was handed."
                (sort unexercised #'string<))
          (format t "  every hook is named, attached to and watched by one of ~
                     the three suites~%")))))

;;; ---------------------------------------------------------------- gate 15

(banner 15 "an override that takes an option away offers one of its own")
;; GATE 11 CHECKED THAT SOMEBODY READS THE OPTION.  IT CANNOT CHECK THAT THE
;; READ HAPPENS.
;;
;; The rule this project settled on is one sentence: the generic is the
;; extension point, the option is what its shipped method returns, and *a
;; policy that overrides that method stops reading the option*.  Gate 11 and
;; OPTION-READERS made the first two clauses facts about the image.  The third
;; stayed prose, and it is the clause with a user on the other end of it.
;;
;; It is not hypothetical.  Load the lattice and MAKE-WORKSPACE is answered by
;; a method that never reaches the shipped one, so *NEW-WORKSPACE* --
;; registered, documented, settable, printed by --list-options, and certified
;; as read by gate 11, which is right, because the method that reads it still
;; exists and simply never runs -- decides nothing.  Every instrument the
;; project owns said that option worked.  The single statement of the truth was
;; the last paragraph of the option's own docstring, and doc/EXTENDING.org,
;; PLAN.org §generics and README all rotted this way first.
;;
;; SO ASK THE METHOD LIST, WHICH IS THE THING THAT KNOWS.  An override is a
;; primary method at least as narrow as the reader everywhere and narrower
;; somewhere; a *total* override narrows only the policy argument, so it
;; applies wherever the reader applied.  OPTION-SHADOWS computes both from the
;; image and the generated surface prints them under every option, so the
;; answer to "I set it and my policy ignored it" is now a fact about the
;; running program.
;;
;; WHAT IS LEFT IS THE PART THE IMAGE CANNOT ANSWER.  Whether an override
;; composes -- whether CALL-NEXT-METHOD is in it -- is not recorded on a
;; method: SBCL keeps no flag and the compiled body has no constant to find.
;; It is in the source, so this reads the source, and reads the *form* the
;; compiler recorded for that method rather than grepping the file, because a
;; grep for CALL-NEXT-METHOD in lattice/policy.lisp hits eleven methods and a
;; paragraph of prose.
;;
;; AND THE RULE IT ENFORCES IS A TRADE, NOT A BAN.  Overriding wholesale is
;; legitimate -- the lattice's workspace is a plane, always, and that is the
;; single method that makes the Z axis exist.  What is not legitimate is doing
;; it and leaving the user with nothing: they set the documented knob and the
;; program ignored them.  So a total override either composes, and the option
;; still reaches, or it reads an option of its own, and the decision is still
;; tier-0 configurable by somebody who will never write a DEFMETHOD.  The
;; lattice pays this: it takes *NEW-WORKSPACE* and gives *NEW-WORKSPACE-CELLS*.
;;
;; THIS ONE PASSED THE DAY IT WAS WRITTEN, which is worth saying plainly rather
;; than dressing up.  The other eight added gates each failed first.  What this
;; found was not a broken method but an invisible fact: one shipped option that
;; the flagship extension silently switches off, printed nowhere, checkable by
;; nothing.  It is a ratchet against the sixteenth generic being overridden by
;; the second extension with no knob offered and nobody noticing for a month.
(flet ((toplevel-form (path index)
         "The INDEXth toplevel form of PATH, read in the package it declares.

Not a grep.  SB-INTROSPECT records which form of which file each method came
from, so this reads that form and nothing else -- prose in the file, and the
ten other methods around it, cannot answer for this one."
         (with-open-file (in path :external-format :utf-8)
           (let ((*package* *package*)
                 (*read-eval* nil))
             (loop for position from 0
                   for form = (handler-case (read in nil :eof) (error () :eof))
                   until (eq form :eof)
                   do (when (and (consp form) (eq (first form) 'cl:in-package))
                        (setf *package* (or (find-package (second form)) *package*)))
                      (when (= position index) (return form))))))
       (calls-p (tree name)
         "True when TREE contains a written call to NAME.

NOT `THE SYMBOL APPEARS SOMEWHERE IN THE FORM', WHICH IS WHAT THIS WAS.  The
old test was (STRING= (SYMBOL-NAME FORM) NAME) at every node of the tree, with
no package check and no evaluation-position check -- so '(CALL-NEXT-METHOD)
quoted, a keyword written :CALL-NEXT-METHOD, or the symbol sitting in a data
literal all answered a question about whether the method *composes*.  The
failing half of this gate was one apostrophe away from unreachable, and the
remedy the gate prints -- add CALL-NEXT-METHOD -- was satisfiable by adding it
somewhere it could never run.

Operator position, or after #', and QUOTE subtrees are not walked at all.

WHAT IT STILL CANNOT SEE, said plainly rather than left to be discovered:
(WHEN NIL (CALL-NEXT-METHOD)) is a written call that never runs, and deciding
otherwise is reachability analysis this file has no business attempting.  The
bar it now holds is `you wrote the call', which is the bar a reader of the
method would apply; the bar it held before was `you wrote the word'."
         (labels ((named-p (form)
                    (and form (symbolp form) (string= (symbol-name form) name)))
                  (walk-list (rest)
                    (and (consp rest)
                         (or (walk (car rest)) (walk-list (cdr rest)))))
                  (walk (form)
                    (and (consp form)
                         (not (eq (car form) 'cl:quote))
                         (or (named-p (car form))
                             (and (eq (car form) 'cl:function)
                                  (named-p (second form)))
                             ;; ((LAMBDA ...) ...) and every nested form.
                             (walk (car form))
                             (walk-list (cdr form))))))
           (walk tree)))
       (specializer-names (method)
         (mapcar (lambda (specializer)
                   (typecase specializer
                     (closer-mop:eql-specializer
                      (list 'eql (closer-mop:eql-specializer-object specializer)))
                     (class (class-name specializer))
                     (t specializer)))
                 (closer-mop:method-specializers method))))
  (let* ((rows (call "latticewm/policy:all-options"))
         (reads (make-hash-table :test #'equal))
         (total 0)
         (composing 0)
         (traded '())
         (offenders '()))
    ;; Every (GENERIC SPECIALIZERS) that reads some option, and which.
    (dolist (row rows)
      (dolist (reader (call "latticewm/policy:option-readers" (first row)))
        (when (and (consp reader) (= 3 (length reader)))
          (push (second row) (gethash (list (second reader) (third reader)) reads)))))
    (dolist (row rows)
      (dolist (shadow (call "latticewm/policy:option-shadows" (first row)))
        (destructuring-bind (generic method totalp) shadow
          (when totalp
            (incf total)
            (let* ((source (ignore-errors (sb-introspect:find-definition-source method)))
                   (path (and source (sb-introspect:definition-source-pathname source)))
                   (index (and source (first (sb-introspect:definition-source-form-path
                                              source))))
                   (form (and path index (probe-file path)
                              (ignore-errors (toplevel-form path index))))
                   (composes (and form (or (calls-p form "CALL-NEXT-METHOD")
                                           (calls-p form "NEXT-METHOD-P"))))
                   (offered (gethash (list generic (specializer-names method)) reads))
                   (name (call "latticewm/policy:option-shadow-name" shadow)))
              (cond (composes (incf composing))
                    ((null form)
                     (fail 15 "no source form for ~a, so whether it composes ~
                               cannot be established.~%~
                               ~4tThe gate reads the form SB-INTROSPECT recorded ~
                               for the method;~%~4tif the sources are not beside ~
                               the image it cannot run at all." name))
                    (offered
                     (push (list (second row) name (sort (copy-list offered) #'string<
                                                         :key #'symbol-name))
                           traded))
                    (t (push (list (second row) name) offenders))))))))
    (format t "  options~46t~d~%  total overrides of a reader~46t~d~%~
               ~4tof those, composing~46t~d~%~4tof those, trading one option ~
               for another~46t~d~%"
            (length rows) total composing (length traded))
    (dolist (trade (sort traded #'string< :key (lambda (row) (symbol-name (first row)))))
      (format t "    ~(~a~) is not read under ~a~%~14tit offers ~{~(~a~)~^, ~} instead~%"
              (first trade) (second trade) (third trade)))
    (when offenders
      (fail 15 "~d option~:p an override switches off without replacing:~
                ~{~%    ~(~a~) is not read under ~a~}~%~
                ~4tThe method wins wherever the reader applied and never calls~%~
                ~4tCALL-NEXT-METHOD, so the option decides nothing under that~%~
                ~4tpolicy -- while --list-options prints it, the generated~%~
                ~4tsurface documents it and gate 11 certifies it as read.~%~
                ~4tThat is *smart-gaps* again with a longer fuse.~%~
                ~4tCompose with CALL-NEXT-METHOD if the shipped answer is~%~
                ~4tstill wanted, or read an option of your own so the~%~
                ~4tdecision stays tier-0 for somebody who will never write a~%~
                ~4tDEFMETHOD.  Overriding wholesale is allowed; doing it~%~
                ~4tsilently is not."
            (length offenders)
            (loop for (option name) in (sort offenders #'string<
                                             :key (lambda (row) (symbol-name (first row))))
                  append (list option name))))))

;;; --------------------------------------------------------------- gate 16

(banner 16 "every name the published packages export is one somebody can reach")
;; AN EXPORT IS A PROMISE, AND TWO OF THEM WERE PROMISES THE IMAGE COULD NOT
;; KEEP.  REPLACE-CHILD was exported from LATTICEWM/CORE and defined nowhere at
;; all -- a DEFGENERIC that does not exist, on the advertised container
;; protocol.  It was found by reading, fixed by hand, and nothing was put in
;; the way of the next one; WORLD-PROPS was already the next one, sitting beside
;; MAKE-WORLD in the same export list, left behind when the accessor was renamed
;; to PROPS and shared with NODE.
;;
;; The failure mode is specific and it is not a compile error.  EXPORT interns
;; the symbol, so a config file saying (c:world-props *world*) reads perfectly
;; and dies at run time with an undefined function -- which looks like a bug in
;; the user's config rather than a hole in ours.  A name on a published surface
;; is either something or it is a trap.
;;
;; AND FIVE MORE WERE DEFINED, EXPORTED, AND REACHED BY NOTHING: TREE-INSERT-AT,
;; WEIGHT-AT, SET-WEIGHT, NORMALIZED-WEIGHTS and AXIS-OF.  No caller in src/,
;; lattice/ or examples/; no test; no line of any document.  AXIS-OF is the one
;; that says what this is: its whole docstring was "Alias for DIRECTION-AXIS,
;; for call sites that read better this way", and there were no call sites.
;;
;; WHY NO EXISTING GATE COULD SEE IT.  Gate 2 asks whether a generic is
;; documented, gate 6 how much behaviour is answered from outside src/, gate 11
;; whether an option is read, gate 13 whether a property key is written, gate 14
;; whether a hook is attached to.  Every one of them is a question about a
;; *mechanism* -- a generic, an option, a prop, a hook -- and each mechanism has
;; a registry the gate can enumerate.  A plain exported function belongs to no
;; mechanism and appears in no registry.  It is a line somebody typed in
;; package.lisp, and until this gate the only thing that had ever checked that
;; line against the program was a person reading it.
;;
;; TWO QUESTIONS, AND THE SECOND IS THE EXPENSIVE ONE.
;;
;;   1. Does the export name anything?  A function, a value, a class or a type.
;;      Nothing at all is a failure, always, with no exemption -- there is no
;;      arrangement of a program in which a published name that denotes nothing
;;      is what somebody meant.
;;
;;   2. Does anything reach it?  A caller in the program, a test, a build tool,
;;      or a sentence in a document somebody could read and then call it from a
;;      config file.  None of the four is dead by every definition available:
;;      not used, not tested, not documented, not findable.
;;
;; TWO INSTRUMENTS, BECAUSE THE FOUR PLACES ARE NOT ALIKE.  src/, lattice/ and
;; examples/ are READ, the way gate 13 reads them: a symbol is matched by
;; *identity*, so c:axis-of, r::axis-of, AXIS-OF inside LATTICEWM/CORE itself
;; and a package-local nickname nobody has invented yet are all one name, and a
;; docstring discussing AXIS-OF is a string rather than a symbol and so cannot
;; answer for code that does not exist.  A form's own name is subtracted from
;; what that form mentions, which is what stops a recursive call, a RETURN-FROM
;; and a DECLAIM from counting as somebody wanting the function -- LAST-LEAF-PATH
;; calls itself and TREE-TRANSPLANT returns from itself, and neither is a reader.
;;
;; tests/, tools/ and the documents are searched as *text*, which is gate 14's
;; bargain and taken here for gate 14's reason: the test packages need fiveam,
;; the gates must run without it, and a name is still a name.  tests/ is read
;; through CODE-OF so the paragraph you are reading could not satisfy it.  tools/
;; is not, and that is deliberate: gate 2 reaches UNDOCUMENTED-GENERICS through
;; the string "latticewm/policy:undocumented-generics", because a tool that runs
;; before the system is loaded cannot name its symbols, and blanking the strings
;; would make this gate blind to the gate above it.
;;
;; The named cost of that: a comment in tools/ that merely mentions a dead name
;; keeps it alive here.  That is a gate that can be made quieter by prose and
;; never louder, which is the acceptable direction -- and lamdan/ is excluded
;; from the documents for exactly this reason, since a critique naming a
;; function as dead must not be the thing that saves it.
;;
;; THE .org FILES ARE SEARCHED FOR MARKED-UP NAMES ONLY, AND THAT IS NOT A
;; REFINEMENT, IT IS WHAT MADE THE FAILING HALF REACHABLE.  A space counts as a
;; name boundary, and src/package.lisp exports thirty-two ordinary English
;; words -- WINDOW, CLOSE, FLOAT, FOCUS, MOVE, SPLIT, TAB, TAG, UNDO, KEY,
;; NODE, WORLD.  So one sentence containing the word `window' answered for the
;; export WINDOW, and about thirty published names could not be reported dead
;; however dead they were.  Org marks code as ~name~ or =name=; a document
;; answers for a name when it says the name as a name.  See TEXT-OF's :PROSE.
;;
;; WHAT IS EXEMPT FROM QUESTION 2, EACH FOR A DIFFERENT REASON:
;;
;;   generics -- an uncalled generic is the extension surface doing its job, and
;;     how much of it anybody answers is gate 6's question, asked with a number.
;;   commands -- reached by *name* from a keymap and from M-x, so having no
;;     caller is the normal and correct state for all seventy-four of them.
;;   names DEFSTRUCT and DEFCLASS generate -- RECT-P arrives with MAKE-RECT and
;;     RECT-X as a set, and asking a project to write (:predicate nil) to quiet a
;;     gate is the gate writing the design.  SBCL is asked rather than the source
;;     guessed at: it records the DEFSTRUCT-DESCRIPTION a name came from.
;;   variables -- gate 11 already asks whether an option is read, and it asks it
;;     better, from the cross-reference rather than from a scan.
;;
;; THE TWO PUBLISHED PACKAGES, AND NOT THE THIRD.  LATTICEWM/CORE and
;; LATTICEWM/POLICY are the surface DESIGN names -- "the lattice depends only on
;; the core's exported policy package" -- and they are what doc/EXTENSION-SURFACE
;; and doc/CONTAINER-SURFACE describe.  LATTICEWM/RUNTIME is not published in
;; that sense and its exports are overwhelmingly commands and protocol wrappers,
;; which are covered by gate 2 and gate 5 and would arrive here as seventy-seven
;; false positives.  Extending this to it means answering for those first.
;;
;; THE ASYMMETRY IS GATE 13'S, FOR GATE 13'S REASON.  Reached by nothing is a
;; failure.  Reached only by a suite or only by a document is *printed*: these
;; two packages exist to be called from outside this repository, so a model
;; function the program itself never needs is exactly what a published API looks
;; like -- and a reader who can see the list can ask, of any line on it, who it
;; is for.
(flet ((program-mentions (path table)
         "Record every interned symbol PATH's forms name, minus the ones they define."
         (with-open-file (in path)
           (let ((*package* *package*)
                 (*read-eval* nil))
             (labels ((defined (form)
                        (let ((head (and (consp form) (symbolp (car form))
                                         (symbol-name (car form)))))
                          (when (and head (eql 0 (search "DEF" head)))
                            (let ((name (second form)))
                              (cond ((symbolp name) (list name))
                                    ((and (consp name) (eq (car name) 'cl:setf))
                                     (list (second name)))
                                    ;; (DEFSTRUCT (RECT (:CONSTRUCTOR ...)) ...)
                                    ((and (consp name) (symbolp (car name)))
                                     (list (car name)))
                                    (t '()))))))
                      (skip-p (form)
                        ;; A DECLAIM names a function to talk about its type, not
                        ;; to call it, and the FTYPE proclamations in model/
                        ;; sit beside the very DEFUNs this is asking about.
                        (and (consp form) (symbolp (car form))
                             (member (symbol-name (car form))
                                     '("DECLAIM" "PROCLAIM") :test #'string=)))
                      (walk (form defined)
                        (cond ((and form (symbolp form))
                               ;; #:FOO in a DEFPACKAGE reads as an uninterned
                               ;; symbol, so the export list cannot be the thing
                               ;; that answers for an export.  Nothing to special
                               ;; case: an uninterned symbol has no package.
                               (when (and (symbol-package form)
                                          (not (member form defined)))
                                 (pushnew (relative path) (gethash form table)
                                          :test #'string=)))
                              ((consp form) (walk (car form) defined)
                                            (walk (cdr form) defined)))))
               (loop for form = (read in nil :eof)
                     until (eq form :eof)
                     do (when (and (consp form) (eq (first form) 'cl:in-package))
                          (setf *package* (or (find-package (second form)) *package*)))
                        (unless (skip-p form)
                          (walk form (defined form))))))))
       (text-of (path mode)
         "PATH's text: :CODE with comments and strings gone, :RAW as written,
:PROSE as only the parts of a document that are marked up as code, :TOOL as
code plus the string literals that are wholly a qualified name.

:PROSE IS THE ONE THAT MADE THE FAILING HALF OF THIS GATE REACHABLE AGAIN.
The document search treats a space as a name boundary, and src/package.lisp
exports thirty-two ordinary English words -- WINDOW, CLOSE, FLOAT, FOCUS, MOVE,
SPLIT, TAB, TAG, UNDO, KEY, NODE, WORLD and the rest.  So any sentence in any
.org file containing the word `window' answered for the export WINDOW, and
roughly thirty published names could not be reported dead however dead they
were.  The gate's own preamble says a document counts because it is `a sentence
somebody could read and then call it from a config file' -- and a sentence
about a window is not that sentence.

Org already draws the distinction: ~name~ and =name= are code, everything else
is prose, and this tree writes them consistently.  So a document answers for a
name when it says the name *as a name* -- inside those, inside a source or
example block, or on a #+ line, which is where the CLAIMs live.  Marking up a
name is one character either side, and it is what the surrounding paragraph
needed anyway.

.txt and roff keep the whole text: the generated surfaces and the man pages are
reference material where a bare word is already a name, and they are generated
from the image rather than written, so they cannot drift into flattering it.

:TOOL is the whole reason this is not just CODE-OF, and it is narrow on
purpose.  Gate 2 reaches the undocumented-generic readers through strings like
\"latticewm/policy:all-options\", because a tool that runs before the system is
loaded cannot name its symbols -- so blanking every string in tools/ makes this
gate blind to the gate above it.  Keeping every string instead makes it blind to
what it is for: the first draft of this gate kept them, and the five dead names
in its own preamble came back reported as reached by a build tool.

So a string counts only when the *entire* string is one package-qualified name.
That is exactly what the CALL and SYM idiom writes and it is not a shape prose
can fall into: a sentence mentioning latticewm/core:tree-move has a space in it,
and this one is the proof, since it does not answer for TREE-MOVE."
         (handler-case
             (case mode
               (:code (format nil "~{~a~^~%~}" (mapcar #'cdr (code-lines path))))
               (:raw (with-open-file (in path :external-format :utf-8)
                       (let ((buffer (make-string (file-length in))))
                         (subseq buffer 0 (read-sequence buffer in)))))
               (:prose
                (with-open-file (in path :external-format :utf-8)
                  (let ((out (make-string-output-stream))
                        (in-block nil))
                    (loop for line = (read-line in nil) while line
                          do (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
                               (cond
                                 ((eql 0 (search "#+" trimmed))
                                  (write-line line out)
                                  (let ((keyword (string-upcase trimmed)))
                                    (cond ((search "BEGIN_" keyword) (setf in-block t))
                                          ((search "END_" keyword) (setf in-block nil)))))
                                 (in-block (write-line line out))
                                 (t
                                  ;; ~code~ and =verbatim=, org's own two ways of
                                  ;; saying "this is a name and not a word".
                                  (dolist (marker '(#\~ #\=))
                                    (let ((at 0))
                                      (loop for open = (position marker line :start at)
                                            for close = (and open (position marker line
                                                                           :start (1+ open)))
                                            while close
                                            do (write-line (subseq line (1+ open) close) out)
                                               (setf at (1+ close))))))))
                          finally (return (get-output-stream-string out))))))
               (t
                (let* ((text (with-open-file (in path :external-format :utf-8)
                               (let ((buffer (make-string (file-length in))))
                                 (subseq buffer 0 (read-sequence buffer in)))))
                       (end (length text))
                       (names '())
                       (index 0))
                  ;; CODE-OF's walk, kept only for the string bodies: the #\;
                  ;; and #\\ clauses are here so that a semicolon inside a string
                  ;; does not start a comment and a #\" literal does not open one.
                  (loop while (< index end)
                        do (let ((c (char text index)))
                             (cond
                               ((char= c #\\) (incf index 2))
                               ((char= c #\;)
                                (setf index (or (position #\Newline text :start index)
                                                end)))
                               ((char= c #\")
                                (let ((stop (loop with i = (1+ index)
                                                  while (< i end)
                                                  do (cond ((char= (char text i) #\\)
                                                            (incf i 2))
                                                           ((char= (char text i) #\")
                                                            (return i))
                                                           (t (incf i)))
                                                  finally (return end))))
                                  (let ((body (subseq text (1+ index) (min stop end))))
                                    (when (and (find #\: body)
                                               (every (lambda (ch)
                                                        (or (alphanumericp ch)
                                                            (find ch "-*+%/&:")))
                                                      body))
                                      (push body names)))
                                  (setf index (min end (1+ stop)))))
                               (t (incf index)))))
                  (format nil "~{~a~^~%~}~%~{~a~^~%~}"
                          (mapcar #'cdr (code-lines path)) names))))
           (error () "")))
       (names-token-p (text token)
         "True when TEXT contains TOKEN as a whole name rather than as a part.

The boundary set includes the constituents of a Lisp name -- so ADJUST-WEIGHT
does not answer for WEIGHT and latticewm/policy does not answer for POLICY --
but not the colon, because latticewm/policy:undocumented-generics and
c:axis-of are how a name is written when it is genuinely being used."
         (let ((n (length token))
               (end (length text)))
           (loop for at = (search token text) then (search token text :start2 (1+ at))
                 while at
                 do (let ((before (and (plusp at) (char text (1- at))))
                          (after (and (< (+ at n) end) (char text (+ at n)))))
                      (flet ((constituent (c)
                               (and c (or (alphanumericp c) (find c "-*+%/&")))))
                        (unless (or (constituent before) (constituent after))
                          (return t))))))))
  (let* ((packages (remove nil (list (find-package "LATTICEWM/CORE")
                                     (find-package "LATTICEWM/POLICY"))))
         (program (make-hash-table))
         (elsewhere '())
         (missing '())
         (unreached '())
         (published '())
         (scanned 0))
    ;; The program, by symbol identity.
    (dolist (path (append (directory (merge-pathnames "src/**/*.lisp" *root*))
                          (directory (merge-pathnames "lattice/*.lisp" *root*))
                          (directory (merge-pathnames "examples/*.lisp" *root*))))
      (incf scanned)
      (handler-case (program-mentions path program)
        (error (condition)
          ;; A file this cannot read is a gate that cannot run, not a gate that
          ;; passes -- the same ruling gate 13 makes about the same files.
          (fail 16 "~a does not read: ~a" (relative path) condition))))
    ;; Everything else, as text: (region . text) pairs, searched once per name.
    (dolist (path (directory (merge-pathnames "tests/*.lisp" *root*)))
      (incf scanned)
      (push (cons "a suite" (string-downcase (text-of path :code))) elsewhere))
    (dolist (path (directory (merge-pathnames "tools/*.lisp" *root*)))
      (incf scanned)
      (push (cons "a build tool" (string-downcase (text-of path :tool)))
            elsewhere))
    (dolist (path (append (directory (merge-pathnames "*.org" *root*))
                          (directory (merge-pathnames "doc/*.org" *root*))))
      (incf scanned)
      (push (cons "a document" (string-downcase (text-of path :prose))) elsewhere))
    (dolist (path (append (directory (merge-pathnames "doc/*.txt" *root*))
                          (directory (merge-pathnames "doc/latticewm.1" *root*))
                          (directory (merge-pathnames "doc/latticewm-config.5" *root*))))
      (incf scanned)
      (push (cons "a document" (string-downcase (text-of path :raw))) elsewhere))
    (dolist (package packages)
      (do-external-symbols (symbol package)
        (when (eq (symbol-package symbol) package)
          (push symbol published)
          (let ((function (and (fboundp symbol) (not (macro-function symbol))
                               (fdefinition symbol))))
            (cond
              ;; Question 1.
              ((not (or (fboundp symbol) (boundp symbol) (find-class symbol nil)
                        (sb-int:info :type :kind symbol)))
               (push symbol missing))
              ;; Question 2, once the exempt kinds are out of the way.
              ((or (null (or function (macro-function symbol)))
                   (typep function 'generic-function)
                   (call "latticewm/policy:find-command" symbol)
                   ;; SBCL records the DEFSTRUCT a generated name came from as
                   ;; the source transform it installed for it.
                   (consp (ignore-errors
                           (nth-value 0 (sb-int:info :function :source-transform
                                                     symbol))))))
              ((gethash symbol program))
              (t
               ;; Not used by the program.  Whatever else names it decides
               ;; between the report and the failure, and nothing does both.
               (let ((token (string-downcase (symbol-name symbol))))
                 (push (cons symbol
                             (remove-duplicates
                              (loop for (region . text) in elsewhere
                                    when (names-token-p text token) collect region)
                              :test #'string= :from-end t))
                       unreached))))))))
    (let ((dead (remove-if #'cdr unreached))
          (outward (remove-if-not #'cdr unreached)))
      (format t "  files scanned~46t~d~%  names published~46t~d~%~
                 ~2tpublished, and the program does not use them~46t~d~%~
                 ~2treached by nothing at all~46t~d~%"
              scanned (length published) (length outward) (length dead))
      ;; The report half, in full rather than counted, because its whole use is
      ;; that a reader can look at a name and ask who it is for.
      (dolist (row (sort outward #'string< :key (lambda (r) (symbol-name (car r)))))
        (format t "    reached only by ~{~a~^ and ~}: ~(~a~)~%" (cdr row) (car row)))
      (when missing
        (fail 16 "~d export~:p that name~:[s~;~] nothing at all:~{~%    ~(~a~)~}~%~
                  ~4tEXPORT interns the symbol, so (~(~a~) ...) in a config~%~
                  ~4tfile reads perfectly and dies at run time with an~%~
                  ~4tundefined function -- which reads as a bug in the~%~
                  ~4tuser's config rather than a hole in ours.  Define it,~%~
                  ~4tor take the line out of package.lisp."
              (length missing) (rest missing)
              (sort (mapcar #'symbol-name missing) #'string<)
              (first (sort (copy-list missing) #'string< :key #'symbol-name))))
      (when dead
        (fail 16 "~d published function~:p reached by nothing:~{~%    ~(~a~)~}~%~
                  ~4tNo caller in src/, lattice/ or examples/; no test; no~%~
                  ~4tbuild tool; no line of any document.  It is not used, not~%~
                  ~4texercised, and not findable, so the surface advertises~%~
                  ~4tsomething nobody has ever run.  Delete it, or give it the~%~
                  ~4tone reader that makes it real -- a caller, a test, or a~%~
                  ~4tsentence in doc/EXTENDING.org telling somebody it is there."
              (length dead)
              (sort (mapcar (lambda (row) (symbol-name (car row))) dead) #'string<)))
      (unless (or missing dead)
        (format t "  every published name is defined and reachable~%")))))

;;; --------------------------------------------------------------- gate 17

(banner 17 "an option named after a generic is that generic's shipped answer")
;; THE ONE THING ABOUT AN OPTION THAT WAS STILL ONLY TRUE BY COINCIDENCE.
;;
;; Five options are named after a policy generic: *GAPS* and GAPS,
;; *BORDER-WIDTH* and BORDER-WIDTH, *KEYS-HINT* and KEYS-HINT,
;; *MOVE-INTO-OCCUPIED* and MOVE-INTO-OCCUPIED, *NEW-CHILD-SIDE* and
;; NEW-CHILD-SIDE.  lamdan/ read that as two mechanisms answering one question
;; -- "resolved by which one you found first" -- and asked for the options to
;; be deleted, on the ground that the generic was always the real extension
;; point.  Half of that is right and it is the half that is not the remedy.
;;
;; THE PROJECT HAS SINCE RULED THE OTHER WAY, and the rule is better than the
;; deletion.  An option is not a competing answer; it is what a generic's
;; shipped method returns.  policy/hooks.lisp states it, OPTION-READERS makes
;; the first half a fact about the image, OPTION-SHADOWS makes the second half
;; one, and gate 15 requires a policy that overrides a method wholesale to ship
;; an option of its own so the decision stays tier-0 for somebody who will
;; never write a DEFMETHOD.  Under that rule these five pairs are not a hedge,
;; they are the rule's worked examples, and deleting them would take four P1
;; design forks -- DESIGN's "where a fork is situational rather than
;; principled, ship both" -- away from exactly the user the rule protects.
;;
;; WHAT WAS ACTUALLY UNCHECKED IS THE SHARED NAME.  *GAPS* is read by
;; GAPS (LAYOUT-POLICY T) today, so the two names mean one thing, and nothing
;; anywhere required that.  Move the read into a helper, answer the option from
;; a different generic, or rename the method's job out from under it, and the
;; program would hold a generic and an identically-named option with no
;; relationship at all -- and every gate would pass, because gate 11 only asks
;; that *something* reads the option and gate 15 only asks about methods that
;; override a reader.  A user reads GAPS, sets *GAPS*, and is wrong for a
;; reason no instrument can see.
;;
;; THAT IS THIS PROJECT'S OWN RECURRING BUG, TWICE OVER.  *SMART-GAPS* was a
;; documented value wired to nothing and *NEW-WORKSPACE* was a reader that
;; never ran; both were invisible until the relationship between two
;; independently maintained artifacts was asked about rather than assumed.  A
;; shared name is that same kind of assumption, written in the one place a
;; reader is most likely to trust it.
;;
;; SO ASK THE IMAGE.  OPTIONS-BY-GENERIC is OPTION-READERS read backwards: for
;; every policy generic, the options its *methods* read.  An option whose name
;; matches a generic has to appear under that generic.  No file can be moved to
;; satisfy it and no docstring can argue with it.
;;
;; IT PASSES THE DAY IT IS WRITTEN, like gate 15 and unlike the other nine, and
;; it is worth saying plainly rather than dressing up: what it defends is not a
;; bug that was found but the meaning of a name, which is the thing this
;; project has watched rot in every other medium it keeps.  It is also why the
;; generated surface now prints `answered from' under each generic -- the check
;; and the document are the same fact, and a reader should not have to run the
;; build to learn it.
;;
;; IT RUNS AFTER GATE 6, for gate 11's reason: the lattice and the four worked
;; examples are loaded by then, so a method one of them contributes counts.
(defparameter *shared-name-floor* 5
  "Options named after a policy generic.  See the floor check below.

At the number, like gate 6's floors and for the same reason: this one exists
because the gate's own remedy is a way out of the gate, so the population it
enumerates has to be a thing somebody is made to argue about before it
shrinks.")
(let* ((rows (call "latticewm/policy:all-options"))
       (generics (call "latticewm/policy:policy-generics"))
       (by-generic (call "latticewm/policy:options-by-generic"))
       (names (mapcar #'symbol-name generics))
       (pairs 0)
       (orphans '()))
  (dolist (row rows)
    (let* ((variable (second row))
           (bare (string-trim "*" (symbol-name variable)))
           (generic (find bare generics :key #'symbol-name :test #'string=)))
      (when (member bare names :test #'string=)
        (if (member variable (gethash generic by-generic))
            (incf pairs)
            (push (list variable generic) orphans)))))
  (format t "  options named after a generic~46t~d  (floor ~d)~%"
          (+ pairs (length orphans)) *shared-name-floor*)
  (format t "  and read by a method on it~46t~d~%" pairs)
  ;; THE REMEDY THIS GATE PRINTS IS ALSO THE WAY OUT OF IT, WHICH IS WHY THERE
  ;; IS A FLOOR UNDER THE POPULATION.  The failure message ends "rename one of
  ;; them" -- and renaming the *option* empties the set this gate enumerates,
  ;; so it prints `options named after a generic 0' and passes, with the user
  ;; left in exactly the state the preamble above calls the failure: an option
  ;; and a generic that are the same decision under two names, with nothing
  ;; saying so.  A gate whose remedy can be applied to the gate is a gate that
  ;; measures whether anybody has been annoyed by it lately.
  ;;
  ;; So the convention has a floor as well as a rule.  Five options are named
  ;; after a policy generic and each of them is a real relationship; dropping
  ;; below that is a decision about the convention, which is a thing to argue
  ;; with in a commit message rather than a thing to do quietly while making a
  ;; red build green.
  (when (< (+ pairs (length orphans)) *shared-name-floor*)
    (fail 17 "~d option~:p named after a policy generic, against a floor of ~d.~%~
              ~4tThe convention is that an option and the generic it is named~%~
              ~4tafter are one decision seen twice -- set the value or write~%~
              ~4tthe method, and you know which wins.  This gate enumerates~%~
              ~4tthe options that make that claim, and its own remedy ends~%~
              ~4t`rename one of them', which empties the population and~%~
              ~4tpasses.  If the convention is being retired, retire it here~%~
              ~4tand say so; do not let it evaporate one rename at a time."
          (+ pairs (length orphans)) *shared-name-floor*))
  (if orphans
      (fail 17 "~d option~:p named after a policy generic that no method on ~
                that generic reads:~{~%    ~(~a~) / ~(~a~)~}~%~
                ~4tThe shared name is a claim: that the option is what this~%~
                ~4tgeneric's shipped method returns, so a user can set the~%~
                ~4tvalue or write the method and knows which wins.  Nothing~%~
                ~4telse in the program says so.  If the relationship is real,~%~
                ~4tread the option from a method on the generic.  If it is~%~
                ~4tnot, rename one of them -- two unrelated things under one~%~
                ~4tname is the *SMART-GAPS* failure with a docstring agreeing."
            (length orphans)
            (loop for (variable generic) in (sort (copy-list orphans) #'string<
                                                  :key (lambda (o)
                                                         (symbol-name (first o))))
                  collect variable collect generic))
      (format t "  every shared name is one relationship~%")))

;;; --------------------------------------------------------------- gate 18

(banner 18 "a DEFAULTS- file names an algorithm that defines no methods")
;; THE ONLY FILE-LAYOUT RULE THIS TREE ACTUALLY KEEPS, AND IT WAS KEPT BY LUCK.
;;
;; policy/motion.lisp is the walk up the ancestor chain and defines no methods
;; at all; policy/defaults-motion.lisp is the set of answers that walk asks the
;; shipped containers for.  That pair is predictable from outside: a reader
;; looking for STEP-ADDRESS does not have to know what step-address is *about*,
;; only that it is an answer, and the prefix says which file holds answers.
;;
;; policy/defaults-lifecycle.lisp wore the same prefix and was not the same
;; thing.  It held SHOULD-FLOAT-P, DEFAULT-FLOAT-RECT, WINDOW-CAPABILITIES and
;; DECORATION-MODE; policy/lifecycle.lisp held five more methods on the same
;; protocol class, and the rule offered for the split was that one file was
;; "about a window's properties" and the other "about tree surgery".  That is a
;; sort by subject, and subject is exactly what a reader cannot predict: it put
;; WINDOW-RULE-FOR -- whose entire job is setting a window's properties -- in
;; the other file, which the properties file then needed a comment to explain.
;; The two files are now one, and the sections inside it are the sort.
;;
;; WHY A GATE AND NOT JUST THE MERGE.  The merge fixes the instance.  What made
;; the instance possible is that DEFAULTS- was decoration: no check, nothing
;; written down, and a name that reads as meaningful in a tree where it means
;; something once.  The next file to take the prefix takes it from the same
;; place, which is the file list in policy/conventional.lisp's header -- and
;; that header had already drifted to "the methods are in five files" over a
;; list of seven.  A convention nobody is made to keep is a convention a reader
;; is misled by, which is the same failure as *SMART-GAPS* one medium over.
;;
;; WHAT IT CANNOT SEE, SAID PLAINLY.  It cannot tell a good decomposition from
;; a bad one in general, and it does not try -- INPUT-POLICY's shipped answers
;; live in six files for six good reasons and no rule this file could state
;; would separate those from the lifecycle pair.  What it can do is hold the
;; one rule that *is* stated: the prefix is a promise about the shape of the
;; pair, and the promise is now checkable.  It passed the day it was written,
;; like gates 15 and 17.
;;
;; IT READS SOURCE, NOT THE IMAGE, for gate 13's reason: which file a method is
;; in is a fact about the text, and SB-INTROSPECT would answer with a truename
;; either way.  CODE-LINES blanks comments and strings first, so the paragraph
;; above cannot satisfy or trip it.
(defparameter *defaults-pair-floor* 1
  "Algorithm/answers pairs the tree is expected to have.  See the floor below.")
(let ((pairs '())
      (problems '()))
  (dolist (path (sort (append (directory "src/**/*.lisp")
                              (directory "lattice/*.lisp"))
                      #'string< :key #'namestring))
    (let ((name (pathname-name path)))
      (when (and (> (length name) 9) (string= "defaults-" name :end2 9))
        (let* ((stem (subseq name 9))
               (engine (merge-pathnames (make-pathname :name stem :type "lisp")
                                        path)))
          (cond
            ((not (probe-file engine))
             (push (format nil "~a has no ~a.lisp beside it" (relative path) stem)
                   problems))
            (t
             (let ((methods (loop for (number . code) in (code-lines engine)
                                  when (search "(defmethod " code)
                                    collect number)))
               (if methods
                   (push (format nil "~a defines ~d method~:p, at line~p ~{~d~^, ~}"
                                 (relative engine) (length methods)
                                 (length methods) methods)
                         problems)
                   (push (cons (relative engine) (relative path)) pairs)))))))))
  (dolist (pair (reverse pairs))
    (format t "    ~a~36tanswered by ~a~%" (car pair) (cdr pair)))
  (format t "  algorithm/answers pairs~46t~d  (floor ~d)~%"
          (length pairs) *defaults-pair-floor*)
  ;; AND A FLOOR UNDER THE POPULATION, BECAUSE THE POPULATION IS FILENAMES.
  ;; Everything above enumerates files literally called defaults-*.lisp, and
  ;; the merge that prompted this gate took the count from two to one.  Rename
  ;; the last one and the gate governs the empty set: no problems, no pairs,
  ;; `algorithm/answers pairs 0', pass -- forever, with the convention gone and
  ;; the check that was supposed to keep it still printing a green line.  A
  ;; gate whose subject can be renamed out from under it is a gate about
  ;; whether anybody has renamed anything.
  ;;
  ;; The floor is the convention itself: there is one pair, it is the one this
  ;; gate was written for, and its disappearance is a decision about how the
  ;; policy layer is filed.  That decision is allowed -- it just has to be made
  ;; out loud, by lowering a number in this file, rather than by a rename that
  ;; leaves every instrument saying yes.
  (when (< (length pairs) *defaults-pair-floor*)
    (fail 18 "~d algorithm/answers pair~:p, against a floor of ~d.~%~
              ~4tThis gate's population is files named defaults-*.lisp, so~%~
              ~4trenaming the last of them makes it govern nothing and pass~%~
              ~4tfor good.  If the DEFAULTS- convention is being retired,~%~
              ~4tretire it here and say why; the prefix means something in~%~
              ~4ta tree where it means something once."
          (length pairs) *defaults-pair-floor*))
  (if problems
      (fail 18 "~d DEFAULTS- file~:p that is not the answers half of a pair:~
                ~{~%    ~a~}~%~
                ~4tThe prefix says the file beside it is an *algorithm* -- one~%~
                ~4tthat asks the questions these methods answer and defines no~%~
                ~4tmethods itself, the way policy/motion.lisp does.  A pair~%~
                ~4tsorted any other way cannot be predicted from outside, so~%~
                ~4tthe reader who wants to override one of these has two files~%~
                ~4tand a coin.  Merge them, or rename the file after what it~%~
                ~4tactually holds."
            (length problems) (sort problems #'string<))
      (format t "  every DEFAULTS- file is the answers half of one~%")))

;;; --------------------------------------------------------------- gate 19

(defun spells-p (spelling line)
  "True when LINE writes SPELLING as a word rather than inside a longer one.

`twenty' IS A SUBSTRING OF `twenty-one', and this gate's table holds both.  The
plain SEARCH that used to be here reported every document as saying `twenty
gates' the moment the twenty-first gate arrived and the documents were updated
to say twenty-one -- eight failures, all of them the gate misreading a sentence
that was correct.  A hyphen is part of a number word here, so it counts as a
word constituent on both sides."
  (flet ((constituent (character)
           (and character (or (alpha-char-p character) (char= character #\-)))))
    (let ((from 0))
      (loop for at = (search spelling line :start2 from)
            while at
            do (setf from (1+ at))
               (let ((before (and (plusp at) (char line (1- at))))
                     (after (let ((end (+ at (length spelling))))
                              (and (< end (length line)) (char line end)))))
                 (unless (or (constituent before) (constituent after))
                   (return t)))))))

(banner 19 "the project says the same thing about itself everywhere")
;; THE THIRD COPY IS THE ONE THAT REACHES A STRANGER, AND IT WAS THE WRONG ONE.
;;
;; PLAN.org records fixing `:license' in both .asd files: they said
;; BSD-3-Clause and were simply wrong.  Both were corrected.  flake.nix's
;; `meta.license' — the copy that becomes what `nix search' prints, the copy a
;; distribution packager reads, the copy that decides what licence an incoming
;; contribution arrives under — said `licenses.bsd3' beside a LICENSE file
;; holding 674 lines of GPLv3, and stayed wrong for as long as the other two
;; had been right.
;;
;; The species is gate 9's, one level up and stated in its own comment: *a
;; number repeated in two files is the FINDINGS §census failure*.  A licence
;; repeated in four is the same failure in a currency where being wrong is a
;; legal fact rather than a stale one.  Nothing here can pick the right answer
;; — only that all four agree, which is the whole of what was missing.
;;
;; The .asd values come from the live image rather than from the text, because
;; ASDF has already read them and a second reader would be a second thing to be
;; wrong.  flake.nix and LICENSE are read as text: there is no nix here, and the
;; declaration is one line naming an attribute in `pkgs.lib.licenses'.
(flet ((line-containing (path text)
         (when (probe-file path)
           (with-open-file (in path)
             (loop for line = (read-line in nil) while line
                   when (search text line) return line)))))
  (let* ((problems '())
         ;; SPDX, which is what a .asd carries, against the nixpkgs attribute
         ;; name, which is what a flake carries.  Only the licences this project
         ;; could plausibly be under; an answer outside the table is reported as
         ;; unrecognised rather than assumed to agree, because a table that
         ;; silently passes what it does not know is gate 17's disease.
         (spdx->nix '(("GPL-3.0-or-later" . "gpl3Plus")
                      ("GPL-3.0-only"     . "gpl3Only")
                      ("BSD-3-Clause"     . "bsd3")
                      ("MIT"              . "mit")))
         (core (asdf:system-license (asdf:find-system "latticewm")))
         (extension (asdf:system-license (asdf:find-system "lattice")))
         (flake (line-containing "flake.nix" "license = licenses."))
         (declared (and flake
                        (let ((start (+ (search "license = licenses." flake)
                                        (length "license = licenses."))))
                          (string-right-trim
                           ";" (string-trim " " (subseq flake start))))))
         (expected (cdr (assoc core spdx->nix :test #'string=))))
    (format t "  latticewm.asd~46t~a~%" core)
    (format t "  lattice.asd~46t~a~%" extension)
    (format t "  flake.nix~46tlicenses.~a~%" (or declared "(none declared)"))
    (unless (equal core extension)
      (push (format nil "latticewm.asd says ~s and lattice.asd says ~s"
                    core extension)
            problems))
    (unless expected
      (push (format nil "latticewm.asd's ~s is not an SPDX identifier this ~
gate knows how to check against a nixpkgs attribute; add it to the table here ~
rather than leaving the flake unchecked"
                    core)
            problems))
    ;; ASKED ONLY WHEN THERE IS A FLAKE, for gate 9's reason: §packaging's test
    ;; is that deleting flake.nix leaves a project that still builds.
    (when (probe-file "flake.nix")
      (cond ((null declared)
             (push (format nil "flake.nix declares no meta.license, so the ~
packaged copy tells `nix search' nothing about a project that ships 674 lines ~
of licence text")
                   problems))
            ((and expected (not (string= declared expected)))
             (push (format nil "flake.nix says licenses.~a and the .asd files ~
say ~s, which is licenses.~a"
                           declared core expected)
                   problems))))
    ;; And the text itself, because three declarations agreeing with each other
    ;; and not with the file is the same failure with better manners.
    (let ((header (line-containing "LICENSE" "GENERAL PUBLIC LICENSE")))
      (cond ((not (probe-file "LICENSE"))
             (push "there is no LICENSE file" problems))
            ((and (search "GPL-3.0" (or core "")) (null header))
             (push (format nil "the .asd files say ~s and LICENSE does not ~
look like the GNU General Public License" core)
                   problems))))
    ;; THE VERSION, WHICH WAS WRITTEN OUT IN FOUR PLACES AND TAGGED AS A FIFTH.
    ;;
    ;; latticewm.asd, lattice.asd, flake.nix and the .TH line of each man page
    ;; all said 0.1.0, and the sole git tag said v0.1 and matched none of them.
    ;; main.lisp's --version was the only reader that got it right, because it
    ;; asked ASDF rather than holding a copy.
    ;;
    ;; All of them read VERSION now, so most of this gate is checking that they
    ;; still do rather than that they agree -- except the man pages, which are
    ;; roff and have no way to read a file, so for those two it is the whole
    ;; check.  A roff page whose .TH says one version while --version says
    ;; another is the same defect as the flake's licence: the copy a stranger
    ;; reads disagreeing with the copy the program knows.
    (let* ((declared (with-open-file (in (merge-pathnames "VERSION" *root*)
                                         :if-does-not-exist nil)
                       (and in (string-trim '(#\Space #\Tab #\Return)
                                            (or (read-line in nil) "")))))
           (core-version (asdf:component-version (asdf:find-system "latticewm")))
           (extension-version (asdf:component-version (asdf:find-system "lattice"))))
      (format t "  VERSION~46t~a~%" (or declared "(missing)"))
      (cond
        ((null declared)
         (push "there is no VERSION file for anything to read" problems))
        ((string/= declared core-version)
         (push (format nil "VERSION says ~s and latticewm.asd resolves to ~s, ~
so something is holding a copy again" declared core-version)
               problems))
        ((string/= declared extension-version)
         (push (format nil "VERSION says ~s and lattice.asd resolves to ~s"
                       declared extension-version)
               problems)))
      ;; The three comparisons above are the file against itself: both .asd
      ;; files resolve :version by READING VERSION, so DECLARED and the resolved
      ;; versions are always the same text and the STRING/= can never fire.  An
      ;; .asd that hardcoded a stale literal would sail through.  So assert the
      ;; thing that actually matters -- that each still reads the file rather
      ;; than holding a copy -- exactly as the flake.nix check below does.
      (dolist (asd '("latticewm.asd" "lattice.asd"))
        (when (probe-file asd)
          (unless (line-containing asd ":read-file-line")
            (push (format nil "~a no longer reads VERSION with :read-file-line, ~
so its version agrees with VERSION only by luck" asd)
                  problems))))
      ;; flake.nix must *read* it rather than agree with it: a literal that
      ;; happens to match today is the state this was in yesterday.
      (when (probe-file "flake.nix")
        (let ((literal (line-containing "flake.nix" "version = \"")))
          (when (and literal (not (search "pname" literal)))
            (push (format nil "flake.nix writes a version literal (~a); it ~
should read VERSION, the way it already reads river's version out of PINNED"
                          (string-trim " " literal))
                  problems)))
        (unless (line-containing "flake.nix" "./VERSION")
          (push "flake.nix never reads VERSION" problems)))
      ;; AND THE CHANGELOG, WHICH SAID THIS GATE ALREADY HELD IT.
      ;;
      ;; CHANGELOG.md's own header reads "gate 19 holds the man pages and this
      ;; file to the same number".  It held the man pages.  Nothing in this
      ;; file had ever opened CHANGELOG.md, so the sentence describing the
      ;; check was itself the thing no check could see -- which is gate 12's
      ;; disease occurring inside gate 19's documentation.
      ;;
      ;; What is asked is the weakest thing worth asking: that the version in
      ;; VERSION has a heading of its own.  Not that it is the first heading,
      ;; because `## Unreleased' is above it by design, and not that the
      ;; entries under it say anything in particular, because that is review's
      ;; job and not a gate's.  The release procedure at the top of that file
      ;; puts the heading in at step 2 and runs `make check' at step 3, so this
      ;; fires exactly when a version has been bumped and not written down.
      (when declared
        (let ((heading (format nil "## ~a" declared)))
          (cond
            ((not (probe-file "CHANGELOG.md"))
             (push "there is no CHANGELOG.md" problems))
            ((not (line-containing "CHANGELOG.md" heading))
             (push (format nil "VERSION says ~s and CHANGELOG.md has no ~s ~
heading, so a version has been bumped without being written down -- see the ~
release procedure at the top of that file"
                           declared heading)
                   problems)))))
      ;; And the two roff pages, which cannot read anything.
      (when declared
        (dolist (page '("doc/latticewm.1" "doc/latticewm-config.5"))
          (let ((header (line-containing page ".TH ")))
            (cond
              ((null header)
               (push (format nil "~a has no .TH line" page) problems))
              ((not (search (format nil "LatticeWM ~a" declared) header))
               (push (format nil "~a's .TH line is not \"LatticeWM ~a\": ~a"
                             page declared (string-trim " " header))
                     problems)))))))

    ;; AND THE OTHER NUMBER THIS PROJECT REPEATS IN PROSE: HOW MANY GATES.
    ;;
    ;; "Eighteen gates run on every build" is asserted in README.org, in
    ;; INSTALL.org three times, and in the CI workflow's own header comment.
    ;; Nothing read any of them, so adding a gate silently made five sentences
    ;; false — which is the *SMART-GAPS* shape one level up: a documented fact
    ;; wired to nothing, in the documents this project stakes its record on.
    ;;
    ;; The count comes from this file's own text rather than from a variable,
    ;; because a variable is a nineteenth place to be wrong.  Gate 1 lives in
    ;; tools/build.lisp and is counted here because every document counts it.
    (let* ((banners (with-open-file (in "tools/gates.lisp")
                      (loop for line = (read-line in nil) while line
                            count (eql 0 (search "(banner " line)))))
           (total (1+ banners))              ; gate 1 runs during the load
           (spelled '((16 . "sixteen") (17 . "seventeen") (18 . "eighteen")
                      (19 . "nineteen") (20 . "twenty") (21 . "twenty-one")
                      (22 . "twenty-two") (23 . "twenty-three")
                      (24 . "twenty-four") (25 . "twenty-five")))
           (word (cdr (assoc total spelled)))
           ;; "Gate 14 is the one the other eighteen could not ask" is a claim
           ;; about the count too, and it is the count minus the gate doing the
           ;; asking.  It was stale by one before this check existed, which is
           ;; the argument for spending four lines on the exception rather than
           ;; excusing the whole sentence shape.
           (others (cdr (assoc (1- total) spelled)))
           ;; The live claims only.  PLAN.org is a session log and FINDINGS.org
           ;; is a record of what was true when it was written; both are
           ;; append-only by discipline and correcting them would be laundering
           ;; the record, which DESIGN.org:101 exists to demonstrate not doing.
           ;; CONTRIBUTING.md and the PR template are on this list from the
           ;; day they were written, because a first contributor is exactly
           ;; the reader who cannot tell a stale number from a true one, and
           ;; the number they are being told is how many ways their branch can
           ;; fail.
           ;; doc/ONBOARDING.org is on this list for CONTRIBUTING.md's reason
           ;; one step further in: it is the page somebody reads in their second
           ;; week, when they have started to rely on the numbers in it and have
           ;; not yet read enough of the tree to doubt one.
           (documents '("README.org" "INSTALL.org" "bootstrap.sh"
                        "CONTRIBUTING.md" "doc/ONBOARDING.org"
                        ".github/PULL_REQUEST_TEMPLATE.md"
                        ".github/workflows/check.yml")))
      (format t "  gates that run~46t~d~%" total)
      (if (null word)
          (push (format nil "~d gates and no spelling for it in this gate's ~
table; add one, or the documents cannot be checked" total)
                problems)
          (dolist (path documents)
            (when (probe-file path)
              (with-open-file (in path)
                (loop for line = (read-line in nil)
                      for number from 1
                      while line
                      do (let ((lower (string-downcase line)))
                           (dolist (entry spelled)
                             (let ((spelling (cdr entry)))
                               (when (and (spells-p spelling lower)
                                          (search "gate" lower)
                                          (not (string= spelling word))
                                          (not (and (search "other" lower)
                                                    (equal spelling others))))
                                 (push (format nil
                                               "~a:~d says ~s gates and ~d run"
                                               path number spelling total)
                                       problems))))))))))
      ;; And the one that is not prose: the verdict line has to be able to name
      ;; every gate that failed, so a gate whose number is not in the banner
      ;; sequence would be reported as a gate nobody can find.
      (unless (= banners (length (remove-duplicates
                                  (with-open-file (in "tools/gates.lisp")
                                    (loop for line = (read-line in nil) while line
                                          when (eql 0 (search "(banner " line))
                                            collect (subseq line 8
                                                            (position #\Space line
                                                                      :start 8)))))))
        (push "two gates share a number" problems)))
    (if problems
        (fail 19 "~{~%    ~a~}~%~
                  ~4tThe copy that is wrong is usually the one nobody edits,~%~
                  ~4tand that is the packaging metadata rather than the~%~
                  ~4tsystem definition -- so it is also the copy a distribution~%~
                  ~4tand a first contributor actually read."
              (reverse problems))
        (format t "  the declarations, the LICENSE text and the gate count agree~%"))))

;;; --------------------------------------------------------------- gate 20

(banner 20 "the file the project calls the deliverable contains what it says")
;; THE GATE THE SOURCE SAID EXISTED.
;;
;; src/policy/protocol.lisp opens with the first substantive sentence a stranger
;; sent to "the deliverable" reads:
;;
;;     This file contains generic functions and their docstrings.  It contains
;;     no methods, and that rule is enforced by a build gate.
;;
;; There was no such gate.  `grep -rn protocol.lisp tools/ tests/ Makefile
;; .github/' returned nothing at all: no instrument in the project read that
;; file's shape, and the rule had been true only for as long as nobody broke
;; it.  This is *SMART-GAPS* -- a documented rule wired to nothing, which gate
;; 11 exists to abolish -- in the one file handed to strangers, and it was
;; invisible because gate 12 reads .org, .1 and .5 while this claim lives in a
;; .lisp comment.  Gate 12 reads docstrings now and still would not have caught
;; it: a file header is not a docstring either.
;;
;; THE RULE IS WORTH KEEPING, WHICH IS WHY THIS IS THE GATE AND NOT A DELETION.
;; The whole argument of the file is that reading it end to end tells you
;; everything the window manager can be talked out of.  A method in it is a
;; default hiding in the catalogue -- the reader would have to know that some
;; of these paragraphs are also behaviour, and could no longer tell which by
;; looking.  conventional.lisp exists to be the other half.
;;
;; AND THE OPENING CLAUSE WAS ALREADY STRETCHED.  "Generic functions and their
;; docstrings" was never the whole truth: the file carries the six protocol
;; classes, the MOP introspection that answers POLICY-GENERICS, and the
;; specials that the shipped methods read.  So the census is printed rather
;; than only the violation, and the header now names what is in there.  A count
;; nobody prints is a count that drifts, which is this project's oldest lesson
;; about itself.
(let* ((path (merge-pathnames "src/policy/protocol.lisp" *root*))
       (text (code-of path))
       (kinds '("defmethod" "defgeneric" "defclass" "defun" "defmacro"
                "defvar" "defparameter"))
       (census '()))
  (dolist (kind kinds)
    (let ((marker (format nil "(~a " kind))
          (count 0)
          (start 0))
      (loop for at = (search marker text :start2 start :test #'char-equal)
            while at
            do (setf start (+ at (length marker)))
               (incf count))
      (push (cons kind count) census)))
  (setf census (nreverse census))
  (dolist (row census)
    (format t "  ~a~46t~d~%" (car row) (cdr row)))
  (let ((methods (cdr (assoc "defmethod" census :test #'string=))))
    (if (plusp methods)
        (fail 20 "~d method~:p in src/policy/protocol.lisp:~%~
                  ~4tIts own header says it has none, and the reason is the~%~
                  ~4treason the file exists: it is the catalogue of what the~%~
                  ~4twindow manager can be talked out of, and a default hiding~%~
                  ~4tin it is a paragraph that is also behaviour with nothing~%~
                  ~4tto tell a reader which.  The shipped answers go in~%~
                  ~4tconventional.lisp, as methods on CONVENTIONAL-POLICY."
              methods)
        (format t "  the catalogue is a catalogue: no methods answer here~%")))
  ;; The positive half of the banner, which "no methods" alone does not cover:
  ;; a protocol.lisp gutted to zero DEFGENERICs has no methods either and would
  ;; pass green.  The file's reason to exist is the generics and the six
  ;; protocol classes, so assert they are actually present.
  (let ((generics (cdr (assoc "defgeneric" census :test #'string=)))
        (classes (cdr (assoc "defclass" census :test #'string=))))
    (when (zerop generics)
      (fail 20 "src/policy/protocol.lisp declares no generic functions:~%~
                ~4tan empty catalogue is a deleted protocol, not a passing one."))
    (when (< classes 6)
      (fail 20 "src/policy/protocol.lisp carries ~d protocol class~:p, fewer ~
than the six~%~4tthe file promises to hold."
            classes))))

;;; --------------------------------------------------------------- gate 21

(banner 21 "the packaged build runs the build, and does not restate it")
;; GATE 9 WITH THE OTHER PHASE, AND THE OTHER PHASE IS WHERE IT WAS NEEDED.
;;
;; flake.nix's installPhase used to hand-roll a second list of what ships,
;; beside install.sh's, and the two had drifted apart everywhere; gate 9 fixed
;; that and stands over it, with six references to `installPhase' in this file.
;; Directly *above* it in the same file, buildPhase was five hand-typed
;;
;;     sbcl --non-interactive --load tools/build.lisp
;;     sbcl --non-interactive --load tools/gates.lisp
;;     ...
;;
;; lines under a comment reading "The same steps `make check` runs."  Zero
;; references to `buildPhase' anywhere.  One phase healed and the second list
;; left standing three lines away, with the gate written to enforce "one list"
;; aimed at the half that no longer needed it.
;;
;; THE DRIFT THIS PREVENTS IS NOT HYPOTHETICAL EITHER.  `make check' is
;; `build gates test integration' and it has changed twice in this project's
;; life.  A step added to the Makefile would not have reached the packaged
;; build, which is the path that produces the store path a user actually
;; installs -- so `nix build' would have gone on passing a shorter check than
;; the one the README tells contributors to run, and the comment three lines up
;; would have gone on saying they were the same.
;;
;; ASKED ONLY WHEN THERE IS A FLAKE, on gate 9's argument exactly: §packaging's
;; test is that deleting flake.nix leaves a project that still builds, so a
;; gate that failed on its absence would assert the dependency the file exists
;; to deny.
(let ((phase (nix-phase "buildPhase"))
      (problems '()))
  (cond
    ((not (probe-file "flake.nix"))
     (format t "  no flake.nix, so there is one build and it is the Makefile~%"))
    ((null phase)
     (push (format nil "flake.nix has no buildPhase, so this gate no longer ~
knows what a packaged build runs")
           problems))
    (t
     (format t "  buildPhase lines~46t~d~%" (length phase))
     (unless (some (lambda (line) (search "make " line)) phase)
       (push (format nil "flake.nix's buildPhase does not run make, so the ~
packaged build has its own idea of what building is and nothing compares the ~
two")
             problems))
     (let ((handrolled
             (remove-if-not
              (lambda (line)
                (some (lambda (verb) (search verb line))
                      '("sbcl " "--load tools/" "asdf:load-system")))
              phase)))
       (when handrolled
         (push (format nil "flake.nix's buildPhase runs the build itself:~
~{~%      ~a~}~%    Every one of these is a step the Makefile already names. ~
Add it there and let this phase call it, or the two lists drift and only one ~
of them is what anybody runs"
                       (mapcar (lambda (line) (string-trim " " line)) handrolled))
               problems)))))
  (if problems
      (fail 21 "~{~%    ~a~}" (reverse problems))
      (when phase
        (format t "  the packaged build delegates to the one that is checked~%"))))

;;; --------------------------------------------------------------- gate 22

(banner 22 "every request goes through the one door, and the door is checked")
;; 462 LINES OF WRAPPER WHOSE ONLY READER WAS THE GATE THAT COUNTED THEM.
;;
;; src/wire/wrappers.lisp generates a wrapper for all 123 requests, and gate 5
;; asserts the count so that a protocol update adding a request cannot arrive
;; unwrapped.  Eighty of those wrappers are verbatim identity -- %REQUEST-CLASS
;; answers :ANY for everything outside two lists, and an :ANY wrapper is
;; `(defun w:foo (a0 a1) (river:foo a0 a1))'.  The defence of that is that the
;; wire layer is the one place this program speaks to river, so a request that
;; is reclassified later costs one line here and nothing anywhere else.
;;
;; IT WAS NOT TRUE.  Thirteen call sites in runtime/ named the generated
;; request directly -- three in layer.lisp, two in outputs.lisp, five in
;; seats.lisp, three in surface.lisp -- and they are all teardowns, which is
;; the half of the protocol nobody watches.  So the discipline was applied to
;; the requests somebody remembered to route and to no others, and the count
;; gate went on passing because it counts wrappers rather than callers.
;;
;; AND THE CIRCLE CLOSED IN ONE FILE.  WRAPPERS.LISP defines WM-MANAGE-FINISH
;; with the docstring "Prefer WITH-MANAGE-SEQUENCE", and WITH-MANAGE-SEQUENCE
;; -- in wire/sequence.lisp, twenty lines away -- sent manage_finish raw.  The
;; wrapper pointed at the macro and the macro went around the wrapper.
;;
;; WHAT IS ALLOWED, AND WHY IT IS NOT ZERO MENTIONS.  Binding a global names an
;; *interface*: `(wl:wl-registry.bind registry name 'river:river-seat-v1 v)' is
;; a class name and there is nothing to wrap.  So the check is not "does this
;; file mention the protocol package" -- it is "does it name a *request*", and
;; a request is a symbol with a dot in it, which is how the generated bindings
;; spell `interface.request' and how nothing else in this tree spells anything.
(let ((offenders '()) (interfaces 0) (files 0))
  (dolist (path (sort (mapcar #'namestring
                              (append (directory (merge-pathnames "src/**/*.lisp" *root*))
                                      (directory (merge-pathnames "lattice/*.lisp" *root*))))
                      #'string<))
    (unless (under "src/wire/" path)
      (incf files)
      (let ((text (code-of path))
            (marker "river:river-")
            (start 0))
        (loop for at = (search marker text :start2 start)
              while at
              do (setf start (+ at (length marker)))
                 (let* ((end (or (position-if-not
                                  (lambda (c)
                                    (or (alphanumericp c) (find c "-.")))
                                  text :start at)
                                 (length text)))
                        (token (subseq text at end)))
                   (if (find #\. token)
                       (push (format nil "~a  ~a" (relative path) token) offenders)
                       (incf interfaces)))))))
  (format t "  files outside src/wire/~46t~d~%~
             ~4tinterfaces named, to bind a global~46t~d~%~
             ~4trequests named directly~46t~d~%"
          files interfaces (length offenders))
  (if offenders
      (fail 22 "~d request~:p called without going through src/wire/:~{~%    ~a~}~%~
                ~4tThe wire layer exists so that the manage/render discipline~%~
                ~4tis applied once rather than at every call site, and so that~%~
                ~4treclassifying a request costs one line.  A call that names~%~
                ~4tthe generated function directly has neither property, and~%~
                ~4tthe wrappers it walks past are what gate 5 counts.  Add an~%~
                ~4tALIAS in src/wire/wrappers.lisp and call that."
            (length offenders) (reverse offenders))
      (format t "  the wire layer is the only thing that speaks to river~%")))

;;; ---------------------------------------------------------------- verdict

(format t "~&~%~76,,,'=<~>~%")
(if *failures*
    (progn (dolist (f (reverse *failures*)) (format t "FAIL ~a~%" f))
           (format t "~d gate failure~:p~%" (length *failures*))
           (sb-ext:quit :unix-status 1))
    (progn (format t "ALL GATES PASS~%")
           ;; QUITTING ON THE PASSING SIDE TOO, so that reaching the end of
           ;; tools/run-gates.lisp means the verdict never ran.  Falling
           ;; through worked under `--load' and made the driver's truncation
           ;; check fire on every green build.
           (finish-output)
           (sb-ext:quit :unix-status 0)))
