;;;; tools/gates.lisp --- the build gates.  PLAN.org asked for six; there
;;;; are ten, and the four that were added are the four that found something.
;;;;
;;;; "All six run on every commit from day one.  They are cheap and they are
;;;; the only automated defence the project has."
;;;;
;;;; Gate 1 lives in tools/build.lisp because it has to run *during* the load.
;;;; The other nine run here, against the loaded image.

(require :asdf)
(require :sb-introspect)

(defparameter *failures* '())

(defun fail (gate format &rest arguments)
  (push (format nil "gate ~a: ~?" gate format arguments) *failures*))

(defun banner (gate title)
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
(defparameter *lattice-line-budget* 2600
  "Past this, the lattice is a second window manager rather than an extension.")
(defparameter *lattice-method-floor* 12
  "Below this, it is not extending through the protocol -- it is going round it.")
(if (probe-file "lattice.asd")
    (let* ((files (directory "lattice/*.lisp"))
           (lines (reduce #'+ files :key
                          (lambda (path)
                            (with-open-file (in path)
                              (loop for line = (read-line in nil) while line
                                    count t)))))
           (methods (reduce #'+ files :key
                            (lambda (path)
                              (with-open-file (in path)
                                (loop for line = (read-line in nil) while line
                                      count (search "(defmethod " line))))))
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
      (format t "  lattice/: ~d lines (budget ~d), ~d defmethods (floor ~d)~%"
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
        (fail 3 "the lattice is ~d lines against a budget of ~d -- ~
                 an extension this size is a second core, and the thesis is ~
                 that a container kind costs a few hundred lines"
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
  (dolist (row '(("river-window-management-v1" "river_window_manager_v1"
                  "latticewm/runtime::+window-management-version+")
                 ("river-xkb-bindings-v1" "river_xkb_bindings_v1"
                  "latticewm/runtime::+xkb-bindings-version+")))
    (destructuring-bind (file interface variable) row
      (let ((declared (declared-interface-version
                       (format nil "src/protocol/~a.xml" file) interface))
            (bound (symbol-value (sym variable))))
        (if (eql declared bound)
            (format t "  ~a~40tbinds v~d~%" interface bound)
            (fail 5 "~a declares version ~a in the vendored XML, but ~a is ~a"
                  interface declared variable bound))))))

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
;; 15 of 65 generics have ever been specialised by anything but their own
;; shipped default.  Fifty carry a docstring and a gate-2 obligation against a
;; maybe.  That is the real state of the extension surface, and it is now the
;; number printed on every build instead of a comfortable ratio.
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
(defparameter *outside-generic-floor* 15
  "Policy generics with at least one specialising method outside src/.

SET AT THE NUMBER, NOT COMFORTABLY BELOW IT, and that is a deliberate break
with how the ratio's floor was set.  A floor with slack under it is an
invitation to spend the slack; this one is a ratchet.  It is safe to be a
ratchet because the number cannot fall by accident -- there is no rearrangement
of the tree that lowers it, only the deletion of a method somebody wrote on the
outside.  Lowering it is therefore a decision, which is what a threshold is
for.

PLAN.org §generics wrote down the shape and never got it: \"If this list
reaches thirty, the decomposition has gone wrong in the direction of ceremony.\"
The list is at sixty-five.  A threshold nobody is ever made to argue with is a
decoration, and the way to stop that is to leave no slack to spend quietly.")

(defparameter *outside-method-floor* 22
  "Methods on policy generics defined outside src/.

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
      (let ((files (remove-duplicates
                    (loop for method in (closer-mop:generic-function-methods
                                         (fdefinition name))
                          for source = (ignore-errors
                                        (sb-introspect:find-definition-source method))
                          for file = (and source
                                          (sb-introspect:definition-source-pathname
                                           source))
                          ;; A method whose source SBCL cannot name counts as
                          ;; inside, which is the conservative direction: it
                          ;; can only make this gate harder to pass.
                          when (and file (not (under "src/" file)))
                            collect (relative file))
                    :test #'string=)))
        (when files (push (cons name files) outside))))
    (let ((count (length outside))
          (methods (reduce #'+ outside :key (lambda (entry) (length (cdr entry))))))
      (dolist (entry (sort outside #'string< :key (lambda (e) (symbol-name (car e)))))
        (format t "    ~(~a~)~24t~{~a~^  ~}~%" (car entry) (cdr entry)))
      (format t "  specialised from outside src/~46t~d of ~d  (floor ~d)~%"
              count (length generics) *outside-generic-floor*)
      (format t "  methods answering them from outside src/~46t~d  (floor ~d)~%"
              methods *outside-method-floor*)
      (format t "  answered only by their own shipped default~46t~d~%"
              (- (length generics) count))
      (when (< count *outside-generic-floor*)
        (fail 6 "~d policy generic~:p ~:*~[have~;has~:;have~] a specialising ~
                 method outside src/, against a floor of ~d.~%~
                 ~4tPLAN.org §extensibility-real: the boundary is not the~%~
                 ~4tdisease, how little of the system lives above it is.~%~
                 ~4tSomething that used to be answerable from outside is not."
              count *outside-generic-floor*))
      (when (< methods *outside-method-floor*)
        (fail 6 "~d method~:p on policy generics are defined outside src/, ~
                 against a floor of ~d.~%~
                 ~4tThe surface is being demonstrated in fewer places than it~%~
                 ~4twas.  A generic nobody has answered from outside is a~%~
                 ~4tdocstring, not an extension point."
              methods *outside-method-floor*)))))

;;; ---------------------------------------------------------------- gate 7

(banner 7 "every declared hook is run, and every run hook is declared")
;; THE BUG THIS EXISTS FOR IS THE ONE NOTHING ELSE CAN SEE.
;;
;; :FOCUS-CHANGED was declared, documented "For status bars", and listed in
;; the generated extension surface -- and no line anywhere ran it.  A status
;; bar attached to it would simply never have updated, and every check this
;; project has would have kept passing: gate 2 sees a documented hook, the
;; surface document lists it, the tests never fire it.
;;
;; The mirror image is just as quiet.  A RUN-HOOKS on a name nobody declared
;; is a seam nobody can find, because the extension surface is built from the
;; declarations.
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

(banner 9 "the image, the installer and the sample config agree")
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
;; So this gate reads the three files and checks they say the same thing.  It
;; is text matching, which is crude, and it is the only check that can be made
;; without actually installing to a clean prefix — which the next gate does.
(flet ((contains (path text)
         (and (probe-file path)
              (with-open-file (in path)
                (loop for line = (read-line in nil) while line
                      thereis (search text line))))))
  (let ((problems '()))
    (unless (contains "tools/image.lisp" "(asdf:load-system \"lattice\")")
      (push "tools/image.lisp does not build the lattice into the image" problems))
    (unless (contains "install.sh" "lattice.asd")
      (push "install.sh does not install lattice.asd" problems))
    (unless (contains "install.sh" "EXTENDING.org")
      (push "install.sh does not install the extension guide" problems))
    ;; The last two are asked of the *live image* rather than of the text,
    ;; which is both more direct and immune to a docstring that happens to
    ;; mention the thing being checked for.
    (let ((sample (call "latticewm/runtime:sample-config"))
          (directories (mapcar #'namestring
                               (call "latticewm/runtime:data-directories"))))
      (when (search "asdf:load-system" sample)
        (push "the sample config calls asdf:load-system directly; it should ~
call LOAD-EXTENSION, which knows where an installed system lives"
              problems))
      (unless (search "load-extension" sample)
        (push "the sample config never mentions LOAD-EXTENSION, so nothing ~
tells a user how to load one" problems))
      (unless (some (lambda (path) (search "share/latticewm/" path)) directories)
        (push "DATA-DIRECTORIES does not look under share/latticewm/, which is ~
where install.sh puts it" problems)))
    (if problems
        (fail 9 "~{~%    ~a~}" (reverse problems))
        (format t "  image builds it, install.sh ships it, the runtime finds it~%"))))

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

;;; ---------------------------------------------------------------- verdict

(format t "~&~%~76,,,'=<~>~%")
(if *failures*
    (progn (dolist (f (reverse *failures*)) (format t "FAIL ~a~%" f))
           (format t "~d gate failure~:p~%" (length *failures*))
           (sb-ext:quit :unix-status 1))
    (format t "ALL GATES PASS~%"))
