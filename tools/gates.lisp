;;;; tools/gates.lisp --- PLAN.org's six build gates.
;;;;
;;;; "All six run on every commit from day one.  They are cheap and they are
;;;; the only automated defence the project has."
;;;;
;;;; Gate 1 lives in tools/build.lisp because it has to run *during* the load.
;;;; The other five run here, against the loaded image.

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

(banner 3 "the lattice touches no core")
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
                       collect line))))
      (format t "  lattice/: ~d lines (budget ~d), ~d defmethods (floor ~d)~%"
              lines *lattice-line-budget* methods *lattice-method-floor*)
      (when core-edits
        (fail 3 "lattice.asd claims a component under src/: ~{~a~^ ~}"
              core-edits))
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
      (unless (or core-edits (> lines *lattice-line-budget*)
                  (< methods *lattice-method-floor*))
        (format t "  no core edits, and the numbers are in range~%")))
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

(banner 6 "the runtime-to-policy line ratio")
(flet ((count-lines (pattern &key (skip '()))
         ;; Generated files are excluded and named.  The ratio is meant to
         ;; measure *authored* runtime against authored policy; a vendored font
         ;; table is neither, and counting 95 lines of hex as runtime would make
         ;; the number say something it does not mean.  Excluding it is only
         ;; honest if the exclusion is visible, so it is printed.
         (reduce #'+ (remove-if (lambda (path)
                                  (member (file-namestring path) skip
                                          :test #'string=))
                                (directory pattern))
                 :key
                 (lambda (path)
                   (with-open-file (in path)
                     (loop for line = (read-line in nil) while line
                           unless (or (zerop (length (string-trim " " line)))
                                      (char= #\; (char (string-left-trim " " line) 0)))
                             count t))))))
  (let* ((runtime (+ (count-lines "src/wire/*.lisp")
                     (count-lines "src/runtime/*.lisp" :skip '("font.lisp"))))
         (policy (+ (count-lines "src/model/*.lisp")
                    (count-lines "src/policy/*.lisp")
                    (count-lines "lattice/*.lisp")))
         (ratio (if (plusp runtime) (/ (float policy) runtime) 0)))
    ;; PLAN.org §extensibility-real: Lisp is not what kept Emacs alive, the
    ;; *ratio* is — 1.3 million lines of Elisp on 400,000 of C, so every
    ;; feature is a worked example of how to write a feature.  Vim, Neovim and
    ;; Hyprland all have a scripting language and are not Emacs; the boundary
    ;; is not the disease, how little of the system lives above it is.
    ;;
    ;; A NUMBER *AND* A FLOOR.  It was a number alone, and a number alone
    ;; cannot fail — so this was the second of the two gates that were
    ;; measurements wearing a gate's uniform.  The floor is deliberately well
    ;; below where the project sits, because the point is to catch a *trend*
    ;; before it becomes a shape: runtime outgrowing policy is the earliest
    ;; visible symptom of the monolith failure mode, and it shows up weeks
    ;; before anything else does.
    (format t "  runtime (wire + runtime)~40t~d lines  (font.lisp excluded: generated)~%"
            runtime)
    (format t "  policy  (model + policy + lattice)~40t~d lines~%" policy)
    (format t "  ratio~40t~,2f~a~%" ratio
            (cond ((>= ratio 2.0) "   Emacs-shaped")
                  ((>= ratio 1.0) "   healthy")
                  ((>= ratio 0.8) "   <-- watch this: runtime is outgrowing policy")
                  (t "   FAILING")))
    (when (< ratio 0.8)
      (fail 6 "the runtime-to-policy ratio is ~,2f, below the floor of 0.80.~%~
               ~4tPLAN.org §extensibility-real: Lisp is not what kept Emacs~%~
               ~4talive, the *ratio* is.  Something that should have been a~%~
               ~4tdecision has been written as machinery."
            ratio))))

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
