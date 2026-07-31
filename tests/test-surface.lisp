;;;; tests/test-surface.lisp --- The extension surface, tested as a product.
;;;;
;;;; PLAN.org: "Nothing verifies that the decomposition is good.  Gate 2 checks
;;;; that generics have docstrings.  It cannot check that they are the right
;;;; generics."  That is true, and these tests do not fix it.  What they do fix
;;;; is the weaker claim that is nonetheless the one that fails first: that the
;;;; surface *works* — that a DEFMETHOD from outside actually changes
;;;; behaviour, live, with no core edit and no restart.
;;;;
;;;; Each test here is an extension somebody might really write.

(in-package #:latticewm/tests)
(in-suite surface)

(test every-policy-generic-is-documented
  ;; Gate 2, as a test as well as a build step.  An undocumented generic is an
  ;; extension point nobody can find.
  (let ((undocumented
          (remove-if (lambda (symbol) (documentation symbol 'function))
                     (p:policy-generics))))
    (is (null undocumented)
        "undocumented extension-surface generics: ~{~a~^ ~}" undocumented)))

(test the-surface-is-neither-ceremony-nor-monolith
  ;; PLAN.org: "If this list reaches thirty, the decomposition has gone wrong in
  ;; the direction of ceremony.  If it drops below ten, it has gone wrong in the
  ;; direction of a monolith."
  ;;
  ;; The ceiling has moved twice and both moves are recorded rather than
  ;; quietly applied, because a tripwire somebody steps over without comment is
  ;; not a tripwire.  Thirty to forty was the layout and lifecycle surface
  ;; settling; forty to forty-five was the interactive layer, which added
  ;; COMPLETE-CANDIDATES and ARGUMENT-TYPE-FOR.  Both earn it: completion style
  ;; is the most personal decision in the minibuffer, and a naming convention
  ;; that cannot see which command it is talking about cannot be corrected per
  ;; command.
  ;;
  ;; Forty-five to sixty is the third move, and it is a different *kind* of
  ;; move from the first two, so it is worth being explicit about why it is
  ;; not the failure this test exists to catch.
  ;;
  ;; The first two were the surface growing as features arrived.  This one is
  ;; existing behaviour crossing the line from runtime to policy without any
  ;; new feature at all: four generics because the split mechanism was five
  ;; inline (TYPEP x 'SPLIT) tests that no method could override, and the rest
  ;; because the drawing layer was a second implementation of "decisions the
  ;; author happened to make" living in src/runtime/ where nobody could reach
  ;; them.  Every one of them replaces something that was already a decision;
  ;; none of them adds a knob that did not exist as a hardcoded answer.
  ;;
  ;; That is the ruling in PLAN §log3 and it is the whole point of gate 6: the
  ;; ratio moves by code crossing the line, not by the line moving.
  ;;
  ;; What would still make this wrong is generics arriving one per feature.
  ;; The test to apply before raising it a fourth time: for each of the last
  ;; five, name the hardcoded answer it replaced.  If you cannot, it is
  ;; ceremony and the number should come down instead.
  (let ((n (length (p:policy-generics))))
    (is (<= 10 n 60) "the extension surface has ~d generics" n)))

(test the-surface-is-what-takes-a-policy-and-nothing-else
  "POLICY-GENERICS used to mean 'every exported generic in the package'.

That is indistinguishable from the structural test for exactly as long as the
only generics in LATTICEWM/POLICY are the surface — and it stopped being so
the moment a class with slot readers moved into the package.  Ten CLOS
accessors became extension-surface entries, gate 2 started demanding
docstrings for COMMAND-FUNCTION, and the sentence the surface document prints
at the top of itself — \"every generic below takes a POLICY as its first
argument\" — was false.

POLICY-GENERIC-P had been written, documented, and never called.

This asserts the two halves separately, because a test that only counted
would have passed throughout the bug."
  (dolist (symbol (p:policy-generics))
    (is (p:policy-generic-p symbol)
        "~a is in the surface but is not specialized on a policy" symbol))
  ;; The accessors that exposed it: exported, generic, and correctly excluded.
  (dolist (symbol '(p:command-name p:command-function p:command-lambda-list
                    p:argument-type-prompt p:argument-type-parser))
    (is (typep (fdefinition symbol) 'generic-function)
        "~a really is a generic, so this test is not vacuous" symbol)
    (is (not (p:policy-generic-p symbol))
        "~a is a slot reader on a command, not something you specialize" symbol)
    (is (not (member symbol (p:policy-generics)))
        "~a must not be in the extension surface" symbol))
  ;; And something that genuinely is the surface still is.
  (is (p:policy-generic-p 'p:gaps))
  (is (member 'p:container-axis (p:policy-generics))))

(test every-option-is-documented-and-has-a-default
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore value))
      (is (stringp documentation) "~a has no docstring" key)
      (is (not (eq default :unset)) "~a has no default" variable))))

(test every-option-is-reachable-from-a-config-file
  "A config file is read in LATTICEWM/USER, so an option whose symbol that
package cannot see is not merely inconvenient — it fails silently.

  (setf *terminal* \"alacritty\")

in an init.lisp interns a *new* symbol in LATTICEWM/USER, sets that, and
changes nothing whatsoever.  No error, no warning, and the starter config
this program writes named *TERMINAL* on exactly that line.  Twenty-four of
the thirty-one runtime options were in this state and every one of them was
documented, registered and listed by --list-options, so nothing else in the
build had any reason to complain.

The check is symbol identity rather than accessibility: an option that is
merely PRESENT in LATTICEWM/USER because something else interned it there is
still the wrong symbol."
  (dolist (row (p:all-options))
    (destructuring-bind (key variable value default documentation) row
      (declare (ignore key value default documentation))
      (let* ((name (symbol-name variable))
             (in-user (find-symbol name '#:latticewm/user)))
        (is (eq in-user variable)
            "~a is not reachable from a config file: LATTICEWM/USER sees ~
             ~:[nothing by that name~;a different symbol~].  Export it from ~a."
            variable in-user (package-name (symbol-package variable)))))))

(test options-round-trip
  (let ((before (p:option :gaps)))
    (unwind-protect
         (progn (setf (p:option :gaps) 12)
                (is (= 12 p:*gaps*) "the keyword and the variable are one thing"))
      (setf (p:option :gaps) before))))

;;; ---------------------------------------------------------- tier 1: a method

(defclass gapless-policy (p:conventional-policy) ()
  (:documentation "A policy that never leaves a gap.  Tier 1, one method."))

(defmethod p:gaps ((policy gapless-policy) container)
  (declare (ignore container))
  0)

(test tier-1-a-method-from-outside-changes-behaviour
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
        (p:*gaps* 20))
    (let ((wide (p:layout-node (make-instance 'p:conventional-policy) root
                               (c:make-rect 0 0 100 100)))
          (tight (p:layout-node (make-instance 'gapless-policy) root
                                (c:make-rect 0 0 100 100))))
      (is (= 40 (c:rect-w (third (second wide)))))
      (is (= 50 (c:rect-w (third (second tight))))
          "and no core file was edited to get it"))))

;;; ------------------------------------------------- tier 2: method plus state

(defclass remembering-policy (p:conventional-policy) ()
  (:documentation
   "Entry resolution with last-focus memory — the behaviour DESIGN D20
deliberately did *not* ship, added from outside with no core edit.

The state has nowhere to live except PROPS, which is exactly the case D20
predicted and exactly why every node carries one."))

(defmethod p:entry-address ((policy remembering-policy) (split c:split)
                            direction reference rects)
  (declare (ignore direction reference rects))
  (or (c:prop split :remembering/last) (call-next-method)))

(defmethod p:on-focus-change ((policy remembering-policy) world old new)
  (declare (ignore old))
  (let ((chain (c:resolve-chain (c:world-root world) new)))
    (loop for node in chain
          for address in new
          when (typep node 'c:split)
            do (setf (c:prop node :remembering/last) address)))
  (call-next-method))

(test tier-2-behaviour-plus-state-with-no-core-edit
  (let* ((policy (make-instance 'remembering-policy))
         (inner (c:make-split :horizontal (list (leaf-with "b") (leaf-with "c"))))
         (root (c:make-split :horizontal (list (leaf-with "a") inner)))
         (world (c:make-world :root root :cursor '(0))))
    ;; Visit "c", which teaches the inner split that 1 was last focused.
    (p:jump-cursor policy world '(1 1))
    (p:jump-cursor policy world '(0))
    (is (equal "c" (app-at root (p:find-motion-target policy root '(0) :right)))
        "memory sent us back to the pane we had been in")
    ;; And the shipped policy, on the same tree, still does the geometric thing.
    (is (equal "b" (app-at root (p:find-motion-target (policy) root '(0) :right))))))

;;; -------------------------------------------- tier 3: a whole new behaviour

(defclass monocle-policy (p:conventional-policy) ()
  (:documentation
   "Every split shows only its focused child, full size — the 'monocle' or
'maximized' layout, as a policy rather than as a mode.

A whole layout model in four lines, with zero core edits.  This is the shape
the lattice has to take, in miniature."))

(defmethod p:layout-children ((policy monocle-policy) (split c:split) rect)
  (let ((address (or (c:prop split :monocle/focus) 0)))
    (when (c:child-at split address)
      (list (cons address rect)))))

(test tier-3-a-new-layout-model-with-zero-core-edits
  (let* ((policy (make-instance 'monocle-policy))
         (root (c:make-split :horizontal
                             (list (leaf-with "a") (leaf-with "b") (leaf-with "c"))))
         (placements (p:layout-node policy root (c:make-rect 0 0 800 600))))
    (let ((visible (remove-if-not #'fourth (rest placements))))
      (is (= 1 (length visible)) "exactly one pane is drawn")
      (is (= 800 (c:rect-w (third (first visible)))) "and it has the whole rect"))
    (setf (c:prop root :monocle/focus) 2)
    (let* ((placements (p:layout-node policy root (c:make-rect 0 0 800 600)))
           (visible (remove-if-not #'fourth (rest placements))))
      (is (equal "c" (app-at root (second (first visible))))))))

;;; ------------------------------------------ a new container kind from outside

(defclass ring (c:sequential-container)
  ((offset :initform 0 :accessor ring-offset))
  (:documentation
   "A container whose motion wraps around — the last child's right neighbour is
the first.

The point of this test is not the ring.  It is that a *container kind the core
has never heard of* participates fully in motion, layout, surgery and focus
repair, with no edit to anything under src/.  If this test passes, the lattice
can be added the same way; if it could not, the container protocol was drawn in
the wrong place and D21's experiment has already failed."))

(defmethod p:layout-children ((policy p:conventional-policy) (r ring) rect)
  (mapcar #'cons (c:container-addresses r)
          (c:divide-rect rect :horizontal
                         (make-list (c:container-count r) :initial-element 1))))

(defmethod p:step-address ((policy p:conventional-policy) (r ring) address direction)
  (when (eq (c:direction-axis direction) :horizontal)
    (mod (+ address (c:direction-sign direction)) (c:container-count r))))

(defmethod p:entry-address ((policy p:conventional-policy) (r ring)
                            direction reference rects)
  (declare (ignore reference rects))
  (if (eq direction :left) (1- (c:container-count r)) 0))

(test a-container-kind-the-core-never-heard-of-works-everywhere
  (let* ((policy (policy))
         (r (make-instance 'ring
                           :children (list (leaf-with "a") (leaf-with "b")
                                           (leaf-with "c"))))
         (root (c:make-stack (list r))))
    ;; Motion, including the wrap that no core container does.
    (is (equal "b" (app-at root (p:find-motion-target policy root '(0 0) :right))))
    (is (equal "a" (app-at root (p:find-motion-target policy root '(0 2) :right)))
        "it wrapped, and motion never had to be told what a ring is")
    ;; Layout: the stack, the ring, and its three children.
    (let* ((placements (p:layout-node policy root (c:make-rect 0 0 300 100)))
           (leaves (remove-if-not (lambda (pl) (typep (first pl) 'c:leaf))
                                  placements)))
      (is (= 5 (length placements)))
      (is (= 3 (length leaves)))
      (is (every (lambda (pl) (= 100 (c:rect-w (third pl)))) leaves)
          "the ring divided its rectangle evenly, using core geometry it did
not have to reimplement"))
    ;; Surgery, and focus repair through it.
    (multiple-value-bind (removed new-root focus)
        (c:tree-remove-at root '(0 1) :focus-path '(0 2))
      (declare (ignore removed))
      (is (equal "c" (app-at new-root focus))
          "focus followed the node through a container the core cannot name"))))

;;; --------------------------------------- the split mechanism, as policy

(defmethod p:container-axis ((policy p:conventional-policy) (r ring))
  "A ring divides its rectangle along the horizontal, exactly as a split does.

Answering this one question is the whole of what it takes to join the split
mechanism: RESIZE finds it while walking up for an ancestor along the axis,
and TREE-SPLIT-AT joins it rather than nesting inside it."
  :horizontal)

(defvar *ring-equalized* nil)

(defmethod p:equalize-container ((policy p:conventional-policy) (r ring))
  "What evening out means for a ring is the ring's business.  The verb does
not need to know, which is the difference between a generic and a TYPEP."
  (setf *ring-equalized* t))

(test the-split-mechanism-needs-no-core
  "Five places used to ask (TYPEP x 'SPLIT), and a TYPEP cannot be specialized.

The consequence was specific rather than theoretical: a container kind from
outside the core that divides space in exactly the way a split does was
invisible to RESIZE, EQUALIZE, EQUALIZE-ALL and TAB, and TREE-SPLIT-AT would
never join it — so splitting three times inside one built a ladder of nested
two-child containers instead of one row of four, with no method anywhere able
to say otherwise.

All five are generics now, and this asserts that answering them from outside
src/ is sufficient."
  (let* ((policy (policy))
         (r (make-instance 'ring :children (list (leaf-with "a")
                                                 (leaf-with "b"))))
         (root (c:make-stack (list r))))
    ;; The question the four verbs ask.
    (is (eq :horizontal (p:container-axis policy r))
        "a kind the core cannot name declares its axis")
    (is (null (p:container-axis policy (c:make-stack (list (leaf-with "x")))))
        "a stack divides nothing and says so")
    (is (null (p:container-axis policy nil))
        "and so does the top of a parent chain, so no caller needs a guard")
    ;; EQUALIZE and EQUALIZE-ALL, which map over every node including leaves.
    (let ((*ring-equalized* nil))
      (is (p:equalize-container policy r))
      (is (not (null *ring-equalized*))
          "the verb dispatched rather than testing a type"))
    (is (null (p:equalize-container policy (leaf-with "z")))
        "a leaf declines instead of signalling NO-APPLICABLE-METHOD")
    ;; TAB declines by default rather than doing something wrong.
    (is (null (p:tab-siblings policy r 0))
        "no shipped answer for a kind nobody has taught it about")
    (is (equal '(0 1) (multiple-value-list
                       (p:tab-siblings policy
                                       (c:make-split :horizontal
                                                     (list (leaf-with "a")
                                                           (leaf-with "b")))
                                       0)))
        "and the shipped rule still folds a pair of siblings")
    ;; The surgery half: TREE-SPLIT-AT joins the ring instead of nesting.
    (is (funcall (p:split-join-predicate policy) r :horizontal))
    (is (not (funcall (p:split-join-predicate policy) r :vertical))
        "and declines on the axis it does not divide")
    (multiple-value-bind (new-root path)
        (c:tree-split-at root '(0 1) (leaf-with "c")
                         :axis :horizontal
                         :join-p (p:split-join-predicate policy))
      (declare (ignore path))
      (let ((joined (c:resolve-path new-root '(0))))
        (is (typep joined 'ring) "it joined the ring rather than nesting")
        (is (= 3 (c:container-count joined))
            "one row of three, not a ring holding a two-child split")))
    ;; And the default predicate, which src/model/ uses when nobody passes one,
    ;; still refuses — purity preserved, behaviour unchanged for the core.
    (is (not (c:default-split-join-p r :horizontal))
        "the pure default knows only SPLIT, which is why the policy one exists")))

;;; ------------------------------------------------------------------ fonts

(test a-font-is-data-and-the-policy-picks-it
  "The window manager shipped with one font wired in as three constants.

Fonts are policy now: which one is drawn for which role is FONT-FOR, and the
generated Terminus table is simply the one that registers itself as the
default.  What this asserts is the part that had a real bug in it — the
representation.  An earlier version stored one byte per row and refused
anything wider than eight pixels, on the reasoning that Terminus's larger
sizes are taller rather than wider.  They are not: ter-122b is eleven wide and
ter-d28n is fourteen, so the cap refused every size above the smallest."
  (let ((policy (policy)))
    ;; The shipped font is registered and is what FONT-FOR answers with.
    (is (p:font-p p:*default-font*))
    (is (equal "terminus" (p:font-name p:*default-font*)))
    (is (eq p:*default-font* (p:font-for policy :default)))
    (is (eq p:*default-font* (p:font-for policy :a-role-nobody-has-defined))
        "an unknown role inherits the default rather than signalling")
    ;; Stride is derived from width, at both widths that matter.
    (is (= 1 (p:font-stride (p:make-font "narrow" 8 16 32 (byte-vector 16)))))
    (is (= 2 (p:font-stride (p:make-font "wide" 14 28 32 (byte-vector 56)))))
    ;; Bit order: leftmost pixel is the high bit, at either stride.
    (let ((narrow (p:make-font "n" 8 1 65 (byte-vector 1 #x80))))
      (is (logbitp 7 (p:glyph-row narrow #\A 0)) "leftmost pixel is bit 7")
      (is (zerop (p:glyph-row narrow #\A 5)) "a row past the end is blank")
      (is (zerop (p:glyph-row narrow #\Space 0)) "so is a glyph before FIRST-CODE"))
    (let ((wide (p:make-font "w" 14 1 65 (byte-vector 2 #x80 #x00))))
      (is (logbitp 15 (p:glyph-row wide #\A 0))
          "at stride 2 the leftmost pixel is bit 15, not bit 7")
      (is (= 2 (p:font-stride wide))))
    ;; *UI-FONT* names a font, and naming one changes what FONT-FOR answers.
    (let ((mine (p:make-font "test-font" 12 24 32 (byte-vector (* 24 2 95)))))
      (p:register-font mine)
      (is (eq mine (p:find-font "test-font")))
      (is (eq mine (p:find-font mine)) "a font passes through FIND-FONT")
      (is (member "test-font" (p:font-names) :test #'equal))
      (let ((p:*ui-font* "test-font"))
        (is (eq mine (p:font-for policy :echo))
            "setting the option is the whole of changing the font"))
      (is (eq p:*default-font* (p:font-for policy :echo))
          "and unsetting it puts the shipped one back")
      (let ((p:*ui-font* "a-font-that-was-never-registered"))
        (is (eq p:*default-font* (p:font-for policy :echo))
            "a name nobody registered falls back rather than drawing nothing")))
    ;; Metrics come from the font, which is what makes a bigger one bigger.
    (let ((big (p:make-font "big" 12 24 32 (byte-vector 1))))
      (is (= 60 (p:font-text-width big "hello")))
      (is (= 24 (p:font-text-height big)))
      (is (= 120 (p:font-text-width big "hello" :scale 2))))))

;;; ------------------------------------------------------ shifted keys

(test shift-produces-the-shifted-glyph
  "River sends the *unshifted* keysym, so the shifted glyph is ours to work out.

The design assumed the opposite and said so in a comment: that xkb produces
`parenleft' for Shift+9 and river passes it through with Shift still set.  So
every printable keysym was bound twice, bare and shifted, and the prompt used
the keysym as the character.

On a real keyboard what arrives for Shift+9 is keysym `9' with Shift set.
Typing (+ 1 2) into M-: produced `9= 1 20' -- ( became 9, + became =, ) became
0 -- and the reader reported an unbound variable called 9=.  That log line is
the whole of the evidence and it took bare metal to produce it."
  (is (char= #\( (p:shifted-character #\9)))
  (is (char= #\+ (p:shifted-character #\=)))
  (is (char= #\) (p:shifted-character #\0)))
  (is (char= #\_ (p:shifted-character #\-)))
  (is (char= #\? (p:shifted-character #\/)))
  (is (char= #\: (p:shifted-character #\;)))
  (is (char= #\" (p:shifted-character #\')))
  ;; Letters need no table entry.
  (is (char= #\A (p:shifted-character #\a)))
  (is (char= #\Z (p:shifted-character #\z)))
  ;; And the table is a tier-0 value, because on a German or Dvorak layout the
  ;; shipped answer is wrong and there is no way to derive the right one.
  (let ((p:*shift-map* (cons '(#\8 . #\() p:*shift-map*)))
    (is (char= #\( (p:shifted-character #\8))
        "a config file can move the bracket to where their keyboard has it")))

;;; --------------------------------------------- what river is told about

(test a-submap-does-not-steal-the-alphabet
  "Registration must tell river the *first* key of every chord and nothing else.

This is the bug bare metal found, and it is the only one in this project so
far that could not have been found any other way.

Registration walked ALL-BOUND-KEYS, which descends into submaps.  So the help
submap's second keys -- b, c, f, k, o, a, s -- were registered with river as
bare, unmodified, global bindings.  River then ate every one of those
keypresses, because a registered binding is by definition not delivered to the
focused window, and HANDLE-KEY did nothing with them, because LOOKUP-KEY
searches the global keymap where `s' is not bound -- it is bound one level
down, in a submap that was not pending.

Seven letters silently vanished.  Not misbehaved: vanished.  Typing `ls' in a
terminal produced `l'.

It survived every nested session and 745 checks because the sessions were
driven through the SWANK bridge and nobody had ever sat down and typed into a
window.  ALL-BOUND-KEYS was correct for what help and where-is want; it was
simply the wrong list to hand the compositor."
  (let ((map (p:make-keymap :name "global"))
        (sub (p:make-keymap :name "help")))
    (p:define-key sub "b" '("help"))
    (p:define-key sub "s" '("set-option"))
    (p:define-key map "Shift+Super+question" sub)
    (p:define-key map "Super+Return" '("terminal"))
    (let ((registered (p:bindable-keys map))
          (everything (p:all-bound-keys map)))
      (is (= 2 (length registered))
          "river hears the chord's first key and Super+Return, and no more")
      (is (= 4 (length everything))
          "help and where-is still see inside the submap")
      ;; The property that actually matters, stated directly.
      (is (null (remove-if #'cdr registered :key #'car))
          "not one registered key is unmodified: ~s"
          (mapcar (lambda (e) (p:key-to-string (car e)))
                  (remove-if #'cdr registered :key #'car)))
      ;; And the submap's keys are still reachable where they should be.
      (is (equal '("help") (p:lookup-key sub (p:kbd "b"))))
      (is (null (p:lookup-key map (p:kbd "b")))
          "but not from the global map, which is what made them disappear"))))

;;; ------------------------------------- how the system describes itself

(test the-composition-path-runs-end-to-end
  "Every screen the window manager uses to explain itself, exercised once.

This exists because none of it was covered, and the gap cost a real bug: a
function moved from LATTICEWM/RUNTIME to LATTICEWM/POLICY and one caller was
left behind naming a symbol in the package it had left.  KEYMAP-CHOICES then
called an undefined function, so *pressing a chord prefix would have errored*
-- and gate 1 passed, gate 2 passed, and 727 checks passed, because nothing
ever called it.

The lesson is narrow and worth keeping: a test that constructs the object is
not the same as a test that renders it.  These five are the rendering."
  (let* ((policy (policy))
         (m (p:make-keymap :name "test")))
    (p:define-key m "a" '("help"))
    (p:define-key m "b" '("quit"))
    ;; which-key, which is what actually broke.
    (let ((choices (p:keymap-choices policy m)))
      (is (= 2 (length choices)))
      (is (equal "a" (car (first choices))))
      (is (plusp (length (cdr (first choices))))
          "and it found the command's docstring, not just its name"))
    (let ((segments (let ((p:*pending-keymap* m))
                      (p:pending-keymap-segments policy 120))))
      (is (<= 2 (length segments)) "a prompt segment and one row per choice"))
    ;; A binding describes itself with its arguments substituted in.
    (let ((description (p:binding-description policy '("focus" :left))))
      (is (search "left" description)
          "the argument reached the docstring: ~s" description)
      (is (not (search "DIRECTION" description))
          "and the placeholder did not survive: ~s" description)
      (is (not (find (code-char 8212) description))
          "a summary cut at a dash does not keep the dash: ~s" description))
    ;; describe-command, and where-is.
    (let ((rows (p:command-help-rows policy (p:find-command "focus"))))
      (is (plusp (length rows)))
      (is (every (lambda (row) (and (consp row) (stringp (cdr row)))) rows)))
    ;; The welcome screen, derived rather than written down.
    (let ((rows (p:welcome-rows policy)))
      (is (<= 10 (length rows) 20) "about a dozen, ending with how to quit")
      (is (some (lambda (row) (search "Escape" (car row))) rows)
          "and the way out is on it"))
    ;; The full keymap screen, over a keymap that actually has bindings.
    (let ((entries (p:help-entries policy m)))
      (is (= 2 (length entries)))
      (is (every (lambda (e) (and (stringp (car e)) (stringp (cdr e)))) entries)))))

;;; ------------------------------------------------------------ live surgery

(test redefining-a-method-takes-effect-immediately
  ;; The claim the whole language decision rests on, reduced to an assertion.
  (let ((root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
        (policy (policy)))
    (flet ((first-width ()
             (c:rect-w (third (second (p:layout-node policy root
                                                     (c:make-rect 0 0 100 100)))))))
      (is (= 50 (first-width)))
      (defmethod p:layout-children ((p p:conventional-policy) (s c:split) rect)
        (declare (ignore rect))
        (list (cons 0 (c:make-rect 0 0 77 100))))
      (unwind-protect
           (is (= 77 (first-width)) "no restart, no rebuild, no lost state")
        ;; Put the shipped method back.
        (remove-method #'p:layout-children
                       (find-method #'p:layout-children '()
                                    (list (find-class 'p:conventional-policy)
                                          (find-class 'c:split)
                                          (find-class 't))))
        (defmethod p:layout-children ((policy p:conventional-policy)
                                      (split c:split) rect)
          (mapcar #'cons (c:container-addresses split)
                  (c:divide-rect rect (c:split-axis split) (c:weights split)
                                 :gap (p:gaps policy split)))))
      (is (= 50 (first-width)) "and the restore took effect the same way"))))

(test adding-a-slot-to-a-live-class-migrates-existing-instances
  ;; SPIKE-WEEK0 §ext-3, which is what settled DEFCLASS over DEFSTRUCT for core
  ;; state.  An instance made *before* the redefinition gains the slot.
  (let ((before (c:make-leaf)))
    (eval '(defclass c:leaf (c:node)
             ((window :initarg :window :initform nil :accessor c:leaf-window)
              (test-only-slot :initform :migrated :accessor leaf-test-only-slot))))
    (unwind-protect
         (is (eq :migrated (funcall (read-from-string "latticewm/tests::leaf-test-only-slot")
                                    before))
             "an instance that predates the redefinition has the new slot")
      (eval '(defclass c:leaf (c:node)
               ((window :initarg :window :initform nil :accessor c:leaf-window)))))))
