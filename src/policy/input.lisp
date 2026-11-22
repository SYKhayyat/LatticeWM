;;;; policy/input.lisp --- The shipped answers for the *reading* half of
;;;; INPUT-POLICY, and the reservation hook every widget accumulates into.
;;;;
;;;; Completion, argument types, and how much of an output is left for the
;;;; layout once the echo area and any third-party panels have taken theirs.
;;;;
;;;; The completion style is here rather than in the minibuffer for a stated
;;;; reason: it is the single most personal decision in the whole prompt, and
;;;; the shipped answer is a default rather than a law.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; READING FROM THE USER
;;; ==================================================================

(define-option *argument-naming-convention*
  '((:direction     "direction")
    (:axis          "axis")
    (:side          "side")
    (:number        "number" "count" "steps" "step" "by" "index"
                    "cols" "rows" "cells" "columns" "visible" "x" "y")
    (:fraction      "amount" "fraction" "weight")
    (:pixels        "pixels" "distance")
    (:shell-command "command" "program")
    (:key           "spec" "key")
    (:name          "name" "label")
    (:string        "text" "string" "title")
    (:sexp          "form" "expression"))
  "Which kind of value a command's parameter holds, keyed by its name.

A convention rather than a declaration, so that a command written the obvious
way is interactively callable without its author doing anything.  Every
parameter name in the shipped commands is here, which is not a coincidence:
they were named to read well in a docstring, and a name that reads well in a
docstring is a name that says what kind of thing it is.

Where a name is genuinely ambiguous — SPAWN's COMMAND is a shell command line
and DESCRIBE-COMMAND's NAME is one of ours — DEFCOMMAND's (:interactive ...)
clause says so at the command instead of bending the table.

PATH is deliberately absent.  A tree path is a list of integers that nobody
should be asked to type, and leaving it out of the table is what makes M-x
say so rather than putting up a prompt that cannot be answered.")

(defmethod argument-type-for ((policy input-policy) command parameter)
  "The shipped naming convention.  See *ARGUMENT-NAMING-CONVENTION*."
  (declare (ignore command))
  (let ((name (string-downcase (string parameter))))
    (loop for (type . names) in *argument-naming-convention*
          when (member name names :test #'string=)
            return type)))

(defun subsequence-match-p (needle haystack)
  "True when NEEDLE's characters appear in HAYSTACK in order, gaps allowed.

The `fzf' match, and the reason `swsp' finds `send-to-workspace'.  Deliberately
last of the three tests in COMPLETE-CANDIDATES: on its own it matches nearly
everything, and a completion list that matches nearly everything has told you
nothing."
  (let ((position 0))
    (every (lambda (character)
             (let ((found (position character haystack :start position
                                                       :test #'char-equal)))
               (when found (setf position (1+ found)))))
           needle)))

(defun rank-candidates (candidates)
  "Shortest first, then alphabetical.

Length before alphabet because the shorter of two matches is nearly always the
more general one — `close' before `close-float' — and the general one is what
somebody typing four letters meant."
  (sort candidates (lambda (a b)
                     (if (= (length a) (length b))
                         (string< a b)
                         (< (length a) (length b))))))

(defmethod complete-candidates ((policy input-policy) input candidates)
  "Prefix, then substring, then subsequence.  See the generic's docstring."
  (if (zerop (length input))
      (rank-candidates (copy-list candidates))
      (let ((prefix '()) (substring '()) (fuzzy '()))
        (dolist (candidate candidates)
          (let ((position (search input candidate :test #'char-equal)))
            (cond ((eql position 0) (push candidate prefix))
                  (position (push candidate substring))
                  ((subsequence-match-p input candidate) (push candidate fuzzy)))))
        (nconc (rank-candidates prefix)
               (rank-candidates substring)
               (rank-candidates fuzzy)))))

(defun run-reserve-hooks (output)
  "The total reservation for OUTPUT, as (TOP RIGHT BOTTOM LEFT).

Reservations *accumulate*, which is a hook's shape and not a method's: two
status bars should each get their strip and the second must not have to know
about the first.  So this is the one place that reads what RUN-HOOKS returns.

It goes through the declared hook mechanism rather than through a special
variable of its own, which it used to.  Two ways to hook is one way too many —
gate 7 could see the declared one and not the other, and the extension guide
documented only the half a user could find."
  (let ((total (list 0 0 0 0)))
    (dolist (edges (run-hooks :reserve-space output) total)
      (when (and (listp edges) (= 4 (length edges))
                 (every #'realp edges))
        (setf total (mapcar #'+ total edges))))))

(defmethod reserved-space ((policy layout-policy) (output c:output))
  "Whatever the reserve hooks ask for, and nothing else.

Consults the runtime through a hook rather than calling it directly, because
policy may not depend on the runtime — the echo area is a runtime thing, a
third-party panel's exclusive zone is a compositor thing, and this is the seam
between all three."
  (run-reserve-hooks output))

;;; ------------------------------------------------------- keys, as policy

(defmethod on-key ((policy input-policy) world key)
  "The shipped answer is `I did not handle it', so the binding runs.

Specializing this is how a modal layer -- a resize mode, a vi-style submap --
intercepts everything without unbinding anything."
  (declare (ignore world key))
  nil)

(define-option *readline-chords* "abdefgknpuwy"
  "Which Ctrl-letter chords a prompt reads.

The keys fingers press at a prompt without being asked: C-a, C-e, C-k, C-w and
the rest.  Bound only while a prompt is up, so C-w still means close-tab to the
browser underneath.

An option rather than a constant because the two obvious other answers are a
whole different muscle memory -- a vi user wants almost none of these and a
readline maximalist wants more -- and because it is the one part of
CAPTURE-KEYS somebody might reasonably want to change without writing a
method.")

(defmethod capture-keys ((policy input-policy))
  "Printable ASCII bare and shifted, the keys that move and delete, and the
readline chords.

THE LIST MOVED HERE FROM src/runtime/seats.lisp, WHERE IT WAS A DEFPARAMETER.
See the generic.  The composition is unchanged and each of the three parts is
worth its comment:

Printable ASCII appears *twice*, once bare and once with Shift, and the second
copy is not redundant — its absence is a bug you find by trying to type a
bracket.  River matches a binding on keysym *and* modifiers, and the keysym xkb
produces for Shift+9 on a US layout is `parenleft' with Shift still in the
modifier set, so a binding for parenleft with no modifiers never fires and M-:
could not read a single open bracket.  Doing it by mask rather than by guessing
which characters are shifted on which layout is what makes this work on a
Dvorak or a German keyboard.

Two hundred-odd bindings sounds like a lot and is one round trip at startup;
the alternative is not being able to read text at all.

THE FOURTH GROUP IS HERE SO THAT `ANY KEY CLOSES THIS' IS TRUE.  It is printed
across the top of the welcome screen, the keymap overlay, an apropos listing
and the undo history, and this list is the whole of what an overlay can ever be
handed: a key that is not here is not merely unbound, river never tells us it
was pressed.  Making the dismissal reachable — see CAPTURE-ARMED-NOW-P and
DISMISS-OVERLAY — closed the structural half of that; this closes the rest, and
the two halves are worth keeping distinct because only the first was a defect.

Function keys, Page Up and Page Down, Insert and Menu: the keys somebody
plausibly presses at a help screen and which nothing else here would have asked
for.  F1 because on every other program in the world it means help; Page Down
because the keymap overlay is long enough to look scrollable.

Bare modifiers are deliberately not on the list and `any key' does not include
them.  An overlay that vanished when a finger came to rest on Shift would be
obnoxious, and river has no binding for a modifier alone to give us anyway."
  (declare (ignore policy))
  (append
   (loop for code from #x20 to #x7e
         append (list (cons code '()) (cons code '(:shift))))
   ;; Backspace, Tab, Return, Escape, KP_Enter, Delete, and the six that move.
   (loop for keysym in '(#xff08 #xff09 #xff0d #xff1b #xff8d #xffff
                         #xff51 #xff52 #xff53 #xff54 #xff50 #xff57)
         collect (cons keysym '()))
   ;; Page_Up, Page_Down, Insert, Menu, and F1 through F12.
   (loop for keysym in '(#xff55 #xff56 #xff63 #xff67)
         collect (cons keysym '()))
   (loop for keysym from #xffbe to #xffc9
         collect (cons keysym '()))
   (loop for letter across *readline-chords*
         collect (cons (char-code letter) '(:ctrl)))))

;;; ==================================================================
;;; TOUCH GESTURES (#16)
;;; ==================================================================
;;;
;;; The declarative half of #16: a table of gesture specifications, each naming
;;; a command to run when the gesture is recognised.  The runtime half lives in
;;; runtime/gestures.lisp and is deliberately stupid -- it takes the plans this
;;; file produces and does exactly what they say.
;;;
;;; THE PROTOCOL IS RIVER-TOUCH-GESTURES-V1, AND IT IS TOUCHSCREEN-ONLY.
;;; Trackpad three-finger swipes are libinput's and river does not expose them
;;; (river issue #1182); what this table speaks to is the touchscreen's own
;;; finger.  The issue asked for both; the finding is that the compositor
;;; determines which half exists, and the half the user can have is the half
;;; this implements.  See doc/FINDINGS.org.

(define-option *gestures*
  '(;; Three fingers, the gesture the issue asks for first: the screen itself
    ;; becomes the pager.
    ((:fingers 3 :direction :left) . "next-workspace")
    ((:fingers 3 :direction :right) . "previous-workspace")
    ((:fingers 3 :direction :up) . "focus-previous")
    ((:fingers 3 :direction :down) . "focus-next"))
  "Which touch gesture runs which command, as an assoc list.

Each entry is a SPEC and the command its recognition runs, and a spec is four
shape/trigger pairs:

  ( :fingers N :direction :left | :right | :up | :down )   a centroid swipe
  ( :fingers N :pinch :in | :out )                          a pinch, in or out
  ( :fingers N :edge :top | :bottom | :left | :right )     a swipe from an edge (one finger is the ordinary reading)

Fingers are how many touch points the gesture needs, counted the way this
program counts everything: the issue's three is three.  A pinch needs at least
two, which the protocol itself enforces.

Three tuning keywords are recognised and optional, with the values they name:

  :distance  pixels of centroid motion before a direction or edge gesture
             triggers (default 200)
  :scale     the pinch size that triggers a pinch, so 0.8 means `closer
             together than four-fifths' and 1.25 means `farther apart than a
             quarter' (default 0.8 in, 1.25 out)
  :command   the command to run; the assoc cdr is the usual place, and the
             first entry per gesture wins

The command may be any command -- a workspace, a focus move, a verb an
extension adds.  A gesture that names an unknown command is dropped when the
table is planned, with a log line, in exactly the way a key that names an
unknown command is.

These are touchscreen gestures (see the finding above).  The table is still
the right shape for trackpad gestures the day river exposes them: the planning
is protocol-agnostic and the plan is what the runtime applies.")

(defparameter +gesture-directions+ '(:up :down :left :right)
  "The four swipe directions the protocol knows, in its own order.")

(defparameter +gesture-edges+ '(:top :bottom :left :right)
  "The four edges a gesture may have to start from.")

(defparameter +gesture-pinches+ '(:in :out)
  "The two pinch directions: fingers together, fingers apart.")

(defparameter +gesture-swipe-distance+ 200
  "How far the centroid must travel before a swipe counts, in pixels.

Two hundred is a tenth of a 1920-wide screen: far enough that a tap-and-drag
or a shaky finger is not a swipe, near enough that the gesture does not feel
like a demand.  Any spec may override it with :DISTANCE.")

(defparameter +gesture-pinch-scale+ (cons 0.8 1.25)
  "The pinch sizes that trigger, as (IN . OUT) ratios.

0.8 means the pinch distance fell to four fifths of what it was at touch down;
1.25 that it grew by a quarter.  Ratios rather than percentages because that is
what the protocol measures, and a spec may override either with :SCALE.")

(defun gesture-trigger (spec)
  "Which of :DIRECTION, :PINCH or :EDGE SPEC names, or NIL if it names none."
  (cond ((member (getf spec :direction) +gesture-directions+) :direction)
        ((member (getf spec :pinch) +gesture-pinches+) :pinch)
        ((member (getf spec :edge) +gesture-edges+) :edge)))

(defun gesture-spec-p (spec)
  "Is SPEC a gesture specification this program will apply?

A spec is a plist with a positive :FINGERS count and exactly one trigger that
the protocol knows.  Nothing here checks the command: a spec and the command it
runs are two facts, and the table holds them as a cons."
  (and (listp spec)
       (integerp (getf spec :fingers))
       (plusp (getf spec :fingers))
       (gesture-trigger spec)
       ;; A pinch needs two points to have a distance at all; the protocol
       ;; says it ignores the scale threshold below two rather than refusing,
       ;; which is a gesture that can never trigger.  Refusing it here is
       ;; kinder than applying it and never seeing it.
       (not (and (eq (gesture-trigger spec) :pinch)
                 (< (getf spec :fingers) 2)))))

(defun gesture-command-of (entry)
  "The command an ENTRY from *GESTURES* names: its :COMMAND, else its cdr."
  (or (getf (car entry) :command) (cdr entry)))

(defun gesture-plans (&optional (table *gestures*))
  "The gesture table as plans the runtime can apply, in order, first entry wins.

Each plan is a plist with :FINGERS, exactly one of :DIRECTION, :PINCH or :EDGE,
the threshold that trigger uses (:DISTANCE or :SCALE), and the :COMMAND to run.
Tuning keywords are filled in from defaults here rather than in the runtime, so
that the numbers live in one place and a plan says everything.

A malformed spec, an unknown command and a second entry for the same gesture are
all dropped with a log line and the rest of the table still applies -- the same
arithmetic that makes one bad line in a config file cost one feature rather than
the session."
  (let ((out '()) (seen '()))
    (dolist (entry table)
      (let* ((spec (car entry))
             (command (gesture-command-of entry))
             (key (and (listp spec)
                       (list (getf spec :fingers) (gesture-trigger spec)
                             (or (getf spec :direction) (getf spec :pinch)
                                 (getf spec :edge))))))
        (cond
          ((not (gesture-spec-p spec))
           (logmsg :warn "gesture ~s is not a gesture this program knows" spec))
          ((null command)
           (logmsg :warn "gesture ~s names no command" spec))
          ((member key seen :test #'equal)
           (logmsg :warn "gesture ~s is defined more than once; the first one ~
                          stands" spec))
          (t
           (unless (find-command command)
             (logmsg :warn "gesture ~s names an unknown command ~s; it will ~
                            not fire" spec command))
           (push key seen)
           (push (list :fingers (getf spec :fingers)
                       (gesture-trigger spec)
                       (or (getf spec :direction) (getf spec :pinch)
                           (getf spec :edge))
                       :distance (or (getf spec :distance)
                                     +gesture-swipe-distance+)
                       :scale (gesture-scale-for spec)
                       :command command)
                 out)))))
    (nreverse out)))

(defun gesture-scale-for (spec)
  "The pinch threshold SPEC asks for, as a number on the right side of one.

The protocol takes both thresholds at once and triggers on either, so a spec
that wants `pinch in' has to say what would count as out -- and the answer is a
number no pinch reaches.  That is why this is a pair of constants rather than
the single :SCALE the spec writes: :OUT is pushed past anything a hand does
and :IN down to nearly nothing."
  (let ((pinch (getf spec :pinch))
        (scale (getf spec :scale)))
    (cond
      ;; IF, NOT OR-AND: `(or (and (numberp scale) (< scale 1)) default)' is the
      ;; shape that answers T -- the test is what the `and' returns, not the
      ;; number -- and a threshold of T is a pinch nothing can trigger.  The
      ;; suite caught it on the first run, which is why the spec has a test.
      ((eq pinch :in)
       (if (and (numberp scale) (< scale 1))
           scale
           (car +gesture-pinch-scale+)))
      ((eq pinch :out)
       (if (and (numberp scale) (> scale 1))
           scale
           (cdr +gesture-pinch-scale+)))
      (t nil))))

(defun gesture-add (spec command)
  "Put SPEC in front of *GESTURES*, running COMMAND when it is recognised.

The imperative half of the declarative table: what `M-x gesture' calls.  It
does not check the command exists -- an extension may define it later in the
same config file -- but the plan drops an unknown one when it is applied."
  (setf *gestures* (acons spec command (remove spec *gestures* :key #'car
                                                         :test #'equal)))
  *gestures*)

(defun gesture-remove (spec)
  "Take SPEC out of *GESTURES*.  Answers the new table."
  (setf *gestures* (remove spec *gestures* :key #'car :test #'equal)))

(defun gesture-table-names (&optional (table *gestures*))
  "The spec of every entry in TABLE, in order, for saying what is bound."
  (mapcar #'car table))

(defun parse-gesture-spec (text)
  "Read `3 left', `2 pinch in' or `1 edge top' into a gesture spec.

Answers the spec and NIL, or NIL and a sentence saying what was wrong.  The
parser is here rather than in the minibuffer because it is the *same* reading a
config file does by hand, and a shape that is read two ways is a shape that can
be written two ways and mean different things.

Deliberately no arithmetic and no evaluation: this is a prompt, and the only
thing a prompt reads is words somebody typed."
  (let* ((words (remove "" (uiop:split-string (string-downcase (or text ""))
                                              :separator '(#\Space #\Tab))
                        :test #'string=))
         ;; ALL DIGITS OR NOT A COUNT.  PARSE-INTEGER with :JUNK-ALLOWED-T
         ;; answers 3 for "3+1", which is arithmetic read as a finger count --
         ;; the one thing a prompt must not do.  Checking that the whole word
         ;; was consumed is the whole defence.
         (fingers (and words
                       (every #'digit-char-p (first words))
                       (not (zerop (length (first words))))
                       (parse-integer (first words))))
         (rest (and words (rest words))))
    (cond
      ((null words) (values nil "say how many fingers and which gesture"))
      ((or (null fingers) (not (plusp fingers)))
       (values nil "a gesture starts with a finger count, one or more"))
      ((null rest) (values nil "and then a direction, pinch or edge"))
      ((member (first rest) '("up" "down" "left" "right") :test #'string=)
       (values (list :fingers fingers :direction (intern (string-upcase (first rest))
                                                        :keyword))
               nil))
      ((and (string= (first rest) "pinch")
            (second rest)
            (member (second rest) '("in" "out") :test #'string=))
       (values (list :fingers fingers :pinch (intern (string-upcase (second rest))
                                                    :keyword))
               nil))
      ((and (string= (first rest) "edge")
            (second rest)
            (member (second rest) '("top" "bottom" "left" "right") :test #'string=))
       (values (list :fingers fingers :edge (intern (string-upcase (second rest))
                                                    :keyword))
               nil))
      (t (values nil (format nil "~{~a~^ ~} is not a gesture; try `3 left', ~
                                  `2 pinch in' or `1 edge top'"
                             rest))))))

(defun gesture-spec-text (spec)
  "SPEC as the words PARSE-GESTURE-SPEC reads, for echoing it back."
  (format nil "~d ~a" (getf spec :fingers)
          (cond ((getf spec :direction) (string-downcase (symbol-name (getf spec :direction))))
                ((getf spec :pinch) (format nil "pinch ~(~a~)" (getf spec :pinch)))
                ((getf spec :edge) (format nil "edge ~(~a~)" (getf spec :edge)))
                (t "?"))))
