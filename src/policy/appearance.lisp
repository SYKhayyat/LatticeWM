;;;; policy/appearance.lisp --- What the window manager draws, as decisions.
;;;;
;;;; WHY THIS FILE EXISTS
;;;;
;;;; The split here is the Emacs one.  The C core does redisplay primitives;
;;;; Elisp decides what goes on the mode line.  Shared-memory buffers, the fd
;;;; path, glyph blitting and surface lifecycle stay in src/runtime/.  Colours,
;;;; scales, what a status line is made of, how many columns a help screen
;;;; uses, and *which font gets drawn for which role* are decisions, and they
;;;; live here.
;;;;
;;;; THIS HEADER USED TO SAY THAT GATE 6 IS WHY, AND THAT WAS THE PROBLEM.
;;;;
;;;; Gate 6 was a line-count ratio — (model + policy + lattice) against (wire +
;;;; runtime), floored at 0.80 — and it read as a direct measurement of PLAN
;;;; §extensibility-real: Lisp is not what kept Emacs alive, the *ratio* is.
;;;; The move that created this file was made partly to satisfy it, and this
;;;; header said so, and PLAN §log4 through §log6 are three sessions of moving
;;;; code to make a number go up.
;;;;
;;;; The number went up.  Two of those commits reproduce 92-100% of the moved
;;;; lines verbatim and added no dispatch point at all, and the one that claimed
;;;; to be "a real change rather than an accounting one" was 90% a change to the
;;;; counting rule.  A metric that can be satisfied by `git mv' will be, and
;;;; once it can be, it has stopped measuring anything.  Gate 6 is now the
;;;; question the ratio was a proxy for and could not ask — how much of the
;;;; behaviour is expressible from *outside* src/ — which no amount of moving
;;;; files can change.
;;;;
;;;; What survives from the old argument is the argument itself, above.  A
;;;; DEFINE-OPTION that lives in src/runtime/ is still something a user has to
;;;; read the runtime to find, and that is true whether or not anything counts
;;;; the lines.  The moves this file is made of were right for that reason.
;;;; They were not right because they moved a number.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; THE ECHO AREA
;;; ==================================================================

(define-option *echo-area* t
  "Show a status line along the bottom of the screen.

It reports where the cursor is, what is on this workspace, and the last
message.  Turn it off if you want the screen entirely to yourself; you will
lose the only permanent indication of which cell you are in.")

(define-option *echo-height* 24
  "Height of the echo area in pixels.")

(define-option *echo-scale* 1
  "Integer scale factor for echo-area text.

1 is the font's own size — sixteen pixels tall, which is an ordinary status
bar.  2 is for a HiDPI display, where it is again ordinary rather than large.
There is no half step, deliberately: a bitmap font scaled by anything but a
whole number is a smear, and a bitmap font scaled by a whole number is crisp
at any size.")

(define-option *echo-position* :bottom
  "Which edge the echo area sits on: :BOTTOM or :TOP.

Bottom is Emacs's minibuffer and is the default.  Top is worth knowing about
for two situations: running nested inside another desktop whose panel is along
the bottom, and any setup where something else already owns that edge.")

(define-option *echo-background* '(0.09 0.09 0.12 0.92)
  "Echo area background, as (R G B A).  Slightly translucent by default so it
reads as an overlay rather than as a window.")

(define-option *echo-foreground* '(0.75 0.78 0.85 1.0)
  "Echo area text colour.")

(define-option *echo-accent* '(0.40 0.65 1.00 1.0)
  "Colour for the part of the echo area that says where you are.")

(define-option *echo-divider* '(0.28 0.28 0.34 1.0)
  "Colour of the separators between echo-area segments.

Dim on purpose: a separator is punctuation, and punctuation you notice is
punctuation that is too loud.")

(define-option *echo-message-seconds* 6
  "How long a message stays in the echo area before it is dropped.")

;;; ==================================================================
;;; THE MINIBUFFER
;;; ==================================================================

(define-option *minibuffer-prompt-color* '(0.95 0.75 0.35 1.0)
  "Colour of the prompt in the echo area.")

(define-option *minibuffer-completion-color* '(0.50 0.55 0.65 1.0)
  "Colour of the completion hint after what you have typed.")

(define-option *minibuffer-caret-color* '(0.95 0.75 0.35 1.0)
  "Colour of the caret — the bar showing where what you type will go.")

(define-option *minibuffer-candidates-shown* 6
  "How many completion candidates the echo area lists at once.")

(define-option *history-length* 100
  "How many past entries each minibuffer history ring keeps.")

;;; ==================================================================
;;; THE HELP OVERLAY
;;; ==================================================================

(define-option *help-columns* 2
  "How many columns of bindings the help overlay uses.

Two, not three.  Three fits more rows and truncates almost every description,
and a help screen where you can read the keys but not what they do has kept
the wrong half.")

(define-option *help-background* '(0.07 0.07 0.10 0.94)
  "Help overlay background, as (R G B A).")

(define-option *help-key-color* '(0.45 0.70 1.00 1.0)
  "Colour of the key in the help overlay.")

(define-option *help-text-color* '(0.78 0.80 0.86 1.0)
  "Colour of the description in the help overlay.")

(define-option *help-scale* 1
  "Integer scale factor for help-overlay text.")

;;; ==================================================================
;;; EMPTY PANES, AND OVERLAY BUFFERS
;;; ==================================================================

(define-option *show-empty-panes* t
  "Draw an outline around empty panes, and the spawn hints inside the focused
one.

Turning this off gives you a window manager where standing in an empty pane
looks exactly like a broken keyboard.  It is here because it is an option, not
because it is a good idea.")

(define-option *empty-pane-hint* t
  "Show which keys open something, inside the focused empty pane.")

(define-option *empty-outline-color* '(0.30 0.32 0.40 0.55)
  "Outline colour of an unfocused empty pane.")

(define-option *empty-hint-color* '(0.70 0.75 0.86 1.0)
  "Text colour of the hint inside the focused empty pane.")

(define-option *overlay-buffer-idle* '(:help)
  "Which overlays release their pixel buffers while hidden.

A list of overlay kinds, or T for all of them, or NIL for none.

THE ARITHMETIC THIS OPTION WAS ARGUED FROM WAS WRONG BY 8x IN THE AUTHOR'S OWN
FAVOUR.  It read: \"A full-screen ARGB buffer is about four megabytes ... keeping
their buffers costs eight megabytes of resident memory to save one allocation on
a keypress, which is the wrong way round.\"  Four megabytes is 1920x1080x4 at
scale 1.  MAKE-CANVAS multiplies by the output's scale *before* computing the
stride, because that is what HiDPI means, so on the 1920x1080 panel at scale 2
the machine was sitting in front of, a full-screen canvas is 3840x2160x4 = 33
megabytes.  Two buffers of it — and there are two now, because one was a race
against the compositor — is 66.

Which makes both halves of the sentence bigger, in opposite directions, and
that is why this is a list rather than a boolean.  Releasing is worth much more
than it said for the help screen: 66 MB resident for a page shown for four
seconds a week is not a trade anybody would take.  It is worth much less than
it said for anything toggled continuously — the drawn map appears when a zoom
crosses a threshold, and a threshold crossed by a continuous gesture is crossed
repeatedly, so releasing there means mkstemp, ftruncate(33MB), mmap and
create_pool twice per wobble.

So the shipped answer is the one overlay that is large and rare, by name, and
everything else keeps what it has.  T restores the old behaviour for every
overlay including an extension's; NIL keeps every buffer forever.")

(defun overlay-buffer-idle-p (kind)
  "Whether an overlay of KIND should give up its buffers while hidden."
  (let ((setting *overlay-buffer-idle*))
    (cond ((eq setting t) t)
          ((null setting) nil)
          ((listp setting) (and (member kind setting) t))
          (t t))))

;;; ==================================================================
;;; WHAT THE STATUS LINE SAYS
;;; ==================================================================
;;;
;;; ECHO-CONTENT was declared a generic in protocol.lisp from the first day,
;;; with a docstring explaining that the content of the status line is a
;;; decision.  Its only method then lived in src/runtime/echo.lisp, next to
;;; the blitter -- so the decision was overridable in principle and sat on the
;;; wrong side of the line in fact, which is the more subtle half of the same
;;; mistake as a DEFINE-OPTION in the runtime.
;;;
;;; This is Emacs's `mode-line-format\' and it should be read that way: the
;;; single most-customised variable in that system, and the reason a status
;;; line is a thing people make their own rather than a thing they tolerate.
;;;
;;; What stays in the runtime is ECHO-SEGMENTS, which chooses between "a
;;; prompt is up" and "ask the policy" -- and that genuinely is runtime state,
;;; because whether the minibuffer is reading is not a decision anybody wants
;;; to override.

(defvar *echo-message* nil "A cons of text and the time it was posted.")

(defun current-message ()
  "The message to show, or NIL once it has aged out."
  (let ((message *echo-message*))
    (when (and message
               (< (- (get-universal-time) (cdr message)) *echo-message-seconds*))
      (car message))))

(define-option *keys-hint* t
  "Keep a one-line reminder of the essential keys in the status line, until
the keymap has been opened at least once.

WHY THIS EXISTS, IN THE WORDS OF THE FIRST PERSON TO USE IT COLD: \"i have no
clue how to close windows or how to pick which splits\".

Everything needed was already there -- the welcome overlay lists these keys,
Super+/ draws the whole keymap, and the empty pane already says `e t b to
open\'.  None of it helped, for a reason worth writing down: the welcome
overlay is dismissed by *any* key, so the first keystroke of an impatient
person removes it before it has been read, and every other affordance is
behind a key you have to already know.

A status line is the one place that cannot be dismissed by accident.  So the
essentials live there until Super+/ has been pressed once, at which point the
hint has done its job and goes away by itself rather than nagging forever.")

(defvar *keymap-ever-opened* nil
  "True once the help overlay has been shown.  Turns *KEYS-HINT* off.")

(defgeneric keys-hint (policy world)
  (:documentation
   "The one-line reminder of essential keys, or NIL for none.

Shown in the status line until the keymap has been opened once.  Derived from
*MODIFIER* so it follows a rebinding, and deliberately six items long: this is
the smallest set somebody cannot work the machine without -- open something,
split it, move between the pieces, close one, and find everything else."))

(defmethod keys-hint ((policy appearance-policy) world)
  (declare (ignore world))
  (when (and *keys-hint* (not *keymap-ever-opened*))
    (let ((mod (string-capitalize (string *modifier*))))
      (format nil "~a+Return term  ~a+d/s split  ~a+hjkl move  ~a+q close  ~
                   ~a+/ all keys"
              mod mod mod mod mod))))

(defmethod window-name ((policy appearance-policy) (window c:window))
  "The label, the app id, the title, or a last resort that is still a noun."
  (or (c:prop window :label)
      (c:window-app-id window)
      (c:window-title window)
      "a window"))

(defgeneric cursor-place-name (policy world)
  (:documentation
   "Where the cursor is, in whatever vocabulary this policy uses for places.

A short string for the status line, never NIL.  The shipped answer is the
cursor path -- 0.1.0 -- because a path is the one name every layout model has.
A policy that gives places a better name answers with that instead: the lattice
returns the cell coordinate, 3,-2.

THIS GENERIC IS THE REPAIR OF A CORE EDIT, and the edit is worth knowing about
because nothing in the project could see it.  ECHO-CONTENT's default method
used to read :LATTICE/ADDRESS off the node and destructure the cons itself --
so the extension's private property key and its private representation of an
address were both hard-coded into src/policy/, by the shipped default, on the
argument that the lattice \"would sensibly add the viewport\" to the echo area.
The lattice never overrode ECHO-CONTENT; it did not have to, because the core
had already done the work for it.

Gate 3 checks that the lattice touches no core and could not see this, because
it was the core touching the *lattice*.  It checks both directions now."))

(defmethod cursor-place-name ((policy appearance-policy) world)
  "The cursor path, dotted: the fallback every layout model can answer."
  (format nil "~{~a~^.~}" (c:world-cursor world)))

(defmethod echo-content ((policy appearance-policy) world &optional (columns 120))
  "The shipped echo area: workspace, place, contents, counts, last message.

The last segment is the one that gets cut, so the last segment is the one that
takes the budget: a message is truncated at a word boundary and the standing
key hint is dropped whole rather than shown as a fragment.  Which is right way
round — the message is about what just happened and the hint is about what is
always true, so the hint is the one that can wait for a quieter line."
  (let* ((root (c:world-root world))
         (workspaces (c:world-workspaces world))
         (window (c:world-focus-window world))
         (leaf (c:world-leaf-at world))
         (segments '()))
    ;; NAMED, NOT JUST NUMBERED.  This used to render a bare [2/5], and the
    ;; identical three characters appeared for a *tab strip* one level down —
    ;; so one object with three names showed the same badge for two of them and
    ;; a user who had not read the design document had no way to tell which.
    ;; The model stays collapsed; the vocabulary stops being.
    (when workspaces
      (push (cons (format nil "~a ~d/~d"
                          (world-role-name world workspaces :policy policy)
                          (1+ (c:container-selection workspaces))
                          (c:container-count workspaces))
                  :normal)
            segments))
    ;; Where the cursor is, in whatever terms the layout makes available --
    ;; asked, not assumed.  See CURSOR-PLACE-NAME for what this line used to be
    ;; and why it is a generic now.
    (push (cons (or (guarded "cursor-place-name" (cursor-place-name policy world))
                    "")
                :accent)
          segments)
    (push (cons (cond ((null leaf) "")
                      ((c:leaf-empty-p leaf)
                       (format nil "empty -- ~{~a~^ ~} to open"
                               (mapcar (lambda (entry) (string (car entry)))
                                       *empty-pane-keys*)))
                      ;; ASKED, NOT READ.  This used to name the window by its
                      ;; app id, so a window rule's :LABEL -- documented,
                      ;; honoured into a property, and read by nothing -- could
                      ;; not reach the one line that says what you are looking
                      ;; at.  See WINDOW-NAME.
                      (window (window-name policy window))
                      (t ""))
                :normal)
          segments)
    (let ((count (c:node-window-count root))
          (scratch (length (c:world-scratchpad world)))
          (floats (length (c:world-floats world))))
      (push (cons (format nil "~d window~:p~@[ ~d float~:p~]~@[ ~d hidden~]"
                          count (and (plusp floats) floats)
                          (and (plusp scratch) scratch))
                  :normal)
            segments))
    ;; RECORDING, and standing rather than announced.  The message below says
    ;; that a capture *started*, and then scrolls away; the thing you actually
    ;; need is the one fact your own screen cannot show you, for as long as it
    ;; is true.  Pushed only when there is something to say, so the line is
    ;; exactly what it was on a machine nobody is recording.
    (let ((captures (c:world-captures world)))
      (when captures
        (push (cons (if (rest captures)
                        (format nil "REC ~d" (length captures))
                        "REC")
                    :accent)
              segments)))
    ;; A message wins the space when there is one -- it is about what just
    ;; happened, and the hint is about what is always true.
    (let ((message (current-message))
          (hint (keys-hint policy world))
          ;; What is left after everything above, with three characters per
          ;; separator: " | ".  The separators are what makes this arithmetic
          ;; rather than a LENGTH, and forgetting them is how a budget comes out
          ;; two words optimistic.
          (room (- columns
                   (reduce #'+ (mapcar (lambda (segment) (length (car segment)))
                                       segments)
                           :initial-value 0)
                   (* 3 (length segments)))))
      ;; EIGHT, not one.  Something cut down to "..." has told you that there
      ;; was news and taken away what it was, which is the worst of both -- and
      ;; it can only arise on a line whose fixed segments already fill the
      ;; screen, where the honest answer is that there is no room.  The echo
      ;; area's own cut is the backstop if even those do not fit.
      ;;
      ;; SHORTENED RATHER THAN DROPPED, and the hint is the case that decides
      ;; it.  With the lattice loaded the hint is about 130 characters and a
      ;; 1280-pixel screen holds 155 of them all told, so `whole or nothing'
      ;; means the beginner's hint disappears on an ordinary laptop -- and the
      ;; sentence this option exists because of is "i have no clue how to close
      ;; windows".  Four bindings and an ellipsis are worth more than none, and
      ;; TRUNCATE-TEXT cuts at a word boundary, so what is left ends at the end
      ;; of something.
      (cond ((and message (>= room 8))
             (push (cons (truncate-text message room) :accent) segments))
            ((and hint (>= room 8))
             (push (cons (truncate-text hint room) :normal) segments))))
    (nreverse segments)))

(defun keymap-choices (policy keymap)
  "KEYMAP's bindings as (KEYS . DESCRIPTION), merged the way the help screen
merges them: two keys that do the same thing are one choice with two keys on
it, not two choices."
  (let ((by-description '()))
    (loop for (key . target) in (keymap-keys keymap)
          for description = (binding-description policy target)
          for entry = (assoc description by-description :test #'string=)
          do (if entry
                 (setf (cdr entry) (append (cdr entry) (list (keysym-name (car key)))))
                 (push (cons description (list (keysym-name (car key))))
                       by-description)))
    (mapcar (lambda (entry)
              (cons (format nil "~{~a~^/~}" (rest entry)) (first entry)))
            (nreverse by-description))))

(defun pending-keymap-segments (policy &optional (columns 120))
  "What an armed chord offers, as echo-area segments, inside COLUMNS.

which-key, in a window manager: the moment you press the first key of a chord
the echo area lists what the second key can be, built from the live submap and
each command's own docstring.  A chord you have to remember is a chord you will
not use, and the only reason Emacs's C-x map is usable by anybody is that
somebody eventually wrote this.

The budget is arithmetic rather than clipping.  Letting the compositor cut the
line at the screen edge is what it did first, and the last choice on the line
then read as a word that had lost its ending — which is worse than not
offering it, because it looks like a bug rather than like a list that goes on."
  (let* ((keymap *pending-keymap*)
         (label (format nil "~a-" (or (keymap-name keymap) "prefix")))
         (choices (keymap-choices policy keymap))
         (room (- columns (length label) 4))
         (shown '()))
    (loop for (keys . description) in choices
          ;; Each choice is at least its keys plus a word of explanation; if
          ;; even that does not fit, everything after it is `+n more'.
          for text = (format nil "~a ~a" keys (truncate-text description 26))
          while (> room (+ (length text) 8))
          do (push (cons text :normal) shown)
             (decf room (+ (length text) 3)))
    (let ((left (- (length choices) (length shown))))
      (cons (cons label :prompt)
            (nreverse (if (plusp left)
                          (cons (cons (format nil "+~d more" left) :dim) shown)
                          shown))))))

;;; ==================================================================
;;; HOW THE SYSTEM DESCRIBES ITSELF
;;; ==================================================================
;;;
;;; These five turn the live keymap and the live command registry into rows of
;;; text: which bindings are worth showing, how a binding describes itself,
;;; what a describe-command screen contains, and which dozen keys a new user
;;; sees first.
;;;
;;; They were plain functions in src/runtime/, and the ruling that moved
;;; everything else applies to them exactly: a decision should be
;;; *specializable*, not merely redefinable.  Redefining a function in a live
;;; image works and is half the point of the language, but it replaces the
;;; shipped answer rather than extending it -- there is no CALL-NEXT-METHOD on
;;; a DEFUN, so "the usual list, plus mine" has to become "a copy of the usual
;;; list, plus mine", which is the thing that goes stale.
;;;
;;; WELCOME-ROWS is the one worth being embarrassed about: it was written two
;;; sessions after the ruling and still shipped as a DEFUN.

(defmethod binding-description ((policy appearance-policy) target)
  "A short description of what a key does.

Prefers the command's own docstring — its first line, which is written to be
exactly this — over the command name, because the name is usually the least
informative thing available."
  (etypecase target
    (null "")
    (keymap (format nil "+ ~a..." (or (keymap-name target) "prefix")))
    (function "a function")
    (string (binding-description policy (list target)))
    (cons
     (let* ((command (find-command (first target)))
            (text (summary-of (and command (command-documentation command))))
            (arguments (remove-if #'keywordp (rest target)
                                  :key (lambda (x) (and (keywordp x) x)))))
       (declare (ignore arguments))
       (cond
         ((null command) (format nil "~{~(~a~)~^ ~}" target))
         ((null text) (format nil "~{~(~a~)~^ ~}" target))
         (t (substitute-arguments text command (rest target))))))))

(defmethod help-entries ((policy appearance-policy) keymap)
  "Every binding as (KEYS . DESCRIPTION), sorted for reading.

Bindings that do the same thing are merged onto one row, because the shipped
keymap deliberately binds both the vi letters and the arrow keys — the arrows
are what somebody uses on their first day and the letters are what they use on
their hundredth — and listing each twice would make the help screen twice as
long while saying nothing extra.

Sorted by *what the key does* rather than by the key, so the four directions of
one verb end up together and the screen reads as a set of verbs rather than as
an alphabet."
  (let ((by-description (make-hash-table :test #'equal))
        (order '()))
    (dolist (row (keymap-keys keymap))
      (destructuring-bind (key . target) row
        (let ((description (binding-description policy target)))
          (unless (gethash description by-description) (push description order))
          (push (key-to-string key) (gethash description by-description)))))
    (sort (loop for description in order
                collect (cons (format nil "~{~a~^ / ~}"
                                      (sort (gethash description by-description)
                                            #'< :key #'length))
                              description))
          #'string< :key #'cdr)))

(defmethod keys-running ((policy appearance-policy) name)
  "Every key bound to the command called NAME, as a printable string.

Emacs's `where-is', folded into describe-command because the question `what
does this do' and the question `how do I do it without typing its name' are
asked at the same moment."
  (let ((keys (loop for (key . target) in (all-bound-keys)
                    when (and (consp target)
                              (stringp (first target))
                              (string-equal name (first target)))
                      collect (key-to-string key))))
    (when keys (format nil "~{~a~^, ~}" (sort keys #'string<)))))

(defmethod command-help-rows ((policy appearance-policy) command)
  "COMMAND's documentation, its arguments and its keys, as overlay rows."
  (let ((rows '())
        (keys (keys-running policy (command-name command))))
    (push (cons "" (format nil "(~a~{ ~(~a~)~})" (command-name command)
                           (command-lambda-list command)))
          rows)
    (push (cons "" "") rows)
    (dolist (line (wrap-text (or (command-documentation command) "Undocumented.")
                             78))
      (push (cons "" line) rows))
    (let ((arguments (remove nil (command-arguments command) :key #'second)))
      (when arguments
        (push (cons "" "") rows)
        (dolist (argument arguments)
          (destructuring-bind (symbol type kind) argument
            (let ((argument-type (argument-type type)))
              (push (cons "" (format nil "  ~(~a~) (~(~a~)~@[, ~(~a~)~])~@[ -- ~a~]"
                                     symbol type
                                     (unless (eq kind :required) kind)
                                     (and argument-type
                                          (argument-type-documentation argument-type))))
                    rows))))))
    (push (cons "" "") rows)
    (push (cons "" (if keys
                       (format nil "Bound to ~a." keys)
                       "Not bound to any key -- reach it with M-x."))
          rows)
    (nreverse rows)))

(defmethod welcome-rows ((policy appearance-policy))
  "The important keys, as overlay rows.

Deliberately about a dozen entries.  The full keymap is one key away and is a
*reference*; this is the smaller thing a reference cannot be — the six or so
facts somebody needs before they can use the machine at all, ending with how
to get out.

Both halves are derived: the key names from *MODIFIER*, so rebinding it moves
every row, and the descriptions from each command's own docstring, so they
cannot drift from what the command actually does."
  (let ((mod (string-capitalize (string *modifier*))))
    (flet ((row (keys command &optional text)
             (let ((found (find-command command)))
               (cons keys (or text
                              (summary-of (and found
                                               (command-documentation found)))
                              command)))))
      (list
       (row (format nil "~a+Return" mod) "terminal")
       (row (format nil "~a+d / ~a+s" mod mod) "split"
            "Split the focused pane, side by side or stacked")
       (row (format nil "~a+h j k l" mod) "focus"
            "Move focus -- the arrow keys do this too")
       (row (format nil "~a+q" mod) "close")
       (row (format nil "~a+1 ... ~a+0" mod mod) "workspace"
            "Go to a workspace")
       (row (format nil "~a+Space" mod) "toggle-float")
       (cons "" "")
       (row (format nil "~a+/" mod) "help"
            "Every key binding, with what it does")
       (row (format nil "~a+x" mod) "run-command-by-name"
            "Run any command by name")
       (row (format nil "Shift+~a+?" mod) "describe-key"
            "Ask about a key, a command or a setting")
       (row (format nil "~a+;" mod) "eval-expression"
            "Evaluate a Lisp form inside the running window manager")
       (cons "" "")
       (row (format nil "Shift+~a+Escape" mod) "quit")))))
