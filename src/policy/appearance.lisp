;;;; policy/appearance.lisp --- What the window manager draws, as decisions.
;;;;
;;;; GATE 6, AND WHY THIS FILE EXISTS
;;;;
;;;; Gate 6 reports (model + policy + lattice) against (wire + runtime), on the
;;;; argument in PLAN §extensibility-real: Lisp is not what kept Emacs alive,
;;;; the *ratio* is.  1.3 million lines of Elisp on 400,000 of C means every
;;;; feature is a worked example of how to write a feature.  Vim, Neovim and
;;;; Hyprland all have a scripting language and none of them is Emacs; the
;;;; boundary is not the disease, how little of the system lives above it is.
;;;;
;;;; The number went 1.20 -> 0.82 -> 0.80 as the drawing and interaction
;;;; subsystem was built, and PLAN §log2 left the question open: is gate 6
;;;; measuring the wrong pair, or is the decomposition failing?  §log3 rules:
;;;; *neither*.  The widget layer should become extensible, and the ratio
;;;; should move because code crosses the line — not because the line moves.
;;;;
;;;; Gate 6 counts by directory, which is not a flaw in it.  It is the gate
;;;; insisting the decomposition be real: a DEFINE-OPTION that lives in
;;;; src/runtime/ is still something a user has to read the runtime to find.
;;;;
;;;; So the split here is the Emacs one.  The C core does redisplay primitives;
;;;; Elisp decides what goes on the mode line.  Shared-memory buffers, the fd
;;;; path, glyph blitting and surface lifecycle stay in src/runtime/.  Colours,
;;;; scales, what a status line is made of, how many columns a help screen
;;;; uses, and *which font gets drawn for which role* are decisions, and they
;;;; live here.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; FONTS
;;; ==================================================================
;;;
;;; The window manager shipped with exactly one font, wired in as three
;;; constants and a lookup table, because it draws short strings that are read
;;; at a glance and Pango would have been a C font stack in the way of the one
;;; requirement that matters -- surviving without a maintainer.  That reasoning
;;; still holds for the *default*.  It was never an argument for the font being
;;; unchangeable.
;;;
;;; A FONT here is data: a name, a cell size, and a glyph table.  The generated
;;; Terminus table registers itself as one at load time, LOAD-PSF makes one out
;;; of any console font on disk, and FONT-FOR decides which is used where.  A
;;; user who wants their terminal's font in the echo area writes two lines.

(defstruct (font (:constructor %make-font))
  "A bitmap font: fixed cell, one byte per row, bit 7 leftmost.

Deliberately a struct of plain data rather than a class with behaviour.  A
font is a table somebody may want to load from a file, hand to a method, or
build in a REPL, and none of that wants a protocol."
  (name "unnamed" :type string)
  (width 8 :type fixnum)
  (height 16 :type fixnum)
  (stride 1 :type fixnum)
  (first-code 32 :type fixnum)
  (glyphs (make-array 0 :element-type '(unsigned-byte 8))
          :type (array (unsigned-byte 8) (*))))

(defun make-font (name width height first-code glyphs)
  "A font called NAME whose glyphs are WIDTH by HEIGHT, starting at FIRST-CODE.

STRIDE -- the bytes each row occupies -- is derived rather than passed, because
it is a fact about WIDTH and there is no combination of the two that is
meaningful and disagrees."
  (%make-font :name name :width width :height height
              :stride (ceiling width 8)
              :first-code first-code :glyphs glyphs))

(defvar *fonts* (make-hash-table :test #'equal)
  "Every registered font, by name.  See REGISTER-FONT and FIND-FONT.")

(defvar *default-font* nil
  "The font used when nothing more specific is asked for.

Set by the generated font table at load time.  A defvar rather than a
define-option because it holds a font *object*: the option a user sets is
*UI-FONT*, which holds a name and can therefore appear in a config file and
survive being written down.")

(defun register-font (font)
  "Add FONT to the registry under its own name, replacing any of that name."
  (setf (gethash (font-name font) *fonts*) font))

(defun find-font (name)
  "The registered font called NAME, or NIL.  A font itself passes through,
so anywhere a font name is accepted a font object is too."
  (etypecase name
    (null nil)
    (font name)
    (string (gethash name *fonts*))
    (symbol (gethash (string-downcase (symbol-name name)) *fonts*))))

(defun font-names ()
  "Every registered font name, sorted."
  (sort (loop for name being the hash-keys of *fonts* collect name) #'string<))

(define-option *ui-font* nil
  "The font to draw interface text in, by name, or NIL for the built-in one.

    (setf *ui-font* \"ter-118n\")

Names come from FONT-NAMES.  The shipped Terminus is registered as
\"terminus\".  LOAD-PSF registers any console font on disk under its own file
name, so putting a different one everywhere is two lines in a config file:

    (register-font (load-psf \"/usr/share/kbd/consolefonts/ter-124n.psf.gz\"))
    (setf *ui-font* \"ter-124n\")

Per-role control is one method rather than one option -- see FONT-FOR.")

(defgeneric font-for (policy role)
  (:documentation
   "Which font should ROLE be drawn in?

ROLE is a keyword naming a place text appears: :ECHO for the status line,
:MINIBUFFER for the prompt, :HELP for the keymap and describe screens, :HINT
for the empty-pane hint, :OVERLAY for the coordinate overlay, :MAP for the
drawn map, and :DEFAULT for anything that has not been given a role.

The shipped answer is the same font everywhere -- *UI-FONT* if it names one,
otherwise the built-in Terminus -- because one font everywhere is what a
window manager should look like until somebody decides otherwise.  Deciding
otherwise is one method:

    (defmethod font-for ((policy conventional-policy) (role (eql :map)))
      (find-font \"ter-112n\"))

A role is a keyword rather than a subclass so that an extension can invent
one -- a widget the core has never heard of asks FONT-FOR with its own
keyword and inherits the default answer for free."))

(defmethod font-for ((policy policy) role)
  (declare (ignore role))
  (or (find-font *ui-font*) *default-font*))

;;; ------------------------------------------------------------- metrics
;;;
;;; Pure functions of a font and a string.  They live here rather than beside
;;; the blitter because "how wide is this text" is the question every layout
;;; decision on screen is made of, and a policy that cannot answer it cannot
;;; decide anything about a widget's shape.

(defun glyph-row (font character row)
  "Row ROW of CHARACTER in FONT, as an integer of (* 8 STRIDE) bits.

The leftmost pixel is the *highest* bit -- bit 7 for an eight-pixel font, bit
15 for a font up to sixteen -- so a caller walks columns with

    (logbitp (- (* 8 (font-stride font)) 1 column) bits)

which is the same expression at every width.  Fonts wider than eight pixels
are not a corner case to tolerate: Terminus ships 8, 10, 11, 12, 14 and 16
pixels wide, and every size above the smallest is one of the wide ones, so a
one-byte-per-row representation would have refused every font somebody with a
high-resolution screen actually wants.

Out of range in either direction is a blank row rather than an error: a font
that does not cover a character should leave a gap, not stop the window
manager from drawing the rest of the line."
  (let* ((code (char-code character))
         (height (font-height font))
         (stride (font-stride font))
         (first-code (font-first-code font))
         (glyphs (font-glyphs font))
         (start (+ (* (- code first-code) height stride) (* row stride))))
    (if (and (<= first-code code) (< -1 row height)
             (<= 0 start) (<= (+ start stride) (length glyphs)))
        (loop with bits = 0
              for i from 0 below stride
              do (setf bits (logior (ash bits 8) (aref glyphs (+ start i))))
              finally (return bits))
        0)))

(defun font-text-width (font string &key (scale 1) (tracking 0))
  "How wide STRING will be drawn in FONT, in pixels."
  (let ((advance (* scale (+ (font-width font) tracking))))
    (max 0 (- (* advance (length string)) (* scale tracking)))))

(defun font-text-height (font &key (scale 1))
  "How tall one line of FONT is, in pixels."
  (* scale (font-height font)))

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

(define-option *overlay-buffer-idle* t
  "Release an overlay's pixel buffer while it is hidden.

A full-screen ARGB buffer is about four megabytes, and the help screen and the
drawn map are each hidden almost all of the time.  Keeping their buffers costs
eight megabytes of resident memory to save one allocation on a keypress, which
is the wrong way round.

Set to NIL if you would rather have the allocation happen once.")

;;; ==================================================================
;;; TEXT, AS IT READS
;;; ==================================================================
;;;
;;; Pure functions of a string, and every one of them is a decision somebody
;;; may disagree with: where a summary ends, where a line breaks, what a
;;; truncation looks like.  They were in src/runtime/help.lisp beside the
;;; blitter, which made them look like part of drawing.  They are not -- the
;;; blitter puts pixels down, and these decide what the pixels say.

(defun summary-of (string)
  "The first *sentence* of STRING, which is the part written to be a summary.

Docstrings here open with a one-line summary and then explain themselves, and
the explanation is what makes a help screen unreadable.  Cutting at the first
full stop or dash keeps the sentence and drops the essay."
  (when string
    (let* ((line (subseq string 0 (or (position #\Newline string) (length string))))
           ;; The *earliest* of the candidates, not the first one that happens
           ;; to be found.  An OR here takes whichever test is written first,
           ;; so a full stop at the end of the line beat an em dash in the
           ;; middle and the whole clause survived.
           (stops (remove nil (list (position #\. line)
                                    (search " -- " line)
                                    (position (code-char 8212) line))))
           (stop (when stops (reduce #'min stops)))
           ;; Keep a full stop, drop a dash.  A sentence ends with its full
           ;; stop and reads wrong without one; a dash is the *start* of the
           ;; clause being cut, and keeping it leaves "Move the cursor one
           ;; pane left —" trailing off mid-thought on every help screen.
           (stop (when stop
                   (if (char= #\. (char line stop)) stop (1- stop)))))
      (if stop
          (string-right-trim " " (subseq line 0 (min (1+ stop) (length line))))
          line))))

(defun truncate-text (text characters)
  "TEXT cut to CHARACTERS, ending in an ellipsis if anything was lost.

Cut at a word boundary where there is one within reach, because a description
that stops mid-word reads as a rendering bug rather than as an abbreviation."
  (if (<= (length text) characters)
      text
      (let* ((room (max 0 (- characters 3)))
             (space (position #\Space text :from-end t :end (min room (length text)))))
        (concatenate 'string
                     (subseq text 0 (if (and space (> space (floor room 2)))
                                        space
                                        room))
                     "..."))))

(defun wrap-text (text width)
  "TEXT broken into lines of at most WIDTH characters, keeping its blank lines.

Docstrings here are paragraphs with a one-line summary on top, and the whole
point of showing one on screen is that the paragraph is where the reasoning
is.  Preserving the blank lines keeps it readable; ignoring them would produce
one grey slab."
  (let ((lines '()))
    (dolist (paragraph (split-lines text) (nreverse lines))
      (if (zerop (length (string-trim " " paragraph)))
          (push "" lines)
          (let ((current ""))
            (dolist (word (split-words paragraph))
              (cond
                ((zerop (length current)) (setf current word))
                ((<= (+ (length current) 1 (length word)) width)
                 (setf current (concatenate 'string current " " word)))
                (t (push current lines) (setf current word))))
            (when (plusp (length current)) (push current lines)))))))

(defun split-lines (text)
  "TEXT split on newlines."
  (loop with start = 0
        for position = (position #\Newline text :start start)
        collect (subseq text start position)
        while position
        do (setf start (1+ position))))

(defun split-words (string)
  "STRING split on runs of whitespace."
  (let ((out '()) (start nil))
    (dotimes (i (length string) (nreverse (if start
                                              (cons (subseq string start) out)
                                              out)))
      (let ((space (member (char string i) '(#\Space #\Tab))))
        (cond ((and space start) (push (subseq string start i) out) (setf start nil))
              ((not (or space start)) (setf start i)))))))

(defun substitute-arguments (text command arguments)
  "Replace a docstring's argument placeholders with the actual arguments.

Command docstrings name their parameters in capitals, the way a lambda list
does — \"Move the cursor one pane DIRECTION\" — so a binding of that command to
a particular direction can read as an ordinary sentence rather than as a
template with the hole still in it.  The difference between

    Super+h   left Move the cursor one pane DIRECTION
    Super+h   Move the cursor one pane left.

is the difference between a reference and a help screen."
  (let ((result text))
    (loop for parameter in (command-lambda-list command)
          for argument in arguments
          for name = (string parameter)
          do (unless (char= #\& (char name 0))
               (let ((position (search name result :test #'char-equal)))
                 (when position
                   (setf result
                         (concatenate 'string (subseq result 0 position)
                                      (string-downcase (princ-to-string argument))
                                      (subseq result (+ position (length name)))))))))
    result))

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

(defmethod echo-content ((policy policy) world)
  "The shipped echo area: workspace, place, contents, counts, last message."
  (let* ((root (c:world-root world))
         (workspaces (c:world-workspaces world))
         (window (c:world-focus-window world))
         (leaf (c:world-leaf-at world))
         (segments '()))
    (when workspaces
      (push (cons (format nil "[~d/~d]" (1+ (c:stack-selected workspaces))
                          (c:container-count workspaces))
                  :normal)
            segments))
    ;; Where the cursor is, in whatever terms the layout makes available.  The
    ;; lattice puts a coordinate on the node; without it, the path is the
    ;; honest answer and is still better than nothing.
    (let* ((node (c:world-node-at world))
           (place (or (and node (c:prop node :lattice/address)
                           (format nil "~d,~d"
                                   (car (c:prop node :lattice/address))
                                   (cdr (c:prop node :lattice/address))))
                      (format nil "~{~a~^.~}" (c:world-cursor world)))))
      (push (cons place :accent) segments))
    (push (cons (cond ((null leaf) "")
                      ((c:leaf-empty-p leaf)
                       (format nil "empty -- ~{~a~^ ~} to open"
                               (mapcar (lambda (entry) (string (car entry)))
                                       *empty-pane-keys*)))
                      (window (or (c:window-app-id window) "?"))
                      (t ""))
                :normal)
          segments)
    (let ((count (length (c:node-windows root)))
          (scratch (length (c:world-scratchpad world)))
          (floats (length (c:world-floats world))))
      (push (cons (format nil "~d window~:p~@[ ~d float~:p~]~@[ ~d hidden~]"
                          count (and (plusp floats) floats)
                          (and (plusp scratch) scratch))
                  :normal)
            segments))
    (let ((message (current-message)))
      (when message (push (cons message :accent) segments)))
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

(defmethod binding-description ((policy policy) target)
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

(defmethod help-entries ((policy policy) keymap)
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

(defmethod keys-running ((policy policy) name)
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

(defmethod command-help-rows ((policy policy) command)
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

(defmethod welcome-rows ((policy policy))
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
