;;;; runtime/help.lisp --- The keymap, on screen.
;;;;
;;;; Emacs's actual advantage over every editor that copied its keybindings is
;;;; not the bindings.  It is that you can always ask.  C-h k, C-h b, and the
;;;; fact that every command carries its own documentation mean the system is
;;;; *discoverable* — you are never more than one keystroke from finding out
;;;; what a key does or what keys exist.
;;;;
;;;; A window manager is worse off than an editor here, because it has nowhere
;;;; to print to.  It has a log file nobody reads and a manual page that goes
;;;; stale.  So the keymap gets drawn on the screen, from the live keymap, with
;;;; each binding's real command and that command's real docstring — which
;;;; means it cannot be wrong and cannot go out of date, including for bindings
;;;; the user added five minutes ago at a REPL.
;;;;
;;;; This is the same argument as the generated extension-surface document, one
;;;; level down: documentation derived from the running system rather than
;;;; maintained beside it.

(in-package #:latticewm/runtime)

(defvar *help-overlay* nil)

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
           (stop (when stops (reduce #'min stops))))
      (if stop
          (string-right-trim " " (subseq line 0 (min (1+ stop) (length line))))
          line))))

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

(defun binding-description (target)
  "A short description of what a key does.

Prefers the command's own docstring — its first line, which is written to be
exactly this — over the command name, because the name is usually the least
informative thing available."
  (etypecase target
    (null "")
    (keymap (format nil "+ ~a..." (or (keymap-name target) "prefix")))
    (function "a function")
    (string (binding-description (list target)))
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

(defun help-entries (&optional (keymap *keymap*))
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
        (let ((description (binding-description target)))
          (unless (gethash description by-description) (push description order))
          (push (key-to-string key) (gethash description by-description)))))
    (sort (loop for description in order
                collect (cons (format nil "~{~a~^ / ~}"
                                      (sort (gethash description by-description)
                                            #'< :key #'length))
                              description))
          #'string< :key #'cdr)))

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

(defun draw-help-overlay ()
  "Draw the keymap, or hide it."
  (let ((output (first (all-outputs))))
    (unless (and *help-visible* output *server*)
      (when *help-overlay* (overlay-hide *help-overlay*))
      (return-from draw-help-overlay nil))
    (unless *help-overlay*
      (setf *help-overlay* (make-instance 'overlay :name "help")))
    (let* ((area (c:output-rect output))
           (canvas (ensure-overlay *help-overlay* (c:rect-w area) (c:rect-h area))))
      (when canvas
        (let* ((customp (consp *help-visible*))
               (entries (if customp (cdr *help-visible*) (help-entries)))
               (line (+ 4 (text-height :scale p:*help-scale*)))
               (title-top 16)
               (top (+ title-top (* 2 line)))
               (available (max 1 (floor (- (c:rect-h area) top line) line)))
               ;; Prose gets one column and a table gets as many as it needs.
               ;; A paragraph poured into two columns reads as two paragraphs,
               ;; each of them half a sentence wide.
               ;;
               ;; *HELP-COLUMNS* is a preference, not a law: on a short screen
               ;; two columns of forty rows silently lost the last eighteen
               ;; bindings, which is the worst thing a reference can do.  Three
               ;; is the hard stop — past that the descriptions are narrower
               ;; than the keys.
               (columns (if customp
                            1
                            (min 3 (max p:*help-columns*
                                        (ceiling (length entries) available)))))
               (rows (ceiling (length entries) columns))
               (column-width (floor (- (c:rect-w area) 40) columns))
               (key-color (apply #'argb p:*help-key-color*))
               (text-color (apply #'argb p:*help-text-color*))
               ;; One left column, as wide as the widest thing in it, so that
               ;; every description starts at the same x.  Ragged descriptions
               ;; are what made the apropos screen read as a paragraph with
               ;; some words in bold rather than as a table, and the eye scans
               ;; a column for free.
               ;;
               ;; Capped at half the column, because alignment is worth less
               ;; than legibility: one forty-character chord — and the keymap
               ;; has some — would otherwise squeeze every description on the
               ;; screen down to nothing to stay level with it.
               (gap (text-width "  " :scale p:*help-scale*))
               (left (min (floor column-width 2)
                          (+ gap (reduce #'max entries :initial-value 0
                                         :key (lambda (entry)
                                                (text-width (car entry)
                                                            :scale p:*help-scale*))))))
               (shown 0))
          (canvas-fill canvas (apply #'argb p:*help-background*))
          (loop for entry in entries
                for index from 0
                for column = (floor index rows)
                for row = (mod index rows)
                for x = (+ 20 (* column column-width))
                for y = (+ top (* row line))
                while (< y (- (c:rect-h area) line))
                do (incf shown)
                   (let ((used (canvas-text canvas x y (car entry) key-color
                                            :scale p:*help-scale*)))
                     ;; Truncate rather than overflow into the next column: a
                     ;; help screen that overlaps itself is worse than one that
                     ;; abbreviates.
                     (let* ((offset (max left (+ used gap)))
                            (room (max 0 (- column-width offset gap 10)))
                            (fits (max 0 (floor room (text-width "m"
                                                                :scale p:*help-scale*)))))
                       (canvas-text canvas (+ x offset) y
                                    (truncate-text (cdr entry) fits)
                                    text-color :scale p:*help-scale*))))
          (canvas-text canvas 20 title-top
                       (cond
                         (customp (format nil "~a  --  any key closes this"
                                          (car *help-visible*)))
                         ;; What did not fit is said, rather than left to be
                         ;; discovered by somebody looking for a binding that
                         ;; is in fact bound.
                         ((< shown (length entries))
                          (format nil "LatticeWM -- ~d of ~d bindings; the rest ~
                                       are in --list-keys.  Any key closes this."
                                  shown (length entries)))
                         (t (format nil "LatticeWM -- ~d bindings.  ~
                                         Any key closes this."
                                    (length entries))))
                       key-color :scale p:*help-scale*))
        (overlay-commit *help-overlay* :rect area)))))

(defcommand help ()
  "Show every key binding on screen, with what it does.

Built from the live keymap and each command's own docstring, so it includes
anything you bound yourself and cannot go out of date.  Any key dismisses it."
  (setf *help-visible* (not *help-visible*))
  (mark-dirty)
  *help-visible*)

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

(defun keys-running (name)
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

(defun show-help-page (title rows)
  "Put TITLE and ROWS on the help overlay.  Any key takes it down again."
  (setf *help-visible* (cons title rows))
  (mark-dirty)
  t)

(defun command-help-rows (command)
  "COMMAND's documentation, its arguments and its keys, as overlay rows."
  (let ((rows '())
        (keys (keys-running (command-name command))))
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

(defcommand describe-command (name)
  "Show a command's documentation, its arguments and the keys that run it.

Emacs's C-h f, and the reason the docstrings in this system are written as
paragraphs rather than as labels: this is where they are read."
  (:interactive :command)
  (let ((command (find-command name)))
    (if command
        (show-help-page (format nil "M-x ~a" (command-name command))
                        (command-help-rows command))
        (notify "no such command: ~a" name))))

(defcommand describe-option (name)
  "Show a configuration value: what it is now, what it shipped as, and why.

Every option in this system carries a paragraph explaining what turning it off
costs you, and this is where those paragraphs are read."
  (:interactive :option)
  (if (not (p:option-boundp name))
      (notify "there is no option called ~(~a~)" name)
      (show-help-page
       (format nil "~(~a~)" name)
       (append (list (cons "now" (prin1-to-string (p:option name)))
                     (cons "default" (prin1-to-string (p:option-default name)))
                     (cons "" ""))
               (mapcar (lambda (line) (cons "" line))
                       (wrap-text (or (p:option-documentation name)
                                      "Undocumented.")
                                  78))
               (list (cons "" "")
                     (cons "" (format nil "Change it now with M-x set-option, ~
                                           or for good with (setf ~(~a~) ...) ~
                                           in your init.lisp."
                                      (second (assoc name (p:all-options))))))))))

(defcommand apropos-command ()
  "Search every command by name and by documentation, and show what matches.

Emacs's C-h a.  The one question a discoverable system has to be able to
answer is `is there a command for this', and neither the keymap nor M-x can
answer it — the keymap only knows what somebody bound, and M-x only matches
names.  This reads the docstrings, so `screen' finds `toggle-fullscreen' even
though the word does not occur in its name."
  (read-string "apropos: "
               (lambda (text)
                 (when (and text (plusp (length text)))
                   (let ((rows (loop for command in (all-commands)
                                     for documentation = (or (command-documentation
                                                              command) "")
                                     when (or (search text (command-name command)
                                                      :test #'char-equal)
                                              (search text documentation
                                                      :test #'char-equal))
                                       collect (cons (command-name command)
                                                     (or (summary-of documentation)
                                                         "")))))
                     (if rows
                         (show-help-page (format nil "apropos ~s -- ~d command~:p"
                                                 text (length rows))
                                         rows)
                         (notify "nothing matches ~a" text)))))
               :history "apropos"))

(add-hook :draw-overlays 'draw-help-overlay)
