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

(p:define-option *help-columns* 2
  "How many columns of bindings the help overlay uses.

Two, not three.  Three fits more rows and truncates almost every description,
and a help screen where you can read the keys but not what they do has kept
the wrong half.")

(p:define-option *help-background* '(0.07 0.07 0.10 0.94)
  "Help overlay background, as (R G B A).")

(p:define-option *help-key-color* '(0.45 0.70 1.00 1.0)
  "Colour of the key in the help overlay.")

(p:define-option *help-text-color* '(0.78 0.80 0.86 1.0)
  "Colour of the description in the help overlay.")

(p:define-option *help-scale* 1
  "Integer scale factor for help-overlay text.")

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
        (let* ((entries (help-entries))
               (line (+ 4 (text-height :scale *help-scale*)))
               (columns (max 1 *help-columns*))
               (rows (ceiling (length entries) columns))
               (column-width (floor (- (c:rect-w area) 40) columns))
               (key-color (apply #'argb *help-key-color*))
               (text-color (apply #'argb *help-text-color*))
               (title-top 16))
          (canvas-fill canvas (apply #'argb *help-background*))
          (canvas-text canvas 20 title-top
                       (format nil "LatticeWM -- ~d bindings.  Any key closes this."
                               (length entries))
                       key-color :scale *help-scale*)
          (loop for entry in entries
                for index from 0
                for column = (floor index rows)
                for row = (mod index rows)
                for x = (+ 20 (* column column-width))
                for y = (+ title-top (* 2 line) (* row line))
                while (< y (- (c:rect-h area) line))
                do (let ((used (canvas-text canvas x y (car entry) key-color
                                            :scale *help-scale*)))
                     ;; Truncate rather than overflow into the next column: a
                     ;; help screen that overlaps itself is worse than one that
                     ;; abbreviates.
                     (let* ((room (max 0 (- column-width used
                                            (* 2 (text-width " " :scale *help-scale*))
                                            10)))
                            (fits (max 0 (floor room (text-width "m" :scale *help-scale*)))))
                       (canvas-text canvas
                                    (+ x used (text-width " " :scale *help-scale*))
                                    y
                                    (truncate-text (cdr entry) fits)
                                    text-color :scale *help-scale*)))))
        (overlay-commit *help-overlay* :rect area)))))

(defcommand help ()
  "Show every key binding on screen, with what it does.

Built from the live keymap and each command's own docstring, so it includes
anything you bound yourself and cannot go out of date.  Any key dismisses it."
  (setf *help-visible* (not *help-visible*))
  (mark-dirty)
  *help-visible*)

(defcommand describe-command (name)
  "Print a command's full documentation to the echo area and the log."
  (let ((command (find-command name)))
    (if command
        (notify "~a: ~a" (command-name command)
                (let ((documentation (command-documentation command)))
                  (if documentation
                      (subseq documentation 0 (or (position #\Newline documentation)
                                                  (length documentation)))
                      "undocumented")))
        (notify "no such command: ~a" name))))

(add-hook :draw-overlays #'draw-help-overlay)
