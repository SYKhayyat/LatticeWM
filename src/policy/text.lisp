;;;; policy/text.lisp --- Pure functions of a string, each of them a decision.
;;;;
;;;; SPLIT OUT OF POLICY/APPEARANCE.LISP; see policy/font.lisp for why that
;;;; file was three libraries in a trench coat.
;;;;
;;;; Where a summary ends, where a line breaks, what a truncation looks like:
;;;; every one of these is something somebody may reasonably disagree with, and
;;;; every one of them is a pure function of a string, so disagreeing costs a
;;;; redefinition and nothing else.

(in-package #:latticewm/policy)

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
that stops mid-word reads as a rendering bug rather than as an abbreviation.

*IT NEVER RETURNS MORE THAN IT WAS ASKED FOR*, which it used to do: below four
characters there is no room for a word and an ellipsis, and the ellipsis alone
is three, so asking for two got three back.  A function named TRUNCATE-TEXT
that can overrun its budget is the same species of bug as the status line that
made this docstring necessary, one layer down, and its callers are entitled to
believe the number they passed."
  (cond
    ((<= (length text) characters) text)
    ;; No room for a word and a mark, so all that can be said is that
    ;; something was left out -- and even that, only as far as it fits.
    ((< characters 4) (subseq "..." 0 (max 0 (min 3 characters))))
    (t (let* ((room (- characters 3))
              (space (position #\Space text :from-end t :end (min room (length text)))))
         (concatenate 'string
                      (subseq text 0 (if (and space (> space (floor room 2)))
                                         space
                                         room))
                      "...")))))

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
