;;;; tests/test-minibuffer.lisp --- Reading a line from the user.
;;;;
;;;; A window manager's prompt looks like the least testable thing in it — it
;;;; is drawn on a compositor surface and driven by protocol events — and is in
;;;; fact one of the most testable, because everything that matters happens
;;;; between a keysym arriving and a string changing.  Both ends are ours.
;;;;
;;;; These are here because the interactive layer is the part a user touches
;;;; most and notices first.  A completion that ranks badly, a C-w that eats
;;;; one character too many, an argument prompt that fires in the wrong order:
;;;; none of those is a crash, all of them are the difference between a system
;;;; somebody keeps and one they uninstall, and none of them would ever be
;;;; caught by the model tests next door.

(in-package #:latticewm/tests)
(in-suite minibuffer)

(defun edit (input point &rest keys)
  "Apply KEYS to INPUT with the caret at POINT, returning (INPUT POINT).

A key is a character, a (CHARACTER . MODIFIERS) cons, or the name of one of
the non-printing keys."
  (let ((r::*prompt* "test> ")
        (r::*input* input)
        (r::*point* point)
        (r::*completions* '())
        (r::*kill* "")
        (r::*prompt-callback* (lambda (value) (declare (ignore value))))
        (r::*history-name* nil)
        (r::*history-index* nil))
    (dolist (key keys (list r::*input* r::*point*))
      (etypecase key
        (character (r::prompt-key (char-code key) '() key))
        (cons (r::prompt-key (char-code (car key)) (cdr key) (car key)))
        (symbol (r::prompt-key (cdr (assoc key r::+prompt-keysyms+)) '() nil))))))

;;; ------------------------------------------------------------- editing

(test typing-inserts-at-the-caret
  (is (equal '("abc" 3) (edit "" 0 #\a #\b #\c)))
  (is (equal '("aXbc" 2) (edit "abc" 3 :left :left #\X))
      "the caret is a position, not the end of the string")
  (is (equal '("Xabc" 1) (edit "abc" 3 :home #\X)))
  (is (equal '("abcX" 4) (edit "abc" 0 :end #\X))))

(test the-caret-cannot-leave-the-string
  (is (equal '("abc" 0) (edit "abc" 0 :left :left :left)))
  (is (equal '("abc" 3) (edit "abc" 3 :right :right))))

(test deleting-in-both-directions
  (is (equal '("ab" 2) (edit "abc" 3 :backspace)))
  (is (equal '("bc" 0) (edit "abc" 0 :delete)))
  (is (equal '("abc" 0) (edit "abc" 0 :backspace))
      "backspace at the start of the line is a no-op, not an error")
  (is (equal '("abc" 3) (edit "abc" 3 :delete))))

(test readline-chords
  (is (equal '("hello" 5) (edit "hello world" 5 '(#\k . (:ctrl)))))
  (is (equal '("world" 0) (edit "hello world" 6 '(#\u . (:ctrl)))))
  (is (equal '("hXello" 2) (edit "hello" 5 '(#\a . (:ctrl)) '(#\f . (:ctrl)) #\X)))
  (is (equal '("ab" 2) (edit "ab" 2 '(#\z . (:ctrl))))
      "a chord the prompt does not know is swallowed, not typed as its letter"))

(test kill-a-word-at-a-time
  ;; A command name is hyphenated, and correcting the last part of one is the
  ;; single commonest edit anybody makes at this prompt.
  (is (equal '("send-to" 7) (edit "send-to-work" 12 '(#\w . (:ctrl)))))
  (is (equal '("send" 4) (edit "send-to-work" 12 '(#\w . (:ctrl)) '(#\w . (:ctrl)))))
  (is (equal '("" 0) (edit "one" 3 '(#\w . (:ctrl)))))
  (is (equal '("" 0) (edit "" 0 '(#\w . (:ctrl))))))

(test yank-puts-back-what-was-killed
  (is (equal '("hello world" 6)
             (edit "hello world" 6 '(#\u . (:ctrl)) '(#\y . (:ctrl))))
      "C-u then C-y is a round trip")
  (is (equal '("worldhello " 11)
             (edit "hello world" 6 '(#\u . (:ctrl)) :end '(#\y . (:ctrl))))
      "and yanking somewhere else is how text gets moved")
  (is (equal '("a" 1) (edit "abc" 3 :backspace :backspace '(#\y . (:ctrl))))
      "backspace does not fill the kill, so C-y after it puts nothing back"))

;;; ------------------------------------------------------------ history

(test history-is-a-ring-of-distinct-entries
  (let ((r::*histories* (make-hash-table :test #'equal)))
    (r::history-push "t" "one")
    (r::history-push "t" "two")
    (r::history-push "t" "one")
    (is (equal '("one" "two") (r::history "t"))
        "saying the same thing twice does not make it two entries")
    (r::history-push "t" "")
    (is (equal '("one" "two") (r::history "t")) "an empty line is not history")))

(test walking-the-history-and-coming-back
  (let ((r::*histories* (make-hash-table :test #'equal)))
    (r::history-push "t" "older")
    (r::history-push "t" "newer")
    (let ((r::*prompt* "> ") (r::*input* "half-typed") (r::*point* 10)
          (r::*history-name* "t") (r::*history-index* nil) (r::*history-saved* nil)
          (r::*completions* '()) (r::*prompt-callback* nil))
      (r::history-walk -1)
      (is (equal "newer" r::*input*))
      (r::history-walk -1)
      (is (equal "older" r::*input*))
      (r::history-walk -1)
      (is (equal "older" r::*input*) "the far end of the ring is a wall, not a wrap")
      (r::history-walk 1)
      (r::history-walk 1)
      (is (equal "half-typed" r::*input*)
          "coming back out restores what was being typed before")
      (is (= 10 r::*point*)))))

;;; --------------------------------------------------------- completion

(test completion-ranks-prefix-then-substring-then-subsequence
  (let ((candidates '("close" "close-float" "toggle-float" "clear")))
    (is (equal '("close" "close-float")
               (p:complete-candidates (policy) "close" candidates)))
    (is (equal '("clear" "close" "close-float")
               (p:complete-candidates (policy) "cl" candidates))
        "shortest first inside a group, and alphabetical inside a length")
    (is (equal '("close-float" "toggle-float")
               (p:complete-candidates (policy) "float" candidates))
        "a substring match is a match, and comes after every prefix match")))

(test completion-finds-a-command-from-its-initials
  (is (equal '("workspace" "new-workspace" "send-to-workspace")
             (p:complete-candidates (policy) "wsp"
                                    '("send-to-workspace" "workspace"
                                      "new-workspace"))))
  (is (null (p:complete-candidates (policy) "zq" '("workspace"))))
  (is (p:subsequence-match-p "wsp" "send-to-workspace"))
  (is (not (p:subsequence-match-p "spw" "send-to-workspace"))
      "in order, or it is not a subsequence"))

(test tab-expands-only-as-far-as-the-candidates-agree
  (let ((r::*prompt* "M-x ") (r::*input* "clo") (r::*point* 3)
        (r::*completions* '("close" "close-float" "clear"))
        (r::*prompt-callback* nil))
    (r::complete-input)
    (is (equal "close" r::*input*) "the common prefix, not the first candidate")
    (is (= 5 r::*point*))))

(test the-prompt-draws-exactly-one-caret-in-the-right-place
  (let ((r::*prompt* "M-x ") (r::*input* "clo") (r::*point* 2)
        (r::*completions* '("close")) (r::*prompt-callback* nil))
    (is (= 1 (count :caret (r::prompt-segments) :key #'cdr)))
    (is (equal '("M-x " "cl" "o" "se")
               (mapcar #'car (remove :caret (r::prompt-segments) :key #'cdr)))
        "the input is split around the caret, and `se' is the hint")))

;;; ------------------------------------------- commands with arguments

(test the-naming-convention-covers-the-shipped-commands
  ;; The claim this rests on: a parameter named to read well in a docstring is
  ;; already named after what kind of thing it is.  If that ever stops being
  ;; true this test says which command broke it.
  (is (equal '((r::direction :direction :required))
             (r::command-arguments (r::find-command "focus"))))
  (is (equal '((r::direction :direction :required) (r::amount :fraction :optional))
             (r::command-arguments (r::find-command "resize"))))
  (is (equal '((r::number :number :required))
             (r::command-arguments (r::find-command "workspace"))))
  (is (equal '((r::command :shell-command :rest))
             (r::command-arguments (r::find-command "spawn")))
      "spawn's COMMAND is a shell command line, and says so at the command"))

(test every-command-but-one-can-be-run-from-m-x
  ;; The one is FOCUS-PATH, whose argument is a list of integers naming a place
  ;; in the tree.  Nobody types that, and the point of the exception is that
  ;; M-x says so rather than putting up a prompt that cannot be answered.
  (let ((refused (remove-if #'r::command-interactive-p (r::all-commands))))
    (is (equal '("focus-path") (mapcar #'r::command-name refused)))))

(test arguments-parse-or-say-why-not
  (flet ((parse (type text)
           (funcall (r::argument-type-parser (r::argument-type type)) text)))
    (is (eq :left (parse :direction "left")))
    (is (eq :left (parse :direction " LEFT ")))
    (is (= 12 (parse :number " 12 ")))
    (is (= -3 (parse :number "-3")))
    (is (= 1/20 (parse :fraction "1/20")))
    (is (= 0.05 (parse :fraction "0.05")))
    (signals error (parse :direction "sideways"))
    (signals error (parse :number "soon"))
    (signals error (parse :fraction "a lot"))))

(test a-rest-argument-is-a-command-line
  (is (equal '("firefox" "--new-window")
             (r::split-words "  firefox  --new-window ")))
  (is (null (r::split-words "   "))))

;;; ------------------------------------------------------------- help

(test documentation-wraps-and-keeps-its-paragraphs
  (is (equal '("one two" "three" "four") (r::wrap-text "one two three four" 9)))
  (is (equal '("a" "" "b") (r::wrap-text (format nil "a~%~%b") 10))
      "a blank line survives, because that is where the paragraph was")
  (is (equal '("supercalifragilistic") (r::wrap-text "supercalifragilistic" 5))
      "a word longer than the line is not cut in half"))
