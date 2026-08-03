;;;; runtime/minibuffer.lisp --- Reading a line from the user.
;;;;
;;;; M-x, M-:, and the reason they are worth having in a window manager at all.
;;;;
;;;; Every command in this system is named, documented, and takes arguments.
;;;; Without a way to invoke one by name, that is a fact about the source code:
;;;; the only commands you can reach are the ones somebody bound to a key, and
;;;; the other twenty are theoretical.  With one, the keymap becomes an
;;;; *optimisation* — the commands you use often — rather than the interface,
;;;; which is exactly the relationship Emacs has and exactly what makes it
;;;; possible to ship a hundred commands without a hundred keys.
;;;;
;;;; HOW A WINDOW MANAGER READS A LINE, given that it has no input focus of its
;;;; own.  River delivers keys to the *window* that has focus, and gives the
;;;; window manager only the keys it asked to be told about.  So reading text
;;;; means binding every key you might read — and then enabling those bindings
;;;; for exactly as long as you are reading.
;;;;
;;;; river_xkb_binding_v1 has enable and disable precisely so a binding can be
;;;; conditional.  Two hundred-odd bindings are created once, on first use, and
;;;; enabled while a prompt is up.  This is the same mechanism DESIGN D19 needs
;;;; for typing in an empty pane, so the two share it: one set of bindings, one
;;;; place that decides whether they are live, and a handler that decides what
;;;; a key means right now.
;;;;
;;;; WHY THE EDITING KEYS ARE NOT OPTIONAL.  A prompt you can only append to
;;;; and backspace out of is a prompt where a typo in the second character
;;;; costs you the whole line.  That is survivable in a dialog box you use once
;;;; a month and intolerable in the thing you press before every command you do
;;;; not have a key for.  The full set is here — point, the arrows, C-a, C-e,
;;;; C-w, C-k, C-y and a history ring — because each of them is about four
;;;; lines and their absence is what makes a homemade prompt feel homemade.

(in-package #:latticewm/runtime)

(defvar *prompt* nil
  "The current prompt string, or NIL when nothing is being read.")

(defvar *input* "" "What has been typed so far.")

(defvar *point* 0
  "Where in *INPUT* the caret is.  Emacs's point, and for the same reason: an
insertion position is not the same thing as the end of the string, and a prompt
that assumes it is cannot be edited.")

(defvar *prompt-callback* nil
  "Called with the finished string, or with NIL if cancelled.")

(defvar *completions* '()
  "Candidate strings for TAB completion, or NIL.")

(defvar *kill* ""
  "The last text killed with C-k, C-u or C-w, for C-y.  One string rather than
a ring: a window manager prompt is one line long, and a kill *ring* in a
one-line buffer is ceremony.")

;;; ------------------------------------------------------------- history

(defvar *histories* (make-hash-table :test #'equal)
  "History name -> a list of past entries, most recent first.")

(defvar *history-name* nil "Which ring the current prompt reads from.")
(defvar *history-index* nil "How far back we have walked, or NIL for not.")
(defvar *history-saved* nil "What was typed before walking back.")

(defun history (name)
  "The history ring called NAME."
  (and name (gethash name *histories*)))

(defun history-push (name entry)
  "Record ENTRY at the front of NAME's ring.

Consecutive duplicates collapse and an earlier occurrence is promoted rather
than repeated, so walking back through the ring is walking back through
distinct things you did rather than through how many times you did them."
  (when (and name (plusp (length entry)))
    (let ((ring (remove entry (gethash name *histories*) :test #'string=)))
      (push entry ring)
      (setf (gethash name *histories*)
            (subseq ring 0 (min (length ring) p:*history-length*))))))

;;; ----------------------------------------------------------- the prompt

(defun reading-p ()
  "True while a prompt is up."
  (and *prompt* t))

(defun read-string (prompt callback &key completions initial history)
  "Prompt in the echo area and call CALLBACK with what was typed.

CALLBACK receives NIL if the user pressed Escape.  COMPLETIONS is a list of
candidate strings for Tab.  HISTORY names a ring that Up and Down walk, and
that the finished string is added to."
  (setf *prompt* prompt
        *input* (or initial "")
        *point* (length *input*)
        *prompt-callback* callback
        *completions* completions
        *history-name* history
        *history-index* nil
        *history-saved* nil)
  (mark-dirty)
  (request-manage)
  t)

(defun end-prompt (&optional (value nil valuep))
  "Take the prompt down, and deliver VALUE to whoever asked."
  (let ((callback *prompt-callback*)
        (name *history-name*))
    (when (and valuep value) (history-push name value))
    (setf *prompt* nil *input* "" *point* 0 *prompt-callback* nil
          *completions* '() *history-name* nil *history-index* nil)
    (mark-dirty)
    (request-manage)
    (when callback
      (guarded "prompt callback" (funcall callback (and valuep value))))))

;;; ------------------------------------------------------------ completion

(defun candidates-for (input)
  "Completions matching INPUT, best first.

The ranking is a policy decision — see COMPLETE-CANDIDATES — because it is the
most personal thing in the minibuffer and the shipped answer is a default
rather than a law."
  (if (null *completions*)
      '()
      (guarded "complete-candidates"
        (p:complete-candidates (p:current-policy) input *completions*))))

(defun common-prefix (strings)
  "The longest string every one of STRINGS starts with."
  (if (null strings)
      ""
      (let ((prefix (first strings)))
        (dolist (candidate (rest strings) prefix)
          (let ((limit (min (length prefix) (length candidate))))
            (setf prefix (subseq prefix 0 (or (mismatch prefix candidate :end1 limit
                                                                        :end2 limit)
                                              limit))))))))

(defun exact-prefix-matches (input)
  "Candidates INPUT is a literal prefix of.

Tab expands over these and not over the fuzzy matches, which is not an
inconsistency: a *hint* that `wsp' might mean `send-to-workspace' is helpful,
and silently rewriting what you typed into it is not."
  (remove-if-not (lambda (candidate)
                   (and (<= (length input) (length candidate))
                        (string-equal input candidate :end2 (length input))))
                 *completions*))

(defun complete-input ()
  "Tab: extend what is typed as far as the candidates agree."
  (let ((matches (exact-prefix-matches *input*)))
    (cond
      (matches
       (let ((prefix (if (null (rest matches)) (first matches)
                         (common-prefix matches))))
         (when (> (length prefix) (length *input*))
           (setf *input* prefix *point* (length prefix)))))
      ;; Nothing starts with what was typed, but something contains it: Tab
      ;; takes the best fuzzy match whole, because otherwise Tab appears to do
      ;; nothing while the echo area is visibly listing matches.
      (t (let ((ranked (candidates-for *input*)))
           (when ranked
             (setf *input* (first ranked) *point* (length *input*))))))))

;;; --------------------------------------------------------- line editing

(defun insert-at-point (string)
  (setf *input* (concatenate 'string (subseq *input* 0 *point*) string
                             (subseq *input* *point*))
        *point* (+ *point* (length string))))

(defun delete-region (start end &optional save)
  "Cut [START, END) out of the input, saving it for C-y when SAVE.

Backspace does not save, and that is Emacs's rule rather than an oversight: a
kill you did not think of as a kill is a kill that silently replaces the one
you did."
  (let ((start (max 0 (min start (length *input*))))
        (end (max 0 (min end (length *input*)))))
    (when (< start end)
      (when save (setf *kill* (subseq *input* start end)))
      (setf *input* (concatenate 'string (subseq *input* 0 start)
                                 (subseq *input* end))
            *point* start))))

(defun word-start (position)
  "Where the word before POSITION begins.

Word here means anything that is not a space or a dash, so C-w in `send-to-
workspace' takes off one hyphenated part at a time.  That is what somebody
correcting a command name wants and it is not what a strict word boundary
gives them."
  (let* ((separators '(#\Space #\- #\/ #\. #\Tab))
         (separatorp (lambda (character) (member character separators)))
         (limit (min position (length *input*)))
         ;; Step back over the separators first, so that C-w at `send-to-'
         ;; takes off `to-' rather than stopping on the hyphen it is standing
         ;; on and deleting nothing.
         (word-end (or (position-if-not separatorp *input* :end limit :from-end t)
                       0)))
    (or (position-if separatorp *input* :end (min (1+ word-end) (length *input*))
                                        :from-end t)
        0)))

(defun history-walk (step)
  "Up and Down: STEP through the ring, -1 being further back."
  (let ((ring (history *history-name*)))
    (when ring
      (let* ((current (or *history-index* -1))
             (next (+ current (- step))))
        (cond
          ((minusp next)
           ;; Back out of the ring into whatever was being typed when we
           ;; entered it.  Losing that is the single most annoying thing a
           ;; history can do.
           (setf *history-index* nil
                 *input* (or *history-saved* "")
                 *point* (length *input*)))
          ((< next (length ring))
           (when (null *history-index*) (setf *history-saved* *input*))
           (setf *history-index* next
                 *input* (nth next ring)
                 *point* (length *input*))))))))

(defparameter +prompt-keysyms+
  '((:escape . #xff1b) (:return . #xff0d) (:kp-return . #xff8d)
    (:backspace . #xff08) (:tab . #xff09) (:delete . #xffff)
    (:left . #xff51) (:up . #xff52) (:right . #xff53) (:down . #xff54)
    (:home . #xff50) (:end . #xff57))
  "The non-printing keys a prompt understands, by name.

A table because the alternative is a COND of magic hexadecimal, and a COND of
magic hexadecimal is where the bug that swaps Home and End lives.")

(defun prompt-keysym (keysym)
  "Which named key KEYSYM is, or NIL."
  (car (rassoc keysym +prompt-keysyms+)))

(defun prompt-key (keysym modifiers character)
  "Handle one keypress while a prompt is up.  Returns T if it was consumed.

MODIFIERS is a list of keywords, and matters for exactly one reason: Ctrl.
Emacs's line-editing keys are what fingers do without being asked, and the
readline set is close enough to the same thing that binding both costs
nothing."
  (let ((control (member :ctrl modifiers))
        (named (prompt-keysym keysym)))
    (flet ((control-key (letter) (and control character (char-equal character letter))))
      (cond
        ;; --- leaving ------------------------------------------------------
        ((or (eq named :escape) (control-key #\g)) (end-prompt) t)
        ((member named '(:return :kp-return))
         (let ((text *input*))
           ;; Complete to the single candidate if there is exactly one, so that
           ;; typing three letters and pressing Return does what it looks like
           ;; it should.
           (let ((matches (exact-prefix-matches text)))
             (when (and matches (null (rest matches))) (setf text (first matches))))
           (end-prompt text))
         t)
        ;; --- moving -------------------------------------------------------
        ((or (eq named :left) (control-key #\b))
         (setf *point* (max 0 (1- *point*))) (mark-dirty) t)
        ((or (eq named :right) (control-key #\f))
         (setf *point* (min (length *input*) (1+ *point*))) (mark-dirty) t)
        ((or (eq named :home) (control-key #\a)) (setf *point* 0) (mark-dirty) t)
        ((or (eq named :end) (control-key #\e))
         (setf *point* (length *input*)) (mark-dirty) t)
        ;; --- history ------------------------------------------------------
        ((or (eq named :up) (control-key #\p)) (history-walk -1) (mark-dirty) t)
        ((or (eq named :down) (control-key #\n)) (history-walk 1) (mark-dirty) t)
        ;; --- deleting -----------------------------------------------------
        ((eq named :backspace)
         (when (plusp *point*) (delete-region (1- *point*) *point*))
         (mark-dirty) t)
        ((or (eq named :delete) (control-key #\d))
         (delete-region *point* (1+ *point*)) (mark-dirty) t)
        ((control-key #\k) (delete-region *point* (length *input*) t) (mark-dirty) t)
        ((control-key #\u) (delete-region 0 *point* t) (mark-dirty) t)
        ((control-key #\w) (delete-region (word-start *point*) *point* t) (mark-dirty) t)
        ((control-key #\y) (insert-at-point *kill*) (mark-dirty) t)
        ;; --- completing ---------------------------------------------------
        ((eq named :tab) (complete-input) (mark-dirty) t)
        ;; --- typing -------------------------------------------------------
        ;; Last, so that a Ctrl chord this prompt does not know is swallowed
        ;; rather than inserted: C-t arriving as the letter `t' in the middle
        ;; of a command name is worse than C-t doing nothing.
        ((and character (not control)) (insert-at-point (string character)) (mark-dirty) t)
        (control t)
        (t nil)))))

;;; ---------------------------------------------------------- what it looks like

(defun prompt-segments ()
  "The echo area's contents while a prompt is up.

Replaces the status line entirely rather than appending to it: when you are
being asked something, what you are being asked is the only thing that matters,
and a prompt competing with a window count is a prompt you will misread.

The caret is a segment with no text, drawn as a bar where it falls.  Splitting
the input around it is what makes the bar land between the right two
characters at any scale without this function knowing how wide a glyph is."
  (let* ((matches (candidates-for *input*))
         (prefix-matches (exact-prefix-matches *input*))
         (completion (when (and prefix-matches (plusp (length *input*)))
                       (let ((prefix (common-prefix prefix-matches)))
                         (when (> (length prefix) (length *input*))
                           (subseq prefix (length *input*)))))))
    (remove nil
            (list (cons *prompt* :prompt)
                  (cons (subseq *input* 0 *point*) :accent)
                  (cons "" :caret)
                  (cons (subseq *input* *point*) :accent)
                  (when completion (cons completion :dim))
                  (when (rest matches)
                    ;; The two leading spaces are not decoration.  A prompt
                    ;; draws its segments contiguously, so without them the
                    ;; caret sits hard against the candidate count and `wsp[5]'
                    ;; reads as one word — which is exactly what it looked
                    ;; like the first time this was drawn on a screen.
                    (cons (format nil "  [~d]  ~{~a~^  ~}" (length matches)
                                  (subseq matches 0 (min p:*minibuffer-candidates-shown*
                                                         (length matches))))
                          :dim))))))

;;; ==================================================================
;;; THE ARGUMENT TYPES
;;; ==================================================================
;;;
;;; One per kind of thing a command can be asked for.  The list is short on
;;; purpose: these are the kinds that appear in the shipped commands' parameter
;;; lists, and a user who adds a command taking something else adds a type for
;;; it in three lines rather than editing this file.

(defun keyword-named (text)
  "TEXT as a keyword, without interning rubbish on a typo."
  (find-symbol (string-upcase (string-trim " " text)) :keyword))

(defun labelled-names ()
  "Every name anybody has given a node, sorted."
  (let ((names '()))
    (when *world*
      (c:map-nodes (lambda (node)
                     (when (c:node-label node)
                       (pushnew (c:node-label node) names :test #'equal)))
                   (c:world-root *world*)))
    (sort names #'string<)))

(define-argument-type :direction "direction: "
  :documentation "One of left, right, up or down."
  :candidates '("left" "right" "up" "down")
  :parse (lambda (text)
           (let ((keyword (keyword-named text)))
             (unless (c:direction-p keyword)
               (error "~a is not a direction -- try left, right, up or down" text))
             keyword)))

(define-argument-type :axis "axis: "
  :documentation "Horizontal or vertical."
  :candidates '("horizontal" "vertical")
  :parse (lambda (text)
           (let ((keyword (keyword-named text)))
             (unless (member keyword '(:horizontal :vertical))
               (error "~a is not an axis -- try horizontal or vertical" text))
             keyword)))

(define-argument-type :side "side: "
  :documentation "Which side of the existing pane the new one goes on."
  :candidates '("before" "after")
  :parse (lambda (text)
           (let ((keyword (keyword-named text)))
             (unless (member keyword '(:before :after))
               (error "~a is not a side -- try before or after" text))
             keyword)))

(define-argument-type :number "number: "
  :documentation "A whole number, which may be negative."
  :parse (lambda (text)
           (or (parse-integer (string-trim " " text) :junk-allowed t)
               (error "~a is not a number" text))))

(define-argument-type :fraction "amount: "
  :documentation "A fraction of the whole: 1/20, or 0.05."
  :parse (lambda (text)
           (let ((value (ignore-errors
                         (let ((*read-eval* nil))
                           (read-from-string (string-trim " " text))))))
             (unless (realp value)
               (error "~a is not an amount -- try 1/20 or 0.05" text))
             value)))

(define-argument-type :pixels "pixels: "
  :documentation "A distance in pixels."
  :parse (lambda (text)
           (or (parse-integer (string-trim " " text) :junk-allowed t)
               (error "~a is not a number of pixels" text))))

(define-argument-type :command "command: "
  :documentation "The name of one of our own commands."
  :candidates (mapcar #'command-name (all-commands)))

(define-argument-type :key "key: "
  :documentation "A key spec, as a binding is written: Super+Return, C-x."
  :candidates (mapcar (lambda (row) (key-to-string (car row)))
                      (all-bound-keys)))

(define-argument-type :name "name: "
  :documentation "A name somebody has given a pane or a cell."
  :candidates (labelled-names))

(define-argument-type :string "text: "
  :documentation "Any text at all.")

;; :shell-command is defined in config.lisp, where the programs it offers as
;; candidates are.

(define-argument-type :sexp "form: "
  :documentation "A Lisp form, read but not evaluated."
  :parse (lambda (text)
           (let ((*package* (find-package '#:latticewm/user))
                 (*read-eval* nil))
             (read-from-string text))))

;;; ==================================================================
;;; CALLING A COMMAND INTERACTIVELY
;;; ==================================================================

(defun read-arguments (command arguments continuation)
  "Ask for ARGUMENTS one at a time, then call CONTINUATION with the list.

Written as a chain of callbacks rather than a loop because there is no thread
to block: the window manager is a single event loop, and a prompt that waited
for its answer would wait for the loop that delivers it.  Each answer starts
the next question."
  (labels ((step-through (remaining collected)
             (if (null remaining)
                 (funcall continuation (nreverse collected))
                 (destructuring-bind (symbol type kind) (first remaining)
                   (let ((argument-type (argument-type type)))
                     (if (null argument-type)
                         ;; An optional argument nobody can be asked for ends
                         ;; the questions; the command's own default takes it
                         ;; from here.
                         (funcall continuation (nreverse collected))
                         (read-string
                          (format nil "~a ~a" (command-name command)
                                  (or (argument-type-prompt argument-type)
                                      (format nil "~(~a~): " symbol)))
                          (lambda (text)
                            (cond
                              ;; Escape anywhere in the chain abandons the
                              ;; whole command rather than running it with the
                              ;; arguments gathered so far.
                              ((null text) (notify "cancelled"))
                              ((and (zerop (length text)) (eq kind :required))
                               (notify "cancelled"))
                              ((zerop (length text))
                               (funcall continuation (nreverse collected)))
                              (t
                               (handler-case
                                   (let ((value (funcall (argument-type-parser
                                                          argument-type)
                                                         text)))
                                     (step-through
                                      (rest remaining)
                                      (if (eq kind :rest)
                                          ;; A &rest parameter is a command
                                          ;; line: `firefox --new-window' is
                                          ;; two arguments, not one string
                                          ;; with a space in it.
                                          (revappend (split-words text) collected)
                                          (cons value collected))))
                                 (error (condition) (notify "~a" condition))))))
                          :completions (funcall (argument-type-candidates
                                                 argument-type))
                          :history (format nil "~(~a~)" type))))))))
    (step-through arguments '())))

(defun call-interactively (command)
  "Run COMMAND, asking for whatever it needs first.

This is what makes the command registry a user interface rather than an
implementation detail.  Before it existed, M-x could reach the commands taking
no arguments and told you to open a REPL for the rest — which is a reasonable
thing to say to a Lisp programmer and no answer at all to anybody else."
  (let ((arguments (command-arguments command)))
    (cond
      ((null arguments) (run-command (command-name command)) (after-command))
      ((not (command-interactive-p command))
       (notify "~a takes ~{~(~a~)~^ ~} -- run it from a REPL"
               (command-name command) (command-lambda-list command)))
      (t (read-arguments command arguments
                         (lambda (values)
                           (apply #'run-command (command-name command) values)
                           (after-command)))))))

;;; ==================================================================
;;; THE COMMANDS
;;; ==================================================================

(defcommand run-command-by-name ()
  "Type a command name and run it.  Emacs's M-x.

Tab completes, Return runs, Escape cancels, Up walks back through what you ran
before.  Every command in the system is reachable this way — including the ones
that take arguments, which are asked for one at a time once the name is in —
and that is what lets the keymap be the commands you use often rather than the
only ones that exist."
  (read-string "M-x "
               (lambda (name)
                 (when (and name (plusp (length name)))
                   (let ((command (find-command name)))
                     (if (null command)
                         (notify "no such command: ~a" name)
                         (call-interactively command)))))
               :completions (mapcar #'command-name (all-commands))
               :history "command"))

(defvar latticewm/user::$ nil
  "What the last EVAL-EXPRESSION returned.

Lives in the user package under a name nothing else wants, so that the second
expression you type can use the first one's answer without having said in
advance that it would want to.")

(defcommand eval-expression ()
  "Type a Lisp form and evaluate it.  Emacs's M-:.

The form is read in LATTICEWM/USER — the same package as your init.lisp — so
everything is visible without a prefix, and it runs in the window manager's own
thread, so it may touch the compositor:

    (setf *gaps* 12)
    (relayout :force t)
    (mapcar #'window-app-id (all-windows))

The result appears in the echo area and in the log, and is bound to $ for the
next expression.  This is the whole live-image argument reduced to one
keystroke: no SWANK, no editor, no second machine — the running window manager
will simply answer a question about itself."
  (read-string "M-: "
               (lambda (text)
                 (when (and text (plusp (length text)))
                   (handler-case
                       (let* ((*package* (find-package '#:latticewm/user))
                              (form (read-from-string text))
                              (value (eval form)))
                         (setf latticewm/user::$ value)
                         (notify "~a" (truncate-text (prin1-to-string value) 200))
                         (after-command))
                     (error (condition)
                       (notify "~a" (explain-evaluation-error condition))))))
               :history "eval"))

(defcommand repeat ()
  "Run the last command again, with the same arguments.

Emacs's C-x z, vi's `.', and the reason a command that took three prompts to
say is one keystroke to say twice."
  (if (null *last-command*)
      (notify "nothing to repeat")
      (progn (apply #'run-command *last-command*)
             (after-command))))

(defcommand goto-named-cell ()
  "Type a name and jump to whatever has it.

Works for anything with a label — the lattice's named cells, and any node you
have named yourself."
  (let ((names (labelled-names)))
    (if (null names)
        (notify "nothing is named yet")
        (read-string "go to: "
                     (lambda (name)
                       (when name
                         (let ((node (c:find-node-if
                                      (lambda (n) (equal (c:node-label n) name))
                                      (c:world-root *world*))))
                           (if node
                               (progn
                                 (p:jump-cursor (p:current-policy) *world*
                                                (c:node-path-to (c:world-root *world*)
                                                                node))
                                 (after-command))
                               (notify "no such name: ~a" name)))))
                     :completions names
                     :history "name"))))

(defcommand name-this ()
  "Give the focused node a name, so it can be jumped to by it."
  (read-string "name: "
               (lambda (name)
                 (when name
                   (let ((node (current-node)))
                     (when node
                       (setf (c:node-label node)
                             (and (plusp (length name)) name))
                       (notify "named ~a" name)
                       (after-command)))))
               :history "name"))
