;;;; runtime/minibuffer.lisp --- Reading a line from the user.
;;;;
;;;; M-x, and the reason it is worth having in a window manager at all.
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
;;;; conditional.  Ninety-odd bindings are created once, on first use, and
;;;; enabled while a prompt is up.  This is the same mechanism README D19 needs
;;;; for typing in an empty pane, so the two share it: one set of bindings, one
;;;; place that decides whether they are live, and a handler that decides what
;;;; a key means right now.

(in-package #:latticewm/runtime)

(p:define-option *minibuffer-prompt-color* '(0.95 0.75 0.35 1.0)
  "Colour of the prompt in the echo area.")

(p:define-option *minibuffer-completion-color* '(0.50 0.55 0.65 1.0)
  "Colour of the completion hint after what you have typed.")

(defvar *prompt* nil
  "The current prompt string, or NIL when nothing is being read.")

(defvar *input* "" "What has been typed so far.")

(defvar *prompt-callback* nil
  "Called with the finished string, or with NIL if cancelled.")

(defvar *completions* '()
  "Candidate strings for TAB completion, or NIL.")

(defun reading-p ()
  "True while a prompt is up."
  (and *prompt* t))

(defun read-string (prompt callback &key completions initial)
  "Prompt in the echo area and call CALLBACK with what was typed.

CALLBACK receives NIL if the user pressed Escape.  COMPLETIONS is a list of
candidate strings for Tab."
  (setf *prompt* prompt
        *input* (or initial "")
        *prompt-callback* callback
        *completions* completions)
  (mark-dirty)
  (request-manage)
  t)

(defun end-prompt (&optional (value nil valuep))
  "Take the prompt down, and deliver VALUE to whoever asked."
  (let ((callback *prompt-callback*))
    (setf *prompt* nil *input* "" *prompt-callback* nil *completions* '())
    (mark-dirty)
    (request-manage)
    (when callback
      (guarded "prompt callback" (funcall callback (and valuep value))))))

(defun candidates-for (input)
  "Completions that start with INPUT."
  (remove-if-not (lambda (candidate)
                   (and (<= (length input) (length candidate))
                        (string-equal input candidate :end2 (length input))))
                 *completions*))

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

(defun prompt-key (keysym character)
  "Handle one keypress while a prompt is up.  Returns T if it was consumed."
  (cond
    ((= keysym #xff1b) (end-prompt) t)                       ; Escape
    ((or (= keysym #xff0d) (= keysym #xff8d))                ; Return
     (let ((text *input*))
       ;; Complete to the single candidate if there is exactly one, so that
       ;; typing three letters and pressing Return does what it looks like it
       ;; should.
       (let ((matches (candidates-for text)))
         (when (and matches (null (rest matches))) (setf text (first matches))))
       (end-prompt text))
     t)
    ((= keysym #xff08)                                       ; Backspace
     (when (plusp (length *input*))
       (setf *input* (subseq *input* 0 (1- (length *input*)))))
     (mark-dirty) t)
    ((= keysym #xff09)                                       ; Tab
     (let ((matches (candidates-for *input*)))
       (when matches
         (setf *input* (common-prefix matches))
         (when (null (rest matches)) (setf *input* (first matches)))))
     (mark-dirty) t)
    (character
     (setf *input* (concatenate 'string *input* (string character)))
     (mark-dirty) t)
    (t nil)))

(defun prompt-segments ()
  "The echo area's contents while a prompt is up.

Replaces the status line entirely rather than appending to it: when you are
being asked something, what you are being asked is the only thing that matters,
and a prompt competing with a window count is a prompt you will misread."
  (let* ((matches (candidates-for *input*))
         (completion (when (and matches (plusp (length *input*)))
                       (let ((prefix (common-prefix matches)))
                         (when (> (length prefix) (length *input*))
                           (subseq prefix (length *input*)))))))
    (remove nil
            (list (cons *prompt* :prompt)
                  (cons *input* :accent)
                  (when completion (cons completion :dim))
                  (when (rest matches)
                    (cons (format nil "[~d]  ~{~a~^  ~}" (length matches)
                                  (subseq matches 0 (min 6 (length matches))))
                          :dim))))))

;;; ------------------------------------------------------------ the commands

(defcommand run-command-by-name ()
  "Type a command name and run it.  Emacs's M-x.

Tab completes, Return runs, Escape cancels.  Every command in the system is
reachable this way, which is what lets the keymap be the ones you use often
rather than the only ones that exist."
  (read-string "M-x "
               (lambda (name)
                 (when (and name (plusp (length name)))
                   (let ((command (find-command name)))
                     (cond
                       ((null command) (notify "no such command: ~a" name))
                       ((command-lambda-list command)
                        ;; A command that takes arguments cannot be run from a
                        ;; bare name, and guessing would be worse than saying
                        ;; so.  The REPL is the answer for those, and the
                        ;; message says where to look.
                        (notify "~a takes ~{~(~a~)~^ ~} -- run it from a REPL"
                                name (command-lambda-list command)))
                       (t (run-command name) (after-command))))))
               :completions (mapcar #'command-name (all-commands))))

(defcommand goto-named-cell ()
  "Type a name and jump to whatever has it.

Works for anything with a label — the lattice's named cells, and any node you
have named yourself."
  (let ((names '()))
    (c:map-nodes (lambda (node)
                   (when (c:node-label node) (pushnew (c:node-label node) names
                                                      :test #'equal)))
                 (c:world-root *world*))
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
                     :completions (sort names #'string<)))))

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
                       (after-command)))))))
