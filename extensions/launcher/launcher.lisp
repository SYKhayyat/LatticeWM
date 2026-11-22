;;;; launcher/launcher.lisp --- Everything you can start, behind one prompt.
;;;;
;;;; WHY THIS IS AN EXTENSION AND NOT A PROGRAM.  A launcher seems to want to
;;;; be its own little daemon with a window and a config file, and every one of
;;;; those is a thing LatticeWM already has: a resident process, a prompt with
;;;; completion, a command registry, a configuration language, a notification
;;;; path.  What is missing is only the *candidates* -- a list of things worth
;;;; running -- and a way to rank them against what you typed.  So that is all
;;;; this file is: a registry of sources, one scoring function, and one
;;;; command.
;;;;
;;;; A RECORD IS A PLIST: (:title :subtitle :source :action).  The action is
;;;; something to call; the title is what you type and see.  Nothing else is
;;;; required, which is what makes a new source four lines rather than a
;;;; protocol: a function of no arguments returning records.

(in-package #:launcher)

;;; ============================================================== the state

(defvar *actions* '()
  "Called actions: (NAME DESCRIPTION FUNCTION), most recently defined first.")

(defvar *extra-sources* '()
  "A list of (NAME . FUNCTION), in the order ADD-SOURCE put them.")

(defvar *source-timeout-seconds* nil
  "Reserved: sources run synchronously, so a slow one is felt as a slow
prompt.  Nothing in this file sleeps, and a network source does not belong in
the shipped set for exactly that reason -- see the README.")

(defun define-action (name description function)
  "Register a Lisp action under NAME, described by DESCRIPTION, calling FUNCTION.

The name is what you type into the launcher, so it should be short and it
should be what you would say out loud.
    (launcher:define-action \"notes\" \"Today's notes\"
      (lambda () (r:spawn \"emacs\" \"~/notes/today.md\")))"
  (setf *actions*
        (acons (string-downcase (string name)) (cons description function)
               (remove (string-downcase (string name)) *actions*
                       :key #'car :test #'string=)))
  name)

(defun remove-action (name)
  "Forget the action registered under NAME.  Returns T when there was one."
  (let* ((key (string-downcase (string name)))
         (before (length *actions*)))
    (setf *actions* (remove key *actions* :key #'car :test #'string=))
    (/= before (length *actions*))))

(defun actions ()
  "The registered actions, as an alist of NAME to (DESCRIPTION . FUNCTION)."
  *actions*)

(defun define-source (name function)
  "Add FUNCTION to the candidate sources under NAME.

FUNCTION takes no arguments and returns records.  Defining a name twice
replaces it, because a source is a place to look and a place to look is one
thing: two functions scanning the same directories would double every entry
and halve the value of the list."
  (let ((key (string-downcase (string name))))
    (setf *extra-sources*
          (acons key function (remove key *extra-sources*
                                      :key #'car :test #'string=)))
    name))

(defun remove-source (name)
  "Forget the source NAME.  Returns T when there was one."
  (let* ((key (string-downcase (string name)))
         (before (length *extra-sources*)))
    (setf *extra-sources* (remove key *extra-sources* :key #'car :test #'string=))
    (/= before (length *extra-sources*))))

(defun sources ()
  "Every (NAME . FUNCTION) the launcher consults, in order."
  (append *extra-sources*
          (list (cons "actions" 'action-entry-records)
                (cons "desktop" 'desktop-entry-records)
                (cons "projects" 'project-entry-records)
                (cons "path" 'path-entry-records))))

;;; ============================================================== the options

(p:define-option *limit* 200
  "How many candidates the launcher offers at once.

The prompt's completion list holds every candidate and ranks it as you type;
this bounds what the *page* shows when you ask to see them.  A list of two
thousand lines in an overlay is a list nobody reads.")

(p:define-option *desktop-directories* nil
  "Where desktop entries are read from, or NIL to ask XDG for the answer.

Bound by tests, which must not read the machine's real applications; left
alone by everybody else.")

(p:define-option *path-command-limit* 4000
  "How many `$PATH' executables to collect.  A number, not a promise: a
directory with ten thousand files in it is a directory somebody put there on
purpose, and the ones found first are the ones first on the path.")

(p:define-option *enable-path-source* t
  "Offer executables found on `$PATH'.  Turn it off to keep the list to
applications, projects and actions.")

(p:define-option *enable-desktop-source* t
  "Offer XDG desktop applications.")

(p:define-option *spawn-function* nil
  "How a program is started, or NIL for R:SPAWN.

A function of one argument, the argv list.  Bound by tests, which must not
open windows on somebody's desktop because a list had a name in it -- the
same ruling DECLARED-SESSIONS and LAYOUT-PERSISTENCE made for their spawn
paths.")

(p:define-option *terminal-argument* "-e"
  "What to pass a terminal emulator to run a command inside it.

`foot -e cmd', `xterm -e cmd', `alacritty -e cmd' -- the flag is the one
thing that differs, so it is the thing that is configurable.")

;;; ============================================================== scoring

(defun letter-case-fold-p (a b)
  (or (char= a b) (char= (char-downcase a) (char-downcase b))))

(defun word-boundary-p (string position)
  "True when POSITION starts a word of STRING.

A word starts at the beginning, after a separator, and at a capital letter --
the last one is why `gtw' finds `Git-Tab-Widget' in a list where nobody wrote
it that way."
  (or (zerop position)
      (let ((previous (char string (1- position)))
            (here (char string position)))
        (or (member previous '(#\Space #\- #\_ #\/ #\. #\:) :test #'char=)
            (and (lower-case-p previous) (upper-case-p here))))))

(defun fuzzy-score (pattern string)
  "How well PATTERN matches STRING as a subsequence, or NIL for no match.

The number is a score, not a length: a match that starts a word beats one in
the middle of one, and consecutive letters beat scattered ones, because that
is the difference between typing three letters and typing three letters and
getting the wrong thing.  Empty PATTERN matches everything equally."
  (cond
    ((zerop (length pattern)) 0)
    ((> (length pattern) (length string)) nil)
    (t
     (let ((score 0) (run 0) (position 0))
       (loop for pattern-index from 0 below (length pattern)
             for wanted = (char pattern pattern-index)
             for found = (position wanted string :start position
                                             :test #'letter-case-fold-p)
             do (when (null found) (return-from fuzzy-score nil))
                (setf score (+ score 1))
                (when (word-boundary-p string found) (incf score 3))
                ;; POSITION is already one past the previous match, so
                ;; equality here means "next to it" -- the difference between
                ;; typo correction and a hit.
                (if (and (> found 0) (= found position))
                    (progn (incf run) (incf score run))
                    (setf run 0))
                (setf position (1+ found)))
       ;; A shorter string that matched the same letters beat a longer one.
       (+ score (max 0 (- 8 (length string))))))))

(defun rank-records (records pattern)
  "RECORDS ordered best first for PATTERN, dropping the ones that do not match.

Ties break on the title, so two runs of the same keystrokes give the same
list: a prompt whose third row moves between openings is a prompt you have to
read every time.  This is the function to replace if you want frecency,
because it is one function and nothing else in the file ranks anything."
  (let ((scored '()))
    (dolist (record records)
      (let ((score (fuzzy-score pattern (or (getf record :title) ""))))
        (when score (push (cons score record) scored))))
    (mapcar #'cdr
            (stable-sort scored
                         (lambda (a b)
                           (if (= (car a) (car b))
                               (string-lessp (or (getf (cdr a) :title) "")
                                             (or (getf (cdr b) :title) ""))
                               (> (car a) (car b))))))))

;;; ============================================================== records

(defun a-record (title source action &optional subtitle)
  "The shape every source returns."
  (list :title title :subtitle subtitle :source source :action action))

(defun candidates (&optional pattern)
  "Every record, ranked against PATTERN, from every source.

A source that signals is skipped rather than fatal: the launcher is how you
start programs, and one unreadable directory must not make that impossible.
The failure goes to the log, which is where a broken source is debuggable --
without it the source silently contributes nothing and looks like it works."
  (let ((records '()))
    (dolist (source (sources))
      (when (source-enabled-p source)
        (dolist (record (or (handler-case (funcall (cdr source))
                            (error (condition)
                                (r:logmsg :warn "launcher source ~a failed: ~a"
                                          (car source) condition)
                                nil))
                            '()))
          (push record records))))
    (let ((ranked (rank-records records (or pattern ""))))
      (if (and *limit* (> (length ranked) *limit*))
          (subseq ranked 0 *limit*)
          ranked))))

(defun source-enabled-p (source)
  "Whether SOURCE (a (NAME . FUNCTION) pair) should be consulted."
  (cond ((string= (car source) "desktop") *enable-desktop-source*)
        ((string= (car source) "path") *enable-path-source*)
        (t t)))

(defun entry-by-title (title)
  "The record whose title is TITLE, or the best match for it.

Looked up afresh rather than carried from the prompt: the prompt hands back a
string, and a string is what the command form receives too, so both paths ask
the same question and get the same record."
  (or (find title (entries) :key (lambda (record) (getf record :title))
                          :test #'string=)
      (first (rank-records (entries) title))))

(defun entries ()
  "Every record from every source, unranked."
  (let ((records '()))
    (dolist (source (sources))
      (dolist (record (or (handler-case (funcall (cdr source))
                            (error () nil))
                          '()))
        (push record records)))
    (nreverse records)))

(defun run-entry (record)
  "Run the action RECORD carries, reporting a failure instead of signalling.

The launcher runs in the window manager's own process, so an action that
signals must not take the session down with it: a broken action costs a
message in the echo area and nothing else."
  (cond
    ((null record) (r:notify "nothing to run") nil)
    (t (let ((title (getf record :title))
             (action (getf record :action)))
         (if (null action)
             (progn (r:notify "~a has nothing to run" title) nil)
             (handler-case
                 (progn (funcall action) title)
               (error (condition)
                 (r:logmsg :warn "launcher: ~a failed: ~a" title condition)
                 (r:notify "~a failed: ~a" title condition)
                 nil)))))))

;;; ====================================================== desktop entries
;;;
;;; The format is an INI file with exactly one section we care about.  It is
;;; also the one place in this extension where a file *anybody* can install
;;; supplies a command line, so the parsing is deliberately dull: read the
;;; [Desktop Entry] group, keep Name and Exec and Terminal, skip anything
;;; marked NoDisplay or Hidden, and never hand a string to a shell.

(defun desktop-directories ()
  "The directories desktop entries are read from."
  (or *desktop-directories*
      (let ((home (merge-pathnames ".local/share/applications/"
                                   (user-homedir-pathname))))
        (cons home
              (loop for dir in (uiop:split-string
                                (or (uiop:getenv "XDG_DATA_DIRS")
                                    "/usr/local/share:/usr/share")
                                :separator ":")
                    when (plusp (length dir))
                      collect (merge-pathnames "applications/"
                                               (uiop:ensure-directory-pathname dir)))))))

(defun desktop-files (&optional (directories (desktop-directories)))
  "Every .desktop file in DIRECTORIES and one level below, sorted.

One level below, because some systems install their applications in a
subdirectory named after the vendor, and a launcher that cannot see Firefox
because it lives in `applications/mozilla/' is a launcher nobody trusts."
  (sort (loop for directory in directories
              append (loop for file in (append
                                        (ignore-errors
                                         (directory (merge-pathnames "*.desktop" directory)))
                                        (ignore-errors
                                         (directory (merge-pathnames "*/*.desktop" directory))))
                           when (probe-file file) collect file))
        #'string< :key #'namestring))

(defun read-desktop-lines (file)
  "The [Desktop Entry] group of FILE as an alist of KEY to VALUE, or NIL.

Later keys win, which is what the format says.  A file that cannot be read
answers NIL rather than signalling: one broken entry in one bad package must
not be able to make the launcher fail."
  (handler-case
      (with-open-file (in file :external-format :utf-8)
        (let ((group nil) (keys '()))
          (loop for line = (read-line in nil nil)
                while line
                do (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                     (cond
                       ((zerop (length trimmed)))
                       ((char= (char trimmed 0) #\#))
                       ((char= (char trimmed 0) #\[)
                        (setf group (subseq trimmed 1 (max 0 (1- (length trimmed))))))
                       ((and group (string= group "Desktop Entry")
                             (position #\= trimmed))
                        (let ((equals (position #\= trimmed)))
                          (setf keys
                                (acons (string-trim " " (subseq trimmed 0 equals))
                                       (string-trim " " (subseq trimmed (1+ equals)))
                                       keys)))))))
          (nreverse keys)))
    (error () nil)))

(defun desktop-value (entry key)
  "The value of KEY in ENTRY, or NIL.  The last occurrence wins, as the format
says -- which is why ENTRY is walked in order and the answer is the last
match rather than the first."
  (let ((found nil))
    (loop for (k . v) in entry when (string= k key) do (setf found v))
    found))

(defun desktop-true-p (entry key)
  (let ((value (desktop-value entry key)))
    (and value (string-equal (string-trim " " value) "true"))))

(defun strip-field-codes (exec)
  "EXEC with the desktop-entry field codes removed.

%f, %F, %u, %U and friends are placeholders for a file or URL the launcher
does not have.  An entry whose command *needs* one is an entry that cannot be
run from here, and passing `%f' through as an argument would run the program
with a file called `%f', which is worse than running it with none."
  (let ((out (make-string-output-stream)))
    (loop with i = 0
          while (< i (length exec))
          do (let ((character (char exec i)))
               (cond
                 ((and (char= character #\%)
                       (< (1+ i) (length exec))
                       (char= (char exec (1+ i)) #\%))
                  (write-char #\% out)
                  (incf i 2))
                 ((char= character #\%)
                  (incf i 2))
                 (t (write-char character out)
                    (incf i)))))
    (string-trim " " (get-output-stream-string out))))

(defun exec-argv (exec)
  "EXEC split into an argv list, without a shell.

Quotes group an argument and a backslash escapes the next character; nothing
else is special.  Deliberately not a shell: the file this comes from was
written by a package, not by you, and `sh -c' would make every desktop entry
on the machine a program the launcher runs on your behalf."
  (let ((argv '()) (current (make-string-output-stream)) (pending nil)
        (quote nil))
    (labels ((finish ()
               (push (get-output-stream-string current) argv)))
      (loop for character across exec
            do (cond
                 (pending
                  (write-char character current)
                  (setf pending nil))
                 ((char= character #\\)
                  (setf pending t))
                 (quote
                  (if (char= character quote)
                      (setf quote nil)
                      (write-char character current)))
                 ((or (char= character #\") (char= character #\'))
                  (setf quote character))
                 ((member character '(#\Space #\Tab))
                  (finish))
                 (t (write-char character current))))
      (finish))
    ;; REVERSED, because PUSH accumulates backwards and an argv list is a
    ;; sequence with a first element: `foot' has to be the program.
    (nreverse (remove "" argv :test #'string=))))

(defun parse-desktop-entry (file)
  "A record for FILE, or NIL when it is not an application worth offering.

The rulings: Type must be Application (Links and Directories are different
things with the same extension); NoDisplay and Hidden mean what they say; and
an entry with no Exec has nothing to run."
  (let* ((entry (read-desktop-lines file))
         (type (and entry (desktop-value entry "Type")))
         (name (and entry (desktop-value entry "Name")))
         (exec (and entry (desktop-value entry "Exec"))))
    (when (and entry
               (or (null type) (string-equal type "Application"))
               (not (desktop-true-p entry "NoDisplay"))
               (not (desktop-true-p entry "Hidden"))
               name exec
               (plusp (length (string-trim " " name)))
               (plusp (length (string-trim " " exec))))
      (let* ((argv (exec-argv (strip-field-codes exec)))
             (terminal (desktop-true-p entry "Terminal"))
             (comment (desktop-value entry "Comment")))
        (when argv
          (a-record name "desktop"
                    (lambda ()
                      (if terminal
                          (spawn-argv (append (uiop:split-string p:*terminal*
                                                                 :separator " ")
                                              (list *terminal-argument*)
                                              argv))
                          (spawn-argv argv)))
                    (and comment (plusp (length comment)) comment)))))))

(defun desktop-entry-records ()
  "Every desktop application on this machine, as records."
  (loop for file in (desktop-files)
        for record = (parse-desktop-entry file)
        when record collect record))

;;; ================================================================ path

(p:define-option *path-directories* nil
  "Where executables are found, or NIL to ask `$PATH'.

Bound by tests, which must not describe the machine they run on; left alone by
everybody else.")

(defun path-directories ()
  "The directories on `$PATH', in order and without duplicates."
  (remove-duplicates
   (if *path-directories*
       (mapcar #'uiop:ensure-directory-pathname *path-directories*)
       (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                           :separator ":")
             when (plusp (length dir))
               collect (uiop:ensure-directory-pathname dir)))
   :test #'equal))

(defun executable-file-p (path)
  (and (ignore-errors (uiop:file-pathname-p path))
       (ignore-errors (logbitp 0 (sb-posix:stat-mode (sb-posix:stat path))))))

(defun path-entry-records ()
  "Executables on `$PATH', as records.  Bounded by *PATH-COMMAND-LIMIT*."
  (let ((seen (make-hash-table :test #'equal)) (records '()))
    (loop for directory in (path-directories)
          while (< (length records) *path-command-limit*)
          do (dolist (file (ignore-errors (directory (merge-pathnames "*" directory))))
               (let ((name (pathname-name file)))
                 (when (and name
                            (not (gethash name seen))
                            (executable-file-p file)
                            (< (length records) *path-command-limit*))
                   (setf (gethash name seen) t)
                   (push (a-record name "path"
                                   (lambda () (spawn-argv (list name)))
                                   (namestring directory))
                         records)))))
    (nreverse records)))

;;; ============================================================ projects

(defun project-entry-records ()
  "Registered projects, when the projects extension is loaded.

Asked through FIND-PACKAGE rather than depended on: a launcher that will not
load without a project registry is a launcher you cannot use, and this is the
one composition the issue asks for by name."
  (let ((package (find-package :projects)))
    (if (null package)
        '()
        (let ((names (ignore-errors
                      (funcall (intern "ALL-PROJECT-NAMES" package))))
              (switch (intern "SWITCH-TO-PROJECT" package)))
          (loop for name in names
                collect (a-record name "project"
                                  (lambda () (funcall switch name))
                                  "project"))))))

;;; ============================================================= actions

(defun action-entry-records ()
  "The actions defined with DEFINE-ACTION, as records."
  (loop for (name . (description . function)) in *actions*
        collect (a-record name "action" function description)))

;;; ========================================================== calculator
;;;
;;; Typing `2+2' into the launcher and pressing Return should give 4.  The
;;; temptation is (eval (read line)), and it is the one thing this file must
;;; never do: the prompt accepts anything a person can type, and READING is
;;; code execution with a friendlier name.  So: a four-operator parser over
;;; digits, and a string that does not fit it is simply not a calculation.

(defun expression-p (string)
  "True when STRING is arithmetic the calculator can answer.

A digit, an operator and no letters: `2+2', `(3*4)/2', `1.5 - 2'.  A string
with a letter in it is not a calculation, which is what keeps `firefox' from
being read as a variable."
  (and (plusp (length string))
       (every (lambda (character)
                (or (digit-char-p character)
                    (member character
                            (list #\+ #\- #\* #\/ (code-char 40) (code-char 41)
                                  #\. #\Space #\^)
                            :test #'char=)))
              string)
       (some #'digit-char-p string)))

(defun tokenize-expression (string)
  "STRING as a list of numbers and single-character operator symbols."
  (let ((tokens '()) (i 0) (length (length string)))
    (loop while (< i length)
          do (let ((character (char string i)))
               (cond
                 ((member character '(#\Space #\Tab) :test #'char=) (incf i))
                 ((or (digit-char-p character) (char= character #\.))
                  (let ((start i))
                    (loop while (and (< i length)
                                     (or (digit-char-p (char string i))
                                         (char= (char string i) #\.)))
                          do (incf i))
                    (let ((number (read-from-string (subseq string start i))))
                      (unless (numberp number)
                        (error "not a number: ~a" (subseq string start i)))
                      (push number tokens))))
                 (t (push (intern (string character) :keyword) tokens) (incf i)))))
    (nreverse tokens)))

(defun calculate (string)
  "The value of the arithmetic in STRING as a string, or NIL.

An expression that does not parse, divides by zero, or is not arithmetic at
all answers NIL -- the launcher shows its answer as one more candidate, and a
candidate that does not exist is the right way to say \"that is not a sum\"."
  (when (expression-p string)
    (handler-case
        (let ((tokens (tokenize-expression string)))
          (multiple-value-bind (value next) (parse-sum tokens 0)
            (when (= next (length tokens))
              (let ((printed (format nil "~a"
                                     (if (and (rationalp value)
                                              (not (integerp value)))
                                         (coerce value 'single-float)
                                         value))))
                ;; 4.0 answers 4, because a calculator that shows a trailing
                ;; zero on an integer is a calculator that looks broken.
                (if (and (floatp value) (= value (fround value)))
                    (format nil "~d" (round value))
                    printed)))))
      (error () nil))))

(defun parse-sum (tokens position)
  "Precedence climbing: sums of products of powers of atoms.  (VALUE . NEXT)."
  (multiple-value-bind (value next) (parse-product tokens position)
    (loop while (and (< next (length tokens))
                     (member (nth next tokens) (list :+ :-) :test #'eq))
          do (let ((operator (nth next tokens)))
               (multiple-value-bind (right after) (parse-product tokens (1+ next))
                 (setf value (if (eq operator :+) (+ value right) (- value right))
                       next after))))
    (values value next)))

(defun parse-product (tokens position)
  "Products and quotients, left to right.  (VALUE . NEXT)."
  (multiple-value-bind (value next) (parse-power tokens position)
    (loop while (and (< next (length tokens))
                     (member (nth next tokens) (list :* :/) :test #'eq))
          do (let ((operator (nth next tokens)))
               (multiple-value-bind (right after) (parse-power tokens (1+ next))
                 (when (and (eq operator :/) (zerop right))
                   (error "division by zero"))
                 (setf value (if (eq operator :*) (* value right) (/ value right))
                       next after))))
    (values value next)))

(defun parse-power (tokens position)
  "A number or a parenthesised expression, optionally raised to a power.
(VALUE . NEXT)."
  (multiple-value-bind (value next) (parse-atom tokens position)
    (if (and (< next (length tokens)) (eq (nth next tokens) :^))
        (multiple-value-bind (exponent after) (parse-power tokens (1+ next))
          (values (expt value exponent) after))
        (values value next))))

(defun parse-atom (tokens position)
  "A number, or (EXPRESSION).  (VALUE . NEXT)."
  (let ((token (and (< position (length tokens)) (nth position tokens))))
    (cond
      ((numberp token) (values token (1+ position)))
      ((eq token (intern "(" :keyword))
       (multiple-value-bind (value next) (parse-sum tokens (1+ position))
         (unless (and (< next (length tokens))
                      (eq (nth next tokens) (intern ")" :keyword)))
           (error "unbalanced parentheses"))
         (values value (1+ next))))
      ((eq token :-) (multiple-value-bind (value next) (parse-atom tokens (1+ position))
                       (values (- value) next)))
      ((eq token :+) (parse-atom tokens (1+ position)))
      (t (error "not an expression")))))

(defun calculator-record (pattern)
  "A record answering PATTERN arithmetically, or NIL.

Its action copies the answer to the clipboard when there is a clipboard to
copy to, and says the answer either way -- a calculator you cannot get the
number out of is a calculator you have to retype."
  (when (and pattern (plusp (length pattern)))
    (let ((answer (calculate pattern)))
      (when answer
        (a-record answer "=" (lambda () (offer-answer answer)) pattern)))))

;;; ============================================================ running it

(defun spawn-argv (argv)
  "Start ARGV, through *SPAWN-FUNCTION* when it is bound."
  (if *spawn-function*
      (funcall *spawn-function* argv)
      (apply #'r:spawn argv)))

(defun clipboard-program ()
  "How to put text on the Wayland clipboard, or NIL with nothing installed.

Asked of `$PATH' rather than guessed at run time: a launcher that copies your
answer by failing to copy it is worse than one that says the number."
  (find-if (lambda (name)
             (some (lambda (directory)
                     (probe-file (merge-pathnames name directory)))
                   (path-directories)))
           '("wl-copy" "xclip" "xsel")))

(defun offer-answer (answer)
  "Give ANSWER to the user: the clipboard when there is one, the echo area always."
  (let ((program (clipboard-program)))
    (when program
      (ignore-errors
       (sb-ext:run-program program
                           (if (string= program "wl-copy") '()
                               (list "-selection" "clipboard"))
                           :input (make-string-input-stream answer)
                           :search t :wait nil :output nil :error nil)))
    (r:notify "~a~@[ (copied)~]" answer (and program t))
    answer))

;;; ============================================================= commands

(p:define-argument-type :launcher-entry "run: "
  :documentation "Something the launcher can start."
  :candidates (lambda ()
                (mapcar (lambda (record) (getf record :title)) (entries))))

(r:defcommand (launcher "launcher") ()
  "Open the launcher: type part of a name, press Return.

The list is every desktop application, every executable on `$PATH', every
project, every action you have defined -- and, if what you typed is a sum,
the answer to it.  Candidates are ranked as you type, so three letters are
usually enough.

Nothing is spawned by a shell and nothing is read as Lisp: an entry that
names a program gets an argv list, and a calculation is parsed as arithmetic.
Both of those are about the same thing -- this prompt accepts anything a
person can type, so nothing it accepts may be executed as code."
  ()
  (r:read-string
   "run: "
   (lambda (title)
     (when (and title (plusp (length title)))
       (run-entry (entry-by-title title))))
   :completions (lambda (input)
                  (let ((records (candidates input)))
                    ;; The calculation is offered against what has been
                    ;; typed rather than against the whole world, so it is
                    ;; the one candidate that is *about* the input.
                    (let ((sum (calculator-record input)))
                      (mapcar (lambda (record) (getf record :title))
                              (if sum (cons sum records) records)))))
   :history "launcher"))

(r:defcommand launch (entry)
  "Run ENTRY, named the way the launcher names it.

The command form of the prompt, for a key that opens one thing, a project
that starts what it needs, or a configuration file that says what your
morning is."
  (:interactive :launcher-entry)
  (let ((record (entry-by-title entry)))
    (cond
      (record (run-entry record))
      (t (r:notify "nothing called ~a to run" entry) nil))))

;;; ============================================================== the switch

(defun enable ()
  "Turn the launcher on.  Idempotent; there is nothing to install, because the
sources are read when you open the prompt rather than cached."
  t)

(defun disable ()
  "Turn the launcher off by emptying its sources.  The actions are kept.

Kept, because they were defined by you rather than by the module, and losing
a registry of your own commands because you turned off one prompt would be
the kind of thing that makes people stop turning things off."
  (setf *extra-sources* '())
  t)

;; The names, in the package a configuration file is read in.  An option a
;; config file cannot say is an option that does not exist -- and *LIMIT* on a
;; line of someone's init.lisp has to mean this *LIMIT* rather than a new
;; symbol in LATTICEWM/USER that nothing reads.
(r:install-vocabulary '#:launcher)
