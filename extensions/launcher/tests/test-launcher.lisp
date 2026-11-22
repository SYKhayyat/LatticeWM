;;;; tests/test-launcher.lisp --- Everything you can start must be testable
;;;; with nothing to start.

(defpackage #:launcher/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:l #:launcher))
  (:export #:run-all))

(in-package #:launcher/tests)

(def-suite launcher :description "The launcher extension.")
(in-suite launcher)

(t*:register-extension-suite "LAUNCHER/TESTS" "LAUNCHER")

(defun run-all ()
  "Run the LAUNCHER suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'launcher)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-launcher (&body body)
  `(let ((l:*actions* '())
         (l:*extra-sources* '())
         (l:*enable-desktop-source* t)
         (l:*enable-path-source* t)
         (l:*desktop-directories* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun a-desktop-file (directory name &key (exec "firefox") (terminal nil)
                                         (type "Application") (hidden nil))
  "Write a minimal .desktop file under DIRECTORY and return its path."
  (let ((path (merge-pathnames (format nil "~a.desktop" name)
                               (uiop:ensure-directory-pathname directory))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "[Desktop Entry]~%Type=~a~%Name=~a~%Exec=~a~%"
              type name exec)
      (when terminal (format out "Terminal=true~%"))
      (when hidden (format out "Hidden=true~%")))
    path))

;;; ============================================================== scoring

(test subsequence-matching-is-fuzzy-and-cased
  "Three letters find a name long before the word is finished: that is what
makes a launcher a launcher instead of a menu."
  (with-launcher
    (is-true (l:fuzzy-score "ffx" "firefox"))
    (is-true (l:fuzzy-score "gtw" "Git-Tab-Widget")
             "word starts join the subsequence")
    (is-true (l:fuzzy-score "FIREF" "firefox") "case does not matter")
    (is-false (l:fuzzy-score "xft" "firefox") "order matters")
    (is-false (l:fuzzy-score "firefoxo" "firefox")
              "a pattern cannot outgrow its string")))

(test word-starts-and-runs-score-higher
  "Ranking is a score, not a length: the match that starts a word beats the
one in the middle of a word, and consecutive letters beat scattered ones."
  (with-launcher
    (is (> (l:fuzzy-score "wget" "wget") (l:fuzzy-score "wget" "awget"))
        "a name that begins with the pattern beats one that buries it")
    (is (> (l:fuzzy-score "fo" "foot") (l:fuzzy-score "fo" "falkon"))
        "consecutive letters beat the same letters scattered")))

(test rank-records-orders-best-first-and-ties-alphabetically
  (let ((records (mapcar (lambda (title) (list :title title))
                         '("firefox" "falkon" "foot" "emacs"))))
    (is (equal '("foot" "falkon" "firefox")
               (mapcar (lambda (r) (getf r :title))
                       (l:rank-records records "fo")))
        "the consecutive match wins; the rest keep their order")
    (let ((again (mapcar (lambda (r) (getf r :title))
                         (l:rank-records records "fo"))))
      (is (equal '("foot" "falkon" "firefox") again)
          "and it is stable: the same keystrokes give the same list"))))

;;; ====================================================== desktop entries

(test desktop-entries-are-read-and-understood
  (with-launcher
    (let* ((dir (t*:fresh-temporary-directory "lp-desktop"))
           (l:*desktop-directories* (list dir)))
      (a-desktop-file dir "firefox" :exec "firefox --new-window")
      (a-desktop-file dir "hidden-thing" :hidden t :exec "weird")
      (a-desktop-file dir "a-link" :type "Link" :exec "weird")
      (a-desktop-file dir "noterm" :terminal t :exec "tmux")
      (let ((records (l:desktop-entry-records)))
        (is (= 2 (length records)) "two offered, two hidden kinds skipped")
        (is-true (find "firefox" records :key (lambda (r) (getf r :title))
                                   :test #'string=))
        (is-true (find "noterm" records :key (lambda (r) (getf r :title))
                                  :test #'string=)
                 "Terminal=true entries come through")
        ;; And how the records spawn.
        (let ((spawned '()))
          (let ((l:*spawn-function* (lambda (argv) (push argv spawned))))
            (funcall (getf (find "firefox" records
                                 :key (lambda (r) (getf r :title))
                                 :test #'string=) :action))
            (is (equal '("firefox" "--new-window") (first spawned))
                "the Exec becomes an argv list, not a string"))))
      ;; %f is stripped, because there is no file to pass.
      (is (equal '("gimp") (l:exec-argv (l:strip-field-codes "gimp %f")))))))

(test exec-argv-quotes-group-and-backslash-escapes
  "The one shell-free way to split a command line."
  (with-launcher
    (is (equal '("foot") (l:exec-argv "foot")))
    (is (equal '("echo" "hello world") (l:exec-argv "echo \"hello world\"")))
    (is (equal '("echo" "a b") (l:exec-argv "echo a\\ b")))
    (is (equal '("weirdurl") (l:exec-argv (l:strip-field-codes "weird%Uurl")))
        "a field code vanishes rather than becoming an argument")))

(test broken-desktop-files-are-skipped-not-fatal
  (with-launcher
    (let* ((dir (t*:fresh-temporary-directory "lp-desktop"))
           (l:*desktop-directories* (list dir))
           (path (merge-pathnames "empty.desktop"
                                  (uiop:ensure-directory-pathname dir))))
      (with-open-file (out path :direction :output) (write-string "" out))
      (is (null (l:parse-desktop-entry path)))
      (is (equal '() (l:desktop-entry-records)) "one bad file, zero broken"))
    (is (null (l:parse-desktop-entry "/nonexistent/file.desktop"))
        "and a missing file answers NIL")))

;;; ================================================================ path

(defun write-a-program (directory name)
  "Create NAME in DIRECTORY, executable."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "#!/bin/sh" out))
    (sb-posix:chmod (namestring path) #o755)
    path))

(test path-entry-records-finds-executables-not-anything-else
  "A file on the path that is not executable is a file, not a program."
  (with-launcher
    (let* ((dir (t*:fresh-temporary-directory "lp-path"))
           (l:*path-directories* (list dir)))
      (write-a-program dir "runme")
      (with-open-file (out (merge-pathnames "plain" dir) :direction :output)
        (write-string "not a program" out))
      (let ((records (l:path-entry-records)))
        (is (null (find "plain" records :key (lambda (r) (getf r :title))
                                    :test #'string=)))
        (is-true (find "runme" records :key (lambda (r) (getf r :title))
                                   :test #'string=))))))

(test path-records-spawn-by-name
  (with-launcher
    (let* ((dir (t*:fresh-temporary-directory "lp-path"))
           (l:*path-directories* (list dir))
           (spawned '()))
      (write-a-program dir "runme")
      (let ((l:*spawn-function* (lambda (argv) (push argv spawned))))
        (funcall (getf (find "runme" (l:path-entry-records)
                             :key (lambda (r) (getf r :title))
                             :test #'string=) :action))
        (is (equal '("runme") (first spawned)))))))

;;; =============================================================== actions

(test actions-are-records-run-through-run-entry
  (with-launcher
    (l:define-action "notes" "Open today's notes" (lambda () :opened))
    (let ((record (l:entry-by-title "notes")))
      (is-true record)
      (is (equal "notes" (l:run-entry record))
          "RUN-ENTRY answers the title it ran"))
    (is-true (l:remove-action "notes"))
    (is-false (l:remove-action "notes") "removing it twice says so")))

(test defining-an-action-twice-replaces-it
  (with-launcher
    (let ((ran nil))
      (l:define-action "notes" "first" (lambda () :first))
      (l:define-action "notes" "second" (lambda () (setf ran :second)))
      (is (= 1 (length (l:actions))) "one action, not two")
      (l:run-entry (l:entry-by-title "notes"))
      (is (eq :second ran) "and the later definition is the one that ran"))))

(test a-failing-action-is-reported-and-the-session-lives
  (with-launcher
    (l:define-action "boom" "Fails on purpose" (lambda () (error "kaboom")))
    (let ((p:*echo-message* nil))
      (is (null (l:run-entry (l:entry-by-title "boom"))) "it reports, returns NIL")
      (is-true (search "kaboom" (car p:*echo-message*))))))

;;; =========================================================== calculator

(test calculate-answers-arithmetic
  (with-launcher
    (is (equal "4" (l:calculate "2+2")))
    (is (equal "7" (l:calculate "1 + 2 * 3")) "precedence")
    (is (equal "9" (l:calculate "(1+2)*3")) "parentheses")
    (is (equal "3" (l:calculate " 6 / 2 ")) "spaces")
    (is (equal "-1" (l:calculate "1-2")))
    (is (equal "8" (l:calculate "2^3")))
    (is (equal "1.5" (l:calculate "3/2")))
    (is (null (l:calculate "1/0")) "division by zero")
    (is (null (l:calculate "firefox")) "a name is not arithmetic")
    (is (null (l:calculate "")))
    (is (null (l:calculate "2+()))")) "unbalanced parentheses")))

(test calculator-records-answer-during-candidate-collection
  (with-launcher
    (let ((record (l:calculator-record "2+2")))
      (is-true record)
      (is (equal "4" (getf record :title))))))

;;; ============================================================== the list

(test candidates-answer-best-first-and-honour-the-limit
  (with-launcher
    (let ((l:*enable-path-source* nil) (l:*enable-desktop-source* t)
          (l:*limit* 10)
          (l:*desktop-directories*
           (list (let ((dir (t*:fresh-temporary-directory "lp-cands")))
                   (a-desktop-file dir "firefox" :exec "firefox")
                   (a-desktop-file dir "foot" :exec "foot")
                   dir))))
      (l:define-action "copy" "Copy" (lambda () :copied))
      (let ((titles (mapcar (lambda (r) (getf r :title))
                            (l:candidates "fo"))))
        (is (equal '("foot" "firefox") titles)
            "candidates are ranked, not listed")))))

;;; ============================================================= the prompt

(test launcher-prompts-and-runs-what-you-pick
  "The one-key door: the prompt opens with candidates that follow the input,
and the callback runs the record the chosen title names."
  (with-launcher
    (let ((ran nil) (r::*prompt* nil) (r::*completions* nil)
          ;; The machine's own programs are not this test's subject and
          ;; would fill the bounded list before the action got a look in.
          (l:*enable-path-source* nil) (l:*enable-desktop-source* nil))
      (l:define-action "note" "a note" (lambda () (setf ran :yes)))
      (l:launcher)
      (is-true r::*prompt* "a prompt is up")
      (is (functionp r::*completions*) "candidates are a function of the input")
      (let ((titles (funcall r::*completions* "2+2")))
        (is-true (find "4" titles :test #'string=)
                 "live candidates: the answer exists once the sum does")
        (is-true (find "note" (funcall r::*completions* "") :test #'string=)
                 "and the actions are there when nothing has been typed"))
      ;; Driving the callback by hand, exactly as Return does.
      (let ((callback r::*prompt-callback*))
        (setf r::*prompt* nil)
        (funcall callback "note"))
      (is (eq :yes ran) "the chosen record ran"))))
