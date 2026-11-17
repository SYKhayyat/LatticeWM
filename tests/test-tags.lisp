;;;; tests/test-tags.lisp --- A tag is a name, not a keyword.
;;;;
;;;; #87: NORMALIZE-TAG interned every name it was handed into the keyword
;;;; package.  Tags arrive from a prompt, so a misspelling was interned forever
;;;; -- a decision with no undo, in the package every protocol key lives in.
;;;; Nothing here needed :KEYWORD; what it needed was one object per name.
;;;;
;;;; The tags in these checks come from the three places they really come from:
;;;; a prompt (a string), a window rule (whatever the config reader made) and a
;;;; state file (whatever *that* reader made).  Those are three symbol
;;;; identities and one name, and the checks are about the name.

(in-package #:latticewm/tests)
(in-suite tags)

(defvar *tag-probe-count* 0
  "Makes each probe name distinct within a run of this file.")

(defun fresh-tag-name ()
  "A tag name no other check in this run uses."
  (format nil "test-tag-~d-~d" (sb-posix:getpid) (incf *tag-probe-count*)))

;;; --------------------------------------------------- the keyword package

(test a-tag-name-is-never-interned
  "The bug, in one check: normalizing a string used to put a keyword in the
package for the life of the process, and every typo did it again."
  (let ((name (fresh-tag-name)))
    (is (null (find-symbol (string-upcase name) :keyword))
        "the name starts absent")
    (let ((tag (r:normalize-tag name)))
      (is (symbolp tag))
      (is (null (find-symbol (string-upcase name) :keyword))
          "and normalizing it did not put it there")
      (is (eq tag (r:normalize-tag (string-upcase name)))
          "the same name is the same tag"))))

(test one-name-is-one-tag-whatever-the-case-or-the-way-it-arrived
  "A rule writes :MAIL, a prompt says \"mail\", and both have to mean the tag
the window carries.  They are one object, so EQ works and PUSHNEW works."
  (is (eq (r:normalize-tag "Mail") (r:normalize-tag :mail)))
  (is (eq (r:normalize-tag "  mail ") (r:normalize-tag "MAIL"))))

;;; ---------------------------------------------------- the three identities

(test a-rule-tag-and-a-prompt-tag-are-one-tag
  "A window rule stores whatever the config reader made; a prompt makes an
uninterned symbol.  The tag lookup is by name, so neither has to know."
  (let ((window (win "term")))
    (setf (c:window-tags window) (list :work))
    (is (r:window-tagged-p window "work")
        "a prompt asking by string finds a rule's keyword")
    (setf (c:window-tags window) (list (r:normalize-tag "play")))
    (is (r:window-tagged-p window :play)
        "and a rule's keyword finds a prompt's uninterned tag")))

(test adding-the-same-tag-twice-does-not-duplicate-it
  "The two symbols are one tag, so the list holds it once.  PUSHNEW's default
EQL would grow the list on every prompt if it did not."
  (let ((window (win "term")))
    (setf (c:window-tags window) (list :work))
    (pushnew (r:normalize-tag "work") (c:window-tags window) :test #'r::tag-name=)
    (is (= 1 (length (c:window-tags window))))
    (is (r:window-tagged-p window "WORK"))))

;;; ------------------------------------------------------ the state file path

(test an-uninterned-tag-survives-the-state-file
  "A prompt tag prints as `#:MAIL', and reading `#:MAIL' makes a *new*
uninterned symbol.  The restore normalizes, so the tag comes back as one tag
rather than as a name that only looks the same."
  (let* ((name (fresh-tag-name))
         (world (fresh-world))
         (window (win "term"))
         (index (make-hash-table :test #'equal)))
    (setf (c:window-identifier window) "id-tag"
          (c:window-tags window) (list (r:normalize-tag name)))
    (let ((facts (r:window-facts (list window))))
      (let ((fresh (win "term")))
        (setf (c:window-identifier fresh) "id-tag"
              (gethash "id-tag" index) fresh)
        (r:restore-window-facts world facts index)
        (is (r:window-tagged-p fresh name)
            "the tag came back, by name")
        (is (eq (first (c:window-tags fresh)) (r:normalize-tag name))
            "as one object, so a later prompt reuses it")))))

(test a-scratchpad-name-from-a-file-is-the-tag-a-prompt-makes
  "The saved :TAGS row and the saved :SCRATCHPAD row are the same name printed
twice, which the reader makes two symbols of.  If the restore kept them as read,
SCRATCHPAD-SHOW would look for one and find the other."
  (let* ((name (fresh-tag-name))
         (world (fresh-world))
         (window (win "term"))
         (index (make-hash-table :test #'equal))
         ;; Two separate reads: what WINDOW-FACTS wrote and what the file's
         ;; reader makes of each `#:<name>'.
         (written (lambda () (read-from-string (format nil "#:~a" name)))))
    (setf (c:window-identifier window) "id-pad"
          (gethash "id-pad" index) window)
    (r:restore-window-facts
     world
     (list (list "id-pad" :tags (list (funcall written))
                 :scratchpad (funcall written)))
     index)
    (is (not (eq (funcall written) (funcall written)))
        "the fixture really does produce distinct symbols per read")
    (is (eql (r::scratchpad-name window) (r:normalize-tag name))
        "the stored scratchpad name is the canonical tag")
    (is (r:window-tagged-p window name)
        "and the tag row agrees with it")))

;;; ---------------------------------------------------------- the table's bound

(test a-name-nothing-carries-is-forgotten
  "The keyword package was the unbounded part; the table would be the same bug
quieter.  Pruning is what makes this a fix rather than a move."
  (let ((name (fresh-tag-name)))
    (r:normalize-tag name)
    (is (gethash (r::tag-key name) r::*tag-table*) "registered while in use")
    (let ((r:*server* nil))            ; no windows at all
      (r::forget-unused-tags))
    (is (null (gethash (r::tag-key name) r::*tag-table*))
        "and gone once nothing carries it")))

(test a-tag-a-window-carries-survives-the-prune
  "Pruning must not forget a tag that is still on a window: the next prompt
would make a second symbol for it."
  (let* ((name (fresh-tag-name))
         (window (win "term"))
         (server (make-instance 'r::server)))
    (setf (c:window-tags window) (list (r:normalize-tag name)))
    (setf (gethash window (r::server-windows server)) window)
    (let ((r:*server* server))
      (r::forget-unused-tags))
    (is (gethash (r::tag-key name) r::*tag-table*))))

(test all-tags-still-lists-what-windows-carry
  "The public answer, after the rewrite: names, lowercased, and no keyword
package involved."
  (let* ((name (fresh-tag-name))
         (window (win "term"))
         (server (make-instance 'r::server)))
    (setf (c:window-tags window) (list (r:normalize-tag name)))
    (setf (gethash window (r::server-windows server)) window)
    (let ((r:*server* server))
      (is (member (string-downcase name) (r:all-tags) :test #'string=)))))
