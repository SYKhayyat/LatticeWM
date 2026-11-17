;;;; tests/test-state.lisp --- The file whose failure mode is losing the layout.
;;;;
;;;; #88 IS TWO BUGS WITH ONE SUBJECT.  SAVE-STATE opened the state file with
;;;; :IF-EXISTS :SUPERSEDE, so it truncated the file before writing a byte: the
;;;; write that was supposed to survive a crash was the write that destroyed the
;;;; previous save when it crashed.  And LOAD-STATE, which knew enough to keep a
;;;; file from a version it did not recognise, let a file the *reader* could not
;;;; parse sit there — where the first debounced save replaced it.  A truncated
;;;; write left exactly one recoverable copy, and the program deleted it.
;;;;
;;;; Both halves are testable without a compositor, which is why this file
;;;; exists rather than a paragraph in the integration run: the interesting
;;;; moment is inside the write, and only a test can stand there.

(in-package #:latticewm/tests)
(in-suite state)

;;; ------------------------------------------------------------------ fixtures

(defvar *state-scratch-count* 0
  "Makes each scratch directory distinct within a run of this file.")

(defmacro with-state-file ((path) &body body)
  "Run BODY with PATH naming a state file in an empty private directory.

The directory is removed on the way out, so a failing check does not leave a
layout in /tmp that a later run then reads.  Every check here writes real files,
because the subject is the filesystem: a fixture that stubbed the stream would
be testing the fixture."
  (let ((directory (gensym "STATE-DIRECTORY-")))
    `(let* ((,directory
              (uiop:ensure-directory-pathname
               (format nil "/tmp/latticewm-test-state-~d-~d/"
                       (sb-posix:getpid) (incf *state-scratch-count*))))
            (,path (merge-pathnames "state.lisp" ,directory)))
       (ensure-directories-exist ,path)
       (unwind-protect (progn ,@body)
         (ignore-errors
           (uiop:delete-directory-tree ,directory :validate t
                                       :if-does-not-exist :ignore))))))

(defun write-text (path text)
  "Replace PATH's contents with TEXT."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (write-string text out)))

(defun read-text (path)
  "PATH's contents, as a string."
  (with-open-file (in path :direction :input)
    (let ((text (make-string (file-length in))))
      (read-sequence text in)
      text)))

(defun directory-files (path)
  "The file names in PATH's directory, sorted."
  (sort (mapcar #'file-namestring
                (directory (merge-pathnames "*.*"
                                            (uiop:pathname-directory-pathname path))))
        #'string<))

;;; ------------------------------------------------- the write that replaces

(test the-file-is-not-touched-until-the-replacement-is-complete
  "THE BUG IN ONE CHECK, ASSERTED FROM INSIDE THE WRITE.

SAVE-STATE opened the shipped file with :IF-EXISTS :SUPERSEDE, which truncates it
immediately, so at this moment the old layout was already gone and a crash,
signal or full disk left a fragment in its place.  The fix writes a sibling and
renames it, so at every instant the path names a complete file.

Only a check that runs *during* the write can tell those two implementations
apart, which is why this calls the mechanism rather than looking for a temp file
afterwards."
  (with-state-file (path)
    (write-text path "the old layout")
    (r::call-with-replacement-file
     path
     (lambda (out)
       (write-string "the new layout" out)
       (is (equal "the old layout" (read-text path))
           "mid-write, the path still names the previous save")))
    (is (equal "the new layout" (read-text path))
        "and the new one once the write returned")))

(test a-write-that-fails-leaves-the-last-good-file-alone
  "The half of #88 that needed no crash: a condition in the writer, and the
layout that was on screen is still there when the condition is handled."
  (with-state-file (path)
    (write-text path "the old layout")
    (is (eq :failed
            (handler-case
                (progn
                  (r::call-with-replacement-file
                   path
                   (lambda (out)
                     (write-string "half a layout" out)
                     (finish-output out)
                     (error "disk full")))
                  :wrote)
              (error () :failed))))
    (is (equal "the old layout" (read-text path))
        "the previous save is intact")
    (is (equal '("state.lisp") (directory-files path))
        "and the temporary was cleaned up, so the next save starts clean")))

(test the-temporary-is-a-sibling
  "RENAME is only atomic within a filesystem, so a temporary in $TMPDIR would
be the bug again with extra steps.  The check is the directory, not the name."
  (with-state-file (path)
    (is (equal (uiop:pathname-directory-pathname path)
               (uiop:pathname-directory-pathname (r::temporary-state-path path))))))

(test save-state-round-trips-and-leaves-no-temporary
  "The ordinary path, after the rewrite: one file, no debris, still readable."
  (with-state-file (path)
    (let ((r::*world* (fresh-world)))
      (is (probe-file (r:save-state path)))
      (is (equal '("state.lisp") (directory-files path)))
      (let ((form (r::read-state-form path)))
        (is (consp form))
        (is (eql r::+state-version+ (getf form :version)))
        (is (getf form :root) "with a tree in it")))))

;;; ---------------------------------------- the read that refused, and deleted

(test a-truncated-state-file-is-kept-instead-of-clobbered
  "THE DATA-LOSS HALF OF #88.

LOAD-STATE answered NIL for a file it could not read — correctly — and left it
at the live path, where the first debounced save replaced it.  A crashed write
leaves exactly one copy of a layout, and the program that could not read that
copy deleted it.  Now the unreadable file is moved aside, byte for byte, and the
save that follows writes a new one without touching it."
  (with-state-file (path)
    (let ((fragment "(:version 2 :root (:stack")
          (kept (make-pathname :type "lisp.corrupt" :defaults path)))
      (write-text path fragment)
      (is (null (r:load-state path)) "an unreadable file is not a layout")
      (is (probe-file kept) "it is kept under `.lisp.corrupt'")
      (is (equal fragment (read-text kept)) "with its bytes untouched")
      (is (not (probe-file path)) "and no longer at the live path")
      ;; The next save is the thing that used to destroy it.
      (let ((r::*world* (fresh-world)))
        (r:save-state path))
      (is (probe-file path) "the next save writes a fresh file")
      (is (equal fragment (read-text kept))
          "and the copy kept from the failed read is still there"))))

(test a-file-the-reader-rejects-is-not-evaluated
  "A hand-editable file in a state directory is untrusted input: `#.' in it was
a way to run code at startup.  It is now a read failure like any other — kept,
not executed — which is the same ruling EVALUATE-FOR-IPC makes for the socket."
  (with-state-file (path)
    (write-text path "(:version 2 :root #.(progn :executed))")
    (is (null (r:load-state path)))
    (is (probe-file (make-pathname :type "lisp.corrupt" :defaults path)))))

(test a-file-from-a-newer-build-is-kept-under-its-version
  "The behaviour that was already right, pinned here because the same function
now serves two kinds of unreadable: the version suffix must not have been
replaced by `.corrupt' while making room for it."
  (with-state-file (path)
    (write-text path "(:version 99 :root (:leaf))")
    (is (null (r:load-state path)))
    (is (probe-file (make-pathname :type "lisp.v99" :defaults path)))
    (is (not (probe-file (make-pathname :type "lisp.corrupt" :defaults path))))))
