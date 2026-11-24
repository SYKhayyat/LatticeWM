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

;;; ---------------------------------- a float whose place is an offset (#151)

(defun state-server-holding (windows)
  "A server object whose window table holds WINDOWS, for ALL-WINDOWS.

RESTORE-WINDOW-FACTS matches rows to live windows through that table, so a test
of restoring has to put the windows where the runtime would have put them."
  (let ((server (make-instance 'r::server))
        (table (make-hash-table :test #'eq)))
    (dolist (window windows)
      (setf (gethash (c:window-identifier window) table) window))
    (setf (r::server-windows server) table)
    server))

(defun a-floating-pip (world identifier anchor rect)
  "WINDOW for IDENTIFIER, floating on WORLD at RECT, anchored to ANCHOR."
  (let ((window (win identifier)))
    (setf (c:window-identifier window) identifier
          (c:window-floating-p window) t)
    (push (make-instance 'c:floating-window :window window :rect rect
                         :anchor anchor)
          (c:world-floats world))
    window))

(test a-saved-float-remembers-the-pane-it-hangs-on
  "THE RECTANGLE IS AN OFFSET, SO THE FILE HAS TO CARRY THE BASE (#151).

An anchored float's rectangle is stored relative to its anchor's -- that is what
ANCHOR-FLOAT re-expresses it to, and what keeps it still when the pane moves.
SAVE-STATE wrote the rectangle and not the anchor, so a restart kept 14,9 and
lost `from this pane': the float came back *visible*, at the top-left corner of
the screen, which reads as a placement bug rather than as a missing field.

The anchor is written as a path for the reason #149 gives for snapshots: a node
object does not survive being written to a file any more than it survives being
copied into a snapshot."
  (with-state-file (path)
    (let* ((world (fresh-world))
           (pane (c:current-workspace world))
           (window (a-floating-pip world "pip" pane (c:make-rect 14 9 200 150))))
      (let ((r::*world* world)
            (r:*server* (state-server-holding (list window))))
        (r:save-state path))
      (let* ((form (r::read-state-form path))
             (row (find "pip" (getf form :windows) :key #'first :test #'equal)))
        (is-true row "the floating window is in the file")
        (is (equal (list (first (c:world-cursor world)))
                   (getf (rest row) :anchor-path))
            "with the path of the pane it hangs on, not only its own rectangle")
        ;; THE FILE THIS TEST FOUND UNREADABLE (#151).  C:RECT has an explicit
        ;; constructor, so a RECT object prints as `#S(...)' and reads back as
        ;; an error -- for the entire file.  A layout saved with a float in it
        ;; was therefore discarded on the next start, and `confirm that floats
        ;; persist' (#25) had been confirming the row, not the round trip.
        (is (integerp (getf (getf (rest row) :float-rect) :x))
            "and a rectangle that READ can make a form of"))
      ;; And a restart, which is the point of writing any of it down.
      (let* ((new-world (fresh-world))
             (again (win "pip"))
             (r::*world* new-world)
             (r:*server* (state-server-holding (list again))))
        (setf (c:window-identifier again) "pip")
        (is-true (r:load-state path) "the layout came back")
        (let* ((float (c:float-of-window new-world again))
               (expected (c:resolve-path (c:world-root new-world)
                                         (list (first (c:world-cursor world))))))
          (is-true float "the float came back as a float")
          (is (eq expected (c:float-anchor float))
              "hanging on the pane at the same path, in the restored tree")
          (is (eq expected (c:resolve-path (c:world-root new-world)
                                           (list (first (c:world-cursor new-world)))))
              "which is the restored workspace")
          (let ((rect (c:float-rect float)))
            (is (= 14 (c:rect-x rect)) "the offset that was written")
            (is (= 9 (c:rect-y rect)))
            (is (= 200 (c:rect-w rect)))
            (is (= 150 (c:rect-h rect)))))))))

(test a-float-whose-pane-is-gone-comes-back-unpinned-and-not-at-the-corner
  "AN OFFSET WITH NO BASE IS NOT A RECTANGLE (#151).

The pane a float hung on may have been closed while the program was not
running, so the path in the file may resolve to nothing.  Keeping the rectangle
then places the float at 14,9 in *screen* coordinates -- a window stuffed into
the top-left corner at a size nobody chose -- which is worse than saying so.  It
comes back floating, unpinned, at the default float size, and the log says why.

Written by editing the file, because that is how the case arises: an
unresolvable path is what a hand edit or a closed pane leaves behind."
  (with-state-file (path)
    (let* ((world (fresh-world))
           (pane (c:current-workspace world))
           (window (a-floating-pip world "pip" pane (c:make-rect 14 9 200 150))))
      (let ((r::*world* world)
            (r:*server* (state-server-holding (list window))))
        (r:save-state path))
      ;; Rewrite the anchor path to one the tree does not have.
      (let* ((form (r::read-state-form path))
             (rows (getf form :windows))
             (row (find "pip" rows :key #'first :test #'equal)))
        (setf (getf (rest row) :anchor-path) '(0 7 7))
        (let ((*package* (find-package :keyword))
              (*print-readably* nil))
          (with-open-file (out path :direction :output :if-exists :supersede)
            (write form :stream out))))
      (let* ((new-world (fresh-world))
             (again (win "pip"))
             (r::*world* new-world)
             (r:*server* (state-server-holding (list again)))
             (p:*log-level* nil))
        (setf (c:window-identifier again) "pip")
        (is-true (r:load-state path))
        (let ((float (c:float-of-window new-world again)))
          (is-true float "still a float")
          (is-false (c:float-anchor float) "and no longer pinned to nothing")
          (let ((rect (c:float-rect float))
                (default (c:make-default-float-rect 100 100)))
            (is (= (c:rect-x default) (c:rect-x rect))
                "at the default float rectangle, not at a base-less offset")
            (is (= (c:rect-y default) (c:rect-y rect)))
            (is (= (c:rect-w default) (c:rect-w rect)))
            (is (= (c:rect-h default) (c:rect-h rect)))))))))

;;; --------------------------------------- the seam the core promises (#131)

(test the-core-answers-extra-facts-for-a-node-it-knows-nothing-extra-about
  "NODE-EXTRA-FACTS is documented as `NIL by default' and STORE-NODE-EXTRA-FACTS
as `the default does nothing'.  Neither default existed: only the tabs extension
defined a method, so a session with no extension loaded reached SERIALIZE-NODE,
found no applicable method, and never wrote a layout at all (#131).

The check is on a container rather than a leaf on purpose -- an image with the
tabs extension loaded has a LEAF method, and a check that only works in a bare
image would stop testing the thing it was written for."
  (let ((split (c:make-split :horizontal (list (c:make-leaf) (c:make-leaf))
                             '(1 1))))
    (is (null (c:node-extra-facts split))
        "the core knows nothing extra about a node, and says so")
    (is (null (c:restore-node-extra-facts split '(:tabs ("a" "b")) 0))
        "and facts from a file it cannot use are ignored, not an error")))
