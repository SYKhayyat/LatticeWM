;;;; tools/stamp.lisp --- The fasl cache says which trees it was built from.
;;;;
;;;; #146.  A `package.lisp' change recompiles everything downstream of it,
;;;; including the vendored protocol bindings under src/protocol/ -- and ASDF
;;;; decides staleness *one file at a time*: a file whose own source has not
;;;; changed is not recompiled because something it was compiled against did.
;;;; That is fine for a DEFUN and not fine for a DEFPACKAGE, because a package
;;;; is a shared surface rather than a dependency edge ASDF can see.  A symbol
;;;; that changed package, a home package that was replaced, an export list
;;;; that moved: every fasl compiled against the old one is reading a package
;;;; that no longer exists in the shape it was compiled against.
;;;;
;;;; WHAT THAT COST.  After such a rebuild, `make integration' failed four runs
;;;; in a row inside a section the diff had never reached -- `(<= (+ start
;;;; nbytes) end)' with end = 0 in wayflan's cb-shiftout, then `invalid object
;;;; 0' and a dead connection -- and `make clean && make build' made it green.
;;;; A red run that reports a protocol desync in an untouched section costs the
;;;; reader the whole diff before they learn it was a build artifact.
;;;;
;;;; SO THE CACHE IS STAMPED.  Every run records the content hash of each
;;;; DEFPACKAGE file with the fasls built from it; the next run compares, and
;;;; when a package file has moved it drops this project's cached fasls instead
;;;; of loading them.  The answer to "is this cache the right one" is then a
;;;; string comparison rather than a guess about timestamp ordering, which is
;;;; what a restored CI cache defeats.
;;;;
;;;; WHY *ONLY* THE PACKAGE FILES.  Because that is the hazard ASDF cannot see
;;;; and the scope that costs nothing: any other file that changes is its own
;;;; input, so ASDF recompiles it, and this stays out of the development loop.
;;;; Stamping every source would mean a full rebuild each time `make test' is
;;;; run against an edited file, which is the loop this project lives in.

;; A REQUIRE IS NOT A COMPILE-TIME FACT UNLESS IT SAYS SO.  Gate 1 compiles this
;; file as well as loading it, and a bare top-level REQUIRE is not evaluated
;; before the reader reaches the SB-MD5 package further down -- which is a READ
;; error during COMPILE-FILE, not a warning.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :sb-md5))

(in-package #:cl-user)

(defparameter *stamp-file-name* "latticewm-stamp"
  "The receipt's name, beside the fasls and not among them.

It lives in the cache directory because that is the thing it is about, and
FORGET-FIRST-PARTY-FASLS skips it because dropping the cache is not the same act
as forgetting which tree it was for: `make clean' removes the directory and
takes the stamp with it, which is the honest outcome, and a build that drops its
fasls leaves a receipt saying what they will be rebuilt from.")

(defparameter *package-files-glob* "**/package.lisp"
  "Every file that declares a package, found rather than listed.

GLOBBED FOR THE REASON TOOLS/BUILD.LISP GLOBS THE EXTENSIONS: an extension
added tomorrow is covered on the day it lands.  The dependencies under .deps/
are excluded -- their packages are their own build's problem, and this project
does not recompile them.")


;;; ------------------------------------------------------------ where things are

(defun project-fasl-directory (&optional root)
  "The directory ASDF caches this project's fasls in.

ASKED OF ASDF, not spelled out.  The path is
~/.cache/common-lisp/<lisp>-<version>/<the project>, and the <lisp>-<version>
part changes with the compiler -- so a hard-coded one is a wrong answer on
somebody else's machine and a silent no-op on this one the day SBCL is
upgraded."
  (uiop:pathname-directory-pathname
   (asdf:apply-output-translations
    (merge-pathnames (make-pathname :name *stamp-file-name* :type "lisp")
                     (or root *default-pathname-defaults*)))))

(defun stamp-pathname (&optional root)
  "Where the receipt for ROOT's cache is."
  (merge-pathnames (make-pathname :name *stamp-file-name* :type "sexp")
                   (project-fasl-directory root)))

(defun package-files (&optional root)
  "Every DEFPACKAGE file in this tree, sorted, .deps/ excluded."
  (let ((deps (namestring (merge-pathnames ".deps/" (or root *default-pathname-defaults*)))))
    (sort (remove-if (lambda (path) (eql 0 (search deps (namestring path))))
                     (directory (merge-pathnames *package-files-glob*
                                                 (or root *default-pathname-defaults*))))
          #'string< :key #'namestring)))

(defun hash-string (path)
  "PATH's MD5, as lowercase hex.

A STRING AND NOT THE BYTE VECTOR SB-MD5 ANSWERS WITH.  EQUAL does not compare
general arrays elementwise -- it falls back to EQL, which is identity -- so the
first version of this compared two byte vectors that were the same and called
every package file in the tree moved.  Hex is also what makes the stamp a file a
person can read, diff and paste into an issue."
  (format nil "~(~{~2,'0x~}~)" (coerce (sb-md5:md5sum-file path) 'list)))

(defun package-fingerprint (&optional root)
  "What this tree's package surface is, as an alist of (PATH . HEX DIGEST).

CONTENT AND NOT A TIMESTAMP.  #146's other half is the restored CI fasl cache,
whose files come back stamped *after* the checkout: mtimes in a cache say when
the archive was unpacked, and content is the only thing that says what was
compiled."
  (loop for path in (package-files root)
        collect (cons (enough-namestring path (or root *default-pathname-defaults*))
                      (hash-string path))))

(defun which-packages-moved (recorded current)
  "The paths in CURRENT whose hash disagrees with RECORDED, or that are new."
  (remove nil
          (loop for (path . hash) in current
                collect (unless (equal hash (cdr (assoc path recorded :test #'equal)))
                          path))))

;;; --------------------------------------------------------- the stamp itself

(defun read-stamp (path)
  "The fingerprint in PATH, or NIL when there is none or it cannot be read.

NIL MEANS \"nothing is known\", which is a different answer from \"the same\",
and the caller treats it as one: a cache nobody has stamped is a cache whose
age cannot be argued with, and ASDF's own staleness rule is left to apply."
  (when (probe-file path)
    (handler-case
        (with-open-file (in path :external-format :utf-8)
          (let ((form (read in nil nil)))
            (when (consp form) form)))
      (error () nil))))

(defun write-stamp (path fingerprint)
  "Record FINGERPRINT at PATH, so the next run can compare."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (prin1 fingerprint out)
        (terpri out))))
  fingerprint)

;;; ------------------------------------------------------------ dropping fasls

(defun first-party-fasls (&optional root)
  "Every file ASDF cached for this tree, the dependencies' under .deps/ aside.

THE CACHE DIRECTORY MIRRORS THE SOURCE TREE, so \"ours\" is a question about a
path prefix and nothing more: this project's own sources, the generated .lisp
stubs wayflan writes beside their fasls, the test systems, and the extensions'
test systems -- but not .deps/, which is where bootstrap.sh put the libraries
and which this project does not pay to recompile."
  (let* ((dir (project-fasl-directory root))
         (deps (namestring (merge-pathnames ".deps/" dir)))
         (out '()))
    (labels ((skip-p (path)
               (or (eql 0 (search deps (namestring path)))
                   (equal (pathname-name path) *stamp-file-name*)))
             (walk (directory)
               (when (probe-file directory)
                 (dolist (file (uiop:directory-files directory))
                   (unless (skip-p file) (push file out)))
                 (dolist (sub (uiop:subdirectories directory))
                   (unless (skip-p sub) (walk sub))))))
      (walk dir)
      out)))

(defun forget-first-party-fasls (&optional root)
  "Delete every cached artifact of this tree, and say how many.

DELETING RATHER THAN CLEARING.  ASDF's CLEAR-SYSTEM drops a component from the
image and the next load consults the disk again, where the stale fasl still
stands.  Deleting the output is the one act ASDF cannot second-guess: a missing
fasl compiles, in dependency order, exactly as a cold build would."
  (let ((files (first-party-fasls root)))
    (dolist (file files)
      (ignore-errors (delete-file file)))
    (length files)))

;;; ------------------------------------------------------------------ the check

(defun check-fasl-stamp (&key root (announce t))
  "Drop this project's fasls when a package file has moved since they were built.

Returns :DROPPED when it dropped them, :STAMPED when it found no stamp to
compare against (and wrote one), and NIL when the cache is the one for this
tree.  ANNOUNCE NIL is for callers that want the answer and not the paragraph.
"
  (let* ((root (or root *default-pathname-defaults*))
         (stamp (stamp-pathname root))
         (current (package-fingerprint root))
         (recorded (read-stamp stamp)))
    (cond
      ((null recorded)
       (write-stamp stamp current)
       (when announce
         (format *error-output*
                 "~&the fasl cache carries no stamp yet; writing one for ~d ~
                  package file~:p~%"
                 (length current)))
       :stamped)
      ((not (null (which-packages-moved recorded current)))
       (let ((moved (which-packages-moved recorded current))
             (dropped (forget-first-party-fasls root)))
         (write-stamp stamp current)
         (when announce
           (format *error-output*
                   "~&~%~{~a~^, ~} moved since this fasl cache was built -- a~%~
                    DEFPACKAGE change invalidates every file compiled against it,~%~
                    and ASDF decides staleness one file at a time (#146).~%~
                    Dropped ~d cached file~:*~:p and will rebuild from source.~%"
                   moved dropped))
         :dropped))
      (t nil))))
