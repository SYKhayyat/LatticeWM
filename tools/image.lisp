;;;; tools/image.lisp --- Dump ./latticewm.
;;;;
;;;; SAVE-LISP-AND-DIE does not cost live redefinition: the dumped core keeps
;;;; the compiler, so SWANK connects to the shipped binary and DEFMETHOD still
;;;; works at runtime.  This is StumpWM's shipping model.  ASDF is left in so
;;;; that user extensions can be ASDF:LOAD-SYSTEM-ed from ~/.config/latticewm/.
;;;;
;;;; The three lines before the dump are worth their space.  A Lisp image is
;;;; the largest single thing this project asks a user to accept — DESIGN
;;;; estimated 40 to 60 MB RSS and conceded it against a compiled binary's 10 —
;;;; so it is worth spending a minute of build time to keep the concession
;;;; small.  Together they take the resident set from ~108 MB to well under
;;;; that, by discarding the build-time garbage and by merging the many
;;;; thousands of duplicate strings that a system with this much documentation
;;;; in it necessarily accumulates.

(require :asdf)
(require :sb-introspect)

;; The lattice goes in.  It is the flagship worked example, the starter
;; configuration offers to enable it, and an image that did not contain it
;; could only load it from a build tree — which is not present on any machine
;; that installed from a package.  Gate 9 asserts that this line, install.sh
;; and SAMPLE-CONFIG continue to agree.
;;
;; Loading it is not enabling it: LATTICE:ENABLE is what changes the policy,
;; and gate 4 still proves the core runs with the lattice absent, because gate
;; 4 runs in an image that never loads this file.
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm")
  (asdf:load-system "lattice")
  (asdf:load-system "swank"))

;; Merge identical string constants — docstrings are the bulk of this image and
;; many of them share long substrings.  Level 2 coalesces aggressively.
(setf (sb-alien:extern-alien "gc_coalesce_string_literals" sb-alien:char) 2)

;; Throw away everything the build allocated and could not have been asked to
;; keep: fasl buffers, the reader's intermediate structures, ASDF's plans.
(sb-ext:gc :full t)

;; Shrink the nursery.  The default lets ~50 MB of garbage accumulate before
;; collecting, which trades a longer pause for fewer of them — the right trade
;; for a batch program and the wrong one here.  River waits for our manage
;; sequence before it processes further input, so a GC pause during a keystroke
;; is *input latency*, directly and visibly.  8 MB makes collections more
;; frequent and each one short enough to disappear inside a frame.
;;
;; This is the one place the "a garbage collector in the window manager does
;; not really matter" argument needs help: it is true because we are not in the
;; render path, and it stays true because the pauses are kept small.
(setf (sb-ext:bytes-consed-between-gcs) (* 8 1024 1024))

;;; COMPRESSION IS A BUILD OPTION OF SBCL, NOT A PROPERTY OF ITS VERSION, AND
;;; THE DECLARED FLOOR ONLY EVER KNEW THE SECOND HALF OF THAT.
;;;
;;; latticewm.asd reasons the 2.2.6 floor entirely from the release at which
;;; core compression became zstd and its levels became 0..22 -- correct, and
;;; silently assuming that an SBCL of that version can compress at all.  It is
;;; `--with-sb-core-compression' at *its* build time.  Fedora's package has it;
;;; the official x86-64 binary tarballs for 2.2.6 and 2.2.9 do not, so the
;;; distribution of SBCL a person is most likely to install by hand when they
;;; need an old one is exactly the one this fails on.
;;;
;;; What that failure looked like was the whole reason to write this: an
;;; unhandled SIMPLE-ERROR, "Unable to save compressed core: this runtime was
;;; not built with zstd support", under eighteen frames of SBCL backtrace, at
;;; the end of a build that had otherwise passed every gate and every check.
;;; Nothing in it names this project, the variable that turns compression off,
;;; or the fact that the rest of the build was fine.
;;;
;;; So it is asked before the dump rather than discovered during it.  A missing
;;; feature is a failure and not a silent fallback to an uncompressed image:
;;; `make image' is what `make install' installs, install-check has a size
;;; ceiling precisely because an uncompressed image once shipped by accident,
;;; and quietly producing the 190 MB one here is how that happened.
;;; WRITTEN AS A LIST OF LINES RATHER THAN ONE FORMAT STRING, because a
;;; diagnostic whose whole job is to be readable must not have its indentation
;;; decided by CL's tilde-newline rule.  The first draft of this used `~%~' at
;;; each line end, which ignores the newline *and the whitespace after it* --
;;; so every indented line in the message came out flush left and the two
;;; options stopped looking like a list.  One line per line is immune to that
;;; and diffs better besides.
(let ((setting (sb-ext:posix-getenv "LATTICEWM_COMPRESS")))
  (when (and (not (equal setting "0"))
             (not (find :sb-core-compression *features*)))
    (format *error-output* "~&make image: this SBCL cannot compress a core.~2%")
    (format *error-output*
            "  It is ~a, which is new enough: the floor in latticewm.asd is~%"
            (lisp-implementation-version))
    (dolist (line '("  about the compression *level*, and this is about whether"
                    "  there is any compression at all.  Core compression is a"
                    "  build-time option of SBCL itself, and this one was built"
                    "  without it.  Distribution packages generally have it; the"
                    "  official binary tarballs on sbcl.org generally do not."
                    ""
                    "  Two ways on, and the first is the one you want:"
                    ""
                    "    * Install an SBCL built with core compression.  Every"
                    "      major distribution's package has it -- dnf, apt,"
                    "      pacman, zypper, nix."
                    ""
                    "    * LATTICEWM_COMPRESS=0 make image"
                    "      An uncompressed image: about 52 MB instead of 13,"
                    "      starting some 350 ms sooner.  This is what `make"
                    "      image-fast' already does and it is fine for"
                    "      development.  Do not install it -- install-check"
                    "      carries a size ceiling for exactly that mistake."
                    ""
                    "  Everything else about this build succeeded.  Only the"
                    "  dump is refused, and nothing has been written."))
      (write-line line *error-output*))
    (finish-output *error-output*)
    (sb-ext:exit :code 1 :abort t)))

(sb-ext:save-lisp-and-die
 "latticewm"
 :executable t
 ;; Compression is 13 MB against 52 MB, for 350 ms of startup — paid once per
 ;; session, by a program that then runs for weeks.  Set LATTICEWM_COMPRESS=0
 ;; to skip it while developing, where the image is rebuilt constantly.
 :compression (let ((setting (sb-ext:posix-getenv "LATTICEWM_COMPRESS")))
                (cond ((equal setting "0") nil)
                      ((null setting) 22)
                      (t (parse-integer setting))))
 :save-runtime-options t
 :toplevel (lambda ()
             (funcall (read-from-string "latticewm/runtime:main"))
             0))
