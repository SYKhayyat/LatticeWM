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

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm")
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

(sb-ext:save-lisp-and-die
 "latticewm"
 :executable t
 ;; Compression is 13 MB against 190 MB, for 350 ms of startup — paid once per
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
