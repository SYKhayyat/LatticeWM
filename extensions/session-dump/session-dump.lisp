;;;; session-dump/session-dump.lisp --- Quitting, with the whole image as luggage.

(in-package #:session-dump)

;;; ================================================================ state

(defun session-core-file ()
  "Where a dumped session is written: $XDG_STATE_HOME/latticewm/session.core,
the same directory discipline as the saved layout beside which it lives."
  (merge-pathnames
   "latticewm/session.core"
   (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
       (merge-pathnames ".local/state/" (user-homedir-pathname)))))

;;; ============================================================ the resume

(defun resume-toplevel ()
  "The dumped core's entry point.

Runs when somebody boots `sbcl --core session.core'.  START reconnects to
whatever river is running now -- the old connection died with the dump and
nothing here pretends otherwise -- builds a fresh world, and restores the
saved layout.  The configuration file is NOT re-read: the whole point of a
dumped session is that its settings are already in the image.  Booting the
stock binary remains the way to pick up init.lisp changes."
  (r:start :config nil))

;;; ================================================================ command

(r:defcommand dump-session ()
  "Write the running image to disk and exit.

Everything in memory persists through the dump: extensions loaded, options
changed, rules defined as methods, macros recorded.  The next login resumes
it instead of the stock image re-reading init.lisp:

    sbcl --core ~/.local/state/latticewm/session.core

The layout is also written out first, so the resumed session reconnects AND
restores what was on screen.  This command ends the current session; it is
quitting, with the whole image as luggage."
  ;; The layout save is not optional: the resume path relies on it, and the
  ;; emergency thunk registered at startup would only fire if something went
  ;; WRONG.  Here nothing has gone wrong; we are simply leaving.
  (r:save-state)
  #+sbcl
  (sb-ext:save-lisp-and-die (namestring (session-core-file))
                            :toplevel #'resume-toplevel)
  #-sbcl
  (r:logmsg :error "dump-session requires SBCL"))
