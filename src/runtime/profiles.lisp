;;;; runtime/profiles.lisp --- Named configurations you can switch between (#12).
;;;;
;;;; A PROFILE IS A LISP FILE, NOT A JSON DOCUMENT.  The issue proposed JSON in
;;;; ~/.config/latticewm/profiles/; this program's configuration has been Lisp
;;;; since the first commit, and for a reason that has not changed: an option's
;;;; value can be a keymap, a rule table, a lambda, a list of lists, and every
;;;; one of those has a printed form that reads back.  A JSON profile would need
;;;; a second syntax, a second parser and a mapping between the two -- and the
;;;; mapping is where `no way to say this in JSON' becomes `this feature has a
;;;; hole in it'.
;;;;
;;;; So the trust model is exactly the one init.lisp already has: a profile is
;;;; LOADED, because it is the user's own file in the user's own directory, and
;;;; this program has never pretended otherwise.  Nothing here reads a profile
;;;; as data and evaluates it piecewise; that would be a sandbox pretending to
;;;; be one (see the launcher, which parses arithmetic for exactly that reason:
;;;; it reads a *string from a desktop file*, not a file the user wrote).
;;;;
;;;; WHAT SAVING CAPTURES is the interesting decision.  A profile records what
;;;; differs from what shipped -- options whose value is not the declared
;;;; default, the gesture table, and the key bindings -- rather than the whole
;;;; state of the image, because a profile that froze every option would pin a
;;;; user to the defaults of the release that wrote it and silently lose every
;;;; improvement after.  `What did I change' is the question a profile is the
;;;; answer to.

(in-package #:latticewm/runtime)

(defvar *current-profile* nil
  "The name of the profile most recently loaded, or NIL.

A name for reporting and for a status line, not a state machine: nothing here
refuses to load another profile, and a profile that loads two others is a file
that says so.")

;;; ---------------------------------------------------------------- the place

(defvar *profiles-directory* nil
  "Where profiles live, or NIL for $XDG_CONFIG_HOME/latticewm/profiles/.

A variable rather than a computation, bound by a test that must not write into
the machine's real configuration.  The suite points it at a temporary
directory; a session leaves it NIL and gets the documented place.")

(defun profiles-directory ()
  "Where profiles live: $XDG_CONFIG_HOME/latticewm/profiles/."
  (or *profiles-directory*
      (merge-pathnames "profiles/" (config-directory))))

(defun profiles-directory-name ()
  "The same directory written the way a document should write it.

The distinction CONFIG-FILE-NAME exists for, and for the same reason: an
absolute path in a generated document is a per-machine measurement in a
committed artifact."
  "~/.config/latticewm/profiles/")

(defun profile-name-p (name)
  "Is NAME a profile name: a plain word, and one that stays in its directory?

THIS IS THE ONE SECURITY-SHAPED DECISION IN THE FILE and it is not decoration.
A profile name arrives from a prompt, from a config file and from the control
socket, and it is used to build a path.  `../../.ssh/authorized_keys' as a
profile name is a write to a file nobody asked to write; `.bashrc' as one is an
overwrite of something this program has no business touching.  A name is
letters, digits, dashes, underscores and dots, and it does not begin with a dot."
  (and (stringp name)
       (plusp (length name))
       (not (char= (char name 0) #\.))
       (every (lambda (character)
                (or (alphanumericp character)
                    (member character '(#\- #\_ #\.))))
              name)
       ;; Alphanumericp is true for accented letters, and a unicode name is
       ;; fine -- but not for anything a filesystem or a shell would treat as
       ;; structure, which the member test above already excludes.
       t))

(defun profile-path (name)
  "The file NAME names, or a signalled error if NAME is not a profile name."
  (unless (profile-name-p name)
    (error "~s is not a profile name: letters, digits, dashes and dots, and it ~
            may not begin with a dot" name))
  (merge-pathnames (format nil "~a.lisp" name) (profiles-directory)))

(defun profile-names ()
  "Every profile in the directory, sorted, without the .LISP."
  (let ((directory (profiles-directory))
        (out '()))
    (when (probe-file directory)
      (dolist (file (uiop:directory-files directory "*.lisp"))
        (push (pathname-name file) out)))
    (sort out #'string<)))

(defun profile-exists-p (name)
  "Is there a profile called NAME?"
  (and (profile-name-p name) (probe-file (profile-path name)) t))

;;; -------------------------------------------------------------- the verbs

(defun load-profile (name)
  "Load the profile called NAME into the running session.

Loading a *file* is the whole mechanism, so a profile can set options, bind
keys, define commands, load extensions and call anything else a config file can
-- which is what the issue asked for under `profile contents' and the only
definition of a profile that has no holes in it.

THE TWO THINGS THAT MUST FOLLOW A LOAD are the two the same edit in init.lisp
would need: river has to be told the new key bindings, and the gesture objects
have to be rebuilt.  Neither is automatic, because an option is a variable and
nothing watches it."
  (let ((path (profile-path name)))
    (unless (probe-file path)
      (error "no profile called ~s; ~a holds ~{~a~^, ~}"
             name (profiles-directory-name) (profile-names)))
    (load path)
    (setf *current-profile* name)
    ;; Only with a compositor: a profile loaded from a REPL or a test has no
    ;; server to tell, and the file has still done its work.
    (when *server*
      (guarded "rebind-keys" (rebind-keys))
      (setf (server-gestures-dirty *server*) t)
      (request-manage))
    (mark-dirty)
    (notify "profile: ~a" name)
    name))

(defun option-changes ()
  "Every option whose value differs from the default it was declared with.

Sorted by name, so two saves of the same session produce the same file -- the
same reason the generated documents are sorted, and the check that keeps this
honest in the suite is exactly `save twice, compare the two files'."
  (let ((out '()))
    (dolist (row (p:all-options))
      (destructuring-bind (key variable value default documentation) row
        (declare (ignore variable documentation))
        (unless (equal value default)
          (push (list key value) out))))
    (sort out #'string< :key (lambda (row) (symbol-name (first row))))))

(defun profile-forms ()
  "The forms a profile file holds, as a list of forms rather than text.

Built as data and printed by one printer, so that a value that cannot be read
back is a fact this function can be asked about rather than a file that fails
to load six months later."
  (let ((forms '()))
    ;; Options first: they are what most profiles are.
    (dolist (change (option-changes))
      (destructuring-bind (key value) change
        (push `(setf (p:option ,key) ',value) forms)))
    ;; The gesture table, when it is not what shipped: it is a list of conses
    ;; and it reads back.
    (unless (equal p:*gestures* (p:option-default :gestures))
      (push `(setf p:*gestures* ',p:*gestures*) forms))
    ;; Key bindings last, because they are the largest section and the one a
    ;; reader is most likely to edit by hand.
    (let ((bindings (profile-key-forms)))
      (when bindings
        (push '(:key-bindings) forms)
        (dolist (form bindings) (push form forms))))
    (nreverse forms)))

(defun profile-key-forms ()
  "The keymap as DEFINE-KEY forms, in the order KEYS.txt would print them.

CHORDS ARE LISTED IN A COMMENT rather than reproduced: a submap is a keymap
object with a name, and rebuilding one would mean inventing a name for it that
no config file has ever used.  A profile that silently lost a submap would be
worse than one that says it did not carry it."
  (let ((forms '()))
    (dolist (row (p:keymap-keys p:*keymap*))
      (let ((key (p:key-to-string (car row)))
            (target (cdr row)))
        (if (typep target 'p:keymap)
            (push (format nil ";; (chord ~a -> ~a: not captured in a profile)"
                          key (or (p:keymap-name target) "submap"))
                  forms)
            (push `(p:define-key p:*keymap* ,key ',target) forms))))
    (nreverse forms)))

(defun write-profile (name)
  "Write NAME with the session's current changes.  Answers the path written."
  (let ((path (profile-path name)))
    (ensure-directories-exist path)
    (call-with-replacement-file
     path
     (lambda (stream)
       (format stream ";;;; ~a.lisp --- a LatticeWM profile, saved from a running session.~a~a"
               name #\Newline #\Newline)
       (format stream ";;;; Load it with M-x profile-load ~a, or from init.lisp:~a"
               name #\Newline)
       (format stream ";;;;   (load \"~aprofiles/~a.lisp\")~a~a"
               (namestring (config-directory)) name #\Newline #\Newline)
       (format stream ";;;; A profile records what DIFFERS from what shipped: options changed~a"
               #\Newline)
       (format stream ";;;; from their declared default, the gesture table, and key bindings.~a"
               #\Newline)
       (format stream ";;;; Edit it by hand -- it is an ordinary configuration file.~a~a"
               #\Newline #\Newline)
       (format stream "(in-package #:latticewm/user)~a~a" #\Newline #\Newline)
       (let ((*print-pretty* t)
             (*print-case* :downcase)
             (*print-right-margin* 78))
         (dolist (form (profile-forms))
           (when (and (consp form) (eq (first form) :key-bindings))
             (format stream "~a;;; ---- keys ~a" #\Newline #\Newline))
           (if (stringp form)
               (format stream "~a~a" form #\Newline)
               (progn (pprint form stream) (terpri stream)))))))
    path))

(defun delete-profile (name)
  "Remove the profile called NAME.  Answers T when there was one."
  (let ((path (profile-path name)))
    (when (probe-file path)
      (delete-file path)
      (when (equal *current-profile* name)
        (setf *current-profile* nil))
      t)))

;;; -------------------------------------------------------------- the commands

(defcommand profiles ()
  "List the profiles in your configuration directory."
  ()
  (let ((names (profile-names)))
    (notify "~d profile~:p~@[: ~{~a~^, ~}~]"
            (length names) names)
    names))

(defcommand profile-load (name)
  "Load a profile: a file of settings you can switch between.

A profile is an ordinary configuration file in
~/.config/latticewm/profiles/.  Loading one applies it to the running session --
options, key bindings, gestures, and anything else the file does -- so the
difference between two profiles is one command rather than one restart."
  (:interactive :profile)
  (handler-case (load-profile name)
    (error (condition) (notify "~a" condition))))

(defcommand profile-save (name)
  "Save this session's changes as a profile called NAME.

What is written is what DIFFERS from the shipped configuration: options you
changed, the gesture table, and your key bindings.  That is deliberate -- a
profile that froze every value would pin you to the defaults of the release
that wrote it -- and the file is an ordinary config file you can edit."
  (:interactive :profile-name)
  (handler-case
      (let ((path (write-profile name)))
        (notify "profile saved: ~a (~d setting~:p)"
                (file-namestring path) (length (option-changes))))
    (error (condition) (notify "~a" condition))))

(defcommand profile-delete (name)
  "Delete a profile file.  The window manager does not ask twice."
  (:interactive :profile)
  (handler-case
      (if (delete-profile name)
          (notify "profile deleted: ~a" name)
          (notify "no profile called ~a" name))
    (error (condition) (notify "~a" condition))))

(defcommand profile-current ()
  "Say which profile was loaded most recently, if any."
  ()
  (notify "~@[profile: ~a~]~:[~;no profile loaded~]"
          *current-profile* (null *current-profile*))
  *current-profile*)
