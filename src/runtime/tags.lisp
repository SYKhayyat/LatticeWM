;;;; runtime/tags.lisp --- Tags, and named scratchpads built on them.
;;;;
;;;; WINDOW-TAGS WAS A DEAD SLOT: declared, exported, documented as "free-form
;;;; symbols an extension may attach", and read by nothing at all.  A dead slot
;;;; in a *persisted, human-readable* state file is worse than a dead slot in
;;;; memory, because it becomes de-facto API — somebody sets `tags' in a rule
;;;; and reasonably expects it to do something.
;;;;
;;;; Two answers were available: delete it, or make it mean something.  This is
;;;; the second, and it is the better one because the feature it enables is one
;;;; the assessment separately asked for.
;;;;
;;;; A TAG IS A NAME YOU GIVE A WINDOW.  Not a workspace, not a place — the
;;;; window carries it wherever it goes, and several windows may share one.
;;;; That makes three things fall out of one mechanism:
;;;;
;;;;   jump-to-tag     go to the window called `mail', wherever it is now
;;;;   gather-tag      bring every window called `work' to where I am
;;;;   scratchpad      a tagged window that is minimized, summoned by name
;;;;
;;;; The third is i3's named scratchpad, and it is the reason this file is not
;;;; just a nicety.  The existing scratchpad is a flat most-recently-minimized
;;;; list, so `bring back my music player' means restoring things one at a time
;;;; until it turns up.  With a name it is one keystroke, and the same keystroke
;;;; puts it away again.

(in-package #:latticewm/runtime)

;;; --------------------------------------------------------------- tags

(defun window-tag-names (window)
  "WINDOW's tags as strings, in the order they were added."
  (mapcar #'string (c:window-tags window)))

(defvar *tag-table* (make-hash-table :test #'equal)
  "The canonical symbol for each tag name, keyed by TAG-KEY.

NOT THE KEYWORD PACKAGE, WHICH WAS THE BUG (#87).  A tag arrives from a prompt,
so its name is whatever somebody typed, and INTERNing that is a decision with no
undo: every misspelling stays in :KEYWORD for the life of the process, where
DO-SYMBOLS and APROPOS walk past it and a later FIND-SYMBOL cannot tell it from
a protocol key.  Nothing here needed the keyword package.  What it needed was
one object per name, and one object per name is a table.

The table is pruned by FORGET-UNUSED-TAGS on the paths that list tags for
completion, so it holds a name only while a window or scratchpad carries it --
a typo costs one symbol until the next prompt after its window is gone, and
nothing after that.  An entry is never load-bearing on its own: every
comparison below is by name, so forgetting one cannot break a tag that exists.")

(defun tag-key (name)
  "The tag table's key for NAME: its name, upcased.

A string's surrounding spaces are trimmed, because a prompt can supply them; a
symbol's name is exact and is taken as it is.  One function, because this is the
definition of which tags are the same tag: it is used for storage, for
comparison and for pruning, and three copies would be three chances to disagree
about `Mail'."
  (etypecase name
    (symbol (string-upcase (symbol-name name)))
    (string (string-upcase (string-trim " " name)))))

(defun tag-name= (a b)
  "Whether A and B are the same tag, by name rather than by symbol identity.

The tags on a window can come from a prompt (an uninterned symbol), a window
rule (whatever the config file's reader made) or a state file (whatever the
reader of that made).  Those are three different symbol identities and one name,
and the name is the tag."
  (equal (tag-key a) (tag-key b)))

(defun normalize-tag (name)
  "NAME as the symbol a tag is stored as.

\"mail\", \"Mail\" and :MAIL are one tag: one typed at a prompt and one written
in a rule have to be the same thing or the feature does not work at all.

NEVER INTERNS, which is what this used to do (#87).  A name the session has
already seen answers the same symbol; a new one gets an uninterned symbol named
by TAG-KEY, and a symbol that was handed in is kept as itself so that a rule's
:MAIL stays EQ to the tag the same prompt has made."
  (let ((key (tag-key name)))
    (or (gethash key *tag-table*)
        (setf (gethash key *tag-table*)
              (etypecase name
                (symbol name)
                (string (make-symbol key)))))))

(defun window-tagged-p (window name)
  "True when WINDOW carries the tag NAME.

By name, not by EQ: see TAG-NAME= for the three ways the two symbols can
differ."
  (and (member (tag-key name) (c:window-tags window)
               :key #'tag-key :test #'equal)
       t))

(defun windows-tagged (name)
  "Every live window carrying tag NAME, in no particular order."
  (let ((key (tag-key name)))
    (remove-if-not (lambda (window)
                     (and (c:window-live-p window)
                          (member key (c:window-tags window)
                                  :key #'tag-key :test #'equal)))
                   (all-windows))))

(defun all-tags ()
  "Every tag anybody has given a window, sorted, as strings."
  (forget-unused-tags)
  (let ((out '()))
    (dolist (window (all-windows))
      (dolist (tag (c:window-tags window))
        (pushnew (string-downcase (string tag)) out :test #'string=)))
    (sort out #'string<)))

(define-argument-type :tag "tag: "
  :documentation "A name you have given a window."
  :candidates (all-tags))

(defcommand tag-window (name)
  "Give the focused window a tag, so it can be found by name.

A tag travels with the window rather than with the place, which is what makes
it useful: `jump to mail' finds your mail client wherever it has ended up,
across workspaces, across cells, and after any amount of rearranging."
  (:interactive :tag)
  (let ((window (focused-window)))
    (cond
      ((null window) (notify "nothing here to tag"))
      (t
       (pushnew (normalize-tag name) (c:window-tags window) :test #'tag-name=)
       (notify "tagged ~(~a~): ~{~(~a~)~^ ~}"
               (p:name-of-window window)
               (window-tag-names window))
       window))))

(defcommand untag-window (name)
  "Take a tag off the focused window."
  (:interactive :tag)
  (let ((window (focused-window)))
    (when window
      (setf (c:window-tags window)
            (remove (tag-key name) (c:window-tags window)
                    :key #'tag-key :test #'equal))
      (forget-unused-tags)
      (notify "untagged ~(~a~)" name)
      window)))

(defcommand jump-to-tag (name)
  "Go to the window carrying tag NAME, wherever it is.

A minimized one is restored first, which is what makes a tag and a named
scratchpad the same feature seen twice."
  (:interactive :tag)
  (let ((windows (windows-tagged name)))
    (cond
      ((null windows) (notify "nothing is tagged ~(~a~)" name))
      (t
       (let ((window (first windows)))
         (when (c:window-minimized-p window) (restore-window window))
         (focus-existing-window window)
         (notify "~(~a~)~@[ (~d more)~]" name
                 (and (rest windows) (length (rest windows))))
         window)))))

(defcommand gather-tag (name)
  "Bring every window tagged NAME to the workspace you are on.

The `set up my project' verb: tag the four windows once, and afterwards one
keystroke collects them wherever they have drifted to."
  (:interactive :tag)
  (let ((windows (windows-tagged name))
        (moved 0))
    (cond
      ((null windows) (notify "nothing is tagged ~(~a~)" name))
      (t
       (dolist (window windows)
         (when (c:window-minimized-p window) (restore-window window))
         (let* ((root (c:world-root *world*))
                (leaf (c:leaf-holding root window))
                (from (and leaf (c:node-path-to root leaf))))
           (when (and from (not (c:path-equal (c:parent-path from)
                                              (c:parent-path (current-path)))))
             (multiple-value-bind (new-root landed)
                 (c:tree-move root from (current-path)
                              :join (p:move-into-occupied (policy) *world*
                                                          from (current-path)))
               (setf (c:world-root *world*) new-root)
               (incf moved)
               (p:jump-cursor (policy) *world* landed)))))
       (mark-dirty)
       (notify "gathered ~d window~:p tagged ~(~a~)" moved name)
       moved))))

;;; ------------------------------------------------------ named scratchpads

(defun scratchpad-name (window)
  "The scratchpad WINDOW belongs to, or NIL."
  (c:prop window :scratchpad))

(defun scratchpad-windows (name)
  "Every window on the scratchpad called NAME."
  (let ((key (tag-key name)))
    (remove-if-not (lambda (window)
                     (let ((stored (scratchpad-name window)))
                       (and (c:window-live-p window)
                            stored
                            (equal key (tag-key stored)))))
                   (all-windows))))

(defun forget-unused-tags ()
  "Drop canonical tags nothing carries, so a typo does not outlive its window.

Called from the paths that list tags for completion, which already walk every
window.  Without it the table would be the same unbounded record of every
misspelling the keyword package was, only quieter -- and \"unbounded\" is the
word in the issue, not \"not a keyword\".

Nothing breaks if an entry is missed or dropped early: every comparison is by
name and every write re-registers.  That is what makes pruning safe to do here
rather than in a sweep somebody has to remember to call."
  (let ((used (make-hash-table :test #'equal)))
    (dolist (window (all-windows))
      (dolist (tag (c:window-tags window))
        (setf (gethash (tag-key tag) used) t))
      (let ((name (scratchpad-name window)))
        (when name (setf (gethash (tag-key name) used) t))))
    (dolist (key (loop for key being the hash-keys of *tag-table* collect key))
      (unless (gethash key used) (remhash key *tag-table*)))))

(defun all-scratchpads ()
  "Every named scratchpad that currently holds something, as strings."
  (forget-unused-tags)
  (let ((out '()))
    (dolist (window (all-windows))
      (let ((name (scratchpad-name window)))
        (when name (pushnew (string-downcase (string name)) out :test #'string=))))
    (sort out #'string<)))

(define-argument-type :scratchpad "scratchpad: "
  :documentation "A named scratchpad -- a window you put away by name."
  :candidates (all-scratchpads))

(defcommand scratchpad-put (name)
  "Put the focused window away under a name, to be summoned by that name later.

i3's named scratchpad.  The existing minimize is a flat most-recently-hidden
list, so `bring back my music player' means restoring things one at a time
until it turns up; with a name it is one keystroke, and the same keystroke puts
it away again."
  (:interactive :scratchpad)
  (let ((window (focused-window)))
    (cond
      ((null window) (notify "nothing here to put away"))
      (t
       (setf (c:prop window :scratchpad) (normalize-tag name))
       (pushnew (normalize-tag name) (c:window-tags window) :test #'tag-name=)
       (minimize-window window)
       (notify "~(~a~) put away" name)
       window))))

(defcommand scratchpad-show (name)
  "Summon the window on the scratchpad called NAME."
  (:interactive :scratchpad)
  (let ((windows (scratchpad-windows name)))
    (cond
      ((null windows) (notify "the ~(~a~) scratchpad is empty" name))
      (t
       (dolist (window windows)
         (when (c:window-minimized-p window) (restore-window window)))
       (focus-existing-window (first windows))
       (notify "~(~a~)" name)
       (first windows)))))

(defcommand scratchpad-toggle (name)
  "Summon the NAME scratchpad, or put it away again if it is already up.

One key, both directions.  That is the whole point of a scratchpad: the
terminal you consult twenty times an hour should cost one keystroke each way
and should never need to be found."
  (:interactive :scratchpad)
  (let* ((windows (scratchpad-windows name))
         (visible (remove-if #'c:window-minimized-p windows)))
    (cond
      ((null windows) (notify "the ~(~a~) scratchpad is empty" name))
      (visible
       (dolist (window visible) (minimize-window window))
       (notify "~(~a~) put away" name)
       nil)
      (t (scratchpad-show name)))))

;;; ------------------------------------------------ claims: the part that lasts
;;;
;;; #15.  A named scratchpad already survives a restart, because membership is a
;;; prop on the window and WINDOW-FACTS writes props (#117's neighbours made
;;; that true for tags and it was true for this too).  What does not survive is
;;; a scratchpad whose *window* did not come back -- a reboot, a crash, a closed
;;; terminal -- and that is the case the issue is really about: `my music player
;;; belongs on the music scratchpad' is a statement about an application, not
;;; about a window that happens to exist right now.
;;;
;;; So a claim is (NAME . APP-ID): when a window with that application id opens,
;;; it joins that scratchpad and is put away, exactly as if you had typed the
;;; command.  This is the tmux bargain -- the slot survives, and whatever comes
;;; back into it is filled -- without pretending we can start programs for you
;;; (that is #97's job, and it is a different one).
;;;
;;; Stored in its own small file: claims are user data, they outlive a layout,
;;; and they are read before any window exists.

(defvar *scratchpad-claims* '()
  "A list of (SCRATCHPAD-NAME . APP-ID), as strings.")

(defvar *scratchpad-claims-loaded-p* nil
  "Have the claims been read from disk yet?  Loading is lazy and once.")

(defun scratchpad-claims-file ()
  "Where claims are kept: $XDG_STATE_HOME/latticewm/scratchpad-claims.lisp."
  (merge-pathnames
   "latticewm/scratchpad-claims.lisp"
   (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
       (merge-pathnames ".local/state/" (user-homedir-pathname)))))

(defun save-scratchpad-claims (&optional (path (scratchpad-claims-file)))
  "Write the claims out.  Never signals; see SAVE-STATE's argument."
  (guarded "save scratchpad claims"
    (ensure-directories-exist path)
    (call-with-replacement-file
     path
     (lambda (out)
       (let ((*print-readably* nil) (*print-pretty* t))
         (format out ";;;; LatticeWM scratchpad claims.  (#;(\"name\" . \"app-id\") ...)~%")
         (write *scratchpad-claims* :stream out)
         (terpri out))))
    path))

(defun load-scratchpad-claims (&optional (path (scratchpad-claims-file)))
  "Read the claims, keeping whatever is in memory if the file is unusable.

A file somebody edited by hand is untrusted input, the same ruling READ-NODE
makes: a claim that is not a (name . app-id) pair is skipped, and a file that
cannot be read at all keeps the claims this session already has rather than
throwing them away."
  (setf *scratchpad-claims-loaded-p* t)
  (guarded "load scratchpad claims"
    (when (probe-file path)
      (multiple-value-bind (form ok)
          (with-open-file (in path :if-does-not-exist nil)
            (let ((*read-eval* nil) (*package* (find-package :keyword)))
              (if in (values (read in nil nil) t) (values nil nil))))
        (when (and ok (listp form))
          (setf *scratchpad-claims*
                (remove-if-not (lambda (claim)
                                 (and (consp claim) (stringp (car claim))
                                      (stringp (cdr claim))))
                               form))))))
  *scratchpad-claims*)

(defun ensure-scratchpad-claims ()
  (unless *scratchpad-claims-loaded-p* (load-scratchpad-claims)))

(defun scratchpad-claim-names ()
  "The scratchpad names something is claimed for, as completion wants them.

A function rather than the variable, because the argument type that offers these
lives in a file loaded before this one -- and a compile-time reference to
*SCRATCHPAD-CLAIMS* from there is an undefined variable, which is at least a
warning and was a build failure."
  (ensure-scratchpad-claims)
  (mapcar #'car *scratchpad-claims*))

(defun claim-for (app-id)
  "The scratchpad name APP-ID is claimed by, or NIL."
  (ensure-scratchpad-claims)
  (let ((found (find app-id *scratchpad-claims*
                     :key (lambda (claim) (cdr claim)) :test #'string=)))
    (when found (normalize-tag (car found)))))

(defun claim-scratchpad (name app-id)
  "Say that windows from APP-ID belong on the NAME scratchpad.  Writes the file."
  (let ((name (string-downcase (string name))))
    (setf *scratchpad-claims*
          (cons (cons name app-id)
                (remove name *scratchpad-claims* :key #'car :test #'string=)))
    (save-scratchpad-claims)
    name))

(defun unclaim-scratchpad (name)
  "Forget the claim on NAME.  Members already on it stay there."
  (setf *scratchpad-claims*
        (remove (string-downcase (string name)) *scratchpad-claims*
                :key #'car :test #'string=))
  (save-scratchpad-claims))

(defun claim-window (window)
  "Put a newly opened WINDOW on the scratchpad that claims it, and hide it.

The named function behind :window-opened, so ADD-HOOK can replace it rather
than accumulating a fresh closure each time ENABLE runs."
  (let ((name (claim-for (c:window-app-id window))))
    (when name
      (setf (c:prop window :scratchpad) name)
      (pushnew name (c:window-tags window) :test #'tag-name=)
      ;; Hidden, because that is what a scratchpad member is: assigning a window
      ;; to a scratchpad in i3 does not leave it on screen either.  A window that
      ;; arrives and covers your work is the opposite of the promise.
      (minimize-window window)
      (logmsg :info "~a claimed for the ~(~a~) scratchpad" (p:name-of-window window) name)
      window)))

(defun install-scratchpad-claims ()
  "Connect the claims to newly opened windows, and read them once.  Idempotent."
  (p:add-hook :window-opened 'claim-window)
  (load-scratchpad-claims)
  t)

(defcommand scratchpad-claim (name app-id)
  "Put every window from APP-ID on the NAME scratchpad from now on.

The durable half of a scratchpad: the membership already survives a restart,
but a scratchpad whose *window* did not come back needs a statement about the
application rather than about a window.  Claim it once and its next window
joins by itself -- put away, ready for the one key that summons it."
  (:interactive :name :app-id)
  (let ((canonical (claim-scratchpad name app-id)))
    (notify "~(~a~) will claim ~a" canonical app-id)
    canonical))

(defcommand scratchpad-unclaim (name)
  "Stop claiming a window for the NAME scratchpad.  Its current members stay."
  (:interactive :scratchpad-with-claim)
  (unclaim-scratchpad name)
  (notify "~(~a~) claims nothing now" name)
  nil)

(defcommand list-scratchpad-claims ()
  "Show which application goes on which scratchpad."
  (ensure-scratchpad-claims)
  (if (null *scratchpad-claims*)
      (notify "no scratchpad claims -- M-x scratchpad-claim")
      (show-help-page
       (format nil "scratchpad claims -- ~d" (length *scratchpad-claims*))
       (loop for (name . app-id) in *scratchpad-claims*
             collect (cons name app-id)))))

(defcommand list-scratchpads ()
  "Show every named scratchpad and what is on it."
  (let ((names (all-scratchpads)))
    (if (null names)
        (notify "nothing is on a named scratchpad")
        (show-help-page
         (format nil "scratchpads -- ~d" (length names))
         (loop for name in names
               collect (cons name
                             (format nil "~{~a~^, ~}"
                                     (or (mapcar #'p:name-of-window
                                                 (scratchpad-windows name))
                                         (list "?")))))))))

(defcommand list-tags ()
  "Show every tag and the windows carrying it."
  (let ((names (all-tags)))
    (if (null names)
        (notify "nothing is tagged")
        (show-help-page
         (format nil "tags -- ~d" (length names))
         (loop for name in names
               collect (cons name
                             (format nil "~{~a~^, ~}"
                                     (or (mapcar #'p:name-of-window
                                                 (windows-tagged name))
                                         (list "?")))))))))
