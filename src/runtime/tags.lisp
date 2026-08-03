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

(defun normalize-tag (name)
  "NAME as the symbol a tag is stored as.

Interned in the keyword package and upcased, so that \"mail\", \"Mail\" and
:MAIL are one tag.  A tag typed at a prompt and a tag written in a rule have to
be the same thing or the feature does not work at all."
  (etypecase name
    (symbol name)
    (string (intern (string-upcase (string-trim " " name)) :keyword))))

(defun window-tagged-p (window name)
  "True when WINDOW carries the tag NAME."
  (and (member (normalize-tag name) (c:window-tags window)) t))

(defun windows-tagged (name)
  "Every live window carrying tag NAME, in no particular order."
  (let ((tag (normalize-tag name)))
    (remove-if-not (lambda (window)
                     (and (c:window-live-p window)
                          (member tag (c:window-tags window))))
                   (all-windows))))

(defun all-tags ()
  "Every tag anybody has given a window, sorted, as strings."
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
       (pushnew (normalize-tag name) (c:window-tags window))
       (notify "tagged ~(~a~): ~{~(~a~)~^ ~}"
               (or (c:window-app-id window) "window")
               (window-tag-names window))
       window))))

(defcommand untag-window (name)
  "Take a tag off the focused window."
  (:interactive :tag)
  (let ((window (focused-window)))
    (when window
      (setf (c:window-tags window)
            (remove (normalize-tag name) (c:window-tags window)))
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

(defun focus-existing-window (window)
  "Put the cursor on WINDOW wherever it is, switching workspace if needed.

The workspace switch is the part that matters and the part that is easy to
forget: a window on another workspace can be focused perfectly correctly and
still be on no screen, which looks exactly like the command having done
nothing."
  (let ((float (c:float-of-window *world* window)))
    (cond
      (float
       (setf (c:world-focused-float *world*) float)
       (mark-dirty)
       (request-manage))
      (t
       (let* ((root (c:world-root *world*))
              (leaf (c:leaf-holding root window))
              (path (and leaf (c:node-path-to root leaf))))
         (when path
           (let ((stack (workspace-stack)))
             (when (and stack (first path))
               (setf (c:container-selection stack) (first path))
               (let ((output (current-output)))
                 (when output (setf (c:prop output :workspace) (first path))))))
           (p:jump-cursor (policy) *world* path)
           (mark-dirty)
           (request-manage)
           path))))))

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
  (let ((tag (normalize-tag name)))
    (remove-if-not (lambda (window)
                     (and (c:window-live-p window)
                          (eql tag (scratchpad-name window))))
                   (all-windows))))

(defun all-scratchpads ()
  "Every named scratchpad that currently holds something, as strings."
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
       (pushnew (normalize-tag name) (c:window-tags window))
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
                                     (or (remove nil
                                                 (mapcar #'c:window-app-id
                                                         (scratchpad-windows name)))
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
                                     (or (remove nil
                                                 (mapcar #'c:window-app-id
                                                         (windows-tagged name)))
                                         (list "?")))))))))
