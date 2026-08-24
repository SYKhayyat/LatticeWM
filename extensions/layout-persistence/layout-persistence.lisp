;;;; layout-persistence/layout-persistence.lisp --- Names for arrangements.

(in-package #:layout-persistence)

;;; ================================================================ state

(defvar *enabled* nil "True while the save-on-change hook is installed.")

(defvar *mode* :best-effort
  "How RESTORE-LAYOUT treats an arrangement it cannot honour whole.

:BEST-EFFORT places every window it can and leaves an empty pane where an
application has not come back yet.  :EXACT refuses the whole restore unless
every named application is live and nothing unexpected sits in the way --
half a remembered layout is worse than none, if you say so.")

(defvar *save-on-change* t
  "Save the session layout as soon as it changes, not only on the periodic
pass and at exit.  The write goes through the core's own coalescer; this
option only decides whether every change asks for one.")

(defvar *layouts-directory* nil
  "Where named layouts are written, or NIL for <data directory>/layouts/.
Bound by tests; left alone by everybody else.")

(defun enabled-p () "True when the save-on-change hook is installed." *enabled*)

(defun note-layout-changed ()
  "Ask the core to save, coalesced like every other save.

The named function behind :layout-changed, because ADD-HOOK keeps what it is
given and a fresh closure per call would accumulate rather than replace."
  (r:save-state-soon))

(defun enable ()
  "Start asking for a save on every layout change.  Idempotent by name."
  (when *save-on-change*
    (p:add-hook :layout-changed 'note-layout-changed))
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop asking.  The core's periodic save and its exit save were never ours."
  (p:remove-hook :layout-changed 'note-layout-changed)
  (setf *enabled* nil)
  nil)

;;; ============================================================= the files

(defun layouts-directory ()
  "Where named layouts live: the first data directory that has a layouts/
subdirectory we can write, or NIL if there is nowhere at all."
  (or *layouts-directory*
      (let ((dir (find-if
                  (lambda (root)
                    (let ((layouts (merge-pathnames "layouts/" root)))
                      (ignore-errors
                       (ensure-directories-exist layouts)
                       (probe-file layouts))))
                  (r:data-directories))))
        (and dir (merge-pathnames "layouts/" dir)))))

(defun layout-file (name)
  "The file NAME's layout is kept in, or NIL with nowhere to keep it."
  (let ((dir (layouts-directory)))
    (and dir
         (merge-pathnames (make-pathname :name name :type "lisp") dir))))

(defun all-layout-names ()
  "Every saved layout name, sorted."
  (let ((dir (layouts-directory)))
    (when dir
      (sort (mapcar #'pathname-name
                    (directory (merge-pathnames "*.lisp" dir)))
            #'string<))))

(defun delete-layout (name)
  "Forget the layout called NAME.  Whatever windows it described stay open."
  (let ((file (layout-file name)))
    (when (and file (probe-file file))
      (delete-file file)
      name)))

;;; ======================================================= app-id swapping

(defun identifier-to-app-id (form mapping)
  "Rewrite a serialized tree from river identifiers to application ids.

MAPPING is the identifiers of the workspace's own windows to their
application ids; the core's state file keys windows by identifier, which is
exactly right for a snapshot taken a minute ago and exactly wrong for one
meant to outlive a reboot -- identifiers are compositor-local facts.  An
application id is what both yesterday and tomorrow know the window by."
  (if (atom form)
      form
      (if (eq (first form) :leaf)
          ;; Rebuild the plist rather than append to it: GETF reads the
          ;; FIRST occurrence of a key, and an appended correction would
          ;; sit unread behind the identifier it was correcting.
          (let ((out '()))
            (loop for (key value) on (rest form) by #'cddr
                  do (setf out
                           (append out
                                   (list key (if (eq key :window)
                                                 (and value
                                                      (gethash value mapping))
                                                 value)))))
            `(:leaf ,@out))
          (mapcar (lambda (child) (identifier-to-app-id child mapping))
                  form))))

(defvar *live-windows* nil
  "The windows saved app-ids may match against, or NIL to ask the runtime.

Bound by tests, which have no compositor to ask; left alone by everybody
else, because the honest answer is the runtime's.")

(defun live-window-index ()
  "Live windows keyed by application id, for DESERIALIZE-NODE to look up.

Later windows win: two instances of one application are a fact, and the
arrangement names the role rather than the process, so either may take it."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (window (or *live-windows* (r:all-windows)) index)
      (let ((app-id (c:window-app-id window)))
        (when app-id (setf (gethash app-id index) window))))))

(defun saved-leaf-ids (form)
  "Every application id the arrangement names, deduplicated."
  (let ((ids '()))
    (labels ((walk (form)
               (when (consp form)
                 (if (eq (first form) :leaf)
                     (let ((id (getf (rest form) :window)))
                       (when id (pushnew id ids :test #'equal)))
                     (dolist (child form) (walk child))))))
      (walk form)
      ids)))

;;; ============================================================== commands

(defun current-workspace-node ()
  "The node at the cursor's workspace, as (INDEX NODE), or NIL."
  (let ((stack (c:world-workspaces r:*world*))
        (path (c:world-cursor r:*world*)))
    (when (and stack path)
      (let ((index (first path)))
        (when (< index (c:container-count stack))
          (list index (c:child-at stack index)))))))

(defun save-layout (name)
  "Save the workspace under the cursor as the layout NAME.

Windows are named by application id, not by identifier, so the file means
the same thing after a reboot."
  (let ((file (layout-file name))
        (workspace (current-workspace-node)))
    (cond
      ((null file) (r:notify "nowhere to save layouts") nil)
      ((null workspace) nil)
      (t (destructuring-bind (index node) workspace
           (declare (ignore index))
           ;; The identifiers in the serialized tree belong to THIS
           ;; workspace's windows; only they can translate to app-ids.
           (let ((mapping (make-hash-table)))
             (dolist (window (c:node-windows node))
               (when (c:window-identifier window)
                 (setf (gethash (c:window-identifier window) mapping)
                       (c:window-app-id window))))
             (ensure-directories-exist file)
             (with-open-file (out file :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
               (let ((*package* (find-package :keyword)))
                 (write (list :version 1
                              :root (identifier-to-app-id
                                     (r:serialize-node node) mapping))
                        :stream out))))
           (r:logmsg :info "saved layout ~a" name)
           name)))))

(defun restore-into-workspace (form index &key (mode *mode*)
                                              (world r:*world*))
  "Put FORM back into workspace INDEX of WORLD.

Returns :RESTORED, :INCOMPLETE (best-effort ran short somewhere) or :REFUSED
(exact mode found the arrangement could not be honoured whole).  The
workspace's own content is replaced; windows that were there and are not in
the arrangement come back as extra panes after it, because losing a window
is not any mode's price."

  ;; The strictness check first, before anything moves.
  (let* ((stack (c:world-workspaces world))
         (wanted (saved-leaf-ids form))
         (present (mapcar #'c:window-app-id
                          (c:node-windows (c:child-at stack index)))))
    (when (eq mode :exact)
      (unless (and (every (lambda (id) (member id present :test #'equal))
                          wanted)
                   (every (lambda (id) (member id wanted :test #'equal))
                          (remove nil present)))
        (return-from restore-into-workspace :refused)))
    ;; Build from the saved form against live windows.  DESERIALIZE-NODE
    ;; looks its windows up in an index; ours is keyed by app-id, which is
    ;; what the saved leaves carry.
    (let* ((index-table (live-window-index))
           (node (r:deserialize-node (first form) (rest form) index-table))
           ;; An empty pane where an application has not come back yet.
           (missing (count-if #'null
                              (mapcar #'c:leaf-window (c:node-leaves node))))
           ;; Windows that were here but are not in the arrangement:
           ;; appended as panes of their own, not dropped.
           (extras (loop for window in (c:node-windows (c:child-at stack index))
                         unless (member (c:window-app-id window)
                                        wanted :test #'equal)
                         collect (c:make-leaf window)))
           (node (cond
                   ((and extras (rest extras))
                    (c:make-split :horizontal (cons node extras) nil))
                   (extras
                    (c:make-split :horizontal (list node (first extras)) nil))
                   (t node))))
      (c:remove-child stack index)
      (c:insert-child stack index node)
      (setf (c:world-cursor world)
            (c:repair-path (c:world-root world)
                           (c:world-cursor world)))
      (if (zerop missing)
          :restored
          :incomplete))))

(defun guarded-read (file)
  "Read one form from FILE, or NIL.  A hand-edited layout file is untrusted
input, same as the core's state file."
  (ignore-errors
   (with-open-file (in file)
     (let ((*package* (find-package :keyword)))
       (read in nil nil)))))

(defun restore-layout (name &optional (world r:*world*))
  "Put the layout called NAME back into WORLD's cursor workspace.

Returns :RESTORED, :INCOMPLETE or :REFUSED, or NIL when there is no such
layout -- the same words the interactive command reports."
  (let ((file (layout-file name)))
    (when (and file (probe-file file))
      (let ((form (guarded-read file)))
        (and form
             (restore-into-workspace (getf form :root)
                                     (first (c:world-cursor world))
                                     :world world))))))

(r:defcommand (restore-named-layout "restore-layout") (name)
  "Restore the layout called NAME into the workspace under the cursor.

How strictly depends on *MODE*.  Best effort places everything it can;
exact refuses whole rather than half-honour.  Either way, a window that was
here and is not part of the arrangement comes back as an extra pane."
  (:interactive :layout-name)
  (ecase (restore-layout name)
    (:restored (r:mark-dirty) name)
    (:incomplete (r:mark-dirty)
                 (r:notify "restored ~a, some applications absent" name)
                 name)
    (:refused (r:notify "refusing ~a: it cannot be honoured whole" name) nil)
    ((nil) (r:notify "no layout named ~a" name) nil)))

(p:define-argument-type :layout-name "layout: "
  :documentation "A saved layout, with completion over the files."
  :candidates (all-layout-names))

(r:defcommand (save-named-layout "save-layout") (name)
  "Save the workspace under the cursor as the layout NAME."
  (:interactive :name)
  (cond
    ((save-layout name) name)
    (t nil)))
