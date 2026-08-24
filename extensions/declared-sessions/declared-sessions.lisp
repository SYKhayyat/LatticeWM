;;;; declared-sessions/declared-sessions.lisp --- What should be open, said out loud.

(in-package #:declared-sessions)

;;; ================================================================ state

(defvar *sessions-directories* nil
  "Where session manifests live, or NIL for <data directory>/sessions/.
Bound by tests; the default is computed from the data directories.")

(defvar *pending-arrivals* '()
  "Windows this module is waiting for, as (APP-ID . PATH) pairs, most
recently declared first.  Each answers placement through :WINDOW-RULE with
a :PATH override -- the pane was built before the window existed, and this
is how the window finds it.")

(defun pending-arrivals ()
  "What the module is still waiting for."
  (copy-list *pending-arrivals*))

;;; ============================================================= manifests

(defun sessions-directory ()
  "The directory manifests are read from: the override, or sessions/ under
the first data directory that has one."
  (or *sessions-directories*
      (let ((dir (find-if
                  (lambda (root)
                    (let ((sessions (merge-pathnames "sessions/" root)))
                      (ignore-errors
                       (ensure-directories-exist sessions)
                       (probe-file sessions))))
                  (r:data-directories))))
        (and dir (merge-pathnames "sessions/" dir)))))

(defun session-file (name)
  "The file session NAME is declared in."
  (let ((dir (sessions-directory)))
    (and dir (merge-pathnames (make-pathname :name name :type "lisp") dir))))

(defun all-session-names ()
  "Every declared session name, sorted."
  (let ((dir (sessions-directory)))
    (when dir
      (sort (mapcar #'pathname-name
                    (directory (merge-pathnames "*.lisp" dir)))
            #'string<))))

;;; ============================================================ interpreter

(defun ensure-workspace (number)
  "The node at workspace NUMBER (counting from one), growing the workspace
list to reach it exactly as the WORKSPACE command does."
  (let ((stack (c:world-workspaces r:*world*))
        (index (1- (max 1 number))))
    (loop while (<= (c:container-count stack) index)
          do (let ((next (c:container-count stack)))
               (c:insert-child stack next
                               (or (p:make-workspace (p:current-policy)
                                                     r:*world* next)
                                   (c:make-leaf)))))
    (c:child-at stack index)))

(defun guess-app-id (command)
  "The application id a command's window will probably announce: the first
word.  A guess, because app-ids are the application's own admission --
override per app with (:as \"real-id\") where they differ."
  (first (uiop:split-string (string-trim " " command))))

(defun workspace-form-p (form)
  "Is FORM a workspace declaration?"
  (and (consp form)
       (symbolp (first form))
       (string= (symbol-name (first form)) "WORKSPACE")))

(defun build-node (spec &optional (path '(0)))
  "Turn a manifest form into tree structure, registering arrivals.

:SPLIT becomes a split; :APP becomes an empty leaf -- the pane that waits --
plus a pending arrival so the window knows where to go when it shows up.
PATH is the absolute path of SPEC in the workspace, which is what the
arrival rule will name."
  (case (first spec)
    (:split
     (destructuring-bind (axis . children) (rest spec)
       (c:make-split axis
                     (loop for child in children
                           for i from 0
                           collect (build-node
                                    child (append path (list i))))
                     nil)))
    (:app
     (destructuring-bind (command &key ((:as declared-id))) (rest spec)
       (let ((leaf (c:make-leaf))
             (id (or declared-id (guess-app-id command))))
         (push (cons id (copy-list path)) *pending-arrivals*)
         (r:logmsg :info "session: waiting for ~a at ~s" id path)
         leaf)))
    (t (error "session: cannot interpret ~s" spec))))

(defun fill-arrived-window (window)
  "Retire the arrival a window has just filled.

Called from :WINDOW-OPENED.  The placement itself already happened -- the
rule answered :PATH while the window was being managed -- so all that is
left is to stop promising the pane to the next window of the same kind."
  (let ((app-id (c:window-app-id window)))
    ;; Remove ONE matching arrival per arriving window: two terminals were
    ;; declared, two arrivals exist, each window consumes its own.
    (let ((entry (assoc app-id *pending-arrivals* :test #'equal)))
      (when entry
        (setf *pending-arrivals* (remove entry *pending-arrivals*))))))

(defun rule-hook-bridge (window)
  "Answer :WINDOW-RULE with the path of the pane waiting for this window."
  (let ((path (cdr (assoc (c:window-app-id window)
                          *pending-arrivals* :test #'equal))))
    (when path
      (list :path (copy-list path)))))

;;; ======================================================= the interpreter

(defun interpret-workspace-form (form name)
  "Interpret one (:workspace NUMBER . CHILDREN) declaration."
  (destructuring-bind (number &body children) (rest form)
    (let* ((index (1- (max 1 number)))
           (stack (c:world-workspaces r:*world*))
           (spec (if (= 1 (length children))
                     (first children)
                     (cons 'split children))))
      ;; Grow the workspace list first, exactly as the WORKSPACE command
      ;; would, so the declaration may name any number.
      (ensure-workspace (max 1 number))
      (when (< index (c:container-count stack))
        (let ((existing (c:child-at stack index)))
          (cond
            ;; Never drop live windows for a declaration.
            ((plusp (length (c:node-windows existing)))
             (r:logmsg :warn "session ~a: workspace ~d is occupied; left alone"
                       name number))
            (t
             ;; Build the skeleton BEFORE spawning, so a fast application
             ;; cannot arrive before its pane.
             (c:remove-child stack index)
             (c:insert-child stack index
                             (build-node spec (list index)))
             (spawn-apps spec))))))))

(defvar *spawn-function* nil
  "How :APP commands are run, or NIL for R:SPAWN.

A function of one argument, the argv list.  NIL means the real thing --
detached, output discarded.  Bound by tests, which must not open actual
windows on the user's desktop just because a manifest says (app \"foot\").")

(defun spawn-apps (spec)
  "Spawn every :APP in SPEC.  The command string splits on spaces, which is
what the manifest promised."
  (when (consp spec)
    (case (first spec)
      (:app
       (let ((argv (uiop:split-string (string-trim " " (second spec)))))
         (if *spawn-function*
             (funcall *spawn-function* argv)
             (apply #'r:spawn argv))))
      (:split (mapc #'spawn-apps (cddr spec))))))

(defun read-manifest (file)
  "Read every form from FILE, or NIL if it cannot be read."
  (ignore-errors
   (with-open-file (in file)
     (let ((*package* (find-package :keyword)))
       (loop for form = (read in nil nil)
             until (null form)
             collect form)))))

(defun load-session (name)
  "Build the arrangement declared in session NAME.

Workspaces are grown and their contents replaced with the declared skeleton;
applications are spawned; windows that arrive are placed by their pending
:path rules.  A workspace that already holds windows is left alone -- the
declaration describes what SHOULD be open, not permission to drop what IS.
Returns the session name, or NIL if there is no such file or it cannot be
read."
  (let ((file (session-file name)))
    (cond
      ((null file) nil)
      ((not (probe-file file))
       (r:notify "no session named ~a" name)
       nil)
      (t
       ;; LET*, not LET: VALID's initializer reads FORMS, and a plain LET
       ;; evaluates every initializer outside the scope of its own
       ;; bindings -- which is exactly how this function once compiled with
       ;; FORMS unbound on every path.
       (let* ((forms (read-manifest file))
              ;; Every top-level form must be a workspace declaration.
              ;; READ cannot be trusted to reject a truncated manifest --
              ;; with errors suppressed, an unclosed parenthesis is
              ;; silently closed at end of file -- so validity is checked
              ;; here, structurally, against what the interpreter
              ;; understands.
              (valid (and forms
                          (every #'workspace-form-p forms))))
         (unless valid
           (r:notify "~a is not readable as a session" name)
           (return-from load-session nil))
         ;; Both hooks stay installed: arrivals may appear seconds from
         ;; now, and each hook is a no-op while nothing is pending.
         (p:add-hook :window-rule 'rule-hook-bridge)
         (p:add-hook :window-opened 'fill-arrived-window)
         (dolist (form forms)
           (interpret-workspace-form form name))
         (r:mark-dirty)
         name)))))

(p:define-argument-type :session-name "session: "
  :documentation "A declared session, with completion over the manifests."
  :candidates (all-session-names))

(r:defcommand load-session-command (name)
  "Build the arrangement declared in session NAME."
  (:interactive :session-name)
  (load-session name))
