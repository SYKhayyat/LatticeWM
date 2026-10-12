;;;; auto-respawn/auto-respawn.lisp --- Pick up where you left off.

(in-package #:auto-respawn)

;;; A session's windows are keyed by river identifier, which is a compositor
;;; local fact — meaningless after a reboot.  The declared-sessions extension
;;; can rebuild a session but only from a manifest the user writes.  This
;;; extension closes the gap the issue names: *record* what launched each
;;; window, save that beside the per-workspace arrangement, and on respawn run
;;; the recorded commands again into the panes they left.

;;; ================================================================ state

(defvar *enabled* nil "True while recording and the save hooks are installed.")

(defvar *launcher-commands* '()
  "What launched each window, as (APP-ID . ARGV) pairs, newest first.

Recorded best-effort: a window whose launch was not seen — opened by an
application rather than by a spawn — is simply absent, and respawn leaves
its pane empty rather than inventing a command.")

(defvar *recent-spawns* '()
  "The ARGV of recent spawns, newest first, capped.

The bridge between (spawn \"foot\") and the window that arrives a moment
later: the window manager sees the command and then the window, never the
two together, and this is where they meet.  Capped so a long session of
launching does not accumulate.")

(defparameter +recent-spawn-cap+ 16
  "How many spawns the recorder will match a window against.")

(defvar *window-command-guesses* '()
  "APP-ID to the command that (probably) launches it, for the app-ids whose
launcher's first word is not the app-id.

    (setf *window-command-guesses*
          (quote ((\"emacs\" . \"emacsclient -c\")
                 (\"ranger\" . \"alacritty -e ranger\"))))

Consulted when a window opens and no recent spawn matches it: the guess is
the fallback that makes the recorder useful for the applications whose
command line and app-id do not agree.")

(defvar *save-directory* nil
  "Where the recorded session is written, or NIL for <data>/respawn/.
Bound by tests.")

(defvar *spawn-function* nil
  "How respawn runs a recorded command, or NIL for R:SPAWN.

A function of one argument, the ARGV list.  Bound by tests, which must not
open real windows just because a session was recorded.")

(defvar *pending-arrivals* '()
  "Windows respawn is waiting for, as (APP-ID . PATH), newest first.

PATH is the pane the window is promised.  The placement itself happens
through :WINDOW-RULE answering :PATH while the window is managed, exactly
as the declared-sessions extension does it.")

(defvar *respawn-on-startup* nil
  "Run RESPAWN-SESSION once at startup when a recorded session exists.

Off by default: respawning applications at login is a choice, and the
command is always reachable.  Set it in init.lisp and ENABLE installs the
hook; the switch is consulted when the hook fires.")

(defparameter +session-version+ 1
  "The recorded-session file format version.")

(defvar *last-save* 0
  "Internal-real-time of the last debounced save.")

(defparameter *save-interval-seconds* 5
  "How long a changed session may sit before it is written out.
Five seconds is the point where the writes are invisible and a crash loses
at most the last five seconds — the same reasoning the core's own
*SAVE-INTERVAL-SECONDS* gives.")

(defun enabled-p () "True while recording." *enabled*)

(defun launcher-commands ()
  "What has been recorded, as (APP-ID . ARGV) pairs."
  (mapcar (lambda (pair) (cons (car pair) (copy-list (cdr pair))))
          *launcher-commands*))

;;; ================================================================ saving

(defun save-directory ()
  "Where the recorded session lives: the override, or respawn/ under the
first data directory that exists."
  (or *save-directory*
      (let ((dir (find-if
                  (lambda (root)
                    (let ((respawn (merge-pathnames "respawn/" root)))
                      (ignore-errors
                        (ensure-directories-exist respawn)
                        (probe-file respawn))))
                  (r:data-directories))))
        (and dir (merge-pathnames "respawn/" dir)))))

(defun session-file ()
  "The file the recorded session is kept in, or NIL with nowhere to keep it."
  (let ((dir (save-directory)))
    (and dir (merge-pathnames "session.lisp" dir))))

(defun identifier-to-app-id (form mapping)
  "Rewrite a serialized FORM's leaves from river identifiers to app-ids.

MAPPING is identifier -> app-id for the workspace's own windows.  An
identifier that means something for a moment is not the name an arrangement
wants; the app-id is what both yesterday and tomorrow know the window by."
  (if (atom form)
      form
      (if (eq (first form) :leaf)
          (let ((out '()))
            (loop for (key value) on (rest form) by #'cddr
                  do (setf out
                           (append out
                                   (list key (if (eq key :window)
                                                 (and value (gethash value mapping))
                                                 value)))))
            `(:leaf ,@out))
          (mapcar (lambda (child) (identifier-to-app-id child mapping))
                  form))))

(defun workspace-forms ()
  "Every workspace as (INDEX . FORM), where FORM is its serialized tree with
leaves naming windows by app-id."
  (let ((stack (c:world-workspaces r:*world*)))
    (when stack
      (loop for index below (c:container-count stack)
            for node = (c:child-at stack index)
            for mapping = (let ((table (make-hash-table :test #'equal)))
                            (dolist (window (c:node-windows node))
                              (when (c:window-identifier window)
                                (setf (gethash (c:window-identifier window)
                                               table)
                                      (c:window-app-id window))))
                            table)
            collect (list index
                          (identifier-to-app-id
                           (r:serialize-node node) mapping))))))

(defun guarded-save (file)
  "Write the session to FILE, swallowing the errors that must not be fatal."
  (ignore-errors
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* nil)
            (*print-pretty* t)
            (*package* (find-package :keyword)))
        (format out ";;;; LatticeWM auto-respawn session.  Written by the ~
                     auto-respawn extension.~%")
        (write (list :version +session-version+
                     :commands (mapcar (lambda (pair)
                                         (cons (car pair)
                                               (copy-list (cdr pair))))
                                       *launcher-commands*)
                     :workspaces (workspace-forms))
               :stream out)
        (terpri out)))))

(defun save-session ()
  "Write the recorded launchers and the per-workspace arrangement.

Never signals: a full disk produces a log line and a session that keeps
running, exactly the contract the core's own SAVE-STATE has."
  (let ((file (session-file)))
    (when file
      (guarded-save file)
      (r:logmsg :info "respawn: saved ~d launcher~:p and the arrangement"
                (length *launcher-commands*))
      file)))

(defun save-session-soon ()
  "Ask for a save, debounced so a held resize key does not write each step."
  (let ((now (get-internal-real-time)))
    (when (>= (- now *last-save*)
              (* *save-interval-seconds* internal-time-units-per-second))
      (setf *last-save* now)
      (save-session))))

;;; ============================================================= recording

(defun split-command (command)
  "COMMAND as an ARGV list, splitting on whitespace the way a shell would."
  (uiop:split-string (string-trim " " command)))

(defun note-spawn (argv)
  "Remember that ARGV was spawned, for the window it will produce."
  (when (consp argv)
    (push argv *recent-spawns*)
    (when (> (length *recent-spawns*) +recent-spawn-cap+)
      (setf *recent-spawns* (butlast *recent-spawns* 1))))
  argv)

(defun spawn-matching-p (argv app-id)
  "Could the window APP-ID plausibly have come from ARGV?  Its first word is."
  (and (consp argv) (string-equal (first argv) app-id)))

(defun most-recent-spawn-matching (app-id)
  "The newest recorded spawn whose first word is APP-ID, or NIL."
  (find-if (lambda (argv) (spawn-matching-p argv app-id)) *recent-spawns*))

(defun note-window-opened (window)
  "Record what launched WINDOW, if we can work it out.

Called from :WINDOW-OPENED.  A seen spawn whose first word is the window's
app-id is the fact and wins; the guess table is the fallback for the apps
whose launcher and app-id disagree.  The first recording of an app-id wins,
so reopening a window does not clobber the launch that survives a reboot."
  (when *enabled*
    (let ((app-id (c:window-app-id window)))
      (when (and app-id (not (assoc app-id *launcher-commands*
                                    :test #'equal)))
        (let* ((guess (cdr (assoc app-id *window-command-guesses*
                                  :test #'equal)))
               (argv (or (most-recent-spawn-matching app-id)
                         (and guess (split-command guess)))))
          (when argv
            (push (cons app-id (copy-list argv)) *launcher-commands*)
            (save-session-soon)))))))

(defun launcher-argv (command arguments)
  "The ARGV COMMAND will spawn, or NIL when it is not a launcher.

TERMINAL, EDITOR, BROWSER and FILES each call SPAWN directly rather than
through the command registry, so their arguments are empty at this layer;
their argv is the option they hand to spawn."
  (case (p:command-symbol command)
    (r:spawn arguments)
    (r:terminal (list p:*terminal*))
    (r:editor (list p:*editor*))
    (r:browser (list p:*browser*))
    (r:files (list p:*file-manager*))
    (t nil)))

(defun record-spawn-wrapper (command arguments continue)
  "The command wrapper: note every launcher's argv, then run the command."
  (let ((argv (launcher-argv command arguments)))
    (when argv (note-spawn argv)))
  (funcall continue))

;;; ============================================================= restoring

(defun guarded-read (file)
  "Read one form from FILE, or NIL.  A hand-edited file is untrusted input,
the same ruling the core's state file makes."
  (ignore-errors
    (with-open-file (in file)
      (let ((*package* (find-package :keyword)))
        (read in nil nil)))))

(defun substitute-leaves (form path)
  "Replace every leaf naming an app-id with an empty leaf, registering each
arrival for the PATH it will occupy.

Recurses through :CHILDREN (the sequential-container shape) and :CELLS
triples (the grid shape), so the walk works at every depth of every kind.
Kinds this module has never heard of pass through untouched."
  (cond
    ((atom form) form)
    ((eq (first form) :leaf)
     (let ((app-id (getf (rest form) :window)))
       (when (and app-id (stringp app-id))
         (push (cons app-id (copy-list path)) *pending-arrivals*)))
     '(:leaf))
    (t
     (let ((new-plist
            (loop for (key value) on (rest form) by #'cddr
                  append (list key
                               (cond
                                 ((eq key :children)
                                  (let ((out '()) (i 0))
                                    (dolist (child value (nreverse out))
                                      (push (substitute-leaves
                                             child (append path (list i)))
                                            out)
                                      (incf i))))
                                 ((eq key :cells)
                                  (let ((out '()))
                                    (dolist (triple value (nreverse out))
                                      (destructuring-bind (x y child) triple
                                        (push (list x y
                                                    (substitute-leaves
                                                     child (append path (list x y))))
                                              out)))))
                                 (t value))))))
       `(,(first form) ,@new-plist)))))

(defun restore-workspace (index form)
  "Put saved FORM back into workspace INDEX — replacing only an empty one.

The skeleton is built with empty panes before any command is spawned, so a
fast application cannot arrive before its pane.  A workspace that already
holds windows is left alone: a recorded arrangement is not permission to
drop what is live."
  (let ((stack (c:world-workspaces r:*world*)))
    (when stack
      (loop while (<= (c:container-count stack) index)
            do (let ((next (c:container-count stack)))
                 (c:insert-child stack next (c:make-leaf))))
      (let ((existing (c:child-at stack index)))
        (when (zerop (c:node-window-count existing))
          (let ((node (r:deserialize-node
                       (first form)
                       (rest (substitute-leaves form (list index)))
                       (make-hash-table :test #'equal))))
            (c:remove-child stack index)
            (c:insert-child stack index node)))))))

(defun rule-hook-bridge (window)
  "Answer :WINDOW-RULE with the path of the pane waiting for this window."
  (let ((entry (assoc (c:window-app-id window) *pending-arrivals*
                      :test #'equal)))
    (when entry
      (list :path (copy-list (cdr entry))))))

(defun fill-arrived-window (window)
  "Retire the arrival a window has just filled.

One matching arrival per window: two terminals were recorded, two arrivals
exist, each window consumes its own.  Called from :WINDOW-OPENED; the
placement itself already happened through the rule bridge."
  (let ((entry (assoc (c:window-app-id window) *pending-arrivals*
                      :test #'equal)))
    (when entry
      (setf *pending-arrivals* (remove entry *pending-arrivals*)))))

(defun spawn-command (argv)
  "Run ARGV through the seam: *SPAWN-FUNCTION* in tests, R:SPAWN in use."
  (if *spawn-function*
      (funcall *spawn-function* argv)
      (apply #'r:spawn argv)))

(defun respawn-session ()
  "Run every recorded launcher again, into the arrangement the windows left.

Returns the number of commands spawned, or NIL when there is no recorded
session or its version is not this one's.  A recorded command whose window
never arrives leaves the empty pane it was promised — the honest outcome
when an application has gone missing."
  (let ((file (session-file)))
    (when (and file (probe-file file))
      (let ((form (guarded-read file)))
        (when (and (consp form)
                   (eql +session-version+ (getf form :version)))
          ;; Build every skeleton before spawning, so a fast application
          ;; cannot arrive before its pane.
          (setf *pending-arrivals* '())
          (dolist (entry (getf form :workspaces))
            (when (consp entry)
              (destructuring-bind (index node-form) entry
                (when (consp node-form)
                  (restore-workspace index node-form)))))
          ;; Both hooks stay installed: arrivals may appear seconds from now,
          ;; and each hook is a no-op while nothing is pending.
          (p:add-hook :window-rule 'rule-hook-bridge)
          (p:add-hook :window-opened 'fill-arrived-window)
          (let ((spawned 0))
            (dolist (entry (getf form :commands))
              (when (consp entry)
                (spawn-command (cdr entry))
                (incf spawned)))
            (r:mark-dirty)
            (r:notify "respawned ~d application~:p" spawned)
            spawned))))))

(defun respawn-at-startup ()
  "The :LAYOUT-RESTORED hook: respawn once, at startup, if asked."
  (when *respawn-on-startup*
    (respawn-session)))

;;; ========================================================= enable/disable

(defun enable ()
  "Start recording launchers and saving the session as it changes.

Idempotent.  Installs the spawn wrapper, the recording handler, the
debounced save on layout change, a final save at shutdown, and — when
*RESPAWN-ON-STARTUP* — the respawn-on-restore hook."
  (p:add-command-wrapper #'record-spawn-wrapper)
  (p:add-hook :window-opened 'note-window-opened)
  (p:add-hook :layout-changed 'save-session-soon)
  (p:add-hook :layout-restored 'respawn-at-startup)
  (p:add-hook :shutdown 'save-session)
  (setf *enabled* t)
  nil)

(defun disable ()
  "Stop recording.  What was already saved is still there to respawn."
  (p:remove-command-wrapper #'record-spawn-wrapper)
  (p:remove-hook :window-opened 'note-window-opened)
  (p:remove-hook :layout-changed 'save-session-soon)
  (p:remove-hook :layout-restored 'respawn-at-startup)
  (p:remove-hook :shutdown 'save-session)
  (setf *enabled* nil)
  nil)