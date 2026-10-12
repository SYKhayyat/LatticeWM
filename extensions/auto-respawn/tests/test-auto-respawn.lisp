;;;; tests/test-auto-respawn.lisp --- Pick up where you left off.

(defpackage #:auto-respawn/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ar #:auto-respawn))
  (:export #:run-all))

(in-package #:auto-respawn/tests)

(def-suite auto-respawn
    :description "Recording launchers and respawning the arrangement.")
(in-suite auto-respawn)

(t*:register-extension-suite "AUTO-RESPAWN/TESTS" "AUTO-RESPAWN")

(defun run-all ()
  "Run the AUTO-RESPAWN suite."
  (let ((results (run 'auto-respawn)))
    (explain! results)
    (values (results-status results) (length results))))

(defun fresh-save-directory ()
  "A directory that exists and holds nobody else's session.  Unique per run:
RANDOM alone is deterministic on hosts whose entropy is quiet."
  (let ((dir (merge-pathnames (format nil "ar-test-~d-~d/"
                                      (sb-posix:getpid) (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defmacro with-session (&body body)
  `(let ((ar::*enabled* t)
         (ar::*launcher-commands* '())
         (ar::*recent-spawns* '())
         (ar::*pending-arrivals* '())
         (ar::*save-directory* (fresh-save-directory))
         (ar::*spawn-function* nil)
         (ar::*window-command-guesses* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ============================================================ recording

(test a-spawns-window-is-recorded-by-its-app-id
  "A spawn whose first word is the window's app-id is the launcher."
  (with-session
    (ar::note-spawn '("foot"))
    (ar::note-window-opened (make-instance 'c:window :app-id "foot"))
    (is (equal '(("foot" "foot")) ar::*launcher-commands*))))

(test the-guess-table-fills-what-the-spawn-cannot-attribute
  "A launcher whose first word is not the app-id is attributed by the guess
table, not by the unseen spawn."
  (with-session
    (let ((ar::*window-command-guesses*
           '(("emacs" . "emacsclient -c"))))
      (ar::note-spawn '("emacsclient" "-c"))
      ;; The spawn's first word is emacsclient, which does not match; the
      ;; guess table's answer is the fallback.
      (ar::note-window-opened (make-instance 'c:window :app-id "emacs"))
      (is (equal '(("emacs" "emacsclient" "-c")) ar::*launcher-commands*)
          "the guessed command, split on spaces, was recorded"))))

(test a-window-with-no-visible-launcher-is-not-recorded
  "A window whose launch was never seen is absent from the record — an honest
blank, not a guess."
  (with-session
    (ar::note-window-opened (make-instance 'c:window :app-id "firefox"))
    (is (null ar::*launcher-commands*))))

(test the-first-recording-of-an-app-id-wins
  "Reopening a window does not clobber the launch that survives a reboot."
  (with-session
    (ar::note-spawn '("foot"))
    (ar::note-window-opened (make-instance 'c:window :app-id "foot"))
    (ar::note-spawn '("foot" "--old"))
    (ar::note-window-opened (make-instance 'c:window :app-id "foot"))
    (is (equal '(("foot" "foot")) ar::*launcher-commands*))))

;;; ============================================================ enable

(test enable-installs-and-disable-removes-the-recording-hooks
  "ENABLE attaches the recording handler and the spawn wrapper; DISABLE takes
them back.  Membership, not counts."
  (with-session
    (unwind-protect
         (progn
           (ar:enable)
           (is-true (member 'ar::note-window-opened
                            (gethash :window-opened p:*hooks*))
                    "the recorder listens to :window-opened")
           (is-true (member #'ar::record-spawn-wrapper p:*command-wrappers*)
                    "the spawn wrapper is installed")
           (ar:disable)
           (is-false (member 'ar::note-window-opened
                             (gethash :window-opened p:*hooks*))
                     "and detached"))
      (ignore-errors (ar:disable)))))

(test the-wrapper-records-the-launcher-commands-only
  "The wrapper leaves every non-launcher command alone."
  (with-session
    (let ((p:*terminal* "foot"))
      (is (null (ar::launcher-argv (p:find-command "focus") (list :left))))
      (is (equal '("foot") (ar::launcher-argv (p:find-command "terminal") ())))
      (is (equal '("emacsclient" "-c")
                 (ar::launcher-argv (p:find-command "spawn")
                                    (list "emacsclient" "-c")))))))

;;; ================================================================ saving

(test save-writes-app-ids-not-identifiers
  "The session file names windows by application id, because an identifier is
a compositor-local fact and the whole point is that it outlives one."
  (with-session
    (let ((emacs (make-instance 'c:window :app-id "emacs" :identifier "E"))
          (mail (make-instance 'c:window :app-id "thunderbird" :identifier "T")))
      (p:on-window-open p:*policy* r:*world* emacs)
      (p:on-window-open p:*policy* r:*world* mail)
      (setf ar::*launcher-commands*
            (list (cons "emacs" '("emacsclient" "-c"))
                  (cons "thunderbird" '("thunderbird"))))
      (ar::save-session))
    (let* ((form (with-open-file (in (ar::session-file))
                   (let ((*package* (find-package :keyword))) (read in))))
           (commands (getf form :commands))
           (workspaces (getf form :workspaces))
           (ws0 (cadr (assoc 0 workspaces))))
      (is (eql ar::+session-version+ (getf form :version)))
      (is (equal '("emacs" . ("emacsclient" "-c"))
                 (first (member "emacs" commands :test #'equal :key #'car)))
          "the launcher and its argv round-tripped")
      (is (eq :split (first ws0)))
      (is (equal '("emacs" "thunderbird")
                 (sort (mapcar (lambda (leaf)
                                 (getf (rest leaf) :window))
                               (getf (rest ws0) :children))
                       #'string<))
          "the leaves name windows by app-id, not identifier"))))

;;; ============================================================ restoring

(defun arrive (app-id)
  "A window of APP-ID appears, as the runtime would deliver it: placement
through the policy, then the :window-opened bookkeeping."
  (let ((window (make-instance 'c:window :app-id app-id)))
    (p:on-window-open p:*policy* r:*world* window)
    (p:run-hooks :window-opened window)
    window))

(defun workspace-leaves ()
  (c:node-leaves (c:child-at (c:world-workspaces r:*world*) 0)))

(defun workspace-app-ids ()
  (sort (mapcar #'c:window-app-id (c:node-windows
                                    (c:child-at (c:world-workspaces r:*world*) 0)))
        #'string<))

(defun seed-session ()
  "Record and save a two-window session: emacs left of thunderbird."
  (let ((emacs (make-instance 'c:window :app-id "emacs" :identifier "E"))
        (mail (make-instance 'c:window :app-id "thunderbird" :identifier "T")))
    (p:on-window-open p:*policy* r:*world* emacs)
    (p:on-window-open p:*policy* r:*world* mail))
  (setf ar::*launcher-commands*
        (list (cons "emacs" '("emacsclient" "-c"))
              (cons "thunderbird" '("thunderbird"))))
  (ar::save-session))

(test respawn-roundtrips-across-a-reboot
  "Save yesterday, throw the world away, respawn — and the commands run again
into the arrangement they left."
  (with-session
    (seed-session)
    ;; Today: a fresh world, as a reboot leaves it, and a spawn seam that
    ;; records instead of launching.
    (setf r:*world* (c:make-world))
    (setf ar::*launcher-commands* '()
          ar::*pending-arrivals* '())
    (let (spawned)
      (setf ar::*spawn-function* (lambda (argv) (push argv spawned)))
      (is (= 2 (ar:respawn-session)) "both launchers ran again")
      (is (= 2 (length spawned)))
      (dolist (app-id (mapcar #'car ar::*pending-arrivals*))
        (arrive app-id))
      (is (equal '("emacs" "thunderbird") (workspace-app-ids))
          "each application landed in the pane it left")
      (is (null ar::*pending-arrivals*) "and no arrival is still promised"))))

(test respawn-leaves-an-empty-pane-for-a-lost-command
  "A recorded launcher whose window never arrives leaves the empty pane it was
promised — an incomplete morning beats no morning at all."
  (with-session
    (seed-session)
    (setf r:*world* (c:make-world))
    (setf ar::*launcher-commands* '())
    (let ((ar::*spawn-function* (lambda (argv) (declare (ignore argv)))))
      (ar:respawn-session)
      (arrive "emacs")          ; thunderbird never comes back
      (is (= 2 (length (workspace-leaves))) "the split keeps both panes")
      (is (= 1 (length (remove nil (mapcar #'c:leaf-window
                                           (workspace-leaves)))))
          "one window, one empty pane")
      (is (assoc "thunderbird" ar::*pending-arrivals* :test #'equal)
          "thunderbird's pane is still promised"))))

(test respawn-refuses-a-version-it-does-not-know
  "A session file of another version is declined rather than half-understood."
  (with-session
    (with-open-file (out (ar::session-file) :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
      (let ((*package* (find-package :keyword)))
        (write '(:version 999 :commands () :workspaces ()) :stream out)))
    (is (null (ar:respawn-session)))))

(test respawn-declines-when-there-is-no-session
  "No file, no respawn — and no error."
  (with-session
    (is (null (ar:respawn-session)))))