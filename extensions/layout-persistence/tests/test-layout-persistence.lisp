;;;; tests/test-layout-persistence.lisp --- Arrangements by name, not by id.

(defpackage #:layout-persistence/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:lp #:layout-persistence))
  (:export #:run-all))

(in-package #:layout-persistence/tests)

(def-suite layout-persistence
    :description "Named workspace layouts keyed by app-id.")
(in-suite layout-persistence)

(t*:register-extension-suite "LAYOUT-PERSISTENCE/TESTS" "LAYOUT-PERSISTENCE")

(defun run-all ()
  "Run the LAYOUT-PERSISTENCE suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'layout-persistence)))
    (explain! results)
    (values (results-status results) (length results))))

(defun fresh-layouts-directory ()
  "A directory that exists and holds nobody else's layouts."
  (let ((dir (merge-pathnames (format nil "lp-test-~d/" (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defmacro with-layouts (&body body)
  `(let ((lp::*enabled* nil)
         (lp::*mode* :best-effort)
         (lp::*layouts-directory* (fresh-layouts-directory))
         (lp::*live-windows* '())
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; A workspace list ships with one empty leaf; put NODE in its place.
(defun install-workspace-content (node &optional (world r:*world*))
  (let ((stack (c:world-workspaces world)))
    (c:remove-child stack 0)
    (c:insert-child stack 0 node)))

(defun workspace-app-ids (&optional (world r:*world*))
  (mapcar #'c:window-app-id (c:node-windows (c:child-at
                                             (c:world-workspaces world) 0))))

;;; ================================================================ tests

(test save-writes-app-ids-not-identifiers
  "The file names windows by application id.  An identifier is a
compositor-local fact; the whole point of a named layout is that it outlives
one."
  (with-layouts
    (install-workspace-content
     (c:make-split :horizontal
                   (list (c:make-leaf (make-instance 'c:window :app-id "emacs" :identifier "E"))
                         (c:make-leaf (make-instance 'c:window
                                                     :app-id "thunderbird"
                                                     :identifier "T")))
                   nil))
    (lp:save-layout "work")
    ;; REST is the plist; the tag itself is not a key.
    (let* ((form (with-open-file (in (lp::layout-file "work"))
                   (let ((*package* (find-package :keyword))) (read in))))
           (plist (rest (getf form :root)))
           (children (getf plist :children)))
      (is (equal :horizontal (getf plist :axis)))
      (is (equal "emacs"
                 (getf (rest (first children)) :window))
          "the first leaf names emacs by app-id")
      (is (equal "thunderbird"
                 (getf (rest (second children)) :window))
          "the second names thunderbird"))))

(test restore-roundtrips-across-a-reboot
  "Save, throw the world away, open fresh windows that happen to have the
same application ids -- and the arrangement comes back."
  (with-layouts
    ;; Yesterday: editor left of mail.
    (setf lp::*live-windows*
          (list (make-instance 'c:window :app-id "emacs" :identifier "E")
                (make-instance 'c:window :app-id "thunderbird" :identifier "T")))
    (install-workspace-content
     (c:make-split :horizontal
                   (list (c:make-leaf (first lp::*live-windows*))
                         (c:make-leaf (second lp::*live-windows*)))
                   nil))
    (lp:save-layout "work")
    ;; Today: identifiers gone, new windows, empty workspace.
    (let ((editor (make-instance 'c:window :app-id "emacs" :identifier "E2"))
          (mail (make-instance 'c:window :app-id "thunderbird" :identifier "T")))
      (setf lp::*live-windows* (list mail editor))
      (install-workspace-content (c:make-leaf))
      (is (eq :restored (lp:restore-layout "work")))
      ;; The saved order survives even though the live list was reversed.
      (let ((ids (workspace-app-ids)))
        (is (= 2 (length ids)))
        (is (member "emacs" ids :test #'equal))
        (is (member "thunderbird" ids :test #'equal))))))

(test best-effort-leaves-an-empty-pane-for-the-absent
  ":BEST-EFFORT places what came back and keeps the place for what did not --
an incomplete morning beats no morning at all."
  (with-layouts
    (setf lp::*live-windows* (list (make-instance 'c:window :app-id "emacs" :identifier "E2")))
    (install-workspace-content
     (c:make-split :horizontal
                   (list (c:make-leaf (first lp::*live-windows*))
                         (c:make-leaf (make-instance 'c:window
                                                     :app-id "thunderbird")))
                   nil))
    (lp:save-layout "work")
    (install-workspace-content (c:make-leaf))
    (setf lp::*live-windows* (list (make-instance 'c:window :app-id "emacs" :identifier "E2")))
    (is (eq :incomplete (lp:restore-layout "work")))
    ;; Two leaves: emacs placed, and an empty pane keeping thunderbird's
    ;; place for the day it comes back.
    (is (= 2 (length (c:node-leaves
                      (c:child-at (c:world-workspaces r:*world*) 0)))))))

(test exact-refuses-what-it-cannot-honour-whole
  ":EXACT changes nothing when an application is missing.  Half a remembered
layout is worse than none, if you say so."
  (with-layouts
    (setf lp::*mode* :exact)
    (setf lp::*live-windows* (list (make-instance 'c:window :app-id "emacs" :identifier "E2")))
    (install-workspace-content
     (c:make-split :horizontal
                   (list (c:make-leaf (first lp::*live-windows*))
                         (c:make-leaf (make-instance 'c:window
                                                     :app-id "thunderbird")))
                   nil))
    (lp:save-layout "work")
    ;; Thunderbird never came back.
    (install-workspace-content (c:make-leaf))
    (setf lp::*live-windows* (list (make-instance 'c:window :app-id "emacs" :identifier "E2")))
    (is (eq :refused (lp:restore-layout "work")))
    ;; And the workspace is untouched: still the empty pane we left there.
    (is (typep (c:child-at (c:world-workspaces r:*world*) 0) 'c:leaf))))

(test extras-come-back-as-extra-panes
  "A window ON THE WORKSPACE that the arrangement does not name is appended,
not dropped -- losing a window is not any mode's price."
  (with-layouts
    (let ((emacs (make-instance 'c:window :app-id "emacs" :identifier "E2"))
          (firefox (make-instance 'c:window :app-id "firefox" :identifier "F")))
      (setf lp::*live-windows* (list emacs firefox))
      ;; Saved with only firefox on the workspace.
      (install-workspace-content (c:make-leaf firefox))
      (lp:save-layout "just-browser")
      ;; Today BOTH sit on the workspace, but the arrangement only named
      ;; firefox -- emacs was opened here afterwards.
      (setf lp::*live-windows* (list emacs firefox))
      (install-workspace-content
       (c:make-split :horizontal
                     (list (c:make-leaf emacs) (c:make-leaf firefox))
                     nil))
      (is (eq :restored (lp:restore-layout "just-browser")))
      (let ((ids (workspace-app-ids)))
        (is (member "emacs" ids :test #'equal)
            "the extra window survived")
        (is (member "firefox" ids :test #'equal)
            "and so did the arrangement")))))

(test delete-and-list
  "DELETE-LAYOUT removes the file and ALL-LAYOUT-NAMES reports what is left,
sorted, because completion candidates are read by people."
  (with-layouts
    (install-workspace-content (c:make-leaf))
    (lp:save-layout "zeta")
    (lp:save-layout "alpha")
    (is (equal '("alpha" "zeta") (lp:all-layout-names)))
    (is (equal "zeta" (lp:delete-layout "zeta")))
    (is (equal '("alpha") (lp:all-layout-names)))
    (is-false (lp:delete-layout "never-existed"))))

(test enable-installs-save-on-change-hook
  "ENABLE attaches the save-on-change hook when the option says so, and
DISABLE takes it back; *SAVE-ON-CHANGE* NIL enables without one.

Membership, not counts: other suites in this image attach their own things,
and an absolute count would be asserting on their tidiness."
  (with-layouts
    (unwind-protect
         (progn
           (setf lp::*save-on-change* t)
           (lp:enable)
           (is-true (member 'lp::note-layout-changed
                            (gethash :layout-changed p:*hooks*))
                    "attached")
           (lp:disable)
           (is-false (member 'lp::note-layout-changed
                             (gethash :layout-changed p:*hooks*))
                     "and detached"))
      (ignore-errors (lp:disable))))
  (with-layouts
    (unwind-protect
         (progn
           (setf lp::*save-on-change* nil)
           (lp:enable)
           (is-false (member 'lp::note-layout-changed
                             (gethash :layout-changed p:*hooks*))
                     "no hook when the option is off"))
      (ignore-errors (lp:disable)))))
