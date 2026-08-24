;;;; tests/test-window-restarts.lisp --- A crash is a question, not a log line.

(defpackage #:window-restarts/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:wr #:window-restarts))
  (:export #:run-all))

(in-package #:window-restarts/tests)

(def-suite window-restarts :description "Retry, undo, dismiss.")
(in-suite window-restarts)

(t*:register-extension-suite "WINDOW-RESTARTS/TESTS" "WINDOW-RESTARTS")

(defun run-all ()
  "Run the WINDOW-RESTARTS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'window-restarts)))
    (explain! results)
    (values (results-status results) (length results))))

(defmacro with-restarts (&body body)
  `(let ((wr::*enabled* nil)
         (wr::*last-broken* nil)
         (wr::*last-user-close* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun close-a-window (app-id)
  "Run :WINDOW-CLOSED the way the runtime would."
  (p:run-hooks :window-closed (make-instance 'c:window :app-id app-id)))

;;; ================================================================ tests

(test unexpected-exit-is-recorded-and-signalled
  "A window that goes away unasked-for leaves the app-id reachable, and any
HANDLER-BIND out there sees a BROKEN-WINDOW saying so."
  (with-restarts
    (let ((seen nil))
      (wr:enable)
      (unwind-protect
           (progn
             (handler-bind ((wr:broken-window
                             (lambda (condition)
                               (setf seen (wr:broken-app-id condition)))))
               (close-a-window "editor"))
             (is (equal "editor" seen) "the handler was told")
             (is (equal "editor" (wr:last-broken-app-id))
                 "and so was the menu"))
        (wr:disable)))))

(test a-requested-close-is-not-a-crash
  "Running CLOSE arms the suppression; a window vanishing within the window
is life working normally, not a broken window."
  (with-restarts
    (let ((seen nil))
      (wr:enable)
      (unwind-protect
           (progn
             ;; The wrapper sees every command; hand it CLOSE the way
             ;; RUN-COMMAND would.
             (wr::note-user-close (p:find-command "close") '()
                                  (lambda () nil))
             (handler-bind ((wr:broken-window
                             (lambda (condition)
                               (declare (ignore condition))
                               (setf seen t))))
               (close-a-window "editor"))
             (is-false seen "no condition")
             (is-false (wr:last-broken-app-id) "and nothing remembered"))
        (wr:disable)))))

(test dismissal-forgets-without-touching-anything-else
  "DISMISS clears the memory; there is nothing else to do, which is the point."
  (with-restarts
    (setf wr::*last-broken* (list :app-id "editor" :time (get-universal-time)))
    (wr:dismiss-broken-window)
    (is-false (wr:last-broken-app-id))))

(test retry-respawns-and-clears
  "RETRY spawns the application again -- the same name, because windows are
not processes and a resurrection is not on offer -- and forgets the crash."
  (with-restarts
    (setf wr::*last-broken* (list :app-id "true" :time (get-universal-time)))
    ;; "true" exits 0 immediately; SPAWN detaches, so this cannot hang and
    ;; has no observable effect except that it did not error.
    (wr:retry-broken-window)
    (is-false (wr:last-broken-app-id) "the crash is forgotten")))

(test retry-with-nothing-to-retry-says-so
  "RETRY with no remembered crash declines rather than spawning nothing."
  (with-restarts
    (is-false (wr:retry-broken-window))))

(test the-menu-binds-the-three-answers
  "The menu is R retry, U undo, D dismiss -- the order the notification says,
because a menu whose keys are a surprise is a puzzle, not a menu."
  (with-restarts
    (is (equal '("retry-broken-window") (r:lookup-key wr::*menu* (r:kbd "r"))))
    (is (equal '("undo-for-broken-window") (r:lookup-key wr::*menu* (r:kbd "u"))))
    (is (equal '("dismiss-broken-window")
               (r:lookup-key wr::*menu* (r:kbd "d"))))))

(test enable-and-disable-wire-and-unwire
  "ENABLE installs the hook; DISABLE takes it back."
  (with-restarts
    (unwind-protect
         (progn
           (wr:enable)
           (is-true (member 'wr::note-window-closed
                            (gethash :window-closed p:*hooks*))
                    "attached")
           (wr:disable)
           (is-false (member 'wr::note-window-closed
                             (gethash :window-closed p:*hooks*))
                     "detached"))
      (ignore-errors (wr:disable)))))
