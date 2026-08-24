;;;; tests/test-declared-sessions.lisp --- What should be open, said out loud.

(defpackage #:declared-sessions/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ds #:declared-sessions))
  (:export #:run-all))

(in-package #:declared-sessions/tests)

(def-suite declared-sessions :description "Sessions from a file.")
(in-suite declared-sessions)

(t*:register-extension-suite "DECLARED-SESSIONS/TESTS" "DECLARED-SESSIONS")

(defun run-all ()
  "Run the DECLARED-SESSIONS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'declared-sessions)))
    (explain! results)
    (values (results-status results) (length results))))

(defun fresh-sessions-directory ()
  (let ((dir (merge-pathnames (format nil "ds-test-~d/" (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defun write-session (name text)
  "Write a manifest into the test sessions directory."
  (let ((file (merge-pathnames (make-pathname :name name :type "lisp")
                               ds::*sessions-directories*)))
    (with-open-file (out file :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))
    name))

(defvar *spawned* '())

(defmacro with-sessions (&body body)
  `(let ((ds::*sessions-directories* (fresh-sessions-directory))
         (ds::*pending-arrivals* '())
         ;; Tests must never run a manifest's commands for real: the spawn
         ;; seam records what WOULD have been launched.
         (ds::*spawn-function* (lambda (argv)
                                 (push (copy-list argv) *spawned*)))
         (*spawned* '())
         (p:*hooks* (make-hash-table :test #'eq))
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

;;; ---------------------------------------------------------------- tests

(test manifests-spawn-through-the-seam
  "A loaded session records exactly what it WOULD launch -- and in tests,
records instead of launching, because a test suite has no business opening
windows on anybody's desktop."
  (with-sessions
    (write-session "work"
                   "(workspace 1
                      (split :horizontal
                             (app \"emacsclient -c\")
                             (app \"foot\")))")
    (is (equal "work" (ds:load-session "work")))
    (is (= 2 (length *spawned*)))
    (is (member '("emacsclient" "-c") *spawned* :test #'equal)
        "the emacsclient command was recorded")
    (is (member '("foot") *spawned* :test #'equal)
        "and so was the foot command")))
(test skeleton-is-built-before-the-windows-exist
  "Loading a session grows the workspace list and builds the declared split
as empty panes -- the panes wait for the windows, not the other way round."
  (with-sessions
    (write-session "work"
                   "(workspace 1
                      (split :horizontal
                             (app \"emacsclient -c\")
                             (app \"foot\")))")
    (ds:load-session "work")
    (is (= 1 (c:container-count (c:world-workspaces r:*world*))))
    (let ((node (c:child-at (c:world-workspaces r:*world*) 0)))
      (is (= 2 (length (c:node-leaves node))) "two declared panes")
      (is (= 0 (length (c:node-windows node))) "and no windows yet"))
    (is (= 2 (length (ds:pending-arrivals)))
        "two arrivals pending")))

(test arrivals-answer-placement-with-a-path
  "A window whose app-id is pending gets its pane's path as a :PATH rule --
which is how the window finds the pane that was built for it."
  (with-sessions
    (write-session "solo" "(workspace 1 (app \"foot\"))")
    (ds:load-session "solo")
    ;; The bridge answers through :WINDOW-RULE like placement asks.
    (let ((answers (p:run-hooks :window-rule
                                (make-instance 'c:window :app-id "foot"))))
      (is (equal '((:path (0))) (remove nil answers))
          "the pending arrival names its pane"))))

(test paths-distinguish-siblings-in-a-split
  "Two applications declared in one split get two different panes."
  (with-sessions
    (write-session "split"
                   "(workspace 1 (split :horizontal
                                        (app \"a\")
                                        (app \"b\")))")
    (ds:load-session "split")
    (let ((left (p:run-hooks :window-rule
                             (make-instance 'c:window :app-id "a")))
          (right (p:run-hooks :window-rule
                              (make-instance 'c:window :app-id "b"))))
      (is (equal '((:path (0 0))) (remove nil left)))
      (is (equal '((:path (0 1))) (remove nil right))))))

(test filling-consumes-one-arrival-per-window
  "When a window arrives, its arrival retires; two terminals declared means
two arrivals exist, each window consumes its own."
  (with-sessions
    (write-session "two"
                   "(workspace 1 (split :horizontal
                                        (app \"foot\")
                                        (app \"foot\")))")
    (ds:load-session "two")
    (is (= 2 (length (ds:pending-arrivals))))
    ;; First terminal arrives.
    (p:run-hooks :window-opened (make-instance 'c:window :app-id "foot"))
    (is (= 1 (length (ds:pending-arrivals))))
    ;; Second arrives.
    (p:run-hooks :window-opened (make-instance 'c:window :app-id "foot"))
    (is (= 0 (length (ds:pending-arrivals))) "all filled")))

(test unknown-applications-do-not-consume-a-slot
  "A window whose app-id nothing waits for leaves the arrivals alone."
  (with-sessions
    (write-session "one" "(workspace 1 (app \"foot\"))")
    (ds:load-session "one")
    (p:run-hooks :window-opened (make-instance 'c:window :app-id "firefox"))
    (is (= 1 (length (ds:pending-arrivals)))
        "still waiting for foot")))

(test occupied-workspaces-are-left-alone
  "The declaration describes what SHOULD be open, not permission to drop
what IS: a workspace holding windows is skipped with a note."
  (with-sessions
    ;; Put a window on workspace one first.
    (p:on-window-open p:*policy* r:*world*
                      (make-instance 'c:window :app-id "live-window"))
    (write-session "grabby" "(workspace 1 (app \"new-thing\"))")
    (ds:load-session "grabby")
    (is (= 1 (length (c:node-windows
                      (c:child-at (c:world-workspaces r:*world*) 0))))
        "the live window survived")
    (is (= 0 (length (ds:pending-arrivals)))
        "and no arrival was promised to a pane that does not exist")))

(test unreadable-manifests-decline
  "A manifest that is not a list of workspace declarations is declined,
not half-loaded.  Two shapes are covered: something readable but wrong
(an unknown top-level form), and the truncated file -- which READ silently
closes at end of file, so structural validation, not the reader, is what
catches it."
  (with-sessions
    (write-session "nonsense" "(frobnicate 42)")
    (is-false (ds:load-session "nonsense"))
    (is (= 0 (length (ds:pending-arrivals)))))

  (with-sessions
    (write-session "broken" "(workspace 1 (split :horizontal")
    (is-false (ds:load-session "broken"))
    (is (= 0 (length (ds:pending-arrivals))))))

(test missing-session-declines
  "Asking for a session nobody wrote says so and changes nothing."
  (with-sessions
    (is-false (ds:load-session "never-written"))))
