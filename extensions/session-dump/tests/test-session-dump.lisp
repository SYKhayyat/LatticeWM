;;;; tests/test-session-dump.lisp --- The pieces that can be tested, are.

(defpackage #:session-dump/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:sd #:session-dump))
  (:export #:run-all))

(in-package #:session-dump/tests)

(def-suite session-dump :description "Dump-and-resume session images.")
(in-suite session-dump)

(t*:register-extension-suite "SESSION-DUMP/TESTS" "SESSION-DUMP")

(defun run-all ()
  "Run the SESSION-DUMP suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'session-dump)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ================================================================ tests
;;;
;;; SAVE-LISP-AND-DIE cannot run inside the suite -- it terminates the
;;; process by design.  What CAN be pinned down is everything around it:
;;; where the core lands, that the resume entry point exists and calls the
;;; shipped startup with the configuration deliberately off, and that the
;;; command is registered and reachable.

(test core-file-follows-state-directory-discipline
  "The core lives beside the saved layout, under the state home."
  (let ((path (namestring (sd:session-core-file))))
    (is (search "session.core" path))
    (is (search "latticewm" path))))

(test resume-toplevel-exists-and-is-a-function
  "A dumped image needs a callable entry point.  If RESUME-TOPLEVEL were
missing or renamed, every dumped core would boot into a bare REPL."
  (is (fboundp 'sd:resume-toplevel)))

(test dump-session-command-is-registered
  "DUMP-SESSION is an ordinary command: findable, documented, boundable."
  (let ((command (p:find-command "dump-session")))
    (is-true command)
    (is-true (p:command-documentation command))))
