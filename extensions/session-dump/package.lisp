;;;; session-dump/package.lisp

(defpackage #:session-dump
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "The session as a dumped image.

    M-x dump-session

writes the running image and exits; resuming that core re-runs START, which
reconnects to the fresh river.  Code, options and extension state persist;
Wayland connections cannot.")
  (:export #:session-core-file
           #:resume-toplevel
           #:dump-session-command))
