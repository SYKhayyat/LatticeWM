;;;; launcher/package.lisp

(defpackage #:launcher
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "One searchable surface for everything you can start.

    M-x launcher       ; type three letters, press Return

The candidates come from desktop entries, `$PATH', arithmetic, the projects
registry and any Lisp action you define.  Selecting one runs it in this
process -- there is no second daemon, no second window and no second
configuration language, because the window manager is already a resident
Lisp with a prompt.

    (launcher:define-action \"notes\"
      \"Open today's notes\"
      (lambda () (find-file \"~/notes/today.md\")))

A source is a function of no arguments returning records; a record is a
plist (:title :subtitle :source :action).  Adding one does not require
touching this file.")
  (:export
   ;; the two commands anyone actually needs
   #:launcher #:launch
   ;; the library
   #:candidates #:entries #:run-entry #:entry-by-title
   #:define-action #:remove-action #:actions
   #:define-source #:remove-source #:sources
   #:desktop-entry-records #:path-entry-records #:project-entry-records
   #:action-entry-records #:calculator-record
   #:parse-desktop-entry #:exec-argv #:desktop-files
   #:fuzzy-score #:rank-records
   #:calculate #:expression-p
   ;; options
   #:*limit* #:*desktop-directories* #:*path-command-limit*
   #:*actions* #:*extra-sources*
   #:*source-timeout-seconds* #:*enable-path-source* #:*enable-desktop-source*
   #:*path-directories* #:path-directories #:source-enabled-p
   #:strip-field-codes #:desktop-value #:desktop-true-p #:a-record
   #:executable-file-p #:clipboard-program #:spawn-argv #:offer-answer
   #:parse-sum #:parse-product #:parse-power #:parse-atom
   #:*spawn-function* #:*terminal-argument*))
