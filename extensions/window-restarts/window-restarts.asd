;;;; window-restarts.asd --- The condition system, applied to broken windows.

(defsystem "window-restarts"
  :description "When an application exits unexpectedly: retry, undo, dismiss."
  :long-description
  "\"My app crashed and now my layout is weird\" becomes a menu pick.

When a window goes away without you having closed it, this module notices,
says so, and offers the three answers an SLDB buffer would -- RETRY (spawn
the application again), UNDO (revert the last layout change), DISMISS (carry
on).  Bind them to keys yourself, or put the whole menu behind one key:

    (load-extension \"window-restarts\")
    (window-restarts:enable)
    (define-key *keymap* \"Super+Escape\" window-restarts:*menu*)

Lisp code that wants the same decision sees a BROKEN-WINDOW condition with
those three restarts around every place one is offered."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:file "package")
   (:file "window-restarts")))

(defsystem "window-restarts/tests"
  :description "Tests for the window-restarts extension."
  :depends-on ("window-restarts" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-window-restarts"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
