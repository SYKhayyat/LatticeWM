;;;; kbd-overview.asd --- Zoom out to a keyboard of windows; type to navigate.

(defsystem "kbd-overview"
  :description "All windows arranged as a keyboard; press letters to go or pull."
  :long-description
  "Zoom out: every window in the session rearranges into QWERTY rows under
big letter badges.  Type.

    (load-extension \"kbd-overview\")
    (kbd-overview:enable)
    (define-key *keymap* \"Super+k\" '(\"toggle-kbd-overview\"))

A plain letter GOes -- focus jumps to that window, wherever it lives.
SHIFT+letter PULLs -- the window is marked, and you keep typing.  RET
gathers every marked window into the workspace you started on and snaps
back.  ESC cancels and snaps back, moving nothing.

Letters are assigned home-row-first over windows in stable workspace order,
so muscle memory accumulates."
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
   (:file "kbd-overview")))

(defsystem "kbd-overview/tests"
  :description "Tests for the kbd-overview extension."
  :depends-on ("kbd-overview" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-kbd-overview"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
