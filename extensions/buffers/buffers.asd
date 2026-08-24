;;;; buffers.asd --- Windows you call by name; panes are views.

(defsystem "buffers"
  :description "Named windows and a global registry: any pane can show any window."
  :long-description
  "Every window can carry a name.  A pane asks for a buffer by name and shows
it, wherever it lives; asking for a buffer that is already on screen jumps to
the pane showing it, because a live Wayland window has exactly one rectangle
and cannot be drawn twice.

    (load-extension \"buffers\")

Commands: NAME-BUFFER names the focused window (or any window), SWITCH-TO-BUFFER
shows a named buffer in the current pane -- with completion over every name --
and BUFFERS lists what is named."
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
   (:file "buffers")))

(defsystem "buffers/tests"
  :description "Tests for the buffers registry and switching."
  :depends-on ("buffers" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-buffers"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
