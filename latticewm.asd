;;;; latticewm.asd --- The LatticeWM core system.
;;;;
;;;; Layer 0.  Everything here is meant to be extended from outside, never
;;;; edited.  See doc/EXTENDING.org.
;;;;
;;;; The lattice — coordinates, viewport, zoom, pan — is deliberately NOT part
;;;; of this system.  It lives in lattice.asd, depends on LATTICEWM/POLICY and
;;;; nothing else, and must be expressible with zero edits under src/.  That is
;;;; DESIGN D21's experiment, and keeping it a separate system is what makes
;;;; the experiment run on every build instead of once in week three.

(defsystem "latticewm"
  :description "An extensible window manager for the river Wayland compositor."
  :author "Shaul Khayyat"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :defsystem-depends-on ("wayflan-client")
  :depends-on ("wayflan-client" "alexandria" "closer-mop" "bordeaux-threads"
               (:require "sb-introspect") (:require "sb-posix")
               ;; The control socket.  In SBCL itself, so this costs a REQUIRE
               ;; and no dependency anybody has to keep alive.
               (:require "sb-bsd-sockets"))
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:module "protocol"
      :serial t
      :components
      ((:wayflan-client-impl "river-window-management-v1"
        :in-package "LATTICEWM/RIVER" :export t)
       (:wayflan-client-impl "river-xkb-bindings-v1"
        :in-package "LATTICEWM/RIVER" :export t)
       (:wayflan-client-impl "river-layer-shell-v1"
        :in-package "LATTICEWM/RIVER" :export t)
       ;; Input configuration.  Three protocols, and the order is a dependency:
       ;; both of the others carry an `input_device' event whose argument is a
       ;; river_input_device_v1, so input-management has to be generated first.
       ;;
       ;; River hands the window manager the machine's input devices and
       ;; expects it to configure them.  There is nothing else on the system
       ;; that will -- which is why a window manager that vendored these three
       ;; XML files and bound none of them had no way to turn on tap-to-click,
       ;; set a key repeat rate, or use a keyboard that is not American.
       (:wayflan-client-impl "river-input-management-v1"
        :in-package "LATTICEWM/RIVER" :export t)
       (:wayflan-client-impl "river-libinput-config-v1"
        :in-package "LATTICEWM/RIVER" :export t)
       (:wayflan-client-impl "river-xkb-config-v1"
        :in-package "LATTICEWM/RIVER" :export t)))
     (:module "model"
      :serial t
      :components
      ((:file "geometry")
       (:file "node")
       ;; surface after node: it describes the container protocol NODE
       ;; declares, and policy/surface.lisp shares its description helpers.
       (:file "surface")
       (:file "path")
       (:file "surgery")
       (:file "window")
       (:file "input")
       (:file "world")))
     (:module "policy"
      :serial t
      :components
      (;; options first: logging is configurable, and DEFINE-OPTION is what
       ;; makes a value discoverable rather than merely settable.
       (:file "options")
       ;; log second: GUARDED is the boundary every policy method is called
       ;; behind, so it has to exist before the first one is written.
       (:file "log")
       (:file "protocol")
       ;; hooks needs DEFINE-OPTION from protocol and GUARDED from log.
       (:file "hooks")
       ;; conventional first: it defines the class and every tier-0 value the
       ;; methods below read.
       (:file "conventional")
       (:file "layout")
       ;; motion is the *algorithm*; defaults-motion is the set of answers the
       ;; shipped containers give it.
       (:file "motion")
       (:file "defaults-motion")
       (:file "structure")
       (:file "defaults-lifecycle")
       (:file "lifecycle")
       (:file "input")
       ;; devices after input: the *hardware* half of input policy, kept in its
       ;; own file because it changes for a different reason -- a touchpad
       ;; setting is not a completion style.
       (:file "devices")
       (:file "surface")
       (:file "commands")
       ;; keys before appearance: the status-line and which-key
       ;; composition reads *PENDING-KEYMAP* and the keymap tree.
       (:file "keys")
       (:file "appearance")))
     (:module "wire"
      :serial t
      :components
      ((:file "sequence")
       (:file "wrappers")))
     (:module "runtime"
      :serial t
      :components
      ((:file "server")
       (:file "keys")
       (:file "font")
       (:file "psf")
       (:file "surface")
       (:file "minibuffer")
       (:file "echo")
       (:file "help")
       (:file "cursor")
       ;; layer before pointer: APPLY-KEYBOARD-FOCUS asks whether a layer
       ;; surface holds the keyboard, and a locker holding it must win over
       ;; anything the pointer just did.
       (:file "layer")
       (:file "pointer")
       ;; config comes before verbs because it declares the program defaults
       ;; the launcher commands use, and after keys because it installs the
       ;; default keymap.
       (:file "config")
       ;; welcome comes after config because it derives its key names from
       ;; *MODIFIER*, and after help because it is a help page.
       (:file "welcome")
       (:file "emit")
       (:file "windows")
       ;; The session, split along its reasons to change.  Order is by
       ;; dependency and each step is deliberate: the loop before the things
       ;; that drive it, the connection last because it binds the globals every
       ;; one of them then uses.
       (:file "sequence")
       (:file "swank")
       (:file "outputs")
       (:file "seats")
       ;; input before session: BIND-ONE-GLOBAL hooks each of the three input
       ;; globals where it binds it, because the registry listener stays
       ;; attached for the life of the connection and a global can arrive at
       ;; any time.
       (:file "input")
       (:file "session")
       ;; ipc after session: the control socket runs forms in the window
       ;; manager's thread, and the queue that makes that possible is there.
       (:file "ipc")
       ;; history before verbs: undo installs itself as a command wrapper, and
       ;; a verb defined before the wrapper exists is still covered — the
       ;; wrapper is consulted at call time — but reading them in this order is
       ;; how the relationship is meant to be understood.
       (:file "history")
       (:file "verbs")
       (:file "tags")
       (:file "state")
       (:file "main")))))))

(defsystem "latticewm/tests"
  :description "Unit tests for the parts of a window manager that are testable
without a compositor — which is the whole model."
  :depends-on ("latticewm" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")
     (:file "test-geometry")
     (:file "test-tree")
     (:file "test-motion")
     (:file "test-lifecycle")
     (:file "test-surface")
     ;; container after surface: the two are siblings, and reading them in this
     ;; order is how the second is meant to be understood.
     (:file "test-container")
     (:file "test-minibuffer")
     (:file "test-devices")
     (:file "test-examples"))))
  :perform (test-op (o c) (symbol-call :latticewm/tests :run-all)))
