;;;; latticewm.asd --- The LatticeWM core system.
;;;;
;;;; Layer 0.  Everything here is meant to be extended from outside, never
;;;; edited.  See doc/EXTENDING.org.
;;;;
;;;; The lattice — coordinates, viewport, zoom, pan — is deliberately NOT part
;;;; of this system.  It lives in lattice.asd, depends on LATTICEWM/POLICY and
;;;; nothing else, and must be expressible with zero edits under src/.  That is
;;;; README D21's experiment, and keeping it a separate system is what makes
;;;; the experiment run on every build instead of once in week three.

(defsystem "latticewm"
  :description "An extensible window manager for the river Wayland compositor."
  :author "Shaul Khayyat"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :defsystem-depends-on ("wayflan-client")
  :depends-on ("wayflan-client" "alexandria" "closer-mop" "bordeaux-threads"
               (:require "sb-introspect") (:require "sb-posix"))
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
        :in-package "LATTICEWM/RIVER" :export t)))
     (:module "model"
      :serial t
      :components
      ((:file "geometry")
       (:file "node")
       (:file "path")
       (:file "surgery")
       (:file "window")
       (:file "world")))
     (:module "policy"
      :serial t
      :components
      ((:file "protocol")
       (:file "conventional")
       (:file "motion")
       (:file "lifecycle")
       (:file "surface")))
     (:module "wire"
      :serial t
      :components
      ((:file "sequence")
       (:file "wrappers")))
     (:module "runtime"
      :serial t
      :components
      ((:file "log")
       (:file "hooks")
       (:file "server")
       (:file "commands")
       (:file "keys")
       ;; config comes before verbs because it declares the program defaults
       ;; the launcher commands use, and after keys because it installs the
       ;; default keymap.
       (:file "config")
       (:file "emit")
       (:file "windows")
       (:file "session")
       (:file "verbs")
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
     (:file "test-examples"))))
  :perform (test-op (o c) (symbol-call :latticewm/tests :run-all)))
