;;;; latticewm.asd --- The LatticeWM core system.
;;;;
;;;; Layer 0.  Everything here is meant to be extended from outside, never
;;;; edited.  See doc/EXTENDING.org.

(defsystem "latticewm"
  :description "An extensible window manager for the river Wayland compositor."
  :author "Shaul Khayyat"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :defsystem-depends-on ("wayflan-client")
  :depends-on ("wayflan-client" "alexandria" "closer-mop" "bordeaux-threads")
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
     (:module "wire"
      :serial t
      :components
      ((:file "sequence")
       (:file "wrappers")))))))
