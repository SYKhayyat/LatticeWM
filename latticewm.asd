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

;;; THE SBCL FLOOR, DECLARED ONCE AND CHECKED IN THREE PLACES.
;;;
;;; This project reaches into SBCL far enough that "needs SBCL" was not the
;;; whole requirement, and nothing said the rest of it.  bootstrap.sh printed
;;; `sbcl --version' and checked nothing; the build reached for a named symbol
;;; in SBCL's *C runtime* and would have failed on an older one with a foreign
;;; linkage error naming neither this project nor a version.  Debian and Ubuntu
;;; LTS ship 2.2.9 and Arch ships current, so the spread is real and the
;;; question was empirical and unasked.
;;;
;;; What the floor rests on, named rather than guessed:
;;;
;;;   2.2.6  core compression became zstd, and its levels became 0..22.
;;;          tools/image.lisp dumps at level 22; on a zlib SBCL the valid
;;;          range was -1..9 and 22 is an error, so the *shipping image* --
;;;          not the build, the thing a user installs -- cannot be made at all
;;;          below this.  This is the binding constraint and it is why the
;;;          number is 2.2.6 rather than something older.
;;;
;;; Everything else this program uses is older than that and is listed so the
;;; floor can be lowered honestly if the compression ever moves: sb-introspect
;;; xref (gate 11), sb-int:broken-pipe (main.lisp), sb-bsd-sockets (ipc.lisp),
;;; sb-posix, and the extern-alien "gc_coalesce_string_literals" byte that
;;; tools/image.lisp sets.
;;;
;;; It is checked here because this is the file a stranger loads --
;;; (ql:quickload :latticewm) reaches this form and nothing else -- and again
;;; in tools/prelude.lisp, which every make target loads, and again in
;;; bootstrap.sh, which reads the number back out of this file with sed rather
;;; than keeping a copy.  Same shape as reading river's version out of
;;; src/protocol/PINNED.
(defparameter cl-user::*latticewm-minimum-sbcl* "2.2.6"
  "The oldest SBCL this project is known to work on.  See the comment above.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((numeric (string)
           ;; "2.2.9.debian" and "2.6.0" both become a list of integers, and a
           ;; component that is not a number ends the list rather than being
           ;; guessed at -- git snapshots are versioned like 2.4.1.42-abcdef.
           (loop with start = 0
                 for dot = (position #\. string :start start)
                 for piece = (subseq string start (or dot (length string)))
                 for value = (ignore-errors (parse-integer piece))
                 while value collect value
                 while dot do (setf start (1+ dot)))))
    (let ((have (numeric (lisp-implementation-version)))
          (want (numeric cl-user::*latticewm-minimum-sbcl*)))
      #-sbcl
      (error "LatticeWM is SBCL-only: it uses sb-introspect for gate 11, ~
sb-bsd-sockets for the control socket, and save-lisp-and-die for the image. ~
This is ~a." (lisp-implementation-type))
      #+sbcl
      (when (loop for h = (or (pop have) 0)
                  for w = (or (pop want) 0)
                  when (/= h w) return (< h w)
                  while (or have want))
        (error "LatticeWM needs SBCL ~a or newer; this is ~a.~2%~
The binding constraint is core compression: tools/image.lisp dumps the ~
shipping image at zstd level 22, and SBCL used zlib with a maximum level of ~
9 before ~a.  The build and the tests may well work here -- try ~
`make build gates test' -- but `make image' cannot, so there is no way to ~
install what you have built.~2%~
Debian and Ubuntu LTS ship 2.2.9; Arch, Fedora and nixpkgs ship current."
               cl-user::*latticewm-minimum-sbcl*
               (lisp-implementation-version)
               cl-user::*latticewm-minimum-sbcl*)))))

(defsystem "latticewm"
  :description "An extensible window manager for the river Wayland compositor."
  :author "Shaul Khayyat"
  :license "GPL-3.0-or-later"
  ;; ONE VERSION, IN A FILE, READ BY EVERYTHING THAT NEEDS ONE.
  ;;
  ;; It used to be written out in four places -- here, lattice.asd, flake.nix
  ;; and the .TH line of each man page -- and the sole git tag was `v0.1',
  ;; which matched none of them.  main.lisp's --version was the only reader
  ;; that got it right, because it asked ASDF instead of holding a copy.
  ;;
  ;; The pattern is not new here.  flake.nix and shell.nix both read *river's*
  ;; version out of src/protocol/PINNED with builtins.match rather than
  ;; duplicating it, and say in a comment that a warning which can disagree
  ;; with the check the program performs is worse than no warning.  It was
  ;; invented, used twice, and never turned inward on the file's own version
  ;; string two hundred lines above.  Gate 19 now holds all of them to VERSION.
  :version (:read-file-line "VERSION")
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
       ;; lifecycle is every shipped answer LIFECYCLE-POLICY gives.  It was
       ;; two files, defaults-lifecycle and lifecycle, and the pair had no
       ;; rule a reader could apply: both were methods on the same protocol
       ;; class, sorted by subject.  DEFAULTS- means the motion split -- an
       ;; algorithm file that defines no methods, beside the answers it asks
       ;; for -- and gate 18 is what makes that mean something.
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
       ;; capture before windows and outputs: both event handlers call into
       ;; it, and it needs NOTIFY from echo above.
       (:file "capture")
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
     ;; capture after lifecycle: it borrows FRESH-WORLD, and minimizing a
     ;; window is half of what it has to walk.
     (:file "test-capture")
     (:file "test-surface")
     ;; container after surface: the two are siblings, and reading them in this
     ;; order is how the second is meant to be understood.
     (:file "test-container")
     ;; hooks after lifecycle: the focus-change checks borrow FRESH-WORLD, and
     ;; the regression they are for is that ON-WINDOW-CLOSE never announced
     ;; where focus went.
     (:file "test-hooks")
     (:file "test-minibuffer")
     (:file "test-devices")
     ;; The three error boundaries.  Nothing had ever asked them a question,
     ;; which is how all three came to be built on HANDLER-CASE -- so the one
     ;; class of error a live-editable window manager exists to let you fix
     ;; was reported as a single line with no frames.
     (:file "test-boundaries")
     ;; The buffers we draw our own pixels into.  There was one per overlay and
     ;; no wl_buffer.release listener, so every redraw wrote into the buffer the
     ;; compositor was reading -- and the canvas docstring said so as though it
     ;; were a design property.
     (:file "test-overlay")
     (:file "test-examples"))))
  :perform (test-op (o c) (symbol-call :latticewm/tests :run-all)))
