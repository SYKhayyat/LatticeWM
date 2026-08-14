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
;;; IT HAS BEEN ASKED NOW, AND THE ANSWER IS THE NUMBER BELOW.  The whole build
;;; -- twenty-two gates, the unit suite, and `make surface' -- has been run on
;;; SBCL 2.2.6, on 2.2.9 and on 2.6.6, and is green on all three.  The docstring
;;; on *LATTICEWM-MINIMUM-SBCL* says "the oldest SBCL this project is known to
;;; work on", and that sentence is now a report rather than an inference.
;;;
;;; It was false when it was written, and the way it was false is worth keeping.
;;; Nothing about the *program* failed below 2.6: what failed was a macro in
;;; tests/test-boundaries.lisp whose body referred to a variable bound only
;;; after the body ran.  SBCL 2.6 deletes that reference because its value is
;;; discarded and 2.2.9 does not, so four tests in the suite covering GUARDED,
;;; BEST-EFFORT and WITH-ABANDON died on an UNBOUND-VARIABLE without reaching an
;;; assertion.  The `plain' CI job was built to ask exactly this question, on
;;; exactly that SBCL, and it answered "no" on thirty consecutive runs that
;;; nobody read.  The instrument was right, was pointed at the right thing, and
;;; was reporting a defect in the instrument -- which is the failure mode after
;;; the one the gates were designed against.
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
;;; AND THE NUMBER IS NECESSARY RATHER THAN SUFFICIENT, WHICH THIS COMMENT
;;; ASSERTED THE OPPOSITE OF BY OMISSION.  Core compression is a build-time
;;; option of SBCL -- :SB-CORE-COMPRESSION in *FEATURES* -- and the version
;;; governs which algorithm it is, not whether it was compiled in at all.  The
;;; official x86-64 binary tarballs for 2.2.6 and 2.2.9 have no compression
;;; support, which is unlucky in the exact place it matters: they are what
;;; somebody installs by hand when they need an old SBCL to check this floor
;;; with.  Distribution packages generally do have it.
;;;
;;; Nothing above changes as a result -- the program *builds*, and passes
;;; twenty-two gates and the whole unit suite, on an SBCL with no compression
;;; at all; it is only `make image' that cannot run.  What changed is that
;;; tools/image.lisp now asks before it dumps and says all of that in a
;;; sentence, instead of failing inside SAVE-LISP-AND-DIE with an unhandled
;;; SIMPLE-ERROR under eighteen frames of backtrace that name neither this
;;; project nor the variable that turns compression off.
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
  :long-description
  "A window manager whose behaviour is a set of documented generic functions
you specialize from a configuration file, with the whole of Common Lisp
available at runtime: redefine a method over SLIME and the windows move,
without a restart and without losing your layout.  The window tree is a
container protocol rather than a fixed set of kinds, so an extension can add a
kind of container the core has never heard of -- which is what the `lattice'
system, an infinite zoomable plane of cells, is a proof of."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  ;; THE METADATA ABOVE IS THE FRONT DOOR FOR EVERY LISP THAT IS NOT A CLONE OF
  ;; THIS REPOSITORY.  Quicklisp, ocicl and qlot all read a system definition
  ;; and none of them read a README; without :HOMEPAGE and :SOURCE-CONTROL a
  ;; system that arrives through a dist is a name, a description and no way
  ;; back to the project.  `git clone && ./bootstrap.sh && make' is a C
  ;; project's install story wearing Lisp, and it was the only one on offer.
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
       ;; The keymap is policy, and it lived in runtime/config.lisp beside the
       ;; code that finds $XDG_CONFIG_HOME.  After keys, which is what it uses.
       (:file "keymap")
       ;; APPEARANCE.LISP WAS THREE LIBRARIES, because an early gate 6 measured
       ;; a line-count ratio and the file was shaped to keep it honest.  That
       ;; gate was replaced three commits later; the shape outlived it.
       (:file "font")
       (:file "text")
       (:file "appearance")))
     (:module "wire"
      :serial t
      :components
      ((:file "sequence")
       (:file "wrappers")))
     (:module "runtime"
      ;; :DEPENDS-ON AND NOT :SERIAL T, AND THE DIFFERENCE IS TWENTY-NINE FILES
      ;; IN A HARDCODED TOTAL ORDER OVER A DEMONSTRABLE PARTIAL ONE.  A serial
      ;; module says every file depends on every file before it, which made
      ;; editing one of them recompile most of what follows -- and two of the
      ;; ordering comments that used to live here disclaimed being dependencies
      ;; in as many words: "the wrapper is consulted at call time -- but reading
      ;; them in this order is how the relationship is meant to be understood".
      ;; That is a narrative encoded as a build constraint.
      ;;
      ;; The graph below is every edge for which one file *references a name*
      ;; another defines, which is a superset of what ASDF needs and is what
      ;; makes recompilation propagate correctly.  117 edges where a total
      ;; order has 406.  The load order is unchanged, so a clean build is
      ;; byte-identical; what changes is which files an edit reaches.
      ;;
      ;; Where an edge exists for a reason a reference cannot show, it is kept
      ;; and the reason is the comment beside it.
      :components
      ((:file "server")
       (:file "keys" :depends-on ("server"))
       (:file "font-table")
       (:file "font" :depends-on ("font-table"))
       (:file "psf")
       (:file "surface" :depends-on ("server" "font"))
       (:file "minibuffer" :depends-on ("server"))
       (:file "echo" :depends-on ("server" "font" "surface" "minibuffer"))
       (:file "help" :depends-on ("server" "font" "surface" "minibuffer" "echo"))
       (:file "cursor" :depends-on ("server" "font" "surface"))
       (:file "layer" :depends-on ("server" "surface"))
       (:file "pointer" :depends-on ("server" "minibuffer" "layer"))
       (:file "config" :depends-on ("minibuffer" "echo"))
       (:file "welcome" :depends-on ("surface" "minibuffer" "help" "config"))
       (:file "emit" :depends-on ("server" "surface" "echo"))
       (:file "capture" :depends-on ("echo"))
       (:file "windows" :depends-on ("server" "minibuffer" "echo" "layer" "pointer" "emit" "capture"))
       (:file "sequence" :depends-on ("server" "layer" "pointer" "emit" "windows"))
       (:file "swank" :depends-on ("server" "sequence"))
       (:file "outputs" :depends-on ("server" "surface" "minibuffer" "layer" "capture" "sequence"))
       (:file "seats" :depends-on ("server" "keys" "minibuffer" "layer" "pointer" "sequence"))
       (:file "input" :depends-on ("server" "minibuffer" "echo" "outputs"))
       (:file "session" :depends-on ("server" "minibuffer" "windows" "sequence" "outputs" "seats" "input"))
       (:file "ipc" :depends-on ("server" "minibuffer" "sequence" "swank" "session"))
       (:file "history" :depends-on ("server" "minibuffer" "echo" "help" "sequence"))
       (:file "verbs" :depends-on ("server" "minibuffer" "echo" "help" "config" "emit" "capture" "windows" "sequence" "outputs" "seats" "history"))
       (:file "tags" :depends-on ("server" "echo" "help" "windows" "sequence" "outputs" "verbs"))
       (:file "state" :depends-on ("server" "minibuffer" "sequence" "history" "verbs" "tags"))
       (:file "main" :depends-on ("server" "minibuffer" "help" "config" "welcome" "emit" "sequence" "swank" "input" "session" "ipc" "verbs" "state"))))))))

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
     ;; Which rivers this build will talk to.  The version check was an
     ;; equality, inline in BIND-ONE-GLOBAL and unreachable without a live
     ;; registry, so the one function deciding whether the program starts on
     ;; anybody's machine had no test at all.
     (:file "test-versions")
     (:file "test-examples"))))
  :perform (test-op (o c) (symbol-call :latticewm/tests :run-all)))
