;;;; runtime/config.lisp --- Two defaults, the user's file, and where
;;;; extensions live.
;;;;
;;;; PLAN.org promotes the configuration file from a detail to a primary
;;;; deliverable: "under the extensibility ruling this is a primary
;;;; deliverable, not a detail.  Every P1 fork must appear here as a tier-0
;;;; value."
;;;;
;;;; THE DEFAULT KEYMAP IS NOT HERE ANY MORE.  It was, for as long as this file
;;;; existed, and it did not belong: the five values a person changes first
;;;; were moved into policy/ on exactly that argument while ninety-seven lines
;;;; of DEFINE-KEY -- the most customised object in any window manager -- stayed
;;;; behind beside the code that finds $XDG_CONFIG_HOME.  See
;;;; policy/keymap.lisp.  What is left here is about *files*: where the user's
;;;; configuration is, how it is read, and how an installed extension is found.

(in-package #:latticewm/runtime)

;;; --------------------------------------------------- the program defaults

(p:define-option *cursor-theme* nil
  "The xcursor theme name, or NIL to leave the compositor's default alone.")

(p:define-option *cursor-size* 24
  "The xcursor size in logical pixels.")

(define-argument-type :shell-command "run: "
  :documentation "A program to run, with its arguments."
  :candidates (list *terminal* *editor* *browser* *file-manager*)
  ;; No parser: a command line is text, and the only thing that could be
  ;; checked here — does this program exist — is better answered by trying it
  ;; and saying what happened.
  )

(define-argument-type :option "option: "
  :documentation "One of the configuration values -- see SET-OPTION."
  :candidates (mapcar (lambda (row) (string-downcase (string (first row))))
                      (p:all-options))
  :parse (lambda (text)
           (let ((keyword (find-symbol (string-upcase (string-trim " " text))
                                       :keyword)))
             (unless (and keyword (p:option-boundp keyword))
               (error "there is no option called ~a" text))
             keyword)))

(defcommand set-option (name value)
  "Change a configuration value, now, without restarting anything.

    M-x set-option  gaps  8

VALUE is read as a Lisp object, so a number is a number, a string wants its
quotes, and a colour is a list: (0.9 0.5 0.2 1.0).

This is the tier-0 half of DESIGN's live-reconfiguration argument made
available without a REPL.  Nothing here is written to your init.lisp — put it
there when you are sure, and until then the cost of being wrong is one more
prompt."
  (:interactive :option :sexp)
  (handler-case
      (progn (setf (p:option name) value)
             (notify "~(~a~) = ~s" name (p:option name))
             (relayout :force t))
    (error (condition) (notify "~a" condition))))

;;; ------------------------------------------------------- the user's file

(defun config-directory ()
  "Where the user's configuration lives: $XDG_CONFIG_HOME/latticewm/."
  (merge-pathnames
   "latticewm/"
   (or (uiop:getenv-absolute-directory "XDG_CONFIG_HOME")
       (merge-pathnames ".config/" (user-homedir-pathname)))))

(defun config-file ()
  "The user's init.lisp."
  (merge-pathnames "init.lisp" (config-directory)))

(defun config-file-name ()
  "The user's init.lisp, written the way a *document* should write it.

/home/shaul/.config/latticewm/init.lisp WAS COMMITTED TO THIS REPOSITORY, on
line 1 of doc/OPTIONS.txt, in the file the corpus calls the un-driftable
reference.  Nothing was wrong with the code: CONFIG-FILE expands the path
because a program that is about to open a file needs an absolute one.  The
mistake was printing that answer into a document, which turns a per-machine
measurement into a committed artifact and puts the author's home directory in
front of every reader.

So there are two functions.  CONFIG-FILE is for opening; this is for saying,
and it says ~/ and $XDG_CONFIG_HOME rather than resolving them, because those
are what the reader has to type.  `make surface && git diff --exit-code doc/'
in CI is what keeps the distinction honest: a generated document that is not
reproducible fails the build on the next machine that regenerates it.

A constant and not a branch on $XDG_CONFIG_HOME, for the same reason: a
document that says one thing on a machine where that variable is set and
another where it is not is reproducible on neither."
  "~/.config/latticewm/init.lisp")

;;; ------------------------------------------------- where extensions live
;;;
;;; A SHIPPED CONFIGURATION THAT ONLY LOADS ON THE AUTHOR'S MACHINE IS NOT A
;;; SHIPPED CONFIGURATION.  The starter init.lisp offers to load the lattice;
;;; the lattice is an ASDF system; ASDF finds a system by finding its .asd on a
;;; path.  During development that path is the build tree, which is why nothing
;;; noticed — and on any machine that installed from a package the build tree
;;; is not there, so the default configuration failed at load and the failure
;;; routed straight into the startup path.
;;;
;;; Two halves fix it and both are needed.  install.sh copies lattice.asd and
;;; lattice/ into $PREFIX/share/latticewm/, and this registers that directory
;;; with ASDF before the configuration file is read.  A build gate now asserts
;;; that the image, the installer and the sample configuration agree.

(defun executable-directory ()
  "The directory the running binary is in, or NIL.

Used to find $PREFIX/share/latticewm/ relative to $PREFIX/bin/latticewm, which
is what makes a relocatable install — a home-directory prefix, a store path, a
tarball unpacked anywhere — find its own data without being told where it is."
  (ignore-errors
   (let ((argv0 (first sb-ext:*posix-argv*)))
     (when argv0
       (let ((path (or (probe-file argv0)
                       (probe-file (merge-pathnames argv0 (uiop:getcwd))))))
         (when path (make-pathname :name nil :type nil :defaults path)))))))

(defun data-directories ()
  "Every directory that may hold installed LatticeWM data, best first.

The order is the order of decreasing specificity, and each entry is here
because it is where somebody actually installs things:

  $LATTICEWM_DATA          said explicitly, wins over everything
  ~/.config/latticewm/     a user's own systems, alongside their init.lisp
  ../share/latticewm/      relative to the binary: a relocatable install
  $PREFIX/share/latticewm/ the two conventional system prefixes
  $LATTICEWM_ROOT          the build tree, for `make run' and for a REPL

Two subdirectories ride along with entries that have them, because an
extension's .asd sits beside its sources rather than at a registry root:

  <dir>/extensions/        under the build tree, and under ~/.config/latticewm/

This file knows about the directory named \"extensions\" and nothing else about
what lives in it -- the same altitude as EXAMPLES in tools/build.lisp.  Which
extensions exist, and which of them a session enables, is the configuration
file's business and nobody else's."
  (remove nil
          (append (list (uiop:getenv-absolute-directory "LATTICEWM_DATA")
                        (config-directory)
                        (let ((bin (executable-directory)))
                          (when bin (merge-pathnames "../share/latticewm/" bin)))
                        #p"/usr/local/share/latticewm/"
                        #p"/usr/share/latticewm/")
                  (let ((root (uiop:getenv-absolute-directory "LATTICEWM_ROOT")))
                    (when root
                      (list root (merge-pathnames "extensions/" root))))
                  ;; And the extensions/ subdirectory of the two places a user
                  ;; actually puts their own things.
                  (let ((home (config-directory)))
                    (when home
                      (list (merge-pathnames "extensions/" home)))))))

(defun register-data-registry ()
  "Put every existing data directory on ASDF's central registry.

Called from START before the configuration file is read, so that
(asdf:load-system \"lattice\") in a configuration file works on a machine where
the build tree was never present.  Existing entries are not disturbed and
nothing is added twice, so this is safe to call again from a REPL after
installing something new."
  (let ((added '()))
    (dolist (directory (data-directories) (nreverse added))
      (let ((probe (ignore-errors (probe-file directory))))
        (when (and probe (not (member probe asdf:*central-registry*
                                      :test #'equal)))
          (push probe asdf:*central-registry*)
          (push probe added)
          (logmsg :debug "extensions may be loaded from ~a" probe))))))

(defun extension-loaded-p (name)
  "True when the ASDF system NAME is already in this image.

Checked before loading, because the shipped image *contains* the lattice — it
is the flagship worked example and it costs a megabyte — so asking ASDF to load
it again would send it looking for an .asd it does not need."
  (or (and (find-package (string-upcase name)) t)
      (and (asdf:registered-system name) t)))

(defcommand load-extension (name)
  "Load an extension system by name, from wherever it is installed.

    (load-extension \"lattice\")

Prefer this to a bare (asdf:load-system ...) in a configuration file: it knows
where an *installed* LatticeWM keeps its systems, it does nothing when the
system is already in the image, and it reports a failure as a message rather
than as an error that stops the rest of your configuration from loading."
  (:interactive :string)
  (cond
    ((extension-loaded-p name)
     (logmsg :info "~a is already loaded" name)
     t)
    (t
     (register-data-registry)
     (handler-case
         (progn (asdf:load-system name)
                (logmsg :info "loaded extension ~a" name)
                t)
       (error (condition)
         (notify "could not load ~a: ~a" name condition)
         (logmsg :error "could not load extension ~a: ~a~%~
                         looked in: ~{~a~^, ~}"
                 name condition (data-directories))
         nil)))))

(defun load-config (&optional (path (config-file)))
  "Load the user's configuration, if there is one.

It is read in the LATTICEWM/USER package, which inherits the model, the policy
surface and the runtime wholesale — so a configuration file writes

    (defmethod should-float-p ((p policy) (win window))
      (or (call-next-method) (equal (window-app-id win) \"pavucontrol\")))

with no package prefixes anywhere.

An error in the file is logged and does not stop startup.  A window manager
that refuses to run because of a typo in its configuration leaves the user
with no way to fix the typo, which is the worst possible failure for this
particular program."
  (if (probe-file path)
      (handler-case
          (let ((*package* (find-package '#:latticewm/user)))
            (load path)
            (logmsg :info "loaded ~a" path)
            t)
        (error (condition)
          (logmsg :error "error in ~a: ~a~%(continuing with defaults)"
                  path condition)
          nil))
      (progn (logmsg :info "no configuration at ~a; using defaults" path)
             nil)))

(defun write-sample-config (&optional (path (config-file)))
  "Write a commented starter configuration, if none exists yet."
  (when (probe-file path)
    (return-from write-sample-config nil))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :error)
    (write-string (sample-config) out))
  (logmsg :info "wrote a starter configuration to ~a" path)
  path)

(defun sample-config ()
  "The text of the starter configuration file."
  (format nil "~
;;;; ~~/.config/latticewm/init.lisp
;;;;
;;;; Plain Common Lisp, read in the LATTICEWM/USER package, which has the whole
;;;; model, the policy surface and the runtime visible without prefixes.
;;;;
;;;; Everything here takes effect at startup.  Everything here can ALSO be
;;;; evaluated into the running window manager and take effect immediately,
;;;; with no restart and without losing your layout.  Three ways in:
;;;;
;;;;   latticewm --eval '(setf *gaps* 12)'   the control socket, on by default
;;;;   Super+;                               a minibuffer that reads a form
;;;;   M-x slime-connect                     a full REPL, one form away:
;;;;
;;;;     Super+;  (start-swank 4005)      then M-x slime-connect to 4005
;;;;
;;;; THAT IS THE WHOLE OF IT AND IT NEEDS NO RESTART.  Type it into the
;;;; minibuffer when you want a REPL; there is nothing listening until you do.
;;;; StumpWM's manual asks for two forms and a Quicklisp load to reach the same
;;;; place, and this is the same idea with the loading already done.
;;;;
;;;; Put (setf *swank-port* 4005) in this file if you would rather have it at
;;;; every startup.  It is off by default because a REPL on a TCP port is
;;;; arbitrary code execution with no authentication step, in a program that
;;;; starts as your session — so it should be a thing you asked for rather than
;;;; a thing you have.  It binds to loopback (*swank-interface*).
;;;;
;;;; If you write Lisp, do this on your first day: redefining a method in a
;;;; running window manager and watching the windows move is what this project
;;;; is for, and it is the one thing no other Wayland compositor can offer.
;;;;
;;;; You do not have to start here.  Super+? o reads a setting's documentation
;;;; on screen and Super+? s changes it on the spot; this file is for making
;;;; the ones you settled on stick.
;;;;
;;;; \"latticewm --list-options\" prints every value with its default and its
;;;; documentation.  \"latticewm --extension-surface\" prints every generic you
;;;; can specialize.

(in-package #:latticewm/user)

;;; ------------------------------------------------------------- tier 0
;;; Values.  Change these first; they cover most of what people want.

;; (setf *gaps* 6)                     ; space between panes
;; (setf *border-width* 2)
;; (setf *focused-border-color* '(0.9 0.5 0.2 1.0))
;; (setf *terminal* \"alacritty\")
;; (setf *spawn-mode* :split)          ; :split | :fill-first | :stack
;; (setf *split-axis* :longer)         ; :longer | :horizontal | :vertical
;; (setf *focus-after-close* :stay)    ; :stay | :mru | :next
;; (setf *focus-follows-mouse* t)

;;; --------------------------------------------------- when a window opens
;;; The three settings above decide it in general.  This decides it per
;;; application, which is what people actually want: a volume mixer should
;;; float, a browser should always land on the same workspace, and every dialog
;;; should float without naming them one at a time.
;;;
;;; The first rule that matches wins, so put the specific ones first.  Match on
;;; :APP-ID, :TITLE, either of those as :APP-ID-CONTAINS / :TITLE-CONTAINS, or
;;; :PARENT for what river reports as a child window.  To find out what to
;;; write, ask the running program at Super+; —
;;;
;;;     (mapcar #'window-app-id (all-windows))
;;;
;;; which is the same list a rule is matched against.
;;;
;;; examples/02-window-rules.lisp is the worked version, including the tier-1
;;; method to write when a table is not enough.

;; (setf *window-rules*
;;       '(((:app-id \"pavucontrol\")        :float t)
;;         ((:app-id \"firefox\")            :workspace 2)
;;         ((:title-contains \"Picture-in\") :float t)
;;         ((:parent t)                     :float t)))

;;; ------------------------------------------------------- your hardware
;;; The keyboard, the mouse and the touchpad.  On river there is nothing else
;;; on the machine that configures these — no settings daemon, no xinput — so
;;; if you want tap-to-click or a layout that is not American, it happens here.
;;;
;;; \"(list-inputs)\" at Super+x prints every device with its name and
;;; everything settable on it, which is where the names below come from.

;; (setf *tap-to-click* t)             ; on by default; here so you can see it
;; (setf *natural-scroll* t)           ; scroll like a phone
;; (setf *repeat-rate* 50)             ; key repeats per second
;; (setf *repeat-delay* 300)           ; ms held before repeating starts
;; (setf *accel-speed* 0.3d0)          ; pointer speed, -1 slowest to 1 fastest

;; The keyboard layout itself.  This changes what the keys do for every
;; application, and adopts the matching shift map so the window manager's own
;; prompt agrees with your keyboard.  Needs xkbcli, which ships with
;; libxkbcommon and is therefore already installed.
;; (setf *xkb-layout* \"de\")
;; (setf *xkb-options* \"ctrl:nocaps\")  ; caps lock becomes control

;; A laptop has a touchpad AND a mouse, and they want opposite things.  Later
;; rules win, so a rule for T is a default and a rule naming a device is an
;; override.  The matcher is T, a device kind, a substring of the name, or a
;; function of one device.
;; (setf *input-rules*
;;       '((\"Touchpad\"  :natural-scroll t :accel-speed 0.3d0
;;                      :click-method :clickfinger)
;;         (\"TrackPoint\" :scroll-method :on-button-down :scroll-button 274)
;;         (\"Logitech\"   :accel-profile :flat)))

;;; ------------------------------------------------------------- keys
;;; A binding names a command, so redefining the command later takes effect
;;; without rebinding the key.

;; (define-key *keymap* \"Super+Return\" '(\"terminal\"))
;; (define-key *keymap* \"Super+Shift+q\" '(\"close\"))

;; A chord.  River's ensure_next_key_eaten makes submaps exit cleanly on a key
;; they do not bind, so this behaves the way it does in Emacs.
;; (let ((window-map (make-keymap :name \"window\")))
;;   (define-key window-map \"h\" '(\"focus\" :left))
;;   (define-key window-map \"l\" '(\"focus\" :right))
;;   (define-key *keymap* \"Super+w\" window-map))

;;; ------------------------------------------------------------- tier 1
;;; Behaviour.  One DEFMETHOD changes one decision.  See doc/EXTENDING.org for
;;; the full list; every one of them has a docstring you can read at a REPL
;;; with (describe 'should-float-p).

;; Float a couple of applications that get tiling wrong.
;; (defmethod should-float-p ((policy conventional-policy) (win window))
;;   (or (call-next-method)
;;       (member (window-app-id win) '(\"pavucontrol\" \"blueman-manager\")
;;               :test #'equal)))

;; Bigger gaps around lattice cells than around splits inside them.
;; (defmethod gaps ((policy conventional-policy) (container split)) 4)

;;; ------------------------------------------------------------- commands
;;; A command is a named, documented function.  Every one of them is reachable
;;; from Super+x by name, and the ones that take arguments are asked for them —
;;; the *name* of a parameter decides what it is asked for.

;; (defcommand work ()
;;   \"Open the two things I always open.\"
;;   (spawn *editor*)
;;   (spawn *terminal*))
;; (define-key *keymap* \"Super+F1\" '(\"work\"))

;;; ------------------------------------------------------------- hooks
;;; Notice that something happened.  Use a method when you want to *change* a
;;; decision; use a hook when you only want to react.
;;;
;;; Give the function a name and add the SYMBOL.  A #'function or a lambda is a
;;; snapshot: redefining it later leaves the hook calling the old one, and
;;; re-evaluating the ADD-HOOK adds a second copy instead of replacing it.

;; (defun note-window (win) (logmsg :info \"opened ~~a\" (window-app-id win)))
;; (add-hook :window-opened 'note-window)

;;; ------------------------------------------------------ the lattice
;;; The infinite 2D plane of cells, with zoom and pan.  It is a separate system
;;; and enabling it is opt-in.  It ships inside the binary, so LOAD-EXTENSION
;;; usually has nothing to do; it is still the right thing to write, because it
;;; also finds an extension installed beside your init.lisp.

;; (load-extension \"lattice\")
;; (lattice:enable)

;;; Every workspace is then a plane — including the ones you make later — so
;;; the workspace keys walk a stack of infinite planes, one behind another.
;;; Where a new plane starts, and where you land on it:
;;
;; (setf lattice:*new-workspace-zoom* :inherit)    ; the zoom you were at
;; (setf lattice:*new-workspace-origin* :inherit)  ; directly behind this one
;; (setf lattice:*workspace-entry* :aligned)       ; keep X and Y, change plane

;;; ------------------------------------------------------- your own systems
;;; Anything you drop in ~~/.config/latticewm/ as an ASDF system is findable by
;;; name.  So a configuration that has outgrown one file becomes:
;;;
;;;     ~~/.config/latticewm/my-desktop.asd
;;;     ~~/.config/latticewm/my-desktop/...
;;;
;; (load-extension \"my-desktop\")
"))
