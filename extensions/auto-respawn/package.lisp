;;;; auto-respawn/package.lisp

(defpackage #:auto-respawn
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Pick up where you left off: remembers what command launched each window,
and brings the session back after a reboot by running them again into the
arrangement they left.

Load it, enable it, and login is something you get back rather than rebuild:

    (load-extension \"auto-respawn\")
    (auto-respawn:enable)

(load-extension \"auto-respawn\") adds it to the running session; enable
starts recording. Run the extension's respawn-session command after a login,
or set *RESPAWN-ON-STARTUP*, and the editor, browser and terminals are open
in the cells you left them in without a manifest having been written.")
  (:export #:*window-command-guesses*
           #:*respawn-on-startup*
           #:enable #:disable #:enabled-p
           #:launcher-commands
           #:respawn-session))
