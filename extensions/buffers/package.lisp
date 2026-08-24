;;;; buffers/package.lisp

(defpackage #:buffers
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "Windows you call by name; panes are views.

A window carries a name in a global registry.  SWITCH-TO-BUFFER shows a named
window in the current pane -- recalling it from the scratchpad if it was put
away -- and jumps to the pane already showing it otherwise, because a live
Wayland window has one rectangle and cannot be drawn twice.

    (load-extension \"buffers\")

Two options govern the edges: *FOCUS-FOLLOWS-RECALL* (default T) says whether
the cursor follows a window recalled into the current pane, and
*UNDO-INCLUDES-SWAPS* (default NIL) says whether a buffer switch is an undo
step or a change of view the undo machinery does not record.")
  (:export #:*focus-follows-recall* #:*undo-includes-swaps*
           #:registry #:buffer-name #:buffer-names #:window-named
           #:name-window #:name-buffer #:switch-to-buffer #:buffers))
