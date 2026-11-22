;;;; grid/package.lisp

(defpackage #:grid
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "n windows in a rectangle of cells, in reading order, as a container kind.

The current workspace's windows become the cells of a grid with a chosen number
of columns; the last row is short rather than stretched.  Motion reads the grid
the way it is read: left and right run along a row and cross into the next one,
up and down move by a whole row.

    (load-extension \"grid\")
    (grid:grid)        ; as square as it goes
    (grid:grid 3)      ; three columns

It is a container and not a policy because an arrangement belongs to ONE
workspace: two workspaces may want different ones, and a user should not have to
choose between them at startup.  That is the same argument that made
scrolling-columns a container rather than a layout-children override.")
  (:export #:grid-container #:grid-p #:grid-column-count #:grid #:grid-columns
           #:grid-default-columns #:*grid-columns*))
