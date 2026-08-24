;;;; scrolling-columns/package.lisp

(defpackage #:scrolling-columns
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "A scrolling strip of columns: niri and PaperWM, as a container kind.

The current workspace becomes an unbounded horizontal strip of columns, of
which a window's worth is on screen at a time.  Moving past the right edge
scrolls the strip by one rather than refusing, which is what makes it feel
unbounded instead of merely wide.

    (load-extension \"scrolling-columns\")
    (scrolling-columns:scrolling)")
  (:export #:strip #:strip-offset #:strip-visible
           #:strip-width #:scrolling))
