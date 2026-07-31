;;;; lattice/package.lisp --- The lattice extension.
;;;;
;;;; This system depends on LATTICEWM/POLICY and the runtime, and on nothing
;;;; else.  It contains no edit to any file under src/, which is README D21's
;;;; experiment: if the lattice can be added as an extension — new methods, new
;;;; state on PROPS, a new container kind, no core surgery — then the
;;;; extensibility claim is true and there is proof of it.
;;;;
;;;; The result is written up in FINDINGS.org.

(defpackage #:lattice
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "An infinite two-dimensional plane of cells, each holding a split tree.

Each workspace becomes a GRID: cells addressed by integer (X, Y), origin at
(0,0), +X right and +Y up.  The viewport is an N-by-M window over the plane,
and *zoom is the choice of N and M* — 1 cell, 2, 4, 6, 8 — which is the novel
part of the design.  Zoom is normally a mode you enter and leave; here it is a
continuous layout control, one knob replacing both workspace switching and
tiling-layout selection.

    (asdf:load-system \"lattice\")
    (lattice:enable)

Everything the conventional layer does still works inside every cell, and
motion runs straight through the cell boundary as though it were not there.")
  (:export
   ;; coordinates
   #:cell #:cell-x #:cell-y #:cell-equal #:cell-string
   ;; the plane
   #:grid #:make-grid #:grid-cells #:grid-viewport #:grid-names
   #:grid-col-widths #:grid-row-heights
   #:col-width #:row-height #:uniform-p
   #:ensure-cell #:occupied-cells #:tidy-grid
   ;; the viewport
   #:viewport #:viewport-origin #:viewport-cols #:viewport-rows
   #:viewport-cells #:viewport-contains-p #:ensure-visible
   #:zoom-index #:set-zoom #:zoom-origin #:cell-rects
   ;; the policy
   #:lattice-policy
   #:*zoom-mode* #:*zoom-ladder* #:*cell-width* #:*cell-height* #:*cell-gap*
   #:*skip-empty-cells* #:*coordinate-overlay* #:*lattice-border-parity*
   #:*coordinate-tint* #:coordinate-hue #:tag-cell
   ;; using it
   #:enable #:disable #:install-lattice-keys #:install-vocabulary
   #:*overlay-scale* #:*overlay-background* #:*overlay-foreground*
   #:*overlay-min-cell* #:cell-label #:draw-coordinate-overlay
   #:*map-threshold* #:*map-background* #:*map-cell-color* #:*map-empty-color*
   #:*map-text-color* #:map-mode-p #:draw-map
   #:current-grid #:current-cell #:grid-path #:cell-path
   #:goto-cell #:tag-cell-parity))
