;;;; policy/lifecycle.lisp --- What happens when a window arrives or leaves.
;;;;
;;;; These are the default methods for the lifecycle half of the extension
;;;; surface.  They are separated from conventional.lisp only because that file
;;;; is about *geometry and motion* and this one is about *events*, and mixing
;;;; them makes both harder to read.  Everything here is equally overridable.

(in-package #:latticewm/policy)

(defun node-rect (world node)
  "The rectangle NODE occupied at the last relayout, or a sensible guess.

The layout is recomputed from scratch each time and cached on the world's
PROPS, so this is a lookup rather than a computation.  The fallback is the
first output, which is right during startup when no layout has happened yet
and is harmless afterwards."
  (or (loop for (placed nil rect nil) in (c:prop world :last-placements)
            when (eq placed node) return rect)
      (let ((output (first (c:world-outputs world))))
        (if output (c:output-rect output) (c:make-rect 0 0 1920 1080)))))

(defun place-node (policy world node path disposition &key direction)
  "Put NODE into the world at PATH according to DISPOSITION.

DISPOSITION is :FILL, :SPLIT or :STACK, as returned by SPAWN-TARGET.  Returns
the path NODE ended up at.  This is the one place that turns a spawn decision
into tree surgery, so a policy that invents a new disposition adds a clause
here — or, better, does its own surgery in ON-WINDOW-OPEN and never calls
this."
  (let* ((root (c:world-root world))
         (target (c:resolve-path root path)))
    (multiple-value-bind (new-root new-path)
        (cond
          ;; Nothing there, or a deliberately empty pane: it becomes the thing.
          ((or (null target)
               (and (typep target 'c:leaf) (c:leaf-empty-p target)))
           (c:tree-replace-at root path node))
          ((eq disposition :stack)
           (let ((stack (c:make-stack (list target node) 1)))
             (values (c:tree-replace-at root path stack)
                     (c:path-append path 1))))
          (t
           (let ((axis (split-axis-for policy target (node-rect world target)))
                 (side (new-child-side policy target direction)))
             (c:tree-split-at root path node :axis axis :side side))))
      (setf (c:world-root world) new-root)
      ;; The cursor may now be pointing at the split we just created rather
      ;; than at a place, because the node it named grew children.  Repairing
      ;; it here rather than at each call site is the same argument as D18's
      ;; single focus-repair rule: one place, or fifteen subtly different ones.
      (repair-cursor policy world)
      new-path)))

(defmethod on-window-open ((policy conventional-policy) world (window c:window))
  "Place a newly appeared window and, by default, focus it.

The float decision comes first and short-circuits everything: a floated window
is never in the tree, so there is no placement to make.  Returning NIL tells
the runtime that no tiled path exists."
  (let ((rule (window-rule-for policy window)))
    ;; A declarative rule wins over the computed guess, so that people who do
    ;; not want to write methods still get the escape hatch.
    (when (getf rule :float)
      (setf (c:window-floating-p window) t))
    (when (and (not (getf rule :float))
               (should-float-p policy window))
      (setf (c:window-floating-p window) t))
    (cond
      ((c:window-floating-p window) nil)
      (t
       (multiple-value-bind (path disposition) (spawn-target policy world window)
         (let* ((path (or (getf rule :path) path))
                (leaf (c:make-leaf window))
                (landed (place-node policy world leaf path disposition)))
           (when (if (member :focus rule)
                     (getf rule :focus)
                     *focus-new-windows*)
             (jump-cursor policy world landed))
           landed))))))

(defmethod on-window-close ((policy conventional-policy) world (window c:window)
                            path)
  "Take the window's pane out with it, and let its sibling grow.

This is README D17's CLOSE.  The other half of D17 — CLEAR, which empties the
pane but leaves it standing, so that you are now *in* an empty pane — is a
separate command rather than a mode, because they are different intentions and
a mode would make you remember which one you were in."
  (let ((root (c:world-root world)))
    (unless (and path (c:resolve-path root path))
      (return-from on-window-close (c:world-cursor world)))
    (multiple-value-bind (removed new-root suggested)
        (c:tree-remove-at root path
                          :simplify (lambda (node) (should-collapse-p policy node))
                          :focus-path (c:world-cursor world))
      (declare (ignore removed))
      (setf (c:world-root world) new-root)
      (let ((landed (focus-after-remove policy world path suggested)))
        (setf (c:world-cursor world) (c:repair-path new-root landed))))))

(defmethod on-minimize ((policy conventional-policy) world (window c:window))
  "Take the window out of the tree entirely and put it on the scratchpad.

The stated requirement, honoured literally: *minimized windows leave the
tiling tree and the remaining windows retile without them*.  Minimize is not
'hide it somewhere', it is 'take it out of the layout'.  River is explicit
that this is entirely ours to define — the window manager is free to ignore
the request, hide the window, or do whatever else it chooses.

The path it came from is remembered on the window, so RESTORE can put it back
where it was if that place still exists."
  (let* ((root (c:world-root world))
         (leaf (c:leaf-holding root window))
         (path (and leaf (c:node-path-to root leaf))))
    (when path
      (setf (c:window-home-path window) path)
      (multiple-value-bind (removed new-root suggested)
          (c:tree-remove-at root path :focus-path (c:world-cursor world))
        (declare (ignore removed))
        (setf (c:world-root world) new-root
              (c:world-cursor world) (c:repair-path new-root suggested))))
    (setf (c:window-minimized-p window) t)
    (pushnew window (c:world-scratchpad world))
    window))

(defmethod on-restore ((policy conventional-policy) world (window c:window))
  "Bring a window back from the scratchpad.

To the slot it was minimized from when that slot still exists, and to the
cursor's pane otherwise.  Remembering the slot is worth the one accessor: the
common case is minimize-look-at-something-else-restore, and landing back where
you were is the difference between minimize being useful and being a way to
lose a window."
  (setf (c:world-scratchpad world) (remove window (c:world-scratchpad world))
        (c:window-minimized-p window) nil)
  (let* ((root (c:world-root world))
         (home (c:window-home-path window))
         (target (and home (c:resolve-path root home)))
         (leaf (c:make-leaf window)))
    (cond
      ;; The old slot survives and is empty: it is still yours.
      ((and target (typep target 'c:leaf) (c:leaf-empty-p target))
       (setf (c:world-root world) (c:tree-replace-at root home leaf))
       (jump-cursor policy world home))
      (t
       (let ((landed (place-node policy world leaf (c:world-cursor world)
                                 (if (let ((here (c:world-leaf-at world)))
                                       (and here (c:leaf-empty-p here)))
                                     :fill :split))))
         (jump-cursor policy world landed))))))

(defmethod key-unbound ((policy conventional-policy) world keysym)
  "Typing in an empty pane spawns something there.  README D19.

Returns the *name* of a command to run, or NIL — deliberately not running it,
because policy may not depend on the runtime and the command registry lives
there.  The runtime looks the name up and runs it.

Only fires when the cursor is on an empty pane; anywhere else an unbound key
is simply unbound, which is what you want, because otherwise every typo in
every application would launch a browser."
  (let ((leaf (c:world-leaf-at world)))
    (when (and leaf (c:leaf-empty-p leaf) (characterp keysym))
      (cdr (assoc (char-downcase keysym) *empty-pane-keys* :test #'eql)))))
