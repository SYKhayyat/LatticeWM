;;;; package.lisp --- Package structure for LatticeWM.
;;;;
;;;; The package boundaries encode the architecture:
;;;;
;;;;   LATTICEWM/RIVER    generated protocol bindings.  Plumbing.  Policy code
;;;;                      must never call into this package directly — see
;;;;                      LATTICEWM/WIRE for why.
;;;;   LATTICEWM/WIRE     the sequence discipline and one checked wrapper per
;;;;                      generated request.  Still plumbing.
;;;;   LATTICEWM/CORE     the model: nodes, containers, paths, geometry, and
;;;;                      pure tree surgery.  No protocol calls anywhere.
;;;;   LATTICEWM/POLICY   *the extension surface*.  Generic functions and their
;;;;                      docstrings, plus the shipped default methods.  An
;;;;                      extension depends on this package and nothing else.
;;;;   LATTICEWM/RUNTIME  window lifecycle, the placement emitter, keys,
;;;;                      commands, the session.
;;;;   LATTICEWM/USER     the package ~/.config/latticewm/init.lisp is read in.
;;;;                      Everything a user or an extension needs is visible
;;;;                      here without a single prefix.
;;;;
;;;; The dependency arrows run CORE <- POLICY <- RUNTIME, and WIRE <- RUNTIME.
;;;; POLICY never depends on WIRE: a policy decides *where things go*, and the
;;;; runtime decides which protocol sequence that turns into.

(defpackage #:latticewm/river
  (:use #:cl)
  ;; river-window-management-v1 refers to wl_surface (for shell surfaces and
  ;; decorations) without defining it.  wayflan already generates the core
  ;; Wayland protocol into its own client package, so import the class rather
  ;; than generating a second, incompatible copy of it here.
  (:import-from #:xyz.shunter.wayflan.client #:wl-surface)
  (:documentation
   "Generated river protocol bindings, produced by wayflan's scanner from the
vendored XML under src/protocol/.

Do not call anything in here from policy code.  wayflan generates requests as
plain functions rather than generic functions, so the manage/render sequence
discipline cannot be attached to them with DEFMETHOD, and redefining a
generated function in place would be silently reverted by the next scanner
run.  LATTICEWM/WIRE exists to hold that discipline."))

(defpackage #:latticewm/wire
  (:use #:cl)
  (:local-nicknames (#:river #:latticewm/river)
                    (#:wl #:xyz.shunter.wayflan.client)
                    (#:a #:alexandria))
  (:documentation
   "The manage/render sequence discipline, and a checked wrapper for every
generated request.

River splits client-callable state into two disjoint categories.  *Window
management state* — dimensions, fullscreen, keyboard focus, keybindings — may
only be modified during a manage sequence.  *Rendering state* — position,
render order, hide/show, borders, clip boxes — may be modified during either.
Violating this is a hard protocol error that kills the connection.

Rather than encode that per call site, every generated request gets a wrapper
that consults *SEQUENCE* before a single byte reaches the wire, and signals a
continuable SEQUENCE-VIOLATION if the context is wrong.")
  (:export
   ;; the discipline
   #:*sequence*
   #:sequence-violation
   #:sequence-violation-request
   #:sequence-violation-actual
   #:sequence-violation-required
   #:check-sequence
   #:with-manage-sequence
   #:with-render-sequence
   #:*render-legal-in-manage*
   #:request-sequence-class
   #:all-wrapped-requests
   ;; the small vocabulary the runtime uses
   #:wm-manage-finish #:wm-render-finish #:wm-manage-dirty
   #:wm-exit-session #:wm-stop #:wm-get-shell-surface
   #:window-close #:window-propose-dimensions #:window-set-dimension-bounds
   #:window-set-capabilities #:window-set-tiled #:window-use-csd #:window-use-ssd
   #:window-fullscreen #:window-exit-fullscreen
   #:window-inform-maximized #:window-inform-unmaximized
   #:window-inform-fullscreen #:window-inform-not-fullscreen
   #:window-inform-resize-start #:window-inform-resize-end
   #:window-hide #:window-show #:window-set-borders
   #:window-set-clip-box #:window-set-content-clip-box
   #:window-get-node #:window-destroy
   #:node-set-position #:node-place-top #:node-place-bottom
   #:node-place-above #:node-place-below #:node-destroy
   #:seat-focus-window #:seat-clear-focus #:seat-focus-shell-surface
   #:seat-set-xcursor-theme #:seat-pointer-warp
   #:seat-op-start-pointer #:seat-op-end #:seat-get-pointer-binding
   #:output-set-presentation-mode
   #:binding-enable #:binding-disable #:binding-set-layout-override
   #:bindings-get-seat #:bindings-get-xkb-binding
   #:bindings-seat-ensure-next-key-eaten
   #:bindings-seat-cancel-ensure-next-key-eaten
   #:bindings-seat-modifiers-watch
   ;; enums, as plain integers
   #:+mod-none+ #:+mod-shift+ #:+mod-ctrl+ #:+mod-alt+
   #:+mod-mod3+ #:+mod-super+ #:+mod-mod5+
   #:+edges-none+ #:+edge-top+ #:+edge-bottom+ #:+edge-left+ #:+edge-right+
   #:+edges-all+
   #:+cap-window-menu+ #:+cap-maximize+ #:+cap-fullscreen+ #:+cap-minimize+
   #:+caps-all+ #:+protocol-modifier-bits+ #:+modifier-order+
   #:modifier-mask #:modifier-names #:color-component))

(defpackage #:latticewm/core
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:documentation
   "The model.  Nodes, the container protocol, paths, pure geometry and pure
tree surgery.

Nothing in this package touches the protocol, the compositor, or any global
state.  All of it is unit-testable without a compositor, and all of it is
deliberately decomposed so that policy can redirect it.")
  (:export
   ;; rectangles
   #:rect #:make-rect #:rect-x #:rect-y #:rect-w #:rect-h
   #:rect-p #:copy-rect #:rect-right #:rect-bottom #:rect-contains-p
   #:rect-intersect #:rect-empty-p #:rect-equal #:rect-inset #:rect-center
   #:divide-rect
   ;; directions
   #:+directions+ #:direction-p #:direction-axis #:direction-sign
   #:opposite-direction #:direction-horizontal-p #:direction-vertical-p
   #:axis-of #:direction-for
   ;; nodes
   #:node #:props #:prop #:node-id #:node-label
   #:leaf #:leaf-window #:make-leaf #:leaf-empty-p
   #:container #:children #:weights #:container-p #:sequential-container
   #:split #:split-axis #:make-split
   #:stack #:stack-selected #:make-stack
   ;; the container protocol
   #:container-addresses #:child-at #:remove-child #:insert-child
   #:replace-child #:address-equal #:container-count
   #:map-nodes #:find-node-if #:node-leaves #:node-windows #:leaf-holding
   #:copy-node #:node-empty-p #:simplify-node #:default-address
   ;; paths
   #:resolve-path #:resolve-chain #:path-valid-p #:node-path-to
   #:node-contains-p
   #:parent-path #:path-last #:path-append #:path-equal
   ;; weights
   #:weight-at #:set-weight #:normalized-weights #:adjust-weight
   ;; surgery: pure functions on a root, returning (values root path)
   #:tree-insert-at #:tree-remove-at #:tree-replace-at
   #:tree-split-at #:tree-swap #:tree-move #:tree-transplant
   #:repair-path #:first-leaf-path #:last-leaf-path #:leaf-paths
   #:next-leaf-path #:previous-leaf-path
   ;; windows.  The class is model state; the runtime fills it from protocol
   ;; events and owns the proxy, but policy has to be able to ask a window
   ;; about itself, and policy may not depend on the runtime.
   #:window #:window-proxy #:window-identifier #:window-app-id #:window-title
   #:window-width #:window-height #:window-parent-window #:window-live-p
   #:window-min-width #:window-min-height #:window-max-width #:window-max-height
   #:window-floating-p #:window-minimized-p #:window-fullscreen-p
   #:window-decoration-hint #:window-rect #:window-tags #:window-home-path
   #:window-preferred-size
   ;; the world
   #:world #:world-root #:world-cursor #:world-outputs #:world-scratchpad
   #:world-floats #:make-world #:world-props #:world-focused-float
   #:float-of-window
   #:floating-window #:float-anchor #:float-rect #:float-window #:float-node
   #:output #:output-proxy #:output-rect #:output-name #:output-scale
   #:world-node-at #:world-leaf-at #:world-window-at #:world-focus-window
   #:current-workspace #:workspace-path #:world-workspaces))

(defpackage #:latticewm/policy
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:a #:alexandria))
  (:documentation
   "THE EXTENSION SURFACE.

Every decision the window manager makes that a user might plausibly want to
change is a generic function declared here, with a docstring stating its
contract.  src/policy/protocol.lisp contains generics and docstrings only —
not one method — so that it can be read in a single sitting and used as the
map of what is changeable.

The shipped behaviour lives in src/policy/conventional.lisp as methods on
CONVENTIONAL-POLICY.  To change any of it, write a DEFMETHOD from outside.
You never edit this package.")
  (:export
   ;; policy objects and tier-0 options
   #:policy #:conventional-policy #:*policy* #:current-policy #:policy-name
   #:define-option #:option #:option-documentation #:all-options
   #:option-default #:option-boundp
   ;; the tier-0 values themselves.  Every P1 fork in the design is here.
   #:*gaps* #:*outer-gaps* #:*border-width*
   #:*focused-border-color* #:*unfocused-border-color* #:*empty-pane-color*
   #:*spawn-mode* #:*split-axis* #:*new-child-side*
   #:*collapse-degenerate-splits* #:*move-into-occupied* #:*focus-after-close*
   #:*float-dialogs* #:*focus-follows-mouse* #:*focus-new-windows*
   #:*empty-pane-keys* #:*float-fraction* #:*smart-gaps*
   ;; --- layout ---------------------------------------------------------
   #:layout-children #:layout-node #:window-dimensions #:gravity
   #:gaps #:border-width #:border-color #:visible-p #:render-order
   #:clip-rect #:outer-rect #:reserved-space #:*reserve-hooks*
   #:output-content #:default-workspace-for #:ensure-workspaces-for-outputs
   #:echo-content
   ;; --- motion and focus -----------------------------------------------
   #:step-address #:entry-address #:motion-escapes-p
   #:focus-after-remove #:on-focus-change
   #:find-motion-target #:move-cursor #:jump-cursor #:descend-to-leaf
   #:repair-cursor #:motion-rects #:best-aligned-address
   #:mru-path #:node-rect #:place-node
   ;; --- structure ------------------------------------------------------
   #:spawn-target #:split-axis-for #:new-child-side #:should-collapse-p
   #:move-into-occupied #:insertion-weight
   ;; --- window lifecycle -----------------------------------------------
   #:on-window-open #:on-window-close #:should-float-p #:on-minimize
   #:on-restore #:window-capabilities #:decoration-mode
   #:default-float-rect #:window-rule-for
   ;; --- input ----------------------------------------------------------
   #:key-unbound #:on-key #:pointer-focus
   ;; --- stacks / workspaces --------------------------------------------
   #:stack-visible-address #:container-label
   ;; --- introspection --------------------------------------------------
   #:policy-generic-p #:policy-generics #:extension-surface
   #:print-extension-surface #:undocumented-generics #:generic-description
   #:*motion-reference-rect*))

(defpackage #:latticewm/runtime
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:w #:latticewm/wire)
                    (#:river #:latticewm/river)
                    (#:wl #:xyz.shunter.wayflan.client)
                    (#:a #:alexandria))
  (:documentation
   "The parts that talk to river: window lifecycle, the placement emitter,
keybindings, the command registry, and the session loop.")
  (:export
   ;; the live session
   #:*world* #:*server* #:server #:seat #:primary-seat #:server-manager
   #:server-display #:server-seats #:server-running #:seat-proxy
   #:window-of-proxy #:all-windows #:all-outputs #:current-output
   #:current-node #:current-leaf #:current-window #:current-path
   #:focused-window #:in-wm-thread-p
   #:window-river-node #:guarded #:with-abandon
   #:float-window-now #:unfloat-window #:minimize-window #:restore-window
   #:request-fullscreen #:rebind-keys #:request-manage #:after-command
   #:call-in-wm-thread #:in-wm #:start-swank #:*swank-port*
   #:load-config #:config-file #:config-directory #:write-sample-config
   #:install-default-keymap #:print-options #:print-commands #:print-keymap
   #:main #:all-hooks #:defhook
   ;; commands
   #:defcommand #:command #:find-command #:all-commands #:run-command
   #:command-name #:command-documentation #:command-function
   ;; keys
   #:keymap #:make-keymap #:*keymap* #:keymap-entries #:keymap-parent
   #:parse-key #:kbd #:define-key #:lookup-key #:key-to-string
   ;; the loop
   #:start #:quit #:relayout #:mark-dirty #:compute-layout
   #:spawn #:notify #:*log-level* #:logmsg #:*server*
   #:overlay #:make-canvas #:canvas-fill #:canvas-rect #:canvas-text #:argb
   #:ensure-overlay #:overlay-commit #:overlay-hide #:overlay-rect
   #:text-width #:text-height #:*echo-area* #:*echo-scale* #:*echo-position*
   #:current-message #:server-compositor #:server-shm
   #:*help-visible* #:help-entries #:binding-description #:truncate-text
   #:read-string #:reading-p #:end-prompt #:prompt-segments
   #:*minibuffer-prompt-color* #:*minibuffer-completion-color*
   #:summary-of
   ;; hooks
   #:add-hook #:remove-hook #:run-hooks #:*hooks*
   ;; state
   #:save-state #:load-state #:state-file))

(defpackage #:latticewm/user
  (:use #:cl #:latticewm/core #:latticewm/policy #:latticewm/runtime)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:w #:latticewm/wire))
  (:documentation
   "The package a user's init.lisp — and most extensions — are read in.

It inherits LATTICEWM/CORE, LATTICEWM/POLICY and LATTICEWM/RUNTIME wholesale,
so a config file can say

    (defmethod should-float-p ((p policy) (win window))
      (or (call-next-method) (equal (window-app-id win) \"pavucontrol\")))

with no package prefixes anywhere."))
