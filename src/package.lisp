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
  ;; The river protocols refer to core Wayland interfaces without defining
  ;; them: wl_surface for shell surfaces and decorations, wl_output for the
  ;; output a stylus or touchscreen maps to, wl_seat for the seat an input
  ;; device is assigned to.  wayflan already generates the core protocol into
  ;; its own client package, so import the classes rather than generating a
  ;; second, incompatible copy of each here.
  (:import-from #:xyz.shunter.wayflan.client #:wl-surface #:wl-output #:wl-seat)
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
   #:pointer-binding-enable #:pointer-binding-disable
   #:bindings-get-seat #:bindings-get-xkb-binding
   #:bindings-seat-ensure-next-key-eaten
   #:bindings-seat-cancel-ensure-next-key-eaten
   #:bindings-seat-modifiers-watch
   ;; input configuration: keyboards, mice and touchpads
   #:input-destroy #:input-set-repeat-info #:input-set-scroll-factor
   #:input-map-to-output #:input-assign-to-seat
   #:libinput-destroy #:libinput-set-send-events #:libinput-set-tap
   #:libinput-set-tap-button-map #:libinput-set-drag #:libinput-set-drag-lock
   #:libinput-set-three-finger-drag #:libinput-set-accel-profile
   #:libinput-set-accel-speed #:libinput-set-natural-scroll
   #:libinput-set-left-handed #:libinput-set-click-method
   #:libinput-set-clickfinger-button-map #:libinput-set-middle-emulation
   #:libinput-set-scroll-method #:libinput-set-scroll-button
   #:libinput-set-scroll-button-lock #:libinput-set-dwt #:libinput-set-dwtp
   #:libinput-set-rotation #:libinput-set-calibration-matrix
   #:xkb-create-keymap #:xkb-keymap-destroy #:xkb-keyboard-destroy
   #:xkb-keyboard-set-keymap #:xkb-keyboard-set-layout-by-index
   #:xkb-keyboard-set-layout-by-name
   #:xkb-keyboard-capslock-enable #:xkb-keyboard-capslock-disable
   #:xkb-keyboard-numlock-enable #:xkb-keyboard-numlock-disable
   ;; enums, as plain integers
   #:+mod-none+ #:+mod-shift+ #:+mod-ctrl+ #:+mod-alt+
   #:+mod-mod3+ #:+mod-super+ #:+mod-mod5+
   #:+edges-none+ #:+edge-top+ #:+edge-bottom+ #:+edge-left+ #:+edge-right+
   #:+edges-all+
   #:+cap-window-menu+ #:+cap-maximize+ #:+cap-fullscreen+ #:+cap-minimize+
   #:+caps-all+ #:+protocol-modifier-bits+
   #:color-component #:premultiplied-rgba #:clamp-unit))

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
   ;; the container protocol.  REPLACE-CHILD was here for the life of the
   ;; project and was never defined anywhere — a DEFGENERIC that does not exist,
   ;; on the advertised surface, which nothing could see because the container
   ;; protocol had no generated document and no membership test.  It has both
   ;; now; see model/surface.lisp.  (SETF CHILD-AT) is what it would have been.
   #:container-addresses #:child-at #:remove-child #:insert-child
   #:address-equal #:container-count
   #:map-nodes #:find-node-if #:node-leaves #:node-windows #:leaf-holding
   #:copy-node #:copy-node-slots #:node-signature #:node-empty-p #:simplify-node
   #:default-address #:empty-pane-p
   #:container-alternatives-p #:container-selection #:container-splits-along-p
   #:serialize-node #:deserialize-node
   #:*insertion-weight-function* #:default-insertion-weight
   #:insertion-weight-for
   ;; the container protocol, describing itself.  The sibling of
   ;; LATTICEWM/POLICY's extension surface, and it exists for the same reason:
   ;; a surface documented by hand rots, and one generated from the image
   ;; cannot.
   #:container-protocol-p #:container-protocol-generics
   #:undocumented-container-generics #:container-surface
   #:print-container-surface #:+container-protocol-extras+
   #:generic-description #:method-description #:specializer-name
   #:print-generic-descriptions #:accessor-generic-p #:node-lineage-p
   ;; paths
   #:resolve-path #:resolve-chain #:path-valid-p #:node-path-to
   #:node-contains-p
   #:parent-path #:path-last #:path-append #:path-equal
   ;; weights
   #:weight-at #:set-weight #:normalized-weights #:adjust-weight
   ;; surgery: pure functions on a root, returning (values root path)
   #:tree-insert-at #:tree-remove-at #:tree-replace-at
   #:tree-split-at #:tree-swap #:tree-move #:tree-transplant
   #:default-split-join-p
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
   #:window-capture-sessions #:window-captured-p
   ;; input devices.  Model state for the same reason a window is: policy has
   ;; to be able to ask a device what it is before deciding how to configure
   ;; it, and policy may not depend on the runtime.
   #:input-device #:input-device-proxy #:input-device-libinput
   #:input-device-keyboard #:input-device-name #:input-device-kind
   #:input-device-capabilities #:input-device-capability
   #:input-device-settings #:input-device-setting
   #:input-device-matches-p #:+input-device-types+
   ;; the world
   #:world #:world-root #:world-cursor #:world-outputs #:world-inputs
   #:world-scratchpad
   #:world-floats #:make-world #:world-props #:world-focused-float
   #:float-of-window
   #:floating-window #:float-anchor #:float-rect #:float-window #:float-node
   #:output #:output-proxy #:output-rect #:output-name #:output-scale
   #:output-capture-sessions #:output-captured-p #:world-captures
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
   #:option-default #:option-boundp #:option-readers #:option-reader-name
   ;; the tier-0 values themselves.  Every P1 fork in the design is here.
   #:*gaps* #:*outer-gaps* #:*border-width*
   #:*focused-border-color* #:*unfocused-border-color* #:*empty-pane-color*
   #:*cursor-border-color*
   #:*spawn-mode* #:*split-axis* #:*new-child-side*
   #:*collapse-degenerate-splits* #:*move-into-occupied* #:*focus-after-close*
   #:*float-dialogs* #:*focus-follows-mouse* #:*focus-new-windows*
   #:*float-fixed-size-limit* #:*window-rules* #:+window-rule-keys+
   #:window-matches-rule-p #:check-window-rule #:node-window-prop
   #:*empty-pane-keys* #:*float-fraction* #:*smart-gaps*
   ;; --- layout ---------------------------------------------------------
   #:layout-children #:layout-node #:window-dimensions #:gravity
   #:gaps #:border-width #:border-color #:visible-p #:render-order
   #:clip-rect #:outer-rect #:reserved-space #:run-reserve-hooks
   #:*solo-windows* #:solo-window #:output-solo-window #:solo-node-p
   #:output-content #:default-workspace-for #:ensure-workspaces-for-outputs
   #:echo-content #:cursor-place-name
   ;; --- motion and focus -----------------------------------------------
   #:step-address #:entry-address #:motion-escapes-p
   #:focus-after-remove #:on-focus-change #:focus-target
   #:find-motion-target #:move-cursor #:jump-cursor #:descend-to-leaf
   #:repair-cursor #:motion-rects #:best-aligned-address
   #:mru-path #:node-rect #:place-node
   ;; --- structure ------------------------------------------------------
   #:spawn-target #:split-axis-for #:new-child-side #:should-collapse-p
   #:move-into-occupied #:insertion-weight
   ;; what a workspace is made of.  The Z axis of "lattices one behind
   ;; another" is this generic and nothing else.
   #:make-workspace #:*new-workspace*
   ;; the split mechanism, as policy rather than as a TYPEP
   #:container-axis #:equalize-container #:tab-siblings #:resize-container
   #:*resize-amount*
   #:join-existing-split-p #:split-join-predicate
   ;; logging, and the boundary every policy method is called behind
   #:logmsg #:guarded #:with-abandon #:install-debugger-hook
   #:*log-level* #:*log-stream* #:+log-levels+
   #:*log-file* #:*log-max-bytes* #:*log-keep* #:*log-to-stderr*
   #:default-log-file #:resolved-log-file #:close-log-file #:log-backtrace
   #:log-line #:add-emergency-thunk #:*emergency-thunks*
   #:run-emergency-thunks
   ;; the command registry.  §extensibility-real: "the command registry
   ;; is a user interface rather than an implementation detail".
   #:defcommand #:command #:find-command #:all-commands #:run-command
   #:command-name #:command-symbol #:command-documentation #:command-function
   #:command-lambda-list #:command-interactive #:command-interactive-p
   #:command-arguments #:*commands* #:undocumented-commands
   #:*last-command* #:*not-repeatable*
   #:*command-wrappers* #:add-command-wrapper #:remove-command-wrapper
   #:define-argument-type #:argument-type #:argument-type-name
   #:argument-type-prompt #:argument-type-candidates
   #:argument-type-parser #:argument-type-documentation #:*argument-types*
   ;; text, as it reads: where a summary ends, where a line breaks,
   ;; what a truncation looks like -- decisions, not drawing
   #:summary-of #:truncate-text #:wrap-text #:split-lines #:split-words #:substitute-arguments
   ;; the five values people change on their first day
   #:*terminal* #:*editor* #:*browser* #:*file-manager* #:*modifier*
   ;; keysyms, key specs and keymaps.  The keymap is the most
   ;; user-edited object in the system; dispatching it is the runtime's.
   #:keysym-named
   #:keysym-name
   #:parse-key
   #:kbd
   #:key-to-string
   #:keymap
   #:make-keymap
   #:keymap-entries
   #:keymap-parent
   #:keymap-name
   #:define-key
   #:lookup-key
   #:keymap-keys
   #:all-bound-keys
   #:bindable-keys
   #:*shift-map* #:shifted-character #:*warn-on-rebinding*
   #:*keyboard-layout* #:*shift-maps* #:register-shift-map #:find-shift-map
   #:shift-map-names #:current-shift-map #:command-repeatable-p
   ;; input devices: the hardware half of input policy.  River hands the
   ;; window manager the machine's keyboards, mice and touchpads and expects
   ;; it to configure them, because nothing else on the system will.
   #:input-settings #:keyboard-layout-for
   #:option-settings #:apply-input-rules #:*input-rules*
   #:*tap-to-click* #:*tap-and-drag* #:*drag-lock* #:*natural-scroll*
   #:*left-handed* #:*middle-emulation* #:*disable-while-typing*
   #:*click-method* #:*scroll-method* #:*scroll-button*
   #:*accel-profile* #:*accel-speed* #:*scroll-factor*
   #:*repeat-rate* #:*repeat-delay* #:*numlock*
   #:*xkb-layout* #:*xkb-variant* #:*xkb-options* #:*xkb-model* #:*xkb-rules*
   ;; the six protocols POLICY implements
   #:layout-policy #:appearance-policy #:motion-policy
   #:structure-policy #:lifecycle-policy #:input-policy
   #:+policy-protocols+ #:policy-lineage-p
   ;; hooks: noticing that something happened
   #:*hooks*
   #:*hook-documentation*
   #:defhook
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:all-hooks
   #:*warn-on-undeclared-hooks*
   #:*keymap*
   #:*pending-keymap*
   #:*help-visible*
   #:modifier-mask
   #:modifier-names
   #:*modifier-aliases*
   #:+modifier-order+
   #:+named-keysyms+
   #:*echo-message*
   #:current-message
   #:keymap-choices
   #:pending-keymap-segments
   ;; how the system describes itself, as generics rather than defuns
   #:binding-description
   #:help-entries
   #:keys-running
   #:command-help-rows
   #:welcome-rows
   ;; appearance: the widget layer's decisions, moved out of src/runtime/
   #:font #:font-p #:make-font #:font-name #:font-width #:font-height
   #:font-first-code #:font-glyphs #:font-stride
   #:*fonts* #:*default-font* #:register-font #:find-font #:font-names
   #:*keys-hint* #:*keymap-ever-opened* #:keys-hint
   #:*ui-font* #:font-for #:glyph-row #:font-text-width #:font-text-height
   #:*echo-area* #:*echo-height* #:*echo-scale* #:*echo-position*
   #:*echo-background* #:*echo-foreground* #:*echo-accent* #:*echo-divider*
   #:*echo-message-seconds*
   #:*minibuffer-prompt-color* #:*minibuffer-completion-color*
   #:*minibuffer-caret-color* #:*minibuffer-candidates-shown* #:*history-length*
   #:*help-columns* #:*help-background* #:*help-key-color* #:*help-text-color*
   #:*help-scale*
   #:*show-empty-panes* #:*empty-pane-hint* #:*empty-outline-color*
   #:*empty-hint-color* #:*overlay-buffer-idle*
   ;; --- window lifecycle -----------------------------------------------
   #:on-window-open #:on-window-close #:should-float-p #:on-minimize
   #:on-restore #:window-capabilities #:decoration-mode
   #:default-float-rect #:window-rule-for
   ;; --- input ----------------------------------------------------------
   #:key-unbound #:on-key #:pointer-focus #:capture-keys #:*readline-chords*
   #:pointer-drag-rect #:pointer-resize-edges
   #:+pointer-buttons+ #:pointer-button-code #:*pointer-bindings*
   #:*click-to-focus* #:*click-to-raise* #:*float-on-drag*
   #:*honour-client-move-requests* #:*pointer-resize-minimum* #:*pointer-snap*
   ;; --- reading from the user -------------------------------------------
   #:complete-candidates #:argument-type-for #:*argument-naming-convention*
   #:subsequence-match-p #:rank-candidates
   ;; --- stacks / workspaces --------------------------------------------
   #:stack-visible-address #:container-label #:container-role
   #:container-role-name #:world-role-name
   ;; --- introspection --------------------------------------------------
   ;; GENERIC-DESCRIPTION was here and is in LATTICEWM/CORE now, with the two
   ;; helpers it is built from: describing a CLOS generic knows nothing about
   ;; policy, and the container protocol needs the identical description.
   #:policy-generic-p #:policy-generics #:extension-surface
   #:print-extension-surface #:undocumented-generics
   #:*motion-reference-rect*))

(defpackage #:latticewm/runtime
  (:use #:cl)
  ;; The two container-protocol generics whose *methods* live here.  They are
  ;; declared in model/node.lisp with the rest of the protocol and imported so
  ;; that `r:serialize-node' keeps resolving -- the lattice specializes both,
  ;; and the whole point of moving the declaration was to change where the
  ;; obligation is documented, not to break the extension that meets it.
  (:import-from #:latticewm/core
   #:serialize-node
   #:deserialize-node)
  ;; Imported rather than prefixed at every call site.  The command registry
  ;; and the logging boundary moved to LATTICEWM/POLICY -- see PLAN §log5 --
  ;; and importing them means the move cost zero edits in the four hundred
  ;; places that use LOGMSG, GUARDED and DEFCOMMAND.  They are re-exported
  ;; below as well, so `r:defcommand' keeps resolving for the lattice.
  (:import-from #:latticewm/policy
   #:defcommand
   #:command
   #:find-command
   #:all-commands
   #:run-command
   #:command-name
   #:command-symbol
   #:command-documentation
   #:command-function
   #:command-lambda-list
   #:command-interactive
   #:command-interactive-p
   #:command-arguments
   #:command-repeatable-p
   #:*command-wrappers*
   #:add-command-wrapper
   #:remove-command-wrapper
   #:*last-command*
   #:*not-repeatable*
   #:*commands*
   #:undocumented-commands
   #:define-argument-type
   #:argument-type
   #:argument-type-name
   #:argument-type-prompt
   #:argument-type-candidates
   #:argument-type-parser
   #:argument-type-documentation
   #:*argument-types*
   #:logmsg
   #:guarded
   #:with-abandon
   #:install-debugger-hook
   #:*log-level*
   #:*log-stream*
   #:+log-levels+
   #:substitute-arguments
   #:*hooks*
   #:*hook-documentation*
   #:defhook
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:all-hooks
   #:*warn-on-undeclared-hooks*
   #:keysym-named
   #:keysym-name
   #:parse-key
   #:kbd
   #:key-to-string
   #:keymap
   #:make-keymap
   #:keymap-entries
   #:keymap-parent
   #:keymap-name
   #:define-key
   #:lookup-key
   #:keymap-keys
   #:all-bound-keys
   #:*keymap*
   #:*pending-keymap*
   #:*help-visible*
   #:modifier-mask
   #:modifier-names
   #:*modifier-aliases*
   #:+modifier-order+
   #:+named-keysyms+
   #:*echo-message* #:current-message #:keymap-choices
   #:pending-keymap-segments
   #:binding-description
   #:help-entries
   #:keys-running
   #:command-help-rows
   #:welcome-rows
   #:*terminal* #:*editor* #:*browser* #:*file-manager* #:*modifier*
   #:summary-of #:truncate-text #:wrap-text #:split-lines #:split-words)
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
   #:output-at #:output-for-rect #:output-of-window #:output-showing-workspace
   #:node-rect-now
   #:overlay-for #:all-overlays #:destroy-overlay #:forget-overlays-for-output
   #:hide-overlays #:overlay-kind #:overlay-output #:overlay-name
   #:overlay-visible-p #:destroy-canvas #:canvas-width #:canvas-height
   #:current-node #:current-leaf #:current-window #:current-path
   #:focused-window #:in-wm-thread-p
   #:window-river-node #:guarded #:with-abandon
   #:float-window-now #:unfloat-window #:minimize-window #:restore-window
   #:request-fullscreen #:rebind-keys #:request-manage #:after-command
   #:call-in-wm-thread #:in-wm #:start-swank #:*swank-port*
   ;; pointer-driven management
   #:start-pointer-op #:end-pointer-op #:apply-pointer-delta
   #:pointer-op #:pointer-op-kind #:pointer-op-window #:pointer-op-rect
   #:seat-pointer-op #:seat-pointer-window #:seat-pointer-x #:seat-pointer-y
   #:window-under-pointer #:focus-window-from-pointer #:seat-of-proxy
   #:attach-pointer-bindings
   ;; layer shell: panels, bars, wallpapers and lockers
   #:*honour-exclusive-zones* #:layer-shell-holds-keyboard-p
   #:layer-reserved-edges #:set-default-layer-output #:seat-layer-focus
   #:load-config #:config-file #:config-directory #:write-sample-config
   #:sample-config #:data-directories #:register-data-registry
   #:extension-loaded-p #:load-extension #:executable-directory
   #:cannot-start #:connect-to-compositor #:emergency-shutdown
   #:install-default-keymap #:print-options #:print-commands #:print-keymap
   #:main #:all-hooks #:defhook
   ;; commands
   #:defcommand #:command #:find-command #:all-commands #:run-command
   #:command-name #:command-symbol #:command-documentation #:command-function
   #:command-lambda-list #:command-interactive #:*last-command* #:*not-repeatable*
   ;; interactive arguments
   #:define-argument-type #:argument-type #:argument-type-name
   #:argument-type-prompt #:argument-type-candidates #:argument-type-parser
   #:argument-type-documentation #:command-arguments #:command-interactive-p
   #:call-interactively #:read-arguments #:split-words #:labelled-names
   ;; keys
   #:keymap #:make-keymap #:*keymap* #:keymap-entries #:keymap-parent
   #:keymap-name #:keymap-keys #:all-bound-keys #:keysym-name #:keysym-named
   #:parse-key #:kbd #:define-key #:lookup-key #:key-to-string
   ;; the loop
   #:start #:quit #:relayout #:mark-dirty #:compute-layout
   #:spawn #:notify #:*log-level* #:logmsg #:*server*
   #:overlay #:make-canvas #:canvas-fill #:canvas-rect #:canvas-text #:argb
   #:ensure-overlay #:overlay-commit #:overlay-hide #:overlay-rect
   #:text-width #:text-height
   #:server-compositor #:server-shm
   #:*help-visible* #:help-entries #:binding-description #:truncate-text
   #:read-string #:reading-p #:end-prompt #:prompt-segments #:echo-segments #:history #:history-push
   #:summary-of #:wrap-text #:show-help-page #:keys-running #:warp-pointer
   ;; fonts from disk
   #:load-psf #:read-file-bytes #:current-font
   ;; welcome
   #:welcome #:welcome-rows #:welcome-marker #:maybe-show-welcome
   ;; The tier-0 options declared in this package.  Every one of these is a
   ;; value the user is *told* to set — the starter init.lisp names
   ;; *TERMINAL* explicitly — and a config file is read in LATTICEWM/USER,
   ;; which inherits this package.  An unexported option is therefore not a
   ;; missing convenience: (setf *terminal* "alacritty") in a config file
   ;; interns a brand new symbol in LATTICEWM/USER and silently changes
   ;; nothing at all, which is the worst of the three possible outcomes.
   #:*cursor-theme* #:*cursor-size* #:*welcome-on-first-run*
   #:*unfloat-returns-home* #:*save-interval-seconds*
   #:*undo-depth* #:*undo-coalesce-seconds* #:*ipc-socket* #:*ipc-timeout-seconds*
   #:*swank-interface* #:*honour-exclusive-zones* #:*announce-capture*
   ;; undo
   #:undo #:redo #:undo-history #:with-undo #:record-undo #:snapshot-layout
   #:layout-snapshot #:undo-ring #:redo-ring #:restore-snapshot
   ;; tags and named scratchpads
   #:tag-window #:untag-window #:jump-to-tag #:gather-tag
   #:scratchpad-put #:scratchpad-show #:scratchpad-toggle
   #:list-scratchpads #:list-tags
   #:all-tags #:windows-tagged #:window-tagged-p #:normalize-tag
   #:all-scratchpads #:scratchpad-windows #:focus-existing-window
   ;; input devices: keyboards, mice, touchpads.  The commands are exported
   ;; for the same reason the options are -- a configuration file is read in
   ;; LATTICEWM/USER, and (list-inputs) there has to resolve to this one.
   #:input-devices #:list-inputs #:reload-input #:set-input
   #:keyboard-layout #:next-keyboard-layout
   #:apply-input-configuration #:apply-keyboard-layout #:mark-inputs-dirty
   #:wl-output-named #:input-setting-property #:+libinput-settings+
   ;; the control socket
   #:start-ipc-server #:stop-ipc-server #:ipc-socket-path #:ipc-evaluate
   #:call-in-wm-thread-sync #:check-config
   #:*manage-warn-seconds* #:*manage-timeout-seconds*
   ;; hooks, imported from POLICY above and re-exported so that
   ;; `r:add-hook' keeps resolving -- the lattice uses it.
   #:add-hook #:remove-hook #:run-hooks #:*hooks* #:defhook
   #:all-hooks #:*hook-documentation* #:*warn-on-undeclared-hooks*
   ;; state
   #:close-window-later #:run-shutdown-once
   #:on-events #:declare-handled-events #:all-handled-events
   #:save-state #:load-state #:save-state-soon #:state-file
   #:serialize-node #:deserialize-node #:read-node
   #:serialize-children #:deserialize-children
   #:window-facts #:restore-window-facts))

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
