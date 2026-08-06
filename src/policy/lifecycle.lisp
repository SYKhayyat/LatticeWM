;;;; policy/lifecycle.lisp --- What a window is, and what happens to it.
;;;;
;;;; Every shipped answer LIFECYCLE-POLICY gives, in one file.  They are
;;;; separated from conventional.lisp only because that file is about
;;;; *geometry and motion* and this one is about *windows*, and mixing them
;;;; makes both harder to read.  Everything here is equally overridable.
;;;;
;;;; THEY USED TO BE IN TWO FILES AND THE SPLIT WAS NOT A SPLIT.
;;;; policy/defaults-lifecycle.lisp held SHOULD-FLOAT-P, DEFAULT-FLOAT-RECT,
;;;; WINDOW-CAPABILITIES and DECORATION-MODE, and said they were separated
;;;; "because one is about a window's properties and the other is about tree
;;;; surgery".  That is a topic sort wearing the motion split's clothes.
;;;;
;;;; The motion split is the real thing and is worth stating, because it is
;;;; the rule this file was pretending to follow: policy/motion.lisp is the
;;;; *algorithm* — the walk up the ancestor chain — and defines no methods at
;;;; all; policy/defaults-motion.lisp is the set of answers that walk asks the
;;;; shipped containers for.  A reader can predict which of the two holds a
;;;; thing without opening either, because the rule is about what kind of code
;;;; it is rather than what it is about.
;;;;
;;;; The two lifecycle files had no such rule.  Both were shipped methods on
;;;; LIFECYCLE-POLICY, both were things you override, and "properties" versus
;;;; "events" put WINDOW-RULE-FOR — whose whole job is setting a window's
;;;; properties — in the events file, which the properties file then needed a
;;;; comment to explain.  A decomposition that has to be narrated is one the
;;;; reader cannot derive.  So: one file, and the sections below are the sort.
;;;; Gate 18 is the check that a DEFAULTS- prefix means the motion split and
;;;; not this.

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
             (c:tree-split-at root path node :axis axis :side side
                                             :join-p (split-join-predicate policy)))))
      (setf (c:world-root world) new-root)
      ;; The cursor may now be pointing at the split we just created rather
      ;; than at a place, because the node it named grew children.  Repairing
      ;; it here rather than at each call site is the same argument as D18's
      ;; single focus-repair rule: one place, or fifteen subtly different ones.
      (repair-cursor policy world)
      new-path)))

;;; ==================================================================
;;; WINDOW RULES — the declarative escape hatch
;;; ==================================================================
;;;
;;; A rule is a plist of overrides consulted once, when a window appears.  It
;;; exists for people who do not want to write methods; the method is still
;;; there underneath for people who do.
;;;
;;; UNKNOWN KEYS ARE REJECTED OUT LOUD, and that is not pedantry.  The plist
;;; used to be consulted for :FLOAT, :PATH and :FOCUS and *silently ignore
;;; everything else* — including :WORKSPACE, :FULLSCREEN and :BORDER-COLOR,
;;; which the generic's own docstring listed as recognised.  So a rule that
;;; named a documented key did nothing, and a rule with a typo in it —
;;; :FLOATING for :FLOAT — did nothing, with no error, no warning and no way to
;;; discover why.  For a configuration surface edited by hand in a language
;;; with no schema, unknown-key rejection is not optional.

(defparameter +window-rule-keys+
  '(:float :workspace :path :focus :fullscreen :minimize
    :border-color :border-width :decoration :capabilities :tags :label)
  "Every key a window rule may set.  Anything else is a typo, said out loud.")

(defun check-window-rule (rule window)
  "Complain about any key of RULE that means nothing, and return RULE.

Named in the complaint: the key, the window it was about, and the keys that do
exist — because the whole failure mode this prevents is somebody staring at a
rule that looks right."
  (loop for (key nil) on rule by #'cddr
        unless (member key +window-rule-keys+)
          do (logmsg :warn "window rule for ~a: ~s is not a rule key, so it ~
                            does nothing.~%  Known keys: ~{~s~^ ~}"
                     (or (c:window-app-id window) "a window")
                     key +window-rule-keys+))
  rule)

(define-option *window-rules* '()
  "Declarative per-window overrides, as a list of (MATCH . OVERRIDES).

MATCH is a plist selecting windows; OVERRIDES is a plist of what to do with
them.  The first rule that matches wins, so put the specific ones first.

    (setf *window-rules*
          '(((:app-id \"pavucontrol\")           :float t)
            ((:app-id \"firefox\")               :workspace 2)
            ((:title-contains \"Picture-in\")    :float t :border-color (1 0.4 0 1))
            ((:parent t)                         :float t)))

MATCH keys:

    :app-id           the app id, exactly, ignoring case
    :app-id-contains  a substring of it
    :title            the title, exactly, ignoring case
    :title-contains   a substring of it
    :parent           T for windows river reports as having a parent

OVERRIDE keys are the ones in +WINDOW-RULE-KEYS+: :float, :workspace, :path,
:focus, :fullscreen, :minimize, :border-color, :border-width, :decoration,
:capabilities, :tags and :label.

This is the tier-0 half of window placement.  The tier-1 half is a method on
WINDOW-RULE-FOR, and the tier-2 half is a method on ON-WINDOW-OPEN; all three
are supported and this one requires no Lisp beyond a quoted list.")

(defun window-matches-rule-p (window match)
  "True when WINDOW satisfies every clause of MATCH."
  (loop for (key value) on match by #'cddr
        always (case key
                 (:app-id (and (c:window-app-id window)
                               (string-equal value (c:window-app-id window))))
                 (:app-id-contains
                  (and (c:window-app-id window)
                       (search value (c:window-app-id window) :test #'char-equal)
                       t))
                 (:title (and (c:window-title window)
                              (string-equal value (c:window-title window))))
                 (:title-contains
                  (and (c:window-title window)
                       (search value (c:window-title window) :test #'char-equal)
                       t))
                 (:parent (eq (and (c:window-parent-window window) t)
                              (and value t)))
                 (t (progn
                      (logmsg :warn "window rule: ~s is not a match key" key)
                      nil)))))

(defmethod window-rule-for ((policy lifecycle-policy) (window c:window))
  "The first rule in *WINDOW-RULES* whose match clause fits WINDOW."
  (loop for entry in *window-rules*
        when (and (consp entry) (window-matches-rule-p window (first entry)))
          return (check-window-rule (rest entry) window)))

(defun apply-window-rule-appearance (rule window)
  "Put a rule's *appearance* overrides where the appearance generics find them.

On the window's PROPS rather than in slots, because a colour for one window is
exactly the kind of state DESIGN D20 predicted an extension would want and the
core has no business carrying permanently.  BORDER-COLOR and BORDER-WIDTH read
them back."
  (loop for (key value) on rule by #'cddr
        do (case key
             (:border-color (setf (c:prop window :border-color) value))
             (:border-width (setf (c:prop window :border-width) value))
             (:decoration (setf (c:prop window :decoration) value))
             (:capabilities (setf (c:prop window :capabilities) value))
             (:tags (setf (c:window-tags window)
                          (if (listp value) value (list value))))
             (:label (setf (c:prop window :label) value))))
  window)

;;; ==================================================================
;;; WHAT A WINDOW IS — floating, decoration, capabilities
;;; ==================================================================
;;;
;;; The questions asked *about* a window rather than about the tree.  A window
;;; rule overrides every one of them; APPLY-WINDOW-RULE-APPEARANCE above is
;;; what puts the override where these read it back.

(define-option *float-fixed-size-limit* '(900 . 700)
  "The largest a fixed-size window may be and still be floated for it.

A window that pins its minimum and maximum size to the same value is announcing
that it is a dialog — and that signal is right often enough to be the second
half of the shipped floating rule.  It is also *wrong* for a class of
application, which is why there is a limit here:

  * a number of GTK and Qt programs set fixed hints on their **main** window;
  * several set them transiently during startup, before the real size is known.

The user-visible result was a main application window arriving floating for no
reason the user could see and no diagnostic explaining the decision.  A
fixed-size window the size of the screen is not a dialog, and this is where
that gets said.  Set it to NIL to float every fixed-size window, or to
(0 . 0) to float none of them.")

(defmethod should-float-p ((policy lifecycle-policy) (window c:window))
  "Anything with a parent floats; a small fixed-size window floats; else tile.

River's spec says a window with a parent 'might be a dialog, file picker, or
similar', and that one signal covers the overwhelming majority of windows that
should not be tiled — without an application blacklist that goes stale.

A fixed-size hint is the second signal and catches the rest, *with a size
limit*.  Without one this over-floated: a program that pins its main window's
size, or pins it transiently during startup, arrived floating with nothing
anywhere to say why.  Every decision this method makes is now logged at
:DEBUG, naming the hint that caused it, because `why is this floating' had no
answer at all and is the first question anybody asks."
  (cond
    ((and *float-dialogs* (c:window-parent-window window))
     (logmsg :debug "~a floats: it has a parent, so river calls it a dialog"
             (or (c:window-app-id window) "a window"))
     t)
    (t
     (multiple-value-bind (want-w want-h) (c:window-preferred-size window)
       (cond
         ((not (and want-w want-h)) nil)
         ((and *float-fixed-size-limit*
               (or (> want-w (car *float-fixed-size-limit*))
                   (> want-h (cdr *float-fixed-size-limit*))))
          (logmsg :debug "~a asked for a fixed ~dx~d, which is larger than ~
                          *float-fixed-size-limit* — tiling it"
                  (or (c:window-app-id window) "a window") want-w want-h)
          nil)
         (t
          (logmsg :debug "~a floats: it pinned itself to a fixed ~dx~d"
                  (or (c:window-app-id window) "a window") want-w want-h)
          t))))))

(defmethod default-float-rect ((policy lifecycle-policy) (window c:window)
                               (output c:output))
  "Honour the window's own preferred size when it pinned one, centred;
otherwise take a fraction of the output."
  (let ((area (c:output-rect output)))
    (multiple-value-bind (want-w want-h) (c:window-preferred-size window)
      (let ((w (or want-w (round (* (c:rect-w area) *float-fraction*))))
            (h (or want-h (round (* (c:rect-h area) *float-fraction*)))))
        (c:make-rect (+ (c:rect-x area) (floor (- (c:rect-w area) w) 2))
                     (+ (c:rect-y area) (floor (- (c:rect-h area) h) 2))
                     w h)))))

(defmethod window-capabilities ((policy lifecycle-policy) (window c:window))
  "Declare all four.  We honour all four, and fullscreen is free.

Returns a list of keywords drawn from :WINDOW-MENU, :MAXIMIZE, :FULLSCREEN and
:MINIMIZE — river's bitfield, in the keyword-list form the bindings use.
River draws client-side-decoration titlebar buttons from this, so declaring a
capability you do not honour produces a button that does nothing."
  (or (c:prop window :capabilities)
      (list :window-menu :maximize :fullscreen :minimize)))

(defmethod decoration-mode ((policy lifecycle-policy) (window c:window))
  "Server-side, which under river means our borders and no client titlebar.

A window rule's :DECORATION overrides it, which is what an application that
draws a titlebar you actually want — a browser with tabs in it — needs."
  (or (c:prop window :decoration) :ssd))

;;; ==================================================================
;;; WHAT HAPPENS TO IT — arriving, leaving, minimize, restore
;;; ==================================================================
;;;
;;; CONTAINER-LABEL and CONTAINER-ROLE are structure decisions and live in
;;; policy/structure.lisp; ON-KEY is an input decision and lives in
;;; policy/input.lisp.  They were once here, when this file was everything.

(defmethod on-window-open ((policy lifecycle-policy) world (window c:window))
  "Place a newly appeared window and, by default, focus it.

The float decision comes first and short-circuits everything: a floated window
is never in the tree, so there is no placement to make.  Returning NIL tells
the runtime that no tiled path exists.

Every documented rule key is honoured here, which took three of them from
`listed in the docstring' to `does something'."
  (let ((rule (guarded "window-rule-for" (window-rule-for policy window))))
    (apply-window-rule-appearance rule window)
    ;; A declarative rule wins over the computed guess, so that people who do
    ;; not want to write methods still get the escape hatch.  MEMBER rather
    ;; than GETF, so that an explicit (:float nil) can *stop* a window floating
    ;; that SHOULD-FLOAT-P would have floated — which is the whole reason to
    ;; write the rule for half the applications people write rules for.
    (setf (c:window-floating-p window)
          (if (member :float rule)
              (and (getf rule :float) t)
              (and (guarded "should-float-p" (should-float-p policy window)) t)))
    (when (getf rule :fullscreen)
      (setf (c:window-fullscreen-p window) t))
    (cond
      ((c:window-floating-p window) nil)
      (t
       (multiple-value-bind (path disposition) (spawn-target policy world window)
         ;; :WORKSPACE names a workspace by the number written on the key that
         ;; reaches it, so it counts from one like every other workspace in the
         ;; system.  Resolved before :PATH, which is absolute and wins.
         (let* ((path (or (getf rule :path)
                          (workspace-rule-path world (getf rule :workspace))
                          path))
                (disposition (if (or (getf rule :path) (getf rule :workspace))
                                 (if (c:empty-pane-p (c:resolve-path
                                                      (c:world-root world) path))
                                     :fill
                                     :split)
                                 disposition))
                (leaf (c:make-leaf window))
                (landed (place-node policy world leaf path disposition)))
           (when (getf rule :minimize)
             (on-minimize policy world window)
             (return-from on-window-open landed))
           (when (if (member :focus rule)
                     (getf rule :focus)
                     *focus-new-windows*)
             (jump-cursor policy world landed))
           landed))))))

(defun workspace-rule-path (world number)
  "The path of workspace NUMBER, counting from one, growing the list to reach it.

NIL when NUMBER is not a number or the root is not a workspace container, which
is the honest answer for a rule naming a workspace in a policy that has none."
  (when (integerp number)
    (let ((stack (c:world-workspaces world))
          (index (max 0 (1- number))))
      (when stack
        (loop while (<= (c:container-count stack) index)
              do (c:insert-child stack (c:container-count stack) (c:make-leaf)))
        (c:first-leaf-path (c:child-at stack index) (list index))))))

(defmethod on-window-close ((policy lifecycle-policy) world (window c:window)
                            path)
  "Take the window's pane out with it, and let its sibling grow.

This is DESIGN D17's CLOSE.  The other half of D17 — CLEAR, which empties the
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
        ;; THROUGH REPAIR-CURSOR, NOT INTO THE SLOT.  This is the commonest
        ;; focus change there is — close a window and the cursor lands
        ;; somewhere else — and writing the slot here skipped both halves of
        ;; the notification: ON-FOCUS-CHANGE, so :MRU never learned that the
        ;; place had moved, and the :FOCUS-CHANGED hook, which is documented
        ;; "run after the cursor moves" and did not.  REPAIR-CURSOR is exactly
        ;; REPAIR-PATH plus those two, and has been since it was written.
        (repair-cursor policy world landed)))))

(defmethod on-minimize ((policy lifecycle-policy) world (window c:window))
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

(defmethod on-restore ((policy lifecycle-policy) world (window c:window))
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

(defmethod key-unbound ((policy input-policy) world keysym)
  "Typing in an empty pane spawns something there.  DESIGN D19.

Returns the *name* of a command to run, or NIL — deliberately not running it,
because policy may not depend on the runtime and the command registry lives
there.  The runtime looks the name up and runs it.

Only fires when the cursor is on an empty pane; anywhere else an unbound key
is simply unbound, which is what you want, because otherwise every typo in
every application would launch a browser."
  (let ((leaf (c:world-leaf-at world)))
    (when (and leaf (c:leaf-empty-p leaf) (characterp keysym))
      (cdr (assoc (char-downcase keysym) *empty-pane-keys* :test #'eql)))))
