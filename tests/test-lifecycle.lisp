;;;; tests/test-lifecycle.lisp --- Spawning, closing, floating, minimizing.

(in-package #:latticewm/tests)
(in-suite lifecycle)

(defun fresh-world ()
  "A world in its shipped starting shape: one workspace, one empty pane."
  (c:make-world))

(test a-fresh-world-is-one-empty-pane
  (let ((world (fresh-world)))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world))))
    (is (equal '(0) (c:world-cursor world)))
    (is (null (c:world-focus-window world)))))

(test first-window-fills-the-empty-pane
  ;; An empty pane exists because the user made a place for something.  Putting
  ;; the next thing there is the only reading that respects the gesture.
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "emacs"))
    (is (equal '(:stack 0 (:leaf "emacs")) (shape (c:world-root world))))
    (is (equal "emacs" (c:window-app-id (c:world-focus-window world))))))

(test second-window-splits-the-focused-pane
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "emacs"))
    (p:on-window-open pol world (win "term"))
    (is (equal '(:stack 0 (:h (:leaf "emacs") (:leaf "term")))
               (shape (c:world-root world))))
    (is (equal "term" (c:window-app-id (c:world-focus-window world)))
        "and focus followed the new window")))

(test three-windows-stay-one-split
  (let ((world (fresh-world)) (pol (policy))
        (p:*split-axis* :horizontal))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    (is (equal '(:stack 0 (:h (:leaf "a") (:leaf "b") (:leaf "c")))
               (shape (c:world-root world)))
        "n-ary splits keep three side-by-side windows flat")))

(test split-axis-longer-alternates
  (let ((world (fresh-world)) (pol (policy)))
    ;; A wide output makes the first cut horizontal and the second vertical.
    (push (make-instance 'c:output :rect (c:make-rect 0 0 1000 500))
          (c:world-outputs world))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (eq :horizontal
            (c:split-axis (c:resolve-path (c:world-root world) '(0)))))))

(test focus-new-windows-can-be-turned-off
  (let ((world (fresh-world)) (pol (policy))
        (p:*focus-new-windows* nil))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (equal "a" (c:window-app-id (c:world-focus-window world))))))

(test dialogs-float-and-are-not-tiled
  (let* ((world (fresh-world)) (pol (policy))
         (parent (win "gimp"))
         (dialog (win "gimp")))
    (setf (c:window-parent-window dialog) parent)
    (p:on-window-open pol world parent)
    (is (null (p:on-window-open pol world dialog))
        "a floated window returns no tiled path")
    (is-true (c:window-floating-p dialog))
    (is (equal '(:stack 0 (:leaf "gimp")) (shape (c:world-root world)))
        "and never entered the tree")))

(test a-fixed-size-window-floats
  (let ((world (fresh-world)) (pol (policy)) (w (win "splash")))
    (setf (c:window-min-width w) 400 (c:window-max-width w) 400
          (c:window-min-height w) 300 (c:window-max-height w) 300)
    (is-true (p:should-float-p pol w))
    (p:on-window-open pol world w)
    (is-true (c:window-floating-p w))))

(test floating-can-be-turned-off-wholesale
  (let ((pol (policy)) (p:*float-dialogs* nil)
        (dialog (win "gimp")))
    (setf (c:window-parent-window dialog) (win "gimp"))
    (is-false (p:should-float-p pol dialog))))

(test closing-collapses-and-the-sibling-grows
  (let ((world (fresh-world)) (pol (policy)) (a (win "a")))
    (p:on-window-open pol world a)
    (p:on-window-open pol world (win "b"))
    (let ((path (c:node-path-to (c:world-root world)
                                (c:leaf-holding (c:world-root world) a))))
      (p:on-window-close pol world a path))
    (is (equal '(:stack 0 (:leaf "b")) (shape (c:world-root world))))
    (is (equal "b" (c:window-app-id (c:world-focus-window world))))))

(test closing-the-last-window-leaves-an-empty-pane-not-a-hole
  (let ((world (fresh-world)) (pol (policy)) (a (win "a")))
    (p:on-window-open pol world a)
    (p:on-window-close pol world a '(0))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world)))
        "the workspace survives as a place you can spawn into")
    (is (equal '(0) (c:world-cursor world)))))

(test focus-after-close-stays-put-by-default
  ;; The governing property: nothing moves the viewport except the user.
  (let ((world (fresh-world)) (pol (policy)))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    (let* ((root (c:world-root world))
           (b (c:leaf-window (c:resolve-path root '(0 1)))))
      (p:on-window-close pol world b '(0 1))
      (is (equal "c" (c:window-app-id (c:world-focus-window world)))
          "landed on the pane that took the closed one's place"))))

(test focus-after-close-mru-is-one-option-away
  (let ((world (fresh-world)) (pol (policy))
        (p:*focus-after-close* :mru))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    ;; Visit "a" so it is the most recent survivor, then come back and close "c".
    (p:jump-cursor pol world '(0 0))
    (p:jump-cursor pol world '(0 2))
    (let ((c-window (c:leaf-window (c:resolve-path (c:world-root world) '(0 2)))))
      (p:on-window-close pol world c-window '(0 2))
      (is (equal "a" (c:window-app-id (c:world-focus-window world)))))))

(test minimize-takes-the-window-out-of-the-tree
  ;; The stated requirement, honoured literally: the remaining windows retile
  ;; without it.  Not "hide it somewhere".
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (let ((b (c:leaf-window (c:resolve-path (c:world-root world) '(0 1)))))
      (p:on-minimize pol world b)
      (is (equal '(:stack 0 (:leaf "a")) (shape (c:world-root world))))
      (is (equal (list b) (c:world-scratchpad world)))
      (is-true (c:window-minimized-p b))
      (is (equal '(0 1) (c:window-home-path b)) "it remembered where it was"))))

(test restore-returns-to-the-remembered-slot-when-it-survives
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    ;; Clear the pane rather than closing it, so the slot outlives the window.
    (let* ((root (c:world-root world))
           (leaf (c:resolve-path root '(0 1)))
           (b (c:leaf-window leaf)))
      (setf (c:window-home-path b) '(0 1)
            (c:leaf-window leaf) nil)
      (push b (c:world-scratchpad world))
      (p:on-restore pol world b)
      (is (equal '(:stack 0 (:h (:leaf "a") (:leaf "b")))
                 (shape (c:world-root world))))
      (is (null (c:world-scratchpad world))))))

(test restore-falls-back-to-the-cursor
  (let ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (let ((b (c:leaf-window (c:resolve-path (c:world-root world) '(0 1)))))
      (p:on-minimize pol world b)
      (p:on-restore pol world b)
      (is (= 2 (length (c:node-windows (c:world-root world)))))
      (is (equal "b" (c:window-app-id (c:world-focus-window world)))))))

(defclass ruled-policy (p:conventional-policy) ()
  (:documentation "A policy with a declarative window rule.  The escape hatch
for people who do not want to write methods — implemented, of course, as a
method."))

(defmethod p:window-rule-for ((pol ruled-policy) (w c:window))
  (when (equal (c:window-app-id w) "pinentry") (list :float t)))

(test window-rules-override-the-computed-guess
  (let ((world (fresh-world)) (pol (make-instance 'ruled-policy)))
    (p:on-window-open pol world (win "pinentry"))
    (is (equal '(:stack 0 (:leaf nil)) (shape (c:world-root world)))
        "the rule floated it, so the tree is untouched")))

(test empty-pane-keys-only-fire-on-an-empty-pane
  (let ((world (fresh-world)) (pol (policy)))
    (is (equal "terminal" (p:key-unbound pol world #\t))
        "the cursor starts on an empty pane")
    (p:on-window-open pol world (win "emacs"))
    (is (null (p:key-unbound pol world #\t))
        "and an unbound key is simply unbound once something is there")))

(test spawn-mode-stack-makes-tabs
  (let ((world (fresh-world)) (pol (policy)) (p:*spawn-mode* :stack))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    (is (equal '(:stack 0 (:stack 1 (:leaf "a") (:leaf "b")))
               (shape (c:world-root world))))))

(test spawn-mode-fill-first-finds-a-hole
  (let ((world (fresh-world)) (pol (policy)) (p:*spawn-mode* :fill-first))
    (p:on-window-open pol world (win "a"))
    (p:on-window-open pol world (win "b"))
    ;; Clear the first pane, then spawn: it should land in the hole.
    (setf (c:leaf-window (c:resolve-path (c:world-root world) '(0 0))) nil)
    (p:on-window-open pol world (win "c"))
    (is (equal '(:stack 0 (:h (:leaf "c") (:leaf "b")))
               (shape (c:world-root world))))))

;;; ------------------------------------------------------------- layout

(test layout-divides-by-weight
  (let* ((pol (policy))
         (root (c:make-split :horizontal
                             (list (leaf-with "a") (leaf-with "b")) '(1 3)))
         (placements (p:layout-node pol root (c:make-rect 0 0 400 100))))
    (destructuring-bind (whole a b) placements
      (declare (ignore whole))
      (is (= 100 (c:rect-w (third a))))
      (is (= 300 (c:rect-w (third b))))
      (is-true (fourth a)))))

(test layout-hides-unselected-tabs-but-still-visits-them
  ;; River shows a window unless it is explicitly hidden, so a hidden tab that
  ;; the layout never visited would be drawn on top of everything.
  (let* ((pol (policy))
         (root (c:make-stack (list (leaf-with "front") (leaf-with "back")) 0))
         (placements (p:layout-node pol root (c:make-rect 0 0 100 100))))
    (is (= 3 (length placements)) "both children were visited")
    (let ((back (find-if (lambda (pl)
                           (equal "back" (let ((w (and (typep (first pl) 'c:leaf)
                                                       (c:leaf-window (first pl)))))
                                           (and w (c:window-app-id w)))))
                         placements)))
      (is-false (fourth back) "and the hidden one is marked invisible"))))

(test layout-gaps-come-out-of-the-panes
  (let* ((pol (policy)) (p:*gaps* 10)
         (root (c:make-split :horizontal (list (leaf-with "a") (leaf-with "b"))))
         (placements (p:layout-node pol root (c:make-rect 0 0 100 100))))
    (is (= 45 (c:rect-w (third (second placements)))))))

(test window-dimensions-leave-room-for-the-border
  (let* ((pol (policy)) (p:*border-width* 3)
         (leaf (leaf-with "a")))
    (multiple-value-bind (w h) (p:window-dimensions pol leaf (c:make-rect 0 0 100 50))
      (is (= 94 w))
      (is (= 44 h)))))

(test gravity-centres-a-window-that-refused-its-size
  (let* ((pol (policy))
         (r (p:gravity pol (leaf-with "a") (c:make-rect 0 0 100 100) 40 40)))
    (is (= 30 (c:rect-x r)))
    (is (= 30 (c:rect-y r)))))

(test render-order-puts-floats-above-tiles
  (let* ((pol (policy))
         (tiled (leaf-with "tiled"))
         (floated (leaf-with "floated")))
    (setf (c:window-floating-p (c:leaf-window floated)) t)
    (let ((ordered (p:render-order pol (list (list floated '(1) nil t)
                                             (list tiled '(0) nil t)))))
      (is (eq tiled (first (first ordered))))
      (is (eq floated (first (second ordered)))))))

;;; ------------------------------------------------------------ persistence

(test the-layout-round-trips-through-the-state-file
  ;; The first version of the format wrote weights positionally and read them
  ;; back by asking which elements were conses.  Weights *are* a cons, so every
  ;; restart grew a spurious empty pane at the front of every split — a tree
  ;; that was subtly wrong in a way that looked like a layout bug.
  (let* ((world (fresh-world))
         (pol (policy)))
    (dolist (app '("a" "b" "c"))
      (p:on-window-open pol world (win app)))
    ;; Give each window an identifier, as river would.
    (loop for window in (c:node-windows (c:world-root world))
          for i from 0
          do (setf (c:window-identifier window) (format nil "id~d" i)))
    ;; Make the tree non-trivial: a nested split with uneven weights.
    (let ((split (c:resolve-path (c:world-root world) '(0))))
      (setf (c:weights split) '(3 1 2)))
    (let* ((before (shape (c:world-root world)))
           (weights (copy-list (c:weights (c:resolve-path (c:world-root world) '(0)))))
           (form (funcall (read-from-string "latticewm/runtime::serialize-node")
                          (c:world-root world)))
           (index (let ((table (make-hash-table :test #'equal)))
                    (dolist (window (c:node-windows (c:world-root world)) table)
                      (setf (gethash (c:window-identifier window) table) window))))
           (after (funcall (read-from-string "latticewm/runtime::read-node")
                           form index)))
      (is (equal before (shape after))
          "the tree came back identical, with no phantom panes")
      (is (equal weights (c:weights (c:resolve-path after '(0))))
          "and the weights survived, so a restart does not re-equalize"))))

(test the-state-file-is-readable-and-self-describing
  (let* ((world (fresh-world)) (pol (policy)))
    (p:on-window-open pol world (win "a"))
    (let ((form (funcall (read-from-string "latticewm/runtime::serialize-node")
                         (c:world-root world))))
      (is (eq :stack (first form)))
      (is (member :children form) "fields are named, not positional")
      ;; And it must survive a print/read cycle, since that is what the file is.
      (let ((*package* (find-package :keyword)))
        (is (equal form (read-from-string (prin1-to-string form))))))))

(test an-unknown-container-degrades-to-its-contents
  ;; A layout saved with an extension loaded, reloaded without it.  The
  ;; arrangement is lost; the windows must not be.
  (let* ((index (make-hash-table :test #'equal))
         (a (win "a")) (b (win "b")))
    (setf (gethash "ia" index) a (gethash "ib" index) b)
    (let ((node (funcall (read-from-string "latticewm/runtime::read-node")
                         '(:unknown :type "GRID"
                           :children ((:leaf :window "ia") (:leaf :window "ib")))
                         index)))
      (is (equal '("a" "b") (mapcar #'c:window-app-id (c:node-windows node)))
          "both windows survived a container kind that is no longer loaded"))))

(test a-restored-cursor-is-always-on-a-screen
  ;; The bug: restart while on workspace 3 of a single-monitor session.  The
  ;; cursor came back on workspace 3 and the output came back showing workspace
  ;; 1, because which workspace an output displays lives on the output and
  ;; outputs are made fresh at startup.  The result is a black screen and a
  ;; status line confidently reporting [3/3].
  (let* ((stack (c:make-stack (leaves 3)))
         (world (c:make-world :root stack))
         (output (make-instance 'c:output :name "DP-1")))
    (setf (c:world-outputs world) (list output)
          (c:world-cursor world) '(2)
          (c:stack-selected stack) 2)
    (let ((r::*world* world))
      ;; No saved mapping at all -- an older state file, or a monitor that has
      ;; been swapped for a different one.
      (r::restore-output-workspaces '())
      (is (= 2 (c:prop output :workspace))
          "the output follows the cursor when nothing else says otherwise"))))

(test a-saved-output-keeps-its-own-workspace
  (let* ((stack (c:make-stack (leaves 4)))
         (world (c:make-world :root stack))
         (left (make-instance 'c:output :name "DP-1"))
         (right (make-instance 'c:output :name "HDMI-A-1")))
    (setf (c:world-outputs world) (list left right)
          (c:world-cursor world) '(3)
          (c:stack-selected stack) 3)
    (let ((r::*world* world))
      (r::restore-output-workspaces '(("DP-1" . 3) ("HDMI-A-1" . 1)))
      (is (= 3 (c:prop left :workspace)))
      (is (= 1 (c:prop right :workspace))
          "the second monitor keeps its own workspace rather than being
           dragged to the cursor's"))))

(test a-saved-workspace-that-no-longer-exists-is-ignored
  (let* ((stack (c:make-stack (leaves 2)))
         (world (c:make-world :root stack))
         (output (make-instance 'c:output :name "DP-1")))
    (setf (c:world-outputs world) (list output)
          (c:world-cursor world) '(1))
    (let ((r::*world* world))
      (r::restore-output-workspaces '(("DP-1" . 9)))
      (is (= 1 (c:prop output :workspace))
          "an index past the end of the stack is dropped, not clamped to it"))))

;;; ==================================================================
;;; WHAT A NEW WORKSPACE IS MADE OF
;;; ==================================================================
;;;
;;; "Infinite workspaces" was always true — the workspace list is a stack and a
;;; stack grows — and it was true in a way that quietly stopped short: the four
;;; sites that grow it each built an empty pane by hand, so the *shape* of a new
;;; workspace was not a decision anything could take part in.  An extension that
;;; changes what a workspace is could change the ones that existed when it
;;; loaded and no others.
;;;
;;; MAKE-WORKSPACE is the decision, and these are the core's half of it.

(defun conventional-world (&optional (root nil))
  "A world and a conventional policy, ready for the workspace verbs."
  (values (if root (c:make-world :root root) (c:make-world))
          (make-instance 'p:conventional-policy)))

(test asking-for-a-workspace-that-is-not-there-yet-makes-it-and-every-one-before
  ;; This is the whole of "infinite workspaces", and it is four lines of LOOP.
  (multiple-value-bind (world policy) (conventional-world)
    (let ((r::*world* world) (p:*policy* policy))
      (r::workspace 40)
      (is (= 40 (c:container-count (c:world-root world))))
      (is (= 39 (c:container-selection (c:world-root world))))
      (is (equal '(39) (c:world-cursor world))
          "and you are standing in the one you asked for"))))

(test a-new-workspace-is-an-empty-pane-unless-something-says-otherwise
  (multiple-value-bind (world policy) (conventional-world)
    (is (c:empty-pane-p (p:make-workspace policy world 3))
        "DESIGN D19's starting state: a place with nothing in it")))

(test new-workspace-can-be-a-function-of-the-world-and-the-index
  (multiple-value-bind (world policy) (conventional-world)
    (let ((r::*world* world)
          (p:*policy* policy)
          (p:*new-workspace*
            (lambda (world index)
              (declare (ignore world))
              (c:make-split :horizontal
                            (list (c:make-leaf) (c:make-leaf) (c:make-leaf))
                            (list 1 1 index)))))
      (r::workspace 3)
      (is (equal '(:h (:leaf nil) (:leaf nil) (:leaf nil))
                 (shape (c:child-at (c:world-root world) 2)))
          "the workspace is whatever the option returned")
      (is (equal '(1 1 2) (c:weights (c:child-at (c:world-root world) 2)))
          "and it was told which workspace it was making"))))

(test a-broken-new-workspace-function-costs-a-log-line-and-not-the-switch
  ;; This runs on the path that creates the workspace you are in the middle of
  ;; switching to.  A bad lambda in a configuration file must not be able to
  ;; leave you nowhere.
  (multiple-value-bind (world policy) (conventional-world)
    (let ((r::*world* world)
          (p:*policy* policy)
          (p:*new-workspace* (lambda (world index)
                               (declare (ignore world index))
                               (error "deliberately broken"))))
      (finishes (r::workspace 2))
      (is (c:empty-pane-p (c:child-at (c:world-root world) 1))
          "the shipped answer, rather than a hole in the workspace list")
      (is (equal '(1) (c:world-cursor world))
          "and the switch happened"))))

(test a-window-sent-past-the-end-of-the-workspace-list-lands-in-one-place-only
  ;; The duplication bug this command used to have, checked at the far end of
  ;; the growth path rather than the near one.
  (multiple-value-bind (world policy) (conventional-world)
    (let ((r::*world* world) (p:*policy* policy))
      (p:on-window-open policy world (win "a"))
      (p:on-window-open policy world (win "b"))
      (r::send-to-workspace 6)
      (is (= 6 (c:container-count (c:world-root world))))
      (is (equal '("a" "b")
                 (sort (mapcar #'c:window-app-id
                               (c:node-windows (c:world-root world)))
                       #'string<))
          "two windows in the world, not three"))))

;;; ==================================================================
;;; LAYOUT UNDO
;;; ==================================================================
;;;
;;; Nearly free, given the design that enables it — every surgery function
;;; already returns a new root and COPY-NODE already makes a structural copy —
;;; and impossible before COPY-NODE became a generic, because the old TYPECASE
;;; version returned an empty container for any kind it did not know.  An undo
;;; ring built on that would have destroyed a lattice on every press.

(test undo-and-redo-walk-the-layout-back-and-forward
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (dolist (app '("a" "b")) (p:on-window-open pol world (win app)))
    (let ((before (shape (c:world-root world)))
          (snapshot (funcall (find-symbol "SNAPSHOT-LAYOUT" r) "split")))
      ;; Change the tree, then record what it was.
      (p:on-window-open pol world (win "c"))
      (funcall (find-symbol "RECORD-UNDO" r) snapshot)
      (is (= 1 (length (funcall (find-symbol "UNDO-RING" r))))
          "a change that changed something is on the ring")
      (funcall (find-symbol "UNDO" r))
      (is (equal before (shape (c:world-root world)))
          "and undo puts the tree back exactly")
      (funcall (find-symbol "REDO" r))
      (is (= 3 (length (c:node-windows (c:world-root world))))
          "and redo takes you forward again"))))

(test undo-records-nothing-when-nothing-changed
  ;; The signature test, and it is what keeps the ring meaningful: plenty of
  ;; verbs decline to act -- MOVE at the edge of the world, TAB with no sibling
  ;; -- and without this, pressing an inert key ten times would fill the ring
  ;; with ten identical trees and undo would appear not to work.
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (setf (c:prop world :undo-ring) '())
    (p:on-window-open pol world (win "a"))
    (let ((snapshot (funcall (find-symbol "SNAPSHOT-LAYOUT" r) "nothing")))
      ;; A pure focus move: the cursor is not part of the signature.
      (p:move-cursor pol world :right)
      (funcall (find-symbol "RECORD-UNDO" r) snapshot)
      (is (null (funcall (find-symbol "UNDO-RING" r)))
          "moving the cursor is not a layout change"))))

(test undo-drops-windows-that-closed-while-it-was-waiting
  ;; An undone tree points at the same live windows -- there is only ever one
  ;; of those -- but a window that closed in the meantime is gone, and putting
  ;; a dead one back would leave a pane nothing can ever fill or focus.
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME"))
         (doomed (win "doomed")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (p:on-window-open pol world doomed)
    (let ((snapshot (funcall (find-symbol "SNAPSHOT-LAYOUT" r) "before")))
      (setf (c:window-live-p doomed) nil)
      (funcall (find-symbol "PRUNE-DEAD-WINDOWS" r)
               (funcall (find-symbol "LAYOUT-SNAPSHOT-ROOT" r) snapshot))
      (is (null (c:node-windows
                 (funcall (find-symbol "LAYOUT-SNAPSHOT-ROOT" r) snapshot)))
          "the dead window is not put back"))))

;;; ==================================================================
;;; THE CONTROL SOCKET'S FRAMING
;;; ==================================================================

(test an-answer-is-always-one-line
  ;; The wire is one form in, one line out.  Two things produce embedded
  ;; newlines without being asked: a condition report written as a paragraph,
  ;; which the good ones here are, and the pretty printer wrapping a long list.
  ;; Either truncates the answer at a client that reads with READ-LINE -- and
  ;; the half that is lost is the half saying what to do instead.
  (let* ((r (find-package "LATTICEWM/RUNTIME"))
         (one-line (find-symbol "ONE-LINE" r))
         (restore (find-symbol "RESTORE-NEWLINES" r)))
    (dolist (text (list "plain"
                        (format nil "two~%lines")
                        (format nil "a backslash \\ and a ~%newline")
                        (format nil "~%~%")
                        "trailing backslash \\"))
      (let ((encoded (funcall one-line text)))
        (is (null (find #\Newline encoded))
            "~s encodes to a single line" text)
        (is (string= (remove #\Return text) (funcall restore encoded))
            "and decodes back to exactly itself")))))
