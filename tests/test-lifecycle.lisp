;;;; tests/test-lifecycle.lisp --- Spawning, closing, floating, minimizing.

(in-package #:latticewm/tests)
(in-suite lifecycle)

(defun fresh-world ()
  "A world in its shipped starting shape: one workspace, one empty pane."
  (c:make-world))

;;; Two policies an extension author might really write, defined here rather
;;; than installed onto CONVENTIONAL-POLICY by a test that then puts the
;;; shipped method back.  A test that restores by re-DEFMETHODing a hand-copied
;;; body leaves a frozen duplicate of the shipped algorithm on a *more specific*
;;; class for the rest of the image, and every later test dispatches through it
;;; -- which is the exact mistake FINDINGS.org core edit 3 exists to prevent.
;;; A subclass costs one line and cannot do that.

(defclass floats-below-policy (p:conventional-policy) ()
  (:documentation "Floats render underneath the tiled windows."))

(defmethod p:render-order ((policy floats-below-policy) placements)
  (stable-sort (copy-list placements) #'<
               :key (lambda (placement)
                      (let ((node (first placement)))
                        (if (and (typep node 'c:leaf)
                                 (c:leaf-window node)
                                 (c:window-floating-p (c:leaf-window node)))
                            0
                            1)))))

(defclass sloppy-focus-policy (p:conventional-policy) ()
  (:documentation
   "Keyboard focus stays on the last window rather than clearing over an empty
pane -- what most tiling window managers do, and what D18 deliberately does
not.  It is the obvious thing somebody will want back."))

(defmethod p:focus-target ((policy sloppy-focus-policy) world)
  (or (call-next-method) (c:prop world :last-focused)))

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

(test render-order-decides-the-whole-list-and-not-half-of-it
  "THE FLOAT CLAUSE OF THE SHIPPED METHOD USED TO BE UNREACHABLE.

Floats are deliberately not in the tree, so they were never in the PLACEMENTS
this generic was handed: the runtime asked it about the tiled half and then
appended the floats itself, above everything, unconditionally.  A policy that
wanted a float *below* a tiled window wrote a method, watched nothing happen,
and had every reason to conclude the generic was broken rather than the caller.

So the assertion is not `floats end up on top' — the test above already says
that.  It is that a method saying otherwise is obeyed, which is the property
that was false."
  (let* ((tiled (leaf-with "tiled"))
         (floated (leaf-with "floated"))
         (placements (list (list tiled '(0) nil t)
                           (list floated nil nil t))))
    (setf (c:window-floating-p (c:leaf-window floated)) t)
    (let ((ordered (p:render-order (policy) placements)))
      (is (= 2 (length ordered))
          "a float handed in as a placement must come back as one")
      (is (eq floated (first (second ordered)))))
    ;; And now a policy that wants its floats underneath.  One method.
    (let ((below (make-instance 'floats-below-policy)))
      (let ((ordered (p:render-order below placements)))
        (is (eq floated (first (first ordered)))
            "the policy put floats at the bottom and was overruled")
        (is (eq tiled (first (second ordered))))))))

;;; ------------------------------------------------------------ D18, as policy

(test focus-target-is-the-shipped-rule-and-is-a-decision
  "D18 IS THE IDEA THE README ASKS YOU TO READ FIRST AND IT WAS A COND.

Focus is a place; Wayland keyboard focus is *derived* from where the cursor
rests.  That derivation lived inline in runtime/windows.lisp, so a policy could
change where the cursor went, what it looked like, and what happened after it
moved -- and could not change what focus meant.  The one idea the project leads
with was the one decision no method could reach.

Two assertions, and the second is the one that was false: the shipped rule is
still the shipped rule, and something else is now writable."
  (let ((world (fresh-world)) (pol (policy)))
    ;; A fresh world is one empty pane, so the honest answer is `nothing'.
    (is (null (p:focus-target pol world))
        "an empty pane must clear the keyboard, not leave it where it was")
    (p:on-window-open pol world (win "emacs"))
    (is (equal "emacs" (c:window-app-id (p:focus-target pol world))))
    ;; A float takes the keyboard from the cursor's pane, because it is on top
    ;; and is what the user is looking at.
    (let* ((floated (win "dialog"))
           (float (make-instance 'c:floating-window :window floated
                                                    :rect (c:make-rect 0 0 10 10))))
      (push float (c:world-floats world))
      (setf (c:world-focused-float world) float)
      (is (eq floated (p:focus-target pol world))))))

(test sloppy-focus-is-one-method

  "The obvious other rule, written the way an extension author would write it."
  (let ((world (fresh-world))
        (sloppy (make-instance 'sloppy-focus-policy))
        (remembered (win "editor")))
    (setf (c:prop world :last-focused) remembered)
    (is (null (p:focus-target (policy) world))
        "the shipped policy clears over an empty pane")
    (is (eq remembered (p:focus-target sloppy world))
        "and a policy that does not want that says so in one method")))

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
;;; THE HANGUP NOTHING REPORTED
;;; ==================================================================
;;;
;;; Kill the compositor and the window manager used to spin a core for as long
;;; as the machine was up: the socket becomes readable at end of file, the wait
;;; returns instantly for ever, and wayflan cannot say whether the zero bytes it
;;; read mean `gone' or `not yet'.  poll(2) can, and R::CONNECTION-HUNG-UP-P is
;;; the question.
;;;
;;; A pipe with its writer closed, because it is the same fd state and a unit
;;; test can make one in two lines.  The socket case is measured against a real
;;; river, killed on purpose, and written up in FINDINGS.org.

(test a-descriptor-whose-other-end-is-gone-is-recognised
  (multiple-value-bind (read-end write-end) (sb-posix:pipe)
    (unwind-protect
         (progn
           (is (null (r::connection-hung-up-p read-end))
               "an open descriptor with nothing on it has not hung up, which
                is the case this runs in sixty times a second")
           (sb-posix:close write-end)
           (is (r::connection-hung-up-p read-end)
               "and a descriptor whose other end is gone has"))
      (ignore-errors (sb-posix:close read-end)))))

(test the-hangup-question-tolerates-being-asked-about-nothing
  (is (null (r::connection-hung-up-p nil))
      "DISPLAY-FD answers NIL when wayflan's internals move, and the loop still
       has to run"))

;;; ==================================================================
;;; THE WAKEUP THAT GOES MISSING
;;; ==================================================================
;;;
;;; A lost wakeup is not an error: the loop waits its full interval, wakes on
;;; the timeout, drains the queue and carries on, and what the user sees is a
;;; screen that was stale for half a minute and is now correct.  It has cost
;;; this project two separate findings and the second was reached "by the one
;;; path the first fix did not cover".

(test a-pending-wakeup-is-taken-once-and-only-once
  (let ((r::*wakeup-pending* t))
    (is (r::consume-wakeup) "the first caller finds it")
    (is (null r::*wakeup-pending*) "and takes it")
    (is (null (r::consume-wakeup)) "so the second finds nothing")))

(test a-wait-that-ran-its-whole-length-with-work-queued-says-so
  (let ((p::*notes-said* (make-hash-table :test #'eq))
        (r::*wm-thread-queue* (list (lambda () nil))))
    (r::note-lost-wakeup (- (get-internal-real-time)
                            (* 2 r::*poll-interval*
                               internal-time-units-per-second)))
    (is (gethash :lost-wakeup p::*notes-said*)
        "waiting the whole interval with something already in the queue is the
         signature, because whatever put it there was supposed to wake us")))

(test an-idle-wait-and-a-short-one-say-nothing
  ;; Both halves of the test matter: waiting the full interval is *normal* on a
  ;; desktop nobody is touching, and a queue is normal on a busy one.  Only the
  ;; two together mean anything, and a diagnostic that fires on either alone is
  ;; one nobody will read by the time it matters.
  (let ((p::*notes-said* (make-hash-table :test #'eq)))
    (let ((r::*wm-thread-queue* '()))
      (r::note-lost-wakeup (- (get-internal-real-time)
                              (* 2 r::*poll-interval*
                                 internal-time-units-per-second)))
      (is (null (gethash :lost-wakeup p::*notes-said*))
          "a full wait with nothing queued is an idle desktop"))
    (let ((r::*wm-thread-queue* (list (lambda () nil))))
      (r::note-lost-wakeup (get-internal-real-time))
      (is (null (gethash :lost-wakeup p::*notes-said*))
          "and work that arrived while we were awake is the ordinary case"))))

;;; ==================================================================
;;; TWO MONITORS, AND THE ONE WORKSPACE BETWEEN THEM
;;; ==================================================================
;;;
;;; MEASURED ON TWO REAL OUTPUTS, and invisible to everything that existed at
;;; the time.  ENSURE-WORKSPACES-FOR-OUTPUTS was called when an output appeared
;;; and nowhere else; restoring a saved layout replaces the workspace stack
;;; afterwards, so a layout saved on one monitor and reloaded on two left one
;;; workspace for two outputs.  OUTPUT-CONTENT clamps an index into range, both
;;; outputs clamped to the same one, and the second monitor went black while its
;;; status line reported `workspace 1/1, 0,0, 1 window'.
;;;
;;; Nothing could see it and the list of what looked is the finding: gate 14
;;; fires every hook during the integration run, and :LAYOUT-RESTORED fires
;;; there with *one* output, where a one-workspace stack is the right answer.
;;; The integration suite drives a headless river, whose backend announces one
;;; output.  The unit suite constructs worlds, and so constructs their outputs —
;;; which is what these tests are, pointed at the case nobody had constructed.

(defun two-monitor-world (workspaces &key (names '("WL-1" "WL-2")))
  "A world with WORKSPACES workspaces and one output per name."
  (let* ((stack (c:make-stack (leaves workspaces)))
         (world (c:make-world :root stack))
         (outputs (mapcar (lambda (name) (make-instance 'c:output :name name))
                          names)))
    (setf (c:world-outputs world) outputs)
    (values world stack outputs)))

(test a-layout-restored-onto-more-monitors-than-it-was-saved-with-does-not-collapse
  (multiple-value-bind (world stack outputs) (two-monitor-world 1)
    (destructuring-bind (left right) outputs
      (let ((policy (make-instance 'p:conventional-policy)))
        (let ((p:*policy* policy))
          ;; The state as it was actually measured: the props are right, and
          ;; there is one workspace for the two of them to be right about.
          (setf (c:prop left :workspace) 0
                (c:prop right :workspace) 1)
          (is (eq (p:output-content policy world left)
                  (p:output-content policy world right))
              "without the repair both outputs clamp onto the one workspace
               there is, which is the whole bug")
          (p:ensure-workspaces-for-outputs world)
          (is (= 2 (c:container-count stack))
              "the stack grows to cover the monitors that are actually there")
          (is (not (eq (p:output-content policy world left)
                       (p:output-content policy world right)))
              "and no two monitors are showing the same node"))))))

(test two-outputs-that-both-claim-one-workspace-are-moved-apart
  ;; The other half of the invariant, and the one the original lacked: the
  ;; stack is big enough and the *assignment* is wrong.  Reachable from a state
  ;; file written before this rule existed, and from an extension that writes
  ;; the property itself.
  (multiple-value-bind (world stack outputs) (two-monitor-world 4)
    (destructuring-bind (left right) outputs
      (setf (c:prop left :workspace) 2
            (c:prop right :workspace) 2)
      (let ((p:*policy* (make-instance 'p:conventional-policy)))
        (p:ensure-workspaces-for-outputs world)
        (is (= 4 (c:container-count stack))
            "nothing is created, because there were enough already")
        (is (= 2 (c:prop left :workspace))
            "the first output keeps what it had")
        (is (not (eql 2 (c:prop right :workspace)))
            "and the second is moved off it")
        (is (< -1 (c:prop right :workspace) 4)
            "onto a workspace that exists")))))

(test an-output-pointing-past-the-end-of-the-workspace-list-is-brought-back
  (multiple-value-bind (world stack outputs) (two-monitor-world 2)
    (declare (ignore stack))
    (destructuring-bind (left right) outputs
      (setf (c:prop left :workspace) 0
            (c:prop right :workspace) 9)
      (let ((p:*policy* (make-instance 'p:conventional-policy)))
        (p:ensure-workspaces-for-outputs world)
        (is (= 1 (c:prop right :workspace))
            "an index with nothing behind it is repaired rather than clamped
             onto whatever the last workspace happens to be")))))

(test the-invariant-does-nothing-at-all-when-nothing-is-wrong
  ;; It runs before every layout, so `cheap and idempotent' is part of the
  ;; contract rather than an implementation note.
  (multiple-value-bind (world stack outputs) (two-monitor-world 5)
    (destructuring-bind (left right) outputs
      (setf (c:prop left :workspace) 3
            (c:prop right :workspace) 1)
      (let ((p:*policy* (make-instance 'p:conventional-policy)))
        (dotimes (n 3) (p:ensure-workspaces-for-outputs world))
        (is (= 5 (c:container-count stack)) "no workspace was created")
        (is (= 3 (c:prop left :workspace)))
        (is (= 1 (c:prop right :workspace))
            "and nobody was moved")))))

(test switching-to-a-workspace-the-other-monitor-is-showing-trades-for-it
  ;; The third door, and no restore is involved: two monitors, ask for the
  ;; workspace the other one has, and both monitors used to end up on it.
  (multiple-value-bind (world stack outputs) (two-monitor-world 4)
    (destructuring-bind (left right) outputs
      (setf (c:prop left :workspace) 0
            (c:prop right :workspace) 2)
      (let ((r::*world* world) (p:*policy* (make-instance 'p:conventional-policy)))
        (r::show-workspace-on left 2)
        (is (= 2 (c:prop left :workspace))
            "the monitor you are looking at shows what you asked for")
        (is (= 0 (c:prop right :workspace))
            "and the other one takes the workspace you left, rather than
             mirroring you")
        (is (= 4 (c:container-count stack))
            "and nothing was created to do it")))))

(test trading-with-an-output-that-has-nothing-to-trade-leaves-the-invariant-to-fix-it
  (multiple-value-bind (world stack outputs) (two-monitor-world 3)
    (declare (ignore stack))
    (destructuring-bind (left right) outputs
      (setf (c:prop right :workspace) 1)
      (let ((r::*world* world) (p:*policy* (make-instance 'p:conventional-policy)))
        (r::show-workspace-on left 1)
        (is (= 1 (c:prop left :workspace)))
        (is (null (c:prop right :workspace))
            "the other output is left with nothing, which is a state that
             exists for as long as it takes to lay out")
        (p:ensure-workspaces-for-outputs world)
        (is (and (c:prop right :workspace)
                 (/= 1 (c:prop right :workspace)))
            "and the layout gives it one of its own before anything is drawn")))))

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

;;; UNDO IS TAKEN WHERE THE WORLD SETTLES, NOT WHERE A COMMAND RUNS, and these
;;; three checks are written against that rather than against RUN-COMMAND --
;;; which is the point of the change.  None of them runs a command.  Under the
;;; old mechanism, a change made without going through P:RUN-COMMAND produced
;;; no undo entry at all, and `Super+;', the control socket and SWANK are all
;;; exactly that.
(defun settle (label)
  "The settle point, called the way AFTER-COMMAND calls it."
  (funcall (find-symbol "NOTE-LAYOUT-SETTLED" (find-package "LATTICEWM/RUNTIME"))
           label))

(defun fresh-undo-state (world)
  "A world with no history and no baseline, as at the first settle."
  (setf (c:prop world :undo-ring) '()
        (c:prop world :redo-ring) '()
        (c:prop world :undo-baseline) nil))

(test undo-and-redo-walk-the-layout-back-and-forward
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (fresh-undo-state world)
    (dolist (app '("a" "b")) (p:on-window-open pol world (win app)))
    (settle "start")
    (let ((before (shape (c:world-root world))))
      ;; Change the tree with no command anywhere in sight, then settle.
      (p:on-window-open pol world (win "c"))
      (settle "split")
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
    (fresh-undo-state world)
    (p:on-window-open pol world (win "a"))
    (settle "start")
    ;; A pure focus move: the cursor is not part of the signature.
    (p:move-cursor pol world :right)
    (settle "move")
    (is (null (funcall (find-symbol "UNDO-RING" r)))
        "moving the cursor is not a layout change")))

(test a-settle-that-changed-nothing-copies-nothing
  ;; THE COST, ASSERTED RATHER THAN CLAIMED.  Undo used to deep-copy every
  ;; workspace -- all forty, if you had ever pressed `workspace 40' -- and
  ;; hash the result twice, on every arrow key, to record nothing.  Meanwhile
  ;; tools/image.lisp shrinks BYTES-CONSED-BETWEEN-GCS to 8 MB specifically
  ;; because "a GC pause during a keystroke is input latency, directly and
  ;; visibly": the one file that argues GC pressure matters was undermined by
  ;; the one that generated it per keystroke.
  ;;
  ;; EQ on the baseline is the observable form of "no copy was made".  A
  ;; settle that recorded something replaces the baseline object; one that did
  ;; not leaves it alone, and there is no other way for it to have copied.
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (fresh-undo-state world)
    (p:on-window-open pol world (win "a"))
    (settle "start")
    (let ((baseline (funcall (find-symbol "UNDO-BASELINE" r))))
      (is (not (null baseline)) "the first settle establishes a baseline")
      (dotimes (i 10)
        (p:move-cursor pol world :right)
        (p:move-cursor pol world :left)
        (settle "move"))
      (is (eq baseline (funcall (find-symbol "UNDO-BASELINE" r)))
          "twenty inert keystrokes copy the tree zero times")
      (p:on-window-open pol world (win "b"))
      (settle "open")
      (is (not (eq baseline (funcall (find-symbol "UNDO-BASELINE" r))))
          "and a real change does take a fresh one"))))

(test undo-covers-the-doors-run-command-never-reached
  ;; THE FINDING, STATED AS A TEST.  Undo was a wrapper on P:*COMMAND-WRAPPERS*
  ;; whose reach is exactly P:RUN-COMMAND, and none of EVAL-EXPRESSION,
  ;; EVALUATE-FOR-IPC or SWANK go through it -- so `Super+; (setf (c:world-root
  ;; *world*) (c:make-leaf))' destroyed a layout with no undo entry while
  ;; Super+h recorded a snapshot for a cursor move that changed nothing.
  ;;
  ;; This is what an eval through any of those three does: replace the root
  ;; outright, then settle.
  (let* ((world (fresh-world))
         (pol (policy))
         (r (find-package "LATTICEWM/RUNTIME")))
    (setf (symbol-value (find-symbol "*WORLD*" r)) world)
    (fresh-undo-state world)
    (dolist (app '("a" "b")) (p:on-window-open pol world (win app)))
    (settle "start")
    (let ((before (shape (c:world-root world))))
      (setf (c:world-root world) (c:make-leaf))
      (settle "eval")
      (is (= 1 (length (funcall (find-symbol "UNDO-RING" r))))
          "an eval that flattened the world is recoverable")
      (funcall (find-symbol "UNDO" r))
      (is (equal before (shape (c:world-root world)))
          "and undo gets the whole layout back"))))

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

;;; ==================================================================
;;; SMART GAPS — one pane has nothing to be separated from
;;; ==================================================================
;;;
;;; *SMART-GAPS* WAS REGISTERED, DOCUMENTED, EXPORTED AND READ BY NOTHING, from
;;; the commit that added it until the one these tests came with.  Gate 11 is
;;; what stops the next one; these are about the behaviour, and in particular
;;; about the three cases the option's original docstring got wrong.  It said
;;; "when a workspace holds exactly one window", and a promise nobody
;;; implements is a design claim nobody checks against the rest of the design.

(defun solo (policy node &optional (rect (c:make-rect 0 0 1000 500)))
  (p:solo-window policy node rect))

(test smart-gaps-sees-one-window-alone-on-a-workspace
  (let* ((pol (policy))
         (leaf (leaf-with "solo")))
    (is (eq (c:leaf-window leaf) (solo pol leaf))
        "a workspace that is one leaf holding one window")
    (is (eq (c:leaf-window leaf)
            (solo pol (c:make-stack (list leaf))))
        "and the same window through a container that places only it")))

(test smart-gaps-does-not-fire-on-an-empty-pane
  "THE CASE THE ORIGINAL WORDING MOST OBVIOUSLY COVERED AND MUST NOT.

A split holding one window and one empty pane holds exactly one window.
Dropping the borders there is precisely the failure D18 names: focus is a
*place*, an empty pane has no window to hang a border on, and an unmarked one
reads as a broken keyboard rather than as a place."
  (let* ((pol (policy))
         (window (leaf-with "only")))
    (is (null (solo pol (c:make-split :horizontal (list window (c:make-leaf)))))
        "one window and one empty pane is two panes")
    (is (null (solo pol (c:make-leaf)))
        "and an empty workspace has no window to be alone")))

(test smart-gaps-counts-panes-on-the-screen-not-windows-in-the-tree
  (let* ((pol (policy))
         (a (leaf-with "a"))
         (b (leaf-with "b"))
         (c (leaf-with "c")))
    (is (null (solo pol (c:make-split :horizontal (list a b))))
        "two tiled windows are two panes")
    (is (eq (c:leaf-window a) (solo pol (c:make-stack (list a b c) 0)))
        "three tabs are one pane -- that is what is on the screen")
    (is (eq (c:leaf-window c) (solo pol (c:make-stack (list a b c) 2)))
        "and it is whichever tab is selected, because LAYOUT-CHILDREN is asked
rather than a second copy of the rule being kept here")))

(test smart-gaps-is-off-when-the-option-is-off
  (let ((p:*smart-gaps* nil))
    (is (null (solo (policy) (leaf-with "solo")))
        "the one place the option is read")))

(test smart-gaps-drops-the-border-and-nothing-else-does
  (let* ((pol (policy))
         (leaf (leaf-with "solo"))
         (other (leaf-with "other"))
         (p:*border-width* 3))
    (is (= 3 (p:border-width pol leaf nil))
        "no relayout has happened, so there is no screen to be alone on")
    (let ((p:*solo-windows* (list (cons :an-output (c:leaf-window leaf)))))
      (is (= 0 (p:border-width pol leaf nil)))
      (is (= 3 (p:border-width pol other nil))
          "a window that is not the solo one keeps its border")
      (setf (c:prop (c:leaf-window leaf) :border-width) 5)
      (is (= 5 (p:border-width pol leaf nil))
          "a window rule is a narrower statement than a global and still wins"))))

(test smart-gaps-drops-the-screen-edge-gap-and-keeps-the-reserved-space
  (let* ((pol (policy))
         (output (make-instance 'c:output :rect (c:make-rect 0 0 1000 500)))
         (window (win "solo"))
         (p:*outer-gaps* 12))
    (let ((normal (p:outer-rect pol output))
          (solo (let ((p:*solo-windows* (list (cons output window))))
                  (p:outer-rect pol output))))
      ;; Asserted as a difference rather than against absolute numbers,
      ;; because RESERVED-SPACE runs the :RESERVE-SPACE hooks and the echo
      ;; area is on one of them.  The difference is the gap and only the gap,
      ;; which is the claim.
      (is (= 12 (- (c:rect-x normal) (c:rect-x solo))))
      (is (= 24 (- (c:rect-w solo) (c:rect-w normal))))
      (is (= (- (c:rect-h solo) 24) (c:rect-h normal))
          "the reserved strip is untouched: a status bar drew in it"))))

(test smart-gaps-needs-no-case-for-the-inner-gap
  "The claim GAPS' docstring makes, asserted rather than asserted-in-prose.

DIVIDE-RECT spends GAP once per *boundary between* children, so a container
with one child already spends none and a solo workspace has nothing for an
inner gap to sit between.  If that ever stops being true, the smart-gaps
implementation grows a third reader and this fails first."
  (let ((one (c:divide-rect (c:make-rect 0 0 100 50) :horizontal '(1) :gap 20)))
    (is (= 1 (length one)))
    (is (= 100 (c:rect-w (first one))))))

(test smart-gaps-sizes-the-window-for-the-border-it-actually-gets
  "THE BUG A SECOND COMPUTATION WOULD HAVE PRODUCED.

BORDER-WIDTH is asked twice per relayout -- by WINDOW-DIMENSIONS on the way
down, to size the window inside its pane, and by EMIT-BORDERS on the way out,
to send the border.  River draws borders *around* the content rectangle, so if
those two disagree the window overflows its pane by twice the difference.
That is why the runtime computes the solo set once and binds it around both
passes instead of asking twice."
  (let* ((pol (policy))
         (leaf (leaf-with "solo"))
         (rect (c:make-rect 0 0 100 50))
         (p:*border-width* 3))
    (let ((p:*solo-windows* (list (cons :an-output (c:leaf-window leaf)))))
      (multiple-value-bind (w h) (p:window-dimensions pol leaf rect)
        (is (= 100 w) "no border, so the window gets the whole pane")
        (is (= 50 h))
        (is (= 0 (p:border-width pol leaf nil))
            "and that is the same number the emitter will send")))))
