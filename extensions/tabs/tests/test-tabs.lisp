;;;; tests/test-tabs.lisp --- One pane, many windows, shown one at a time.

(defpackage #:tabs/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:tb #:tabs))
  (:export #:run-all))

(in-package #:tabs/tests)

(def-suite tabs :description "Tab groups inside a pane.")
(in-suite tabs)

(t*:register-extension-suite "TABS/TESTS" "TABS")

(defun run-all ()
  "Run the TABS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'tabs)))
    (explain! results)
    (values (results-status results) (length results))))

(defun make-window (app-id)
  (make-instance 'c:window :app-id app-id
                           :identifier (symbol-name (gensym))))

(defmacro with-tabs (&body body)
  `(let ((tb::*enabled* nil)
         (tb::*undo-includes-tab-switches* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy))
         (p:*hooks* (make-hash-table :test #'eq)))
     ;; A single workspace whose content is one leaf under the cursor --
     ;; the ordinary starting shape.
     (setf (c:world-cursor r:*world*) '(0))
     ,@body))

(defun focused-leaf ()
  ;; The runtime's own notion of the focused pane, via the exported
  ;; world accessor: cursor path resolved against the root.
  (c:world-leaf-at r:*world* (c:world-cursor r:*world*)))

;;; ================================================================ tests

(test tab-here-creates-a-group-with-the-visible-window
  "TAB-HERE turns the focused pane into a group of one."
  (with-tabs
    (let ((w1 (make-window "editor")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (is-true (tb:tab-here))
      (is (equal (list w1) (tb:group-of (focused-leaf))))
      ;; Confirming twice does not duplicate.
      (tb:tab-here)
      (is (= 1 (length (tb:group-of (focused-leaf))))))))

(test tab-add-pulls-a-tiled-window-from-elsewhere
  "Adding a window that lives on another workspace takes it out of its
tree and makes it the visible member here -- the old spot empties, the
other window there stands alone."
  (with-tabs
    (let ((w1 (make-window "editor"))
          (w2 (make-window "mail"))
          (stack (c:world-workspaces r:*world*)))
      (setf (c:leaf-window (focused-leaf)) w1)
      ;; Workspace two holds mail beside a third window.
      (loop while (< (c:container-count stack) 2)
            do (c:insert-child stack (c:container-count stack)
                               (c:make-leaf)))
      (c:remove-child stack 1)
      (c:insert-child stack 1
                      (c:make-split :horizontal
                                    (list (c:make-leaf w2)
                                          (c:make-leaf
                                           (make-window "spare")))
                                    nil))
      ;; The cursor stays on workspace one.
      (setf (c:world-cursor r:*world*) '(0))
      (let ((spare nil))
        (setf spare (c:node-windows (c:child-at stack 1)))
        (setf tb::*live-windows* (lambda () (append (list w2 w1) spare))))
      (tb:tab-here)
      (is (equal "mail" (tb:tab-add "mail")))
      ;; Mail is now visible HERE...
      (is (eq w2 (tb:visible-window (focused-leaf))))
      (is (= 2 (length (tb:group-of (focused-leaf)))))
      ;; ...and its old workspace kept the OTHER window only.
      (let ((ws2-windows (c:node-windows (c:child-at stack 1))))
        (is (= 1 (length ws2-windows)))
        (is (not (member w2 ws2-windows)))))))

(test next-and-prev-cycle-with-wraparound
  "Three tabs cycle forward and backward, wrapping at both ends."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (tb:tab-here)
      (tb:tab-add "two")
      (tb:tab-add "three")
      (is (eq w3 (tb:visible-window (focused-leaf))) "add shows the added")
      (tb:tab-next)
      (is (eq w1 (tb:visible-window (focused-leaf))) "wrapped to first")
      (tb:tab-prev)
      (is (eq w3 (tb:visible-window (focused-leaf))) "prev wraps back")
      (tb:tab-prev)
      (is (eq w2 (tb:visible-window (focused-leaf)))))))

(test switching-is-not-an-undo-step-by-default
  "With *UNDO-INCLUDES-TAB-SWITCHES* NIL, cycling leaves the undo ring
exactly as it was -- a change of view is not a change of arrangement."
  (with-tabs
    (let ((w1 (make-window "one")) (w2 (make-window "two")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (tb:tab-here)
      (tb:tab-add "two")
      (let ((before (length (r:undo-history))))
        (tb:tab-next)
        (tb:tab-prev)
        (is (= before (length (r:undo-history)))
            "no steps recorded for view switches")))))

(test closed-windows-are-pruned-from-groups
  "A window that dies stops being a tab; if it was the VISIBLE one, the
next survivor is promoted in its place."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (tb:tab-here)
      (tb:tab-add "two")
      (tb:tab-add "three")
      ;; W3 is visible; kill it.
      (setf (c:window-live-p w3) nil)
      (tb::note-window-closed w3)
      (is (= 2 (length (tb:group-of (focused-leaf)))))
      (is (eq w1 (tb:visible-window (focused-leaf)))
          "a survivor was promoted")
      ;; Kill everything else: the group dissolves.
      (setf (c:window-live-p w1) nil)
      (setf (c:window-live-p w2) nil)
      (tb::note-window-closed w1)
      (tb::note-window-closed w2)
      (is-false (tb:group-of (focused-leaf))
                "one window does not need tabs"))))

(test untab-pops-the-visible-window-beside
  "UNTAB puts the visible window back into the tree as an ordinary pane
next to the group, and promotes the next tab."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (tb:tab-here)
      (tb:tab-add "two")
      (tb:tab-add "three")           ; visible = three
      ;; UNTAB returns the window it popped -- the OBJECT, not a string.
      (is (eq w3 (tb:untab)))
      ;; Three is now a pane BESIDE the group's leaf.  The root is the
      ;; workspace STACK; the split lives one level down.
      (let* ((root (c:world-root r:*world*))
             (split (c:child-at root 0))
             (children (mapcar (lambda (address)
                                 (c:child-at split address))
                               (c:container-addresses split))))
        (is (= 2 (length children)) "split of group and popped pane")
        (let ((group-leaf (find-if (lambda (n)
                                     (and (typep n 'c:leaf)
                                          (tb:group-of n)))
                                   children)))
          (is-true group-leaf "the group survived")
          (is (= 2 (length (tb:group-of group-leaf)))
              "keeping its remaining tabs")))
      ;; Visible switched back to the next survivor.
      (is-false (eq w3 (tb:visible-window (focused-leaf)))))))

(test untab-on-single-pane-workspace-wraps-in-a-split
  "The group IS the whole workspace content: popping still works, by
wrapping both panes in a split rather than trying to insert into the
stack."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (setf tb::*live-windows* (lambda () (list w1 w2)))
      (tb:tab-here)
      (tb:tab-add "two")             ; visible = two
      (is (= 1 (length (c:node-leaves (c:world-root r:*world*)))))
      (tb:untab)
      ;; Now: split [group-leaf(w1), popped(w2)] -- two panes, one group.
      (is (= 2 (length (c:node-windows (c:world-root r:*world*)))))
      (is (= 1 (count-if (lambda (leaf) (tb:group-of leaf))
                         (c:node-leaves (c:world-root r:*world*))))
          "exactly one pane still holds the group"))))

;;; ============================================================== #36 join

(defun a-two-tab-pane (w1 w2)
  "Set up the focused pane as a group of W1 then W2, visible on W2.
Requires WITH-TABS and the live-windows pointer to be set by the caller."
  (setf (c:leaf-window (focused-leaf)) w1)
  (tb:tab-here)
  (tb:tab-add (c:window-app-id w2)))

(test tab-move-reorders-and-stays-on-the-window-you-moved
  "Moving a tab is arranging the list, not choosing a window: the visible
member stays visible, and its position in the group is what changed.  The
check that matters (the one a rotate-the-whole-selection bug would fail):
the member SET is unchanged and the selection is unchanged."
  (with-tabs
    (let ((w1 (make-window "one")) (w2 (make-window "two")))
      (setf tb::*live-windows* (lambda () (list w1 w2)))
      (a-two-tab-pane w1 w2)
      (tb:tab-move :backward)
      (is (equal (list w2 w1) (tb:group-of (focused-leaf)))
          "the visible tab moved back")
      (is (eq w2 (tb:visible-window (focused-leaf)))
          "still showing the one that moved")
      (tb:tab-move :forward)
      (is (equal (list w1 w2) (tb:group-of (focused-leaf)))
          "and forward returns it"))))

(test tab-move-wraps-around-and-says-where-it-landed
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three"))
          (p:*echo-message* nil))
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (a-two-tab-pane w1 w2)
      (tb:tab-add "three")
      ;; visible=three at position 3; move forward wraps to 1.
      (tb:tab-move :forward)
      (is (equal (list w3 w1 w2) (tb:group-of (focused-leaf)))
          "wrapped to the front")
      (is-true (search "tab 1" (car p:*echo-message*))
               "and reported its new place"))))

(test tab-move-to-places-the-visible-tab-at-an-absolute-position
  "An absolute place, counting from 1, and no wrapping: a number past the end
is a mistake and is said to be one."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three"))
          (p:*echo-message* nil))
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (a-two-tab-pane w1 w2)
      (tb:tab-add "three")          ; visible = three, at the end
      (tb:tab-move-to 1)
      (is (equal (list w3 w1 w2) (tb:group-of (focused-leaf)))
          "first in the group")
      (is (eq w3 (tb:visible-window (focused-leaf)))
          "and still the visible one")
      (tb:tab-move-to 99)
      (is (equal (list w3 w1 w2) (tb:group-of (focused-leaf)))
          "out of range changes nothing")
      (is-true (search "no tab 99" (car p:*echo-message*))
               "and says why"))))

(test tab-rename-gives-the-window-a-name-and-clears-it
  "The name sticks to the WINDOW, not the pane: it is the label prop, so
UNTAB moving it elsewhere takes the name with it, and an empty name clears."
  (with-tabs
    (let ((w1 (make-window "one")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (tb:tab-here)
      (tb:tab-rename "mail")
      (is (equal "mail" (c:prop w1 :label)))
      (tb:tab-rename " ")
      (is (null (c:prop w1 :label)) "an empty name clears it"))))

(test tab-send-moves-the-tab-into-the-named-group
  "The other half of TAB-ADD: push this tab out to a *named* destination.
The sending group keeps its remaining member and its selection; the
destination becomes a group of two and shows the newcomer.  Named by a
window rather than by coordinates, so it does not depend on the layout --
and `that terminal belongs over in that group' is the sentence."
  (with-tabs
    ;; LET*, not LET: the split's init form names W3, and in a parallel binding
    ;; list that is a reference to an outer (unbound) variable.
    (let* ((w1 (make-window "one"))
           (w2 (make-window "two"))
           (w3 (make-window "three"))
           (split (c:make-split :horizontal
                                (list (c:make-leaf)
                                      (c:make-leaf w3))
                                nil)))
      ;; One workspace, two panes: ours on the left, three on the right.
      (c:remove-child (c:world-workspaces r:*world*) 0)
      (c:insert-child (c:world-workspaces r:*world*) 0 split)
      (setf (c:world-cursor r:*world*) '(0 0))
      (setf (c:leaf-window (focused-leaf)) w1)
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (tb:tab-here)
      (tb:tab-add "two")            ; visible = two, group = (w1 w2)
      (is (= 2 (length (tb:group-of (focused-leaf)))))
      (tb:tab-send "three")
      ;; Our pane keeps one member and shows it.
      (is (= 1 (length (tb:group-of (focused-leaf))))
          "one member left here")
      (is (eq w1 (tb:visible-window (focused-leaf)))
          "showing the survivor, not an empty pane")
      ;; The destination pane is now a group of (three, two), showing two.
      (let ((destination (c:child-at split 1)))
        (is (typep destination 'c:leaf))
        (is (equal (list w3 w2) (tb:group-of destination))
            "the newcomer joined the destination group")
        (is (eq w2 (tb:visible-window destination))
            "and is what that pane shows")))))

(test tab-send-refuses-a-destination-inside-the-same-group
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (p:*echo-message* nil))
      (setf tb::*live-windows* (lambda () (list w1 w2)))
      (a-two-tab-pane w1 w2)
      (is (null (tb:tab-send "one")))
      (is (= 2 (length (tb:group-of (focused-leaf)))) "the group is untouched")
      (is-true (search "already in this group" (car p:*echo-message*))))))

;;; ========================================================= #39 persistence

(defun tabs-in-serialized (form)
  "The :TABS field the file has for FORM's first leaf, or NIL.

Read STRUCTURALLY rather than by searching the printed text: the printer here
is bounded by *PRINT-LEVEL* and *PRINT-LENGTH*, so a string search would pass or
fail depending on how deep the leaf happened to be."
  (labels ((walk (node)
             (cond
               ((and (consp node) (eq (first node) :leaf))
                (getf (rest node) :extra))
               ((consp node) (some #'walk node))
               (t nil))))
    (getf (walk form) :tabs)))

(test a-tab-group-survives-a-save-and-restore
  "The round trip that #39 asked for: serialize the tree the way the state file
does, deserialize it into a fresh world with an identifier->window index, and
the group comes back with its member order and its selection."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two"))
          (w3 (make-window "three")))
      (setf tb::*live-windows* (lambda () (list w1 w2 w3)))
      (a-two-tab-pane w1 w2)
      (tb:tab-add "three")          ; visible = three, group = (one two three)
      (is (equal (list w1 w2 w3) (tb:group-of (focused-leaf))))
      (let* ((form (r:serialize-node (c:world-root r:*world*)))
             ;; the file has no window objects in it -- only identifiers
             (index (make-hash-table :test #'equal)))
        (dolist (window (list w1 w2 w3))
          (setf (gethash (c:window-identifier window) index) window))
        (is (equal (list (c:window-identifier w1)
                         (c:window-identifier w2)
                         (c:window-identifier w3))
                   (tabs-in-serialized form))
            "the group is in the file at all -- it was not, before")
        (let ((rebuilt (r:read-node form index)))
          (let ((leaf (first (c:node-leaves rebuilt))))
            (is (equal (list w1 w2 w3) (tb:group-of leaf))
                "members and order restored")
            (is (eq w3 (tb:visible-window leaf))
                "and the tab that was showing is still the one showing")))))))

(test a-restored-group-drops-a-member-that-is-gone
  "A state file describes a moment, and one of the windows may have closed in
between.  The group comes back with what still exists; and a group cut down to
one member is not a group, which is the rule UNTAB already follows."
  (with-tabs
    (let ((w1 (make-window "one"))
          (w2 (make-window "two")))
      (setf tb::*live-windows* (lambda () (list w1 w2)))
      (a-two-tab-pane w1 w2)
      (let* ((form (r:serialize-node (c:world-root r:*world*)))
             ;; only ONE of the two identifiers is still a live window
             (index (make-hash-table :test #'equal)))
        (setf (gethash (c:window-identifier w2) index) w2)
        (let ((leaf (first (c:node-leaves (r:read-node form index)))))
          (is (eq w2 (tb:visible-window leaf)) "the survivor is showing")
          (is (null (tb:group-of leaf))
              "one window does not need tabs"))))))

(test a-file-without-tab-facts-still-loads
  "The forward-compatibility path, which is the one that matters to a user who
upgrades: an old file has no :EXTRA, and a leaf reads back as an ordinary pane."
  (with-tabs
    (let ((w1 (make-window "one")))
      (setf (c:leaf-window (focused-leaf)) w1)
      (tb:tab-here)                 ; a group of ONE is not written
      (let* ((form (r:serialize-node (c:world-root r:*world*)))
             (index (make-hash-table :test #'equal)))
        (setf (gethash (c:window-identifier w1) index) w1)
        (is (null (tabs-in-serialized form))
            "a one-member group is not worth a field")
        (let ((leaf (first (c:node-leaves (r:read-node form index)))))
          (is (eq w1 (tb:visible-window leaf)))
          (is (null (tb:group-of leaf))))))))
