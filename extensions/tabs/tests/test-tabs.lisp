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
