;;;; tests/test-kbd-overview.lisp --- The keyboard is the overview.

(defpackage #:kbd-overview/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ko #:kbd-overview))
  (:export #:run-all))

(in-package #:kbd-overview/tests)

(def-suite kbd-overview :description "Zoom out to a keyboard of windows.")
(in-suite kbd-overview)

(t*:register-extension-suite "KBD-OVERVIEW/TESTS" "KBD-OVERVIEW")

(defun run-all ()
  "Run the KBD-OVERVIEW suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'kbd-overview)))
    (explain! results)
    (values (results-status results) (length results))))

(defun make-window (app-id)
  (make-instance 'c:window :app-id app-id
                           :identifier (symbol-name (gensym))))

(defmacro with-overview (&body body)
  `(let ((ko::*active* nil)
         (ko::*letter-layout* "asdfghjklqwertyuiopzxcvbnm")
         (ko::*windows-function* nil)
         (ko::*marked* '())
         (ko::*saved-workspaces* '())
         (ko::*assignments* '())
         (ko::*entry-index* 0)
         (p:*pending-keymap* nil)
         (r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     ,@body))

(defun ensure-workspace-slot (stack index)
  (loop while (<= (c:container-count stack) index)
        do (c:insert-child stack (c:container-count stack) (c:make-leaf))))

(defun put-window-at (index window)
  "Put WINDOW into workspace INDEX as its whole content."
  (let ((stack (c:world-workspaces r:*world*)))
    (ensure-workspace-slot stack index)
    (c:remove-child stack index)
    (c:insert-child stack index (c:make-leaf window))))

;;; ================================================================ tests

(test assign-letters-deals-home-row-first
  "The first window gets A -- home row, strongest finger -- then S, then D.
The order IS the policy."
  (let* ((windows (list (make-window "w1") (make-window "w2")
                        (make-window "w3")))
         (assignments (ko::assign-letters windows)))
    (is (= 3 (length assignments)))
    (is (equal "a" (car (first assignments))))
    (is (equal "s" (car (second assignments))))
    (is (equal "d" (car (third assignments))))))

(test rows-of-layout-cuts-three-qwerty-rows
  "The physical geometry: three rows of ten, nine and seven, with the home
row starting at A -- the shape of the thing your fingers know."
  (let ((rows (ko::rows-of-layout)))
    (is (= 3 (length rows)))
    (is (= 10 (length (first rows))))
    (is (= 9 (length (second rows))))
    (is (= 7 (length (third rows))))
    (is (equal #\a (elt (second rows) 0)) "the second row starts at A")))

(test build-keyboard-tree-holds-every-window
  "Twelve assigned windows produce twelve panes arranged as rows -- and no
keyboard-shaped graveyard of empty panes for letters nobody needed."
  (let* ((windows (loop repeat 12 collect (make-window "app")))
         (tree (ko::build-keyboard-tree (ko::assign-letters windows))))
    (is (= 12 (length (c:node-leaves tree))))
    (is (= 12 (length (c:node-windows tree))))))

(test enter-gathers-all-windows-onto-the-entry-workspace
  "Zooming out pulls every tiled window onto the workspace you started on,
arranged under letters -- including windows from other workspaces, because
GO has to be able to reach them."
  (with-overview
    (let ((w1 (make-window "editor"))
          (w2 (make-window "mail")))
      ;; Two windows on two different workspaces; entry is ws zero.
      (put-window-at 1 w1)
      (put-window-at 2 w2)
      (setf ko::*windows-function* (lambda () (list w2 w1)))
      (ko:enter-overview)
      ;; Both windows are now panes on the ENTRY workspace.
      (is (= 2 (length (c:node-windows (c:child-at
                                        (c:world-workspaces r:*world*) 0)))))
      ;; Their home workspaces were swapped out -- empty for the moment.
      (is (= 0 (length (c:node-windows (c:child-at
                                        (c:world-workspaces r:*world*) 1)))))
      (is (= 0 (length (c:node-windows (c:child-at
                                        (c:world-workspaces r:*world*) 2)))))
      (is (= 3 (length ko::*saved-workspaces*)) "all three originals saved")
      (is-true (ko:active-p))
      ;; And the letters are dealt.
      (is (= 2 (length (ko:assignments)))))))

(test exit-restores-the-exact-prior-trees
  "Snap-back swaps the ORIGINAL nodes back by reference: every window ends
on its own workspace again, and the entry workspace is as empty as it was."
  (with-overview
    (let ((w1 (make-window "editor")))
      (put-window-at 1 w1)
      (setf ko::*windows-function* (lambda () (list w1)))
      ;; Sanity: the window really did move to the entry workspace...
      (ko:enter-overview)
      (is (member w1 (c:node-windows (c:child-at
                                      (c:world-workspaces r:*world*) 0))))
      ;; ...and snap-back puts it home again.
      (ko:exit-overview)
      (is (= 1 (length (c:node-windows (c:child-at
                                        (c:world-workspaces r:*world*) 1)))))
      (is (= 0 (length (c:node-windows (c:child-at
                                        (c:world-workspaces r:*world*) 0)))))
      (is-false (ko:active-p)))))

(test toggle-arms-and-disarms-the-mode
  "TOGGLE enters, building and arming the letter keymap; the second call
exits.  The armed map binds each letter to a closure -- which is what makes
per-window GO work without any new input plumbing."
  (with-overview
    (let ((w1 (make-window "editor")))
      (put-window-at 1 w1)
      (setf ko::*windows-function* (lambda () (list w1)))
      ;; Enter.
      (is-true (ko:toggle-kbd-overview))
      (is-true (ko:active-p))
      (is-true p:*pending-keymap* "the overview keymap is armed")
      (is-true (functionp (r:lookup-key p:*pending-keymap* (r:kbd "a"))))
      ;; Toggle again: exit.
      (is-false (ko:toggle-kbd-overview))
      (is-false (ko:active-p)))))
