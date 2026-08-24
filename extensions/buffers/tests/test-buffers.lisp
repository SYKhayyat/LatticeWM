;;;; tests/test-buffers.lisp --- The registry, the recall, the jump.

(defpackage #:buffers/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:bf #:buffers))
  (:export #:run-all))

(in-package #:buffers/tests)

(def-suite buffers :description "Named windows and pane-as-view switching.")
(in-suite buffers)

(t*:register-extension-suite "BUFFERS/TESTS" "BUFFERS")

(defun run-all ()
  "Run the BUFFERS suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'buffers)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defmacro with-world (&body body)
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy))
         (r:*undo-coalesce-seconds* nil))
     ,@body))

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))

(defun window-at (path &optional (world r:*world*))
  (let ((leaf (c:resolve-path (c:world-root world) path)))
    (and leaf (c:leaf-window leaf))))

;;; ================================================================ tests

(test naming-registers-and-renaming-does-not-orphan
  (with-world
    (open-windows 2)
    (let ((win (window-at '(0 0))))
      (is (equal "chat" (bf:name-window win "chat")))
      (is (eq win (bf:window-named "chat")))
      (is (equal '("chat") (bf:buffer-names)))
      ;; Renaming the same window must not leave a stale entry behind.
      (is (equal "mail" (bf:name-window win "mail")))
      (is-false (bf:window-named "chat"))
      (is (eq win (bf:window-named "mail"))))))

(test auto-names-derive-from-the-app-id-and-stay-unique
  (with-world
    (open-windows 2)
    (let ((a (window-at '(0 0)))
          (b (window-at '(0 1))))
      (setf (c:window-app-id a) "foot")
      (setf (c:window-app-id b) "foot")
      (let ((first-name (bf:name-window a)))
        (is (string= "foot" first-name))
        (is (string= "foot<1>" (bf:name-window b))
            "the second foot is numbered, not silently overwritten")))))

(test a-named-window-that-closes-forgets-itself
  "The hook keeps the registry honest: a name that answers with a dead window
is worse than no name at all."
  (with-world
    (open-windows 1)
    ;; ONE window means no split under the workspace: the leaf is at (0),
    ;; not (0 0), and asking for (0 0) answers NIL -- which is exactly how a
    ;; test can pass NIL where a window belongs without noticing.
    (let ((win (window-at '(0))))
      (bf:name-window win "gone")
      (r:run-hooks :window-closed win)
      (is-false (bf:window-named "gone")))))

(test recalling-a-minimized-buffer-into-the-current-pane
  "The core of the module: stand in one pane, ask for a buffer by name, and
the window arrives HERE -- while the window that was here goes to the
scratchpad remembering this pane as home."
  (with-world
    (open-windows 3)
    ;; Window 0 becomes "chat", then goes to the scratchpad.
    (let ((chat (window-at '(0 0))))
      (bf:name-window chat "chat")
      (r:minimize-window chat)
      ;; Stand at the second pane and ask for chat.
      (setf (c:world-cursor r:*world*) '(0 1))
      (r:run-command "switch-to-buffer" "chat")
      (is (eq chat (window-at '(0 1))) "chat arrived in the pane we were in")
      (is (= 1 (length (c:world-scratchpad r:*world*)))
          "one window was displaced to the scratchpad")
      ;; And switch-and-switch-back restores both.
      (setf (c:world-cursor r:*world*) '(0 1))
      (r:run-command "switch-to-buffer" "app1")
      (is-false (find chat (c:world-scratchpad r:*world*)))
      (is (eq chat (window-at '(0 1)))
          "wait -- app1 was asked for"))))

(test switch-to-an-onscreen-buffer-jumps-and-moves-nothing
  "One window, one rectangle: asking for a buffer that is already visible
takes the KEYBOARD there.  The tree does not change."
  (with-world
    (open-windows 3)
    (bf:name-window (window-at '(0 2)) "far")
    (setf (c:world-cursor r:*world*) '(0 0))
    (let ((shape-before (t*::shape (c:world-root r:*world*))))
      (r:run-command "switch-to-buffer" "far")
      (is (c:path-equal '(0 2) (c:world-cursor r:*world*))
          "the cursor jumped to the pane showing it")
      (is (equal shape-before (t*::shape (c:world-root r:*world*)))
          "and nothing moved"))))

(test an-unknown-name-says-so-and-changes-nothing
  (with-world
    (open-windows 1)
    (let ((before (t*::shape (c:world-root r:*world*))))
      (finishes (r:run-command "switch-to-buffer" "nope"))
      (is (equal before (t*::shape (c:world-root r:*world*)))))))

(test undo-ignores-a-switch-by-default
  "*UNDO-INCLUDES-SWAPS* defaults to NIL: a switch is a change of view, so
after the settle point there is no new step on the ring.  With T, the same
switch records like any other tree change."
  (with-world
    (open-windows 3)
    (bf:name-window (window-at '(0 0)) "zero")
    (bf:name-window (window-at '(0 1)) "one")
    ;; Named while live; it is minimized below, so switching to it later
    ;; exercises the RECALL path rather than the jump path.
    (let ((third (window-at '(0 2))))
      (bf:name-window third "two")
      (r:minimize-window third))
    ;; Establish the baseline undo state.
    (r:note-layout-settled "baseline")
    (let ((ring-length
            (length (r:undo-ring))))
      (setf (c:world-cursor r:*world*) '(0 1))
      (r:run-command "switch-to-buffer" "zero")
      (r:note-layout-settled "baseline")
      (is (= ring-length (length (r:undo-ring)))
          "default: no step recorded for the switch")
      ;; Now flip the option and switch back; the swap is recorded.
      (setf bf:*undo-includes-swaps* t)
      (setf (c:world-cursor r:*world*) '(0 1))
      (r:run-command "switch-to-buffer" "two")
      (r:note-layout-settled "baseline")
      (is (= (1+ ring-length) (length (r:undo-ring)))
          "opt-in: the swap landed on the ring"))))

(test focus-follows-recall-nil-leaves-the-cursor-alone
  "Q1's other answer: a script that recalls a buffer while you work elsewhere
must not drag your keyboard across the screen."
  (with-world
    (open-windows 3)
    (bf:name-window (window-at '(0 0)) "chat")
    (r:minimize-window (window-at '(0 0)))
    (let ((bf:*focus-follows-recall* nil))
      (setf (c:world-cursor r:*world*) '(0 1))
      (r:run-command "switch-to-buffer" "chat")
      (is (c:path-equal '(0 1) (c:world-cursor r:*world*))
          "the cursor stayed where it was"))))

(test focus-follows-recall-t-brings-the-keyboard-along
  (with-world
    (open-windows 3)
    (bf:name-window (window-at '(0 0)) "chat")
    (r:minimize-window (window-at '(0 0)))
    (setf bf:*focus-follows-recall* t)
    (setf (c:world-cursor r:*world*) '(0 1))
    (r:run-command "switch-to-buffer" "chat")
    (let ((leaf-path (bf::leaf-holding-path r:*world*
                                            (bf:window-named "chat"))))
      (is (not (null leaf-path)))
      (is (c:path-equal leaf-path (c:world-cursor r:*world*))
          "the cursor followed the recalled window"))))

(test two-panes-never-show-one-window
  "Q3's ruling, enforced by construction: every entry point either moves the
cursor or swaps contents, so a second pane showing the same window is not a
state the module can produce."
  (with-world
    (open-windows 3)
    (bf:name-window (window-at '(0 0)) "solo")
    (r:run-command "switch-to-buffer" "solo")     ; already on screen: jump
    (r:run-command "switch-to-buffer" "solo")     ; idempotent
    (let ((holders 0))
      ;; Walk every leaf; exactly one may hold the named window.
      (labels ((count-in (node)
                 (typecase node
                   (c:leaf (when (eq (c:leaf-window node)
                                     (bf:window-named "solo"))
                             (incf holders)))
                   (c:container (mapc #'count-in
                                      (mapcar (lambda (a) (c:child-at node a))
                                              (c:container-addresses node)))))))
        (count-in (c:world-root r:*world*)))
      (is (= 1 holders)))))
