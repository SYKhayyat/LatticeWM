;;;; tests/test-workspace-nav.lisp --- What the end of the workspace stack is (#1).
;;;;
;;;; "Infinite workspaces" shipped the day the workspace stack was a container:
;;;; `M-x workspace 40' has always created what it needed.  What #1 found was
;;;; that a *relative* step treated the end of the stack as a wall and wrapped,
;;;; so the key you press to go right came back to the left and the desktop read
;;;; as smaller than it is.  Both answers are correct and they are about
;;;; different desks, so the answer is an option, checked here at the one seam
;;;; both the switching verb and the sending verb read.

(in-package #:latticewm/tests)
(in-suite workspace-nav)

(defmacro with-workspaces (count &body body)
  "Run BODY in a world of COUNT workspaces on the first, the cursor in its
first pane, and a window called `a' under it.

The window is built into the tree rather than opened, because a leaf workspace
holding a window has no *pane path* for a send to name -- the pane is the
workspace.  The pane path is what the sending tests are about.

The nav option is process state, so it is put back afterwards."
  `(unwind-protect
        (let* ((window (win "a"))
               (world (c:make-world
                       :root (c:make-stack
                              (cons (c:make-split :horizontal
                                                  (list (c:make-leaf window)
                                                        (c:make-leaf)))
                                    (loop repeat (1- ,count) collect (c:make-leaf)))
                              0)))
               (r:*world* world)
               (p:*policy* (policy))
               (p:*seat* nil)
               (p:*echo-message* nil)
               (p:*workspace-nav* :wrap))
          (setf (c:world-cursor world) '(0 0))
          ,@body)
     (setf p:*workspace-nav* :wrap)))

(defun stack-count ()
  (c:container-count (c:world-workspaces r:*world*)))

(defun stack-selection ()
  (c:container-selection (c:world-workspaces r:*world*)))

;;; ---------------------------------------------------------------- the seam

(test a-step-inside-the-stack-lands-where-it-says
  (with-workspaces 3
    (multiple-value-bind (index grow) (r::workspace-step-target
                                       (c:world-workspaces r:*world*) 1)
      (is (= 1 index))
      (is (not grow) "the middle of the stack is not an edge"))))

(test wrapping-is-the-shipped-answer
  "It is what i3 does and it was the only answer this program had before #1, so
it stays the default: an option that changed behaviour on upgrade would be a
surprise dressed as a feature."
  (is (eq :wrap p:*workspace-nav*)))

(test a-step-off-the-end-wraps-when-the-option-says-so
  (with-workspaces 2
    (multiple-value-bind (index grow) (r::workspace-step-target
                                       (c:world-workspaces r:*world*) 1)
      (declare (ignore grow))
      (is (= 1 index) "one step from the first of two is the second"))
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (multiple-value-bind (index grow) (r::workspace-step-target
                                       (c:world-workspaces r:*world*) 1)
      (is (= 0 index) "and one step from the last is the first")
      (is (not grow)))))

(test a-step-off-the-end-grows-when-the-option-says-so
  (with-workspaces 2
    (setf p:*workspace-nav* :grow)
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (multiple-value-bind (index grow) (r::workspace-step-target
                                       (c:world-workspaces r:*world*) 1)
      (is (= 2 index) "it asks for the workspace after the last one")
      (is-true grow "and says it has to be made"))))

(test stepping-back-from-the-first-wraps-whatever-the-option-says
  "There is nothing behind workspace one, and a workspace numbered zero would undo
the one thing every number in this program agrees about."
  (with-workspaces 3
    (setf p:*workspace-nav* :grow)
    (multiple-value-bind (index grow) (r::workspace-step-target
                                       (c:world-workspaces r:*world*) -1)
      (is (= 2 index) "the last workspace")
      (is (not grow) "and nothing was made"))))

(test anything-that-is-not-grow-wraps
  "A config file writes the option, so a typo has to leave the desktop behaving
as it did rather than stopping every relative step dead."
  (dolist (value '(:grows "grow" nil :Wrap))
    (setf p:*workspace-nav* value)
    (is (not (p:grow-workspace-nav-p)) (format nil "~s is not :grow" value))))

;;; --------------------------------------------------------------- the verbs

(test next-workspace-grows-at-the-end-and-lands-on-the-new-one
  (with-workspaces 2
    (setf p:*workspace-nav* :grow)
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (r:next-workspace)
    (is (= 3 (stack-count)) "a third workspace exists now")
    (is (= 2 (stack-selection)) "and the cursor is on it")))

(test next-workspace-wraps-under-the-default
  (with-workspaces 2
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (r:next-workspace)
    (is (= 2 (stack-count)) "nothing was made")
    (is (= 0 (stack-selection)) "and it came back round")))

(test previous-workspace-wraps-at-the-first-even-when-growing
  (with-workspaces 3
    (setf p:*workspace-nav* :grow)
    (r:previous-workspace)
    (is (= 3 (stack-count)) "nothing was made behind workspace one")
    (is (= 2 (stack-selection)) "and it wrapped to the last")))

;;; ---------------------------------------------------------- sending a window

(defun windows-in (index)
  "The app ids of the windows in workspace INDEX."
  (mapcar #'c:window-app-id
          (c:node-windows (c:child-at (c:world-workspaces r:*world*) index))))

(test a-pane-can-be-sent-to-the-next-workspace
  (with-workspaces 2
    (is (equal '("a") (windows-in 0)) "it starts on the first workspace")
    (r:send-next-workspace)
    (is (equal '("a") (windows-in 1)) "and the relative send put it on the second")))

(test sending-grows-the-stack-when-the-option-says-so
  (with-workspaces 2
    (setf p:*workspace-nav* :grow)
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (r:send-next-workspace)
    (is (= 3 (stack-count)) "a workspace was made for it to land in")
    (is (equal '("a") (windows-in 2)))))

(test sending-wraps-under-the-default
  (with-workspaces 2
    (setf (c:container-selection (c:world-workspaces r:*world*)) 1)
    (r:send-next-workspace)
    (is (= 2 (stack-count)) "nothing was made")
    (is (equal '("a") (windows-in 0)) "and the window came round to the first")))

(test sending-takes-a-step
  (with-workspaces 3
    (r:send-next-workspace :step 2)
    (is (equal '("a") (windows-in 2)) "two workspaces along, not one")))

(test the-commands-exist
  (dolist (name '("next-workspace" "previous-workspace"
                  "send-next-workspace" "send-previous-workspace"))
    (is-true (p:find-command name) name)))

(test the-nav-option-is-registered-and-is-a-policy-option
  "It is a layout decision, not a runtime fact, so it lives with the other
layout options -- and a config file writes it."
  (let ((row (find :workspace-nav (p:all-options) :key (lambda (row) (first row)))))
    (is-true row)
    (is (eq :wrap (third row)) "wrapping is what an upgrade gets")))

;;; ------------------------------------------------ what a look is, not a move
;;;
;;; #5 asked for one thing worth keeping from a `layout recording' module: an
;;; exact, tested decision about which commands a recorder must not replay.  It
;;; lives in the policy now (P:*VIEW-COMMANDS*), because it is a fact about the
;;; program and not about the recorder that asked first.

(test a-look-is-not-a-move
  (dolist (name '("zoom-in" "zoom-out" "zoom-reset" "pan" "undo" "redo"
                  "repeat" "run-command-by-name"))
    (is-true (p:view-command-p name) name)))

(test a-move-is-recorded-because-a-replay-without-it-puts-the-wrong-window-on-top
  (dolist (name '("focus" "workspace" "split" "send-to-workspace" "swap"
                  "next-window" "move-window"))
    (is (not (p:view-command-p name)) name)))

(test an-unknown-or-unstringy-name-is-not-a-view-command
  "The answer for anything nobody has classified is `this is a move', which is
the safe direction: a replay that repeats one command too many is repairable, a
replay that silently drops a window is not."
  (is (not (p:view-command-p "no-such-command")))
  (is (not (p:view-command-p nil)))
  (is (not (p:view-command-p :zoom-in)) "a bare keyword is not a name"))
