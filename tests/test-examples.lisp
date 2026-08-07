;;;; tests/test-examples.lisp --- The shipped examples, actually run.
;;;;
;;;; PLAN.org Phase 4 sets the bar for these, and it is not "would a person
;;;; understand this":
;;;;
;;;;   "The test is not 'would a person understand this.'  It is *'could a
;;;;   cheap model produce a fourth one by pattern-matching on these three.'*
;;;;   If the answer is no, the surface is wrong and this is the last moment it
;;;;   can be fixed."
;;;;
;;;; Nothing here can check that.  What it can check is the precondition: that
;;;; the examples load, that they do what they claim, and that they keep doing
;;;; it as the core changes.  A worked example that has silently rotted is
;;;; worse than none, because it teaches a shape that no longer works.

(defpackage #:latticewm/tests/examples
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:u #:latticewm/user)
                    (#:t* #:latticewm/tests))
  (:export #:run-all #:examples))

(in-package #:latticewm/tests/examples)

(def-suite examples :description "The shipped worked extensions.")
(in-suite examples)

(defun run-all ()
  (let ((results (run 'examples)))
    (explain! results)
    (values (results-status results) (length results))))

(defun example-path (name)
  (merge-pathnames (format nil "examples/~a" name)
                   (asdf:system-source-directory "latticewm")))

(defmacro with-example ((name) &body body)
  "Load an example into a fresh world, run BODY, and clean up after it.

The cleanup matters: these examples define methods on the *shipped* policy
class, which is the honest thing for them to do — it is what a user's config
does — and it means a test that does not undo them leaks into every test that
runs afterwards."
  `(let ((r:*world* (c:make-world))
         (p:*policy* (make-instance 'p:conventional-policy)))
     (let ((*package* (find-package '#:latticewm/user)))
       (load (example-path ,name)))
     ,@body))

(defun open-windows (n &optional (world r:*world*))
  (dotimes (i n)
    (p:on-window-open p:*policy* world
                      (make-instance 'c:window
                                     :app-id (format nil "app~d" i)))))

;;; ------------------------------------------------------------------ 01

(test focus-follows-mouse-loads-and-respects-floats
  (with-example ("01-focus-follows-mouse.lisp")
    (is-true p:*focus-follows-mouse* "the option was turned on")
    (open-windows 2)
    ;; Put a float over the whole screen and check the pointer is ignored.
    (let* ((window (make-instance 'c:window :app-id "floaty")))
      (setf (c:window-floating-p window) t
            (c:window-rect window) (c:make-rect 0 0 2000 2000))
      (push (make-instance 'c:floating-window :window window
                                              :rect (c:make-rect 0 0 2000 2000))
            (c:world-floats r:*world*))
      (setf (c:prop r:*world* :rect-index) (make-hash-table :test #'eq))
      (setf (c:prop r:*world* :last-placements)
            (p:layout-node p:*policy* (c:world-root r:*world*)
                           (c:make-rect 0 0 1000 1000)))
      (is (null (p:pointer-focus p:*policy* r:*world* 500 500))
          "the pointer is over a float, so the pane underneath is not focused"))))

(test focus-can-change-before-anything-has-been-laid-out
  "The example's ON-FOCUS-CHANGE reads :RECT-INDEX, which EMIT writes after a
layout and which therefore does not exist yet on a world that has never been
placed.  `(gethash node nil)' is a type error, not a miss, so the method used
to take down every focus change that happened before the first frame — session
restore, a config that opens a window, and every policy test that never
renders.  It is installed on CONVENTIONAL-POLICY, so it took them down for
extensions that had never heard of it."
  (with-example ("01-focus-follows-mouse.lisp")
    (open-windows 2)
    (is (null (c:prop r:*world* :rect-index))
        "nothing has been laid out, so there is no index")
    (finishes (p:on-focus-change p:*policy* r:*world*
                                 (c:world-cursor r:*world*)
                                 (c:world-cursor r:*world*)))))

;;; ------------------------------------------------------------------ 02

(test window-rules-float-and-place
  (with-example ("02-window-rules.lisp")
    (let ((pinentry (make-instance 'c:window :app-id "pavucontrol")))
      (is (equal '(:float t) (p:window-rule-for p:*policy* pinentry)))
      (p:on-window-open p:*policy* r:*world* pinentry)
      (is-true (c:window-floating-p pinentry))
      (is (equal '(:stack 0 (:leaf nil)) (t*::shape (c:world-root r:*world*)))
          "a floated window never entered the tree"))
    (let ((firefox (make-instance 'c:window :app-id "firefox")))
      (p:on-window-open p:*policy* r:*world* firefox)
      (is (<= 2 (c:container-count (c:world-root r:*world*)))
          "the rule's workspace was created")
      (is (equal "firefox"
                 (let ((leaf (c:leaf-holding (c:world-root r:*world*) firefox)))
                   (and leaf (c:window-app-id (c:leaf-window leaf)))))
          "and the window is in it"))
    (let ((plain (make-instance 'c:window :app-id "anything-else")))
      (p:on-window-open p:*policy* r:*world* plain)
      (is-false (c:window-floating-p plain) "an unmatched window still tiles"))))

;;; ------------------------------------------------------------------ 03

(test master-stack-is-a-layout-in-one-method
  (with-example ("03-master-stack.lisp")
    (open-windows 4)
    (let* ((tree-before (t*::shape (c:world-root r:*world*)))
           (policy (make-instance (read-from-string "latticewm/user::master-stack-policy")))
           (root (c:world-root r:*world*))
           (placements (p:layout-node policy root (c:make-rect 0 0 1000 800)))
           (leaves (remove-if-not (lambda (pl) (typep (first pl) 'c:leaf))
                                  placements)))
      (is (= 4 (length leaves)))
      (let* ((rects (mapcar #'third leaves))
             (widths (remove-duplicates (mapcar #'c:rect-w rects))))
        (is (= 2 (length widths))
            "two column widths: the master and the stack, ~s" widths))
      (is (equal tree-before (t*::shape (c:world-root r:*world*)))
          "and the TREE is untouched -- only the arrangement changed, which is
why switching back puts every window exactly where it was"))))

(test master-stack-motion-follows-the-drawing
  (with-example ("03-master-stack.lisp")
    (open-windows 4)
    (let* ((policy (make-instance (read-from-string "latticewm/user::master-stack-policy")))
           (root (c:world-root r:*world*))
           (first-leaf (c:first-leaf-path root)))
      ;; From the master column, Right must cross into the stack.
      (let ((to (p:find-motion-target policy root first-leaf :right)))
        (is-true to "Right from the master column goes somewhere")
        (is (not (c:path-equal to first-leaf))))
      ;; And Down must stay inside whichever column it started in.
      (is-true (p:find-motion-target policy root
                                     (c:next-leaf-path root first-leaf) :down)))))

;;; ------------------------------------------------------------------ 04

(test scrolling-columns-is-a-container-kind-from-outside
  (with-example ("04-scrolling-columns.lisp")
    (open-windows 5)
    (funcall (read-from-string "latticewm/user::scrolling") 2)
    (let* ((root (c:world-root r:*world*))
           (strip (c:resolve-path root '(0))))
      (is (typep strip (read-from-string "latticewm/user::strip")))
      (is (= 5 (c:container-count strip)) "every window became a column")
      ;; Only two are drawn.
      (let* ((placements (p:layout-node p:*policy* root (c:make-rect 0 0 1000 800)))
             (visible (remove-if-not (lambda (pl)
                                       (and (fourth pl) (typep (first pl) 'c:leaf)))
                                     placements)))
        (is (= 2 (length visible)) "two columns on screen at a time"))
      ;; And moving right past the edge scrolls rather than refusing.
      (let ((offset-before (funcall (read-from-string "latticewm/user::strip-offset")
                                    strip)))
        (p:find-motion-target p:*policy* root '(0 0) :right)
        (p:find-motion-target p:*policy* root '(0 1) :right)
        (is (< offset-before
               (funcall (read-from-string "latticewm/user::strip-offset") strip))
            "the strip scrolled to follow, which is the whole feature")))))

(test every-example-file-is-loadable
  ;; The cheapest possible guard against the worst failure mode: an example
  ;; that no longer compiles, shipped as documentation.
  (dolist (name '("01-focus-follows-mouse.lisp" "02-window-rules.lisp"
                  "03-master-stack.lisp" "04-scrolling-columns.lisp"))
    (finishes (with-example (name) t))))
