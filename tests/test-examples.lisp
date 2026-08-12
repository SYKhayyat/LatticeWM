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

(defun method-census ()
  "Every method on every generic the program publishes, as (GENERIC . METHODS)."
  (let ((out '()))
    (dolist (package (remove nil (list (find-package '#:latticewm/policy)
                                       (find-package '#:latticewm/core)
                                       (find-package '#:latticewm/runtime)))
             out)
      (do-symbols (symbol package)
        (when (eq (symbol-package symbol) package)
          (let ((function (and (fboundp symbol) (ignore-errors (fdefinition symbol)))))
            (when (typep function 'generic-function)
              (push (cons function
                          (copy-list (sb-mop:generic-function-methods function)))
                    out))))))))

(defun option-census ()
  "Every registered option and the value it has now, as (VARIABLE . VALUE)."
  (mapcar (lambda (row) (cons (second row) (third row)))
          (p:all-options)))

(defun restore-methods (census)
  "Remove every method that was not there when CENSUS was taken."
  (dolist (generic (method-census))
    (let ((was (cdr (assoc (car generic) census :test #'eq))))
      (dolist (method (cdr generic))
        (unless (member method was :test #'eq)
          (remove-method (car generic) method))))))

(defun restore-options (census)
  (dolist (row census)
    (setf (symbol-value (car row)) (cdr row))))

(defun timer-census ()
  "The name of every timer registered right now."
  (let ((out '()))
    (maphash (lambda (name timer) (declare (ignore timer)) (push name out))
             r::*timers*)
    out))

(defun restore-timers (census)
  "Take off every timer that was not registered when CENSUS was taken.

Names rather than the records, and removal rather than restoration, because
that is the whole of what is being undone: an example may only *add* one, and
re-adding under a name that already exists replaces rather than duplicates, so
a timer present in both censuses is the same timer and must be left alone."
  (dolist (name (timer-census))
    (unless (member name census :test #'equal)
      (r:remove-timer name))))

(defmacro with-example ((name) &body body)
  "Load an example into a fresh world, run BODY, and clean up after it.

THE CLEANUP IS THE POINT AND IT USED TO BE ONLY A PARAGRAPH.  This docstring
said `a test that does not undo them leaks into every test that runs
afterwards' and then rebound two specials and called LOAD -- no REMOVE-METHOD,
no option restore, no UNWIND-PROTECT.  The examples define methods on the
*shipped* policy class, which is the honest thing for them to do because it is
what a user's configuration does, and suite.lisp runs this suite before the
lattice suite in the same image.  So the whole plane suite ran with
*FOCUS-FOLLOWS-MOUSE* turned on by example 01 and with example 02's
ON-WINDOW-OPEN standing on CONVENTIONAL-POLICY, consulting a rule list that
floats anything under 400x300 and sends \"firefox\" to another workspace.

It was correct anyway, by string coincidence: no fixture window is called
\"firefox\" and none of them announce a preferred size.  That is not a property
anybody chose and it is not one anybody could have noticed losing -- example
01's own ON-FOCUS-CHANGE leaking into the lattice suite is what took two new
plane tests down, one commit before this was written.

Methods and option values both, because both are global and both are what an
example is made of.

AND TIMERS, WHICH IS THE THIRD ONE AND ARRIVED THE SAME WAY THE OTHER TWO DID.
Example 05 registers a clock at load time, because a status line with a clock
in it has to.  A timer outlives the test that loaded it, fires on the window
manager's own thread for the rest of the image, and calls MARK-DIRTY on a
world that was rebound and thrown away -- and it would have been found the way
the methods were, by an unrelated suite failing for no reason anybody could
see.  The list of globals an example can touch is the list of things this has
to put back, and it is now three long."
  (let ((methods (gensym "METHODS")) (options (gensym "OPTIONS"))
        (timers (gensym "TIMERS")))
    `(let ((r:*world* (c:make-world))
           (p:*policy* (make-instance 'p:conventional-policy))
           (,methods (method-census))
           (,options (option-census))
           (,timers (timer-census)))
       (unwind-protect
            (progn
              (let ((*package* (find-package '#:latticewm/user)))
                (load (example-path ,name)))
              ,@body)
         (restore-methods ,methods)
         (restore-options ,options)
         (restore-timers ,timers)))))

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

;;; ------------------------------------------------------------------ 05

(test the-status-line-composes-rather-than-being-replaced
  "Tier 1, and the thing every window manager answers with a second program.

The point of the example is the shape of the method rather than the segments:
CALL-NEXT-METHOD and append, so the shipped line survives and a second
extension doing the same thing survives too.  A method that returned only its
own segments would silently delete whatever anybody else had added, and this
asserts the difference."
  (with-example ("05-status-line.lisp")
    (open-windows 2)
    ;; The example's own special, read by name: it does not exist until the
    ;; file is loaded, so this file cannot mention it at read time.
    (let* ((switch (read-from-string "latticewm/user::*status-line-extras*"))
           (shipped-length
             (progv (list switch) (list nil)
               (length (p:echo-content p:*policy* r:*world* 200))))
           (with-extras (p:echo-content p:*policy* r:*world* 200)))
      (is (< shipped-length (length with-extras))
          "the extra segments were added rather than replacing the line")
      (is (every (lambda (segment)
                   (and (consp segment) (stringp (car segment))
                        (member (cdr segment) '(:accent :normal))))
                 with-extras)
          "and every segment still has the shape ECHO-CONTENT documents")
      (is (notany (lambda (segment) (string= "" (car segment))) with-extras)
          "a segment with nothing to say is dropped, not drawn empty"))
    ;; The clock is the one segment that is always there, so it is the one
    ;; that says the method ran at all.
    (is (= 5 (length (funcall (read-from-string "latticewm/user::clock-segment"))))
        "hh:mm")))

(test a-clock-segment-comes-with-something-that-makes-it-tick
  "The half of a status line that is not the segment.

The clock read `asked fresh every frame', which was true, and there were no
frames: every redraw in this program is caused by something, and nothing was
caused by time passing.  So the shipped example displayed the time of the last
layout change.  The assertion is that loading it registers a timer, because
that is the part a reader would otherwise write a segment without."
  (with-example ("05-status-line.lisp")
    (is (member :status-line-clock (timer-census) :test #'equal)
        "loading the example started the clock")
    (funcall (read-from-string "latticewm/user::status-line-extras") nil)
    (is (not (member :status-line-clock (timer-census) :test #'equal))
        "and turning the segments off takes the wakeup off with them")
    (funcall (read-from-string "latticewm/user::status-line-extras") t)
    (is (member :status-line-clock (timer-census) :test #'equal)
        "and back on again, with no restart and no second clock")))

(test loading-an-example-twice-leaves-one-of-everything
  "Re-registering by name replaces rather than duplicates.

Loading a configuration file a second time is what SHIFT+SUPER+C does and what
anybody editing their init file at a REPL does all evening.  ADD-HOOK's
docstring records this project losing an afternoon to the other side of it --
`the help overlay drew itself twice, once through each generation of the same
function' -- and a timer is the mechanism where the same mistake costs a
wakeup forever rather than a double draw."
  (with-example ("05-status-line.lisp")
    (let ((before (length (timer-census))))
      (let ((*package* (find-package '#:latticewm/user)))
        (load (example-path "05-status-line.lisp")))
      (is (= before (length (timer-census)))
          "the second load replaced the clock rather than adding one"))))

(test the-count-a-general-purpose-status-bar-cannot-write
  "The segment that is the argument for the whole approach: it is a fact about
the layout, not about the machine, so nothing outside the window manager could
produce it."
  (with-example ("05-status-line.lisp")
    (let ((elsewhere (read-from-string "latticewm/user::elsewhere-segment")))
      (open-windows 2)
      (is (equal "" (funcall elsewhere r:*world*))
          "everything open is in the workspace you are looking at")
      (r:run-command "new-workspace")
      (open-windows 1)
      (is (search "2 elsewhere" (funcall elsewhere r:*world*))
          "and the two you left behind are counted, from the tree"))))

(test every-example-file-is-loadable
  ;; The cheapest possible guard against the worst failure mode: an example
  ;; that no longer compiles, shipped as documentation.
  ;;
  ;; ENUMERATED, NOT LISTED.  This was four filenames typed into the test, so
  ;; the fifth example was covered by whatever tests somebody remembered to
  ;; write for it and by nothing otherwise -- the same shape as every gate in
  ;; this project that checks a directory against a list rather than consulting
  ;; one.
  (let ((files (sort (mapcar #'file-namestring
                             (directory (merge-pathnames
                                         "examples/*.lisp"
                                         (asdf:system-source-directory "latticewm"))))
                     #'string<)))
    (is (<= 5 (length files)) "the examples are there at all: ~s" files)
    (dolist (name files)
      (finishes (with-example (name) t)))))
