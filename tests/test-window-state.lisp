;;;; tests/test-window-state.lisp --- What state is a window in, and who says so.
;;;;
;;;; #25 asked for a state machine over what was five scattered facts: three
;;;; booleans on the window, tree membership in the world, and the scratchpad.
;;;; The answer this lands on is that the state is DERIVED rather than stored --
;;;; C:WINDOW-STATE reads the five and answers -- and the two rulings that makes
;;;; are the two things worth testing:
;;;;
;;;;   * FULLSCREEN IS ORTHOGONAL to tiled and floating.  River renders a
;;;;     fullscreen window alone and ignores the layout, so a fullscreen window
;;;;     is still a leaf or still a float underneath, and leaving fullscreen has
;;;;     to know which.  An enum with one slot would have thrown that away.
;;;;
;;;;   * MINIMIZED IS EXCLUSIVE of tree membership but not of floating: a float
;;;;     that is put away comes back a float.
;;;;
;;;; The third is TRANSITION-WINDOW's contract: idempotent, one name, and every
;;;; verb that changes a window's state goes through it -- so the invariants are
;;;; the five functions' own and are not written a second time here.

(in-package #:latticewm/tests)
(in-suite states)

;;; ------------------------------------------------------------------ fixtures

(defun a-window-in-a-world (&optional (app-id "test"))
  "A world with one output, one workspace and one tiled WINDOW.
Returns (values world window)."
  (let* ((world (c:make-world))
         (output (make-instance 'c:output :name "one"
                                :rect (c:make-rect 0 0 1280 720)))
         (window (make-instance 'c:window :app-id app-id :identifier app-id)))
    (setf (c:world-outputs world) (list output))
    (p:ensure-workspaces-for-outputs world)
    ;; Placed the way the session places a window, so the fixture has the cursor
    ;; and the focus a verb needs.  Building the tree by hand put the leaf where
    ;; the workspace stack should be and every verb that asks `which window'
    ;; answered NIL.
    (p:on-window-open (policy) world window)
    (values world window)))

(defmacro with-window-world ((world window &rest args) &body body)
  "Bind the world, a policy and a seat, with WORLD holding WINDOW tiled."
  `(multiple-value-bind (,world ,window) (a-window-in-a-world ,@args)
     (let ((r:*world* ,world) (p:*policy* (policy)) (p:*seat* nil))
       ,@body)))

;;; ------------------------------------------------------- the state, derived

(test a-window-in-the-tree-is-tiled
  "The ordinary case, and the one that must not need a flag."
  (with-window-world (world window)
    (is (equal '(:tiled) (c:window-state world window)))
    (is (equal "tiled" (c:window-state-name '(:tiled))))))

(test a-float-is-floating-and-outside-the-tree
  "Floating is the disposition; the window is no longer a leaf."
  (with-window-world (world window)
    (r:float-away-from-tree (policy) window)
    (is (equal '(:floating) (c:window-state world window)))
    (is (null (c:leaf-holding (c:world-root world) window))
        "and it really is out of the tree, not just labelled")))

(test fullscreen-is-a-second-axis-not-a-third-state
  "A fullscreen window is still a tile, and still a float when it is one.  This
is the assertion an enum would not let anybody write."
  (with-window-world (world window)
    (r:request-fullscreen window t)
    (is (equal '(:tiled :fullscreen) (c:window-state world window)))
    (r:float-away-from-tree (policy) window)
    (is (equal '(:floating :fullscreen) (c:window-state world window)))
    (r:request-fullscreen window nil)
    (is (equal '(:floating) (c:window-state world window))
        "and leaving fullscreen leaves it exactly where it was")))

(test minimized-is-exclusive-of-the-tree-but-not-of-floating
  "A float that is put away comes back a float, so the membership is kept --
which is a decision, and this is where it is written down."
  (with-window-world (world window)
    (r:float-away-from-tree (policy) window)
    (r:minimize-window window)
    (is (equal '(:minimized :floating) (c:window-state world window)))
    (r:restore-window window)
    (is (equal '(:floating) (c:window-state world window))
        "back as a float")))

(test a-minimized-tile-says-only-minimized
  (with-window-world (world window)
    (r:minimize-window window)
    (is (equal '(:minimized) (c:window-state world window)))
    (r:restore-window window)
    (is (equal '(:tiled) (c:window-state world window)))))

(test a-dead-window-says-nothing-else
  "Liveness wins: a dead window's other flags describe a window that is gone."
  (with-window-world (world window)
    (setf (c:window-live-p window) nil)
    (is (equal '(:dead) (c:window-state world window)))))

(test unplaced-is-a-state-and-it-is-now-reachable-by-name
  "The state #6 calls invisible, which had no name: alive, in no tree, not
floating, not minimized.  It should be unreachable; a caller that sees it has
found a bug, and until #25 nothing could say whether a window was in it."
  (with-window-world (world window)
    ;; The window out of the tree without being floated or put away: the state
    ;; a window is in between `river says it exists' and `somebody placed it',
    ;; reached here by doing exactly that to the model.
    (p::remove-window-from-tree (policy) world window)
    (is (equal '(:unplaced) (c:window-state world window)))
    (is-true (search "nowhere" "nowhere — see the state above")
             "and the inspector gives it words rather than a keyword")))

;;; ---------------------------------------------------- the one way to change

(test transition-window-is-idempotent
  "Asking for the state a window is already in changes nothing and says so.
Every caller that computes a toggle from a view that may be stale depends on
this, and so does anything replaying a journal."
  (with-window-world (world window)
    (is (null (r:transition-window window :tiled))
        "already tiled: nil, and no undo entry")
    (is (eq window (r:transition-window window :floating)) "now floating")
    (is (null (r:transition-window window :floating)) "asking again: nil")
    (is (eq window (r:transition-window window :fullscreen)))
    (is (null (r:transition-window window :fullscreen)) "asking again: nil")
    (is (eq window (r:transition-window window :normal)))
    (is (eq window (r:transition-window window :minimized)))
    (is (null (r:transition-window window :minimized)) "asking again: nil")
    (is (eq window (r:transition-window window :visible)))
    (is (equal '(:floating) (c:window-state world window))
        "and the round trip ends where it started")))

(test transition-window-refuses-a-state-it-does-not-know
  "A typo in an extension or a hand-typed IPC form gets a list of the real
states, not a silently ignored keyword."
  (with-window-world (world window)
    (let ((signalled nil))
      (handler-case (r:transition-window window :invisible)
        (error (e) (setf signalled (princ-to-string e))))
      (is-true signalled)
      (is-true (search ":FLOATING" signalled) "the message lists the states"))))

(test the-verbs-go-through-the-state-machine
  "Gate 16's spirit applied to a state machine: the four verbs are one call
each, so the invariants live in one place.  Checked by behaviour -- the verbs
answer what the transition answers."
  (with-window-world (world window)
    (r:toggle-float)
    (is (equal '(:floating) (c:window-state world window)))
    (r:toggle-float)
    (is (equal '(:tiled) (c:window-state world window)))
    (r:toggle-fullscreen)
    (is (equal '(:tiled :fullscreen) (c:window-state world window)))
    (r:toggle-fullscreen)
    (is (equal '(:tiled) (c:window-state world window)))
    (r:minimize)
    (is (equal '(:minimized) (c:window-state world window)))
    (r:restore-last)
    (is (equal '(:tiled) (c:window-state world window)))))

;;; ------------------------------------------------------------- the inspector

(test describe-window-shows-the-state
  "The page #25 asked for.  It has to say the state in words, and it has to say
where the window is, because those are the two questions."
  (with-window-world (world window)
    (declare (ignore world))
    (let ((r::*help-visible* nil))
      (r:describe-window window)
      (let ((page (format nil "~s" r::*help-visible*)))
        (is-true (search "tiled" page) "the state")
        (is-true (search "the tree" page) "where it is")
        (is-true (search "test" page) "which window")))))

;;; ------------------------------------------------------------- persistence

(test the-saved-layout-carries-the-disposition
  "#25's fourth item: `confirm that floats persist'.  They did not.  A window
left floating at shutdown came back as a tile, in the tree, at a place it had
never been -- the file described the tree faithfully and the tree no longer
mentioned it."
  (with-window-world (world window)
    (declare (ignore world))
    (r:float-away-from-tree (policy) window)
    (r:request-fullscreen window t)
    ;; ALL-WINDOWS reads the compositor's table, which a headless test does not
    ;; have; WINDOW-FACTS takes the list for exactly this reason.
    ;; A row is (IDENTIFIER :KEY VALUE ...), so the plist starts at the REST --
    ;; GETF on the whole row would pair the identifier with the first key and
    ;; then call the list malformed, which is what it is if you read it wrong.
    (let ((row (rest (first (r:window-facts (list window))))))
      (is-true (getf row :floating) "the disposition is written")
      (is-true (getf row :fullscreen) "and the prominence")
      ;; A PLIST, not a RECT OBJECT, and that is the fix in #151: C:RECT has an
      ;; explicit constructor, so `#S(...)' -- which is what a REC T object
      ;; printed as -- cannot be READ back, and a state file with a float in it
      ;; was unreadable in its entirety rather than merely incomplete.
      (let ((rect (r::read-rect (getf row :float-rect))))
        (is-true rect "with the rectangle it had, not 0,0")
        (is (= 768 (c:rect-w rect)) "the width it had")))))
