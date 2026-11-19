;;;; tests/test-verbs.lisp --- Reaching a window by the name written on it.
;;;;
;;;; #34: every way to reach a window by name needed something else set up
;;;; first (a buffer name, a tag, a tab), and none of them answered "the window
;;;; whose title that is" about a window you have not labelled and cannot see.
;;;; FOCUS-WINDOW-BY-NAME does, over the windows the *world* holds rather than
;;;; the ones the compositor has listed -- which is why these checks need no
;;;; compositor: the world is the whole of what the picker reads.
;;;;
;;;; Two of these are about a decided question and not about mechanics: a
;;;; unique name is offered bare, and an ambiguous one is offered with the
;;;; place beside it.  A picker that offered `foot' twice would make Return a
;;;; coin flip, and "focus the window I named" silently focusing the wrong one
;;;; is the failure P:NAME-OF-WINDOW's docstring says a name is allowed to
;;;; have -- two windows may share a name and nothing breaks, *because* the
;;;; name is not an address.

(in-package #:latticewm/tests)
(in-suite verbs)

;;; ------------------------------------------------------------------ fixtures

(defun float-of (window)
  "A floating-window record standing for WINDOW, as FLOAT-WINDOW-NOW makes one."
  (make-instance 'c:floating-window :window window
                                     :rect (c:make-rect 10 10 400 300)))

(defun picker-world ()
  "Three workspaces' worth of windows: tiled, empty pane, put away, floating.

    workspace 1  ( h firefox | empty )
    workspace 2  foot
    scratchpad   foot          <- the second `foot': same name, no address
    float        deadbeef

Values: (values WORLD FIREFOX FOOT-TILED FOOT-AWAY FLOAT-WINDOW)."
  (let* ((firefox (win "firefox"))
         (foot-tiled (win "foot"))
         (foot-away (win "foot"))
         (float-window (win "deadbeef"))
         (workspace-1 (c:make-split :horizontal
                                    (list (c:make-leaf firefox) (c:make-leaf))))
         (workspace-2 (c:make-leaf foot-tiled))
         (world (c:make-world
                 :root (c:make-stack (list workspace-1 workspace-2) 0))))
    (setf (c:window-minimized-p foot-away) t
          (c:world-scratchpad world) (list foot-away)
          (c:world-floats world) (list (float-of float-window)))
    (values world firefox foot-tiled foot-away float-window)))

(defmacro with-picker-world ((world firefox foot-tiled foot-away float-window)
                             &body body)
  "Run BODY with the fixture's world bound and a policy and seat of its own."
  `(multiple-value-bind (,world ,firefox ,foot-tiled ,foot-away ,float-window)
       (picker-world)
     (let ((r:*world* ,world)
           (p:*policy* (policy))
           (p:*seat* nil))
       ,@body)))

(defun picker-labels ()
  "The picker's labels, in the order it offers them."
  (mapcar #'car (r::window-picker-entries)))

(defun label-of (window)
  "What the picker offers for WINDOW — its single entry's label — or NIL."
  (car (find window (r::window-picker-entries) :key #'cdr :test #'eq)))

;;; ------------------------------------------------------------- the enumeration

(test every-window-the-world-holds-is-offered-once
  "Tiled, put away and floating alike: a picker that can only see the tree is
a picker that cannot find the window you put away, which is the one you most
need to find."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore firefox foot-tiled foot-away float-window))
    (is (equal '("deadbeef" "firefox" "foot" "foot")
               (sort (mapcar #'c:window-app-id (r::world-windows)) #'string<))
        "four windows, each exactly once")
    (is (= 4 (length (r::window-picker-entries)))
        "and one entry per window, not per place")))

(test a-window-held-in-two-places-is-still-one-entry
  "The scratchpad is a place a window *left*, so a window that is put away is
not also under the cursor.  Enumeration is by identity for that reason."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (push foot-tiled (c:world-scratchpad r:*world*))
    (is (= 4 (length (r::world-windows)))
        "a window on the scratchpad and in the tree is one window")))

;;; ------------------------------------------------------------------- labels

(test a-unique-name-is-offered-bare
  "The point of asking by name is that the name is enough."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (is (equal "firefox" (label-of firefox)))
    (is (equal "deadbeef" (label-of float-window)))))

(test a-shared-name-is-offered-with-where-the-window-is
  "Two windows called `foot' are two entries you can tell apart, and neither
of them is a bare `foot' that Return could take either way."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (let ((tiled (label-of foot-tiled))
          (away (label-of foot-away)))
      (is (equal "foot (workspace 2)" tiled))
      (is (equal "foot (scratchpad)" away))
      (is (not (equal tiled away)))
      (is (notany (lambda (label) (equal "foot" label)) (picker-labels))
          "no bare `foot' survives, because it would be ambiguous"))))

(test two-windows-sharing-a-name-and-a-place-are-numbered
  "A place is not unique either: two terminals can be in one workspace.  The
label is a picker's promise that choosing it is not a guess, so it is the
number that has to give way."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore firefox))
    (let ((another (win "foot")))
      (setf (c:world-scratchpad r:*world*) (list foot-away another))
      (is (equal "foot (scratchpad)"
                 (label-of foot-away)))
      (is (equal "foot (scratchpad) <2>"
                 (label-of another))))))

;;; ----------------------------------------------------------------- resolving

(test every-label-resolves-to-the-window-it-was-built-for
  "The round trip is the whole contract of a completion prompt: what it shows
you is what Return means."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world))
    (dolist (entry (r::window-picker-entries))
      (is (eq (cdr entry) (r::window-by-picker-name (car entry)))
          (format nil "~s names its own window" (car entry))))))

(test a-bare-name-resolves-when-only-one-window-carries-it
  "Typing the name is the same act as completing it."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world))
    (is (eq firefox (r::window-by-picker-name "firefox")))
    (is (eq firefox (r::window-by-picker-name "  firefox  "))
        "and surrounding space is not part of a name")))

(test an-ambiguous-bare-name-resolves-to-nothing
  "Nil rather than the first one: focusing the wrong window is a visible act
and there is nothing in `foot' that says which."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world foot-tiled foot-away))
    (is (null (r::window-by-picker-name "foot")))
    (is (null (r::window-by-picker-name "nothing is called this")))))

(test a-unique-prefix-is-enough
  "You type the first letters of a long title, not the whole of it."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world foot-tiled))
    (is (eq float-window (r::window-by-picker-name "dead")))
    (is (eq firefox (r::window-by-picker-name "fire")))
    (is (null (r::window-by-picker-name "fo"))
        "a prefix of two ambiguous labels is still ambiguous")))

;;; ------------------------------------------------------------------ the verb

(test focusing-a-window-on-another-workspace-goes-there
  "The half that is easy to forget: a window on another workspace can be
focused perfectly correctly and still be on no screen, which looks exactly
like the command having done nothing."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore foot-away float-window))
    (is (equal '(0 0) (c:world-cursor world))
        "the cursor starts on the first pane of the first workspace")
    (is (eq foot-tiled (r:focus-window-by-name foot-tiled)))
    (is (equal '(1) (c:world-cursor world))
        "and it is now standing on the window, not merely near it")
    (is (= 1 (c:container-selection (c:world-workspaces world)))
        "the workspace stack agrees with the cursor, so the screen shows it")))

(test focusing-a-put-away-window-brings-it-back
  "A window on the scratchpad is on no screen, so `focus it where it is' would
be a command that appears to do nothing.  It is restored first."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore foot-tiled float-window))
    (r:focus-window-by-name foot-away)
    (is (not (c:window-minimized-p foot-away)))
    (is (not (member foot-away (c:world-scratchpad world)))
        "and it is off the scratchpad")
    (is (eq foot-away (r:current-window))
        "the cursor is standing on it")))

(test focusing-a-float-takes-the-keyboard-where-it-stands
  "Focus is a place in the tree (D18) and a float is not in it, so there is no
cell to move to: the float slot is what says the keyboard is elsewhere."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world firefox foot-tiled foot-away))
    (r:focus-window-by-name float-window)
    (is (eq float-window
            (c:float-window (c:world-focused-float r:*world* p:*seat*)))
        "the float holds the keyboard")
    (is (equal '(0 0) (c:world-cursor r:*world*))
        "and the cursor did not move, because there was nowhere to move it")))

(test focusing-refuses-a-name-that-is-not-a-window
  "The verb returns NIL and jumps nowhere rather than guessing."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore foot-tiled foot-away float-window))
    (is (null (r:focus-window-by-name nil))
        "the prompt's parser hands NIL for a name that matches nothing")
    (is (equal '(0 0) (c:world-cursor world)))))

(test the-window-type-is-how-the-prompt-reaches-the-verb
  "M-x asks for a :WINDOW; the type offers the picker's labels and hands the
command the parsed window.  This is the connection the checks above assume:
labels are promises until a prompt makes them sentences."
  (with-picker-world (world firefox foot-tiled foot-away float-window)
    (declare (ignore world))
    (let ((type (p:argument-type :window)))
      (is (equal (picker-labels)
                 (funcall (p:argument-type-candidates type)))
          "the prompt offers exactly the picker's labels")
      (is (eq foot-tiled
              (funcall (p:argument-type-parser type) "foot (workspace 2)"))
          "the parser turns the chosen label into the window")
      (is (eq firefox (funcall (p:argument-type-parser type) "firefox"))
          "and a bare unique name is an answer, not a guess")
      (signals error (funcall (p:argument-type-parser type) "no-one")
        "a name the picker does not know is refused, not guessed"))))

;;; ------------------------------------------------------- the bulk verbs
;;;
;;; #122: six verbs were one loop spelled six ways.  What is worth pinning is
;;; not the loop — it is the rulings the loop carries: EXCEPT is skipped, the
;;; count is what acted rather than what was offered, and a window on another
;;; workspace or floating over this one is not on this workspace.  Those are
;;; the answers that were previously reasoned about in six places.

(defun bulk-world ()
  "One workspace of three windows plus a window on another one and a float.

Values: (values WORLD A B C ELSEWHERE FLOAT-WINDOW)."
  (let* ((a (win "a")) (b (win "b")) (c (win "c"))
         (elsewhere (win "elsewhere"))
         (float-window (win "floating"))
         (world (c:make-world
                 :root (c:make-stack
                        (list (c:make-split :horizontal
                                            (list (c:make-leaf a)
                                                  (c:make-leaf b)
                                                  (c:make-leaf c)))
                              (c:make-leaf elsewhere))
                        0))))
    (setf (c:world-floats world) (list (float-of float-window)))
    (values world a b c elsewhere float-window)))

(defmacro with-bulk-world ((world a b c elsewhere float-window) &body body)
  `(multiple-value-bind (,world ,a ,b ,c ,elsewhere ,float-window)
       (bulk-world)
     (let ((r:*world* ,world)
           (p:*policy* (policy))
           (p:*seat* nil))
       ,@body)))

(test the-bulk-loop-skips-except-and-other-workspaces-and-floats
  "The three rulings in one place: EXCEPT is not touched, a window on another
workspace was never on this one, and a float belongs to the output."
  (with-bulk-world (world a b c elsewhere float-window)
    (declare (ignore world))
    (let ((seen '()))
      (r::act-on-current-workspace (lambda (window) (push window seen) t)
                                    :except b)
      (is (equal (list a c) (nreverse seen))
          "the focus survives and the other workspace's window is absent")
      (is (not (member float-window seen))
          "and the float was never a candidate"))))

(test a-bulk-count-is-what-acted-not-what-was-offered
  "`close-window-later' refuses a window with no proxy, which is every window
in a headless test and no window in a live session.  The count has to be the
refusals' complement, or `close-others' reports closing windows it did not."
  (with-bulk-world (world a b c elsewhere float-window)
    (declare (ignore world elsewhere float-window))
    (is (zerop (r:close-others))
        "nothing has a proxy here, so nothing was closed")
    (is (equal (list b c)
               (r::act-on-current-workspace (lambda (window) window)
                                             :except a))
        "while an action that does act reports what it acted on")))

(test minimize-workspace-still-minimizes-only-this-workspace
  "The composed loop changed the spelling of these four verbs, not what they
mean -- so the meaning gets a check of its own: the window on the other
workspace keeps its pane."
  (with-bulk-world (world a b c elsewhere float-window)
    (declare (ignore float-window))
    (is (= 3 (r:minimize-workspace))
        "three windows were on this workspace")
    (is (notany #'c:window-minimized-p (list elsewhere))
        "the fourth was not, and is still in its pane")
    ;; The other workspace still holds its window, so this workspace's
    ;; emptiness did not travel.
    (is (eq elsewhere
            (c:leaf-window (c:child-at (c:world-workspaces world) 1))))))
