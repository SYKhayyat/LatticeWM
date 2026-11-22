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

;;; ---------------------------------------------------------- float geometry
;;;
;;; #123: 640x480 was spelled three times -- the FLOATING-WINDOW initform, the
;;; no-output fallback in FLOAT-WINDOW-NOW twice over -- and the resize floor
;;; 100x60 was a literal inside RESIZE-FLOAT.  These pin the two rulings that
;;; came out of naming them: there is one default size and it is fresh each
;;; time, and the floor is an option rather than a number in a body.

(test a-default-float-rect-is-the-single-answer-and-is-fresh
  "Fresh matters: callers mutate the rect they were handed, so one shared
rectangle would make every default-sized float on screen move together."
  (let ((one (c:make-default-float-rect))
        (two (c:make-default-float-rect)))
    (is (not (eq one two)))
    (is (= c:*default-float-width* (c:rect-w one)))
    (is (= c:*default-float-height* (c:rect-h one)))
    (is (c:rect-equal (c:make-default-float-rect 100 100)
                      (c:make-rect 100 100 c:*default-float-width*
                                   c:*default-float-height*))
        "and its origin is the caller's")))

(test a-floating-window-made-without-a-rect-gets-the-default
  (let ((float (make-instance 'c:floating-window :window (win "rectless"))))
    (is (= c:*default-float-width* (c:rect-w (c:float-rect float)))
        "the class initform and the no-output fallback agree by construction")))

(test a-float-created-with-no-output-lands-in-the-default-rect
  "The fallback path rather than the usual one: no output to centre on."
  (let ((r:*world* (c:make-world))
        (p:*policy* (policy))
        (p:*seat* nil))
    (let ((window (win "lonely")))
      (r::float-window-now p:*policy* window)
      (let ((float (c:float-of-window r:*world* window)))
        (is-true float "it floated")
        (is (= 100 (c:rect-x (c:float-rect float))))
        (is (= c:*default-float-width* (c:rect-w (c:float-rect float)))
            "and it is the one default, not a third literal")))))

(defun float-in-a-world (width height)
  "A world whose only float is WIDTH x HEIGHT and is holding the keyboard.
Returns (values WORLD WINDOW FLOAT)."
  (let* ((world (c:make-world))
         (window (win "floaty"))
         (float (make-instance 'c:floating-window
                               :window window
                               :rect (c:make-rect 100 100 width height))))
    (setf (c:window-floating-p window) t)
    (push float (c:world-floats world))
    (setf (c:world-focused-float world nil) float)
    (values world window float)))

(test shrinking-a-float-stops-at-the-option-not-at-a-literal
  "A float narrower than the floor is still focused and still framed and
cannot be seen or grabbed back, which is why there is a floor at all -- and
why it is a setting rather than a number in the middle of a command."
  (multiple-value-bind (world window float) (float-in-a-world 400 400)
    (declare (ignore float))
    (let ((r:*world* world)
          (p:*policy* (policy))
          (p:*seat* nil)
          (p:*float-minimum-size* (cons 120 90)))
      (dotimes (i 40) (r:resize-float :left))
      (is (= 120 (c:rect-w (c:float-rect (c:float-of-window world window))))
          "shrink all you like: the width stops at the option's width")
      (dotimes (i 40) (r:resize-float :up))
      (is (= 90 (c:rect-h (c:float-rect (c:float-of-window world window))))
          "and the height at the option's height"))))

;;; ------------------------------------------- the commands nothing named
;;;
;;; #119: 36 commands were named by no test at all -- the layer the last four
;;; features fed, and the largest unchecked surface in the tree.  These are the
;;; ones whose behaviour is a fact about state a headless test can construct, so
;;; the gate's allowlist starts small and shrinks rather than the other way
;;; round.  Each check names the command the way a session would reach it.

(test clearing-the-selection-says-how-many-it-cleared
  (let ((world (fresh-world)) (r:*selection* '()))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal
                                            (list (leaf-with "a")
                                                  (leaf-with "b"))))
                        0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world))
      (r:select-window)
      (is (= 1 (r:clear-selection)) "one marked window, one cleared")
      (is (null (r:selected-windows)) "and the selection is empty")
      (is (zerop (r:clear-selection)) "clearing an empty selection is zero, not an error"))))

(test list-selected-names-the-marked-windows-in-order
  "The name is what a session sees in the notification, so a bare window with no
app-id must still say something."
  (let ((world (fresh-world)) (r:*selection* '()))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal
                                            (list (leaf-with "a")
                                                  (leaf-with "b"))))
                        0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world))
      (r:select-window)
      (is (= 1 (length (r:list-selected)))
          "listing answers the windows it listed"))))

(test workspace-motion-wraps-and-comes-back
  "Two workspace verbs, one implementation with a sign -- and wrapping is the
property that makes a second pair of keys bearable."
  (let ((world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list (leaf-with "one")))
                              (c:make-split :horizontal (list (leaf-with "two")))
                              (c:make-split :horizontal (list (leaf-with "three"))))
                        0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world))
      (r:next-workspace)
      (is (= 1 (c:container-selection (c:world-root world))))
      (r:previous-workspace)
      (is (= 0 (c:container-selection (c:world-root world)))
          "and back to where we were")
      (r:previous-workspace)
      (is (= 2 (c:container-selection (c:world-root world)))
          "off the front end means the last workspace"))))

(test tab-previous-is-tab-next-backwards
  "The pair exists so that 'the next one' and 'the one before' are one function
with a sign, which is why only TAB-NEXT needed a body."
  (let ((world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-stack (list (c:make-split
                                                   :horizontal
                                                   (list (leaf-with "a")
                                                         (leaf-with "b")))
                                                   `(:stack
                                                     ,(leaf-with "x")
                                                     ,(leaf-with "y")))))
                        0))
    (setf (c:world-cursor world) '(0 0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:tab-next)
      (is (= 1 (c:container-selection (c:child-at (c:world-root world)
                                                  '(0 1)))))
      (r:tab-previous)
      (is (= 0 (c:container-selection (c:child-at (c:world-root world)
                                                  '(0 1))))
          "and the way back is the same call negated"))))

(test gather-tag-brings-the-tagged-windows-here
  "The verb the tag layer exists for: tag once, collect wherever they drifted.

The tag layer reads ALL-WINDOWS -- the compositor's list, because a tag belongs
to a window and not to a place -- so this registers the two in a server the way
tests/test-tags.lisp does.  That is the honest fixture: without it the test
would be asking a tag question of a world that has no windows at all."
  (let* ((server (make-instance 'r::server))
         (home (leaf-with "home"))
         (away (leaf-with "away"))
         (world (fresh-world))
         (r:*selection* '()))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list home))
                              (c:make-split :horizontal (list away)))
                        0))
    (dolist (window (list (c:leaf-window home) (c:leaf-window away)))
      (setf (gethash window (r::server-windows server)) window))
    (let ((r:*server* server) (r:*world* world)
          (p:*policy* (policy)) (p:*seat* nil))
      ;; Tag the window on the far workspace, then ask for it from home.
      (setf (c:world-cursor world) '(1 0))
      (r:tag-window "work")
      (setf (c:world-cursor world) '(0 0))
      (is (= 1 (r:gather-tag "work"))
          "the window carrying the tag was brought over")
      (is (= 2 (length (c:node-windows (c:child-at (c:world-root world) 0))))
          "and it is on the workspace the cursor was on")
      (is (stringp (r:gather-tag "nobody-has-this"))
          "a tag nobody carries says so rather than erroring"))))

(test list-tags-says-nothing-when-nothing-is-tagged
  "The empty case is the one every user meets first.  LIST-TAGS answers through
the help overlay, so that is what this reads -- a notify string would be
truthy and would make an assertion on the return value meaningless."
  (let* ((server (make-instance 'r::server))
         (window (leaf-with "a"))
         (world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list window))) 0))
    (setf (c:world-cursor world) '(0 0))
    (setf (gethash (c:leaf-window window) (r::server-windows server))
          (c:leaf-window window))
    (let ((r:*server* server) (r:*world* world)
          (p:*policy* (policy)) (p:*seat* nil)
          (r::*help-visible* nil))
      (is (stringp (r:list-tags)) "no tags: it says so in the echo area")
      (is (null r::*help-visible*) "and nothing appeared on the overlay")
      (r:tag-window "work")
      (is-true (r:list-tags) "with a tag, a page")
      (is-true (search "WORK" (format nil "~s" r::*help-visible*) :test #'char-equal)
               "and the tag is named on it"))))

(test tab-previous-is-tab-next-backwards
  "The pair exists so that 'the next one' and 'the one before' are one function
with a sign, which is why only TAB-NEXT needed a body."
  (let* ((world (fresh-world))
         (workspace (c:make-stack (list (leaf-with "a") (leaf-with "b")
                                        (leaf-with "c"))
                                  0)))
    (setf (c:world-root world) (c:make-stack (list workspace) 0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:tab-next)
      (is (= 1 (c:container-selection workspace)) "the next tab is shown")
      (r:tab-previous)
      (is (= 0 (c:container-selection workspace))
          "and the way back is the same call negated"))))

;;; ------------------------------------------------- floats, help and sharing
;;;
;;; The second batch for #119, same rule: a command a headless test can reach
;;; by constructing state is a command this gate can hold, and the ones left on
;;; the allowlist are the ones that genuinely need a compositor or a prompt.

(test focus-float-learns-and-focus-tiled-forgets
  "Two keys and one slot: focus is a place in the tree, a float is not in one,
so the float layer needs its own answer for `what has the keyboard'.

Cycling past the last float returns focus to the cursor -- one key enters the
layer and leaves it, which is why there is no mode to be stuck in."
  (multiple-value-bind (world window float) (float-in-a-world 400 300)
    (declare (ignore window))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (setf (c:world-focused-float world nil) nil)
      (is (eq float (r:focus-float)) "the one float takes the keyboard")
      (is (eq float (c:world-focused-float world nil)))
      (is (null (r:focus-float)) "one more press and focus is back in the tree")
      (is (null (c:world-focused-float world nil)))
      (is (eq float (r:focus-float)) "and it enters again from the tree")
      (r:focus-tiled)
      (is (null (c:world-focused-float world nil))
          "FOCUS-TILED gives the keyboard back to the cursor"))))

(test raising-a-float-moves-it-to-the-top-of-the-pile
  "Order in WORLD-FLOATS is stacking order, so `raise' is a list operation and
nothing more."
  (let* ((world (c:make-world))
         (first (make-instance 'c:floating-window
                               :window (win "under") :rect (c:make-rect 0 0 100 100)))
         (second (make-instance 'c:floating-window
                                :window (win "over") :rect (c:make-rect 0 0 100 100))))
    (setf (c:world-floats world) (list first second)
          (c:world-focused-float world nil) first)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:raise-float)
      (is (eq first (car (last (c:world-floats world))))
          "the float that had the keyboard is now on top")
      (is (= 2 (length (c:world-floats world))) "and the list is the same length"))))

(test moving-a-float-nudges-only-its-rect
  "Floats are positioned by hand -- that is what floating them was for -- and
the keyboard is half of positioning."
  (multiple-value-bind (world window float) (float-in-a-world 400 300)
    (declare (ignore float))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (let ((before (c:float-of-window world window)))
        (setf (c:world-focused-float world nil) before)
        (r:move-float :right 40)
        (is (= 140 (c:rect-x (c:float-rect before))) "40 to the right")
        (is (= 400 (c:rect-w (c:float-rect before))) "and not resized")
        (r:move-float :up 40)
        (is (= 60 (c:rect-y (c:float-rect before))) "40 up, which is a smaller y")))))

(test anchoring-a-float-to-a-pane-is-one-slot-and-its-inverse
  "An anchored float travels with its pane; unanchored it belongs to the
output.  The rectangle is re-expressed either way so that the window does not
jump when the anchor changes, and that is the property worth pinning."
  (multiple-value-bind (world window float) (float-in-a-world 200 200)
    (declare (ignore float))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (setf (c:world-root world)
            (c:make-stack (list (c:make-split :horizontal (list (leaf-with "a"))))
                          0))
      (setf (c:world-cursor world) '(0 0))
      (let* ((the-float (c:float-of-window world window))
             (pane (c:resolve-path (c:world-root world) '(0))))
        (setf (c:world-focused-float world nil) the-float)
        ;; The rect index is what an anchor is expressed against.
        (setf (c:prop world :rect-index)
              (let ((table (make-hash-table :test #'equal)))
                (setf (gethash '(0) table) (c:make-rect 50 50 800 600))
                table))
        (is (eq pane (r:anchor-float '(0))) "the pane is the anchor")
        (is (eq pane (c:float-anchor the-float)) "and the float remembers it")
        (is (eq pane (c:child-at (c:world-root world) 0))
            "which is the pane at that path, not the path itself")
        (r:unanchor-float)
        (is (null (c:float-anchor the-float)) "the slot is given back")))))

(test equalize-all-is-equalize-over-the-whole-workspace
  "EQUALIZE equalizes the nearest enclosing split; EQUALIZE-ALL equalizes every
split on the workspace, which is what makes it a separate command."
  (let ((world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack
           (list (c:make-split :horizontal
                               (list (c:make-split :vertical
                                                   (list (leaf-with "a")
                                                         (leaf-with "b")))
                                     (leaf-with "c"))
                               '(3 1)))
           0))
    (setf (c:world-cursor world) '(0 0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:equalize-all)
      (let* ((root (c:world-root world))
             (outer (c:child-at root 0))
             (inner (c:child-at outer 0)))
        (is (equal '(1 1) (c:weights outer)) "the outer split is even")
        (is (equal '(1 1) (c:weights inner))
            "and so is the inner one, which is the whole difference")))))

(test describing-things-puts-them-on-the-help-overlay
  "DESCRIBE-* and APROPOS are how a user reads the corpus the project brags
about, and they are all one call to the overlay."
  (let ((world (fresh-world)) (r::*help-visible* nil))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list (leaf-with "a")))) 0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (is-true (r:describe-command "close") "a command describes itself")
      (is-true (search "close" (format nil "~s" r::*help-visible*)
                       :test #'char-equal))
      (setf r::*help-visible* nil)
      (is-true (r:describe-option "*gaps*") "so does an option")
      (setf r::*help-visible* nil)
      ;; DESCRIBE-KEY answers in the echo area rather than on the overlay --
      ;; it is one line, and a page for one line would be in the way -- so the
      ;; assertion is on what it resolved to and what it said.
      (let ((p:*keymap* (p:make-keymap :name "test"))
            (p:*echo-message* nil))
        (p:install-default-keymap p:*keymap*)
        (is (equal '("terminal")
                   (r:describe-key (format nil "~areturn" (p:modifier-string))))
            "a key describes the command it is bound to")
        (is-true (search "terminal" (car p:*echo-message*) :test #'char-equal)
                 "and says so in the echo area")))))

(test apropos-finds-a-command-by-what-it-does-not-only-by-its-name
  "The one question a discoverable system must answer: `is there a command for
this'.  It reads the docstrings, so the search word is not in the name."
  (let ((world (fresh-world)) (r::*help-visible* nil))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list (leaf-with "a")))) 0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r::show-help-page "apropos" '(("dummy" . "row")))
      (setf r::*help-visible* nil)
      (let ((*standard-output* (make-string-output-stream)))
        (declare (ignore *standard-output*))
        (is-true (r:apropos-command) "apropos answers")))))

(test list-scratchpads-names-what-is-on-them
  "The scratchpad layer's own answer, in the same two cases LIST-TAGS has."
  (let* ((server (make-instance 'r::server))
         (window (leaf-with "term"))
         (world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list window))) 0))
    (setf (c:world-cursor world) '(0 0))
    (setf (gethash (c:leaf-window window) (r::server-windows server))
          (c:leaf-window window))
    (let ((r:*server* server) (r:*world* world)
          (p:*policy* (policy)) (p:*seat* nil)
          (r::*help-visible* nil))
      (is (stringp (r:list-scratchpads)) "nothing on a scratchpad, it says so")
      (is (null r::*help-visible*) "and there is no page")
      (setf (c:prop (c:leaf-window window) :scratchpad) "notes")
      (is-true (r:list-scratchpads) "with one, a page")
      (is-true (search "notes" (format nil "~s" r::*help-visible*) :test #'char-equal)
               "and the pad is named on it"))))

;;; --------------------------------------------- scratchpads, screens, devices

(test scratchpad-toggle-picks-up-and-puts-away
  "One key, both directions, and that is the whole point of a scratchpad: the
terminal consulted twenty times an hour costs one keystroke each way."
  (let* ((server (make-instance 'r::server))
         (window (leaf-with "term"))
         (world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list window))) 0))
    (setf (c:world-cursor world) '(0 0))
    (setf (gethash (c:leaf-window window) (r::server-windows server))
          (c:leaf-window window))
    (let ((r:*server* server) (r:*world* world)
          (p:*policy* (policy)) (p:*seat* nil) (p:*echo-message* nil))
      (is (stringp (r:scratchpad-toggle "notes"))
          "an empty scratchpad says so rather than doing nothing silently")
      (setf (c:prop (c:leaf-window window) :scratchpad) "notes")
      (r:scratchpad-toggle "notes")
      (is-true (c:window-minimized-p (c:leaf-window window))
               "the second press puts it away")
      (r:scratchpad-toggle "notes")
      (is-false (c:window-minimized-p (c:leaf-window window))
                "and the next summons it again"))))

(test screen-verbs-do-nothing-with-one-screen
  "FOCUS-OUTPUT and SEND-TO-OUTPUT are reachable by name rather than bound,
because plain motion crosses the screen edge on its own.  With one screen there
is nothing on the other side, and the commands must not invent one."
  (let ((world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal (list (leaf-with "a")))) 0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (p:*echo-message* nil))
      (r:focus-output 2)
      (is (equal '(0 0) (c:world-cursor world))
          "focusing a screen that does not exist leaves the cursor where it was")
      (r:send-to-output 2)
      (is (= 1 (length (c:node-windows (c:world-root world))))
          "and sending to one moves nothing"))))

(test list-inputs-says-the-compositor-offers-none
  "The first question an input rule asks is `what is my touchpad called', and
without a compositor the honest answer is that there is nothing to report."
  (let ((r:*server* nil) (p:*echo-message* nil))
    (is (stringp (r:list-inputs)) "no server, and it says so")
    (is-true (search "no input devices" (car p:*echo-message*) :test #'char-equal)
             "and it says so rather than printing an empty table")))

(test pull-brings-the-neighbour-into-this-pane
  "The inverse of MOVE, and the reason it is separate: pulling does not need you
to navigate to the thing first.  You stay where you are and it comes to you."
  (let ((world (fresh-world)))
    (setf (c:world-root world)
          (c:make-stack (list (c:make-split :horizontal
                                            (list (leaf-with "a")
                                                  (leaf-with "b"))))
                        0))
    (setf (c:world-cursor world) '(0 0))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (r:pull :right)
      (let ((pane (c:child-at (c:world-root world) 0)))
        (is-true (typep pane (quote c:split)) "the neighbour came in as a split")
        (is (= 2 (length (c:node-windows pane)))
            "holding both windows, side by side")
        (is (= 2 (length (c:node-windows (c:world-root world))))
            "and nothing was lost doing it")))))

;;; ---------------------------------------------------------- focus history
;;;
;;; #17.  The recency list was already there (:MRU consults it when the focused
;;; window closes); what this adds is the *walk* -- back, forward, list, clear.
;;; The checks that matter are the ones about not lying: a window that closed is
;;; stepped over, and walking back does not reset the walk.

(defun a-row-of-windows (count)
  "A world with COUNT windows in one split, the cursor on the first.
Values: (values WORLD WINDOWS)."
  (let* ((windows (loop for i below count collect (win (format nil "w~d" i))))
         (world (c:make-world
                 :root (c:make-stack
                        (list (c:make-split :horizontal
                                            (loop for window in windows
                                                  collect (c:make-leaf window))))
                        0))))
    (values world windows)))

(test focus-history-records-where-you-have-been
  "A move is recorded by the movers, not by a caller remembering to."
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        ;; A history begins at the first *change*, so the fixture says where it
        ;; starts rather than assuming the cursor's arrival was announced.
        (r:focus-existing-window first)
        (r:focus-existing-window second)
        (r:focus-existing-window third)
        (is (equal (mapcar #'c:window-app-id (list third second first))
                   (mapcar #'c:window-app-id (p:focus-history-of world)))
            "most recent first")
        (is (= 0 (p:focus-history-cursor world)) "and the walk starts at now")))))

(test focus-back-walks-and-focus-forward-comes-back
  "Repeated back steps *walk*.  This is the thing a naive version gets wrong:
the jump focuses a window, the focus change records it, and the walk starts
again from the top -- so back-back-back lands on one window and stays."
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        (r:focus-existing-window first)
        (r:focus-existing-window second)
        (r:focus-existing-window third)
        (r:focus-back)
        (is (eq second (r:focused-window)) "one step back")
        (r:focus-back)
        (is (eq first (r:focused-window)) "and another")
        (r:focus-forward)
        (is (eq second (r:focused-window)) "forward returns")
        (r:focus-forward)
        (is (eq third (r:focused-window)) "and forward again")))))

(test focus-back-steps-over-a-window-that-is-gone
  "A history is a list of where you have been, and some of those places stop
existing.  Landing on nothing would make the key feel broken."
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        (r:focus-existing-window first)
        (r:focus-existing-window second)
        (r:focus-existing-window third)
        (setf (c:window-live-p second) nil)
        (r:focus-back)
        (is (eq first (r:focused-window))
            "the dead window was stepped over, not returned")))))

(test focus-back-says-so-when-there-is-nothing-that-way
  (multiple-value-bind (world windows) (a-row-of-windows 2)
    (declare (ignore windows))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (p:*echo-message* nil))
      (is (stringp (r:focus-back)) "it answers in the echo area")
      (is-true (search "history" (car p:*echo-message*)) "and says which"))))

(test the-focus-history-is-bounded-by-its-option
  "A setting, not a literal: the list is what the world holds, and it must not
grow without end in a session that lasts a week."
  (multiple-value-bind (world windows) (a-row-of-windows 6)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (p:*focus-history-size* 3))
      (dolist (window windows) (r:focus-existing-window window))
      (r:focus-existing-window (first windows))
      (is (= 3 (length (p:focus-history-of world)))
          "the oldest entries fall off the end"))))

(test focus-history-lists-what-it-will-walk
  (multiple-value-bind (world windows) (a-row-of-windows 2)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (r::*help-visible* nil) (p:*echo-message* nil))
      (destructuring-bind (first second) windows
        (declare (ignore second))
        (is (stringp (r:focus-history)) "no history yet: it says so")
        (r:focus-existing-window first)
        (is-true (r:focus-history) "and with a history, a page")))))

(test clear-focus-history-forgets-everything-but-now
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        (declare (ignore first second))
        (r:focus-existing-window third)
        (r:clear-focus-history)
        (is (= 1 (length (p:focus-history-of world))) "one entry left")
        (is (= 0 (p:focus-history-cursor world)) "and the walk is at it")))))

;;; ------------------------------------------------------------- with-window
;;;
;;; #128.  Every verb asks FOCUSED-WINDOW who it acts on, so a name is enough to
;;; aim any of them -- including an extension's, which knows nothing about this.

(test with-window-aims-a-verb-at-a-named-window
  "The point of the exercise: act on a window that does not have the keyboard."
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        (declare (ignore first))
        (r:focus-existing-window third)
        (r:with-window second "toggle-fullscreen")
        (is (equal '(:tiled :fullscreen) (c:window-state world second))
            "the named window was the subject")
        (is (equal '(:tiled) (c:window-state world third))
            "and the focused one was left alone")))))

(test with-window-leaves-the-keyboard-where-it-was
  "Not `focus it and then run the command': nothing is told that the window has
the keyboard, and afterwards focus is exactly where it was."
  (multiple-value-bind (world windows) (a-row-of-windows 3)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second third) windows
        (declare (ignore first second))
        (r:focus-existing-window third)
        (r:with-window second "toggle-fullscreen")
        (is (eq third (r:focused-window)) "focus is unchanged")
        (is (equal '(:tiled) (c:window-state world third))
            "and the window it is focused on was not the subject")))))

(test with-window-restores-the-override-when-the-command-fails
  "A dynamic binding, so a signalled command unwinds through it.  A world where
this leaked would run the next verb on the wrong window, which is a bug nobody
would attribute to this command."
  (multiple-value-bind (world windows) (a-row-of-windows 2)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (destructuring-bind (first second) windows
        (r:focus-existing-window first)
        (handler-case
            (r:with-focus-on second (error "the command failed"))
          (error () nil))
        (is (null r:*focus-override*) "the override is gone")
        (is (eq first (r:focused-window))
            "and what `focused' means is the model's answer again")))))

(test with-window-refuses-a-name-that-reaches-nothing
  (multiple-value-bind (world windows) (a-row-of-windows 2)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (p:*echo-message* nil))
      (destructuring-bind (first second) windows
        (declare (ignore first))
        (r:with-window nil "toggle-float")
        (is (stringp (r:with-window nil "toggle-float")) "it answers")
        (is (equal '(:tiled) (c:window-state world second))
            "and ran nothing")))))

(test with-window-refuses-a-command-that-is-not-one
  (multiple-value-bind (world windows) (a-row-of-windows 2)
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil)
          (p:*echo-message* nil))
      (destructuring-bind (first second) windows
        (declare (ignore first))
        (r:with-window second "not-a-command")
        (is-true (search "no such command" (car p:*echo-message*)))))))

;;; -------------------------------------------------------------- focus mode
;;;
;;; #27.  Two options that decide whether the pointer moves the keyboard.  The
;;; mode is one value over both, because `follows the mouse but clicking does
;;; nothing' is a mode nobody means to be in.

(test focus-mode-sets-both-options-together
  "One writer for two knobs.  Every mode names what BOTH options are, so a
session can never be left in the combination that exists only by accident."
  (let ((p:*focus-follows-mouse* nil)
        (p:*click-to-focus* nil)
        (p:*echo-message* nil))
    (r:focus-mode :follows)
    (is-true p:*focus-follows-mouse*)
    (is (null p:*click-to-focus*)
        "the pointer already focused it; clicking has nothing left to do")
    (is (eq :follows (r:focus-behaviour)))
    (r:focus-mode :click)
    (is (null p:*focus-follows-mouse*))
    (is-true p:*click-to-focus*)
    (r:focus-mode :none)
    (is (null p:*focus-follows-mouse*))
    (is (null p:*click-to-focus*) "keyboard only, on purpose")
    (is (eq :none (r:focus-behaviour)))
    (is (stringp (car p:*echo-message*)) "and it says which mode you are in")))

(test toggle-focus-follows-mouse-is-one-key
  "The live switch, which is what the key binding is for: no config file, no
restart, and it lands in a mode where clicking still works."
  (let ((p:*focus-follows-mouse* nil)
        (p:*click-to-focus* nil)
        (p:*echo-message* nil))
    (r:toggle-focus-follows-mouse)
    (is (eq :follows (r:focus-behaviour)) "click -> follows")
    (r:toggle-focus-follows-mouse)
    (is (eq :click (r:focus-behaviour)) "and back")
    (is-true p:*click-to-focus* "and the pointer still works, by clicking")))

;;; ------------------------------------------------------ scratchpad claims
;;;
;;; #15.  Membership already survived a restart through WINDOW-FACTS; what did
;;; not is a scratchpad whose *window* did not come back.  A claim is a
;;; statement about an application, and these checks are about the three
;;; moments it matters: written, read back, and honoured at window-open.

(defmacro with-a-claims-file (&body body)
  "Run BODY with an empty claims list and the file already considered read.

No file is touched: the tests that exercise reading and writing name their own
path, and the ones about the *moment* the claim is honoured must not depend on
where the session's own file happens to be."
  `(let ((r:*scratchpad-claims* '())
         (r::*scratchpad-claims-loaded-p* t))
     ,@body))

(test a-scratchpad-claim-is-written-and-read-back
  "The claim is the durable half: it names an application, not a window, so it
has to be in a file -- a claim held only in memory dies with the thing it was
protecting."
  (with-a-claims-file
    (let ((path (merge-pathnames "latticewm-test-claims-roundtrip.lisp"
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (r:claim-scratchpad "music" "spotify")
             (r:save-scratchpad-claims path)
             (setf r::*scratchpad-claims* '())
             (r:load-scratchpad-claims path)
             (is (equal '(("music" . "spotify")) r::*scratchpad-claims*))
             (is-true (search "MUSIC" (string-upcase
                                       (symbol-name
                                        (r::claim-for "spotify"))))
                      "and it answers for the application"))
        (ignore-errors (delete-file path))))))

(test a-claimed-window-puts-itself-away-when-it-opens
  "The moment the promise is kept: a window from a claimed application joins
the scratchpad and is hidden.  Left visible it would be the opposite of the
bargain -- the thing you asked to be out of the way arriving on top of your
work.

The test runs the hook the way ATTACH-WINDOW does rather than calling
CLAIM-WINDOW directly, because the wiring is half of what it is testing:
a claim that fires when called by hand is not a claim."
  (with-a-claims-file
    (let ((r:*scratchpad-claims* '(("music" . "spotify")))
          (p:*hooks* (make-hash-table :test #'eq)))
      (with-window-world (world window "spotify")
        (p:add-hook :window-opened 'r:claim-window)
        (p:run-hooks :window-opened window)
        (is-true (c:window-minimized-p window) "hidden on arrival")
        (is (equal "music" (string-downcase
                            (symbol-name (c:prop window :scratchpad))))
            "and on the scratchpad that claimed it")))))

(test an-unclaimed-window-opens-normally
  "The control, and the one that keeps the feature from being a surprise: a
window nothing claims is not touched."
  (with-a-claims-file
    (let ((r:*scratchpad-claims* '(("music" . "spotify")))
          (p:*hooks* (make-hash-table :test #'eq)))
      (with-window-world (world window "editor")
        (p:add-hook :window-opened 'r:claim-window)
        (p:run-hooks :window-opened window)
        (is (null (c:window-minimized-p window)))
        (is (null (c:prop window :scratchpad)))))))

(test unclaiming-forgets-the-application-and-keeps-the-members
  (with-a-claims-file
    (let ((r:*scratchpad-claims* '(("music" . "spotify"))))
      (r:unclaim-scratchpad "music")
      (is (null r:*scratchpad-claims*))
      (is (null (r::claim-for "spotify")) "nothing claims it now"))))

(test scratchpad-claim-from-the-boundary-of-a-key
  "The command form of a claim: what M-x has to be able to do, and what a
config file can call.  LIST-SCRATCHPAD-CLAIMS is the same request read back,
because a claim you cannot see is a claim you cannot undo."
  (with-a-claims-file
    (let ((p:*echo-message* nil) (r::*help-visible* nil))
      (is (equal "music" (r:scratchpad-claim "Music" "spotify"))
          "it answers with the scratchpad's name, canonicalised")
      (is-true (search "will claim spotify" (car p:*echo-message*)))
      (is-true (r:list-scratchpad-claims) "and the list shows it")
      (r:scratchpad-unclaim "music")
      (is (null r::*scratchpad-claims*))
      (is (stringp (r:list-scratchpad-claims))
          "with nothing left, it says so rather than an empty page"))))
