;;;; tests/test-hidden.lisp --- The hidden set (#6).
;;;;
;;;; The issue's scope note says most of this feature already exists -- minimize
;;;; takes a window out of the tree, restore puts it back in its slot, and #34
;;;; completes over every window.  So what is checked here is exactly the delta:
;;;; that the hidden set can be *listed*, that a picker over it offers the
;;;; hidden windows and nothing else, and that a whole application's windows can
;;;; come back at once.

(in-package #:latticewm/tests)
(in-suite hidden)

;;; ------------------------------------------------------------------ fixtures

(defun a-world-of-three (a b c)
  "A world with an empty workspace and three windows to place in it.

EMPTY FIRST, THEN PLACED THROUGH ON-WINDOW-OPEN, and not a tree built by hand:
a hand-built root puts a leaf where the workspace stack belongs, which is a
mistake this suite has made in another life and the reason the placement verbs
exist.  It also means each window has the home path the program would have
recorded, so `back where it was' is being checked rather than assumed."
  (declare (ignore a b c))
  (c:make-world :root (c:make-stack (list (c:make-leaf)) 0)))

(defmacro with-hidden-world ((a b c) &body body)
  "A row of three windows, of which A and B are on no screen."
  (let ((world (gensym "WORLD")))
    `(let* ((,a (win "terminal"))
            (,b (win "files"))
            (,c (win "editor"))
            (,world (a-world-of-three ,a ,b ,c))
            (r:*world* ,world)
            (p:*policy* (policy))
            (p:*seat* nil)
            (p:*echo-message* nil))
       (declare (ignorable ,a ,b ,c))
       ;; Placed first, then taken out of the layout: through the *verbs*, so
       ;; the home path each one remembers is the one the program would record.
       (dolist (window (list ,a ,b ,c))
         (p:on-window-open p:*policy* ,world window))
       (r:minimize-window ,a)
       (r:minimize-window ,b)
       ,@body)))

;;; ------------------------------------------------------------- the answered set

(test the-hidden-set-is-the-scratchpad-in-hiding-order
  "Most recently hidden first, and the window still on screen is not in it."
  (with-hidden-world (a b c)
    (is (equal (list b a) (r:hidden-windows))
        "B was hidden last, so it is first")
    (is (not (member c (r:hidden-windows)))
        "and the window in the pane is not hidden")))

(test hidden-applications-are-distinct-and-sorted
  (with-hidden-world (a b c)
    (is (equal '("files" "terminal") (r:hidden-applications))
        "one entry per program, in the order a reader scans")))

(test where-a-hidden-window-will-return-is-said-in-words
  "The one detail a list of hidden windows cannot get from the window: RESTORE
puts it back in the slot it left *if that slot still exists*, so two hidden
windows differ by place and a report of titles alone would leave the reader
guessing.  Both readings are reachable and both are worth a check."
  ;; The slot survived: a window alone in its workspace leaves an empty pane
  ;; where it was, and the place is still its own.
  (let ((a (win "terminal"))
        (world (c:make-world :root (c:make-stack (list (c:make-leaf)) 0))))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (p:on-window-open p:*policy* world a)
      (r:minimize-window a)
      (is (string= "back where it was" (r:hidden-return-place a)))))
  ;; The slot did not survive: taking one window out of a row of three
  ;; reflows the row, so the pane it left belongs to somebody else now.
  (with-hidden-world (a b c)
    (is (string= "into the pane you are in" (r:hidden-return-place a))
        "a report that promised the old slot here would be a lie the restore
would have to tell")))

;;; ------------------------------------------------------------- the picker

(test the-hidden-picker-offers-the-hidden-windows-and-nothing-else
  "REACHING THREE YOU CANNOT SEE THROUGH FORTY YOU CAN is the difficulty this
command removes, so the check that matters is a negative one."
  (with-hidden-world (a b c)
    (let ((labels (mapcar #'car (r:hidden-picker-entries))))
      (is (= 2 (length labels)))
      (is (find (p:name-of-window a) labels :test #'string=))
      (is (find (p:name-of-window b) labels :test #'string=)))
    (is (null (r::window-by-picker-name (p:name-of-window c)
                                        (r:hidden-windows)))
        "the visible window is not reachable through this picker, which is what
makes it a different picker rather than the same one with a filter")))

(test two-hidden-windows-of-the-same-program-get-the-same-labels-as-any-picker
  (let* ((one (win "terminal")) (two (win "terminal")) (three (win "files"))
         (world (a-world-of-three one two three)))
    (let ((r:*world* world) (p:*policy* (policy)) (p:*seat* nil))
      (dolist (window (list one two three))
        (p:on-window-open p:*policy* world window))
      (r:minimize-window one)
      (r:minimize-window two)
      (let ((labels (mapcar #'car (r:hidden-picker-entries))))
        (is (= 2 (length labels)) "two windows, two entries")
        (is (not (equal (first labels) (second labels)))
            "and two labels -- a picker that offered the same word twice would
make Return a coin flip")
        (is (search "(" (first labels))
            "disambiguated the way #34's picker spells a shared name")))))

;;; ------------------------------------------------------------- the report

(test list-hidden-says-what-is-hidden-and-where-it-goes
  (with-hidden-world (a b c)
    (let ((r::*help-visible* nil))
      (is (= 2 (length (r:list-hidden))))
      (is (consp r::*help-visible*) "the page is up")
      (let ((rows (cdr r::*help-visible*)))
        (is (= 2 (length rows)))
        (is (search "into the pane you are in" (cdr (first rows)))
            "each row says where its window will come back to")
        (is (find-if (lambda (row) (search "terminal" (cdr row))) rows)
            "and which program it belongs to, which is the other thing a list
of titles does not say")))))

(test list-hidden-on-an-empty-set-says-so-and-draws-no-page
  (let ((r:*world* (c:make-world)) (p:*policy* (policy)) (p:*echo-message* nil))
    (let ((r::*help-visible* nil))
      (is (null (r:list-hidden)))
      (is (null r::*help-visible*) "no page for a list of nothing")
      (is (search "nothing is hidden" (p:current-message))))))

;;; ------------------------------------------------------------- the verbs

(test restore-all-brings-every-hidden-window-back
  "A hidden set with no way to empty it is a set that grows and is never
audited -- the issue's own third gap, and this is the answer to it."
  (with-hidden-world (a b c)
    (is (= 2 (r:restore-all)))
    (is (null (r:hidden-windows)))
    (is (notany #'c:window-minimized-p (list a b c)))
    (is (= 3 (length (c:node-windows (c:world-root r:*world*))))
        "all three are in the tree again, not merely on a list")))

(test restore-app-brings-back-one-program-and-leaves-the-rest-hidden
  (with-hidden-world (a b c)
    (is (= 1 (r:restore-app "terminal")))
    (is (= 1 (length (r:hidden-windows)))
        "the other program's window is still put away")
    (is (string= "files" (c:window-app-id (first (r:hidden-windows)))))
    (is (not (c:window-minimized-p a)))))

(test restoring-a-program-that-has-nothing-hidden-changes-nothing
  (with-hidden-world (a b c)
    (let ((before (copy-list (r:hidden-windows))))
      (is (null (r:restore-app "no-such-app")))
      (is (equal before (r:hidden-windows)))
      (is (search "no hidden window belongs to" (p:current-message))))))

(test the-hidden-commands-exist
  (is-true (p:find-command "list-hidden"))
  (is-true (p:find-command "hidden"))
  (is-true (p:find-command "restore-app"))
  (is-true (p:find-command "restore-all"))
  (is-true (fboundp 'r:show-hidden-window)))
