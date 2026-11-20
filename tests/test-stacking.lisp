;;;; tests/test-stacking.lisp --- raise, lower, pin.
;;;;
;;;; #13.  Tiled windows do not overlap, so stacking is a float question twice
;;;; over: where in the overlap (raise, lower) and who wins it permanently
;;;; (pin).  RENDER-ORDER is where the policy says who is on top, and the checks
;;;; that matter are about PERMANENCE -- a pinned float still wins after
;;;; something else is raised -- and about the list staying the user's recency
;;;; order rather than being quietly re-sorted under them.

(in-package #:latticewm/tests)
(in-suite stacking)

;;; ------------------------------------------------------------------ fixtures

(defun two-floats-in-a-world ()
  "A world with two floating windows.  SECOND is the most recent, so it is on
top: WORLD-FLOATS is OLDEST FIRST and the policy draws it in order, so the END
of the list is the top of the pile -- which is what RAISE and LOWER move.
Returns (values world first second float-of-first float-of-second)."
  (let* ((first (make-instance 'c:window :app-id "one" :identifier "1"))
         (second (make-instance 'c:window :app-id "two" :identifier "2"))
         (float-first (make-instance 'c:floating-window
                                     :window first
                                     :rect (c:make-rect 0 0 100 50)))
         (float-second (make-instance 'c:floating-window
                                      :window second
                                      :rect (c:make-rect 10 10 100 50)))
         (world (c:make-world)))
    (setf (c:world-outputs world)
          (list (make-instance 'c:output :name "screen"
                               :rect (c:make-rect 0 0 1280 720))))
    (p:ensure-workspaces-for-outputs world)
    (setf (c:window-floating-p first) t
          (c:window-floating-p second) t
          (c:world-floats world) (list float-first float-second))
    (values world first second float-first float-second)))

(defmacro with-two-floats ((world first second float-first float-second)
                           &body body)
  `(multiple-value-bind (,world ,first ,second ,float-first ,float-second)
       (two-floats-in-a-world)
     (let ((r:*world* ,world) (p:*policy* (policy)) (p:*seat* nil))
       ,@body)))

(defun drawn-order (world)
  "The windows RENDER-ORDER puts on screen, last-drawn last.

Asked the way the runtime asks: floats are handed to the policy as placements
(they are not in the tree, so they can arrive no other way), and the answer is
the list in draw order -- so the LAST window in it is the one on top."
  (let* ((output (first (c:world-outputs world)))
         (placements (cons (list (c:world-root world) output)
                           (mapcar (lambda (float)
                                     (list (c:make-leaf (c:float-window float))
                                           nil))
                                   ;; oldest first, exactly as the world has
                                   ;; them, so the top of the pile is last.
                                   (c:world-floats world)))))
    (mapcar (lambda (placement) (c:leaf-window (first placement)))
            (remove-if-not (lambda (placement)
                             (typep (first placement) 'c:leaf))
                           (p:render-order p:*policy* placements)))))

;;; ----------------------------------------------------------------- the verbs

(test raise-window-puts-the-focused-float-on-top
  "RAISE moves the window in the world's list, and the policy draws the list
in order -- so the move is the whole statement."
  (with-two-floats (world first second float-first float-second)
    (declare (ignore second))
    (setf (c:world-focused-float world p:*seat*) float-first)
    (r:raise-window)
    (is (eq float-first (car (last (c:world-floats world))))
        "now the newest, so drawn last")
    (is (eq first (car (last (drawn-order world))))
        "and the policy puts it on top of the other float")))

(test lower-window-sends-the-focused-float-to-the-bottom
  (with-two-floats (world first second float-first float-second)
    (declare (ignore first second))
    (setf (c:world-focused-float world p:*seat*) float-first)
    (r:lower-window)
    (is (eq float-first (first (c:world-floats world)))
        "the lowered window is now the oldest, so drawn first")
    (is (eq second (car (last (drawn-order world))))
        "and the other float is on top")))

(test raise-and-lower-refuse-a-tiled-window-by-saying-why
  "`Nothing happened' is the worst answer a command can give; the reason is
that tiled windows do not overlap, which is a fact about the layout and not a
failure."
  (with-window-world (world window "note")
    (let ((r::*echo-message* nil))
      (is (stringp (r:raise-window)))
      (is-true (search "overlap" (car r::*echo-message*)))
      (is (stringp (r:lower-window))))))

;;; --------------------------------------------------------------------- pin

(test lower-float-is-the-command-form-of-lower-window
  "Two names for one verb, because the pair reads as a pair: a key bound to
`raise-float' and one bound to `lower-float' is the sentence a user wants, and
the subject-neutral RAISE-WINDOW is what WITH-WINDOW aims at a named window."
  (with-two-floats (world first second float-first float-second)
    (declare (ignore first second))
    (setf (c:world-focused-float world p:*seat*) float-first)
    (r:lower-float)
    (is (eq float-first (first (c:world-floats world))))))

(test unpin-window-lets-a-pinned-window-stack-again
  "The focus model is the fixture's, not this test's: with the window in the
tree the cursor is on it, so `the focused window' is unambiguous."
  (with-window-world (world window "note")
    (r:pin-window)
    (is-true (c:window-pinned-p window))
    (r:unpin-window)
    (is (null (c:window-pinned-p window)) "unpinned, still floating")))

(test a-pinned-window-stays-on-top-of-anything-raised-after-it
  "The temptation is to implement pin as `raise it again whenever anything
moves'.  This is the check that would catch that: raise an unpinned float and
the pinned one still wins, because the thing you pinned is the reference you are
working against -- if it stepped aside, pinning would be noise."
  (with-two-floats (world first second float-first float-second)
    (declare (ignore second))
    (setf (c:window-pinned-p first) t)
    (is (eq first (car (last (drawn-order world))))
        "pinned, the older float is on top")
    ;; now raise the OTHER one, which is the case that has to still lose
    (setf (c:world-focused-float world p:*seat*) float-second)
    (r:raise-window)
    (is (eq float-second (car (last (c:world-floats world))))
        "the raised one is newest")
    (is (eq first (car (last (drawn-order world))))
        "and still drawn under the pinned one")))

(test pin-window-floats-a-tiled-window
  "A pinned window has to be able to overlap something to be above it, so
pinning a tile floats it -- the same promise the window rule's :PIN makes."
  (with-window-world (world window "note")
    (is (equal '(:tiled) (c:window-state world window)))
    (r:pin-window)
    (is (equal '(:floating) (c:window-state world window)))
    (is-true (c:window-pinned-p window))))

(test toggle-pin-flips-and-says-which
  (with-two-floats (world first second float-first float-second)
    (declare (ignore first float-first))
    (let ((r::*echo-message* nil))
      (setf (c:world-focused-float world p:*seat*) float-second)
      (r:toggle-pin)
      (is-true (c:window-pinned-p second))
      (is-true (search "pinned" (car r::*echo-message*)))
      (r:toggle-pin)
      (is (null (c:window-pinned-p second)))
      (is-true (search "unpinned" (car r::*echo-message*))))))

(test a-window-rule-opens-a-window-pinned-and-floating
  "The declarative half, and it costs no second mechanism: :PIN is a rule key
that floats the window (tiles do not stack) and sets the fact."
  (let ((p:*window-rules* '(((:app-id "note") :pin t))))
    (with-window-world (world window "note")
      (is (equal '(:floating) (c:window-state world window)))
      (is-true (c:window-pinned-p window)))))

;;; ---------------------------------------------------------------- the facts

(test pin-survives-a-save-and-restore
  "Pinned is a fact about the window -- like floating and fullscreen (#25) --
so a pinned reference window is still pinned in the session you come back to."
  (with-two-floats (world first second float-first float-second)
    (declare (ignore float-first float-second))
    (setf (c:window-pinned-p second) t)
    (let ((saved (r:window-facts (list second))))
      (is-true (getf (rest (first saved)) :pinned)
               "the row says so")
      (setf (c:window-pinned-p second) nil)
      (let ((index (make-hash-table :test #'equal)))
        (setf (gethash (c:window-identifier second) index) second)
        (r:restore-window-facts world saved index))
      (is-true (c:window-pinned-p second) "and it comes back"))))
