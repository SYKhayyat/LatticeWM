;;;; policy/structure.lisp --- The shipped answers for STRUCTURE-POLICY.
;;;;
;;;; Where things go when the tree changes: spawning, splitting, joining,
;;;; sizing, tabbing.  This is where DESIGN's P1 lands hardest — where a fork
;;;; is situational rather than principled, both options ship and a tier-0
;;;; value picks the default — so nearly every method here reads an option and
;;;; nearly every option here is a fork somebody argued about.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; STRUCTURE
;;; ==================================================================

(define-option *new-workspace* :empty
  "What a workspace contains at the moment it comes into existence.

  :EMPTY      one empty pane.  DESIGN D19's starting state, and the shipped
              answer: a place with nothing in it, where typing a key spawns
              something.
  a FUNCTION  called with (WORLD INDEX); whatever node it returns is the new
              workspace.  Returning NIL falls back to :EMPTY rather than
              putting a hole in the workspace list.

Two values and not a vocabulary of ten.  Anything a further keyword could have
named, a two-line lambda names better and in the user's own words:

    (setf *new-workspace*
          (lambda (world index)
            (declare (ignore world index))
            (c:make-split :horizontal (list (c:make-leaf) (c:make-leaf)))))

Specialize MAKE-WORKSPACE instead when the answer belongs to a *policy* rather
than to a person — that is what the lattice does, and it is why every workspace
born after `lattice:enable' is a plane rather than a pane.

WHICH MEANS THIS OPTION IS NOT READ AT ALL UNDER THE LATTICE, and that used to
be a sentence you had to find here and trust.  It is a fact about the image
now: `latticewm --extension-surface' lists the overriding method under this
option as an `overridden by', and gate 15 requires an override of that shape to
offer an option of its own — the lattice offers *NEW-WORKSPACE-CELLS*.")

(defmethod make-workspace ((policy structure-policy) world index)
  "An empty pane, or whatever *NEW-WORKSPACE* names.

The option is read here rather than at the four sites that grow the workspace
list, for the reason every other option is read in exactly one place: a fork
the user can set must have one place it is answered, or the four sites drift.

A function that errors or returns NIL gets the empty pane.  Falling back rather
than signalling is deliberate — this runs on the path that creates a workspace
you are in the middle of switching to, and a bad lambda in a configuration file
should cost you a log line and a plain workspace, not the switch."
  (let ((answer *new-workspace*))
    (or (typecase answer
          (function (guarded "*new-workspace*" (funcall answer world index)))
          (symbol (when (and (not (keywordp answer)) (fboundp answer))
                    (guarded "*new-workspace*" (funcall answer world index)))))
        (c:make-leaf))))

(defmethod split-axis-for ((policy structure-policy) node rect)
  "Cut along the longer side, so panes tend towards square."
  (declare (ignore node))
  (ecase *split-axis*
    (:longer (if (>= (c:rect-w rect) (c:rect-h rect)) :horizontal :vertical))
    (:horizontal :horizontal)
    (:vertical :vertical)))

(defmethod new-child-side ((policy structure-policy) node direction)
  (declare (ignore node))
  (cond ((null direction) *new-child-side*)
        ((member direction '(:right :down)) :after)
        (t :before)))

(defmethod should-collapse-p ((policy structure-policy) (split c:split))
  *collapse-degenerate-splits*)

(defmethod should-collapse-p ((policy structure-policy) (stack c:stack))
  "A one-workspace workspace list is a thing you are about to add to, not
debris."
  nil)

(defmethod should-collapse-p ((policy structure-policy) container)
  (declare (ignore container))
  t)

(defmethod move-into-occupied ((policy structure-policy) world from to)
  (declare (ignore world from to))
  *move-into-occupied*)

(defmethod insertion-weight ((policy structure-policy) (split c:split) address)
  (c:default-insertion-weight split address))

;;; AND THE HALF THAT MAKES THE ABOVE MEAN ANYTHING.
;;;
;;; INSERTION-WEIGHT existed, was documented, and was ignored: SPLIT's
;;; INSERT-CHILD :AFTER method inlined the mean-weight formula rather than
;;; calling it, so a policy that specialised the generic changed nothing on the
;;; one path where a weight is actually chosen.  That is the project's own
;;; stated rule broken — *when an extension point exists, nothing may implement
;;; its behaviour except through it* — and it is invisible, because both the
;;; generic and the inlined formula were correct.
;;;
;;; model/ is pure and may not call a policy, so the decision crosses the line
;;; as a closure, exactly the way SPLIT-JOIN-PREDICATE does.  Set here rather
;;; than at startup so that it holds in a unit test and at a REPL as well as in
;;; a running session.
(setf c:*insertion-weight-function*
      (lambda (split address) (insertion-weight (current-policy) split address)))

;;; ------------------------------------------- the split mechanism as policy
;;;
;;; These four replace the (TYPEP x 'SPLIT) tests that used to sit inline in
;;; RESIZE, EQUALIZE, EQUALIZE-ALL, TAB and TREE-SPLIT-AT.  Each fallback is
;;; specialized on T rather than on CONTAINER, deliberately: the callers walk
;;; up parent chains and hand in NIL at the top, and a protocol that signals
;;; NO-APPLICABLE-METHOD at the root is a protocol with a trap in it.  This is
;;; the same lesson LAYOUT-CHILDREN's fallback records above — a partial
;;; operation is not an extension point.

(defmethod container-axis ((policy structure-policy) container)
  "Ask the container itself, through the structural protocol.

Not (TYPEP CONTAINER 'SPLIT): the question is whether this container divides
space along an axis, and a container kind from outside the core that divides
space exactly the way a split does should answer yes.  CONTAINER-SPLITS-ALONG-P
is where it says so, and it is in the core rather than only here because
model/surgery.lisp needs the same answer and may not reach for a policy."
  (when (c:container-p container)
    (cond ((c:container-splits-along-p container :horizontal) :horizontal)
          ((c:container-splits-along-p container :vertical) :vertical))))

(define-option *resize-amount* 1/20
  "How much one press of the resize key moves a divider.

A fraction of the container: 1/20 is five percent of the pane's parent, which
is about the smallest step that is visible in one press and the largest that
does not overshoot.  RESIZE takes it as an optional argument, so a binding can
ask for a coarser one without changing this.")

(defmethod resize-container ((policy structure-policy) container address amount)
  "Nothing to resize in a container that does not divide space."
  (declare (ignore container address amount))
  nil)

(defmethod resize-container ((policy structure-policy) (split c:split) address amount)
  "Translate a fraction of the whole into a transfer between two weights.

The weights sum to whatever they sum to — nothing normalises them — so a
fraction of the container is that fraction *of the sum*.  This is the arithmetic
the resize verb used to inline as a literal 4, which was the sum only when the
container happened to hold four evenly-weighted children."
  (let ((total (reduce #'+ (c:weights split) :initial-value 0)))
    (when (plusp total)
      (c:adjust-weight split address (* amount total))
      t)))

(defmethod equalize-container ((policy structure-policy) container)
  (declare (ignore container))
  nil)

(defmethod equalize-container ((policy structure-policy) (split c:split))
  (setf (c:weights split)
        (make-list (c:container-count split) :initial-element 1))
  t)

(defmethod tab-siblings ((policy structure-policy) container address)
  (declare (ignore container address))
  nil)

(defmethod tab-siblings ((policy structure-policy) (split c:split) address)
  "This child and its next sibling — or its previous one, at the end of a row.

The INTEGERP guard is not paranoia.  A split addresses its children by index,
but the container protocol does not require that of every kind: the lattice
addresses by coordinate, and (1+ '(2 . 3)) is an error rather than a wrong
answer.  Guarding here means a policy that inherits this method for some other
kind declines instead of breaking."
  (let ((count (c:container-count split)))
    (when (and (integerp address) (> count 1) (< -1 address count))
      (let ((other (if (< (1+ address) count) (1+ address) (1- address))))
        (values (min address other) (max address other))))))

(defmethod join-existing-split-p ((policy structure-policy) container axis)
  (eq (container-axis policy container) axis))

(defun split-join-predicate (&optional (policy (current-policy)))
  "POLICY's JOIN-EXISTING-SPLIT-P, as the predicate TREE-SPLIT-AT wants.

src/model/ is pure and must not reach for a policy, so the decision crosses
that line as a closure rather than as a call.  This is the one place the two
representations meet, which is the point: without it every caller would build
the same lambda and one of them would eventually build a different one."
  (lambda (parent axis) (join-existing-split-p policy parent axis)))

;;; ------------------------------------------------------ targeted spawn
;;
;; WHERE THE NEXT WINDOW GOES IS POLICY, and policy reads the cursor.  That is
;; right for the overwhelming majority of spawns — you open a thing where you are
;; looking — and it leaves one intention unsayable: 'put the next one THERE',
;; pointing at a pane that is not where your keyboard currently rests.  Window
;; rules can place a window, but a rule is keyed off the window (its app-id, its
;; title) and requires knowing the absolute path; neither is a gesture.
;;
;; So the latch is a *place*, set by a command, consulted once by the ordinary
;; spawn path, and consumed.  It lives on the policy side because that is where
;; placement is decided: every policy that answers SPAWN-TARGET gets it, rather
;; than only the shipped one, and a policy that wants different consumption
;; rules can read the latch itself.

(defvar *spawn-target* nil
  "Where the next spawned window should land, as (PATH . COUNT), or NIL.

Set by TARGET-HERE and TARGET-HERE-CELL, consumed by SPAWN-TARGET.  A dynamic
variable rather than world state because it describes the person at the
keyboard — 'I am about to open something and I want it here' — and there is one
of those per session, not per world.")

(defun set-spawn-target (path &key (count 1))
  "Make the next COUNT spawned windows land at PATH.

COUNT is clamped to at least one: a latch that can hold zero is a latch that
was cleared, and having two ways to say that is how one of them gets missed."
  (setf *spawn-target* (and path (cons (copy-list path) (max 1 (or count 1)))))
  *spawn-target*)

(defvar *spawn-target-used-p* nil
  "T when the last SPAWN-TARGET call answered from the latch.

SET-SPAWN-TARGET consumes nothing and SPAWN-TARGET places nothing, so neither
can decrement honestly on its own; ON-WINDOW-OPEN is the one place that has
both the answer and the path the window actually landed on, and this flag is
how it knows the answer was the latch rather than the cursor.")

(defun clear-spawn-target ()
  "Drop the latch.  New windows go wherever placement's ordinary rules say."
  (setf *spawn-target* nil))

(defun spawn-target-latch (world)
  "The latched path, if it still names a place in WORLD, or NIL.

A latched path can go stale between the command and the spawn — the pane was
closed, a split collapsed, the workspace went away — and placing a window into
a path that names nothing is how a spawn loses a window.  A stale latch is
dropped and the ordinary target used, which is the same answer a policy gives
when a window rule names nothing."
  (let ((entry *spawn-target*))
    (when entry
      (destructuring-bind (path . ignored) entry
        (declare (ignore ignored))
        (let ((node (and path (c:resolve-path (c:world-root world) path))))
          (cond
            ((null node) (clear-spawn-target) nil)
            ((eq node (c:world-root world)) (clear-spawn-target) nil)
            (t path)))))))

(defun note-spawn-target-used (landed)
  "One window was placed LANDEED.  Spend one from the latch, and re-anchor it.

RE-ANCHORING AT WHAT WAS JUST PLACED is what makes a count read the way people
expect: `target-here 2' from a pane puts the first window beside that pane and
the second beside the *first window*, not beside the pane again — which would
insert every arrival at the same address and reverse their order on screen."
  (let ((entry *spawn-target*))
    (when entry
      (destructuring-bind (ignored . count) entry
        (declare (ignore ignored))
        (if (<= count 1)
            (clear-spawn-target)
            (set-spawn-target landed :count (1- count)))))))

(defmethod spawn-target ((policy structure-policy) world window)
  "Split the focused pane, unless it is empty, in which case fill it.

Filling an empty pane rather than splitting it is not a special case bolted
on: an empty pane exists *because the user made a place for something*, so
putting the next thing there is the only reading that respects the gesture.

A TARGET-HERE latch wins over all of that, and :SPLIT is the right disposition
for it in every case PLACE-NODE cares about: an empty destination replaces, an
occupied pane joins it, and a whole cell joins that cell's own split.  :FILL
would have to ask the same question again and would be wrong for the cell."
  (declare (ignore window))
  (setf *spawn-target-used-p* nil)
  (let ((latched (spawn-target-latch world)))
    (if latched
        (progn (setf *spawn-target-used-p* t)
               (values latched :split))
        (let* ((path (c:world-cursor world *seat*))
               (leaf (c:world-leaf-at world path)))
          (cond
            ((and leaf (c:leaf-empty-p leaf)) (values path :fill))
            ((eq *spawn-mode* :stack) (values path :stack))
            ((eq *spawn-mode* :fill-first)
             ;; LEAF-PATHS of the workspace are relative to the workspace, so they
             ;; must be resolved against it and only then rebased onto the world.
             ;; Resolving a workspace-relative path against the world root is the
             ;; kind of mistake that silently does nothing, which is worse than
             ;; crashing.
             (let* ((workspace (c:current-workspace world))
                    (empty (find-if (lambda (candidate)
                                      (let ((node (c:resolve-path workspace candidate)))
                                        (and (typep node 'c:leaf)
                                             (c:leaf-empty-p node))))
                                    (c:leaf-paths workspace))))
               (if empty
                   (values (append (c:workspace-path world) empty) :fill)
                   (values path :split))))
            (t (values path :split)))))))
;;; ------------------------------------------------------ names and roles

(defmethod container-label ((policy structure-policy) container)
  (c:node-label container))

(defmethod container-role ((policy structure-policy) world container)
  "The root's alternatives are workspaces; anything else's are tabs."
  (cond ((not (and (c:container-p container)
                   (c:container-alternatives-p container)))
         nil)
        ((eq container (c:world-root world)) :workspaces)
        (t :tabs)))

(defun container-role-name (role &key plural)
  "ROLE as the word a person would say.

Singular by default because that is how it is used -- \"workspace 2 of 5\" --
and plural on request for the places that count them."
  (case role
    (:workspaces (if plural "workspaces" "workspace"))
    (:tabs (if plural "tabs" "tab"))
    (t (if plural "views" "view"))))

(defun world-role-name (world container &key plural (policy (current-policy)))
  "The word for what CONTAINER is in WORLD, guarded, with a sane fallback."
  (container-role-name (guarded "container-role"
                         (container-role policy world container))
                       :plural plural))
