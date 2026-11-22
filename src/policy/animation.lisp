;;;; policy/animation.lisp --- How long a thing the window manager draws takes
;;;; to appear (#9).
;;;;
;;;; THE ISSUE ASKED FOR THE COMPOSITOR'S PART OF THIS, AND THE COMPOSITOR IS
;;;; WHERE IT STAYS.  A Wayland client cannot move, scale or fade *another
;;;; client's surface*: a surface has one geometry, and every animated frame of
;;;; a window would be a compositor commit the window manager is not allowed to
;;;; make.  River has no animation request either -- no river_animation_v1, and
;;;; nothing in river_control that would put one there -- so `animate the open of
;;;; a window' is not a feature this program can ship at any price.  That is the
;;;; same finding as #29 (no axis event) and #16 (trackpad gestures are libinput's),
;;;; and it is now the fourth time: AN INPUT OR OUTPUT FEATURE THE COMPOSITOR
;;;; DOES NOT EXPOSE DOES NOT EXIST FOR THE WINDOW MANAGER.
;;;;
;;;; WHAT A WINDOW MANAGER CAN ANIMATE IS WHAT IT DRAWS.  Every overlay in this
;;;; program -- the keymap, the map, the peek, the echo area -- is this
;;;; program's own buffer on this program's own layer surface.  Those can fade,
;;;; and they are the surfaces a person actually notices arriving out of
;;;; nowhere.
;;;;
;;;; So the model here is deliberately small and independent of frames: a
;;;; transition, a curve and a duration, and a function of elapsed time that
;;;; answers a number in [0, 1].  The clock is the runtime's (frames.lisp), and
;;;; keeping the two apart is what makes this testable as arithmetic rather than
;;;; as a video.

(in-package #:latticewm/policy)

;;; ------------------------------------------------------------------- curves
;;;
;;; FOUR, AND NO MORE.  Every easing curve anybody wants is one of these; the
;;; rest of the catalogue is these functions with the arguments reversed, and a
;;; table of twenty would be a table to read rather than a shape to know.

(defparameter +animation-curves+ '(:linear :ease-in :ease-out :ease-in-out)
  "The curves a transition may use.

  :linear      constant speed -- for anything that is a measurement rather
               than a movement
  :ease-in     starts slowly -- things leaving
  :ease-out    ends slowly   -- things arriving, which is what the eye expects
                              of something that has been there in a moment
  :ease-in-out both ends slow -- things that move and stay")

(defun ease (curve progress)
  "PROGRESS in [0,1] shaped by CURVE, still in [0,1].

CLAMPED AT BOTH ENDS, because a frame clock delivers elapsed time rather than
the progress the animation asked for: the last frame of a fade lands past the
duration and a curve extrapolated there is a negative alpha or a surface drawn
past its own edge."
  (let ((p (max 0.0 (min 1.0 (float progress 1.0)))))
    (ecase (or curve :linear)
      (:linear p)
      (:ease-in (* p p))
      (:ease-out (- 1.0 (* (- 1.0 p) (- 1.0 p))))
      (:ease-in-out (if (< p 0.5)
                        (* 2.0 p p)
                        (- 1.0 (* 2.0 (- 1.0 p) (- 1.0 p))))))))

;;; ------------------------------------------------------------------- the table
;;;
;;; DECLARATIVE FIRST, like the gesture table (#16) and the snap places (#14):
;;; the shipped answer is a value in a table, `M-x animation' edits the table,
;;; and the runtime asks the table.  One table means the config file and the
;;; command cannot disagree.

(define-option *animation-duration* 0.18
  "How long a drawn overlay takes to arrive, in seconds.

THE WHOLE OF THE ANSWER FOR ANYTHING A WINDOW MANAGER DRAWS ITSELF.  Windows are
the compositor's; a keymap page, a map or a peek is this program's own buffer,
and this is how long it takes to fade in.  Zero disables it, and the shipped
0.18 is short enough that it reads as `this appeared' rather than as a wait.")

(define-option *animation-curve* :ease-out
  "The shape of that fade: :linear, :ease-in, :ease-out or :ease-in-out.

:out by default -- the eye reads something that arrives quickly and settles as
having been *put* there, and something at constant speed as having been dragged
in.")

(defparameter *animations* (make-hash-table :test #'equal)
  "TRANSITION -> (CURVE DURATION-SECONDS).  See ANIMATION-FOR.

Empty by default, and that is not an oversight: the shipped default lives in the
two options above, so a table entry exists only once somebody has said something
about one transition in particular.  A table pre-filled with five entries
identical to the defaults would be a second place for the same answer.")

(defun animation-for (transition)
  "The curve and duration TRANSITION (a keyword) uses, as a plan.

One function so that the runtime never reads the table or the options: a
transition nobody has configured gets the shipped pair, and there is exactly one
place that decides so."
  (or (gethash (animation-key transition) *animations*)
      (list *animation-curve* *animation-duration*)))

(defun (setf animation-for) (plan transition)
  "Configure TRANSITION: (setf (animation-for ...) '(:ease-in 0.3))."
  (destructuring-bind (curve duration &rest ignore) (append plan nil)
    (declare (ignore ignore))
    (unless (member curve +animation-curves+)
      (error "~s is not one of ~{~(~a~)~^, ~}" curve +animation-curves+))
    (unless (and (realp duration) (not (minusp duration)))
      (error "~s is not a duration in seconds" duration))
    (setf (gethash (animation-key transition) *animations*)
          (list curve (float duration 1.0)))))

(defun animation-key (transition)
  "The table key for TRANSITION, whatever kind of name it arrived as."
  (intern (string-upcase (string transition)) :keyword))

(defun animation-transitions ()
  "Every transition anybody has configured, sorted."
  (let ((out '()))
    (maphash (lambda (key plan) (declare (ignore plan)) (push key out)) *animations*)
    (sort out #'string< :key #'symbol-name)))

(defun animation-forget (transition)
  "Take TRANSITION out of the table: it goes back to the shipped default.

Not FORGET-ANIMATION, which is the command: a command and the function it calls
sharing a name is one definition, and the second one silently wins."
  (remhash (animation-key transition) *animations*)
  transition)

;;; ------------------------------------------------------------------- the maths

(defun animation-progress (transition elapsed)
  "How far into TRANSITION we are after ELAPSED seconds, shaped by its curve.

A pure function of the two, which is the whole reason this file exists apart
from the clock: `half a second into a fade-out' is arithmetic, and arithmetic is
checkable, while `the fade looked wrong' is not.

A duration of zero answers 1 immediately -- `no animation' must be a complete
thing rather than a division by zero."
  (destructuring-bind (curve duration) (animation-for transition)
    (if (or (not (plusp duration)) (not (realp elapsed)))
        1.0
        (ease curve (/ elapsed duration)))))

(defun animation-finished-p (transition elapsed)
  "True once ELAPSED has passed TRANSITION's duration."
  (destructuring-bind (curve duration) (animation-for transition)
    (declare (ignore curve))
    (or (not (plusp duration)) (>= (or elapsed 0) duration))))

;;; --------------------------------------------------------------- the commands
;;;
;;; HERE RATHER THAN IN THE RUNTIME, because DEFCOMMAND lives in this package
;;; and a command is a name, a lambda list and a body -- the registry does not
;;; need a compositor.  What this file may not do is *say* anything: reporting
;;; belongs to the runtime's echo area, so these return answers and the runtime
;;; prints them.

(defcommand animations ()
  "The transitions that have a time of their own, and the shipped default.

Nothing here is animated on a compositor's behalf: windows belong to the
compositor and river has no animation request.  What fades is what this program
draws itself -- the keymap page, the map, a peek -- and this lists the
transitions that have been given a time of their own."
  ()
  (cons (cons :default (list *animation-curve* *animation-duration*))
        (loop for transition in (animation-transitions)
              collect (cons transition (animation-for transition)))))

(defcommand animation (transition curve duration)
  "Give one transition its own curve and duration.

    M-x animation  open  ease-in  0.4

The transition is a keyword -- `:open' for the keymap page arriving, `:peek' for
a glance at the whole plane -- and this is a table entry, not a mode: take it
away with M-x FORGET-ANIMATION and the shipped pair applies again."
  (:interactive :transition :animation-curve :animation-duration)
  (setf (animation-for transition) (list curve duration))
  transition)

(defcommand forget-animation (transition)
  "Take a transition's own timing away; the shipped default applies again.

Answers T when there was something to take away."
  (:interactive :animation-transition)
  (prog1 (and (member (animation-key transition) (animation-transitions)) t)
    (animation-forget transition)))
