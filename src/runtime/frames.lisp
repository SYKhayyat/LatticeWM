;;;; runtime/frames.lisp --- The clock that makes a fade a fade (#9).
;;;;
;;;; THE POLICY KNOWS THE SHAPE AND THIS KNOWS THE TIME.  P:ANIMATION-PROGRESS is
;;;; a function of (transition, elapsed) and nothing else, which is what lets it
;;;; be checked as arithmetic; the only fact that needs a clock is `how long has
;;;; this been going', and that is here.
;;;
;;;; ONE TIMER FOR EVERYTHING.  The frame clock is the timer mechanism this
;;;; program already has (#MAIN's ADD-TIMER, which runs on the window manager's
;;;; own thread between compositor events), registered under a fixed name so a
;;;; configuration loaded twice does not start two.  It runs only while
;;;; something is animating and takes itself off the moment nothing is -- a
;;;; clock that ticks forever is a laptop fan.
;;;
;;;; WHAT FADES IS WHAT THIS PROGRAM DRAWS.  Windows are the compositor's:
;;;; a Wayland client cannot move or fade another client's surface, river has no
;;;; animation request, and the finding is written up in doc/FINDINGS.org.  So
;;;; the surfaces that use this are the keymap page, the map and the peek --
;;;; each one this program's own buffer on this program's own layer surface.

(in-package #:latticewm/runtime)

(defparameter +frame-interval+ (/ 1.0 60.0)
  "Seconds between frames while something is animating.

SIXTY, and it is a redraw of overlays only -- no window is touched and no
surface is reconfigured, because the compositor is doing that and would do it at
its own rate regardless.  A fade at thirty frames is visibly a fade at thirty
frames; a window move at thirty frames is still the compositor's business.")

(defstruct (animation (:constructor %make-animation (key started fade)))
  "One thing fading, when it started, and the transition that times it.

The slot is called FADE rather than TRANSITION so that the reader is
ANIMATION-FADE and the function that answers `which transition is this' can keep
the name ANIMATION-TRANSITION for itself."
  key started fade)

(defvar *animations* (make-hash-table :test #'eq)
  "KEY -> ANIMATION.  One per thing being drawn, so two overlays can fade at once.")

(defvar *animation-origin* nil
  "The internal-real-time the clock measures from, or NIL when it is stopped.")

(defun animation-clock-running-p ()
  "True while the frame clock is on."
  (and *animation-origin* t))

(defun animation-elapsed (key)
  "Seconds since KEY started fading, or NIL when it is not fading."
  (let ((animation (gethash key *animations*)))
    (and animation *animation-origin*
         (/ (float (- (get-internal-real-time) (animation-started animation)) 1.0)
            (float internal-time-units-per-second 1.0)))))

(defun start-animation (key &optional (transition key))
  "Begin fading KEY, timed by TRANSITION's curve and duration.

TRANSITION defaults to KEY: the key an overlay is stored under is also the name
of the transition it uses, so the common case needs one argument and the
uncommon one can still say otherwise."
  (setf (gethash key *animations*)
        (%make-animation key (get-internal-real-time) transition))
  (start-animation-clock)
  key)

(defun stop-animation (key)
  "Stop fading KEY -- it is now fully arrived, or gone."
  (remhash key *animations*)
  (unless (plusp (hash-table-count *animations*))
    (stop-animation-clock))
  nil)

(defun stop-all-animations ()
  "Stop everything that is fading.  Called when the session is torn down."
  (clrhash *animations*)
  (stop-animation-clock))

(defun animation-value (key)
  "How far KEY has faded in, 0 to 1; 1 for anything not fading.

THE ANSWER FOR A KEY THAT IS NOT ANIMATING IS 1, NOT 0, and the difference is
the whole of what this function is for: a surface that is not fading must be
drawn at full strength, and a caller that had to ask `is it animating' first
would get that wrong somewhere."
  (let ((elapsed (animation-elapsed key)))
    (if elapsed
        (if (p:animation-finished-p (animation-transition (gethash key *animations*))
                                    elapsed)
            (progn (stop-animation key) 1.0)
            (p:animation-progress (animation-transition (gethash key *animations*))
                                  elapsed))
        1.0)))

(defun animation-transition (animation)
  "The transition ANIMATION is timed by."
  (animation-fade animation))

(defun animate-frame ()
  "One frame: move every fade on, redraw, and stop the clock when nothing is left.

Called by the frame timer on the window manager's thread.  A fade removes itself
the frame it finishes, so the last frame is drawn at full strength rather than
the surface disappearing a fraction early."
  (let ((live '()))
    (maphash (lambda (key animation)
               (declare (ignore animation))
               (when (< (animation-value key) 1.0) (push key live)))
             *animations*)
    (mark-dirty)
    ;; ASKING THE QUESTION MOVED THEM: ANIMATION-VALUE removes the ones that
    ;; have finished, so this count is the count that is left.
    (unless (plusp (hash-table-count *animations*))
      (stop-animation-clock))
    live))

(defun start-animation-clock ()
  "Put the frame timer on, if it is not already."
  (setf *animation-origin* (get-internal-real-time))
  (add-timer :frames +frame-interval+ #'animate-frame))

(defun stop-animation-clock ()
  "Take the frame timer off."
  (setf *animation-origin* nil)
  (remove-timer :frames))

;;; ------------------------------------------------- what the overlays ask for

(defun overlay-alpha (key)
  "The alpha multiplier for the overlay stored under KEY.

Kept as a one-line function rather than reading ANIMATION-VALUE at each drawing
site: the drawing code then reads as `draw this at the fade it is at' instead of
`ask the clock a question mid-expression', and a site that forgets to ask is a
site that does not fade rather than one that fails."
  (animation-value key))

(defun faded-color (color key)
  "COLOR with its alpha multiplied by KEY's fade, for the overlay painters.

Colours in this program are (R G B A) lists of floats, and a missing alpha means
opaque -- so `the overlay has no alpha of its own' and `the overlay is fading'
compose without either having to know about the other."
  (let ((alpha (overlay-alpha key)))
    (list (first color) (second color) (third color)
          (* (or (fourth color) 1.0) alpha))))
