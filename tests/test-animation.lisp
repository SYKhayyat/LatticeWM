;;;; tests/test-animation.lisp --- Timing the things this program draws (#9).
;;;;
;;;; The half of #9 that could be shipped is the half this program owns.  A
;;;; Wayland client cannot move or fade another client's surface and river has
;;;; no animation request, so `animate a window opening' is not a feature at any
;;;; price -- but the keymap page, the map and a peek are this program's own
;;;; buffers, and they can arrive instead of appearing.
;;;;
;;;; Everything here is arithmetic on a number of seconds.  That is the point of
;;;; splitting the model (policy: curves, table, progress) from the clock
;;;; (runtime: one timer): `half a second into a fade-out' is checkable and
;;;; `the fade looked wrong' is not.

(in-package #:latticewm/tests)
(in-suite animation)

;;; ---------------------------------------------------------------- the curves

(test every-curve-starts-at-nothing-and-ends-at-everything
  "The two values a caller can rely on without knowing which curve is in
force: an animation at its start is invisible and at its end is complete."
  (dolist (curve p:+animation-curves+)
    (is (= 0.0 (p:ease curve 0)) "~(~a~) at zero" curve)
    (is (= 1.0 (p:ease curve 1)) "~(~a~) at one" curve)))

(test a-curve-clamps-because-a-clock-delivers-whole-frames
  "The frame that lands past the duration would otherwise be a negative alpha --
which is a window manager handing the compositor a colour it cannot make."
  (dolist (curve p:+animation-curves+)
    (is (= 0.0 (p:ease curve -0.5)))
    (is (= 1.0 (p:ease curve 1.5)))
    (is (= 1.0 (p:ease curve 4.0)))))

(test the-curves-differ-in-the-middle-which-is-what-they-are-for
  (let ((linear (p:ease :linear 0.5))
        (in (p:ease :ease-in 0.5))
        (out (p:ease :ease-out 0.5))
        (in-out (p:ease :ease-in-out 0.5)))
    (is (= 0.5 linear))
    (is (< in linear) ":ease-in starts slowly, so it is behind at the middle")
    (is (> out linear) ":ease-out arrives quickly, so it is ahead at the middle")
    (is (= 0.5 in-out) "and the two-ended curve is symmetric")))

(test a-curve-that-is-not-one-says-so
  (signals error (p:ease :bouncy 0.5)))

;;; ----------------------------------------------------------------- the table

(test an-unconfigured-transition-uses-the-shipped-pair
  (is (equal (list p:*animation-curve* p:*animation-duration*)
             (p:animation-for :nothing-has-configured-this))))

(test a-transition-can-be-given-its-own-timing
  (unwind-protect
       (progn
         (setf (p:animation-for :open) '(:ease-in 0.4))
         (is (equal '(:ease-in 0.4) (p:animation-for :open)))
         (is (equal '(:ease-in 0.4) (p:animation-for "OPEN"))
             "by name, whatever kind of name it arrived as")
         (is-true (member :open (p:animation-transitions)))
         (p:animation-forget :open)
         (is (not (member :open (p:animation-transitions)))
             "and forgetting it puts the shipped pair back"))
    (p:animation-forget :open)))

(test a-bad-curve-or-a-negative-duration-is-refused
  "A table is a thing a config file writes, so a typo has to be an error with a
sentence in it rather than a transition that silently never finishes."
  (signals error (setf (p:animation-for :open) '(:bouncy 0.2)))
  (signals error (setf (p:animation-for :open) '(:linear -1)))
  (signals error (setf (p:animation-for :open) '(:linear "soon"))))

;;; --------------------------------------------------------------- the progress

(test progress-is-elapsed-over-duration-shaped-by-the-curve
  (unwind-protect
       (progn
         (setf (p:animation-for :open) '(:linear 0.5))
         (is (= 0.0 (p:animation-progress :open 0)))
         (is (= 0.5 (p:animation-progress :open 0.25)))
         (is (= 1.0 (p:animation-progress :open 0.5)))
         (is (= 1.0 (p:animation-progress :open 2.0))
             "past the end is the end, not a value to extrapolate")
         (is-true (p:animation-finished-p :open 0.5))
         (is-false (p:animation-finished-p :open 0.25)))
    (p:animation-forget :open)))

(test a-duration-of-zero-is-a-complete-animation-and-not-a-division-by-zero
  "How the feature is turned off: the table still answers, and the answer is
`already there'."
  (unwind-protect
       (progn
         (setf (p:animation-for :open) '(:linear 0))
         (is (= 1.0 (p:animation-progress :open 0)))
         (is-true (p:animation-finished-p :open 0)))
    (p:animation-forget :open)))

;;; ----------------------------------------------------------------- the clock

(test a-thing-that-is-not-fading-is-at-full-strength
  "One of the two facts the drawing sites rely on: `not animating' answers 1,
not 0, so a painter does not have to branch."
  (is (= 1.0 (r:animation-value :nothing-animates-this)))
  (is (null (r:animation-elapsed :nothing-animates-this))))

(test starting-a-fade-runs-the-clock-and-finishing-it-stops-it
  (unwind-protect
       (progn
         ;; The clock is the SESSION's, and another suite has very likely shown
         ;; a page through SHOW-HELP-PAGE -- so this starts from a known state
         ;; rather than from whatever ran before it.
         (r:stop-all-animations)
         (is (not (r:animation-clock-running-p)) "nothing is animating yet")
         (r:start-animation :probe)
         (is-true (r:animation-clock-running-p))
         (is (< (r:animation-value :probe) 1.0)
             "it has just started, so it is not yet arrived")
         (r:stop-animation :probe)
         (is (= 1.0 (r:animation-value :probe)))
         (is (not (r:animation-clock-running-p))
             "and with nothing left to draw, the clock is off -- a timer that
ticks forever is a laptop fan"))
    (r:stop-all-animations)))

(test an-animation-that-has-finished-is-cleaned-up-by-asking-how-far-it-got
  "A zero-duration transition is the shortest test of the rule: the frame clock
asks, gets 1, and the entry goes -- so the clock stops by itself."
  (unwind-protect
       (progn
         (setf (p:animation-for :instant) '(:linear 0))
         (r:start-animation :probe :instant)
         (is (= 1.0 (r:animation-value :probe)))
         (is (null (r:animation-elapsed :probe))
             "and the entry is gone")
         (is (not (r:animation-clock-running-p))))
    (p:animation-forget :instant)
    (r:stop-all-animations)))

(test a-frame-reports-what-is-still-live
  (unwind-protect
       (progn
         (r:stop-all-animations)
         (setf (p:animation-for :slow) '(:linear 10))
         (r:start-animation :probe :slow)
         (is (equal '(:probe) (r:animate-frame))
             "one thing still fading")
         (r:stop-animation :probe)
         (is (null (r:animate-frame)))
         (is (not (r:animation-clock-running-p))))
    (p:animation-forget :slow)
    (r:stop-all-animations)))

;;; --------------------------------------------------------------- the colour

(test a-faded-colour-is-the-same-colour-with-less-alpha
  "The whole of the drawing half: a colour with no alpha is opaque, and the
fade multiplies rather than replaces."
  (unwind-protect
       (progn
         (setf (p:animation-for :slow) '(:linear 10))
         (is (equal '(1.0 0.5 0.25 1.0) (r:faded-color '(1.0 0.5 0.25) :nothing)))
         (r:start-animation :probe :slow)
         (let ((faded (r:faded-color '(0.2 0.4 0.6 0.8) :probe)))
           (is (= 0.2 (first faded)))
           (is (< (fourth faded) 0.8) "the alpha is what the fade touched")
           (is (< (fourth faded) 0.2) "and a tenth of a second into a ten second
fade is a tenth of the alpha")))
    (p:animation-forget :slow)
    (r:stop-all-animations)))

;;; ------------------------------------------------------------- the consumer

(test showing-a-page-fades-it-in
  "SHOW-HELP-PAGE is what every page in the program is drawn through, so this
one check covers the keymap, apropos, describe-window, list-hidden, list-tags,
list-outputs and animations at once."
  (unwind-protect
       (progn
         (r:stop-all-animations)
         (is (not (r:animation-clock-running-p)))
         (r:show-help-page "a page" '(("k" . "v")))
         (is-true (r:animation-clock-running-p) "the clock started")
         (is (< (r:animation-value :help) 1.0) "and the page is fading in")
         ;; Dismissing it takes the fade with it.
         (r::dismiss-overlay)
         (is (null p:*help-visible*))
         (is (null (r:animation-elapsed :help))))
    (r:stop-all-animations)
    (setf p:*help-visible* nil)))

(test the-animation-commands-exist
  (is-true (p:find-command "animations"))
  (is-true (p:find-command "animation"))
  (is-true (p:find-command "forget-animation"))
  (is-true (fboundp 'r:overlay-alpha)))
