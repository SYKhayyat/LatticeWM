;;;; examples/05-status-line.lisp
;;;;
;;;; TIER 1, AND THE ONE PEOPLE ASK FOR FIRST — put your own things in the
;;;; status line.  Twenty lines, no new class, no state, no restart.
;;;;
;;;; Every other window manager answers this with a *status bar*: a second
;;;; program, a protocol between the two, a configuration language for the bar,
;;;; and a list of modules somebody else decided you might want.  Here the
;;;; status line is one generic returning a list of strings, and adding to it
;;;; is a method that calls the shipped one and conses onto the answer.
;;;;
;;;;     (load "examples/05-status-line.lisp")
;;;;     (status-line-extras)      ; on
;;;;     (status-line-extras nil)  ; off again, with no restart
;;;;
;;;; What it adds: the time, the load average, and a count of the windows that
;;;; are open in a workspace you are not looking at — which is the one thing
;;;; this layout model can tell you and a general-purpose bar cannot, because a
;;;; bar has no idea what a workspace of yours contains.  That asymmetry is the
;;;; argument for the whole approach.
;;;;
;;;; It also shows the other half of a status line, which is that something has
;;;; to make it tick.  A segment that reads a clock is only as fresh as the
;;;; last redraw, and until ADD-TIMER existed there was nothing in this program
;;;; that happened because time passed — so the clock here was, for four days,
;;;; a display of when you last moved a window.

(in-package #:latticewm/user)

(defvar *status-line-extras* t
  "Whether the extra segments are shown.  See STATUS-LINE-EXTRAS.")

;;; --------------------------------------------------------- the segments

(defun clock-segment ()
  "The time, to the minute.  Local, and asked fresh every frame."
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (format nil "~2,'0d:~2,'0d" hour minute)))

;;; ----------------------------------------------------------- and the tick
;;;
;;; A CLOCK NEEDS SOMETHING TO MAKE IT TICK, AND SAYING "EVERY FRAME" IS NOT
;;; IT.  The sentence above is true and was not enough: every redraw in this
;;; program is caused by something -- a key, a window, a monitor -- and nothing
;;; was caused by time passing.  So this clock showed the time of the last
;;; layout change.  Sit still for twenty minutes and it read twenty minutes
;;; ago, which is worse than no clock, because a wrong clock is believed.
;;;
;;; ADD-TIMER is the answer and this is the whole of it.  Fifteen seconds
;;; rather than sixty: the minute the clock displays does not turn over on the
;;; minute *we* started counting, so a sixty-second tick is on average thirty
;;; seconds late.  Four ticks a minute costs nothing, and three of them draw
;;; nothing at all -- MARK-DIRTY asks for a layout, the emit diff notices that
;;; every window is already where it should be, and no request goes to river.
;;; The redraw that is skipped is skipped by machinery that was already there.
;;;
;;; The timer is named, so loading this file twice leaves one clock.

(defun start-clock (&optional (seconds 15))
  "Make the clock segment keep up with the clock."
  (add-timer :status-line-clock seconds #'mark-dirty))

(defun stop-clock ()
  "Stop asking.  The segment still updates whenever anything else redraws."
  (remove-timer :status-line-clock))

(defun load-segment ()
  "The one-minute load average, or \"\" where there is no /proc/loadavg.

A segment that cannot answer returns the empty string rather than NIL or a
placeholder: ECHO-CONTENT drops empty segments, so \"\" is the documented way
for a segment to say nothing this time.  On a machine with no procfs this
simply is not in the line, and nothing has to know that."
  (or (ignore-errors
       (with-open-file (in "/proc/loadavg" :if-does-not-exist nil)
         (when in
           (let ((line (read-line in nil "")))
             (subseq line 0 (position #\Space line))))))
      ""))

(defun elsewhere-segment (world)
  "How many windows are open in a workspace you are not looking at.

THIS IS THE SEGMENT A SEPARATE STATUS BAR CANNOT WRITE.  It is not a fact about
the system, it is a fact about the layout: only the thing that did the laying
out knows which windows are somewhere else.  Every module a general-purpose bar
ships is something it could read from the kernel or from D-Bus, which is
exactly the set of things that are not about your windows.

Counted at the workspace, deliberately, and not at the pane -- a window behind
a tab is on screen in every sense the user means, and calling it hidden would
make the number argue with what they can see."
  (let* ((workspaces (world-workspaces world))
         (all (node-window-count (world-root world)))
         (here (if workspaces
                   (node-window-count (child-at workspaces
                                                (container-selection workspaces)))
                   all))
         (elsewhere (max 0 (- all here))))
    (if (plusp elsewhere) (format nil "~d elsewhere" elsewhere) "")))

;;; ------------------------------------------------------------ the method

(defmethod echo-content ((policy conventional-policy) world &optional (columns 120))
  "The shipped line, plus three segments of our own.

CALL-NEXT-METHOD FIRST AND APPEND, rather than rebuilding the line.  The
shipped method already answers the hard part -- what is in the pane, what the
counts are, which segment gets truncated when the budget runs out -- and it
goes on doing that while this method exists.  Two extensions can both do this
and both appear; a method that returned only its own segments would silently
delete whatever the other one added, and the user's only clue would be that
somebody's feature stopped happening.

The budget is passed down untouched.  These segments go on the end, so they are
the first to be dropped on a narrow screen, which is the right way round for
things that are true all the time.

DECLARED IGNORED RATHER THAN QUIETLY UNUSED, and the reason is one line further
out than this method.  A bare CALL-NEXT-METHOD forwards the original arguments,
so COLUMNS genuinely has nothing to do here — but SBCL says so on every build,
gate 1 is `zero compiler warnings', and the build printed one anyway because
gate 1 compiled src/ and lattice/ and nothing under examples/.  A gate with an
unstated scope is a gate somebody stops reading.  It compiles this directory
now, so an example that would not compile is a failure rather than a line
scrolling past."
  (declare (ignore columns))
  (let ((shipped (call-next-method)))
    (if (not *status-line-extras*)
        shipped
        (append shipped
                (remove ""
                        (list (cons (elsewhere-segment world) :normal)
                              (cons (load-segment) :normal)
                              (cons (clock-segment) :accent))
                        :key #'car :test #'string=)))))

;;; ------------------------------------------------------------ the switch

(defun status-line-extras (&optional (on t))
  "Turn the extra segments on or off.  Takes effect on the next frame.

TURNING IT OFF TAKES THE TIMER OFF TOO, which is the half an example is for.
A timer nobody can see the effect of is still a wakeup four times a minute
forever, and the person who turned this off has no way to know it is there.
An extension that can be disabled has to undo everything it did, and this is
the same lesson REMOVE-HOOK teaches one mechanism over."
  (setf *status-line-extras* on)
  (if on (start-clock) (stop-clock))
  (relayout)
  on)

;;; On by default, because the file is meant to be loaded and then looked at.
(start-clock)
