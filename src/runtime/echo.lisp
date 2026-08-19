;;;; runtime/echo.lisp --- The echo area.
;;;;
;;;; Emacs's minibuffer, minus the reading-from-it part: a strip along the
;;;; bottom of the screen that says where you are and what just happened.
;;;;
;;;; It exists because of a specific, stated risk rather than because status
;;;; bars are nice.  DESIGN calls two-dimensional navigation "the single
;;;; biggest design risk" in the whole design and names three defences —
;;;; minimap, named cells, coordinate overlay — of which PLAN's cut list keeps
;;;; only the last.  A permanent line saying *where you are* is the cheapest
;;;; possible form of that, and unlike a per-cell overlay it costs one surface
;;;; and stays readable at any zoom level, because it does not shrink with the
;;;; cells.
;;;;
;;;; It is also the obvious place for everything a window manager currently has
;;;; no way to tell you: that a command failed, that a submap is armed, that
;;;; you are standing in an empty pane and can press `t'.
;;;;
;;;; Content is a policy decision, so it is a generic — see ECHO-SEGMENTS.
;;;; What is here is the mechanism and a default worth keeping.

(in-package #:latticewm/runtime)

(defun notify (format &rest arguments)
  "Say something in the echo area, and log it.

The window manager's only way to talk to you.  Use it for anything a user
would want to know and cannot otherwise see — which is most of what currently
goes only to a log file nobody is reading."
  (let ((text (apply #'format nil format arguments)))
    (setf *echo-message* (cons text (get-internal-real-time)))
    (logmsg :info "~a" text)
    (mark-dirty)
    text))

(defun echo-segments (world &optional (columns 120))
  "What the echo area is saying right now, inside COLUMNS characters.

Three sources, in the order that decides which one wins when more than one has
something to say: a prompt owns the line outright, an armed chord is the next
most urgent thing, and otherwise the status line is the policy's to fill.

COLUMNS reaches all three now.  It used to reach one: the chord had a budget,
the prompt is as long as what you typed, and the status line was handed no
number at all and ran off the edge of the screen."
  (cond ((reading-p) (prompt-segments))
        (*pending-keymap* (p:pending-keymap-segments (p:current-policy) columns))
        (t (guarded "echo-content"
             (p:echo-content (p:current-policy) world columns)))))

(defun draw-echo-area (world output)
  "Draw and place the echo area along the bottom of OUTPUT.

ONE PER OUTPUT.  It used to be one global surface for the whole program, so on
a two-monitor desktop the status line existed on exactly one screen and stayed
there when the cursor left — which is the one piece of permanent orientation
this window manager has, absent from the monitor you were looking at."
  (let ((overlay (overlay-for :echo output)))
    (unless (and p:*echo-area* *server* output)
      (overlay-hide overlay)
      (return-from draw-echo-area nil))
    (draw-echo-area-on world output overlay)))

(defun draw-echo-area-on (world output overlay)
  "Draw the echo area for OUTPUT into OVERLAY.

NOTHING IS DRAWN PAST THE RIGHT-HAND EDGE, and that is a guarantee of the
mechanism rather than a courtesy of the policy.  This used to draw every segment
it was given and let the canvas end where it ended, so on a 1280-pixel screen
the shipped status line lost its last two words with nothing to say so:
ECHO-CONTENT ended \"...Super+- zoom out\" and the screen ended \"...past a cell
edge = next cel\".

The policy gets a budget too — see ECHO-CONTENT — and that is the better half,
because a policy can choose *what* to drop.  But a budget in characters and a
canvas in pixels are two different arithmetics, an extension may ignore the
budget entirely, and a message can arrive from anywhere; so the guarantee lives
here, where the widths are known exactly, and the budget lives there, where the
priorities are.  A segment that does not fit is cut at a word boundary and
marked with an ellipsis, which is TRUNCATE-TEXT's job everywhere else in this
program."
  (let* ((area (c:output-rect output))
         (height (max (+ 6 (text-height :scale p:*echo-scale*)) p:*echo-height*))
         (width (c:rect-w area))
         (canvas (ensure-overlay overlay width height)))
    (when canvas
      (canvas-fill canvas (apply #'argb p:*echo-background*))
      (let* ((pen 8)
             (baseline (floor (- height (text-height :scale p:*echo-scale*)) 2))
             (normal (apply #'argb p:*echo-foreground*))
             (accent (apply #'argb p:*echo-accent*))
             (divider (apply #'argb p:*echo-divider*))
             (prompt-color (apply #'argb p:*minibuffer-prompt-color*))
             (caret-color (apply #'argb p:*minibuffer-caret-color*))
             (dim (apply #'argb p:*minibuffer-completion-color*))
             (segments (remove-if (lambda (segment)
                                    (and (zerop (length (car segment)))
                                         ;; The caret is the one segment whose
                                         ;; whole content is where it is.
                                         (not (eq (cdr segment) :caret))))
                                  (echo-segments
                                   world
                                   (floor (- width 16)
                                          (max 1 (text-width "m"
                                                             :scale p:*echo-scale*)))))))
        ;; The right-hand margin is the left-hand one, so the line is inset by
        ;; the same eight pixels at both ends and a full line looks deliberate
        ;; rather than jammed against the glass.
        (let ((edge (- width 8))
              (cell (max 1 (text-width "m" :scale p:*echo-scale*))))
          (loop for (text . kind) in segments
                for firstp = t then nil
                do ;; The separator goes *between* segments, which means before
                   ;; every one but the first.  Putting it after each instead
                   ;; leaves a dangling bar at the end of the line, which looks
                   ;; like something failed to render.  A prompt draws its parts
                   ;; contiguously, because "M-x | foc" is not a prompt.
                   (unless (or firstp (reading-p) (eq kind :caret))
                     (incf pen (* 4 p:*echo-scale*))
                     (incf pen (canvas-text canvas pen baseline "|" divider
                                            :scale p:*echo-scale*))
                     (incf pen (* 4 p:*echo-scale*)))
                   (if (eq kind :caret)
                       ;; Drawn, not typed: a bar between two characters rather
                       ;; than a character between them, so that the text does
                       ;; not jump sideways as the caret moves through it.
                       (canvas-fill canvas caret-color
                                    (c:make-rect (min pen edge) baseline
                                                 (* 2 p:*echo-scale*)
                                                 (text-height :scale p:*echo-scale*)))
                       ;; What fits, in characters, because the shipped font is
                       ;; fixed-width and the budget above counts in characters
                       ;; for the same reason.  Fewer than four and there is no
                       ;; room for an abbreviation either, so the line stops.
                       (let ((room (floor (- edge pen) cell)))
                         (when (< room 4) (loop-finish))
                         (let ((fitted (if (<= (length text) room)
                                           text
                                           (p:truncate-text text room))))
                           (incf pen (canvas-text canvas pen baseline fitted
                                                  (case kind
                                                    (:accent accent)
                                                    (:prompt prompt-color)
                                                    (:dim dim)
                                                    (t normal))
                                                  :scale p:*echo-scale*))
                           (unless (eq fitted text) (loop-finish))))))))
      (overlay-commit overlay
                      :rect (c:make-rect (c:rect-x area)
                                         (if (eq p:*echo-position* :top)
                                             (c:rect-y area)
                                             (- (c:rect-bottom area) height))
                                         width height))
      height)))

(defun echo-reserved-height (output)
  "How much of OUTPUT the echo area is using, so the layout can avoid it."
  (declare (ignore output))
  (if (and p:*echo-area* *server*)
      (max (+ 6 (text-height :scale p:*echo-scale*)) p:*echo-height*)
      0))

(defun echo-reserved-edges (output)
  "The echo area's reservation as (TOP RIGHT BOTTOM LEFT)."
  (let ((height (echo-reserved-height output)))
    (if (eq p:*echo-position* :top)
        (list height 0 0 0)
        (list 0 0 height 0))))

;; Take the echo area's strip out of the layout, so windows sit above it
;; rather than under it.
;;
;; By name, not as a lambda.  A lambda here is a fresh object every time this
;; file is loaded, so ADD-HOOK cannot recognise the one already on the list, and
;; reloading echo.lisp into a running window manager reserved the strip twice —
;; the echo area silently ate a second strip of the screen with nothing drawn
;; in it.  Registering the symbol makes the reload idempotent and makes
;; redefining the function take effect, which is the whole point of being able
;; to reload the file at all.
;;
;; Through ADD-HOOK, like everything else.  This used to push onto a special
;; variable that the hook mechanism knew nothing about, so the program had two
;; ways to hook and gate 7 could only see one of them.
(add-hook :reserve-space 'echo-reserved-edges)
