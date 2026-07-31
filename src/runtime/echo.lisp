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

(defvar *echo-overlay* nil)
(defun notify (format &rest arguments)
  "Say something in the echo area, and log it.

The window manager's only way to talk to you.  Use it for anything a user
would want to know and cannot otherwise see — which is most of what currently
goes only to a log file nobody is reading."
  (let ((text (apply #'format nil format arguments)))
    (setf *echo-message* (cons text (get-universal-time)))
    (logmsg :info "~a" text)
    (mark-dirty)
    text))

(defun echo-segments (world &optional (columns 120))
  "What the echo area is saying right now, inside COLUMNS characters.

Three sources, in the order that decides which one wins when more than one has
something to say: a prompt owns the line outright, an armed chord is the next
most urgent thing, and otherwise the status line is the policy's to fill."
  (cond ((reading-p) (prompt-segments))
        (*pending-keymap* (p:pending-keymap-segments (p:current-policy) columns))
        (t (guarded "echo-content" (p:echo-content (p:current-policy) world)))))

(defun draw-echo-area (world output)
  "Draw and place the echo area along the bottom of OUTPUT."
  (unless (and p:*echo-area* *server* output)
    (when *echo-overlay* (overlay-hide *echo-overlay*))
    (return-from draw-echo-area nil))
  (unless *echo-overlay*
    (setf *echo-overlay* (make-instance 'overlay :name "echo")))
  (let* ((area (c:output-rect output))
         (height (max (+ 6 (text-height :scale p:*echo-scale*)) p:*echo-height*))
         (width (c:rect-w area))
         (canvas (ensure-overlay *echo-overlay* width height)))
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
                                  (c:make-rect pen baseline
                                               (* 2 p:*echo-scale*)
                                               (text-height :scale p:*echo-scale*)))
                     (incf pen (canvas-text canvas pen baseline text
                                            (case kind
                                              (:accent accent)
                                              (:prompt prompt-color)
                                              (:dim dim)
                                              (t normal))
                                            :scale p:*echo-scale*)))))
      (overlay-commit *echo-overlay*
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
;; file is loaded, so PUSHNEW cannot recognise the one already on the list, and
;; reloading echo.lisp into a running window manager reserved the strip twice —
;; the echo area silently ate a second strip of the screen with nothing drawn
;; in it.  Registering the symbol makes the reload idempotent and makes
;; redefining the function take effect, which is the whole point of being able
;; to reload the file at all.
(pushnew 'echo-reserved-edges p:*reserve-hooks*)
