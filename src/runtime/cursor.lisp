;;;; runtime/cursor.lisp --- Drawing the empty pane.
;;;;
;;;; DESIGN D18 rules that focus is a *place* rather than a window, and lists
;;;; the costs of that honestly.  This file pays the first one:
;;;;
;;;;   "A focused *empty* cell has no window to hang set_borders on, so the
;;;;   cursor needs a small shell surface of our own.  Same machinery as the
;;;;   minimap."
;;;;
;;;; and the reason it is not optional:
;;;;
;;;;   "You can stand somewhere that typing does not reach.  Mitigated by D19,
;;;;   and by the cursor being unmissable — *that is not optional polish, it is
;;;;   what stops an empty pane reading as a broken keyboard.*"
;;;;
;;;; Before this, starting the window manager gave you a black screen and one
;;;; line of status.  The cursor was somewhere; nothing said where.  A person
;;;; meeting that concludes the thing is broken, and they are not being
;;;; unreasonable.
;;;;
;;;; So an empty pane is drawn: an outline where it is, and inside the focused
;;;; one, the keys that will put something there.  D19 already made those keys
;;;; work; this makes them discoverable, which is the difference between a
;;;; feature and a secret.

(in-package #:latticewm/runtime)

(defun empty-pane-placements (&optional output)
  "Every visible empty leaf, as (PATH . RECT), on OUTPUT if one is given."
  (loop for (node path rect visible) in (c:prop *world* :last-placements)
        when (and visible (c:empty-pane-p node)
                  (plusp (c:rect-w rect)) (plusp (c:rect-h rect))
                  (or (null output)
                      (c:rect-intersect (c:output-rect output) rect)))
          collect (cons path rect)))

(defun hint-lines ()
  "What to say inside the focused empty pane."
  (list "empty pane"
        (format nil "~{~a~^   ~}"
                (mapcar (lambda (entry)
                          (format nil "~a  ~a" (string (car entry)) (cdr entry)))
                        p:*empty-pane-keys*))))

(defun draw-empty-panes ()
  "Outline the empty panes on every output, and label the focused one.

ONE OVERLAY PER OUTPUT.  This drew into a single surface pinned to
(FIRST (ALL-OUTPUTS)), so on a two-monitor desktop an empty pane on the second
monitor was outlined on the first — at coordinates that meant nothing there —
and the pane itself looked exactly like a broken keyboard, which is the failure
this whole file exists to prevent."
  (unless (and p:*show-empty-panes* *server* *world*)
    (dolist (overlay (all-overlays :cursor)) (overlay-hide overlay))
    (return-from draw-empty-panes nil))
  (dolist (output (all-outputs))
    (guarded "empty panes" (draw-empty-panes-on output))))

(defun draw-empty-panes-on (output)
  "Outline the empty panes lying on OUTPUT."
  (let* ((overlay (overlay-for :cursor output))
         (panes (empty-pane-placements output)))
    (cond
      ((null panes)
       (overlay-hide overlay))
      (t
       (let* ((area (c:output-rect output))
              (canvas (ensure-overlay overlay (c:rect-w area) (c:rect-h area))))
         (when canvas
           ;; ENSURE-OVERLAY hands back a canvas already cleared of whatever
           ;; frame it was holding — only those rectangles, because a
           ;; full-screen clear is two million writes for a few outlines.  The
           ;; record lives on the canvas, which is the correction: it used to
           ;; live on the overlay, and with more than one buffer in play "what
           ;; the last frame drew" and "what this buffer holds" are two
           ;; different lists.
           (let ((cursor (c:world-cursor *world*))
                 (dim (apply #'argb p:*empty-outline-color*))
                 (hint (apply #'argb p:*empty-hint-color*))
                 (drawn '()))
             (multiple-value-bind (fr fg fb fa)
                 (p:border-color (p:current-policy) (c:make-leaf) t)
               (let ((bright (argb fr fg fb fa)))
                 (loop for (path . rect) in panes
                       for focusedp = (c:path-equal path cursor)
                       for box = (c:make-rect (- (c:rect-x rect) (c:rect-x area))
                                              (- (c:rect-y rect) (c:rect-y area))
                                              (c:rect-w rect) (c:rect-h rect))
                       do (canvas-rect canvas box (if focusedp bright dim)
                                       :width (if focusedp 3 1))
                          (push box drawn)
                          (when (and focusedp p:*empty-pane-hint*
                                     (> (c:rect-w box) 220)
                                     (> (c:rect-h box) 90))
                            (let* ((lines (hint-lines))
                                   (line-height (+ 8 (text-height :scale 1)))
                                   (top (+ (c:rect-y box)
                                           (floor (- (c:rect-h box)
                                                     (* line-height (length lines)))
                                                  2))))
                              (loop for text in lines
                                    for index from 0
                                    for width = (text-width text :scale 1)
                                    do (canvas-text
                                        canvas
                                        (+ (c:rect-x box)
                                           (floor (- (c:rect-w box) width) 2))
                                        (+ top (* index line-height))
                                        text
                                        (if (zerop index) hint bright)
                                        :scale 1)))))))
             ;; What was drawn, so the next frame into this buffer clears
             ;; exactly it and the compositor is told exactly it.
             (overlay-drew overlay drawn))
           (overlay-commit overlay :rect area)))))))

(add-hook :draw-overlays 'draw-empty-panes)
