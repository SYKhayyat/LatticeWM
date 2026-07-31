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

(p:define-option *show-empty-panes* t
  "Draw an outline around empty panes, and the spawn hints inside the focused
one.

Turning this off gives you a window manager where standing in an empty pane
looks exactly like a broken keyboard.  It is here because it is an option, not
because it is a good idea.")

(p:define-option *empty-pane-hint* t
  "Show which keys open something, inside the focused empty pane.")

(p:define-option *empty-outline-color* '(0.30 0.32 0.40 0.55)
  "Outline colour of an unfocused empty pane.")

(p:define-option *empty-hint-color* '(0.70 0.75 0.86 1.0)
  "Text colour of the hint inside the focused empty pane.")

(defvar *cursor-overlay* nil)
(defvar *cursor-dirty* '())

(defun empty-pane-placements ()
  "Every visible empty leaf, as (PATH . RECT)."
  (loop for (node path rect visible) in (c:prop *world* :last-placements)
        when (and visible (typep node 'c:leaf) (c:leaf-empty-p node)
                  (plusp (c:rect-w rect)) (plusp (c:rect-h rect)))
          collect (cons path rect)))

(defun hint-lines ()
  "What to say inside the focused empty pane."
  (list "empty pane"
        (format nil "~{~a~^   ~}"
                (mapcar (lambda (entry)
                          (format nil "~a  ~a" (string (car entry)) (cdr entry)))
                        p:*empty-pane-keys*))))

(defun draw-empty-panes ()
  "Outline the empty panes and label the focused one."
  (let ((output (first (all-outputs))))
    (unless (and *show-empty-panes* output *server* *world*)
      (when *cursor-overlay* (overlay-hide *cursor-overlay*))
      (return-from draw-empty-panes nil))
    (let ((panes (empty-pane-placements)))
      (cond
        ((null panes)
         (when *cursor-overlay* (overlay-hide *cursor-overlay*))
         (setf *cursor-dirty* '()))
        (t
         (unless *cursor-overlay*
           (setf *cursor-overlay* (make-instance 'overlay :name "cursor")))
         (let* ((area (c:output-rect output))
                (canvas (ensure-overlay *cursor-overlay*
                                        (c:rect-w area) (c:rect-h area))))
           (when canvas
             ;; Clear only what was drawn last time; a full-screen clear is two
             ;; million writes for a few outlines.
             (dolist (rect *cursor-dirty*) (canvas-fill canvas 0 rect))
             (setf *cursor-dirty* '())
             (let ((cursor (c:world-cursor *world*))
                   (dim (apply #'argb *empty-outline-color*))
                   (hint (apply #'argb *empty-hint-color*)))
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
                            (push box *cursor-dirty*)
                            (when (and focusedp *empty-pane-hint*
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
                                          :scale 1))))))))
             (overlay-commit *cursor-overlay* :rect area))))))))

(add-hook :draw-overlays 'draw-empty-panes)
