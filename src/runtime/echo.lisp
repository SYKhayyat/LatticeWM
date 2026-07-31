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
(defvar *echo-message* nil "A cons of text and the time it was posted.")

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

(defun current-message ()
  "The message to show, or NIL once it has aged out."
  (let ((message *echo-message*))
    (when (and message
               (< (- (get-universal-time) (cdr message)) p:*echo-message-seconds*))
      (car message))))

(defmethod p:echo-content ((policy p:policy) world)
  "The shipped echo area: workspace, place, contents, counts, last message."
  (let* ((root (c:world-root world))
         (workspaces (c:world-workspaces world))
         (window (c:world-focus-window world))
         (leaf (c:world-leaf-at world))
         (segments '()))
    (when workspaces
      (push (cons (format nil "[~d/~d]" (1+ (c:stack-selected workspaces))
                          (c:container-count workspaces))
                  :normal)
            segments))
    ;; Where the cursor is, in whatever terms the layout makes available.  The
    ;; lattice puts a coordinate on the node; without it, the path is the
    ;; honest answer and is still better than nothing.
    (let* ((node (c:world-node-at world))
           (place (or (and node (c:prop node :lattice/address)
                           (format nil "~d,~d"
                                   (car (c:prop node :lattice/address))
                                   (cdr (c:prop node :lattice/address))))
                      (format nil "~{~a~^.~}" (c:world-cursor world)))))
      (push (cons place :accent) segments))
    (push (cons (cond ((null leaf) "")
                      ((c:leaf-empty-p leaf)
                       (format nil "empty -- ~{~a~^ ~} to open"
                               (mapcar (lambda (entry) (string (car entry)))
                                       p:*empty-pane-keys*)))
                      (window (or (c:window-app-id window) "?"))
                      (t ""))
                :normal)
          segments)
    (let ((count (length (c:node-windows root)))
          (scratch (length (c:world-scratchpad world)))
          (floats (length (c:world-floats world))))
      (push (cons (format nil "~d window~:p~@[ ~d float~:p~]~@[ ~d hidden~]"
                          count (and (plusp floats) floats)
                          (and (plusp scratch) scratch))
                  :normal)
            segments))
    (let ((message (current-message)))
      (when message (push (cons message :accent) segments)))
    (nreverse segments)))

(defun keymap-choices (keymap)
  "KEYMAP's bindings as (KEYS . DESCRIPTION), merged the way the help screen
merges them: two keys that do the same thing are one choice with two keys on
it, not two choices."
  (let ((by-description '()))
    (loop for (key . target) in (keymap-keys keymap)
          for description = (binding-description target)
          for entry = (assoc description by-description :test #'string=)
          do (if entry
                 (setf (cdr entry) (append (cdr entry) (list (keysym-name (car key)))))
                 (push (cons description (list (keysym-name (car key))))
                       by-description)))
    (mapcar (lambda (entry)
              (cons (format nil "~{~a~^/~}" (rest entry)) (first entry)))
            (nreverse by-description))))

(defun pending-keymap-segments (&optional (columns 120))
  "What an armed chord offers, as echo-area segments, inside COLUMNS.

which-key, in a window manager: the moment you press the first key of a chord
the echo area lists what the second key can be, built from the live submap and
each command's own docstring.  A chord you have to remember is a chord you will
not use, and the only reason Emacs's C-x map is usable by anybody is that
somebody eventually wrote this.

The budget is arithmetic rather than clipping.  Letting the compositor cut the
line at the screen edge is what it did first, and the last choice on the line
then read as a word that had lost its ending — which is worse than not
offering it, because it looks like a bug rather than like a list that goes on."
  (let* ((keymap *pending-keymap*)
         (label (format nil "~a-" (or (keymap-name keymap) "prefix")))
         (choices (keymap-choices keymap))
         (room (- columns (length label) 4))
         (shown '()))
    (loop for (keys . description) in choices
          ;; Each choice is at least its keys plus a word of explanation; if
          ;; even that does not fit, everything after it is `+n more'.
          for text = (format nil "~a ~a" keys (truncate-text description 26))
          while (> room (+ (length text) 8))
          do (push (cons text :normal) shown)
             (decf room (+ (length text) 3)))
    (let ((left (- (length choices) (length shown))))
      (cons (cons label :prompt)
            (nreverse (if (plusp left)
                          (cons (cons (format nil "+~d more" left) :dim) shown)
                          shown))))))

(defun echo-segments (world &optional (columns 120))
  "What the echo area is saying right now, inside COLUMNS characters.

Three sources, in the order that decides which one wins when more than one has
something to say: a prompt owns the line outright, an armed chord is the next
most urgent thing, and otherwise the status line is the policy's to fill."
  (cond ((reading-p) (prompt-segments))
        (*pending-keymap* (pending-keymap-segments columns))
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
