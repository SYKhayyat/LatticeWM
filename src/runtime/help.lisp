;;;; runtime/help.lisp --- The keymap, on screen.
;;;;
;;;; Emacs's actual advantage over every editor that copied its keybindings is
;;;; not the bindings.  It is that you can always ask.  C-h k, C-h b, and the
;;;; fact that every command carries its own documentation mean the system is
;;;; *discoverable* — you are never more than one keystroke from finding out
;;;; what a key does or what keys exist.
;;;;
;;;; A window manager is worse off than an editor here, because it has nowhere
;;;; to print to.  It has a log file nobody reads and a manual page that goes
;;;; stale.  So the keymap gets drawn on the screen, from the live keymap, with
;;;; each binding's real command and that command's real docstring — which
;;;; means it cannot be wrong and cannot go out of date, including for bindings
;;;; the user added five minutes ago at a REPL.
;;;;
;;;; This is the same argument as the generated extension-surface document, one
;;;; level down: documentation derived from the running system rather than
;;;; maintained beside it.

(in-package #:latticewm/runtime)

(defvar *help-overlay* nil)

(defun draw-help-overlay ()
  "Draw the keymap, or hide it."
  (let ((output (first (all-outputs))))
    (unless (and *help-visible* output *server*)
      (when *help-overlay* (overlay-hide *help-overlay*))
      (return-from draw-help-overlay nil))
    (unless *help-overlay*
      (setf *help-overlay* (make-instance 'overlay :name "help")))
    (let* ((area (c:output-rect output))
           (canvas (ensure-overlay *help-overlay* (c:rect-w area) (c:rect-h area))))
      (when canvas
        (let* ((customp (consp *help-visible*))
               (entries (if customp (cdr *help-visible*) (p:help-entries (policy) p:*keymap*)))
               (line (+ 4 (text-height :scale p:*help-scale*)))
               (title-top 16)
               (top (+ title-top (* 2 line)))
               (available (max 1 (floor (- (c:rect-h area) top line) line)))
               ;; Prose gets one column and a table gets as many as it needs.
               ;; A paragraph poured into two columns reads as two paragraphs,
               ;; each of them half a sentence wide.
               ;;
               ;; *HELP-COLUMNS* is a preference, not a law: on a short screen
               ;; two columns of forty rows silently lost the last eighteen
               ;; bindings, which is the worst thing a reference can do.  Three
               ;; is the hard stop — past that the descriptions are narrower
               ;; than the keys.
               (columns (if customp
                            1
                            (min 3 (max p:*help-columns*
                                        (ceiling (length entries) available)))))
               (rows (ceiling (length entries) columns))
               (column-width (floor (- (c:rect-w area) 40) columns))
               (key-color (apply #'argb p:*help-key-color*))
               (text-color (apply #'argb p:*help-text-color*))
               ;; One left column, as wide as the widest thing in it, so that
               ;; every description starts at the same x.  Ragged descriptions
               ;; are what made the apropos screen read as a paragraph with
               ;; some words in bold rather than as a table, and the eye scans
               ;; a column for free.
               ;;
               ;; Capped at half the column, because alignment is worth less
               ;; than legibility: one forty-character chord — and the keymap
               ;; has some — would otherwise squeeze every description on the
               ;; screen down to nothing to stay level with it.
               (gap (text-width "  " :scale p:*help-scale*))
               (left (min (floor column-width 2)
                          (+ gap (reduce #'max entries :initial-value 0
                                         :key (lambda (entry)
                                                (text-width (car entry)
                                                            :scale p:*help-scale*))))))
               (shown 0))
          (canvas-fill canvas (apply #'argb p:*help-background*))
          (loop for entry in entries
                for index from 0
                for column = (floor index rows)
                for row = (mod index rows)
                for x = (+ 20 (* column column-width))
                for y = (+ top (* row line))
                while (< y (- (c:rect-h area) line))
                do (incf shown)
                   (let ((used (canvas-text canvas x y (car entry) key-color
                                            :scale p:*help-scale*)))
                     ;; Truncate rather than overflow into the next column: a
                     ;; help screen that overlaps itself is worse than one that
                     ;; abbreviates.
                     (let* ((offset (max left (+ used gap)))
                            (room (max 0 (- column-width offset gap 10)))
                            (fits (max 0 (floor room (text-width "m"
                                                                :scale p:*help-scale*)))))
                       (canvas-text canvas (+ x offset) y
                                    (truncate-text (cdr entry) fits)
                                    text-color :scale p:*help-scale*))))
          (canvas-text canvas 20 title-top
                       (cond
                         (customp (format nil "~a  --  any key closes this"
                                          (car *help-visible*)))
                         ;; What did not fit is said, rather than left to be
                         ;; discovered by somebody looking for a binding that
                         ;; is in fact bound.
                         ((< shown (length entries))
                          (format nil "LatticeWM -- ~d of ~d bindings; the rest ~
                                       are in --list-keys.  Any key closes this."
                                  shown (length entries)))
                         (t (format nil "LatticeWM -- ~d bindings.  ~
                                         Any key closes this."
                                    (length entries))))
                       key-color :scale p:*help-scale*))
        (overlay-commit *help-overlay* :rect area)))))

(defcommand help ()
  "Show every key binding on screen, with what it does.

Built from the live keymap and each command's own docstring, so it includes
anything you bound yourself and cannot go out of date.  Any key dismisses it."
  (setf p:*keymap-ever-opened* t)
  (setf *help-visible* (not *help-visible*))
  (mark-dirty)
  *help-visible*)

(defun show-help-page (title rows)
  "Put TITLE and ROWS on the help overlay.  Any key takes it down again."
  (setf *help-visible* (cons title rows))
  (mark-dirty)
  t)

(defcommand describe-command (name)
  "Show a command's documentation, its arguments and the keys that run it.

Emacs's C-h f, and the reason the docstrings in this system are written as
paragraphs rather than as labels: this is where they are read."
  (:interactive :command)
  (let ((command (find-command name)))
    (if command
        (show-help-page (format nil "M-x ~a" (command-name command))
                        (p:command-help-rows (policy) command))
        (notify "no such command: ~a" name))))

(defcommand describe-option (name)
  "Show a configuration value: what it is now, what it shipped as, and why.

Every option in this system carries a paragraph explaining what turning it off
costs you, and this is where those paragraphs are read."
  (:interactive :option)
  (if (not (p:option-boundp name))
      (notify "there is no option called ~(~a~)" name)
      (show-help-page
       (format nil "~(~a~)" name)
       (append (list (cons "now" (prin1-to-string (p:option name)))
                     (cons "default" (prin1-to-string (p:option-default name)))
                     (cons "" ""))
               (mapcar (lambda (line) (cons "" line))
                       (wrap-text (or (p:option-documentation name)
                                      "Undocumented.")
                                  78))
               (list (cons "" "")
                     (cons "" (format nil "Change it now with M-x set-option, ~
                                           or for good with (setf ~(~a~) ...) ~
                                           in your init.lisp."
                                      (second (assoc name (p:all-options))))))))))

(defcommand apropos-command ()
  "Search every command by name and by documentation, and show what matches.

Emacs's C-h a.  The one question a discoverable system has to be able to
answer is `is there a command for this', and neither the keymap nor M-x can
answer it — the keymap only knows what somebody bound, and M-x only matches
names.  This reads the docstrings, so `screen' finds `toggle-fullscreen' even
though the word does not occur in its name."
  (read-string "apropos: "
               (lambda (text)
                 (when (and text (plusp (length text)))
                   (let ((rows (loop for command in (all-commands)
                                     for documentation = (or (command-documentation
                                                              command) "")
                                     when (or (search text (command-name command)
                                                      :test #'char-equal)
                                              (search text documentation
                                                      :test #'char-equal))
                                       collect (cons (command-name command)
                                                     (or (summary-of documentation)
                                                         "")))))
                     (if rows
                         (show-help-page (format nil "apropos ~s -- ~d command~:p"
                                                 text (length rows))
                                         rows)
                         (notify "nothing matches ~a" text)))))
               :history "apropos"))

(add-hook :draw-overlays 'draw-help-overlay)
