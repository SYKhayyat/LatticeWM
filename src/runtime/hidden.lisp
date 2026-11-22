;;;; runtime/hidden.lisp --- The hidden set: what is alive and on no screen (#6).
;;;;
;;;; THE ISSUE'S OWN SCOPE NOTE IS THE DESIGN, and it is right: most of
;;;; "invisible windows" already exists.  MINIMIZE takes a window out of the
;;;; tiling tree and onto the world's scratchpad, ON-RESTORE puts it back in the
;;;; slot it left, and #34's FOCUS-WINDOW-BY-NAME completes over every window
;;;; the session has.  The delta is not a new state -- it is that the hidden set
;;;; was never visible as a *set*: nothing listed it, and the only picker that
;;;; reached it offered all forty windows to reach the three you cannot see.
;;;;
;;;; So this file adds no state at all.  It asks the world where the hidden
;;;; windows are -- C:WORLD-SCRATCHPAD, which is already the answer three other
;;;; files give -- and it draws two things on top: a report, and a picker whose
;;;; whole list is the hidden windows.
;;;;
;;;; "INVISIBLE, NOT HIDDEN-BUT-LISTED" was the issue's third question, and the
;;;; answer is that this program has no taskbar to be visible in.  A window on
;;;; the scratchpad is on no screen and in no list until you ask; the difference
;;;; between Emacs's buried buffers and this is a bar nobody draws.

(in-package #:latticewm/runtime)

;;; -------------------------------------------------------------- the answers

(defun hidden-windows (&optional (world *world*))
  "The windows that are alive and on no screen, most recently hidden first.

ASKED OF THE WORLD, NOT OF THE WINDOW'S FLAG.  WINDOW-MINIMIZED-P is a
disposition something sets and the scratchpad is where the window is; they
agree in every path the program has, and where they would disagree the place
is the answer a list of *places* wants.  It is also the answer WINDOW-PLACE-NAME
gives, so the picker and the report cannot disagree about which set this is."
  (and world (c:world-scratchpad world)))

(defun hidden-applications (&optional (world *world*))
  "Which programs have a hidden window, sorted, once each."
  (sort (delete-duplicates
         (remove nil (mapcar #'c:window-app-id (hidden-windows world)))
         :test #'string=)
        #'string<))

(defun hidden-return-place (window &optional (world *world*))
  "Where WINDOW goes back to, in words, for a report that has to say so.

This is the one detail a list of hidden windows needs and cannot get from the
window: RESTORE is not `show it wherever there is room', it is `put it back
where it was if that place still exists'.  A user choosing between three
hidden windows is choosing between three places, and a report that printed only
titles would leave them guessing."
  (let* ((root (and world (c:world-root world)))
         (home (c:window-home-path window))
         (target (and root home (c:resolve-path root home))))
    (if (and target (typep target 'c:leaf) (c:leaf-empty-p target))
        "back where it was"
        "into the pane you are in")))

;;; -------------------------------------------------------------- the picker

(defun hidden-picker-entries ()
  "HIDDEN-WINDOWS labelled the way #34's picker labels windows.

THE SAME LABELLING FUNCTION, not a second one.  A window named twice in one
session is a real case -- two terminals, both called `Terminal' -- and the
pickers agreeing about how to spell that is the difference between a name you
learn once and two names to keep straight."
  (window-picker-entries (hidden-windows)))

(define-argument-type :hidden "hidden: "
  :documentation "A window that is alive and on no screen."
  :candidates (mapcar #'car (hidden-picker-entries))
  :parse (lambda (text)
           (or (window-by-picker-name text (hidden-windows))
               (error "no hidden window matches ~a" text))))

(define-argument-type :hidden-app "hidden application: "
  :documentation "A program that has a hidden window."
  :candidates (hidden-applications)
  :parse (lambda (text)
           (let ((name (string-trim " " text)))
             (if (member name (hidden-applications) :test #'string-equal)
                 (find name (hidden-applications) :test #'string-equal)
                 (error "no hidden window belongs to ~a" name)))))

;;; --------------------------------------------------------------- the verbs

(defcommand list-hidden ()
  "Show what is alive and on no screen, and where each one will come back to.

There is no taskbar here, so a window you put away is in no list until you ask
for one.  This is the list: the windows, the program each belongs to, and the
place RESTORE will put it -- `back where it was' when the slot survived, `into
the pane you are in' when it did not."
  ()
  (let ((windows (hidden-windows)))
    (cond
      ((null windows)
       (notify "nothing is hidden")
       nil)
      (t
       (show-help-page
        (format nil "hidden (~d)" (length windows))
        (mapcar (lambda (window)
                  (cons (p:name-of-window window)
                        (format nil "~a  ~a"
                                (or (c:window-app-id window) "")
                                (hidden-return-place window))))
                windows))
       (notify "~d hidden: ~{~a~^, ~}"
               (length windows)
               (mapcar (lambda (window) (p:name-of-window window)) windows))
       windows))))

(defcommand hidden (window)
  "Bring back one of the windows that is on no screen.

The picker offers the hidden set and nothing else, which is the whole point:
`M-x focus-window-by-name' reaches every window this session has, and reaching
three you cannot see through forty you can is the difficulty this command
removes.  Choosing one restores it to the place it left and puts the cursor
there."
  (:interactive :hidden)
  (cond
    ((null window) (notify "no hidden window matches") nil)
    (t (show-hidden-window window))))

(defun show-hidden-window (window)
  "Restore WINDOW and put the cursor in it.  The one path both commands take."
  (restore-window window)
  (focus-existing-window window)
  (notify "~a (~a)" (p:name-of-window window) (hidden-return-place-so-far window))
  window)

(defun hidden-return-place-so-far (window)
  "Where WINDOW is now, in the same two words WINDOW-PLACE-NAME uses.

Called after the restore, so `scratchpad' is the wrong answer -- the question
the notification answers is where you have just been taken."
  (let ((place (window-place-name window)))
    (if (string= place "scratchpad") "restored" place)))

(defcommand restore-app (app-id)
  "Bring back every hidden window belonging to one program.

THE REASON THIS IS A COMMAND AND NOT A LOOP THE USER WRITES: hidden windows
are hidden as *windows*, but they are put away as *work* -- a browser with
three windows is one thing you stopped doing.  All of them come back, in the
order they were hidden, each to its own remembered place."
  (:interactive :hidden-app)
  (let* ((matches (remove-if-not (lambda (window)
                                   (and (c:window-app-id window)
                                        (string-equal app-id (c:window-app-id window))))
                                 (hidden-windows)))
         (count (length matches)))
    (cond
      ((zerop count) (notify "no hidden window belongs to ~a" app-id) nil)
      (t
       ;; Reverse: the scratchpad is most-recent-first, and restoring in that
       ;; order would put the newest window in the oldest slot's place.
       (dolist (window (reverse matches)) (restore-window window))
       (focus-existing-window (first matches))
       (notify "~d window~:p back from ~a" count app-id)
       count))))

(defcommand restore-all ()
  "Bring back every window that is on no screen.

The complement of `minimize', and the reason it exists: a hidden set with no
way to empty it is a set that grows and is never audited.  Each window goes
back to its own remembered place, oldest first."
  ()
  (let ((windows (reverse (hidden-windows))))
    (cond
      ((null windows) (notify "nothing is hidden") nil)
      (t
       (dolist (window windows) (restore-window window))
       (notify "~d window~:p back" (length windows))
       (length windows)))))
