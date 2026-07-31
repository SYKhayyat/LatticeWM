;;;; examples/02-window-rules.lisp
;;;;
;;;; TIER 1 — a table plus one method.  The extension everybody writes first.
;;;;
;;;; The point of this file is that a *declarative* window-rule system does not
;;;; need to be a feature.  It is a table and a method, it is twenty lines, and
;;;; because it is ordinary Lisp the "rule language" is Lisp — so a predicate
;;;; that needs to consult the time of day, or the number of windows already
;;;; open, or a file, just does.
;;;;
;;;; That is the difference the whole project is arguing for.  A window manager
;;;; with a rules DSL gives you exactly the predicates its author thought of.

(in-package #:latticewm/user)

(defparameter *rules*
  ;; Backquoted, not quoted, so a match can be a function rather than a string.
  `(;; (MATCH . OVERRIDES)
    ;;
    ;; MATCH is an app-id string, or a function of the window.
    ;; OVERRIDES is a plist: :FLOAT, :WORKSPACE, :FOCUS, :PATH.
    ("pavucontrol"      :float t)
    ("blueman-manager"  :float t)
    ("org.gnome.Calculator" :float t)
    ("firefox"          :workspace 1)
    ("thunderbird"      :workspace 2 :focus nil)
    ;; A function match: anything that announced itself as a fixed-size dialog
    ;; and is small.  The shipped SHOULD-FLOAT-P already floats fixed-size
    ;; windows; this is here to show that a predicate is just a predicate.
    (,(lambda (win)
        (multiple-value-bind (width height) (window-preferred-size win)
          (and width height (< (* width height) (* 400 300)))))
     :float t))
  "Rules, in order.  The first match wins.")

(defun window-matches-p (match window)
  (etypecase match
    (string (equal match (window-app-id window)))
    (function (funcall match window))))

(defmethod window-rule-for ((policy conventional-policy) (win window))
  "The first matching rule from *RULES*, or whatever the shipped policy says.

Consulted once, when the window appears.  ON-WINDOW-OPEN honours :FLOAT and
:PATH itself; :WORKSPACE is handled below, because the shipped policy has no
opinion about workspaces and this is how you give it one."
  (or (loop for (match . overrides) in *rules*
            when (window-matches-p match win) return overrides)
      (call-next-method)))

(defmethod on-window-open ((policy conventional-policy) world (win window))
  "Send a window to the workspace its rule names, before placing it.

Note the shape: the rule is *consulted* here and the actual placement is left
to CALL-NEXT-METHOD.  Reimplementing placement in order to add one behaviour
to it is the mistake this surface exists to make unnecessary."
  (let ((workspace (getf (window-rule-for policy win) :workspace)))
    (if (null workspace)
        (call-next-method)
        (let ((stack (world-workspaces world))
              (previous (world-cursor world)))
          (cond
            ((null stack) (call-next-method))
            (t
             ;; Grow the workspace list if the rule names one that does not
             ;; exist yet, then place the window there with the cursor
             ;; temporarily moved, and put the cursor back.
             (loop while (<= (container-count stack) workspace)
                   do (insert-child stack (container-count stack) (make-leaf)))
             (setf (world-cursor world) (repair-path (world-root world)
                                                     (list workspace)))
             (unwind-protect (call-next-method)
               (unless (getf (window-rule-for policy win) :focus)
                 (setf (world-cursor world)
                       (repair-path (world-root world) previous))))))))))
