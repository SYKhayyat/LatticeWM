;;;; window-rules/window-rules.lisp --- The table, and the two doors it needs.
;;;;
;;;; The example's shape, behind a switch: WINDOW-RULE-FOR consults *RULES*
;;;; before the shipped answer, and ON-WINDOW-OPEN honours the rule's
;;;; :WORKSPACE by moving the cursor for the duration of the placement and
;;;; putting it back.  Both are :AROUND methods so DISABLE is exact: with the
;;;; switch off neither has any effect, and nothing was ever replaced.

(in-package #:window-rules)

(defvar *enabled* nil "Whether the rules are in force.")

(defvar *rules* '()
  "Rules, in order.  The first match wins.

Each entry is (MATCH . OVERRIDES).  MATCH is an app-id string or a function of
the window -- backquote the list if you want function literals in it.
OVERRIDES is a plist: :FLOAT, :WORKSPACE, :FOCUS.  A window matching no rule
is placed by whatever would have placed it anyway.")

(defun enabled-p () "True when the rules are in force." *enabled*)

;;; ------------------------------------------------------------- the rules

(defun matches-p (match window)
  (etypecase match
    (string (equal match (c:window-app-id window)))
    (function (funcall match window))))

(defun rule-for (window)
  "The first rule whose MATCH accepts WINDOW, as its OVERRIDES plist, or NIL."
  (loop for (match . overrides) in *rules*
        when (matches-p match window) return overrides))

(defmethod p:window-rule-for :around ((policy p:conventional-policy)
                                      (win c:window))
  "Consult *RULES* first; an unmatched window falls through to the shipped
answer unchanged."
  (when *enabled*
    (let ((overrides (rule-for win)))
      (when overrides
        (return-from p:window-rule-for overrides))))
  (call-next-method))

(defmethod p:on-window-open :around ((policy p:conventional-policy) world
                                     (win c:window))
  "Send a window to the workspace its rule names, before placing it.

The rule is consulted here and the actual placement is left to
CALL-NEXT-METHOD: reimplementing placement in order to add one behaviour to it
is the mistake this surface exists to make unnecessary."
  (if (not *enabled*)
      (call-next-method)
      (let ((workspace (getf (rule-for win) :workspace)))
        (if (null workspace)
            (call-next-method)
            (let ((stack (c:world-workspaces world))
                  (previous (c:world-cursor world)))
              (cond
                ((null stack) (call-next-method))
                (t
                 ;; Grow the workspace list if the rule names one that does not
                 ;; exist yet, then place the window there with the cursor
                 ;; temporarily moved, and put the cursor back.
                 (loop while (<= (c:container-count stack) workspace)
                       do (c:insert-child stack (c:container-count stack)
                                          (c:make-leaf)))
                 (setf (c:world-cursor world)
                       (c:repair-path (c:world-root world) (list workspace)))
                 (unwind-protect (call-next-method)
                   (unless (getf (rule-for win) :focus)
                     (setf (c:world-cursor world)
                           (c:repair-path (c:world-root world)
                                          previous)))))))))))

;;; ------------------------------------------------------- enable / disable

(defun enable ()
  "Turn the rules on, live.  Loading alone changes nothing."
  (setf *enabled* t)
  (values))

(defun disable ()
  "Turn the rules off, live.  The rules stay in *RULES* and take effect again
on the next ENABLE."
  (setf *enabled* nil)
  (values))
