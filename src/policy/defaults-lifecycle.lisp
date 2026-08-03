;;;; policy/defaults-lifecycle.lisp --- The shipped answers for the *window*
;;;; half of LIFECYCLE-POLICY: floating, decoration, capabilities.
;;;;
;;;; The *event* half — what happens when a window arrives, leaves, minimizes
;;;; or is restored — is policy/lifecycle.lisp, along with the window-rule
;;;; vocabulary it reads.  These are separated because one is about a window's
;;;; properties and the other is about tree surgery, and mixing them makes both
;;;; harder to read.

(in-package #:latticewm/policy)

;;; ==================================================================
;;; WINDOW LIFECYCLE
;;; ==================================================================

(define-option *float-fixed-size-limit* '(900 . 700)
  "The largest a fixed-size window may be and still be floated for it.

A window that pins its minimum and maximum size to the same value is announcing
that it is a dialog — and that signal is right often enough to be the second
half of the shipped floating rule.  It is also *wrong* for a class of
application, which is why there is a limit here:

  * a number of GTK and Qt programs set fixed hints on their **main** window;
  * several set them transiently during startup, before the real size is known.

The user-visible result was a main application window arriving floating for no
reason the user could see and no diagnostic explaining the decision.  A
fixed-size window the size of the screen is not a dialog, and this is where
that gets said.  Set it to NIL to float every fixed-size window, or to
(0 . 0) to float none of them.")

(defmethod should-float-p ((policy lifecycle-policy) (window c:window))
  "Anything with a parent floats; a small fixed-size window floats; else tile.

River's spec says a window with a parent 'might be a dialog, file picker, or
similar', and that one signal covers the overwhelming majority of windows that
should not be tiled — without an application blacklist that goes stale.

A fixed-size hint is the second signal and catches the rest, *with a size
limit*.  Without one this over-floated: a program that pins its main window's
size, or pins it transiently during startup, arrived floating with nothing
anywhere to say why.  Every decision this method makes is now logged at
:DEBUG, naming the hint that caused it, because `why is this floating' had no
answer at all and is the first question anybody asks."
  (cond
    ((and *float-dialogs* (c:window-parent-window window))
     (logmsg :debug "~a floats: it has a parent, so river calls it a dialog"
             (or (c:window-app-id window) "a window"))
     t)
    (t
     (multiple-value-bind (want-w want-h) (c:window-preferred-size window)
       (cond
         ((not (and want-w want-h)) nil)
         ((and *float-fixed-size-limit*
               (or (> want-w (car *float-fixed-size-limit*))
                   (> want-h (cdr *float-fixed-size-limit*))))
          (logmsg :debug "~a asked for a fixed ~dx~d, which is larger than ~
                          *float-fixed-size-limit* — tiling it"
                  (or (c:window-app-id window) "a window") want-w want-h)
          nil)
         (t
          (logmsg :debug "~a floats: it pinned itself to a fixed ~dx~d"
                  (or (c:window-app-id window) "a window") want-w want-h)
          t))))))

(defmethod default-float-rect ((policy lifecycle-policy) (window c:window)
                               (output c:output))
  "Honour the window's own preferred size when it pinned one, centred;
otherwise take a fraction of the output."
  (let ((area (c:output-rect output)))
    (multiple-value-bind (want-w want-h) (c:window-preferred-size window)
      (let ((w (or want-w (round (* (c:rect-w area) *float-fraction*))))
            (h (or want-h (round (* (c:rect-h area) *float-fraction*)))))
        (c:make-rect (+ (c:rect-x area) (floor (- (c:rect-w area) w) 2))
                     (+ (c:rect-y area) (floor (- (c:rect-h area) h) 2))
                     w h)))))

(defmethod window-capabilities ((policy lifecycle-policy) (window c:window))
  "Declare all four.  We honour all four, and fullscreen is free.

Returns a list of keywords drawn from :WINDOW-MENU, :MAXIMIZE, :FULLSCREEN and
:MINIMIZE — river's bitfield, in the keyword-list form the bindings use.
River draws client-side-decoration titlebar buttons from this, so declaring a
capability you do not honour produces a button that does nothing."
  (or (c:prop window :capabilities)
      (list :window-menu :maximize :fullscreen :minimize)))

(defmethod decoration-mode ((policy lifecycle-policy) (window c:window))
  "Server-side, which under river means our borders and no client titlebar.

A window rule's :DECORATION overrides it, which is what an application that
draws a titlebar you actually want — a browser with tabs in it — needs."
  (or (c:prop window :decoration) :ssd))

;; WINDOW-RULE-FOR's default method is in policy/lifecycle.lisp, beside the
;; rule vocabulary it reads and the code that applies it.

;; CONTAINER-LABEL and CONTAINER-ROLE are structure decisions and live in
;; policy/structure.lisp; ON-KEY is an input decision and lives in
;; policy/input.lisp.  They were here because this file used to be everything.