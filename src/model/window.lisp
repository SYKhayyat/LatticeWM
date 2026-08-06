;;;; model/window.lisp --- What the window manager knows about a window.
;;;;
;;;; This class lives in the model rather than the runtime, for one structural
;;;; reason: policy has to be able to ask a window about itself — SHOULD-FLOAT-P
;;;; wants its parent and its app id, WINDOW-DIMENSIONS wants its size hints —
;;;; and policy may not depend on the runtime.  The runtime owns the river
;;;; proxy and fills these slots from protocol events; nothing here interprets
;;;; the proxy or calls through it.

(in-package #:latticewm/core)

(defclass window ()
  ((proxy :initarg :proxy :initform nil :accessor window-proxy
          :documentation
          "The river_window_v1 object.  Opaque here: the model stores it so the
runtime can find its way back, and never calls through it.")
   (identifier :initarg :identifier :initform nil :accessor window-identifier
               :documentation
               "River's stable identifier for this window: up to 32 printable
ASCII bytes, unique, never reused.  Sent once at creation.

This is what lets a layout survive a window manager restart — serialise
identifier to (workspace, path) on the way out, match on the way back in.
Note the limit: identifiers are never reused, so they survive a hot-swap but
not a reboot.  Spawn rules are what restore a layout across a reboot.")
   (app-id :initarg :app-id :initform nil :accessor window-app-id
           :documentation "The client's application id, e.g. \"firefox\".")
   (title :initarg :title :initform nil :accessor window-title)
   (parent :initarg :parent :initform nil :accessor window-parent-window
           :documentation
           "The window this one is a child of, or NIL.  River's spec says a
window with a parent 'might be a dialog, file picker, or similar', and that is
the whole basis of the shipped floating rule.")
   ;; Geometry, as last reported by the compositor.  Note these are what the
   ;; window *is*, not what we asked for: propose_dimensions is advisory and
   ;; clients quantise.
   (width :initform 0 :accessor window-width)
   (height :initform 0 :accessor window-height)
   (rect :initform nil :accessor window-rect
         :documentation "The rectangle the layout last assigned, or NIL.")
   ;; Size hints, advisory in both directions.
   (min-width :initform 0 :accessor window-min-width)
   (min-height :initform 0 :accessor window-min-height)
   (max-width :initform 0 :accessor window-max-width)
   (max-height :initform 0 :accessor window-max-height)
   (decoration-hint :initform nil :accessor window-decoration-hint)
   (capture-sessions :initform nil :accessor window-capture-sessions
                     :documentation
                     "How many screen capture sessions are recording this
window, or NIL for `river has not said'.

River sends the count once when the window is created and again whenever it
changes, so NIL means the event has not arrived yet rather than zero — which
is a distinction worth keeping, because it is the difference between `nobody
is recording this' and `we are not being told'.  Everything that reads it goes
through WINDOW-CAPTURED-P, which treats both the same way and is what you
want unless you are checking that the protocol is being spoken.")
   ;; Disposition.
   (floating :initform nil :accessor window-floating-p)
   (minimized :initform nil :accessor window-minimized-p)
   (fullscreen :initform nil :accessor window-fullscreen-p)
   (live :initform t :accessor window-live-p
         :documentation
         "NIL once river has told us the window is gone.  A dead window may
still be referenced briefly by in-flight events; every accessor tolerates it.")
   (home-path :initform nil :accessor window-home-path
              :documentation
              "Where this window was before it was minimized or floated, so it
can go back.  Advisory — the slot may no longer exist.")
   (tags :initform '() :accessor window-tags
         :documentation "Free-form symbols an extension may attach.")
   (props :initform '() :accessor props))
  (:documentation
   "One managed window.

Everything the compositor has told us, plus our own disposition.  Created when
river sends a window event and retained until it tells us the window is
gone."))

(defmethod print-object ((w window) stream)
  (print-unreadable-object (w stream :type t :identity nil)
    (format stream "~@[~a ~]~s~@[ ~dx~d~]~:[~; DEAD~]"
            (window-app-id w)
            (or (window-title w) "")
            (and (plusp (window-width w)) (window-width w))
            (and (plusp (window-width w)) (window-height w))
            (not (window-live-p w)))))

(defun window-captured-p (window)
  "Is something recording this window right now?

The count itself is only interesting when you are writing the indicator; every
other caller is asking this yes-or-no question, and asking it this way is what
keeps NIL — river has not said — from reading as a number somewhere."
  (let ((count (window-capture-sessions window)))
    (and count (plusp count))))

(defun window-preferred-size (window)
  "The size WINDOW would like, as (values WIDTH HEIGHT), or (values NIL NIL).

Only meaningful when the client pinned itself with equal minimum and maximum
hints, which is how a fixed-size dialog announces itself.  That is exactly the
case where honouring it matters, and the case where guessing is worst."
  (let ((min-w (window-min-width window)) (max-w (window-max-width window))
        (min-h (window-min-height window)) (max-h (window-max-height window)))
    (values (when (and (plusp min-w) (= min-w max-w)) min-w)
            (when (and (plusp min-h) (= min-h max-h)) min-h))))
