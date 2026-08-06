;;;; runtime/capture.lisp --- Who is recording, and saying so.
;;;;
;;;; River 0.4.6 added one event, twice: `capture_sessions' on river_window_v1
;;;; and on river_output_v1, each carrying the number of screen capture
;;;; sessions currently reading that window or that whole monitor.  It is sent
;;;; once when the object is created and again on every change, and it is the
;;;; only way a window manager on this protocol can know that something is
;;;; watching the screen.
;;;;
;;;; THIS FILE IS THE FEATURE THAT PARAGRAPH IMPLIES.  src/protocol/PINNED
;;;; carried a sentence saying the two events were unhandled and that this was
;;;; "a feature we do not have rather than a protocol we cannot speak" — true,
;;;; and the honest reading of it is that the feature was worth having: the one
;;;; thing you cannot see by looking at your own screen is that somebody else
;;;; can see it too.  A screen share you forgot to stop looks exactly like a
;;;; screen share you never started.
;;;;
;;;; So the count is recorded on the model, where policy can read it; a change
;;;; is announced in the echo area, which is the only place this program can
;;;; talk; the shipped status line carries a standing REC while it lasts,
;;;; because an announcement scrolls away and the fact does not; and
;;;; :CAPTURE-CHANGED is the seam for anything else — a status bar, a light on
;;;; a keyboard, a script that mutes a microphone.
;;;;
;;;; Nothing here changes what the window manager *does*.  A window being
;;;; recorded is still tiled, hidden, minimized and closed exactly as it was:
;;;; refusing to hide a recorded window would be this file deciding something
;;;; the user did not ask for, and river is perfectly able to go on feeding a
;;;; capture session whatever it feeds it.  This is information, and it is
;;;; deliberately only information.

(in-package #:latticewm/runtime)

(p:define-option *announce-capture* t
  "Say in the echo area when a screen capture starts or stops.

T is the privacy-preserving default and the reason the events are handled at
all.  The standing REC segment in the shipped status line is separate and is
not affected by this — set the option to NIL if you want the fact on screen
without a line about it every time OBS adds a source.")

(defun capture-sessions-of (subject)
  "How many capture sessions SUBJECT — a window or an output — has, or NIL."
  (etypecase subject
    (c:window (c:window-capture-sessions subject))
    (c:output (c:output-capture-sessions subject))))

(defun (setf capture-sessions-of) (count subject)
  (etypecase subject
    (c:window (setf (c:window-capture-sessions subject) count))
    (c:output (setf (c:output-capture-sessions subject) count))))

(defun capture-subject-name (subject)
  "What to call SUBJECT when telling the user about it."
  (etypecase subject
    (c:window (p:name-of-window subject))
    (c:output (format nil "screen ~a" (or (c:output-name subject) "?")))))

(defun note-capture-sessions (subject count)
  "Record that SUBJECT now has COUNT screen capture sessions.

Called from both event handlers.  The stored value starts as NIL — river has
not said — and that is what makes the announcement rule expressible: a window
created with no capture sessions must not say anything, while a window created
*during* a screen share must, and both arrive as the same first event."
  (let* ((before (capture-sessions-of subject))
         (was (and before (plusp before)))
         (now (plusp count)))
    (unless (eql before count)
      (setf (capture-sessions-of subject) count)
      ;; REDRAW ONLY WHEN THE SCREEN WOULD DIFFER.  The status line says
      ;; whether a thing is being recorded, not by how many sessions, so a
      ;; count going 1 -> 2 changes nothing anybody can see.  Every window and
      ;; every output reports a count at creation, and outside a sequence
      ;; MARK-DIRTY asks river for a manage round trip — so marking on every
      ;; count would spend one per object at startup to draw the same line.
      (unless (eq was now) (mark-dirty))
      (when *announce-capture*
        (cond ((and now (not was))
               (notify "recording started: ~a~@[ (~d sessions)~]"
                       (capture-subject-name subject)
                       (and (> count 1) count)))
              ((and was (not now))
               (notify "recording stopped: ~a" (capture-subject-name subject)))))
      (run-hooks :capture-changed subject count)
      (logmsg :debug "capture sessions on ~s: ~a -> ~d" subject before count))
    count))
