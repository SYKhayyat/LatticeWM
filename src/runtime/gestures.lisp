;;;; runtime/gestures.lisp --- Touch gestures, table-driven (#16).
;;;;
;;;; THE TABLE IS THE POLICY'S AND THIS FILE IS THE HANDS.  P:*GESTURES* names
;;;; gestures and the commands they run; P:GESTURE-PLANS* turns that table into
;;;; plans with every number filled in; this file takes plans and makes
;;;; river_touch_gesture_v1 objects out of them.  A spec with no protocol in it
;;;; is testable without a compositor, and the protocol half has exactly one
;;;; question to answer -- which object to configure.
;;;;
;;;; THREE THINGS ABOUT THE PROTOCOL SHAPE THIS FILE.
;;;;
;;;; One object per (finger count, trigger), not one per finger count.  River
;;;; triggers *one* gesture when several thresholds are met at once and leaves
;;;; which one to compositor policy, so a single object cannot report `left or
;;;; up'; two objects can each report their own.
;;;;
;;;; EVERYTHING IS MANAGE-SEQUENCE STATE.  enable, disable and the four
;;;; threshold setters say so in their own descriptions, so defining a gesture
;;;; -- which a command does, from a key binding, outside any sequence -- records
;;;; a dirty flag and SYNC-GESTURES does the work inside the next one.  That is
;;;; the same shape as keybindings and the same reason.
;;;;
;;;; AND THE EVENTS ARRIVE IN A MANAGE SEQUENCE.  River sends gesture events
;;;; followed by manage_start, so a gesture's command runs where a key binding's
;;;; does, through RUN-COMMAND and AFTER-COMMAND, and undo, repeat and the
;;;; command wrappers all work without knowing gestures exist.

(in-package #:latticewm/runtime)

;;; ------------------------------------------------------------- the codes

;;; The protocol's own enums, transcribed once.  Zero is `none' in both and is
;;; deliberately absent from these tables: `none' is what a threshold says when
;;; it is not being asked for, not a direction a spec can name.

(defparameter +gesture-direction-codes+
  '((:up . 1) (:down . 2) (:left . 3) (:right . 4))
  "The river_touch_gesture_v1 direction enum.")

(defparameter +gesture-edge-codes+
  '((:top . 1) (:bottom . 2) (:left . 3) (:right . 4))
  "The river_touch_gesture_v1 edge enum, which is a different enum with the
same names in a different order -- :DOWN is 2 in one and :BOTTOM is 2 in the
other.  Keeping them apart is the whole reason there are two tables.")

(defparameter +gesture-pinch-far+ 10.0
  "The outward threshold given to a gesture that only wants an inward pinch.

The protocol takes both thresholds in one request and triggers on either, so
`pinch in' has to say what would count as out.  Ten times the original distance
is not a pinch anybody makes.")

(defparameter +gesture-pinch-near+ 0.1
  "And the inward threshold for a gesture that only wants an outward one.")

;;; --------------------------------------------------------------- the seats

(defun gesture-seat-of (seat)
  "SEAT's river_touch_gestures_seat_v1, or NIL when there is no protocol."
  (c:prop seat :gesture-seat))

(defun attach-gesture-seat (seat proxy)
  "Give SEAT a gesture seat object, if this compositor speaks gestures at all.

Called from ATTACH-SEAT beside ATTACH-LAYER-SEAT.  Absent protocol is not an
error and not a warning: it is every river older than 0.5.0, which is most of
them, and gestures simply do not exist there."
  (let ((global (server-touch-gestures *server*)))
    (when global
      (setf (c:prop seat :gesture-seat)
            (guarded "gesture seat"
              (w:gestures-get-seat global proxy)))
      (setf (server-gestures-dirty *server*) t)))
  (c:prop seat :gesture-seat))

(defun detach-gesture-seat (seat)
  "Destroy SEAT's gesture objects and its gesture seat."
  (clear-gestures seat)
  (let ((gesture-seat (gesture-seat-of seat)))
    (when gesture-seat
      (best-effort "gesture seat destroy" (w:gestures-seat-destroy gesture-seat))
      (setf (c:prop seat :gesture-seat) nil))))

(defun clear-gestures (seat)
  "Destroy every gesture object SEAT holds.  Safe outside a manage sequence."
  (dolist (entry (c:prop seat :gestures))
    (best-effort "gesture destroy" (w:gesture-destroy (first entry))))
  (setf (c:prop seat :gestures) nil))

;;; --------------------------------------------------------------- the plans

(defun sync-gestures ()
  "Rebuild every seat's gesture objects from the table.  Manage sequence only.

The planner drops what it cannot apply and logs why, so a table with one bad
entry binds the rest -- which is the contract a config file has everywhere else
here.  A compositor without the protocol plans nothing and is told so once."
  (let ((plans (guarded "gesture-plans" (p:gesture-plans))))
    (dolist (seat (server-seats *server*))
      (let ((gesture-seat (gesture-seat-of seat)))
        (clear-gestures seat)
        (when gesture-seat
          (dolist (plan plans)
            (guarded "gesture plan"
              (apply-gesture-plan seat gesture-seat plan))))))
    (when (and plans (null (server-touch-gestures *server*)))
      (logmsg :info "~d gesture~:p configured, but this river has no ~
                     river_touch_gestures_v1; they will not fire"
              (length plans)))
    plans))

(defun apply-gesture-plan (seat gesture-seat plan)
  "Make one river_touch_gesture_v1 for PLAN and enable it."
  (let* ((fingers (getf plan :fingers))
         (object (w:gestures-seat-get-gesture gesture-seat fingers)))
    (configure-gesture object plan)
    (push (cons object plan) (c:prop seat :gestures))
    (watch-gesture object plan)
    (w:gesture-enable object)
    (logmsg :debug "gesture: ~d finger~:p ~(~a~) ~(~a~) -> ~a"
            fingers (or (getf plan :direction) (getf plan :pinch)
                        (getf plan :edge))
            (if (getf plan :pinch) 'pinch 'swipe)
            (getf plan :command))
    object))

(defun configure-gesture (object plan)
  "Set the one threshold PLAN asks for on OBJECT.  Manage sequence only."
  (cond
    ((getf plan :direction)
     (w:gesture-set-direction object
                              (or (cdr (assoc (getf plan :direction)
                                              +gesture-direction-codes+))
                                  0)
                              (getf plan :distance)))
    ((getf plan :edge)
     (w:gesture-set-edge object
                         (or (cdr (assoc (getf plan :edge)
                                         +gesture-edge-codes+))
                             0)
                         (getf plan :distance)))
    ((getf plan :pinch)
     (let ((scale (getf plan :scale)))
       (if (eq (getf plan :pinch) :in)
           (w:gesture-set-threshold-scale object scale +gesture-pinch-far+)
           (w:gesture-set-threshold-scale object +gesture-pinch-near+ scale))))
    (t nil))
  object)

(defun watch-gesture (object plan)
  "Attach the event handlers for OBJECT, which runs PLAN's command at :START."
  (on-events (object "river_touch_gesture_v1")
    (:start (gesture-fired plan))
    ;; Motion and scale are for whoever wants a continuous gesture -- a pan, a
    ;; zoom, a slider.  They are hooks rather than methods because there may be
    ;; several such things at once and because the discrete half above is the
    ;; only part that is a command.
    ;;
    ;; EACH ONE GOES THROUGH A NAMED FUNCTION, which is not ceremony: a hook
    ;; fired from inside an event handler is a hook no test can pull without a
    ;; compositor, and gate 14's whole demand is that somebody has pulled it and
    ;; said what arrived.
    (:delta-motion (gesture-moved (first arguments) (second arguments)))
    (:scale (gesture-scaled (first arguments)))
    (:finger-count (gesture-fingers-changed (first arguments)))
    (:end (gesture-finished plan))
    (:cancel (gesture-finished plan))
    (t nil))
  object)

;;; ------------------------------------------------------ the stream, named
;;;
;;; Four functions between the protocol and the hooks, so that the stream has a
;;; seam a test can drive.  They are not a second layer: each is a RUN-HOOKS and
;;; nothing else.

(defun gesture-moved (dx dy)
  "Report a gesture's centroid motion, cumulative since touch down."
  (run-hooks :gesture-motion dx dy))

(defun gesture-scaled (scale)
  "Report a pinch's scale: 1.0 at touch down, below for together, above for apart."
  (run-hooks :gesture-scale scale))

(defun gesture-fingers-changed (count)
  "Report that the running gesture's touch point count changed."
  (run-hooks :gesture-fingers count))

(defun gesture-finished (plan)
  "Report the end of a gesture, completed or cancelled, with its PLAN."
  (run-hooks :gesture-end plan))

(defun gesture-fired (plan)
  "PLAN's gesture was recognised: run its command, as a key binding would."
  (run-hooks :gesture-start plan)
  (logmsg :debug "gesture fired: ~d finger~:p -> ~a"
          (getf plan :fingers) (getf plan :command))
  (run-command (getf plan :command))
  (after-command))

;;; ------------------------------------------------------------- the commands

(defcommand gestures ()
  "List the gesture table: what each gesture runs, and whether it can fire."
  (let ((plans (p:gesture-plans)))
    (when (null plans)
      (notify "no gestures configured"))
    (when plans
      (notify "~d gesture~:p~@[ (no river_touch_gestures_v1 on this ~
              compositor)~]"
              (length plans) (null (server-touch-gestures *server*)))))
  (dolist (plan (p:gesture-plans))
    (logmsg :info "~d finger~:p ~(~a~)~@[ from the ~(~a~) edge~] -> ~a~@[ ~
                   (will not fire: no gesture protocol)~]"
            (getf plan :fingers)
            (or (getf plan :direction) (getf plan :pinch) (getf plan :edge))
            (getf plan :edge)
            (getf plan :command)
            (null (server-touch-gestures *server*))))
  (p:gesture-table-names))

(defcommand gesture (spec command)
  "Bind a touch gesture to a command, live.

SPEC is fingers then the gesture, written the way you would say it: `3 left',
`3 edge top', `2 pinch in', `4 up'.  The trailing number of an edge gesture is
the edge it must start from, so `1 edge top' is a one-finger swipe down from the
top of the screen.

COMMAND is anything you can run: a workspace switch, a focus move, a command an
extension adds.  The binding is added to the front of the table and takes effect
at the next manage sequence, which is when the protocol allows it."
  (:interactive :gesture-spec :command)
  (p:gesture-add spec command)
  (setf (server-gestures-dirty *server*) t)
  (request-manage)
  (notify "gesture: ~d finger~:p ~(~a~) -> ~a" (getf spec :fingers)
          (or (getf spec :direction) (getf spec :pinch) (getf spec :edge))
          command))

(defcommand forget-gesture (spec)
  "Unbind a touch gesture.  `M-x forget-gesture 3 left'."
  (:interactive :gesture-spec)
  (p:gesture-remove spec)
  (setf (server-gestures-dirty *server*) t)
  (request-manage)
  (notify "gesture forgotten: ~d finger~p ~(~a~)" (getf spec :fingers)
          (or (getf spec :direction) (getf spec :pinch) (getf spec :edge))))

(defcommand refresh-gestures ()
  "Rebuild the gesture objects from the current table.

The table is an option like any other, so a config file reload or a `setf' from
the control socket changes it without telling anybody.  This is the one verb for
`I changed my mind, apply it' -- the same shape as REBIND-KEYS."
  (setf (server-gestures-dirty *server*) t)
  (request-manage)
  (notify "gestures: ~d configured" (length (p:gesture-plans))))
