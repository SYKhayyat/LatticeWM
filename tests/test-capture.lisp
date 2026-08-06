;;;; tests/test-capture.lisp --- Knowing that something is recording.
;;;;
;;;; river reports the number of screen capture sessions on each window and on
;;;; each output.  Everything about what the window manager does with that is
;;;; establishable by constructing state — the three-valued count, the
;;;; announcement rule, which windows a listing has to walk, and whether the
;;;; shipped status line carries the fact — so it belongs here.
;;;;
;;;; The one thing that does not is whether river actually sends the events,
;;;; which is a claim about a compositor and is asserted in
;;;; tools/integration.lisp's "screen capture" section.

(in-package #:latticewm/tests)
(in-suite capture)

(defvar *capture-notices* '()
  "(SUBJECT . COUNT) pairs the test hook saw, most recent first.")

(defun note-capture-for-test (subject count)
  (push (cons subject count) *capture-notices*))

(defmacro with-capture-watch (&body body)
  "Run BODY with the :CAPTURE-CHANGED hook and the echo message watched.

The hook is added by symbol and removed again, so nothing survives into the
next test — the failure ADD-HOOK's own docstring warns about."
  `(let ((*capture-notices* '())
         (p:*echo-message* nil)
         (r::*announce-capture* t))
     (p:add-hook :capture-changed 'note-capture-for-test)
     (unwind-protect (progn ,@body)
       (p:remove-hook :capture-changed 'note-capture-for-test))))

(defun announcement ()
  "What the echo area was last told, or NIL."
  (car p:*echo-message*))

;;; ------------------------------------------------------- the count itself

(test a-count-nobody-has-reported-is-not-zero
  ;; NIL is `river has not said' and 0 is `nobody is recording', and the
  ;; difference is the whole announcement rule: a window created while a screen
  ;; share is running has to say so, and it arrives as the same first event as
  ;; a window created into silence.
  (let ((window (win "firefox")))
    (is (null (c:window-capture-sessions window)))
    (is (null (c:window-captured-p window)))
    (r::note-capture-sessions window 0)
    (is (eql 0 (c:window-capture-sessions window)))
    (is (null (c:window-captured-p window)))))

(test a-recorded-window-says-so
  (let ((window (win "firefox")))
    (r::note-capture-sessions window 0)
    (r::note-capture-sessions window 1)
    (is (c:window-captured-p window))
    (r::note-capture-sessions window 0)
    (is (null (c:window-captured-p window)))))

(test an-output-carries-the-same-count
  (let ((output (make-instance 'c:output :name "DP-1")))
    (is (null (c:output-captured-p output)))
    (r::note-capture-sessions output 2)
    (is (eql 2 (c:output-capture-sessions output)))
    (is (c:output-captured-p output))))

;;; ------------------------------------------------- the hook and the notice

(test starting-and-stopping-are-announced-and-nothing-else-is
  (with-capture-watch
    (let ((window (win "obs-studio")))
      ;; Creation with nothing recording: bookkeeping, no announcement.
      (r::note-capture-sessions window 0)
      (is (null (announcement)))
      (is (equal '(0) (mapcar #'cdr *capture-notices*))
          "the hook still fires, because a status bar wants the zero too")
      ;; Somebody starts recording.
      (r::note-capture-sessions window 2)
      (is (search "recording started" (or (announcement) "")))
      (is (search "obs-studio" (or (announcement) "")))
      (is (search "2 sessions" (or (announcement) ""))
          "and how many, when it is more than one")
      ;; The same count again is not news, and must not re-announce or re-fire:
      ;; river sends this event on every change to *any* of the window's state
      ;; batches, and a status line that flickers on each one is the failure.
      (setf p:*echo-message* nil)
      (r::note-capture-sessions window 2)
      (is (null (announcement)))
      (is (= 2 (length *capture-notices*)))
      ;; And stopping.
      (r::note-capture-sessions window 0)
      (is (search "recording stopped" (or (announcement) "")))
      (is (equal '(0 2 0) (mapcar #'cdr *capture-notices*))))))

(test a-window-born-during-a-screen-share-announces-itself
  ;; NIL -> 1 rather than 0 -> 1.  This is the case the three-valued count
  ;; exists for: without it the first event is indistinguishable from the
  ;; initial value and the window appears, already being recorded, in silence.
  (with-capture-watch
    (r::note-capture-sessions (win "signal-desktop") 1)
    (is (search "recording started" (or (announcement) "")))))

(test the-announcement-can-be-turned-off-without-losing-the-fact
  (with-capture-watch
    (let ((r::*announce-capture* nil)
          (window (win "firefox")))
      (r::note-capture-sessions window 1)
      (is (null (announcement)) "no line in the echo area")
      (is (c:window-captured-p window) "and the fact is recorded anyway")
      (is (equal '(1) (mapcar #'cdr *capture-notices*))
          "and the hook still runs, so a status bar is unaffected"))))

;;; --------------------------------------------------------- what is listed

(test a-listing-walks-every-window-the-model-holds
  ;; A recorded window goes on being recorded while it is minimized or on a
  ;; workspace you walked away from, which is exactly when you can no longer
  ;; see it — so a listing that walked only the tree would go quiet at the
  ;; moment it becomes the only way to know.
  (let* ((world (fresh-world))
         (pol (policy))
         (tiled (win "tiled"))
         (floated (win "floated"))
         (hidden (win "hidden"))
         (quiet (win "quiet"))
         (output (make-instance 'c:output :name "DP-1")))
    (setf (c:world-outputs world) (list output))
    (p:on-window-open pol world quiet)
    (p:on-window-open pol world tiled)
    (p:on-window-open pol world hidden)
    (p:on-minimize pol world hidden)
    (push (make-instance 'c:floating-window :window floated) (c:world-floats world))
    (is (null (c:world-captures world)) "nothing is being recorded")
    (dolist (window (list tiled floated hidden))
      (r::note-capture-sessions window 1))
    (r::note-capture-sessions quiet 0)
    (r::note-capture-sessions output 1)
    (let ((captures (c:world-captures world)))
      (is (= 4 (length captures)))
      (is (eq output (first captures)) "outputs first: a screen share is one")
      (is (member tiled captures))
      (is (member floated captures))
      (is (member hidden captures) "including the one you cannot see")
      (is (not (member quiet captures))))))

;;; ------------------------------------------------------- and on the screen

(test the-status-line-carries-a-standing-rec
  ;; The announcement scrolls away after a few seconds; the fact does not.
  (let* ((world (fresh-world))
         (pol (policy))
         (window (win "firefox"))
         (text (lambda ()
                 (format nil "~{~a ~}" (mapcar #'car (p:echo-content pol world))))))
    (p:on-window-open pol world window)
    (is (not (search "REC" (funcall text))))
    (r::note-capture-sessions window 1)
    (is (search "REC" (funcall text)))
    (let ((output (make-instance 'c:output :name "DP-1")))
      (setf (c:world-outputs world) (list output))
      (r::note-capture-sessions output 1)
      (is (search "REC 2" (funcall text))
          "two things being recorded, not two sessions on one"))
    (r::note-capture-sessions window 0)
    (is (search "REC" (funcall text)) "the screen is still being recorded")))
