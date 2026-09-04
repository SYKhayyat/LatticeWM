;;;; lattice/undo.lisp --- The plane's own undo ring, and UNDO-PLANE.
;;;;
;;;; THE TREE AND THE PLANE ARE TWO KINDS OF STATE, AND THEY USED TO SHARE ONE
;;;; UNDO.  Layout undo snapshots the whole tree, and a grid's copy carries the
;;;; viewport, the track sizes and the names with it — so pressing undo after a
;;;; tree change silently reverted whatever the camera had done since the
;;;; snapshot was taken, and there was no way to undo just the camera.  The
;;;; issue that scoped this (number 26) called it in one line: "undo after a
;;;; pan jumps to a previous tree, silently also reverting the viewport and
;;;; tracks.  There is no 'undo just the zoom/pan' granularity."
;;;;
;;;; TWO RINGS, ONE MECHANISM EACH.  The core keeps its tree ring, and sees
;;;; only the tree now: the grid's NODE-SIGNATURE no longer includes the plane
;;;; state, so a zoom is not a tree step.  This file keeps a second, bounded
;;;; ring of *plane states* — what COLLECT-PLANES gathers and APPLY-PLANES puts
;;;; back — walked by UNDO-PLANE and REDO-PLANE.
;;;;
;;;; HOW THEY STAY INDEPENDENT, IN BOTH DIRECTIONS:
;;;;
;;;;   * a plane verb records only here.  The tree ring never sees it, so UNDO
;;;;     after a pure zoom has nothing to do and does not surprise you;
;;;;   * a tree undo restores only the tree.  When UNDO replaces the roots, this
;;;;     wrapper puts the camera back where the user left it (the state captured
;;;;     before the command ran) and re-baselines the plane ring, so undoing a
;;;;     move does not drag the zoom along with it.
;;;;
;;;; The wrapper is on P:*COMMAND-WRAPPERS*, the same extension seam the core's
;;;; own undo uses, and it covers every command in the system — the plane verbs
;;;; are all commands.  A plane state changed by eval'ing a raw form through a
;;;; REPL or the control socket is not covered, exactly as the core's tree ring
;;;; used not to be before AFTER-COMMAND existed; the door is documented rather
;;;; than silently half-covered.

(in-package #:lattice)

;;; ------------------------------------------------------------- the ring

(defun plane-ring ()
  "The stack of previous plane states, most recent first."
  (and r:*world* (c:prop r:*world* :plane-undo-ring)))

(defun (setf plane-ring) (ring)
  (setf (c:prop r:*world* :plane-undo-ring) ring))

(defun plane-redo-ring ()
  "The stack of plane states undone but not yet redone."
  (and r:*world* (c:prop r:*world* :plane-redo-ring)))

(defun (setf plane-redo-ring) (ring)
  (setf (c:prop r:*world* :plane-redo-ring) ring))

(defun plane-baseline ()
  "The plane state as of the last settle, or NIL before the first."
  (and r:*world* (c:prop r:*world* :plane-baseline)))

(defun (setf plane-baseline) (state)
  (setf (c:prop r:*world* :plane-baseline) state))

(defun push-plane-step (state label)
  "Put STATE on the plane ring, coalescing a run of the same label.

The coalescing rule is the core's: holding the zoom key produces forty plane
changes a second, and forty undo steps across one gesture is not undo.  A run
of the same command within *UNDO-COALESCE-SECONDS* merges into the step it is
part of.  A fresh change abandons the redo branch, which is what every editor
does."
  (when (plusp r:*undo-depth*)
    (let ((ring (plane-ring))
          (now (get-internal-real-time))
          (coalesce (and r:*undo-coalesce-seconds*
                         (* r:*undo-coalesce-seconds*
                            internal-time-units-per-second))))
      (if (and ring coalesce
               (string= (second (first ring)) label)
               (< (- now (third (first ring))) coalesce))
          (setf (third (first ring)) now)
          (setf (plane-ring)
                (subseq (cons (list state label now) ring)
                        0 (min (1+ (length ring)) r:*undo-depth*))))
      (setf (plane-redo-ring) '()))))

(defun note-plane-settled (command arguments before)
  "Record a plane step if the plane changed since the last settle.

Called from PLANE-UNDO-WRAPPER after every command, with the state captured
before the command ran.  Three cases, mirroring the three things a command can
do to the plane:

  * UNDO and REDO replace the roots.  The plane is not part of the tree, so
    the camera that was there before the command is put back on the restored
    tree and becomes the new baseline — undoing a move must not also undo the
    zoom that preceded it.
  * UNDO-PLANE and REDO-PLANE restore the plane themselves.  The ring is the
    bookkeeping; the baseline just follows the state the restore brought.
  * anything else changed the plane as a side effect of a command.  If it
    really changed, the previous baseline is a step worth walking back to."
  (let ((after (collect-planes (c:world-root r:*world*)))
        (name (and command (p:command-name command))))
    (cond
      ((member name '("undo" "redo") :test #'string=)
       (apply-planes (c:world-root r:*world*) before)
       (setf (plane-baseline) (collect-planes (c:world-root r:*world*))))
      ((member name '("undo-plane" "redo-plane") :test #'string=)
       (setf (plane-baseline) after))
      ((null (plane-baseline))
       (setf (plane-baseline) after))
      ((not (equal (plane-baseline) after))
       (push-plane-step (plane-baseline) (r::undo-label command arguments))
       (setf (plane-baseline) after)))))

(defun plane-undo-wrapper (command arguments thunk)
  "On P:*COMMAND-WRAPPERS*: bracket every command with the plane state.

The core undo wrapper runs the same way and for the same reason — the state
that matters is the state before the command ran.  Capturing the whole plane
is the per-keystroke cost of a walk that a snapshot would pay once; it is the
same walk NODE-SIGNATURE makes for the tree ring, over a plane with a handful
of grids rather than a tree with everything in it."
  (let ((before (and r:*world* (c:world-root r:*world*)
                     (collect-planes (c:world-root r:*world*)))))
    (multiple-value-prog1 (funcall thunk)
      (when (and r:*world* before (plusp r:*undo-depth*))
        (note-plane-settled command arguments before)))))

(p:add-command-wrapper 'plane-undo-wrapper)

;;; -------------------------------------------------------------- the verbs

(r:defcommand undo-plane ()
  "Put the plane back the way it was, and nothing else.

The plane is what zoom, pan, resize-column, resize-row, resize-cell and
name-cell change; the tree is what moving, splitting and closing change.  UNDO
walks the tree ring, and this walks the plane's own ring — because a tree
snapshot used to carry the camera with it, undo after a move silently reverted
the zoom that preceded it, and the zoom could not be undone on its own.  Two
rings, and undoing one never drags the other along."
  (let ((ring (plane-ring)))
    (cond
      ((null ring) (r:notify "nothing to undo on the plane"))
      (t
       (let* ((entry (first ring))
              (state (first entry))
              (label (second entry)))
         (setf (plane-ring) (rest ring))
         (apply-planes (c:world-root r:*world*) state)
         (let ((baseline (plane-baseline)))
           (when baseline
             (push (list baseline label (get-internal-real-time))
                   (plane-redo-ring))))
         (r:notify "undo-plane: ~a" label)
         t)))))

(r:defcommand redo-plane ()
  "Undo an undo-plane."
  (let ((ring (plane-redo-ring)))
    (cond
      ((null ring) (r:notify "nothing to redo on the plane"))
      (t
       (let* ((entry (first ring))
              (state (first entry))
              (label (second entry)))
         (setf (plane-redo-ring) (rest ring))
         (apply-planes (c:world-root r:*world*) state)
         (let ((baseline (plane-baseline)))
           (when baseline
             (push (list baseline label (get-internal-real-time))
                   (plane-ring))))
         (r:notify "redo-plane: ~a" label)
         t)))))

(r:defcommand plane-undo-history ()
  "Show what UNDO-PLANE would walk back through."
  (let ((ring (plane-ring)))
    (if (null ring)
        (r:notify "nothing to undo on the plane")
        (r:show-help-page
         (format nil "undo-plane -- ~d step~:p" (length ring))
         (loop for entry in ring
               for index from 1
               collect (cons (format nil "~d" index)
                             (format nil "~a" (second entry))))))))