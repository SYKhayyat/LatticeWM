;;;; runtime/history.lisp --- Layout undo.
;;;;
;;;; NEARLY FREE, AND ITS ABSENCE WAS SURPRISING GIVEN THE DESIGN THAT ENABLES
;;;; IT.  Every surgery function in model/ already returns a new root and
;;;; COPY-NODE already makes a structural copy, so keeping a bounded ring of
;;;; previous roots is a dozen lines and one macro.
;;;;
;;;; It could not have been written before COPY-NODE became a generic, though,
;;;; and that is worth saying plainly: the old TYPECASE version silently
;;;; returned an empty container for any kind it did not know, so an undo ring
;;;; built on it would have quietly destroyed a lattice every time you pressed
;;;; the key — which is a much worse feature than not having the feature.
;;;;
;;;; WHY A COPY AND NOT A LOG OF INVERSES.  Inverse operations are smaller in
;;;; memory and much larger in every other way: every verb needs an inverse,
;;;; every inverse needs to be right, and a verb somebody adds from outside has
;;;; no inverse at all.  A tree of a few dozen nodes is a few kilobytes, and
;;;; the ring is bounded — so the whole feature costs less than one screenshot
;;;; and works for verbs that did not exist when it was written.
;;;;
;;;; WINDOWS ARE SHARED, NOT COPIED.  There is only ever one of those, so an
;;;; undone tree points at the same live windows.  A window that has *closed*
;;;; since the snapshot is dropped on the way back in, because putting a dead
;;;; window back in the tree would leave a pane nothing can ever fill.

(in-package #:latticewm/runtime)

(p:define-option *undo-depth* 32
  "How many previous layouts to keep.  0 turns undo off entirely.

Thirty-two is far more than anybody walks back through and costs a few hundred
kilobytes at the very most, because a snapshot is a tree of a few dozen nodes
and the windows in it are shared rather than copied.")

(p:define-option *undo-coalesce-seconds* 0.6
  "Merge repeated changes made within this long into one undo step.

Holding the resize key produces forty tree changes a second, and forty undo
steps to get back across one drag is not undo, it is a rewind button.  NIL
records every change separately.")

(defstruct (layout-snapshot (:constructor %make-layout-snapshot))
  "A tree, where the cursor was in it, and what produced it."
  (root nil)
  (cursor nil)
  (signature nil)
  (label "change")
  (time 0))

(defun undo-ring ()
  "The stack of previous layouts, most recent first."
  (and *world* (c:prop *world* :undo-ring)))

(defun redo-ring ()
  "The stack of layouts undone but not yet redone."
  (and *world* (c:prop *world* :redo-ring)))

(defun snapshot-layout (&optional (label "change"))
  "The current tree, copied, with the cursor and a signature."
  (%make-layout-snapshot
   :root (c:copy-node (c:world-root *world*))
   :cursor (copy-list (c:world-cursor *world*))
   :signature (c:node-signature (c:world-root *world*))
   :label label
   :time (get-internal-real-time)))

(defun record-undo (snapshot)
  "Put SNAPSHOT on the ring, unless nothing actually changed.

The signature test is what keeps the ring meaningful.  WITH-UNDO wraps whole
verbs, and plenty of verbs do nothing on a given press — MOVE at the edge of
the world, TAB with no sibling to fold, CLOSE on an already-empty pane — so
without it, pressing an inert key several times would fill the ring with
identical trees and undo would appear not to work."
  (when (and *world* snapshot (plusp *undo-depth*))
    (let ((now-signature (c:node-signature (c:world-root *world*))))
      (unless (equal now-signature (layout-snapshot-signature snapshot))
        (let ((ring (undo-ring))
              (coalesce (and *undo-coalesce-seconds*
                             (* *undo-coalesce-seconds*
                                internal-time-units-per-second))))
          ;; Coalesce a run of the same verb: holding a resize key is one
          ;; gesture and should be one step back.
          (if (and ring coalesce
                   (string= (layout-snapshot-label (first ring))
                            (layout-snapshot-label snapshot))
                   (< (- (layout-snapshot-time snapshot)
                         (layout-snapshot-time (first ring)))
                      coalesce))
              (setf (layout-snapshot-time (first ring))
                    (layout-snapshot-time snapshot))
              (setf (c:prop *world* :undo-ring)
                    (subseq (cons snapshot ring)
                            0 (min (1+ (length ring)) *undo-depth*))))
          ;; A fresh change abandons the redo branch, which is what every
          ;; editor does and what everybody expects.
          (setf (c:prop *world* :redo-ring) '()))))
    snapshot))

(defmacro with-undo ((label) &body body)
  "Run BODY, recording the layout it started from so UNDO can get back.

Nothing is recorded when BODY leaves the tree unchanged, so wrapping something
that sometimes does nothing is free and correct."
  (let ((snapshot (gensym "SNAPSHOT")))
    `(let ((,snapshot (when (and *world* (plusp *undo-depth*))
                        (guarded "snapshot" (snapshot-layout ,label)))))
       (multiple-value-prog1 (progn ,@body)
         (when ,snapshot (guarded "record-undo" (record-undo ,snapshot)))))))

(defparameter *undo-exempt-commands*
  '("undo" "redo" "undo-history" "help" "describe-key" "describe-command"
    "describe-option" "apropos-command" "welcome" "eval-expression"
    "run-command-by-name" "spawn" "terminal" "editor" "browser" "files"
    "quit" "restart-wm" "reload-config")
  "Commands that never take a snapshot, whatever they do to the tree.

Two kinds, for two different reasons.  UNDO and REDO are excluded because
snapshotting inside them would make walking back one step also push a step
forward, and the ring would never empty.  The rest are excluded as an
optimisation with a safety property: a command that opens a help screen or
starts a program does not change the layout, so a snapshot around it would be
copied and thrown away — and RELOAD-CONFIG can change *anything*, which is
precisely the moment you do not want a stale tree recoverable by one keystroke.

Everything else is covered, including commands a user writes, because the test
is what happened to the tree rather than which function ran.")

(defun undo-command-wrapper (command arguments thunk)
  "Snapshot the layout around COMMAND, and keep it only if the tree changed.

Installed on P:*COMMAND-WRAPPERS*, so it covers every command in the system
including ones written from outside — which is the whole reason the wrapper
list exists rather than seventeen verbs each remembering to opt in."
  (let ((name (p:command-name command)))
    (if (or (null *world*)
            (not (plusp *undo-depth*))
            (member name *undo-exempt-commands* :test #'string=))
        (funcall thunk)
        (with-undo ((undo-label command arguments))
          (funcall thunk)))))

(defun undo-label (command arguments)
  "What UNDO says out loud when it walks back past this command.

The command's name with its arguments after it, so \"move left\" and \"move
right\" are different steps in the history rather than two entries both reading
\"move\".  Keywords lose their colon, because :LEFT on a help screen reads as
punctuation somebody forgot to remove."
  (if (null arguments)
      (p:command-name command)
      (format nil "~a~{ ~(~a~)~}" (p:command-name command)
              (mapcar (lambda (argument)
                        (if (keywordp argument)
                            (symbol-name argument)
                            argument))
                      arguments))))

(p:add-command-wrapper 'undo-command-wrapper)

(defun prune-dead-windows (root)
  "Empty every leaf holding a window that has since closed.

An undone tree points at the same live windows — there is only ever one of
those — but a window that closed while it was on the ring is gone, and putting
a dead window back would leave a pane nothing can ever fill and nothing can
focus."
  (c:map-nodes (lambda (node)
                 (when (and (typep node 'c:leaf) (c:leaf-window node)
                            (not (c:window-live-p (c:leaf-window node))))
                   (setf (c:leaf-window node) nil)))
               root)
  root)

(defun restore-snapshot (snapshot)
  "Make SNAPSHOT the live layout, and return the one it replaced."
  (let ((current (snapshot-layout (layout-snapshot-label snapshot))))
    (setf (c:world-root *world*) (prune-dead-windows (layout-snapshot-root snapshot))
          (c:world-cursor *world*)
          (c:repair-path (c:world-root *world*) (layout-snapshot-cursor snapshot)))
    ;; Anything we are managing that this tree does not mention has to go
    ;; somewhere, or it would be invisible and unreachable — the same rule
    ;; LOAD-STATE follows, and for the same reason.
    (let ((restored (c:node-windows (c:world-root *world*))))
      (dolist (window (all-windows))
        (unless (or (member window restored)
                    (c:window-floating-p window)
                    (c:window-minimized-p window)
                    (not (c:window-live-p window)))
          (guarded "replace unlisted window"
            (p:on-window-open (p:current-policy) *world* window)))))
    (mark-dirty)
    (request-manage)
    current))

(defcommand undo ()
  "Put the layout back the way it was before the last thing that changed it.

Windows are not closed or opened by this — only their arrangement is restored,
because a window manager cannot un-quit an application and pretending otherwise
would be worse than not offering it.  A window that has closed since is simply
absent from the tree that comes back.

REDO takes you forward again, and anything you do after an undo abandons the
forward branch, which is what every editor does."
  (let ((ring (undo-ring)))
    (cond
      ((null ring) (notify "nothing to undo"))
      (t
       (let ((snapshot (first ring)))
         (setf (c:prop *world* :undo-ring) (rest ring))
         (push (restore-snapshot snapshot) (c:prop *world* :redo-ring))
         (notify "undo: ~a" (layout-snapshot-label snapshot))
         t)))))

(defcommand redo ()
  "Undo an undo."
  (let ((ring (redo-ring)))
    (cond
      ((null ring) (notify "nothing to redo"))
      (t
       (let ((snapshot (first ring)))
         (setf (c:prop *world* :redo-ring) (rest ring))
         (push (restore-snapshot snapshot) (c:prop *world* :undo-ring))
         (notify "redo: ~a" (layout-snapshot-label snapshot))
         t)))))

(defcommand undo-history ()
  "Show what UNDO would walk back through."
  (let ((ring (undo-ring)))
    (if (null ring)
        (notify "nothing to undo")
        (show-help-page
         (format nil "undo -- ~d step~:p" (length ring))
         (loop for snapshot in ring
               for index from 1
               collect (cons (format nil "~d" index)
                             (format nil "~a  (~d window~:p)"
                                     (layout-snapshot-label snapshot)
                                     (length (c:node-windows
                                              (layout-snapshot-root snapshot))))))))))
