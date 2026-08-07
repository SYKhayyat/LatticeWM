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

;;; ------------------------------------------------- where the step is taken
;;;
;;; UNDO USED TO BE BOLTED TO RUN-COMMAND AND THE THREE DOORS THIS PROJECT
;;; EXISTS FOR HAD NO UNDO AT ALL.
;;;
;;; UNDO-COMMAND-WRAPPER sat on P:*COMMAND-WRAPPERS*, and the argument for that
;;; was good as far as it went: one wrapper beats seventeen verbs each
;;; remembering to opt in.  But the wrapper's reach is exactly P:RUN-COMMAND,
;;; and none of the live-system doors go through it —
;;;
;;;   EVAL-EXPRESSION, bound to Super+;   (eval form), directly
;;;   EVALUATE-FOR-IPC, the control socket (eval form), directly
;;;   SWANK                                arbitrary
;;;
;;; — so `Super+; (setf (c:world-root *world*) (c:make-leaf))' destroyed your
;;; layout with no undo entry, while Super+h recorded a snapshot for a cursor
;;; move that changed nothing.  The mechanism that exists to make the tree
;;; recoverable was blind on precisely the surface the project is about.
;;;
;;; THE ALTITUDE WAS THE BUG AND THE COST OF IT SHOWED UP TWICE.
;;;
;;; First, *UNDO-EXEMPT-COMMANDS* was a nineteen-name literal deny-list — not
;;; exported, not an option, not printed by --list-options, not reachable from
;;; a configuration file — doing two unrelated jobs: undo/redo for correctness,
;;; and seventeen others for speed.  Forty lines away, policy/commands.lisp
;;; solves the identical problem correctly, with *NOT-REPEATABLE* backing a
;;; COMMAND-REPEATABLE-P generic whose docstring says outright that "a denylist
;;; of names cannot express `not repeatable under these circumstances'."
;;;
;;; Second, and this is why the deny-list had to exist: *the snapshot was taken
;;; before the test that decides whether to keep it*.  Every arrow-key press
;;; deep-copied every workspace — all forty, if you ever pressed `workspace 40'
;;; — hashed the result, ran a body that changed only WORLD-CURSOR, hashed it
;;; again, found them equal and threw both away.  Meanwhile tools/image.lisp
;;; shrinks BYTES-CONSED-BETWEEN-GCS to 8 MB specifically because "a GC pause
;;; during a keystroke is input latency, directly and visibly".  The one file
;;; that argues GC pressure matters was undermined by the one that generated it
;;; per keystroke, and the two never met.
;;;
;;; SO THE STEP IS TAKEN WHERE THE WORLD SETTLES, NOT WHERE A COMMAND RUNS.
;;;
;;; A *baseline* — one copy of the tree as of the last time anything settled —
;;; is kept on the world.  At a settle point the current signature is compared
;;; to the baseline's; if they differ, the baseline is what you get back, and a
;;; fresh baseline is taken.  If they match, nothing is copied at all.
;;;
;;; Three consequences, and all three are the point:
;;;
;;;   * every door is covered, because a settle point is "a human or a script
;;;     just did something arbitrary" rather than "RUN-COMMAND returned";
;;;   * the deny-list disappears, because the test is now what happened to the
;;;     tree rather than which function ran — which is what its own docstring
;;;     said it was aiming at;
;;;   * an arrow key costs one signature walk instead of two deep copies and
;;;     two signature walks, and a key that changes nothing costs no copy.
;;;
;;; The ruling this does *not* overturn is model/surgery.lisp:5 — the tree
;;; mutates, and a window manager is not a good place for persistent data
;;; structures.  Persistence would push copy-on-write into INSERT-CHILD,
;;; (SETF CHILD-AT) and REMOVE-CHILD, three members of an *open* protocol that
;;; third parties implement, and that obligation on extension authors is far
;;; heavier than one baseline.  It is also why the chokepoint is not (SETF
;;; WORLD-ROOT): RESIZE, EQUALIZE and TAB-NEXT change weights and selections in
;;; place and never replace the root, so a pointer-write hook would silently
;;; lose undo for three verbs.  The signature is the thing that knows.

(defvar *undo-label* "change"
  "What the next recorded step will be called.

Bound by the command wrapper to the command and its arguments, so that \"move
left\" and \"move right\" are different steps.  A plain binding and not a
snapshot, which is the whole of what the wrapper does now.")

(defun snapshot-layout (&optional (label "change") signature)
  "The current tree, copied, with the cursor and a signature.

SIGNATURE is accepted so a caller that has already computed one does not walk
the tree twice, which at a settle point is half the remaining cost."
  (%make-layout-snapshot
   :root (c:copy-node (c:world-root *world*))
   :cursor (copy-list (c:world-cursor *world*))
   :signature (or signature (c:node-signature (c:world-root *world*)))
   :label label
   :time (get-internal-real-time)))

(defun undo-baseline ()
  "The copy of the tree as of the last settle, or NIL before the first."
  (and *world* (c:prop *world* :undo-baseline)))

(defun (setf undo-baseline) (snapshot)
  (setf (c:prop *world* :undo-baseline) snapshot))

(defun reset-undo-baseline (&optional (label "change"))
  "Make the current tree the thing undo would consider unchanged.

Called after a restore and after loading a saved layout: both install a tree
deliberately, and neither is a change the user asked to be able to walk back
past.  Doing it here rather than with a dynamic `inhibit' flag means there is
no state to leave set if something signals in between."
  (when (and *world* (plusp *undo-depth*))
    (setf (undo-baseline) (snapshot-layout label))))

(defun push-undo (snapshot)
  "Put SNAPSHOT on the ring and abandon the redo branch."
  (let ((ring (undo-ring))
        (coalesce (and *undo-coalesce-seconds*
                       (* *undo-coalesce-seconds*
                          internal-time-units-per-second))))
    ;; Coalesce a run of the same verb: holding a resize key is one gesture and
    ;; should be one step back.  This compares the labels of two *recorded*
    ;; steps now; it used to compare them after both copies had been made,
    ;; which is why it could never save the copy it was written to save.
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
    ;; A fresh change abandons the redo branch, which is what every editor does
    ;; and what everybody expects.
    (setf (c:prop *world* :redo-ring) '())
    snapshot))

(defun note-layout-settled (&optional (label *undo-label*))
  "Record an undo step if the tree has changed since the last settle.

THE SETTLE POINTS ARE THE DOORS, and there are four: a key binding running to
completion, `Super+;', the control socket, and a thunk queued from another
thread — which is SWANK.  AFTER-COMMAND is the first three, because
EVAL-EXPRESSION and EVALUATE-FOR-IPC both already end with it.

Cheap when nothing happened, which is the common case: one NODE-SIGNATURE walk
and no copy.  It used to be two COPY-NODEs of every workspace and two walks,
per keystroke, whether or not anything changed."
  (when (and *world* (plusp *undo-depth*))
    (let ((baseline (undo-baseline)))
      (cond
        ;; First settle of the session: there is nothing to get back to yet.
        ((null baseline) (setf (undo-baseline) (snapshot-layout label)))
        (t
         (let ((signature (c:node-signature (c:world-root *world*))))
           (unless (equal signature (layout-snapshot-signature baseline))
             (setf (layout-snapshot-label baseline) label
                   (layout-snapshot-time baseline) (get-internal-real-time))
             (push-undo baseline)
             (setf (undo-baseline) (snapshot-layout label signature)))))))))

(defun undo-command-wrapper (command arguments thunk)
  "Name the step this command would produce, and run it.

Installed on P:*COMMAND-WRAPPERS*, so it covers every command in the system
including ones written from outside.

IT NO LONGER SNAPSHOTS, and that is the whole change.  It used to copy the
tree before the body ran and throw the copy away afterwards if nothing had
happened -- which is every arrow key, and which is why a nineteen-name
deny-list had to exist to keep the cost down.  NOTE-LAYOUT-SETTLED does the
recording, after the fact, where the answer is known."
  (let ((*undo-label* (undo-label command arguments)))
    (funcall thunk)))

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
  "Make SNAPSHOT the live layout, and return the one it replaced.

The returned snapshot is the *baseline* where there is one, rather than a fresh
copy: the baseline already is the tree as of the last settle, which is exactly
the state being replaced.  One copy saved on every undo, and it is the copy
that would have been made while the user was waiting.

RESET-UNDO-BASELINE at the end is what stops undo from recording itself.  The
old code needed UNDO and REDO in a deny-list for this; the baseline says the
same thing without a list, because after it the next settle finds nothing
changed."
  (let ((current (or (undo-baseline)
                     (snapshot-layout (layout-snapshot-label snapshot)))))
    (setf (layout-snapshot-label current) (layout-snapshot-label snapshot))
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
    ;; And the tree we just installed is the new baseline, so the settle that
    ;; follows this command finds nothing changed.  This is what UNDO and REDO
    ;; were in a deny-list for.
    (reset-undo-baseline (layout-snapshot-label snapshot))
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
