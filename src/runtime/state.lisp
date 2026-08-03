;;;; runtime/state.lisp --- Persistence, keyed on river's window identifiers.
;;;;
;;;; river_window_v1.identifier is up to 32 printable ASCII bytes, unique, and
;;;; *never reused*.  Sent once at window creation.  That is what lets a layout
;;;; survive the window manager being restarted while the session keeps
;;;; running: serialise identifier-to-location on the way out, match on the way
;;;; back in.
;;;;
;;;; NOTE THE LIMIT, because it decides how much this is worth.  Identifiers
;;;; are per-window and are not reused, so they survive a window manager
;;;; hot-swap — which is the case that matters, since that is the development
;;;; loop and the crash-recovery path — but *not* a reboot, because after a
;;;; reboot the windows are different windows.  Restoring a layout across a
;;;; reboot is a different feature (spawn rules) and is not this.

(in-package #:latticewm/runtime)

(defun state-file ()
  "Where the layout is saved: $XDG_STATE_HOME/latticewm/state.lisp."
  (merge-pathnames
   "latticewm/state.lisp"
   (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
       (merge-pathnames ".local/state/" (user-homedir-pathname)))))

(defvar *save-timer* nil)

(defparameter +state-version+ 2
  "The layout file format version.

Bumped when the shape changes.  LOAD-STATE refuses anything else rather than
guessing, because a half-understood layout file is worse than none: it puts
windows somewhere plausible and wrong, which is much harder to notice than an
empty desktop.

*Adding a key is not a shape change.*  The file is a plist read with GETF, so
a reader that does not know :OUTPUTS ignores it and a reader that does gets NIL
from a file written before it existed.  Bumping for that would throw away
everybody's layout once to protect against nothing.")

;;; ------------------------------------------------- serialization, as a protocol
;;;
;;; SERIALIZE-NODE WAS A TYPECASE AND THE LATTICE PAID FOR IT.  A GRID matched
;;; no clause, so it fell to the `unknown' branch: the plane, the viewport, the
;;; column widths and every name a user had given a cell were dropped on every
;;; restart, and what came back was a flat split of whatever windows had been
;;; in it.  The flagship extension could not survive the thing persistence
;;; exists for.
;;;
;;; So it is two generics.  A container kind writes itself and reads itself
;;; back, the core knows only that a node has a *tag* and a plist, and the
;;; `unknown' branch goes back to meaning what it says: a kind that is genuinely
;;; not loaded right now.

(defgeneric serialize-node (node)
  (:documentation
   "NODE as a readable s-expression: a tag keyword followed by a plist.

Windows become their river identifier, which is the only part of a window that
means anything across a restart.

*Every field is named.*  The first version of this wrote

    (:split :horizontal (1 1) (:leaf …) (:leaf …))

— weights positionally, children after — and read it back by asking which
elements were conses.  The weights *are* a cons, so they came back as a child,
and every restart grew a spurious empty pane at the front of every split.  The
tree was subtly wrong in a way that looked like a layout bug rather than a
parsing one.  Naming the fields costs eight characters and makes that class of
mistake unavailable.

A container kind adds one method here and one DESERIALIZE-NODE method, and its
users' layouts survive a restart.  Use a namespaced tag — :LATTICE/GRID — so
two extensions cannot collide."))

(defgeneric deserialize-node (tag plist index)
  (:documentation
   "Rebuild the node TAG names from PLIST, looking windows up in INDEX.

TAG is the keyword SERIALIZE-NODE wrote, and is dispatched on with an EQL
specializer, so adding a kind is adding a method rather than editing a CASE.
INDEX maps river window identifiers to live WINDOWs; an identifier that is not
in it belongs to a window that no longer exists and yields an empty pane.

The method on T is the one that matters for forward compatibility: a file
written by an image that had an extension loaded, read by one that does not,
keeps the windows and loses only the arrangement."))

(defun serialize-children (node)
  "Every child of NODE, serialized, in container order.

Through the container protocol, so a sparse coordinate-addressed kind is walked
correctly by a function that has never heard of coordinates."
  (when (c:container-p node)
    (loop for address in (c:container-addresses node)
          for child = (c:child-at node address)
          when child collect (serialize-node child))))

(defun deserialize-children (plist index)
  "The :CHILDREN of PLIST, rebuilt."
  (mapcar (lambda (child) (read-node child index)) (getf plist :children)))

(defun read-node (form index)
  "Rebuild a node from FORM.  The entry point; DESERIALIZE-NODE is the protocol.

A malformed form is an empty pane rather than an error: a state file is
untrusted input the moment somebody edits it by hand, and the whole point of
the file is to be readable enough to edit."
  (if (and (consp form) (keywordp (first form)))
      (or (guarded "deserialize"
            (deserialize-node (first form) (rest form) index))
          (c:make-leaf))
      (c:make-leaf)))

(defmethod serialize-node ((node c:leaf))
  (list :leaf :window (let ((window (c:leaf-window node)))
                        (and window (c:window-identifier window)))
              :label (c:node-label node)))

(defmethod deserialize-node ((tag (eql :leaf)) plist index)
  (let* ((identifier (getf plist :window))
         (leaf (c:make-leaf (and identifier (gethash identifier index)))))
    (setf (c:node-label leaf) (getf plist :label))
    leaf))

(defmethod serialize-node ((node c:split))
  (list :split :axis (c:split-axis node)
               :weights (copy-list (c:weights node))
               :label (c:node-label node)
               :children (serialize-children node)))

(defmethod deserialize-node ((tag (eql :split)) plist index)
  (let ((children (deserialize-children plist index)))
    (if children
        (let ((split (c:make-split (or (getf plist :axis) :horizontal) children
                                   (let ((weights (getf plist :weights)))
                                     (when (= (length weights) (length children))
                                       weights)))))
          (setf (c:node-label split) (getf plist :label))
          split)
        (c:make-leaf))))

(defmethod serialize-node ((node c:stack))
  (list :stack :selected (c:container-selection node)
               :label (c:node-label node)
               :children (serialize-children node)))

(defmethod deserialize-node ((tag (eql :stack)) plist index)
  (let* ((children (deserialize-children plist index))
         (stack (if children
                    (c:make-stack children (or (getf plist :selected) 0))
                    (c:make-stack (list (c:make-leaf)) 0))))
    (setf (c:node-label stack) (getf plist :label))
    stack))

(defmethod serialize-node ((node c:node))
  "A kind with no method of its own: record what it was and what was in it.

So a reload degrades to the contents rather than losing them, and so a human
reading the file can see what happened rather than finding windows missing."
  (list :unknown :type (string (type-of node))
                 :label (c:node-label node)
                 :children (serialize-children node)))

(defmethod deserialize-node ((tag t) plist index)
  "A kind this image does not have a method for.  Keep the windows.

This is the forward-compatibility path and it fires for two different reasons
that want the same answer: a tag from an extension that is not loaded right
now, and a tag from a newer version of one that is.  Both mean `somebody else
knows what this shape was'; neither means `throw the windows away'."
  (declare (ignore tag))
  (let ((children (deserialize-children plist index)))
    (cond ((null children) (c:make-leaf))
          ((null (rest children)) (first children))
          (t (c:make-split :horizontal children)))))

(defun output-workspaces ()
  "Which workspace each output is showing, by output name.

Saved separately from the tree because it is not part of the tree: the
workspace stack knows which of its children is selected, and the *output*
knows which one it is displaying, and on one monitor those look like the same
fact right up until they disagree."
  (loop for output in (c:world-outputs *world*)
        for name = (c:output-name output)
        for index = (c:prop output :workspace)
        when (and name index) collect (cons name index)))

(defun restore-output-workspaces (saved)
  "Point each output back at the workspace it was showing.

Then the safety net, which is the part that matters: *if the workspace the
cursor is on is not on a screen anywhere, put it on one.*

Without it, restarting while on workspace 3 of a single-monitor session
restored the cursor to workspace 3 and left the output showing workspace 1 —
which is a black screen, a status line confidently reporting [3/3], and every
key doing something invisible.  Nothing about it looks like a workspace
problem, which is what makes it worth a paragraph."
  (let ((outputs (c:world-outputs *world*))
        (stack (c:world-workspaces *world*)))
    (when (and stack outputs)
      (dolist (output outputs)
        (let ((index (cdr (assoc (c:output-name output) saved :test #'equal))))
          (when (and index (< -1 index (c:container-count stack)))
            (setf (c:prop output :workspace) index))))
      (let ((wanted (first (c:world-cursor *world*))))
        (when (and (integerp wanted)
                   (< -1 wanted (c:container-count stack))
                   (notany (lambda (output)
                             (eql wanted (c:prop output :workspace)))
                           outputs))
          (setf (c:prop (first outputs) :workspace) wanted
                (c:container-selection stack) wanted)
          (logmsg :debug "no output was showing workspace ~d; ~a is now"
                  (1+ wanted) (or (c:output-name (first outputs)) "the output")))))))

(defun save-state (&optional (path (state-file)))
  "Write the layout out.  Never signals; a failure to save is not fatal."
  (guarded "save-state"
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* nil) (*print-pretty* t) (*package* (find-package :keyword)))
        (format out ";;;; LatticeWM saved layout.  Written automatically.~%~
                     ;;;; Keyed on river window identifiers, which survive a~%~
                     ;;;; window manager restart but not a reboot.~%~%")
        (write (list :version +state-version+
                     :cursor (c:world-cursor *world*)
                     :outputs (output-workspaces)
                     :root (serialize-node (c:world-root *world*)))
               :stream out)
        (terpri out)))
    (logmsg :debug "saved layout to ~a" path)
    path))

(defun load-state (&optional (path (state-file)))
  "Restore a layout, matching windows by identifier.

Windows in the file that are not present now are dropped, and windows present
now that are not in the file keep whatever place they already have.  Both are
normal: the file describes the world as it was a moment ago, not as it must
be."
  (guarded "load-state"
    (unless (probe-file path) (return-from load-state nil))
    (let ((form (with-open-file (in path)
                  (let ((*package* (find-package :keyword)))
                    (read in nil nil)))))
      (unless (and (consp form) (eql +state-version+ (getf form :version)))
        (logmsg :warn "ignoring ~a: version ~s, expected ~d"
                path (and (consp form) (getf form :version)) +state-version+)
        (return-from load-state nil))
      (let ((index (make-hash-table :test #'equal)))
        (dolist (window (all-windows))
          (when (c:window-identifier window)
            (setf (gethash (c:window-identifier window) index) window)))
        (let ((root (read-node (getf form :root) index)))
          ;; Anything we are managing that the file did not mention has to go
          ;; somewhere, or it would be invisible and unreachable.
          (let ((restored (c:node-windows root)))
            (setf (c:world-root *world*) root
                  (c:world-cursor *world*)
                  (c:repair-path root (getf form :cursor)))
            (dolist (window (all-windows))
              (unless (or (member window restored)
                          (c:window-floating-p window)
                          (c:window-minimized-p window))
                (guarded "replace unlisted window"
                  (p:on-window-open (p:current-policy) *world* window)))))
          (restore-output-workspaces (getf form :outputs))
          ;; The restored tree replaced whatever the configuration file built,
          ;; so a policy that requires a shape gets its chance here.  See the
          ;; hook's own documentation for the failure this exists for.
          (run-hooks :layout-restored)
          (logmsg :info "restored layout from ~a" path)
          (mark-dirty)
          t)))))

(p:define-option *save-interval-seconds* 5
  "How long the layout may sit changed before it is written out.

Not zero, because a layout change happens a hundred times a second while a
resize key is held and a hundred file writes would be absurd.  Not large,
because everything between the last write and a crash is lost.  Five seconds is
the point where the writes are invisible and the loss is a shrug.

NIL saves only at a clean shutdown, which is what this program used to do —
SAVE-STATE-SOON existed, was documented, and was called from nowhere at all, so
any crash, any kill -9 and any compositor exit lost the session layout.")

(defvar *last-save* 0
  "Internal-real-time of the last write, for the interval.")

(defun save-state-soon ()
  "Note that the layout has changed and should be written before long.

Debounced: this marks the world and the event loop does the write at its next
idle moment, at most once per *SAVE-INTERVAL-SECONDS*.

MARK-DIRTY calls this, which is the whole fix.  A window manager that saves
only at a clean shutdown loses the layout on exactly the paths where you most
want it back — and given a debugger hook that used to exit rather than unwind,
the crash path was reachable and had no save on it at all."
  (when *world* (setf (c:prop *world* :needs-save) t))
  nil)

(defun save-state-if-needed (&key force)
  "Write the state out if it has changed and enough time has passed."
  (when (and *world* (c:prop *world* :needs-save))
    (let ((now (get-internal-real-time))
          (interval (and *save-interval-seconds*
                         (* *save-interval-seconds*
                            internal-time-units-per-second))))
      (when (or force
                (null interval)
                (>= (- now *last-save*) interval))
        (setf (c:prop *world* :needs-save) nil
              *last-save* now)
        (save-state)))))
