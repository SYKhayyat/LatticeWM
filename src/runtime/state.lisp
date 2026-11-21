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
;;;
;;; THEY ARE DECLARED IN model/node.lisp, with the other seventeen members of
;;; the container protocol, and only their methods are here.  Declaring them
;;; beside the file format put two obligations of an advertised protocol in a
;;; package that protocol's document could not see — and the two whose failure
;;; mode is silent data loss on restart, at that.  The file format is a runtime
;;; concern and stays here; the contract is not.

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
  ;; :EXTRA is written only when an extension has something to say, so a session
  ;; without extensions writes the same file it always did (#39).
  (let ((extra (c:node-extra-facts node)))
    (append (list :leaf :window (let ((window (c:leaf-window node)))
                                  (and window (c:window-identifier window)))
                  :label (c:node-label node))
            (when extra (list :extra extra)))))

(defmethod deserialize-node ((tag (eql :leaf)) plist index)
  (let* ((identifier (getf plist :window))
         (leaf (c:make-leaf (and identifier (gethash identifier index)))))
    (setf (c:node-label leaf) (getf plist :label))
    (c:restore-node-extra-facts leaf (getf plist :extra) index)
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

;;; ------------------------------------------------ what is not in the tree
;;;
;;; A WINDOW'S OWN STATE IS NOT THE TREE'S STATE, and putting it in the tree is
;;; how the feature that most needed it was the feature that lost it.
;;;
;;; runtime/tags.lisp opens by arguing that a dead slot in a *persisted* state
;;; file is worse than one in memory, because it becomes de-facto API.
;;; WINDOW-TAGS had never been in that file.  Neither had :SCRATCHPAD — and a
;;; named scratchpad is a *minimized* window, which is out of the tree by
;;; construction, so no amount of care in SERIALIZE-NODE could have reached it.
;;; Put a terminal away under the name `music', restart the window manager, and
;;; it came back as an anonymous window in the tree: the name gone, the putting
;;; away undone, and the one command for getting it back keyed on the name.
;;;
;;; So this is a second section of the file, on the same identifiers, for the
;;; facts that belong to a window rather than to a place.  Adding a key is not
;;; a shape change — see +STATE-VERSION+ — so an older reader ignores it and a
;;; newer reader gets NIL from a file written before it existed.

(defun window-facts (&optional (windows (all-windows)))
  "Per-window state that is not in the tree, keyed by river identifier.

Only windows that have some.  The common case is none, and a file full of empty
rows is a file nobody reads.

WINDOWS is a parameter, defaulted, rather than a call to ALL-WINDOWS in the
body: everything else in this file can be unit-tested and this could not have
been, which for a persistence path is the wrong way round."
  (loop for window in windows
        for identifier = (c:window-identifier window)
        for tags = (c:window-tags window)
        for scratchpad = (c:prop window :scratchpad)
        for minimized = (c:window-minimized-p window)
        for floating = (c:window-floating-p window)
        for fullscreen = (c:window-fullscreen-p window)
        for pinned = (c:window-pinned-p window)
        for float-rect = (let ((float (c:float-of-window *world* window)))
                           (and float (c:copy-rect (c:float-rect float))))
        when (and identifier (or tags scratchpad minimized floating fullscreen pinned))
          collect (list identifier
                        :tags (copy-list tags)
                        :scratchpad scratchpad
                        :minimized minimized
                        ;; #25: the disposition is state too, and it was not
                        ;; being written.  A window left floating at shutdown
                        ;; came back as a tile, in the tree, at a place it had
                        ;; never been -- the file described the tree faithfully
                        ;; and the tree no longer mentioned it.
                        :floating floating
                        :fullscreen fullscreen
                        :float-rect float-rect
                        :pinned pinned)))

(defun restore-a-float (world window rect)
  "Put WINDOW back on the float list at RECT, or at the default float size.

Called from RESTORE-WINDOW-FACTS, which is why it does not do the tree surgery
FLOAT-AWAY-FROM-TREE does: a window that was floating when the layout was saved
was never in the restored tree in the first place, so there is nothing to take
it out of.  RECT is what it looked like; a file written before #25 has none, and
the default float rectangle is the honest answer then -- the alternative is a
float at 0,0 the size of nothing."
  (let ((rect (or rect (c:make-default-float-rect 100 100))))
    (setf (c:window-floating-p window) t)
    (unless (c:float-of-window world window)
      (push (make-instance 'c:floating-window :window window :rect rect)
            (c:world-floats world)))
    window))

(defun restore-window-facts (world saved index)
  "Put SAVED per-window state back onto the live windows in INDEX.

Tags are unioned rather than assigned: the window is already open, so a window
rule may have tagged it since it did, and the file describes the world as it was
a moment ago rather than as it must be.

Minimizing goes through ON-MINIMIZE like every other minimize, so a policy that
keeps its own scratchpad sees it happen.  It runs *before* LOAD-STATE re-places
the windows the restored tree did not mention, which is what keeps a window that
was deliberately put away from reappearing on screen.

A row that is not a list, or names an identifier no live window has, is skipped
rather than signalled: a state file is untrusted input the moment somebody edits
it by hand, which is the same ruling READ-NODE makes three functions up."
  (dolist (row saved)
    (when (consp row)
      ;; PER ROW, not around the whole loop: the DESTRUCTURING-BIND signals on
      ;; an odd-length plist tail or a key it does not expect -- both reachable
      ;; the moment somebody hand-edits the file -- and a single bad row caught
      ;; only by the outer LOAD-STATE guard abandoned the *entire* restore, not
      ;; just that row.  The docstring promises the row is skipped; this keeps
      ;; that promise for a malformed list, not only for a non-list.
      (guarded "restore-window-fact"
        (destructuring-bind (identifier &key tags scratchpad minimized
                                          floating fullscreen float-rect pinned) row
          (let ((window (gethash identifier index)))
            (when window
              ;; Reversed, so that a window with no tags yet comes back with
              ;; them in the order they were given.  WINDOW-TAG-NAMES documents
              ;; that order and the tag prompt lists them in it.
              ;;
              ;; NORMALIZED, because the file is read as text: a tag typed at a
              ;; prompt is stored as an uninterned symbol and prints as
              ;; `#:MAIL', which the reader makes a *new* uninterned symbol each
              ;; time it appears.  The tag in :TAGS and the same name in
              ;; :SCRATCHPAD would otherwise be two symbols that only look
              ;; alike, and SCRATCHPAD-SHOW would find nothing.
              (dolist (tag (reverse tags))
                (pushnew (normalize-tag tag) (c:window-tags window)
                         :test #'tag-name=))
              (when scratchpad
                (setf (c:prop window :scratchpad) (normalize-tag scratchpad)))
              ;; Disposition before minimization, so that a window put away
              ;; floating comes back with both facts in the order ON-MINIMIZE
              ;; would have seen them.
              (when (and floating (not (c:window-minimized-p window)))
                (guarded "restore float"
                  (restore-a-float world window float-rect)))
              (when fullscreen
                (setf (c:window-fullscreen-p window) t))
              (when pinned
                (setf (c:window-pinned-p window) t))
              (when (and minimized (not (c:window-minimized-p window)))
                (guarded "restore minimized"
                  (p:on-minimize (p:current-policy) world window)))))))))) 

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

;;; ------------------------------------------------------------ writing it out
;;;
;;; A SAVE THAT FAILS MUST NOT DESTROY THE SAVE BEFORE IT.  This wrote the file
;;; with :IF-EXISTS :SUPERSEDE, which truncates it before the first byte: a full
;;; disk, a signal, a compositor that exits halfway through FORMAT, and the
;;; layout that was on screen a moment ago is not merely unsaved but *gone*,
;;; replaced by a fragment LOAD-STATE refuses.  The failure mode of the one
;;; thing whose only job is surviving a crash was to lose the data during the
;;; crash.
;;;
;;; So the write goes to a sibling temporary and is renamed into place.  RENAME
;;; is atomic on a POSIX filesystem and both names are in the same directory, so
;;; the state file a reader can see is always either the old complete one or the
;;; new complete one.  The temporary is finished and FSYNCed before the rename,
;;; because ordering is not durability: without the sync the rename can reach
;;; the disk before the bytes it points at, and a power cut leaves the *name* of
;;; a file whose contents are a hole.

(defun temporary-state-path (path)
  "PATH with a temporary suffix, in the same directory.

Same directory because RENAME is only atomic within a filesystem.  The pid is in
the name so two window managers -- which a --socket argument makes possible --
do not write through each other's temporary."
  (make-pathname :type (format nil "tmp.~d" (sb-posix:getpid))
                 :defaults path))

(defun sync-file (stream)
  "Push STREAM's bytes to the disk, not merely to the kernel."
  (finish-output stream)
  (sb-posix:fsync (sb-sys:fd-stream-fd stream)))

(defun sync-directory (path)
  "FSYNC the directory holding PATH, so the rename itself survives a crash.

Best effort, and deliberately not an error: the rename has already happened and
the data is already there, so what is left to lose is only the *name* across a
power cut.  Filesystems that refuse to open a directory for fsync exist, and
turning their refusal into a failed save would be the original bug with a better
reason."
  (let ((directory (directory-namestring path)))
    (handler-case
        (let ((descriptor
                (sb-posix:open (if (string= directory "") "." directory)
                               (logior sb-posix:o-rdonly
                                       sb-posix:o-directory))))
          (unwind-protect (sb-posix:fsync descriptor)
            (sb-posix:close descriptor)))
      (error (condition)
        (logmsg :debug "could not sync the directory of ~a: ~a" path condition)
        nil))))

(defun call-with-replacement-file (path write)
  "Write PATH through a temporary, replacing PATH only when WRITE succeeds.

WRITE is called with the stream.  If it signals, or the rename fails, PATH is
untouched and the temporary is removed; the condition propagates, and the caller
that must not fail -- SAVE-STATE, under GUARDED -- turns it into a log line.

A parameter rather than a macro because the thing worth testing is exactly the
window in the middle: what PATH contains while WRITE is running."
  (let ((temporary (temporary-state-path path))
        (replaced nil))
    (unwind-protect
         (progn
           (with-open-file (out temporary
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
             (funcall write out)
             (sync-file out))
           (rename-file temporary path)
           (setf replaced t)
           (sync-directory path))
      (unless replaced
        (ignore-errors (delete-file temporary))))))

(defun save-state (&optional (path (state-file)))
  "Write the layout out.  Never signals; a failure to save is not fatal.

OUR OWN CODE, GUARDED DELIBERATELY.  This runs from the event loop's idle
moment and from EMERGENCY-SHUTDOWN, which is the path where the program has
already lost; a full disk here must produce a log line and a session that
keeps running rather than an unhandled condition on the way out the door.
The docstring's first sentence is the contract every caller relies on.

THE FILE IS REPLACED, NOT OVERWRITTEN.  See the section comment above: a failed
write now costs the change it was carrying, not the layout that was on screen."
  (guarded "save-state"
    (ensure-directories-exist path)
    (call-with-replacement-file
     path
     (lambda (out)
       (let ((*print-readably* nil) (*print-pretty* t)
             (*package* (find-package :keyword)))
         (format out ";;;; LatticeWM saved layout.  Written automatically.~%~
                      ;;;; Keyed on river window identifiers, which survive a~%~
                      ;;;; window manager restart but not a reboot.~%~%")
         (write (list :version +state-version+
                      :cursor (c:world-cursor *world*)
                      :outputs (output-workspaces)
                      :windows (window-facts)
                      :root (serialize-node (c:world-root *world*)))
                :stream out)
         (terpri out))))
    (logmsg :debug "saved layout to ~a" path)
    path))

(defun keep-unreadable-state (path version)
  "Rename PATH out of the way before the running program overwrites it.
Returns where it went, or NIL if it could not be moved.

THE NAME SAYS WHICH KIND OF UNREADABLE IT WAS.  A version this build does not
know is kept as `.lisp.v<n>', because what you want back is \"the one from the
newer build\"; a file the reader could not make a form out of at all is kept as
`.lisp.corrupt', because there is no version to name it by.  Both are kept for
the same reason, which is the paragraph below.

THE VERSION GATE DESTROYED THE FILE IT REFUSED.  LOAD-STATE saw a version it
did not recognise, logged, and returned NIL — correctly, because a
half-understood layout is worse than none.  Then MAIN called MARK-DIRTY,
*LAST-SAVE* was still 0, and the first pass of the event loop wrote over it
with :IF-EXISTS :SUPERSEDE.

Rolling *forward* you lose the layout once and the docstring on +STATE-VERSION+
anticipates it.  Rolling *back* — test a build, hit a bug, reinstall the
packaged version — the newer file is gone before you can go back to the build
that wrote it.  And this is a format the documentation invites people to hand
edit, so silently truncating it on version skew punishes its own affordance.

The name carries the version the file claimed rather than a timestamp, because
what you want back is \"the one from the newer build\" and not \"the one from
Tuesday\".  An existing backup of the same version is left alone: the second
downgrade would otherwise overwrite what the first one saved, which is this
bug again one layer out."
  (let ((backup (make-pathname
                 :type (if (integerp version)
                           (format nil "lisp.v~d" version)
                           "lisp.corrupt")
                 :defaults path)))
    (handler-case
        (cond ((probe-file backup) backup)
              (t (rename-file path backup) backup))
      (error (condition)
        (logmsg :warn "could not keep a copy of ~a: ~a" path condition)
        nil))))

(defun read-state-form (path)
  "The form in PATH, or :UNREADABLE if there is not one.

*READ-EVAL* IS NIL, as in EVALUATE-FOR-IPC and for the same reason: this is a
file the documentation invites people to hand-edit, so `#.' in it is a way to
run code, not a way to write a layout.  A file that uses one is kept as corrupt
rather than executed.

One value and no condition, because the caller has nothing useful to do with a
reader's error: the caller that matters is MAIN at startup, with no terminal to
show it on, and its next step is the same for every kind of unreadable."
  (handler-case
      (with-open-file (in path :direction :input)
        (let ((*package* (find-package :keyword))
              (*read-eval* nil))
          (read in nil nil)))
    (error () :unreadable)))

(defun load-state (&optional (path (state-file)))
  "Restore a layout, matching windows by identifier.

Windows in the file that are not present now are dropped, and windows present
now that are not in the file keep whatever place they already have.  Both are
normal: the file describes the world as it was a moment ago, not as it must
be.

A FILE THAT CANNOT BE READ IS KEPT, NOT OVERWRITTEN.  The version gate already
knew this (`keep-unreadable-state' has the whole argument); a file the *reader*
rejected -- a truncated write, a hand edit half finished, a disk error -- went
through the same NIL answer and stayed where the first debounced save then
replaced it.  The one copy of a layout that a crashed write left behind was
deleted by the program that failed to read it."
  (guarded "load-state"
    (unless (probe-file path) (return-from load-state nil))
    (let ((form (read-state-form path)))
      (unless (and (consp form) (eql +state-version+ (getf form :version)))
        (let ((kept (keep-unreadable-state path (and (consp form)
                                                     (getf form :version)))))
          (if (eq form :unreadable)
              (logmsg :warn "ignoring ~a: it could not be read~@[ (kept a copy at ~a)~]"
                      path kept)
              (logmsg :warn "ignoring ~a: version ~s, expected ~d~@[ (kept a copy at ~a)~]"
                      path (and (consp form) (getf form :version)) +state-version+
                      kept)))
        (return-from load-state nil))
      (let ((index (make-hash-table :test #'equal)))
        (dolist (window (all-windows))
          (when (c:window-identifier window)
            (setf (gethash (c:window-identifier window) index) window)))
        ;; Before the tree, because it decides which windows the tree is
        ;; allowed to re-place: a window that was on a named scratchpad is
        ;; minimized again here, and the loop below skips minimized windows.
        (restore-window-facts *world* (getf form :windows) index)
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
          ;; The restored tree is where undo starts from, not something to
          ;; walk back past: the alternative is that the first Super+z after
          ;; login throws away the layout you just got back.
          (reset-undo-baseline "restored layout")
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
