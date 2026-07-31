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
empty desktop.")

(defun serialize-node (node)
  "NODE as a readable s-expression.

Windows become their river identifier, which is the only part of a window that
means anything across a restart.

*Every field is named.*  The first version of this wrote

    (:split :horizontal (1 1) (:leaf …) (:leaf …))

— weights positionally, children after — and read it back by asking which
elements were conses.  The weights *are* a cons, so they came back as a child,
and every restart grew a spurious empty pane at the front of every split.  The
tree was subtly wrong in a way that looked like a layout bug rather than a
parsing one.  Naming the fields costs eight characters and makes that class of
mistake unavailable."
  (typecase node
    (c:leaf
     (list :leaf :window (let ((window (c:leaf-window node)))
                           (and window (c:window-identifier window)))
                 :label (c:node-label node)))
    (c:split
     (list :split :axis (c:split-axis node)
                  :weights (copy-list (c:weights node))
                  :children (mapcar #'serialize-node (c:children node))))
    (c:stack
     (list :stack :selected (c:stack-selected node)
                  :children (mapcar #'serialize-node (c:children node))))
    (t
     ;; A container kind we do not know — an extension's.  Record what it was
     ;; and what was in it, so that a reload degrades to the contents rather
     ;; than losing them, and so a human reading the file can see what
     ;; happened.
     (list :unknown :type (string (type-of node))
                    :children (when (c:container-p node)
                                (loop for address in (c:container-addresses node)
                                      for child = (c:child-at node address)
                                      when child collect (serialize-node child)))))))

(defun deserialize-node (form index)
  "Rebuild a node from FORM, looking windows up in INDEX by identifier."
  (unless (consp form) (return-from deserialize-node (c:make-leaf)))
  (let ((plist (rest form)))
    (flet ((kids ()
             (mapcar (lambda (child) (deserialize-node child index))
                     (getf plist :children))))
      (case (first form)
        (:leaf
         (let* ((identifier (getf plist :window))
                (leaf (c:make-leaf (and identifier (gethash identifier index)))))
           (setf (c:node-label leaf) (getf plist :label))
           leaf))
        (:split
         (let ((children (kids)))
           (if children
               (c:make-split (or (getf plist :axis) :horizontal) children
                             (let ((weights (getf plist :weights)))
                               (when (= (length weights) (length children))
                                 weights)))
               (c:make-leaf))))
        (:stack
         (let ((children (kids)))
           (if children
               (c:make-stack children (or (getf plist :selected) 0))
               (c:make-stack (list (c:make-leaf)) 0))))
        ;; A container an extension owned and that is not loaded now.  Keep the
        ;; windows; lose only the arrangement.
        (:unknown
         (let ((children (kids)))
           (cond ((null children) (c:make-leaf))
                 ((null (rest children)) (first children))
                 (t (c:make-split :horizontal children)))))
        (t (c:make-leaf))))))

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
        (let ((root (deserialize-node (getf form :root) index)))
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
          (logmsg :info "restored layout from ~a" path)
          (mark-dirty)
          t)))))

(defun save-state-soon ()
  "Schedule a save.

Debounced by simply marking the world and letting the next idle moment in the
event loop do the write: a layout change can happen a hundred times a second
while a resize key is held, and a hundred file writes would be absurd."
  (setf (c:prop *world* :needs-save) t))

(defun save-state-if-needed ()
  "Write the state out if anything has changed since the last write."
  (when (c:prop *world* :needs-save)
    (setf (c:prop *world* :needs-save) nil)
    (save-state)))
