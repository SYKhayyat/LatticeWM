;;;; scrolling-columns/strip.lisp --- The strip, and its state on disk.
;;;;
;;;; The example's container, drawing and navigation are here unchanged.  What
;;;; the module adds is the half of the container protocol the example was
;;;; silent about because a teaching file can assume a session that never
;;;; ends: COPY-NODE-SLOTS, NODE-SIGNATURE, SERIALIZE-NODE and
;;;; DESERIALIZE-NODE for the strip's own state.  EXTENDING.org's table lists
;;;; what skipping them costs -- undo that skips past scroll positions, an
;;;; arrangement lost at restart -- and a shipped module does not get to ship
;;;; with those failures built in.

(in-package #:scrolling-columns)

;;; ------------------------------------------------- the container

(defclass strip (c:sequential-container)
  ((offset :initform 0 :accessor strip-offset
           :documentation "Index of the leftmost column on screen.")
   (visible :initarg :visible :initform 2 :accessor strip-visible
            :documentation "How many columns fit on screen at once."))
  (:documentation
   "An unbounded horizontal strip of columns, scrolled by the viewport."))

(defmethod c:default-address ((strip strip))
  "Focus repair lands on the leftmost visible column, never offscreen."
  (min (strip-offset strip) (max 0 (1- (c:container-count strip)))))

(defmethod c:simplify-node ((strip strip))
  "A strip survives as a singleton; an empty one regains a column.

An empty strip is not a simpler state, it is a broken one."
  (when (zerop (c:container-count strip))
    (setf (c:children strip) (list (c:make-leaf))))
  strip)

;;; ------------------------------------------------- how it is drawn

(defmethod p:layout-children ((policy p:conventional-policy) (strip strip) rect)
  "Draw the visible window of columns, side by side, equally wide.

Everything outside the window is omitted, so the layout driver hides it --
which is not optional, since river shows a window unless told otherwise."
  (let* ((n (c:container-count strip))
         (visible (max 1 (strip-visible strip)))
         (start (max 0 (min (strip-offset strip) (max 0 (- n visible)))))
         (end (min n (+ start visible)))
         (addresses (loop for i from start below end collect i)))
    (when addresses
      (mapcar #'cons addresses
              (c:divide-rect rect :horizontal
                             (make-list (length addresses) :initial-element 1)
                             :gap (p:gaps policy strip))))))

;;; ------------------------------------------------- how it is navigated

(defmethod p:step-address ((policy p:conventional-policy) (strip strip)
                           address direction)
  "Left and Right move along the strip, scrolling it to follow.

Moving past the right edge shifts the window of visible columns by one rather
than refusing, which is what makes the strip feel unbounded instead of merely
wide."
  (when (c:direction-horizontal-p direction)
    (let ((next (+ address (c:direction-sign direction)))
          (n (c:container-count strip)))
      (when (and (<= 0 next) (< next n))
        ;; Scroll minimally to bring the target on screen.
        (let ((visible (max 1 (strip-visible strip))))
          (cond ((< next (strip-offset strip))
                 (setf (strip-offset strip) next))
                ((>= next (+ (strip-offset strip) visible))
                 (setf (strip-offset strip) (- next visible -1)))))
        next))))

(defmethod p:entry-address ((policy p:conventional-policy) (strip strip)
                            direction reference rects)
  (declare (ignore reference rects))
  (let ((n (c:container-count strip)))
    (when (plusp n)
      (if (eq direction :left) (1- n) (strip-offset strip)))))

;;; --------------------------------------------------- undo and snapshots

(defmethod c:copy-node-slots progn ((new strip) (old strip))
  "The scroll position and the visible count are part of the arrangement.

Without this, undo restores the columns but not which of them you were
looking at -- an operation the user performs constantly recorded as no change
at all."
  (setf (strip-offset new) (strip-offset old)
        (strip-visible new) (strip-visible old)))

(defmethod c:node-signature ((strip strip))
  "The children, plus the scroll state -- so scrolling records a snapshot and
undo can bring it back."
  (list* 'strip
         (strip-offset strip)
         (strip-visible strip)
         (call-next-method)))

;;; -------------------------------------------------------- persistence

(defmethod r:serialize-node ((strip strip))
  "The strip as a readable form, under a namespaced tag."
  (list :scrolling-columns/strip
        :label (c:node-label strip)
        :offset (strip-offset strip)
        :visible (strip-visible strip)
        :columns (let ((out '()))
                   (dotimes (i (c:container-count strip) (nreverse out))
                     (let ((child (c:child-at strip i)))
                       (when child
                         (push (r:serialize-node child) out)))))))

(defmethod r:deserialize-node ((tag (eql :scrolling-columns/strip)) plist index)
  "Rebuild the strip column by column.  Every field degrades gracefully: a
hand-edited or older file produces a strip with fewer facts rather than an
error, and SIMPLIFY-NODE guarantees validity at the end."
  (let ((strip (make-instance 'strip
                              :visible (max 1 (or (getf plist :visible) 2)))))
    (setf (c:node-label strip) (getf plist :label)
          (strip-offset strip) (max 0 (or (getf plist :offset) 0)))
    (loop for form in (getf plist :columns)
          do (let ((child (ignore-errors (r:read-node form index))))
               (when child
                 (c:insert-child strip (c:container-count strip) child))))
    (c:simplify-node strip)))

;;; -------------------------------------------------------------- the kind

;;; Registered so M-x restyle strip works on any container, not only on a
;;; workspace: a strip inside one half of a split is the same node as a strip
;;; that fills the screen.
(r:define-container-kind
 :strip
 (lambda (children &key argument)
   (make-instance 'strip
                  :visible (max 1 (or (and (integerp argument) argument) 2))
                  :children (copy-list children)))
 :description "columns on a scrollable strip")

;;; ------------------------------------------------------------- verbs

(r:defcommand strip-width (columns)
  "Show COLUMNS columns at a time.  This is the strip's version of zoom."
  (let ((strip (find-if (lambda (node) (typep node 'strip))
                        (c:resolve-chain (c:world-root r:*world*)
                                         (r:current-path)))))
    (when strip
      (setf (strip-visible strip) (max 1 columns))
      (r:relayout))))

(r:defcommand scrolling (&optional (visible 2))
  "Turn the container the cursor is in into a scrolling strip of columns.

Every window that was in it becomes a column, in the order it was laid out --
so nothing is lost and nothing is reordered.

THE CONTAINER THE CURSOR IS IN, through the same RESTYLE seam every other
paradigm uses (#134): the workspace when the cursor is in the workspace, and one
half of a split when that is where you are, so a strip and a grid can sit beside
each other in one workspace."
  (let* ((world r:*world*)
         (container (and world (r:cursor-container (r:current-path)))))
    (cond
      ((null container) (r:notify "nothing here to make a strip of"))
      (t
       (let ((made (r:restyle-container container :strip :argument visible)))
         (if (null made)
             (r:notify "the strip could not be made here")
             (progn
               (r:relayout :force t)
               (r:notify "strip: ~d column~:p, ~d on screen"
                         (c:container-count made)
                         (strip-visible made)))))))))
