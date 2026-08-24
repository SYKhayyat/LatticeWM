;;;; kbd-overview/kbd-overview.lisp --- The keyboard IS the overview.

(in-package #:kbd-overview)

;;; ================================================================ state

(defvar *active* nil "True while the overview is on screen.")

(defvar *letter-layout* "asdfghjklqwertyuiopzxcvbnm"
  "Letters in assignment order: home row first, then upper, then lower.

The order IS the policy -- windows are dealt letters in workspace-and-
position order, and the strongest fingers get the first deals.  Change the
string to change the priority; it must contain only letters.")

(defvar *windows-function* nil
  "The windows the overview shows, or NIL to ask the runtime.

A function of no arguments.  Bound by tests, which have no compositor to
ask; the honest default is every window the session manages.")

(defvar *marked* '()
  "Windows pulled-mark during this overview invocation, most recent last.")

(defvar *saved-workspaces* '()
  "The workspace nodes as they were when the overview opened, as
(INDEX . NODE).  Snap-back puts these objects back exactly where they came
from -- the trees are saved by reference, not serialized, because nothing
can change them while the keyboard holds their windows.")

(defvar *assignments* '()
  "The current letter-to-window pairs, as (LETTER-STRING . WINDOW).")

(defvar *entry-index* 0
  "Zero-based index of the workspace under the cursor when the overview
opened.  This is where RET gathers the marked windows.")

(defun active-p () "True while the overview is on screen." *active*)

(defun assignments ()
  "The current letter-to-window pairs."
  *assignments*)

;;; =========================================================== collection

(defun leaf-path-of (window)
  "The absolute path of the leaf showing WINDOW, or NIL if it floats.

Floats are not on the keyboard: they never entered the tree, and dragging
them out of it for a zoom-out would be the tail wagging the dog."
  (let ((leaf (c:leaf-holding (c:world-root r:*world*) window)))
    (and leaf (c:node-path-to (c:world-root r:*world*) leaf))))

(defun collect-windows ()
  "Every tiled window in the session, in stable workspace-and-position
order: workspace one's windows top-left to bottom-right, then workspace
two's.  Stable order is what lets letter assignments become muscle memory;
floats are skipped because they have no place in the tree to be stable
about."
  (let ((windows (funcall (or *windows-function* #'r:all-windows))))
    (loop for window in windows
          for path = (leaf-path-of window)
          when path
            collect (list window path)
              into paired
          finally (return
                    (mapcar #'first
                            (sort paired
                                  (lambda (a b)
                                    (or (< (first (second a))
                                          (first (second b)))
                                        (and (= (first (second a))
                                                (first (second b)))
                                             (< (second (second a))
                                                (second (second b))))))
                                  ))))))

;;; ======================================================== the keyboard

(defun assign-letters (windows)
  "Deal LETTER-LAYOUT over WINDOWS in order: first window gets A, second
S, and so on.  Returns ((LETTER-STRING . WINDOW) ...)."
  (loop for letter across *letter-layout*
        for window in windows
        collect (cons (string letter) window)))

(defvar *keyboard-rows*
  '("qwertyuiop" "asdfghjkl" "zxcvbnm")
  "The physical keyboard geometry, top to bottom: where each letter SITS
on screen.  Independent of *LETTER-LAYOUT*, which decides which window gets
which letter; together they decide what you see.")

(defun rows-of-layout ()
  "The physical rows, as lists of characters.

This is geometry, not priority -- the shape of the thing your fingers know,
which does not change when you re-order which windows get the good letters."
  (mapcar (lambda (row) (coerce row 'list)) *keyboard-rows*))

(defun build-keyboard-tree (assignments)
  "A tree whose panes hold the assigned windows, arranged as keyboard rows:
a vertical split of horizontal splits.  Empty slots simply do not exist --
three windows make three panes, not a keyboard-shaped graveyard."
  (let* ((by-letter (make-hash-table :test #'equal)))
    (dolist (assignment assignments)
      (setf (gethash (car assignment) by-letter) (cdr assignment)))
    (let ((row-nodes '()))
      (dolist (row (rows-of-layout))
        (let ((leaves '()))
          (dolist (letter row)
            ;; ROW holds characters; the assignments are keyed by STRING.
            (let ((window (gethash (string letter) by-letter)))
              (when window
                (push (c:make-leaf window) leaves))))
          (when leaves
            (push (if (rest leaves)
                      (c:make-split :horizontal (nreverse leaves) nil)
                      (first leaves))
                  row-nodes))))
      (cond
        ((null row-nodes) (c:make-leaf))
        ((rest row-nodes) (c:make-split :vertical (nreverse row-nodes) nil))
        (t (first row-nodes))))))

;;; ================================================== snap back and forth

(defun swap-workspace-content (index new-node)
  "Put NEW-NODE into workspace INDEX, returning what was there."
  (let ((stack (c:world-workspaces r:*world*)))
    (when (< index (c:container-count stack))
      (let ((old (c:child-at stack index)))
        (c:remove-child stack index)
        (c:insert-child stack index new-node)
        old))))

(defun enter-overview ()
  "Rearrange every window into keyboard rows on the entry workspace.

Each workspace's whole node is saved by REFERENCE and swapped out; other
workspaces receive empty leaves, because their windows are on the keyboard
now.  Nothing is destroyed -- snap-back swaps the originals back, which is
also why UNDO does not need to know about any of this."
  (unless *active*
    (let ((stack (c:world-workspaces r:*world*)))
      (setf *entry-index* (or (first (c:world-cursor r:*world*)) 0)
            *marked* '()
            *saved-workspaces* '())
      ;; Collect FIRST: once the other workspaces are emptied below, their
      ;; windows are off-tree and unfindable -- the whole reason this
      ;; function once produced a keyboard of nobody.
      (setf *assignments* (assign-letters (collect-windows)))
      ;; Save and empty every workspace EXCEPT the entry one, which gets
      ;; the keyboard.
      (loop for i from 0 below (c:container-count stack)
            unless (= i *entry-index*)
              do (let ((old (swap-workspace-content i (c:make-leaf))))
                   (push (cons i old) *saved-workspaces*)))
      (let ((keyboard (build-keyboard-tree *assignments*)))
        (let ((entry-old (swap-workspace-content *entry-index* keyboard)))
          (push (cons *entry-index* entry-old) *saved-workspaces*)))
      (setf *active* t)
      (r:mark-dirty)))
  nil)

(defun restore-original-trees ()
  "Swap every saved workspace node back into place."
  (dolist (entry *saved-workspaces*)
    (swap-workspace-content (car entry) (cdr entry)))
  (setf *saved-workspaces* '() *active* nil))

(defun exit-overview (&key gather)
  "Snap back: swap every original workspace node into place, then move the
pulled-marked windows to the entry workspace if GATHER was asked for."
  (restore-original-trees)
  (when gather
    (dolist (window *marked*)
      (let ((path (leaf-path-of window)))
        (when (and path (/= (first path) *entry-index*))
          ;; Focus the pane first: SEND-TO-WORKSPACE moves the focused pane,
          ;; which is the shipped verb for exactly this move.
          (p:jump-cursor (p:current-policy) r:*world* path)
          (p:run-command "send-to-workspace" (1+ *entry-index*))))))
  (setf *marked* '())
  (r:mark-dirty)
  nil)

(defun go-to-window (window)
  "Snap back, then put the cursor on WINDOW -- wherever its workspace is."
  (restore-original-trees)
  (r:mark-dirty)
  (let ((path (leaf-path-of window)))
    (when path
      (r:workspace (1+ (first path)))
      (p:jump-cursor (p:current-policy) r:*world* path))))

(defun toggle-marked (window)
  "Mark or unmark WINDOW for the gather, and keep the overview armed."
  (cond
    ((member window *marked*)
     (setf *marked* (remove window *marked*))
     (r:notify "unmarked (~d)" (length *marked*)))
    (t
     (push window *marked*)
     (setf *marked* (nreverse *marked*))
     (r:notify "marked ~d" (length *marked*))))
  ;; Stay armed for the next pick.
  (setf p:*pending-keymap* (overview-keymap))
  (r:mark-dirty)
  nil)

;;; ================================================================ keymap

(defun overview-keymap ()
  "The mode's keymap, built fresh from the live assignments.

Plain letters run GO closures; shifted letters run PULL toggles that re-arm
this same map, which is how several picks survive the submap's one-key
lifetime.  RET gathers, ESC cancels."
  (let ((map (r:make-keymap :name "kbd-overview")))
    (dolist (assignment *assignments*)
      (destructuring-bind (letter . window) assignment
        (let ((upper (string-upcase letter)))
          ;; GO: jump to the window and snap back.
          (r:define-key map letter
                        (lambda () (go-to-window window)))
          ;; PULL: mark or unmark, staying armed.  A letter that IS its own
          ;; uppercase -- there are none in a letter layout, but a custom
          ;; one might -- would collide with its own GO binding.
          (unless (string= letter upper)
            (r:define-key map upper
                          (lambda () (toggle-marked window)))))))
    (r:define-key map "Return"
                  (lambda ()
                    (exit-overview :gather t)
                    (r:mark-dirty)))
    (r:define-key map "Escape"
                  (lambda ()
                    (exit-overview)
                    (r:mark-dirty)))
    map))

(r:defcommand toggle-kbd-overview ()
  "Zoom out to the keyboard of windows, or back if already there."
  (cond
    (*active*
     (exit-overview)
     (r:mark-dirty)
     nil)
    (t
     (enter-overview)
     ;; Arm the submap: while it is pending, capture-wanted-p says yes and
     ;; river hands us the keys -- the same machinery a chord uses, which
     ;; is why this needs no new input plumbing.
     (setf p:*pending-keymap* (overview-keymap))
     (r:mark-dirty)
     t)))

;;; ============================================================== overlays

(defun draw-badges ()
  "Called from :DRAW-OVERLAYS while the overview is active.

Each assigned window wears its letter at the top-left of its own rect --
the corner applications do not use for titles.  Marked windows are drawn
the same way; the mark count lives in the echo area, because a badge that
changes shape is noise."
  (when *active*
    (dolist (output (r:all-outputs))
      (p:guarded "kbd-overview badges"
        (let* ((area (c:output-rect output))
               (overlay (r:overlay-for :kbd-overview/letters output))
               (canvas (r:ensure-overlay overlay
                                         (c:rect-w area)
                                         (c:rect-h area)))
               (ox (c:rect-x area))
               (oy (c:rect-y area)))
          (when canvas
            (dolist (assignment *assignments*)
              (destructuring-bind (letter . window) assignment
                (let ((rect (ignore-errors (c:window-rect window))))
                  (when rect
                    (let* ((x (max 0 (- (c:rect-x rect) ox)))
                           (y (max 0 (- (c:rect-y rect) oy)))
                           (w (+ 8 (* 4 (r:text-width letter :scale 2))))
                           (h (+ 6 (* 2 (r:text-height :scale 2))))
                           (box (c:make-rect x y w h)))
                      (r:canvas-fill canvas (r:argb #xff 250 250 250) box)
                      (r:canvas-text canvas (+ x 3) (+ y 2) letter
                                     (r:argb #xff 20 20 20)
                                     :scale 2))))))))))))
