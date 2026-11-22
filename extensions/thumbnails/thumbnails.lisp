;;;; thumbnails/thumbnails.lisp --- The deck, with pictures in it.
;;;;
;;;; #130 asked for thumbnails and the answer was no for a long time, for one
;;;; reason: this program draws only what it draws, and a window is somebody
;;;; else's surface.  #135 removed that reason.  What is left is a decision
;;;; about cost, and the decision is a module with a switch.
;;;;
;;;; THE WHOLE INTERFACE IS ONE METHOD.  The deck asks P:CELL-PICTURE for a node
;;;; and draws whatever comes back; the default answer is NIL, and this file
;;;; answers with a picture when it can.  Nothing else in the program knows this
;;;; file exists, which is what makes "off" mean *nothing happens* rather than
;;;; *something happens and is ignored*.
;;;;
;;;; ONE PICTURE PER WINDOW PER LOOK.  The deck is redrawn on every frame; a
;;;; capture per frame would be a video feed wearing a deck's clothes.  So a
;;;; picture is taken when a window is first drawn in a look, and reused for the
;;;; rest of it -- and :PREVIEW-CHANGED throws the cache away at both edges, so
;;;; opening the deck again shows the session as it is now rather than as it was
;;;; the first time you opened it this evening.
;;;;
;;;; A PICTURE IS STALE BY DEFINITION.  It is a copy taken at a moment, of a
;;;; window that has gone on changing, so nothing here pretends otherwise: the
;;;; deck says "this is what that window looked like a second ago", which is
;;;; exactly what a person asks a thumbnail.  There is no refresh timer, and
;;;; there will not be one: a window manager that copies frames nobody is
;;;; looking at is a window manager with a fan spinning up in an empty room.

(in-package #:thumbnails)

;;; ================================================================ state

(p:define-option *thumbnails* nil
  "Whether the deck draws a picture of each window.

Off by default, and the default is a statement rather than caution: a picture
costs a copy out of the compositor, the deck is redrawn whenever anything moves,
and a decoration that spends that on behalf of somebody who did not ask for it is
a decoration that makes the program worse.  Turn it on with =M-x thumbnails on=.

When it is off, nothing is captured and the deck draws the boxes and names it
has always drawn -- so off is not a degraded deck, it is the deck.")

(defvar *pictures* (make-hash-table :test #'eq)
  "Window -> the picture taken of it during the current look.

Keyed by the window object, and emptied at both edges of a look.  Keyed by object
rather than by name because two windows of one application share a name and the
cache is about *this* window's pixels.")

(defvar *captures* 0
  "How many pictures have been taken since the module was loaded.

A counter rather than a guess, because the only way to check that OFF means OFF
is to be able to see that nothing was captured -- and \"nothing looks broken\" is
not a check anybody can run.")

(defvar *picture-source* #'r:capture-window
  "Where a picture comes from: a function of a window, answering PIXELS or NIL.

THE ONE SEAM.  R:CAPTURE-WINDOW needs a compositor, a toplevel list and a frame,
so a test that wants to check the plumbing of this file cannot have any of them.
Binding this to a stub is how that is tested; it is also where a future capture
path would be swapped in, which is why it is a variable and not a test hook
hiding in a comment.")

(defun enabled-p () "True while the deck is drawing pictures." *thumbnails*)

;;; ============================================================== the switch

(defun forget-pictures ()
  "Throw away every cached picture.  Answers how many there were."
  (let ((count (hash-table-count *pictures*)))
    (clrhash *pictures*)
    count))

(defun enable ()
  "Draw pictures in the deck.  Captures nothing until a deck is drawn."
  (setf *thumbnails* t)
  (forget-pictures)
  ;; Take a fresh look at the next draw, whatever was on screen before.
  (r:mark-dirty)
  nil)

(defun disable ()
  "Stop drawing pictures, and forget the ones already taken.  Idempotent."
  (setf *thumbnails* nil)
  (forget-pictures)
  (r:mark-dirty)
  nil)

;;; ============================================================ the pictures

(defun picture-of (pixels)
  "PIXELS as the plist P:CELL-PICTURE documents, or NIL.

The conversion is a *renaming* rather than a conversion: R:PIXELS holds the very
words a canvas holds, one 32-bit ARGB word each, which is the whole reason
CANVAS-BLIT is a copy."
  (when pixels
    (list :width (r:pixels-width pixels)
          :height (r:pixels-height pixels)
          :data (r:pixels-data pixels))))

(defun capture-one (window)
  "Take a picture of WINDOW now, or answer NIL.  Counts what it attempts."
  (incf *captures*)
  (picture-of (funcall *picture-source* window)))

(defun picture-for (node)
  "The picture to draw for NODE: the window it shows, when it shows one.

ONE WINDOW, NOT THE FIRST OF SEVERAL.  A cell holding four tiled windows is four
pictures and one box, and drawing whichever happens to be first would be a
thumbnail of a window that may not even be the visible one.  Those cells keep
their box and their count -- which is what the deck drew before there were
pictures, and is the honest drawing of a cell that is not one thing.

A STACK IS NOT THAT CASE, and it was treated as one (#137): a tab group of four
has a visible window, chosen by STACK-SELECTED, so a picture of it is a picture
of the cell.  So the question is asked of the part that is on screen rather than
of the whole cell: a leaf answers with its window, a stack with its selected
child, and a split with nothing.
  "
  (let* ((shown (c:visible-node node))
         (windows (and shown (c:node-windows shown))))
    (when (and windows (= 1 (length windows)))
      (let* ((window (first windows))
             (cached (gethash window *pictures* :none)))
        (cond
          ((not (eq cached :none)) cached)
          (t (let ((picture (capture-one window)))
               ;; A window that could not be captured is remembered as NIL, so
               ;; the deck does not ask the compositor about it on every frame:
               ;; the answer is "no picture" for the rest of this look.
               (setf (gethash window *pictures*) picture)
               picture)))))))

;;; ============================================================ the answers

(defmethod p:cell-picture ((policy p:policy) (node c:node))
  "A picture of the single window NODE holds, when this module is on.

THE POLICY ARGUMENT IS IGNORED, and that is worth saying out loud: pictures are
not a policy question, they are a fact about what the compositor will hand over.
Specializing on the base POLICY class is how this method is the answer for
every policy -- a lattice policy, a conventional one, and a user's own subclass
-- without a second method per class.  It is registered only while this file is
loaded, which is what makes the module a module.

The ONLY-IF-ON check comes first, so with the module off every call costs one
variable read and no capture is ever attempted: OFF is not `capture and discard'."
  (declare (ignore policy))
  (when *thumbnails*
    (picture-for node)))

(defun on-look-changed (up)
  "Start or end a look: forget what was captured for the previous one."
  (declare (ignore up))
  (forget-pictures)
  (r:mark-dirty))

;;; =============================================================== the verbs

(r:defcommand thumbnails (&optional (state nil))
  "Turn pictures in the deck ON or OFF, and say which.

With no argument it toggles.  =M-x thumbnails on= and =M-x thumbnails off= are
the explicit forms, and either word is accepted before it is parsed as a
preference -- a command that only toggles is a command you cannot script."
  (:interactive :string)
  (let* ((word (and state (string-downcase (string-trim '(#\space #\tab) state))))
         (wanted (cond ((null word) (not *thumbnails*))
                       ((member word '("on" "yes" "true" "1") :test #'string=) t)
                       ((member word '("off" "no" "false" "0") :test #'string=) nil)
                       (t :unrecognised))))
    (cond
      ((eq wanted :unrecognised)
       (r:notify "thumbnails: ~a is neither on nor off" word))
      ((null (r:capture-available-p))
       (when wanted (enable))
       (r:notify "thumbnails are on, but this compositor cannot copy a window"))
      (wanted
       (enable)
       (r:notify "thumbnails on: the deck will take a picture of each window ~
                  when it opens"))
      (t
       (disable)
       (r:notify "thumbnails off: the deck draws its boxes, as it did before")))))

(r:defcommand thumbnails-forget ()
  "Forget every picture taken so far, so the next look takes new ones.

For a window that changed while the deck was open, and for the case where a
picture is simply wrong: the cache is the one piece of stale state this module
keeps."
  (r:notify "forgot ~d picture~:p" (forget-pictures)))

;;; ============================================================== plumbing

;;; THE HOOK IS WHAT MAKES A LOOK A LOOK.  Without it the cache would be
;;; per-session, and a deck opened at breakfast would still be showing breakfast;
;;; with it, the pictures are as old as the look -- which is the honest age for a
;;; copy of a window that has been changing since it was taken.
(p:add-hook :preview-changed 'on-look-changed)

;;; A CONFIGURATION FILE IS READ IN LATTICEWM/USER, so the option's symbol has
;;; to be visible there or (setf *thumbnails* t) in an init file interns a new
;;; symbol, sets that, and changes nothing.  Every extension that defines an
;;; option says so here (#133 learned it the hard way).
(r:install-vocabulary '#:thumbnails)
