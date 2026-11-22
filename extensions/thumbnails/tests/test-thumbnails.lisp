;;;; tests/test-thumbnails.lisp --- The deck, with pictures in it (#130).
;;;;
;;;; The capture itself is checked in the integration harness, where there is a
;;;; compositor and a real window (#135).  What is checked here is everything
;;;; this module decides:
;;;;
;;;;   * OFF costs nothing -- not one capture attempt, ever;
;;;;   * ON takes ONE picture per window per look, and reuses it;
;;;;   * a look has edges, and both of them throw the cache away;
;;;;   * a cell holding several windows keeps its box, because four pictures and
;;;;     one box is a different picture;
;;;;   * a window that cannot be captured is remembered as such, so a deck full
;;;;     of uncapturable windows asks the compositor once each rather than
;;;;     once per frame.
;;;;
;;;; PICTURE-SOURCE is bound to a stub throughout: the real source needs a
;;;; compositor, and a test that needed one would be an integration test.

(defpackage #:thumbnails/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:thumbnails #:thumbnails))
  (:export #:run-all))

(in-package #:thumbnails/tests)

(def-suite thumbnails :description "Pictures of windows in the deck, off by default.")
(in-suite thumbnails)

(t*:register-extension-suite "THUMBNAILS/TESTS" "THUMBNAILS")

(defun run-all ()
  (let ((results (run 'thumbnails)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defvar *stub-calls* nil
  "Every window the stub picture source was asked about, newest first.")

(defun a-picture (&key (width 2) (height 2))
  "A synthetic picture in the shape P:CELL-PICTURE documents."
  (list :width width :height height
        :data (make-array (* width height) :element-type '(unsigned-byte 32)
                                          :initial-element #xff203040)))

(defun a-capturable (window)
  "Stand in for R:CAPTURE-WINDOW: answer a captured frame, and count the asking."
  (push window *stub-calls*)
  (r::%make-pixels :width 4 :height 3
                   :data (make-array 12 :element-type '(unsigned-byte 32)
                                        :initial-element #xff445566)))

(defun an-uncapturable (window)
  (push window *stub-calls*)
  nil)

(defmacro with-the-module ((&key (on t) (source '#'a-capturable)) &body body)
  "This module in a known state, with a stub where the compositor was."
  `(let ((thumbnails:*thumbnails* ,on)
         (thumbnails:*pictures* (make-hash-table :test #'eq))
         (thumbnails:*captures* 0)
         (thumbnails:*picture-source* ,source)
         (*stub-calls* nil)
         (p:*policy* (t*::policy)))
     ,@body))

(defun a-leaf-holding (window)
  "A leaf holding WINDOW, as the tree would hold it."
  (c:make-leaf window))

(defun a-window (&optional (app-id "foot"))
  (make-instance 'c:window :app-id app-id))

;;; ================================================== off is off, and costs nothing

(test off-means-no-captures-at-all
  "The claim `off by default' is worth nothing if off still asks the compositor
and throws the answer away.  This is the check that says it does not."
  (with-the-module (:on nil)
    (let ((leaf (a-leaf-holding (a-window))))
      (is (null (p:cell-picture p:*policy* leaf)))
      (is (null *stub-calls*) "no capture was attempted")
      (is (zerop thumbnails:*captures*)))))

(test turning-it-on-does-not-capture-anything-by-itself
  "A switch that starts copying frames for windows nobody is looking at is a
switch that spins up a fan in an empty room."
  (with-the-module (:on nil)
    (thumbnails:enable)
    (is-true (thumbnails:enabled-p))
    (is (null *stub-calls*) "nothing was captured")
    (thumbnails:disable)
    (is (null (thumbnails:enabled-p)))))

;;; ============================================================ one per look

(test a-capture-is-taken-once-and-reused
  "The deck is redrawn every frame.  One picture per window per look is the whole
difference between a thumbnail and a video feed."
  (with-the-module ()
    (let ((leaf (a-leaf-holding (a-window))))
      (dotimes (i 5)
        (is-true (p:cell-picture p:*policy* leaf) "a picture answers every time"))
      (is (= 1 (length *stub-calls*)) "and was asked for once: ~d"
          (length *stub-calls*))
      (is (= 1 thumbnails:*captures*)))))

(test a-look-has-two-edges-and-both-clear-the-cache
  "Opening the deck again must show the session as it is now, not as it was the
first time this evening."
  (with-the-module ()
    (let ((leaf (a-leaf-holding (a-window))))
      (p:cell-picture p:*policy* leaf)
      (is (= 1 (length *stub-calls*)))
      (p:run-hooks :preview-changed t)
      (is (zerop (hash-table-count thumbnails:*pictures*)))
      (p:cell-picture p:*policy* leaf)
      (is (= 2 (length *stub-calls*)) "a new look takes a new picture")
      (p:run-hooks :preview-changed nil)
      (is (zerop (hash-table-count thumbnails:*pictures*))
          "and taking the deck down forgets it too"))))

(test forgetting-on-demand-takes-new-pictures-next-draw
  (with-the-module ()
    (let ((leaf (a-leaf-holding (a-window))))
      (p:cell-picture p:*policy* leaf)
      (is (= 1 (thumbnails:forget-pictures)))
      (p:cell-picture p:*policy* leaf)
      (is (= 2 (length *stub-calls*))))))

;;; ==================================================== what a cell holds

(test a-cell-of-two-windows-keeps-its-box
  "Four pictures and one box is a different picture.  A cell that is not one
thing draws the thing it always drew, with its +N."
  (with-the-module ()
    (let ((node (c:make-split :vertical
                              (list (a-leaf-holding (a-window "foot"))
                                    (a-leaf-holding (a-window "firefox"))))))
      (is (null (p:cell-picture p:*policy* node)))
      (is (null *stub-calls*) "and nothing was captured for it"))))

(test a-stack-of-four-shows-the-window-that-is-on-screen
  "A tab group is the one cell where a picture is both unambiguous and worth
most: it has a selected child, so one of its four windows is the one the cell
is a picture of.  Before this it drew an empty box, which is the deck declining
to answer its own question."
  (with-the-module ()
    (let* ((windows (list (a-window "one") (a-window "two")
                          (a-window "three") (a-window "four")))
           (stack (c:make-stack (mapcar #'a-leaf-holding windows) 1)))
      (is (eq (second windows) (first (c:node-windows (c:visible-node stack))))
          "the selected child is the visible one")
      (is (p:cell-picture p:*policy* stack) "so the cell has a picture")
      (is (equal (list (second windows)) *stub-calls*)
          "and exactly one window was captured: the visible one"))))

(test a-stack-of-stacks-peels-to-the-thing-that-is-on-screen
  "Four tabs, one of which is itself a group of two.  A selection is a question
you can ask again."
  (with-the-module ()
    (let* ((one (a-window "one"))
           (inner (c:make-stack (list (a-leaf-holding (a-window "two-a"))
                                      (a-leaf-holding (a-window "two-b")))
                                1))
           (outer (c:make-stack (list (a-leaf-holding one) inner) 1)))
      (is (eq (c:visible-node outer) (c:visible-node inner))
          "the selection was asked twice")
      (is (eq (second (c:node-windows inner))
              (first (c:node-windows (c:visible-node outer))))
          "and the answer is the window on screen")
      (is (p:cell-picture p:*policy* outer))
      (is (= 1 (length *stub-calls*)) "and one capture, not two"))))

(test a-stack-whose-visible-child-is-a-split-keeps-its-box
  "The visible child must be *one window*.  A tab that is a four-way split is
still four pictures and one box, and a picture of one of them would be a
picture of something that is not what is selected."
  (with-the-module ()
    (let* ((split (c:make-split :vertical
                              (list (a-leaf-holding (a-window "foot"))
                                    (a-leaf-holding (a-window "firefox")))))
           (stack (c:make-stack (list split (a-leaf-holding (a-window "mail"))) 0)))
      (is (null (p:cell-picture p:*policy* stack)))
      (is (null *stub-calls*) "and nothing was captured for it"))))

(test a-cell-of-no-windows-draws-nothing
  (with-the-module ()
    (is (null (p:cell-picture p:*policy* (c:make-stack '() 0))))
    (is (null *stub-calls*))))

(test a-window-that-cannot-be-captured-is-asked-about-once-per-look
  "A deck full of windows the compositor will not hand over must not ask again on
every frame; `no picture' is an answer worth remembering until the look ends."
  (with-the-module (:source #'an-uncapturable)
    (let ((leaf (a-leaf-holding (a-window))))
      (dotimes (i 4)
        (is (null (p:cell-picture p:*policy* leaf))))
      (is (= 1 (length *stub-calls*)) "asked once: ~d" (length *stub-calls*))
      (is (= 1 thumbnails:*captures*)))))

;;; ========================================================= the conversion

(test a-picture-is-the-words-a-canvas-wants
  "No conversion, no copy: the plist carries the very vector the capture made,
which is what makes drawing it a memcpy."
  (with-the-module (:source (lambda (window)
                              (declare (ignore window))
                              (r::%make-pixels :width 3 :height 2
                                               :data (make-array 6
                                                                 :element-type '(unsigned-byte 32)
                                                                 :initial-element #xff010203))))
    (let* ((leaf (a-leaf-holding (a-window)))
           (picture (p:cell-picture p:*policy* leaf)))
      (is (= 3 (getf picture :width)))
      (is (= 2 (getf picture :height)))
      (is (= 6 (length (getf picture :data))))
      (is (= #xff010203 (aref (getf picture :data) 0))))))

;;; =========================================================== the predicates

(test the-option-is-reachable-from-a-configuration-file
  "(setf *thumbnails* t) in an init file must set THIS variable, which it cannot
do if the symbol it names is not visible in the package the file is read in."
  (is-true (p::option-boundp :thumbnails))
  (is (eq 'thumbnails:*thumbnails* (p::option-variable :thumbnails)))
  (is (null (p::option-default :thumbnails))
      "off is the shipped default, and this is where that is written down"))
