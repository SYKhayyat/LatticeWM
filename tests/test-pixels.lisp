;;;; tests/test-pixels.lisp --- Matching the compositor's windows to ours (#135).
;;;;
;;;; The whole capture sequence needs a compositor, and the integration harness
;;;; is where that is checked (a frame arrives, at a real size, with content in
;;;; it).  What is checked here is the half that is *ours*: which foreign
;;;; toplevel belongs to which of our windows, and what the absence of the
;;;; protocols means.
;;;;
;;;; THAT MATCH IS THE PART THAT CAN BE WRONG QUIETLY.  Two handles that both
;;;; claim an app-id, a handle that never said what it is, a window whose title
;;;; is empty: every one of those has an answer that looks like a picture until
;;;; somebody notices it is a picture of the wrong window.

(in-package #:latticewm/tests)
(in-suite pixels)

(defun a-toplevel (app-id &key (title nil) (closed nil))
  "A TOPLEVEL as the compositor would have announced it."
  (r::make-toplevel :handle (list :handle app-id title)
                    :app-id app-id :title title :closed closed))

(defmacro with-a-toplevel-server ((&rest toplevels) &body body)
  "A server object carrying TOPLEVELS and nothing else that matters."
  `(let ((r:*server* (make-instance 'r::server
                                    :display nil
                                    :toplevels (list ,@toplevels))))
     ,@body))

;;; ================================================================= matching

(test a-window-is-matched-by-its-app-id
  (with-a-toplevel-server ((a-toplevel "foot") (a-toplevel "firefox"))
    (let ((found (r:match-toplevel r:*server* "foot" "whatever")))
      (is-true found)
      (is (string= "foot" (r:toplevel-app-id found))))
    (is (string= "firefox"
                 (r:toplevel-app-id (r:match-toplevel r:*server* "firefox" nil))))))

(test a-title-is-only-consulted-when-the-app-id-says-nothing
  "Two windows of one application share their app-id, so an app-id match that is
ambiguous is not an answer -- but a title that names exactly one of them is."
  (with-a-toplevel-server ((a-toplevel "foot" :title "one")
                           (a-toplevel "foot" :title "two"))
    (is (null (r:match-toplevel r:*server* "foot" nil))
        "two windows, one app-id: no answer rather than the first one")
    (is (string= "two" (r:toplevel-title (r:match-toplevel r:*server* "foot" "two"))))))

(test a-closed-toplevel-is-not-a-candidate
  "The window is gone; a picture of where it was would be a picture of whatever
is behind it now."
  (with-a-toplevel-server ((a-toplevel "foot" :closed t))
    (is (null (r:match-toplevel r:*server* "foot" nil)))))

(test a-toplevel-that-never-said-what-it-is-is-not-matched
  (with-a-toplevel-server ((a-toplevel nil :title nil))
    (is (null (r:match-toplevel r:*server* nil nil)))
    (is (null (r:match-toplevel r:*server* "foot" nil)))
    (is (null (r:match-toplevel r:*server* nil "a title")))))

;;; ============================================================ the conversions

(test a-capture-is-made-opaque
  "XRGB8888 leaves the top byte undefined and ARGB8888 premultiplied needs it to
be 255.  Copying the words as they arrive draws an invisible thumbnail, which
looks exactly like a capture that failed."
  (let ((sap (sb-sys:allocate-system-memory 12)))
    (unwind-protect
         (progn
           (setf (sb-sys:sap-ref-32 sap 0) #x00112233
                 (sb-sys:sap-ref-32 sap 4) #x00aabbcc
                 (sb-sys:sap-ref-32 sap 8) 0)
           (let ((words (r::xrgb-to-argb sap 3 1)))
             (is (= 3 (length words)))
             (is (= #xff112233 (aref words 0)))
             (is (= #xffaabbcc (aref words 1)))
             (is (= #xff000000 (aref words 2))
                 "a black pixel is opaque black, not a hole")))
      (sb-sys:deallocate-system-memory sap 12))))

(test a-checksum-says-whether-anything-was-drawn
  (let ((blank (r::%make-pixels :width 2 :height 1
                                :data (make-array 2 :element-type '(unsigned-byte 32)
                                                    :initial-element #xff000000)))
        (drawn (r::%make-pixels :width 2 :height 1
                                :data (make-array 2 :element-type '(unsigned-byte 32)
                                                    :initial-element #xff000000))))
    (setf (aref (r:pixels-data drawn) 1) #xff123456)
    (is (zerop (nth-value 1 (r:pixels-checksum blank)))
        "an all-black frame reports no non-zero pixels")
    (is (= 1 (nth-value 1 (r:pixels-checksum drawn))))
    (is (= #x123456 (nth-value 0 (r:pixels-checksum drawn)))
        "and the sum is over the colour, not the alpha")))

;;; ============================================== a compositor without the lot

(test nothing-is-available-without-the-managers
  (with-a-toplevel-server ()
    (is (null (r:capture-available-p r:*server*)))
    (let ((report (r:capture-report nil)))
      (is (null (getf report :available)))
      (is (stringp (getf report :reason))))))

(test asking-for-a-picture-answers-nil-rather-than-signalling
  "A thumbnail is decoration.  A decoration that can take the session down is not
worth having, so every entry point answers NIL on a compositor that cannot do
it -- including when there is no session at all."
  (with-a-toplevel-server ()
    (is (null (r:capture-window (make-instance 'c:window :app-id "foot")
                                :server r:*server* :display nil)))
    (is (null (r:capture-window (make-instance 'c:window :app-id "foot")
                                :server nil :display nil)))))

(test the-probe-says-why-instead-of-guessing
  "The diagnostic is a command, and a command's answer is a sentence in the echo
area.  On a compositor that cannot do this the sentence has to say so -- an
empty echo area would be indistinguishable from a probe that worked."
  (with-a-toplevel-server ()
    (let ((message (r:capture-probe)))
      (is (stringp message))
      (is (search "no window pixels" message)))))
