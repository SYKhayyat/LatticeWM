;;;; tests/test-focus-follows-mouse.lisp --- The refinement, without a screen.
;;;;
;;;; The same bar tests/test-examples.lisp holds the promoted example to,
;;;; applied to the module: it loads, it does what it claims, disable really
;;;; restores, and none of it leaks into the suites that run afterwards.

(defpackage #:focus-follows-mouse/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:t* #:latticewm/tests)
                    (#:ffm #:focus-follows-mouse))
  (:export #:run-all))

(in-package #:focus-follows-mouse/tests)

;; A top-level suite of its own -- deliberately not :in the core's MODEL
;; suite, or RUN-ALL would run it twice, once through MODEL and once through
;; the registry.
(def-suite ffm :description "The focus-follows-mouse refinement.")
(in-suite ffm)

;; Join the harness's discovery: tools/test.lisp loads this system, and
;; latticewm/tests:run-all runs every suite that registered itself.
(t*:register-extension-suite "FOCUS-FOLLOWS-MOUSE/TESTS" "FFM")

(defun run-all ()
  "Run the FFM suite (called through RUN-ALL's registry walk)."
  (let ((results (run 'ffm)))
    (explain! results)
    (values (results-status results) (length results))))

;;; ------------------------------------------------------------- fixtures

(defun fresh-world ()
  "A world and policy to work on, with two tiled panes laid out.

The layout matters: POINTER-FOCUS answers from :LAST-PLACEMENTS, so a test
that never placed anything is testing the empty case twice."
  (let ((world (c:make-world))
        (policy (make-instance 'p:conventional-policy)))
    (dotimes (i 2)
      (p:on-window-open policy world
                        (make-instance 'c:window :app-id (format nil "app~d" i))))
    (setf (c:prop world :last-placements)
          (p:layout-node policy (c:world-root world)
                         (c:make-rect 0 0 1000 1000)))
    (values world policy)))

(defun cover-with-float (world)
  "Put one float across the whole output, the way a maximized dialog would."
  (let ((window (make-instance 'c:window :app-id "floaty")))
    (setf (c:window-floating-p window) t
          (c:window-rect window) (c:make-rect 0 0 2000 2000))
    (push (make-instance 'c:floating-window :window window
                                            :rect (c:make-rect 0 0 2000 2000))
          (c:world-floats world))))

;;; ================================================================ tests

(test loading-alone-changes-nothing
  "Loadable without being enabled, which is what makes it removable."
  (multiple-value-bind (world policy) (fresh-world)
    (declare (ignore world))
    (is-false (ffm:enabled-p))
    ;; The method is installed -- the system was loaded -- but passes
    ;; straight through, so the answer is whatever the shipped default says.
    (is (eql (p:pointer-focus policy (c:make-world) 500 500)
             (p:pointer-focus (make-instance 'p:conventional-policy)
                              (c:make-world) 500 500)))))

(test enable-composes-and-disable-restores
  "The four-line contract from EXTENDING.org, exercised."
  (multiple-value-bind (world policy) (fresh-world)
    (cover-with-float world)
    ;; Before: the pointer over the middle of the screen names a pane.
    (is-true (p:pointer-focus policy world 500 500)
             "the default focuses the pane under the pointer")
    ;; Enabled: the float wins, so the pane underneath is left alone.
    (ffm:enable)
    (unwind-protect
         (progn
           (is-false (p:pointer-focus policy world 500 500)
                     "the pointer is over a float, so nothing is focused")
           ;; Enable twice is a no-op, not an error and not an accumulation.
           (finishes (ffm:enable))
           (is-false (p:pointer-focus policy world 500 500)))
      ;; Disabled: exactly the shipped answer again.
      (ffm:disable))
    (is-false (ffm:enabled-p))
    (is-true (p:pointer-focus policy world 500 500)
             "disable restored the shipped behaviour")))

(test enabling-turns-the-prerequisite-on
  "The refinement refines a question the runtime only asks when
*FOCUS-FOLLOWS-MOUSE* is on, so ENABLE turns it on rather than letting the
module load into a session where it can do nothing.  DISABLE leaves it --
the option is the core's knob, and the user may have set it themselves."
  (let ((was p:*focus-follows-mouse*))
    (unwind-protect
         (progn
           (setf p:*focus-follows-mouse* nil)
           (ffm:enable)
           (is-true p:*focus-follows-mouse* "enable turned the prerequisite on")
           (ffm:disable)
           (is-true p:*focus-follows-mouse*
                    "and disable did not reach past the module to unset it"))
      (setf p:*focus-follows-mouse* was))))

(test focus-can-change-before-anything-has-been-laid-out
  "The regression the example carried and the module inherited: ON-FOCUS-CHANGE
reads :RECT-INDEX, which EMIT writes after a layout and which therefore does
not exist yet on a world that has never been placed.  `(gethash node nil)' is
a type error, not a miss, so an unguarded read takes down every focus change
before the first frame."
  (multiple-value-bind (world policy) (fresh-world)
    (declare (ignore policy))
    ;; No :LAST-PLACEMENTS write for THIS world would matter; the guard reads
    ;; :RECT-INDEX, which nothing has written at all.
    (is (null (c:prop world :rect-index)))
    (ffm:enable)
    (unwind-protect
         (finishes (p:on-focus-change (make-instance 'p:conventional-policy)
                                      world
                                      (c:world-cursor world)
                                      (c:world-cursor world)))
      (ffm:disable))))

(test the-refinement-is-invisible-to-a-policy-that-never-heard-of-it
  "Gate 4's shape, at module scale: a conventional policy in a world with no
floats behaves identically with the switch on and off, because the refinement
only ever changes the answer when a float is actually involved."
  (multiple-value-bind (world policy) (fresh-world)
    (let ((plain (p:pointer-focus policy world 250 250)))
      (is-true plain)
      (ffm:enable)
      (unwind-protect
           (handler-bind ((error (lambda (c)
                                   (format t "~&DEBUG signal: ~a~%" c)
                                   (sb-debug:print-backtrace :count 12))))
             (is (c:path-equal plain (p:pointer-focus policy world 250 250))
                 "no float anywhere, so the answer did not move"))
        (ffm:disable)))))
