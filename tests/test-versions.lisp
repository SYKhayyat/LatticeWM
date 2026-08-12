;;;; tests/test-versions.lisp --- Which rivers this build will talk to.
;;;;
;;;; THE SMALLEST FUNCTION IN THE PROJECT WITH THE LARGEST BLAST RADIUS.
;;;; NEGOTIATED-VERSION decides, for every global river announces, whether this
;;;; program binds it and at what version — which is to say it decides whether
;;;; the window manager starts at all.  It was three lines inline in
;;;; BIND-ONE-GLOBAL, unreachable without a live wl_registry, and therefore had
;;;; no test of any kind while it was an equality check that refused every
;;;; river but one.
;;;;
;;;; That equality is the bug these tests exist to keep from coming back.  It
;;;; was written against a real hazard — river-window-management-v1 is young
;;;; enough to change *within* a version number — but it did not address that
;;;; hazard at all: a river that quietly changes what a request means still
;;;; advertises the same number and still passes an equality.  What it caught
;;;; was the announced, polite case, the version bump, which is the one case
;;;; the Wayland object model already makes safe.  The cost was a login screen
;;;; that refused every user whose distribution shipped river before we did.
;;;;
;;;; So the asymmetry below is the whole design, and each half is asserted
;;;; separately: DOWNWARD there is no answer, because a request the compositor
;;;; has never heard of cannot be degraded around.  UPWARD there is always an
;;;; answer, because binding at version N obliges river to speak N.

(in-package #:latticewm/tests)

(def-suite versions :in model
  :description "The protocol version floor and ceiling: which rivers this
build accepts, which it refuses, and what it binds for each.")

(in-suite versions)

;;; The window manager's own numbers, which are the ones that decide whether
;;; anything runs.  Read from the constants rather than written here, so these
;;; tests follow a re-vendor instead of having to be found after one.
(defun wm-floor () r::+window-management-floor+)
(defun wm-ceiling () r::+window-management-version+)

(test a-river-below-the-floor-is-refused
  "Under the floor there is no version to bind, and NIL is how that is said."
  (is (null (r::negotiated-version (1- (wm-floor)) (wm-floor) (wm-ceiling))))
  (is (null (r::negotiated-version 1 (wm-floor) (wm-ceiling))))
  (is (null (r::negotiated-version 0 (wm-floor) (wm-ceiling)))))

(test a-river-at-the-floor-binds-at-the-floor
  "The oldest river we accept is accepted, which an off-by-one would lose.

This is the assertion that costs the most if it is wrong in the quiet
direction: a floor one too high refuses a river that would have worked, and it
refuses it at a login screen with a message saying the user should upgrade."
  (is (eql (wm-floor)
           (r::negotiated-version (wm-floor) (wm-floor) (wm-ceiling)))))

(test a-river-between-the-two-binds-at-what-it-offers
  "Not the ceiling — what river actually has, which is all it can speak."
  (loop for offered from (wm-floor) to (wm-ceiling)
        do (is (eql offered
                    (r::negotiated-version offered (wm-floor) (wm-ceiling))))))

(test a-river-newer-than-this-build-binds-at-our-ceiling
  "THE POINT OF THE WHOLE CHANGE, and the case that used to be a refusal.

Binding at the ceiling is not a compromise.  Wayland obliges the compositor to
speak the version the client bound, so a river ten versions ahead answers this
build in the dialect this build was compiled for.  What we lose is the features
added after our ceiling; what we do not lose is the session."
  (dolist (offered (list (1+ (wm-ceiling)) (+ (wm-ceiling) 10)
                         (+ (wm-ceiling) 100)))
    (is (eql (wm-ceiling)
             (r::negotiated-version offered (wm-floor) (wm-ceiling)))
        "river offering v~d should bind at our ceiling v~d" offered
        (wm-ceiling))))

(test the-bound-version-never-exceeds-the-ceiling
  "The property, over the whole range, rather than five sampled points.

Binding above the ceiling is the failure this clamp exists for and it is
*silent*: it promises river that we can decode anything that version may send,
and the broken promise arrives as DISPATCH-ONE-EVENT logging `undecodable event
ignored' at :DEBUG.  Four of the six protocols bound unclamped until they did
not, and nothing would have said so."
  ;; NO `or' INSIDE `is'.  FiveAM takes the form apart to report both sides of
  ;; a comparison, which means it evaluates the arguments separately and an
  ;; (or (null bound) (<= bound ...)) does not short-circuit the way the
  ;; reading of it says: below the floor BOUND is NIL and `<=' gets it.  The
  ;; first version of this test failed exactly there, which is the test being
  ;; wrong rather than the code, and asserting the two branches apart is
  ;; better anyway -- it says what should happen below the floor instead of
  ;; tolerating it.
  (loop for offered from 1 to (+ (wm-ceiling) 50)
        for bound = (r::negotiated-version offered (wm-floor) (wm-ceiling))
        do (if (< offered (wm-floor))
               (is (null bound)
                   "v~d is below the floor and should have been refused, ~
                    not negotiated to v~a" offered bound)
               (progn
                 (is (<= bound (wm-ceiling))
                     "v~d negotiated to v~d, above the ceiling" offered bound)
                 (is (<= bound offered)
                     "v~d negotiated to v~d, above what river offered"
                     offered bound)))))

(test the-floor-is-not-above-the-ceiling
  "A floor over its ceiling refuses every river in existence, including ours.

Gate 5 asserts this too.  It is here as well because the two constants sit
beside each other and rise on different occasions — the ceiling on every
re-vendor, the floor only when this program starts sending a newer request —
and the failure needs a running compositor to show itself anywhere else."
  (is (<= (wm-floor) (wm-ceiling)))
  (is (<= r::+xkb-bindings-floor+ r::+xkb-bindings-version+)))

(test the-optional-protocols-are-clamped-to-their-own-ceilings
  "The four that used to bind at whatever river offered, whatever that was.

Each is optional and each has a floor of 1, so none of them can refuse — the
only question they can get wrong is the ceiling, and getting it wrong is the
silent direction."
  (dolist (ceiling (list r::+layer-shell-version+ r::+input-manager-version+
                         r::+libinput-config-version+ r::+xkb-config-version+))
    (is (eql ceiling (r::negotiated-version (+ ceiling 5) 1 ceiling))
        "a river past v~d should still bind at v~d" ceiling ceiling)
    (is (eql 1 (r::negotiated-version 1 1 ceiling))
        "a floor of 1 can never refuse")))
