;;;; tests/suite.lisp --- The test harness.
;;;;
;;;; A window manager is mostly untestable without a compositor.  The model is
;;;; the exception, and it is also the part where a bug is worst — a focus
;;;; repair that is subtly wrong produces "where did my cursor go" reports that
;;;; are close to undebuggable, because each instance is local and plausible.
;;;;
;;;; So: everything in src/model/ and src/policy/ is tested here, with no
;;;; compositor, no protocol, and no globals.
;;;;
;;;; THAT FIRST SENTENCE IS TRUE AND WAS TAKEN TOO FAR.  It is a statement about
;;;; *this* file, not about the project: tools/integration.lisp runs a real river
;;;; headlessly in a couple of seconds and asserts on what came back, and every
;;;; bug it has found lives in code where the model was right and the screen was
;;;; wrong — which is precisely the class this suite cannot see, because a test
;;;; that constructs the world can only ever ask the world what it thinks.
;;;;
;;;; The rule for which file a check belongs in: if it can be established by
;;;; constructing state, it belongs here, where it is cheaper and faster.  If it
;;;; is only true when a compositor agreed, it belongs there.

(defpackage #:latticewm/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime)
                    (#:w #:latticewm/wire))
  (:export #:run-all #:model #:geometry #:tree #:motion #:lifecycle #:surface
           #:container #:hooks #:minibuffer #:ipc #:state #:tags #:verbs #:wire
           #:devices
           #:capture
           #:shares
           #:states
           #:boundaries #:pixels #:versions #:wait-until
           #:*extension-suites* #:register-extension-suite
           ;; shared fixtures for extension suites (#120)
           #:fresh-temporary-directory #:block-the-replacement
           #:unblock-the-replacement))

(in-package #:latticewm/tests)

(def-suite model :description "Everything testable without a compositor.")

(def-suite geometry :in model)
(def-suite tree :in model)
(def-suite motion :in model)
(def-suite lifecycle :in model)
(def-suite surface :in model
  :description "The policy protocol, tested as a product: a DEFMETHOD from
outside changes behaviour, live, with no core edit.")

(def-suite container :in model
  :description "The *other* extension surface -- what a new container kind
answers rather than what a new policy answers.  It had no membership test, no
generated document and no documentation gate until it had all three, and its
failures were the ones that absence predicts.")
(def-suite hooks :in model
  :description "The third extension mechanism.  A generic decides, an option
supplies the shipped answer, and a hook notices — and the hook was the one with
nothing asking whether anybody had ever attached to it, which fourteen of the
seventeen had not.")

(def-suite devices :in model
  :description "Input device configuration: which settings a device should
have, which is pure, and how a value reaches the wire, which is not.")

(def-suite capture :in model
  :description "What the window manager knows about being recorded: the count
river reports per window and per output, who is told about a change, and what
the status line says for as long as it lasts.")

(def-suite pixels :in model
  :description "The buffers the window manager draws its own decorations into.
The compositor's half of this cannot be constructed, which is how one canvas
per overlay and no wl_buffer.release listener survived every gate and 1783
checks; what is here is the bookkeeping that decides which buffer is safe to
draw into and what changed since the last one.")

(def-suite minibuffer :in model
  :description "Reading a line, which needs no compositor either — the prompt
is a string, a point and a table of what keys mean.")

(def-suite ipc :in model
  :description "The control socket: one form in, one line out, and what
happens to the form that never returns.  Needs no compositor and gets none,
because the interesting half — which thread waits, which thread runs, and
whether the second is still alive afterwards — is a property of threads.")

(def-suite wire :in model
  :description "What the protocol can carry, at the boundary where the model
stops and the marshaller starts: a coordinate is allowed to be larger than a
Wayland int, and the request is not.")

(def-suite tags :in model
  :description "A tag is a name, not a keyword.  What a prompt, a window rule and
a state file each make of the same name, and why normalizing one must not add a
permanent keyword to the package every protocol key lives in.")

(def-suite verbs :in model
  :description "The commands, where they can be asked a question without a
compositor: which windows a name reaches, what a picker offers for two of them
that share one, and whether choosing an entry lands on it.  #34 is the first
inhabitant -- the door that lets you reach a window you have not labelled.")

(def-suite shares :in model
  :description "One workspace across several monitors (#96), which is the one
place a display question and a model question meet: a group draws one plane
over the union of its rectangles and every member but the leader draws
nothing.  Checkable with no compositor, because an output is a rectangle and a
name.  Two of these checks are about what the feature must NOT do -- a follower
may not add a second layout entry, and the workspace invariant must still move
a stranger off a shared index -- and those are the ones that keep the
exception the feature needs from becoming the collapse it replaced.")

(def-suite stacking :in model
  :description "RAISE, LOWER and PIN (#13).  Tiled windows do not overlap, so
every question here is a float question -- and the two that matter are about
*permanence*: raising is where a window sits now, pinning is where it stays, and
a pinned float must keep its place in the order when another float is raised.
The policy answers both in RENDER-ORDER, so a custom policy can answer them
differently without this vocabulary knowing.")

(def-suite placement :in model
  :description "Where a window sits when you say where: SNAP-RECT (#14) and the
rule that opens a window there.  Pure geometry, so this is the one part of
snapping that needs no compositor at all -- and the parts worth checking are the
awkward ones: an odd-sized screen (a half that is not the half a naive division
gives), the space being the *layout's* space rather than the output's, and a rule
that says :SNAP without saying :FLOAT still getting a float.")

(def-suite outputs :in model
  :description "Monitors (#8).  One stack of workspaces and one viewport per
output means a number already means the same workspace on every screen, so what
was missing was the verb for `this workspace belongs over there' and the report
that says which screen shows what.")

(def-suite hidden :in model
  :description "The hidden set: windows that are alive and on no screen (#6).
No new state -- WM:WORLD-SCRATCHPAD was already the answer three files give.
What was missing was that the set could not be *listed*, and that the only
picker that reached it offered every window of the session to reach the few you
cannot see.")

(def-suite profiles :in model
  :description "Named configuration files you can switch between (#12).  A
profile is a Lisp file in ~/.config/latticewm/profiles/ and loading it is
loading a file -- the same trust init.lisp has, and the reason a profile can
hold a keymap, a rule table and a lambda, which a JSON document could not.  What
saving captures is what DIFFERS from what shipped: a profile that froze every
option would pin a user to the defaults of the release that wrote it.")

(def-suite gestures :in model
  :description "Touch gestures, as a table (#16).  Everything here is the half
that needs no compositor: what a gesture spec is, how the table turns into plans
the runtime applies, how a prompt reads `3 left', and which command a recognised
gesture runs.  The wire half is river_touch_gesture_v1 objects, and it is
covered by the headless-river integration suite -- the split is the same one
P:PREVIEW-PANELS and the preview's drawing have, and for the same reason: the
decisions are in the plan, and the protocol is a consumer of it.")

(def-suite states :in model
  :description "What state a window is in, derived rather than stored (#25).
Two rulings are the whole of it: fullscreen is a second axis (river renders a
fullscreen window alone, so it is still a tile or still a float underneath), and
minimized is exclusive of tree membership but not of floating.  The third thing
here is TRANSITION-WINDOW's contract -- idempotent, one name, and the four verbs
that change a window's state all going through it -- which is what keeps the
invariants in one place instead of in five.")

(def-suite state :in model
  :description "The saved layout, as a file: what a reader sees while it is
being replaced, and what happens to the last copy when it cannot be read.  The
failure mode of both is losing the arrangement rather than reporting it, which
is why the checks stand in the middle of the write instead of after it.")

(defparameter *extension-suites* '()
  "Suites belonging to extensions under extensions/, as (PACKAGE SUITE-NAME).

An extension's tests file registers itself here when it loads, by
REGISTER-EXTENSION-SUITE below, and RUN-ALL runs everything on this list after
the core's own suites.  The lattice is deliberately NOT on it: its suite is
probed by name below because it predates the registry and because gate 4's
argument -- the core runs with the extension absent -- is worth restating in
the one place that would notice if the flagship stopped being optional.  The
promoted examples have no such seniority.")

(defun register-extension-suite (package name)
  "Put (PACKAGE NAME) on *EXTENSION-SUITES*, once.

Called at load time from an extension's tests file.  PUSHNEW rather than PUSH,
because a configuration file gets loaded twice and so does an extension."
  (pushnew (list package name) *extension-suites* :test #'equal))

;;; ------------------------------------------------------- save fixtures
;;;
;;; THE FOURTH COPY IS THE ONE SOMEBODY FORGETS.  Four extension suites each
;;; carried their own BLOCK-THE-REPLACEMENT / UNBLOCK-THE-REPLACEMENT pair --
;;; byte for byte, comment included -- because each had to verify the same
;;; regression (#88's in-place truncating write, met again in #104).  A fix to
;;; the trick meant editing four files in lockstep, and every new extension
;;; that writes a user file had to copy it a fifth time before its test could
;;; say anything (#120).
;;;
;;; It lives in the harness now, next to REGISTER-EXTENSION-SUITE, which is the
;;; same answer to the same question the core already settled.

(defun fresh-temporary-directory (&optional (prefix "lw-test"))
  "A new directory under the system temporary directory, unique per call.

Unique by pid and a random draw because two runs of the suite may overlap, and
a fixed name would have them fighting over one directory.  The name is a
prefix rather than a constant so that a leftover directory says which suite
made it."
  (let ((dir (merge-pathnames (format nil "~a-~d-~d/" prefix
                                      (sb-posix:getpid) (random (expt 2 30)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defun block-the-replacement (file)
  "Make FILE's temporary unopenable, so an atomic save cannot start writing.

The failure being injected is the one an atomic save is built to survive: the
temporary exists to make a write all-or-nothing, so a test of the surviving
half has to stop the temporary from being created at all.  Returns the
pathname that must be handed to UNBLOCK-THE-REPLACEMENT."
  (let* ((temporary (r:temporary-state-path file))
         ;; The namestring plus a slash, because ENSURE-DIRECTORY-PATHNAME
         ;; would re-escape the dot SBCL escapes in the temporary's type.
         (blocked (pathname (concatenate 'string (namestring temporary) "/"))))
    (ensure-directories-exist blocked)
    blocked))

(defun unblock-the-replacement (temporary)
  "Undo BLOCK-THE-REPLACEMENT.  Safe to call twice, and from an unwind."
  (ignore-errors
    (uiop:delete-directory-tree temporary :validate t
                                :if-does-not-exist :ignore)))

(defun wait-until (predicate &key (timeout 30))
  "Poll PREDICATE until it returns true, or TIMEOUT seconds elapse.

A detached child takes a moment to land on a loaded host, and 'a moment' is
a wall-clock fact, not an iteration count: a loop that polls a fixed number
of times gives up after whatever the host actually took, which is exactly the
flake a generous deadline exists to prevent.  PREDICATE is called at least
once, and its final value is returned."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop while (and (not (funcall predicate))
                     (< (get-universal-time) deadline))
          do (sleep 0.05))
    (funcall predicate)))

(defun run-all ()
  "Run every suite that is loaded, and return T when they all pass.

Called by `make test' and by ASDF's TEST-OP.

*Every suite that is loaded*, discovered rather than listed.  The lattice suite
used to be absent from this function and from the command that runs it, so the
flagship extension — 1500 lines, the whole proof that the extension story works
— had no test executed by `make test' and the headline check count said nothing
about it.  A hand-maintained list of suites is exactly the thing that goes
stale, and this is the third place in the project where that has been true."
  (let ((results (append (run 'model)
                         (run-optional-suite "LATTICEWM/TESTS/EXAMPLES" "EXAMPLES")
                         (run-optional-suite "LATTICE/TESTS" "PLANE")
                         (loop for (package suite) in *extension-suites*
                               nconcing (run-optional-suite package suite)))))
    (explain! results)
    (values (results-status results) (length results))))

(defun run-optional-suite (package name)
  "Run the suite NAME from PACKAGE if that package is loaded, else nothing.

Absent is not a failure: gate 4's whole point is that the core runs with the
lattice missing, and a test harness that refused to run without it would be
contradicting the thing it exists to check."
  (let* ((home (find-package package))
         (symbol (and home (find-symbol name home))))
    (if symbol (run symbol) '())))

;;; ------------------------------------------------------------- fixtures

(defun policy ()
  "A fresh conventional policy, so tests never share option state."
  (make-instance 'p:conventional-policy))

(defun leaves (n)
  "N distinct empty leaves, so a test can tell them apart by identity."
  (loop repeat n collect (c:make-leaf)))

(defun win (&optional (app-id "test"))
  "A window object with no proxy — the model never dereferences one."
  (make-instance 'c:window :app-id app-id))

(defun two-output-world (&key (w 1000) (h 800))
  "A world with a monitor left of another, each showing a workspace of its own."
  (let* ((world (c:make-world))
         (left (make-instance 'c:output :name "left"
                              :rect (c:make-rect 0 0 w h)))
         (right (make-instance 'c:output :name "right"
                               :rect (c:make-rect w 0 w h))))
    (setf (c:world-outputs world) (list left right))
    (p:ensure-workspaces-for-outputs world)
    (values world left right)))

(defun three-output-world (&key (w 1000) (h 800))
  "Three monitors in a row, so that a group can grow past two."
  (let* ((world (c:make-world))
         (left (make-instance 'c:output :name "left"
                              :rect (c:make-rect 0 0 w h)))
         (middle (make-instance 'c:output :name "middle"
                                :rect (c:make-rect w 0 w h)))
         (right (make-instance 'c:output :name "right"
                               :rect (c:make-rect (* 2 w) 0 w h))))
    (setf (c:world-outputs world) (list left middle right))
    (p:ensure-workspaces-for-outputs world)
    (values world left middle right)))

(defun leaf-with (app-id)
  "A leaf holding a fresh window tagged APP-ID."
  (c:make-leaf (win app-id)))

(defun byte-vector (length &rest bytes)
  "A glyph table of LENGTH bytes, starting with BYTES and zero after that."
  (let ((v (make-array length :element-type '(unsigned-byte 8)
                              :initial-element 0)))
    (replace v (coerce bytes 'vector))
    v))

(defun app-at (root path)
  "The app-id of the window at PATH under ROOT, or NIL.

Tests assert on this rather than on node identity, because it says what the
user would see."
  (let ((node (c:resolve-path root path)))
    (when (typep node 'c:leaf)
      (let ((w (c:leaf-window node)))
        (when w (c:window-app-id w))))))

(defun shape (node)
  "A readable s-expression of NODE's structure, for whole-tree assertions.

  (:h (:leaf \"a\") (:v (:leaf \"b\") (:leaf nil)))

Comparing shapes catches structural regressions that per-path assertions
miss — a split that should have collapsed and did not, for instance."
  (typecase node
    (c:leaf (list :leaf (let ((w (c:leaf-window node)))
                          (and w (c:window-app-id w)))))
    (c:split (list* (if (eq (c:split-axis node) :horizontal) :h :v)
                    (mapcar #'shape (c:children node))))
    (c:stack (list* :stack (c:stack-selected node)
                    (mapcar #'shape (c:children node))))
    (t (list :node (type-of node)))))
