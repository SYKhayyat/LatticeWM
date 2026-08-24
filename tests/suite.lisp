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
           #:container #:hooks #:minibuffer #:devices #:capture #:boundaries
           #:pixels #:versions
           #:*extension-suites* #:register-extension-suite))

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
