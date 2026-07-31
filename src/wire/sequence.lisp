;;;; wire/sequence.lisp --- The manage/render discipline.
;;;;
;;;; River's most dangerous rule, and the one easiest to get wrong, because
;;;; some requests are legal in both contexts and the compositor's punishment
;;;; for a mistake is to kill the connection.
;;;;
;;;;   Window-management state — dimensions, fullscreen, keyboard focus,
;;;;   keybindings — may only be modified during a MANAGE sequence.
;;;;
;;;;   Rendering state — position, render order, hide/show, borders, clip
;;;;   boxes — may be modified during a manage *or* a render sequence.
;;;;
;;;; Violating it signals river_window_manager_v1.error.sequence_order, which
;;;; is fatal.
;;;;
;;;; A compiled language would put this in the type system.  The dynamic
;;;; equivalent is *better here*, and not as a consolation prize: it catches
;;;; the violation at the point of use, before the bytes reach the wire, *and
;;;; offers a restart*, so a mistake during live development is a question
;;;; rather than a dead session.
;;;;
;;;; THE SPEC CONTRADICTS ITSELF HERE, and it matters.  The
;;;; river_window_manager_v1 interface description says rendering state "may be
;;;; modified by the window manager during a manage sequence or a render
;;;; sequence".  But the individual rendering-state requests each say they "may
;;;; only be made as part of a render sequence" — pointing back at the
;;;; description that contradicts them.  *RENDER-LEGAL-IN-MANAGE* isolates that
;;;; single disputed clause so it can be flipped by one assignment once the
;;;; question is settled against the real compositor.  It matters because doing
;;;; position updates inside the manage sequence halves the round trips.

(in-package #:latticewm/wire)

(defvar *sequence* nil
  "The protocol sequence currently in progress: NIL, :MANAGE or :RENDER.

Bound by WITH-MANAGE-SEQUENCE and WITH-RENDER-SEQUENCE.  Consulted by every
wrapped request before it marshals anything.")

(defvar *render-legal-in-manage* t
  "Whether rendering-state requests are accepted inside a manage sequence.

The spec says both yes and no in different places; see the header.  If it
resolves against us, set this to NIL — that is the whole change.")

(defvar *enforce-sequence* t
  "Whether to check the sequence discipline at all.

Setting this to NIL turns every wrapper into a direct call.  Provided for the
case where the check itself is suspected of being wrong; not for performance,
which is not a consideration at a few hundred messages per relayout.")

(define-condition sequence-violation (error)
  ((request :initarg :request :reader sequence-violation-request)
   (required :initarg :required :reader sequence-violation-required)
   (actual :initarg :actual :reader sequence-violation-actual))
  (:report
   (lambda (condition stream)
     (format stream
             "~s may only be sent during a ~(~a~) sequence, but ~
              ~:[no sequence is in progress~;~:*a ~(~a~) sequence is~]."
             (sequence-violation-request condition)
             (sequence-violation-required condition)
             (sequence-violation-actual condition))))
  (:documentation
   "Signalled when a request is about to be sent in the wrong protocol context.

Continuable: the \"Send it anyway\" restart marshals the request regardless,
which is what you want while establishing whether the spec's
self-contradiction resolves for or against us."))

(defun check-sequence (required request)
  "Signal unless the current sequence permits REQUEST.

REQUIRED is :MANAGE or :RENDER.  Manage-only requests demand a manage
sequence.  Rendering requests are accepted in a render sequence always, and in
a manage sequence when *RENDER-LEGAL-IN-MANAGE*."
  (when *enforce-sequence*
    (unless (or (eq *sequence* required)
                (and (eq required :render)
                     (eq *sequence* :manage)
                     *render-legal-in-manage*))
      (cerror "Send it anyway."
              'sequence-violation :request request
                                  :required required :actual *sequence*)))
  t)

(defmacro with-manage-sequence ((wm) &body body)
  "Run BODY inside a manage sequence on WM, finishing it even if BODY unwinds.

A manage sequence is *always* followed by at least one render sequence, and
the compositor waits for it before processing further input — river has an
`unresponsive' protocol error and will use it.  So the UNWIND-PROTECT is not
tidiness: failing to send manage_finish because a policy method signalled is
how you hang the whole desktop."
  (let ((manager (gensym "WM")))
    `(let ((,manager ,wm))
       (let ((*sequence* :manage))
         (unwind-protect (progn ,@body)
           (river:river-window-manager-v1.manage-finish ,manager))))))

(defmacro with-render-sequence ((wm) &body body)
  "Run BODY inside a render sequence on WM, finishing it even if BODY unwinds."
  (let ((manager (gensym "WM")))
    `(let ((,manager ,wm))
       (let ((*sequence* :render))
         (unwind-protect (progn ,@body)
           (river:river-window-manager-v1.render-finish ,manager))))))
