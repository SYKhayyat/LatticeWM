;;;; examples/02-window-rules.lisp
;;;;
;;;; TIER 1 — a table plus one method.  The extension everybody writes first.
;;;;
;;;; The point of this file is that a *declarative* window-rule system does not
;;;; need to be a feature.  It is a table and a method, and because it is
;;;; ordinary Lisp the "rule language" is Lisp — so a predicate that needs to
;;;; consult the time of day, or the number of windows already open, or a file,
;;;; just does.
;;;;
;;;; That is the difference the whole project is arguing for.  A window manager
;;;; with a rules DSL gives you exactly the predicates its author thought of.
;;;;
;;;; THE TABLE SURVIVED THE PROMOTION; THE TWO METHODS DID NOT.  They are in
;;;; extensions/window-rules, with :AROUND-style switches so DISABLE gives the
;;;; shipped policy back untouched.  What a configuration file writes is the
;;;; table, which is the part that is yours: a rule *list* is data, and the
;;;; program that consults it is one module rather than one per user.
;;;;
;;;; Note what the table now says: matches and overrides, nothing else.  The
;;;; moment a rule list also had to carry "and here is how to install me", every
;;;; user's rules file would be a copy of the same twenty lines of method.

(in-package #:latticewm/user)

(load-extension "window-rules")
(window-rules:enable)

(setf window-rules:*rules*
      ;; Backquoted, not quoted, so a match can be a function rather than a
      ;; string.
      `(;; (MATCH . OVERRIDES)
        ;;
        ;; MATCH is an app-id string, or a function of the window.
        ;; OVERRIDES is a plist: :FLOAT, :WORKSPACE, :FOCUS.
        ("pavucontrol"          :float t)
        ("blueman-manager"      :float t)
        ("org.gnome.Calculator" :float t)
        ("firefox"              :workspace 1)
        ("thunderbird"          :workspace 2 :focus nil)
        ;; A function match: anything that announced itself as a fixed-size
        ;; dialog and is small.  The shipped SHOULD-FLOAT-P already floats
        ;; fixed-size windows; this is here to show that a predicate is just a
        ;; predicate -- it may look at the window, at the world, at the clock.
        (,(lambda (win)
            (multiple-value-bind (width height) (window-preferred-size win)
              (and width height (< (* width height) (* 400 300)))))
         :float t)))

;;; Most specific first; the first match wins.  An unmatched window is placed
;;; exactly as the shipped policy would place it, so this composes with the
;;; built-in *window-rules* table rather than shadowing it.
;;;
;;; To go back:  (window-rules:disable)   -- the table is left alone.
