;;;; tabs/package.lisp

(defpackage #:tabs
  (:use #:cl)
  (:local-nicknames (#:c #:latticewm/core)
                    (#:p #:latticewm/policy)
                    (#:r #:latticewm/runtime))
  (:documentation
   "A tab group is a pane that holds several windows and shows one at a
time.

    (tab-here)                 ; the focused pane becomes a group
    (tab-add \"foot\")          ; pull a live window in
    (tab-next) / (tab-prev)    ; cycle
    (tab-move :forward)        ; reorder the visible tab
    (tab-move-to 1)            ; put it first
    (tab-rename \"mail\")        ; call it something
    (tab-send \"firefox\")       ; push it into that group
    (untab)                    ; pop the visible one back out

Hidden members are simply unplaced, which the runtime already renders as
invisible.  Switching is not an undo step by default.")
  (:export #:*undo-includes-tab-switches*
           #:enable #:disable #:enabled-p
           #:tab-here #:tab-add #:tab-next #:tab-prev #:untab
           ;; #36: the lifecycle the first five left out.
           #:tab-move #:tab-move-to #:tab-rename #:tab-send
           #:a-group-with-order #:leaf-holding-tab
           #:group-of
           #:visible-window))
