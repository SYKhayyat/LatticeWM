;;;; thumbnails.asd --- Pictures of windows in the deck, off by default.

(defsystem "thumbnails"
  :description "Fill the deck's cells with pictures of the windows in them (#130)."
  :long-description
  "This program draws only what it draws, so the deck has always shown boxes and
names.  #135 added the protocol that hands over a window's pixels; this module is
the drawing half, and it is off until you ask for it.

    (load-extension \"thumbnails\")   ; nothing changes on screen
    (thumbnails on)                  ; M-x thumbnails on
    (preview)                        ; the deck, now with pictures
    (thumbnails off)                 ; back to boxes, and nothing captured

One picture per window per look: taking them per frame would be a video feed
wearing a deck's clothes.  M-X THUMBNAILS-FORGET throws the cache away early."
  :author "Shaul Khayyat"
  :mailto "shaul.khayyat@cloudresearch.com"
  :homepage "https://github.com/SYKhayyat/LatticeWM"
  :source-control (:git "https://github.com/SYKhayyat/LatticeWM.git")
  :bug-tracker "https://github.com/SYKhayyat/LatticeWM/issues"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on ("latticewm")
  :serial t
  :components
  ((:file "package")
   (:file "thumbnails")))

(defsystem "thumbnails/tests"
  :description "Tests for the thumbnails extension."
  :depends-on ("thumbnails" "latticewm/tests" "fiveam")
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components ((:file "test-thumbnails"))))
  :perform (test-op (o c)
             (symbol-call :latticewm/tests :run-all)))
