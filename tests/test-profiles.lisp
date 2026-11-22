;;;; tests/test-profiles.lisp --- Named configurations (#12).
;;;;
;;;; A profile is a file, so every check here is about a file: what a name may
;;;; be, what a save captures, that a load applies it, and that two saves of the
;;;; same session produce the same bytes.  The last one is the same discipline
;;;; `make surface && git diff --exit-code' applies to the generated documents,
;;;; and it is what makes a profile a thing a person can keep in their dotfiles.

(in-package #:latticewm/tests)
(in-suite profiles)

;;; ------------------------------------------------------------------ fixtures

(defmacro with-profiles ((&key (options '())) &body body)
  "Run BODY with the profile directory pointed at a fresh temporary one.

Binds the *directory* rather than a *path*, so the code under test builds paths
the way it does on a real machine and the checks cannot pass by agreeing with a
fixture."
  (let ((dir (gensym "DIR")))
    `(let* ((,dir (fresh-temporary-directory))
            ;; An option in the middle so the suite can save something without
            ;; touching the machine's real configuration.
            ,@(loop for (name value) on options by #'cddr
                    collect `(,name ,value))
            (r:*profiles-directory* (merge-pathnames "profiles/" ,dir)))
       (unwind-protect (progn ,@body)
         (ignore-errors (uiop:delete-directory-tree ,dir :validate t))))))

;;; ------------------------------------------------------------------ the names

(test a-profile-name-stays-in-its-own-directory
  (is-true (r:profile-name-p "work"))
  (is-true (r:profile-name-p "work-2024"))
  (is-true (r:profile-name-p "gaming.night"))
  (is-false (r:profile-name-p "../../.ssh/authorized_keys")
            "a name is used to build a path, so a name with a slash in it is a
write to a file nobody asked to write")
  (is-false (r:profile-name-p ".bashrc")
            "and a leading dot is a file this program has no business touching")
  (is-false (r:profile-name-p ""))
  (is-false (r:profile-name-p nil)))

(test a-name-that-is-not-a-name-signals-rather-than-being-corrected
  (signals error (r:profile-path "../escape"))
  (signals error (r:profile-path "")))

;;; ------------------------------------------------------------------ the files

(test saving-a-profile-writes-what-differs-and-not-what-shipped
  (with-profiles (:options (p:*gaps* 7))
    (let ((path (r:write-profile "work"))
          (text nil))
      (is (probe-file path) "the file exists")
      (setf text (uiop:read-file-string path))
      (is (search "(setf (p:option :gaps) '7)" text)
          "the option that was changed is written")
      (is (not (search "(setf (p:option :border-width)" text))
          "and the hundreds that were not are not -- a profile that froze every
value would pin you to the defaults of the release that wrote it")
      (is (search "(in-package #:latticewm/user)" text)
          "so the file reads as a configuration file, which is what it is"))))

(test saving-twice-writes-the-same-bytes
  "The discipline `make surface && git diff --exit-code doc/' applies, applied
to a user file: two saves of the same session must produce the same file, or a
profile in a dotfiles repository is a diff that says nothing every time."
  (with-profiles (:options (p:*gaps* 9))
    (flet ((body (path)
             ;; Everything from IN-PACKAGE on: the header names the file, which
             ;; is the one thing that *should* differ between two saves.
             (let ((text (uiop:read-file-string path)))
               (subseq text (or (search "(in-package" text) 0)))))
      (is (equal (body (r:write-profile "one"))
                 (body (r:write-profile "two")))
          "the same session saved under two names is the same settings"))))

(test a-profile-carries-its-key-bindings
  "Keys are the thing people most want in a profile, and they read back: a
profile that saved options and silently dropped the keymap would be the wrong
half of the feature."
  (with-profiles ()
    (let ((p:*keymap* (p:make-keymap :name "profile-test")))
      (p:define-key p:*keymap* "Super+F1" '("close-window"))
      (let ((text (uiop:read-file-string (r:write-profile "keys"))))
        (is (search "Super+F1" text) "the binding is in the file")
        (is (search "close-window" text) "with the command it runs")))))

(test a-chord-is-named-and-not-reproduced
  (with-profiles ()
    (let ((p:*keymap* (p:make-keymap :name "chord-test")))
      (p:define-key p:*keymap* "Super+x"
                    (p:make-keymap :name "submap"))
      (let ((text (uiop:read-file-string (r:write-profile "chord"))))
        (is (search "chord Super+x" text)
            "a submap is a named object no config file has ever referred to, so
it is reported rather than rebuilt with an invented name")
        (is (search "not captured" text) "and it says so in the file")))))

;;; ---------------------------------------------------------------- the loading

(test loading-a-profile-applies-it-to-the-running-session
  (with-profiles ()
    (let ((path (r:profile-path "loud")))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (format out "(in-package #:latticewm/user)~%(setf (p:option :gaps) 21)~%"))
      (let ((p:*gaps* 0))
        (r:load-profile "loud")
        (is (= 21 p:*gaps*) "the file ran, which is what loading a file means")
        (is (equal "loud" r:*current-profile*)
            "and the session says which profile it is in")))))

(test loading-a-profile-that-is-not-there-says-which-ones-are
  (with-profiles ()
    (let ((why (handler-case (progn (r:load-profile "missing") nil)
                 (error (condition) (princ-to-string condition)))))
      (is-true why)
      (is (search "missing" why) "the name that failed")
      (is (search "profiles" why) "and where profiles would have been"))))

;;; ---------------------------------------------------------------- the verbs

(test the-profile-commands-exist-and-are-only-ever-one-name
  (is-true (p:find-command "profiles"))
  (is-true (p:find-command "profile-load"))
  (is-true (p:find-command "profile-save"))
  (is-true (p:find-command "profile-delete"))
  (is-true (p:find-command "profile-current")))

(test deleting-a-profile-takes-the-file-and-nothing-else
  (with-profiles ()
    (let ((path (r:write-profile "gone")))
      (is-true (r:delete-profile "gone"))
      (is-false (probe-file path))
      (is-false (r:delete-profile "gone")
                "deleting what is not there is a no-op, not an error"))))

(test every-changed-option-is-listed-once-in-name-order
  (let ((changes (mapcar #'car (r:option-changes))))
    (is (equal changes (sort (copy-list changes)
                             #'string< :key #'symbol-name))
        "sorted, because a file whose order depends on a hash table produces a
diff that says nothing every time")
    (is (notany (lambda (key) (> (count key changes) 1)) changes)
        "and once each")))
