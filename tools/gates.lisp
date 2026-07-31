;;;; tools/gates.lisp --- PLAN.org's six build gates.
;;;;
;;;; "All six run on every commit from day one.  They are cheap and they are
;;;; the only automated defence the project has."
;;;;
;;;; Gate 1 lives in tools/build.lisp because it has to run *during* the load.
;;;; The other five run here, against the loaded image.

(require :asdf)
(require :sb-introspect)

(defparameter *failures* '())

(defun fail (gate format &rest arguments)
  (push (format nil "gate ~a: ~?" gate format arguments) *failures*))

(defun banner (gate title)
  (format t "~&~%---- gate ~a: ~a ~a~%" gate title
          (make-string (max 0 (- 50 (length title))) :initial-element #\-)))

(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "latticewm"))

(defmacro sym (name) `(read-from-string ,name))
(defun call (name &rest arguments) (apply (read-from-string name) arguments))

;;; ---------------------------------------------------------------- gate 2

(banner 2 "every policy generic and command is documented")
(let ((generics (call "latticewm/policy:undocumented-generics"))
      (commands (call "latticewm/runtime::undocumented-commands")))
  (dolist (g generics) (format t "  UNDOCUMENTED <-- flag me: ~(~a~)~%" g))
  (dolist (c commands)
    (format t "  UNDOCUMENTED <-- flag me: command ~a~%"
            (call "latticewm/runtime:command-name" c)))
  (if (or generics commands)
      (fail 2 "~d undocumented generic~:p, ~d undocumented command~:p"
            (length generics) (length commands))
      (format t "  all ~d generics and ~d commands documented~%"
              (length (call "latticewm/policy:policy-generics"))
              (length (call "latticewm/runtime:all-commands")))))

;;; ---------------------------------------------------------------- gate 3

(banner 3 "the lattice touches no core")
(if (probe-file "lattice.asd")
    (let* ((files (directory "lattice/*.lisp"))
           (lines (reduce #'+ files :key
                          (lambda (path)
                            (with-open-file (in path)
                              (loop for line = (read-line in nil) while line
                                    count t)))))
           (methods (reduce #'+ files :key
                            (lambda (path)
                              (with-open-file (in path)
                                (loop for line = (read-line in nil) while line
                                      count (search "(defmethod " line)))))))
      ;; Gate 3 is satisfiable in bad faith — push everything through PROPS and
      ;; &rest args and the letter passes while the spirit dies — so PLAN.org
      ;; requires it to report two numbers alongside pass/fail.  A lattice that
      ;; is three hundred lines of methods is the thesis holding; one that is
      ;; two thousand lines re-implementing geometry is the thesis failing
      ;; while wearing the gate's uniform.
      (format t "  lattice/: ~d lines, ~d defmethods~%" lines methods)
      (format t "  (the numbers are the gate; a large lattice re-implementing~%~
                 \   geometry passes the letter and fails the point)~%"))
    (format t "  lattice not built yet~%"))

;;; ---------------------------------------------------------------- gate 4

(banner 4 "the core runs without the lattice")
(let ((world (call "latticewm/core:make-world"))
      (policy (make-instance (sym "latticewm/policy:conventional-policy"))))
  (handler-case
      (progn
        (call "latticewm/policy:on-window-open" policy world
              (make-instance (sym "latticewm/core:window") :app-id "gate"))
        (call "latticewm/policy:on-window-open" policy world
              (make-instance (sym "latticewm/core:window") :app-id "gate2"))
        (let ((placements (call "latticewm/policy:layout-node" policy
                                (call "latticewm/core:world-root" world)
                                (call "latticewm/core:make-rect" 0 0 1920 1080))))
          (format t "  placed 2 windows, laid out ~d node~:p, no lattice loaded~%"
                  (length placements))))
    (error (condition) (fail 4 "~a" condition))))

;;; ---------------------------------------------------------------- gate 5

(banner 5 "codegen counts match the pinned XML")
(let ((expected '(("river-window-management-v1" 8 57 42 10)
                  ("river-xkb-bindings-v1" 3 11 5 1)
                  ("river-layer-shell-v1" 3 6 4 1)))
      (total-requests 0))
  (dolist (row expected)
    (destructuring-bind (name interfaces requests events enums) row
      (let ((path (format nil "src/protocol/~a.xml" name))
            (counts (list 0 0 0 0)))
        (with-open-file (in path)
          (loop for line = (read-line in nil) while line
                do (when (search "<interface " line) (incf (first counts)))
                   (when (search "<request " line) (incf (second counts)))
                   (when (search "<event " line) (incf (third counts)))
                   (when (search "<enum " line) (incf (fourth counts)))))
        (incf total-requests (second counts))
        (if (equal counts (list interfaces requests events enums))
            (format t "  ~a~40t~{~d~^/~}~%" name counts)
            (fail 5 "~a is ~{~d~^/~}, pinned as ~{~d~^/~}"
                  name counts (list interfaces requests events enums))))))
  ;; And every one of them has a checked wrapper, so a protocol change cannot
  ;; leave a request unguarded until the day somebody first calls it.
  (let ((wrapped (length (call "latticewm/wire:all-wrapped-requests"))))
    (format t "  wrapped requests~40t~d~%" wrapped)
    (when (< wrapped total-requests)
      (fail 5 "~d requests in the XML but only ~d wrappers"
            total-requests wrapped))))

;;; ---------------------------------------------------------------- gate 6

(banner 6 "the runtime-to-policy line ratio")
(flet ((count-lines (pattern &key (skip '()))
         ;; Generated files are excluded and named.  The ratio is meant to
         ;; measure *authored* runtime against authored policy; a vendored font
         ;; table is neither, and counting 95 lines of hex as runtime would make
         ;; the number say something it does not mean.  Excluding it is only
         ;; honest if the exclusion is visible, so it is printed.
         (reduce #'+ (remove-if (lambda (path)
                                  (member (file-namestring path) skip
                                          :test #'string=))
                                (directory pattern))
                 :key
                 (lambda (path)
                   (with-open-file (in path)
                     (loop for line = (read-line in nil) while line
                           unless (or (zerop (length (string-trim " " line)))
                                      (char= #\; (char (string-left-trim " " line) 0)))
                             count t))))))
  (let* ((runtime (+ (count-lines "src/wire/*.lisp")
                     (count-lines "src/runtime/*.lisp" :skip '("font.lisp"))))
         (policy (+ (count-lines "src/model/*.lisp")
                    (count-lines "src/policy/*.lisp")
                    (count-lines "lattice/*.lisp")))
         (ratio (if (plusp runtime) (/ (float policy) runtime) 0)))
    ;; PLAN.org §extensibility-real: Lisp is not what kept Emacs alive, the
    ;; *ratio* is — 1.3 million lines of Elisp on 400,000 of C, so every
    ;; feature is a worked example of how to write a feature.  Vim, Neovim and
    ;; Hyprland all have a scripting language and are not Emacs; the boundary
    ;; is not the disease, how little of the system lives above it is.
    ;;
    ;; Not pass/fail.  A number, printed where it cannot be ignored.  Runtime
    ;; growing faster than policy is the earliest visible symptom of the
    ;; monolith failure mode, and it shows up weeks before anything else does.
    (format t "  runtime (wire + runtime)~40t~d lines  (font.lisp excluded: generated)~%"
            runtime)
    (format t "  policy  (model + policy + lattice)~40t~d lines~%" policy)
    (format t "  ratio~40t~,2f~a~%" ratio
            (cond ((>= ratio 2.0) "   Emacs-shaped")
                  ((>= ratio 1.0) "   healthy")
                  (t "   <-- watch this: runtime is outgrowing policy")))))

;;; ---------------------------------------------------------------- verdict

(format t "~&~%~76,,,'=<~>~%")
(if *failures*
    (progn (dolist (f (reverse *failures*)) (format t "FAIL ~a~%" f))
           (format t "~d gate failure~:p~%" (length *failures*))
           (sb-ext:quit :unix-status 1))
    (format t "ALL GATES PASS~%"))
