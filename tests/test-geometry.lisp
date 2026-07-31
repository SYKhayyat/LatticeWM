;;;; tests/test-geometry.lisp --- Rectangles and directions.

(in-package #:latticewm/tests)
(in-suite geometry)

(test rect-basics
  (let ((r (c:make-rect 10 20 100 50)))
    (is (= 110 (c:rect-right r)))
    (is (= 70 (c:rect-bottom r)))
    (is-false (c:rect-empty-p r))
    (is-true (c:rect-contains-p r 10 20))
    (is-false (c:rect-contains-p r 110 20) "the right edge is exclusive")
    (is-true (c:rect-empty-p (c:make-rect 0 0 0 10)))))

(test rect-intersect
  (let ((a (c:make-rect 0 0 100 100))
        (b (c:make-rect 50 50 100 100)))
    (is (c:rect-equal (c:make-rect 50 50 50 50) (c:rect-intersect a b)))
    (is-false (c:rect-intersect a (c:make-rect 200 200 10 10)))
    (is-false (c:rect-intersect a (c:make-rect 100 0 10 10))
              "touching edges do not intersect")))

(test rect-inset-never-goes-negative
  (let ((r (c:rect-inset (c:make-rect 0 0 10 10) 20)))
    (is (= 0 (c:rect-w r)))
    (is (= 0 (c:rect-h r)))))

(test divide-rect-tiles-exactly
  ;; The load-bearing property: pieces must cover the parent with no gap and no
  ;; overlap, at every weighting, or a tree visibly frays after a few splits.
  (dolist (weights '((1 1) (1 2) (1 1 1) (3 1 4 1 5) (7) (1 1 1 1 1 1 1)))
    (dolist (extent '(100 101 999 1920 1))
      (let* ((r (c:make-rect 0 0 extent 50))
             (pieces (c:divide-rect r :horizontal weights)))
        (is (= (length weights) (length pieces)))
        (is (= 0 (c:rect-x (first pieces)))
            "first piece starts at the parent's edge")
        (is (= extent (c:rect-right (car (last pieces))))
            "last piece ends at the parent's edge, weights=~s extent=~d"
            weights extent)
        (loop for (a b) on pieces while b
              do (is (= (c:rect-right a) (c:rect-x b))
                     "no seam between adjacent pieces"))))))

(test divide-rect-respects-proportions
  (let ((pieces (c:divide-rect (c:make-rect 0 0 300 10) :horizontal '(1 2))))
    (is (= 100 (c:rect-w (first pieces))))
    (is (= 200 (c:rect-w (second pieces))))))

(test divide-rect-gaps-come-out-of-the-children
  (let ((pieces (c:divide-rect (c:make-rect 0 0 100 10) :horizontal '(1 1)
                               :gap 10)))
    (is (= 45 (c:rect-w (first pieces))))
    (is (= 45 (c:rect-w (second pieces))))
    (is (= 55 (c:rect-x (second pieces))) "the gap sits between them")))

(test divide-rect-vertical
  (let ((pieces (c:divide-rect (c:make-rect 5 7 40 100) :vertical '(1 1))))
    (is (= 5 (c:rect-x (first pieces))) "x is untouched on a vertical cut")
    (is (= 40 (c:rect-w (first pieces))))
    (is (= 7 (c:rect-y (first pieces))))
    (is (= 57 (c:rect-y (second pieces))))))

(test divide-rect-degenerate
  (is (null (c:divide-rect (c:make-rect 0 0 10 10) :horizontal '())))
  (is (= 1 (length (c:divide-rect (c:make-rect 0 0 10 10) :horizontal '(0))))
      "a zero weight still gets a piece rather than vanishing"))

(test directions
  (is (eq :horizontal (c:direction-axis :left)))
  (is (eq :vertical (c:direction-axis :down)))
  (is (= -1 (c:direction-sign :up)))
  (is (= 1 (c:direction-sign :right)))
  (dolist (d c:+directions+)
    (is (eq d (c:opposite-direction (c:opposite-direction d)))
        "opposite is an involution")
    (is (eq d (c:direction-for (c:direction-axis d) (c:direction-sign d)))
        "axis and sign round-trip back to the direction")))
