;; Implement natural numbers, which are represented as a list containing n
;; copies of (). So 4 would be (() () () ()).

(label 0 ())

(label succ (lambda (x) (cons () x)))
(label succ? (lambda (x) (not (= x 0))))

(label pred (lambda (x) ((succ? x) (cdr x) x)))

(label 1 (succ 0))
(label 2 (succ 1))
(label 3 (succ 2))
(label 4 (succ 3))
(label 5 (succ 4))
(label 6 (succ 5))
(label 7 (succ 6))
(label 8 (succ 7))
(label 9 (succ 8))
(label 10 (succ 9))


(label + (lambda + (x y)
           ((= x 0) y (+ (pred x) (succ y)))))

(label * (lambda * (x y)
           (cond (= x 0) 0
                 (= x 1) y
                 (+ (* (pred x) y) y))))


(label < (lambda < (x y)
           (cond (= y 0) false
                 (= x 0) true
                 (< (pred x) (pred y)))))
(label > (lambda (x y) (< y x)))
(label <= (lambda (x y) (or (= x y) (< x y))))
(label >= (lambda (x y) (or (= x y) (> x y))))


;; abs-diff(x, y) = |x - y|
(label abs-diff (lambda abs-diff (x y)
                  (cond (= x 0) y
                        (= y 0) x
                        (abs-diff (pred x) (pred y)))))


(label fact (lambda fact (x)
              ((= x 0) 1 (* x (fact (pred x))))))


;; (fromDigs 1 2 0) = 120
(label fromDigs-0 (lambda fromDigs-0 (n ds)
                    ((nil? ds) n (fromDigs-0 (+ (* 10 n) (car ds)) (cdr ds)))))

(label fromDigs (lambda ds (fromDigs-0 0 ds)))
