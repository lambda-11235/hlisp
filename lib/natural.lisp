;; Implement natural numbers, which have the forms
;;
;; n := 0
;;    | (inc n)

(label 0 '0)

(defun pred (x) (cond (atom? x) x
                      't (if (single? (cdr x))
                             (cadr x)
                           (cdr x))))

(defun succ (x) (list 'inc x))

(defun succ? (x) (not (atom? x)))

(label 1 (succ 0))
(label 2 (succ 1))
(label 3 (succ 2))
(label 4 (succ 3))
(label 5 (succ 4))
(label 6 (succ 5))
(label 7 (succ 6))
(label 8 (succ 7))
(label 9 (succ 8))


(defun + (x y) (cond (= x 0) y
                     't (+ (pred x) (succ y))))

(defun * (x y) (cond (= x 0) 0
                     (= x 1) y
                     't (+ (* (pred x) y) y)))


;; abs-diff(x, y) = |x - y|
(defun abs-diff (x y) (cond (= x 0) y
                            (= y 0) x
                            't (abs-diff (pred x) (pred y))))
