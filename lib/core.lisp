
;; Core functions that are helpful

(label list (lambda xs xs))

(label nil? (lambda (x) (= x ())))

(label caar (lambda (xs) (car (car xs))))
(label cadr (lambda (xs) (car (cdr xs))))
(label cdar (lambda (xs) (cdr (car xs))))
(label cddr (lambda (xs) (cdr (cdr xs))))


(label true (lambda (x y) x))
(label false (lambda (x y) y))

(label not (lambda (x) (x false true)))
(label and (lambda (x y) (x y x)))
(label or (lambda (x y) (x x y)))


(label id (lambda (x) x))
(label const (lambda (x) (lambda (y) x)))


(label map (lambda map (f xs)
             ((nil? xs)
              ()
              (cons (f (car xs)) (map f (cdr xs))))))


(label reduce (lambda reduce (f x xs)
                ((nil? xs)
                 x
                 (reduce f (f x (car xs)) (cdr xs)))))


(label last (lambda (xs) (reduce (lambda (x y) y) () xs)))
