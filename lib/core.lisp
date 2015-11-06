
;; Core functions that are helpful

(label not (lambda (x) (if x () 't)))
(label and (lambda (x y) (if x y x)))
(label or (lambda (x y) (if x x y)))

(label nil? (lambda (x) (= x ())))
(label cons? (lambda (x) (not (atom? x))))

(label id (lambda (x) x))
(label const (lambda (x) (lambda (y) x)))

(label map (lambda (f xs)
             (if (nil? xs)
               ()
               (cons (f (car xs)) (map f (cdr xs))))))
