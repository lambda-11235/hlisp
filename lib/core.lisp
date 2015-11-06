
;; Core functions that are helpful

;; Forms a list from its arguments.
;; (list (a b c ...))
(label list (macro (xs)
                   (if (= xs ())
                     ()
                     (cons 'cons
                           (cons (car xs)
                                 (cons (cons 'list (cons (cdr xs) ()))
                                       ()))))))


(label defun (macro (name args body)
                    (list ('label name
                           (list ('lambda args body))))))


(label defmacro (macro (name args body)
                    (list ('label name
                           (list ('macro args body))))))


(defun not (x) (if x () 't))
(defun and (x y) (if x y x))
(defun or (x y) (if x x y))

(defun nil? (x) (= x ()))
(defun cons? (x) (not (atom? x)))

(defun id (x) x)
(defun const (x) (lambda (y) x))

(defun map (f xs)
  (if (nil? xs)
    ()
    (cons (f (car xs)) (map f (cdr xs)))))

(defun reduce (f x xs)
  (if (nil? xs)
    x
    (reduce f (f x (car xs)) (cdr xs))))
