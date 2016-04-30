
;; Core functions that are helpful

(label list (lambda xs xs))

(label defun (macro (name args body)
                    (list 'label name (list 'lambda args body))))


(label defmacro (macro (name args body)
                       (list 'label name (list 'macro args body))))

(defun caar (xs) (car (car xs)))
(defun cadr (xs) (car (cdr xs)))
(defun cdar (xs) (cdr (car xs)))
(defun cddr (xs) (cdr (cdr xs)))


(defun not (x) (if x () 't))
(defun and (x y) (if x y x))
(defun or (x y) (if x x y))

;; Macro versions of and and or
(defmacro mand (x y) (list 'if x y x))
(defmacro mor (x y) (list 'if x x y))


(defun nil? (x) (= x ()))
(defun cons? (x) (not (atom? x)))
(defun single? (x) (mand (cons? x)
                         (atom? (cdr x))))


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


(defun last (xs) (reduce (lambda (x y) y) () xs))


(defmacro let (binds body)
  (if (nil? binds)
      body
      (list (list 'lambda (list (car binds))
                  (list 'let (cddr binds) body))
            (cadr binds))))


(defmacro cond xs
  (if (nil? xs)
      ()
      (list 'if (car xs)
            (cadr xs)
            (cons 'cond (cddr xs)))))
