;; Implement integer, which have the forms
;;
;; i := 0
;;    | (dec i)
;;    | (inc i)

(label 0 '0)

(defun pred (x) (if (atom? x)
                  (cons 'dec (cons x ()))
                  (if (= (car x) 'inc)
                    (cdr x)
                    (cons 'dec x))))

(defun succ (x) (if (atom? x)
                  (cons 'inc (cons x ()))
                  (if (= (car x) 'dec)
                    (cdr x)
                    (cons 'inc x))))
