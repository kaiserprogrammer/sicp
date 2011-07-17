(defparameter tolerance 0.00001)

(defun my-sqrt (x)
  (make-guess x 1))

(defun make-guess (x guess)
  (cond
    ((good-enough? x (square guess)) guess)
    (t (make-guess x (average guess (/ x guess))))))

(defun square (x)
  (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (x y)
  (< (abs (- x y)) tolerance))
