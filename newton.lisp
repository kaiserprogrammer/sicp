;;; to find a y such that of f(y) = 0
;;; start with a guess, y0
;;; yn+1 = yn - (f(yn)/(df/dy||y=yn))

(defun newton-sqrt (x)
  (newton (lambda (y) (- x (square y)))
          1))

(defun newton (f guess)
  (let ((df (deriv f)))
    (fix-point-helper
     (lambda (x) (- x (/ (funcall f x) (funcall df x))))
     guess)))

(defparameter dx 0.000001)

(defun deriv (f)
  (lambda (x)
    (/ (- (funcall f (+ x dx))
          (funcall f x))
       dx)))

(defun fix-point (f)
  (fix-point-helper f 1))

(defun fix-point-helper (f y)
  (cond
    ((good-enough? y (funcall f y)) y)
    (t (fix-point-helper f (funcall f y)))))

(defun good-enough? (x y)
  (< (abs (- x y)) tolerance))

(defparameter tolerance 0.000001)

(defun square (x)
  (* x x))

(defun cube (x)
  (* x x x))

(defun cube-root (x)
  (newton (lambda (y) (- x (cube y)))
          1))
