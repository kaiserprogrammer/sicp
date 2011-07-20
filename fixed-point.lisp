
(defparameter tolerance 0.00001)

(defun square (x)
  (* x x))

(defun average (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defun good-enough? (x y)
  (< (abs (- x y)) tolerance))

;;; fix-point: function -> value
;;; find the value of applying the same function over and over to the result
(defun fix-point (f)
  (fix-point-helper f 1))

(defun fix-point-helper (f y)
  (cond
    ((good-enough? y (funcall f y)) y)
    (t (fix-point-helper f (funcall f y)))))

;;; sqrt-fix: number -> function
(defun sqrt-fix (x)
  (lambda (y)
    (average y (/ x y))))

(defun fix-sqrt (x)
  (fix-point (sqrt-fix x)))

;;; sqrt with you using average-damp

(defun fix-average-damp-sqrt (x)
  (fix-point
   (average-damp (lambda (y) (/ x y)))))

(defun average-damp (f)
  (lambda (x)
    (average (funcall f x) x)))
