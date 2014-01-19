(defun sum (term a next b)
  (labels ((sum-acc (term a next b acc)
             (if (> a b)
                 acc
                 (sum-acc term
                          (funcall next a)
                          next
                          b
                          (+ acc (funcall term a))))))
    (sum-acc term a next b 0)))

(defun sum-int (a b)
  (sum #'identity a #'1+ b))

(defun sum-sq (a b)
  (sum (lambda (x) (* x x))
       a
       #'1+
       b))

(defun pi-sum (a b)
  (sum (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

(defun my-sqrt (x)
  (fixed-point
   (lambda (y) (average (/ x y) y))
   1))

(defun fixed-point (f start)
  (labels ((iter (old new)
             (if (close-enoughp old new)
                 new
                 (iter new (funcall f new))))
           (close-enoughp (a b)
             (< (abs (- a b)) 0.000000001)))
    (iter start (funcall f start))))

(defun average (a b)
  (/ (+ a b) 2))

(defun damp-sqrt (x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1))

(defun average-damp (f)
  (lambda (y)
    (average y (funcall f y))))


(defun newton-sqrt (x)
  (newton (lambda (y) (- x (* y y)))
          1))

(defun newton (f guess)
  (let ((df (deriv f)))
    (fixed-point
     (lambda (x) (- x (/ (funcall f x)
                    (funcall df x))))
     guess)))

(defparameter *dx* (float 1/100000))

(defun deriv (f)
  (lambda (x)
    (/ (- (funcall f (+ x *dx*))
          (funcall f x))
       *dx*)))

;;; Wishthul thinking
(defun lambda-cons (a b)
  (lambda (pick)
    (funcall pick a b)))

(defun lambda-car (p)
  (funcall p (lambda (a b) (declare (ignore b)) a)))

(defun lambda-cdr (p)
  (funcall p (lambda (a b) (declare (ignore a)) b)))
