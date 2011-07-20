
(defun deriv (exp var)
  (cond
    ((constant? exp var) 0)
    ((same-variable? exp var) 1)
    ((sum? exp) (make-sum (deriv (a1 exp) var)
                          (deriv (a2 exp) var)))
    ((product? exp) (make-sum
                     (make-product (m1 exp)
                                   (deriv (m2 exp) var))
                     (make-product (deriv (m1 exp) var)
                                   (m2 exp))))))

(defun constant? (exp var)
  (and (atom exp)
       (not (eq exp var))))

(defun same-variable? (exp var)
  (and (atom exp)
       (eq exp var)))

(defun sum? (exp)
  (and (not (atom exp))
       (eql (car exp) '+)))

(defun product? (exp)
  (and (not (atom exp))
       (eql (car exp) '*)))

(defun make-sum (a1 a2)
  (cond
    ((and (numberp a1) (zerop a1)) a2)
    ((and (numberp a2) (zerop a2) a1))
    ((and (numberp a1) (numberp a2)) (+ a1 a2))
    (t (list '+ a1 a2))))

(defun a1 (exp)
  (cadr exp))

(defun a2 (exp)
  (caddr exp))

(defun m1 (exp)
  (cadr exp))

(defun m2 (exp)
  (caddr exp))

(defun make-product (m1 m2)
  (cond
    ((and (numberp m1) (zerop m1)) 0)
    ((and (numberp m2) (zerop m2)) 0)
    ((and (numberp m1) (= m1 1)) m2)
    ((and (numberp m2) (= m2 1)) m1)
    ((and (numberp m1) (numberp m2)) (* m1 m2))
    (t (list '* m1 m2))))
