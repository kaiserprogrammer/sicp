
(defun make-from-real-imag (x y)
  (lambda (op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude)
       (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (t (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)))))

(defun apply-generic (op arg)
  (funcall arg op))

(defun make-from-mag-ang (r a)
  (lambda (op)
    (cond
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      (t (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))
