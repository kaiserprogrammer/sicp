
(defun put (op type item))

(defun get (op type))

(defun install-rectangular-package ()
  )

(defun real-part (z) (car z))
(defun imag-part (z) (cdr z))
(defun make-from-real-imag (x y) (cons x y))
(defun magnitude (z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(defun angle (z)
  (atan (imag-part z) (real-part z)))
(defun make-from-mag-ang (r a)
  (cons (* r (cos a)) (* r (sin a))))
(defun tag (x) (attach-tag 'rectangular x))
(put 'real-part '(rectangular) #'real-part)
(put 'imag-part '(rectangular) #'imag-part)
(put 'magnitude '(rectangular) #'magnitude)
(put 'angle '(rectangular) #'angle)
(put 'make-from-real-imag 'rectangular
     (lambda (x y) (tag (make-from-real-imag x y))))
(put 'make-from-mag-ang 'rectangular
     (lambda (r a) (tag (make-from-mag-ang r a))))

(defun install-polar-package ()
  (labels ((magnitude (z) (car z))
           (angle (z) (cdr z))
           (make-from-mag-ang (r a) (cons r a))
           (real-part (z)
             (* (magnitude z) (cos (angle z))))
           (imag-part (z)
             (* (magnitude z) (sin (angle z))))
           (make-from-real-imag (x y)
             (cons (sqrt (+ (square x) (square y)))
                   (atan y x)))
           (tag (x) (attach-tag 'polar x)))
    (put 'real-part '(polar) #'real-part)
    (put 'imag-part '(polar) #'imag-part)
    (put 'magnitude '(polar) #'magnitude)
    (put 'angle '(polar) #'angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (mapcar #'contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (funcall #'(get 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang (r a)
  (funcall #'(get 'make-from-mag-ang 'polar) r a))
