
(defun real-part (complex-number)
  )

(defun imag-part (complex-number))

(defun magnitude (complex-number))

(defun angle (complex-number))

(defun make-from-real-imag (real-part imag-part))

(defun make-form-mag-ang (mag ang))

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(defun attach-tag (type-tag contents)
  (cons type-tag contents))

(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "Bad tagget datum -- CONTENTS" datum)))

(defun rectangular? (z)
  (eq? (type-tag z) 'rectangular))

(defun polar? (z)
  (eq? (type-tag z) 'polar))

(defun real-part-rectangular (z)
  (car z))
(defun imag-part-rectangular (z)
  (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular
              (cons (* r (cos a))
                    (* r (sin a)))))

(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(defun magnitude-polar (z)
  (car z))
(defun angle-polar (z)
  (cdr z))
(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))

(defun real-part (z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (t (error "Unknown type -- REAL-PART" z))))
(defun imag-part (z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (t (error "Unknown type -- IMAG-PART" z))))
(defun magnitude (z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (t (error "Unknown type -- MAGNITUDE" z))))
(defun angle (z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (t (error "Unknown type -- ANGLE" z))))

(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-mag-ang-polar r a))

