
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
      (error "Bad tagged datum -- Contents" datum)))
