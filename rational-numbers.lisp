
;;; make-rat: Int Int -> Rational
;;; constructs a compound data object from numerator and denominator
(defun make-rat (numerator denominator)
  (cond
    ((< denominator 0) (make-rat (- numerator) (- denominator)))
    (t (let ((g (gcd numerator denominator)))
       (cons (/ numerator g)
             (/ denominator g))))))

;;; numer: Rational -> Int
;;; extracts the numerator from rat, a rational Compound object.
(defun numer (rat)
  (car rat))

;;; denom: Rational -> Int
;;; extracts the denominator from rat, a rational Compound object.
(defun denom (rat)
  (cdr rat))

;;; add-rat: Rational Rational -> Rational
;;; it computes the result of adding the two rational numbers a and b
(defun add-rat (a b)
  (make-rat (+ (* (numer a) (denom b))
               (* (numer b) (denom a)))
            (* (denom a) (denom b))))

;;; sub-rat: Rational Rational -> Rational
;;; it computes the result of substracting b from a
(defun sub-rat (a b)
  (add-rat a (make-rat (- (numer b)) (denom b))))

;;; mul-rat: Rational Rational -> Rational
;;; it computes the result of multiplicating the rationals a and b
(defun mul-rat (a b)
  (make-rat (* (numer a) (numer b))
            (* (denom a) (denom b))))

;;; div-rat: Rational Rational -> Rational
;;; it computes the result of dividing divisor from divident
(defun div-rat (divisor divident)
  (make-rat (* (numer divisor) (denom divident))
            (* (numer divident) (denom divisor))))

;;; equal-rat?: Rational Rational -> boolean
;;; it computes if the two rationals a and b are equal
(defun equal-rat? (a b)
  (= (* (numer a) (denom b))
     (* (denom a) (numer b))))

;;; print-rat: Rational -> void
;;; EFFECT: print rat to screen
(defun print-rat (rat)
  (format t "~a/~a" (numer rat) (denom rat)))
