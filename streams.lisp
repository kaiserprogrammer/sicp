
(defun stream-ref (s n)
  (if (zerop n)
      (stream-car s)
      (stream-ref (stream-cdr s) (1- n))))



(defun stream-map (proc &rest argstreams)  
  (if (reduce (lambda (base next) (or base next)) (mapcar #'stream-null? argstreams))
      the-empty-stream
      (cons-stream 
       (apply proc 
              (mapcar #'stream-car argstreams))
       (apply #'stream-map (cons proc (mapcar #'stream-cdr argstreams))))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
      (progn (funcall proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(defun display-stream (s)
  (stream-for-each #'display-line s))

(defun display-line (x)
  (format t "~&~a" x))

(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (force (cdr stream)))


(defun stream-filter (pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((funcall pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (t (stream-filter pred (stream-cdr stream)))))

(defun stream-foldl (proc base stream)
  (if (stream-null? stream)
      base
      (stream-foldl proc
                    (funcall proc base (stream-car stream))
                    (stream-cdr stream))))

(defun force (delayed)
  (funcall delayed))

(defun memo-proc (proc)
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if (not already-run?)
          (progn (setf result (funcall proc))
                 (setf already-run? t)
                 result)
          result))))

(defmacro delay (expr)
  `(memo-proc (lambda () ,expr)))

(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

(defun stream-null? (stream)
  (equal the-empty-stream stream))

(defparameter the-empty-stream '())

(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (1+ low) high))))

(defvar ones (cons-stream 1 ones))

(defun prime? (x)
  (if (< x 2)
      nil
      (if (= x 2)
          t
          (if (zerop (rem x 2))
              nil
              (loop for i from 3 to (sqrt x) by 2
                 when (zerop (rem x i))
                 return nil
                 finally (return t))))))

(defun stream-integers-from (n)
  (cons-stream n (stream-integers-from (1+ n))))

(defvar stream-fib (cons-stream 1
                                (cons-stream 1
                                             (stream-map #'+ stream-fib (stream-cdr stream-fib)))))

(defun stream-take-while (pred stream)
  (if (funcall pred (stream-car stream))
      (cons-stream (stream-car stream)
                   (stream-take-while pred (stream-cdr stream)))
      the-empty-stream))

(defun sieve (stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (zerop (rem x (stream-car stream)))))
           (stream-cdr stream)))))

(defparameter primes (cons-stream 2
                                  (stream-filter #'prime? (stream-integers-from 3))))
;;; prime-stream? is way slower than prime?
(defun prime-stream? (n)
  (let ((sqrtn (sqrt n)))
    (labels ((iter (ps)
               (cond
                 ((> (stream-car ps) sqrtn) t)
                 ((zerop (rem n (stream-car ps))) nil)
                 (t (iter (stream-cdr ps))))))
      (iter primes))))

(defun stream-scale (stream n)
  (stream-map (lambda (x) (* x n)) stream))

(defun stream-merge (s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (t
     (let ((s1car (stream-car s1))
           (s2car (stream-car s2)))
       (cond ((< s1car s2car)
              (cons-stream s1car (stream-merge (stream-cdr s1) s2)))
             ((> s1car s2car)
              (cons-stream s2car (stream-merge s1 (stream-cdr s2))))
             (t
              (cons-stream s1car (stream-merge (stream-cdr s1) (stream-cdr s2)))))))))

(defun stream-take (stream n)
  (cond
    ((zerop n) the-empty-stream)
    (t (cons-stream (stream-car stream)
                    (stream-take (stream-cdr stream) (1- n))))))

(defun stream-partial-sums (stream)
  (cons-stream (stream-car stream)
               (stream-map #'+ (stream-cdr stream) (stream-partial-sums stream))))

(defun sqrt-improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-stream (x)
  (cons-stream 1
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream 2))))

(defun average (a b)
  (/ (+ a b) 2))

(defun pi-summands (n)
  (cons-stream (/ 1 n)
               (stream-map #'- (pi-summands (+ n 2)))))

(defparameter pi-stream
  (stream-scale (stream-partial-sums (pi-summands 1)) 4))

(defun euler-transform (s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (expt (- s2 s1) 2)
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(defun make-tableau (transform s)
  (cons-stream s
               (make-tableau transform
                             (funcall transform s))))

(defun accelerated-sequence (transform s)
  (stream-map #'stream-car
              (make-tableau transform s)))

(defun add-streams (s1 s2)
  (stream-map #'+ s1 s2))

(defun integral (delayed-integrand initial-value dt)
  (labels ((fint ()
             (cons-stream initial-value
                          (let ((integrand (force delayed-integrand)))
                            (add-streams (stream-scale integrand dt)
                                         (funcall #'fint))))))
    (funcall #'fint)))

(defun solve (f y0 dt)
  (labels ((fy ()
             (integral (delay (funcall #'fdy)) y0 dt))
           (fdy ()
             (stream-map f (funcall #'fy))))
    (funcall #'fy)))
