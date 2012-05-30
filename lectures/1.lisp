(defun average (&rest numbers)
  (/ (reduce #'+ numbers)
     (length numbers)))

(defun try-sqrt (x)
  (labels ((improve (guess)
             (average guess (/ x guess)))
           (good-enoughp (guess)
             (< (abs (- (* guess guess) x))
                0.001))
           (try (guess)
             (if (good-enoughp guess)
                 guess
                 (try (improve guess)))))
    (try 1)))

(defun print-move (from to)
  (format t "From: ~a to: ~a~%" from to))

(defun move (n from to spare)
  (cond ((= n 0) 'done)
        (t
         (move (1- n) from spare to)
         (print-move from to)
         (move (1- n) spare to from))))
