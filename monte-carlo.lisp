(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo2 trials #'cesaro-test))))

(defun cesaro-test ()
  (= (gcd (random 10000) (random 10000)) 1))

(defun monte-carlo (trials experiment)
  (do ((trials-remaining trials (1- trials-remaining))
       (trials-passed 0 (if (funcall experiment)
                            (1+ trials-passed)
                            trials-passed)))
      ((zerop trials-remaining) (/ trials-passed trials))))

(defun monte-carlo2 (trials experiment)
  (/ (loop repeat trials
        when (funcall experiment)
        sum 1)
     trials))
