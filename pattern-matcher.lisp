
(defparameter deriv-rules
  '(
    ( (dd (?c c) (? v)) 0)
    ( (dd (?v v) (? v)) 1)
    ( (dd (?v u) (? v)) 0)

    ( (dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
      (dd (: x2) (: v))))

    ( (dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
      (* (dd (: x1) (: v)) (: x2))))))

(defparameter algebra-rules
  '(
    ( ((? op) (?c e1) (?c e2))
     (: (op e1 e2)))
    
    ( ((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))

    ( (+ 0 (? e))
     (: e))

    ( (* 1 (? e))
     (: e))

    ( (* 0 (? e))
     0)

    ( (* (?c e1) (* (?c e2) (? e3)))
     (* (: (* e1 e2)) (: e3)))

    ( (* (? e1) (* (?c e2) (? e3)))
     (* (: e2) (* (: e1) (: e3))))))

(defun dsimp (exp)
  (funcall (simplifier deriv-rules) exp))

(defun match (pat exp dict)
  (cond
    ((eq? dict 'failed) 'failed)
    ((atom pat) *** atomic patterns)
    *** pattern variable clauses
    ((atom exp) 'failed)
    (t
     (match (cdr pat)
            (cdr exp)
            (match (car pat)
                   (car exp)
                   dict)))))
