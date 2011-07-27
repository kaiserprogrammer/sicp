
(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(defun inverter (input output)
  (labels ((invert-input ()
             (let ((new-value (logical-not (get-signal input))))
               (after-delay inverter-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! input #'invert-input)))

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error (format nil "Invalid signal ~a" s)))))

(defun logical-and (a b)
  (cond ((and (= a 1) (= b 1)) 1)
        (t 0)))
(defun logical-or (a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (t 0)))

(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
             (let ((new-value
                    (logical-and (get-signal a1) (get-signal a2))))
               (after-delay and-gate-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! a1 #'and-action-procedure)
    (add-action! a2 #'and-action-procedure)))

(defun or-gate (o1 o2 output)
  (labels ((or-action-procedure ()
             (let ((new-value
                    (logical-or (get-signal o1) (get-signal o2))))
               (after-delay or-gate-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! o1 #'or-action-procedure)
    (add-action! o2 #'or-action-procedure)))

(defun make-wire ()
  (let ((signal-value 0)
        (action-procedures '()))
    (labels
        ((set-my-signal! (new-value)
           (if (not (= signal-value new-value))
               (progn (setf signal-value new-value)
                      (call-each action-procedures))
               'done))
         (accept-action-procedure! (proc)
           (push proc action-procedures)
           (funcall proc))
         (dispatch (m)
           (cond
             ((eq m 'get-signal) signal-value)
             ((eq m 'set-signal!) #'set-my-signal!)
             ((eq m 'add-action!) #'accept-action-procedure!)
             (t (error (format nil "Unknown operation -- WIRE ~a" m))))))
      #'dispatch)))

(defun call-each (procedures)
  (if (null procedures)
      'done
      (progn
        (funcall (car procedures))
        (call-each (cdr procedures)))))

(defun get-signal (wire)
  (funcall wire 'get-signal))

(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))

(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(defun propagate ()
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (funcall first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (format t "~&~a ~a  New-value = ~a"
                         name
                         (current-time the-agenda)
                         (get-signal wire)))))

(defparameter the-agenda (make-agenda))
(defvar inverter-delay 2)
(defvar and-gate-delay 3)
(defvar or-gate-delay 5)

(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s)
  (car s))
(defun segment-queue (s)
  (cdr s))

(defun make-agenda ()
  (list 0))
(defun current-time (agenda)
  (car agenda))
(defun set-current-time! (agenda time)
  (setf (car agenda) time))
(defun segments (agenda)
  (cdr agenda))
(defun set-segments!  (agenda segments)
  (setf (cdr agenda) segments))
(defun first-segment (agenda)
  (car (segments agenda)))
(defun rest-segments (agenda)
  (cdr (segments agenda)))

(defun empty-agenda? (agenda)
  (null (segments agenda)))

(defun add-to-agenda! (time action agenda)
  (labels
      ((belongs-before? (segments)
         (or (null segments)
             (< time (segment-time (car segments)))))
       (make-new-time-segment (time action)
         (let ((q (make-queue)))
           (insert-queue! q action)
           (make-time-segment time q)))
       (add-to-segments! (segments)
         (if (= (segment-time (car segments)) time)
             (insert-queue! (segment-queue (car segments))
                            action)
             (let ((rest (cdr segments)))
               (if (belongs-before? rest)
                   (setf (cdr segments)
                         (cons (make-new-time-segment time action)
                               (cdr segments)))
                   (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments!
           agenda
           (cons (make-new-time-segment time action)
                 segments))
          (add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is emtpy")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
