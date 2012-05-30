(defpackage :escher
  (:use :cl))

(in-package :escher)

(defclass 2d-vector ()
  ((xcor :initarg :x
         :initform 0
         :accessor xcor)
   (ycor :initarg :y
         :initform 0
         :accessor ycor)))

(defclass segment ()
  ((start :initarg :start
          :initform (make-vector 0 0)
          :accessor seg-start)
   (end :initarg :end
        :initform (make-vector 0 0)
        :accessor seg-end)))

(defclass rectangle ()
  ((horiz :initarg :horiz
          :initform (make-vector 0 0)
          :accessor horiz)
   (vert :initarg :vert
         :initform (make-vector 0 0)
         :accessor vert)
   (origin :initarg :origin
           :initform (make-vector 0 0)
           :accessor origin)))

(defun make-segment (start end)
  (make-instance 'segment :start start :end end))

(defun make-vector (x y)
  (make-instance '2d-vector :x x :y y))

(defun make-rect (origin horiz vert)
  (make-instance 'rectangle
                 :origin origin
                 :horiz horiz
                 :vert vert))

(defmethod +vect ((v1 2d-vector) (v2 2d-vector))
  (make-vector
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))

(defmethod scale ((s number) (v 2d-vector))
  (make-vector (* s (xcor v))
               (* s (ycor v))))


(defun coord-map (rect)
  (lambda (point)
    (+vect
     (+vect (scale (xcor point)
                   (horiz rect))
            (scale (ycor point)
                   (vert rect)))
     (origin rect))))

(defun for-each (f list)
  (loop for i in list
     do (funcall f i))
  nil)

;; (defun drawline (start end)
;;   (format t "start: (~a,~a) end: (~a,~a)~%"
;;           (xcor start)
;;           (ycor start)
;;           (xcor end)
;;           (ycor end)))

(defun drawline (start end)
  (sdl:draw-line
   (sdl:point
    :x (xcor start)
    :y (ycor start))
   (sdl:point
    :x (xcor end)
    :y (ycor end))))

(defun make-picture (seglist)
  (lambda (rect)
    (for-each
     (lambda (s)
       (drawline
        (funcall (coord-map rect) (seg-start s))
        (funcall (coord-map rect) (seg-end s))))
     seglist)))

(defun beside (p1 p2 a)
  (lambda (rect)
    (funcall p1 (make-rect
                 (origin rect)
                 (scale a (horiz rect))
                 (vert rect)))
    (funcall p2 (make-rect
                 (+vect (origin rect)
                        (scale a (horiz rect)))
                 (scale (- 1 a) (horiz rect))
                 (vert rect)))))

(defun above (p1 p2 a)
  (lambda (rect)
    (funcall p2 (make-rect
                 (origin rect)
                 (horiz rect)
                 (scale a (vert rect))))
    (funcall p1 (make-rect
                 (+vect (origin rect)
                        (scale a (vert rect)))
                 (horiz rect)
                 (scale (- 1 a) (vert rect))))))

(defun h-mirror (pict)
  (lambda (rect)
    (funcall pict (make-rect
                   (+vect (origin rect) (horiz rect))
                   (scale -1 (horiz rect))
                   (vert rect)))))

(defun v-mirror (pict)
  (lambda (rect)
    (funcall pict (make-rect
                   (+vect (origin rect) (vert rect))
                   (horiz rect)
                   (scale -1 (vert rect))))))

(defun rotate90 (pict)
  (lambda (rect)
    (funcall pict (make-rect
                   (+vect (origin rect)
                          (horiz rect))
                   (vert rect)
                   (scale -1 (horiz rect))))))

(defvar r (make-instance 'rectangle
                         :horiz (make-vector 300 0)
                         :vert (make-vector 0 200)))

(defvar g (make-picture (list (make-segment (make-vector 0 0.5)
                                            (make-vector 0.5 0))
                              (make-segment (make-vector 0.5 0)
                                            (make-vector 1 0.5))
                              (make-segment (make-vector 1 0.5)
                                            (make-vector 0.5 1))
                              (make-segment (make-vector 0.5 1)
                                            (make-vector 0 0.5)))))

(defparameter g2 (make-picture
                  (list
                   (make-segment (make-vector 0.5 0)
                                 (make-vector 1 0.5))
                   (make-segment (make-vector 1 0.5)
                                 (make-vector 0.5 1))
                   (make-segment (make-vector 0.5 1)
                                 (make-vector 0 0.5)))))

(defun repeated (f n)
  (lambda (x)
    (if (= n 1)
        (funcall f x)
        (funcall (repeated f (1- n)) (funcall f x)))))

(defun pict-push (comb)
  (lambda (pict n a)
    (funcall (repeated
              (lambda (p) (funcall comb pict p a))
              n)
             pict)))

(defun right-push (p n a)
  (funcall (pict-push #'beside) p n a))

(defun up-push (p n a)
  (funcall (pict-push #'above) p n a))

(defun four-pict (p p2 a)
  (above (beside p p2 a)
         (beside p p2 a)
         a))

(defun four-pict-push (p n a)
  (funcall (pict-push #'four-pict) p n a))

(defun square-limit (p n a)
  (let* ((pict (up-push (right-push p n a) n a))
         (oben (beside (h-mirror pict)
                       pict
                       0.5))
         (unten (v-mirror oben))
         (bild (above unten oben 0.5)))
    bild))

;; (ql:quickload :lispbuilder-sdl)
;; (sdl:with-init ()
;;            (sdl:window 600 400)
;;            (sdl:with-events (:poll)
;;              (:quit-event () t)
;;              (:idle ()
;;                     (funcall (square-limit g2 5 0.5) (make-rect (make-vector 0 0) (make-vector 600 0) (make-vector 0 400)))
;;                     (sdl:update-display))
;;              (:video-expose-event (sdl:update-display))))
