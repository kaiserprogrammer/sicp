(defpackage :cs-escher
  (:use :cl))

(in-package :cs-escher)

(defclass picture ()
  ((lines :initform ()
          :initarg :lines
          :accessor lines)))

(defclass point ()
  ((x-pos :initform 0
          :initarg :x)
   (y-pos :initform 0
          :initarg :y)))

(defclass line ()
  ((a-point :initform (make-instance 'point)
            :initarg :a-point)
   (b-point :initform (make-instance 'point)
            :initarg :b-point)))

(defclass rectangle ()
  ((width :initform 0
          :initarg :width)
   (height :initform 0
           :initarg :height)))

(defun create-picture-from-list (list)
  (let ((lines
         (loop for ((a-x a-y) (b-x b-y)) in list
            collect (make-instance 'line
                                   :a-point (make-instance 'point
                                                           :x a-x
                                                           :y a-y)
                                   :b-point (make-instance 'point
                                                           :x b-x
                                                           :y b-y)))))
    (make-instance 'picture
                   :lines lines)))

(defmethod scale-line-x ((l line) (s number))
  (with-slots (a-point b-point) l
    (make-instance 'line
                   :a-point
                   (make-instance 'point
                                  :x (* s (slot-value a-point 'x-pos))
                                  :y (slot-value a-point 'y-pos))
                   :b-point
                   (make-instance 'point
                                  :x (* s (slot-value b-point 'x-pos))
                                  :y (slot-value b-point 'y-pos)))))

(defmethod scale-line-y ((l line) (s number))
  (with-slots (a-point b-point) l
    (make-instance 'line
                   :a-point
                   (make-instance 'point
                                  :x (slot-value a-point 'x-pos)
                                  :y (* s (slot-value a-point 'y-pos)))
                   :b-point
                   (make-instance 'point
                                  :x (slot-value b-point 'x-pos)
                                  :y (* s (slot-value b-point 'y-pos))))))

(defmethod draw-line ((l line))
  (with-slots (a-point b-point) l
    (sdl:draw-line
     (sdl:point :x (slot-value a-point 'x-pos)
                :y (slot-value a-point 'y-pos))
     (sdl:point :x (slot-value b-point 'x-pos)
                :y (slot-value b-point 'y-pos)))))

(defmethod draw-picture ((p picture) (rec rectangle))
  (let ((x-mult (/ (slot-value rec 'height)
                   100))
        (y-mult (/ (slot-value rec 'width)
                   100))
        (lines (slot-value p 'lines)))
    (mapcar (lambda (line)
              (draw-line (scale-line-y (scale-line-x line x-mult) y-mult))) lines)))
