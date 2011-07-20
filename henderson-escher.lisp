;;; primitve: picture, to fit a picture by resizing into a rectangle

;;; rotate: picture -> picture
;;;


;;; beside: picture picture float -> picture
;;; place picture A near B by the proportion

;;; above: picture pictuer float -> picture
;;; place picture A above B by the proportion

;;; make-rect: vector vector vector -> rectangle
;;; make a rectangle out of horiz vert and origin.

;;; horiz-rect: Rectangle -> Vector

;;; vert-rect: Rectangle -> Vector

;;; origin-rect: Rectangle -> Vector

;;; make-vector: Pair Pair -> Vector
;;; Start and End of a vector

;;; start-vector: Vector -> Pair

;;; end-vector: Vector -> Pair

;;; make-pair: Number Number -> Pair

;;; xcor: Pair -> Number

;;; ycor: Pair -> Number


;;; make-picture: (listof Segment) -> Picture

(defun make-picture (seglist)
  (lambda (rect)
    (for-each
     (lambda (s)
       (draw-line
        ((coord-map rect) (seg-start s))
        ((coord-map rect) (seg-end s)))))))
