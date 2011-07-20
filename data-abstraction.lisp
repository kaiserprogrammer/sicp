
;;; make-point: Int Int -> Point
;;; construct a compound object with values x y
(defun make-point (x y)
  (cons x y))

;;; x-point: Point -> Int
;;; extract x value of point p
(defun x-point (p)
  (car p))

;;; y-point: Point -> Int
;;; extract y value of point p
(defun y-point (p)
  (cdr p))

;;; print-point: Point -> void
;;; EFFECT: print point p to screen
(defun print-point (p)
  (format t "(~a,~a)" (x-point p) (y-point p)))

;;; make-segment: Point Point -> Segment
;;; construct a segment from Point start and Point end
(defun make-segment (start end)
  (cons start end))

;;; start-segment: Segment -> Point
;;; extract starting point of Segment segment
(defun start-segment (segment)
  (car segment))

;;; end-segment: Segment -> Point
;;; extract ending point of Segment segment
(defun end-segment (segment)
  (cdr segment))

;;; midpoint-segment: Segment -> Point
;;; it computes the point which lies in the middle of the Segment segment
(defun midpoint-segment (segment)
  (midpoint-points (start-segment segment)
                   (end-segment segment)))

;;; midpoint-points: Point Point -> Point
;;; it computes the point which lies in between the two points a and b
(defun midpoint-points (a b)
  (make-point (average (x-point b) (x-point a))
              (average (y-point a) (y-point b))))

;;; average: Int Int -> Int
;;; it computes the average of two numbers a b
(defun average (a b)
  (/ (+ a b) 2))

;;; make-rectangle: Point Point -> Rectangle
;;; construct a rectangle compound object from lower left point and
;;; upper right point
(defun make-rectangle (lower-left upper-right)
  (cons lower-left upper-right))

;;; lower-left: Rectangle -> Point
;;; extracet the lower left point of the Rectangle rectangle
(defun lower-left (rectangle)
  (car rectangle))

;;; upper-right: Rectangle -> Point
;;; extract the upper right point of the Rectangle rectangle
(defun upper-right (rectangle)
  (cdr rectangle))

;;; width: Rectangle -> Int
;;; extract the width of the Rectangle rectangle
(defun width (rectangle)
  (points-x-distance (lower-left rectangle)
                     (upper-right rectangle)))

;;; height: Rectangle -> Int
;;; extract the height of the Rectangle rectangle
(defun height (rectangle)
  (points-y-distance (lower-left rectangle)
                     (upper-right rectangle)))

;;; points-x-distance: Point Point -> Int
;;; compute the distance of points a and b on the x-axis
(defun points-x-distance (a b)
  (abs (- (x-point a) (x-point b))))

;;; points-y-distance: Point Point -> Int
;;; compute the distance of points a and b on the y-axis
(defun points-y-distance (a b)
  (abs (- (y-point a) (y-point b))))

;;; rectangle-area: Rectangle -> Int
;;; compute the area of a rectangle
(defun rectangle-area (rectangle)
  (* (width rectangle) (height rectangle)))

;;; rectangle-perimeter: Rectangle -> Int
;;; compute the perimeter of a Rectangle
(defun rectangle-perimeter (rectangle)
  (* (+ (height rectangle)) (width rectangle)
     2))
