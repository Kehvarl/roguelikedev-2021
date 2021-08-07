(in-package #:roguelike-2021)

(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)
   (retion :initarg :region :accessor rect/region)))

(defmethod initialize-instance :after ((rect rect) &key x y w h)
  (with-slots (x1 x2 y1 y2) rect
    (setf x1 x
          y1 y
          x2 (+ x w)
          y2 (+ y h))))

(defmethod center ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
    (let ((center-x (floor (+ x2 x1) 2))
          (center-y (floor (+ y2 y1) 2)))
      (values center-x center-y))))

(defmethod rect/random ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
   (let ((rand-x (+ (random (round (- x2 x1 1)))(1+ x1)))
         (rand-y (+ (random (round (- y2 y1 1)))(1+ y1))))
     (values rand-x rand-y))))

(defmethod intersect ((rect rect) (other rect))
  (and (<= (rect/x1 rect) (rect/x2 other))
       (>= (rect/x2 rect) (rect/x1 other))
       (<= (rect/y1 rect) (rect/y2 other))
       (>= (rect/y2 rect) (rect/y1 other))))

(defun random-rect (min-size max-size map-width map-height)
  (let* ((w (+ (random (- max-size min-size)) min-size))
         (h (+ (random (- max-size min-size)) min-size))
         (x (random (- map-width w)))
         (y (random (- map-height h))))
    (make-instance 'rect :x x :y y :w w :h h)))
