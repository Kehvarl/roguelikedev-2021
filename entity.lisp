(in-package #:roguelike-2021)

(defclass entity()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)))

(defmethod draw ((e entity) (map game-map))
  (with-slots (x y char color) e
   (if (tile/visible (aref (game-map/tiles map) x y))
       (setf (blt:color) color
             (blt:background-color) (blt:cell-background-color x y)
            (blt:cell-char x y) char))))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))
