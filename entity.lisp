(in-package #:roguelike-2021)

(defclass entity()
  ((descriptor :initarg :descriptor :accessor entity/descriptor :initform nil)
   (name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks)
   (render-order :initarg :render-order
                 :accessor entity/render-order
                 :initform :corpse)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :ai :accessor entity/ai :initform nil)
   (spawner :initarg :spawner :accessor entity/spawner :initform nil)
   (regenerating :initarg :regenerating :accessor entity/regenerating :initform nil)
   (item :initarg :item :accessor entity/item :initform nil)
   (inventory :initarg :inventory :accessor entity/inventory :initform nil)
   (effects :initarg :effects :accessor entity/effects :initform nil)
   (door :initarg :door :accessor entity/door :initform nil)))

(defclass player (entity)
  ((track :initarg :track :accessor player/track :initform 0)
   (score :initarg :score :accessor player/score :initform 0)))


(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fighter ai spawner inventory effects door) entity
    (when fighter
      (setf (component/owner fighter) entity))
    (when ai
      (setf (component/owner ai) entity))
    (when spawner
      (setf (component/owner spawner) entity))
    (when inventory
      (setf (component/owner inventory) entity))
    (when effects
      (setf (component/owner effects) entity))
    (when door
      (setf (component/owner door) entity))))

(defmethod describe-entity ((e entity))
  (with-slots (descriptor name) e
    (if descriptor
      (format nil "~A of ~A" descriptor name)
      name)))

(defmethod draw ((e entity) (map game-map))
  (with-slots (x y char color) e
   (if (tile/visible (aref (game-map/tiles map) x y))
       (setf (blt:color) color
             (blt:background-color) (blt:cell-background-color x y)
            (blt:cell-char x y) char))))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))

(defgeneric move (e dx dy))
(defgeneric move-safe (e dx dy map entities))
(defgeneric move-towards (e target-x target-y map entities))
(defgeneric distance-to (e other))

(defmethod move-safe ((e entity) dx dy map entities)
  (with-slots (x y) e
    (unless (blocking-entity-at entities (+ x dx) (+ y dy))
         (unless (tile/blocked (aref (game-map/tiles map) (+ x dx) (+ y dy)))
           (move e dx dy)))))

(defmethod move-towards ((e entity) target-x target-y  map entities)
  (with-slots (x y) e
    (let ((path (astar map (cons x y) (cons target-x target-y))))
      (when path
        (let ((next-location (nth 1 path)))
          (unless (blocking-entity-at entities
                                      (car next-location)
                                      (cdr next-location))
            (move e (- (car next-location) x) (- (cdr next-location) y))))))))

(defmethod distance-to ((e entity) (other entity))
  (let ((dx (- (entity/x other) (entity/x e)))
        (dy (- (entity/y other) (entity/y e))))
    (sqrt (+ (expt dx 2) (expt dy 2)))))
