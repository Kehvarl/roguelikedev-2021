(in-package #:roguelike-2021)

(defmacro map-tiles-loop ((map tile-val &key (row-val (gensym))
                                          (col-val (gensym))
                                          (x-start 0) (y-start 0)
                                          (x-end nil) (y-end nil))
                          &body body)
  `(loop :for ,col-val
         :from ,x-start
           :below (if (null ,x-end) (game-map/w ,map) ,x-end)
    :do
      (loop :for ,row-val
             :from ,y-start
               :below (if (null ,y-end) (game-map/h ,map) ,y-end)
       :do
          (let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))
              (declare (ignorable ,tile-val))
              ,@body))))

(defclass game-map ()
  ((width :initarg :w
          :accessor game-map/w)
   (height :initarg :h
           :accessor game-map/h)
   (tiles :accessor game-map/tiles)
   (rooms :accessor game-map/rooms)))

(defmethod initialize-instance :after ((map game-map)
                                       &key (initial-blocked-value t))
  (setf (game-map/rooms map) ())
  (setf (game-map/tiles map) (make-array (list (game-map/w map)
                                               (game-map/h map))))
  (map-tiles-loop (map tile :col-val x :row-val y)
                  (setf (aref (game-map/tiles map) x y)
                        (make-instance 'tile :blocked initial-blocked-value))))

(defmethod blocked-p ((map game-map) x y)
  (tile/blocked (aref (game-map/tiles map) x y)))

(defun entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y))
      (return entity))))

(defun blocking-entity-at (entities x y)
  (let ((entity (entity-at entities x y)))
    (if (and entity (entity/blocks entity))
      entity)))
