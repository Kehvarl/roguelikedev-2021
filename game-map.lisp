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

(defgeneric entities-in-region (map entities region-index))
(defmethod entities-in-region ((map game-map) entities region-index)
  (let ((region-entities nil))
    (map-tiles-loop (map tile :col-val x :row-val y)
      (when (and
             (eql (tile/region (aref (game-map/tiles map) x y)) region-index)
             (entity-at entities x y))
        (setf region-entities (append region-entities
                                      (list (entity-at entities x y))))))
    region-entities))

(defgeneric region-at (map x y))
(defmethod region-at((map game-map) x y)
 (if (and (> x 0) (< x (1- (game-map/w map)))
          (> y 0) (< y (1- (game-map/h map))))
   (tile/region (aref (game-map/tiles map) x y))
   nil))

(defgeneric random-in-region (map region-index))
(defmethod random-in-region ((map game-map) region-index)
  (let ((coords nil))
    (map-tiles-loop (map tile :col-val x :row-val y)
      (when (eql (tile/region (aref (game-map/tiles map) x y)) region-index)
        (setf coords (append coords (list (cons x y))))))
    (let ((coord (nth (random (length coords)) coords)))
      (values (car coord) (cdr coord)))))

(defun entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y))
      (return entity))))

(defun blocking-entity-at (entities x y)
  (let ((entity (entity-at entities x y)))
    (if (and entity (or (entity/blocks entity)
                        (and (entity/door entity)
                             (door/blocks (entity/door entity)))))
      entity)))

(defun wall-p (tile)
  (and tile (tile/blocked tile)))

(defun room-p (tile)
  (and tile (tile/room tile)))

(defun is-door (map x y)
  (let ((door nil)
        (u (get-tile map x (1+ y)))
        (d (get-tile map x (1- y)))
        (l (get-tile map (1- x) y))
        (r (get-tile map (1+ x) y)))
    (when (or (and (room-p u) (not (room-p d))
                   (or (wall-p l) (wall-p r)))
              (and (room-p d) (not (room-p u))
                   (or (wall-p l) (wall-p r)))
              (and (room-p l) (not (room-p r))
                   (or (wall-p u) (wall-p d)))
              (and (room-p r) (not (room-p l))
                   (or (wall-p u) (wall-p d))))
      (setf door t))
    door))

(defgeneric find-doors (map))
(defmethod find-doors ((map game-map))
  (let ((doors nil))
    (map-tiles-loop (map tile :col-val x :row-val y)
      (when (and (tile/corridor tile) (is-door map x y))
        (setf (slot-value tile 'door) t)
        (setf doors (append doors (list (cons x y))))))
    doors))
