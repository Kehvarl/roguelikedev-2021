(in-package #:roguelike-2021)

(defmethod create-room ((map game-map) (room rect))
  (map-tiles-loop (map tile
                       :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                       :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil)))

(defmethod create-h-tunnel ((map game-map) x1 x2 y)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                         :x-start start-x :x-end (1+ end-x)
                         :y-start y :y-end (1+ y))
      (set-tile-slots tile :blocked nil :block-sight nil))))


(defmethod create-v-tunnel ((map game-map) y1 y2 x)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                         :x-start x :x-end (1+ x)
                         :y-start start-y :y-end (1+ end-y))
      (set-tile-slots tile :blocked nil :block-sight nil))))

(defmethod place-entities ((map game-map) (room rect) entities
                                          max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (rect/random room)
        (unless (entity-at entities x y)
          (if (< (random 100) 80)
            (nconc entities (list (make-instance 'entity :name "Orc"
                                                 :x x :y y :color (blt:green)
                                                 :char #\o :blocks t)))
            (nconc entities (list (make-instance 'entity :name "Troll"
                                                 :x x :y y :color (blt:yellow)
                                                 :char #\T :blocks t)))))))))

(defmethod make-map ((map game-map) max-rooms
                                   room-min-size room-max-size
                                   map-width map-height
                                   player entities
                                   max-enemies-per-room)

 (do* ((rooms nil)
       (num-rooms 0)
       (room-index 0 (1+ room-index))
       (w (+ (random (- room-max-size room-min-size)) room-min-size)
          (+ (random (- room-max-size room-min-size)) room-min-size))
       (h (+ (random (- room-max-size room-min-size)) room-min-size)
          (+ (random (- room-max-size room-min-size)) room-min-size))
       (x (random (- map-width w))
          (random (- map-width w)))
       (y (random (- map-height h))
          (random (- map-height h)))
       (new-room (make-instance 'rect :x x :y y :w w :h h)
                 (make-instance 'rect :x x :y y :w w :h h))
       (can-place-p t t))
      ((>= room-index max-rooms))
   (dolist (other-room rooms)
           (if (intersect new-room other-room)
             (setf can-place-p nil)))
   (when can-place-p
     (create-room map new-room)
     (multiple-value-bind (new-x new-y) (center new-room)
       (if (zerop num-rooms)
           (setf (entity/x player) new-x
                 (entity/y player) new-y)
           (multiple-value-bind (prev-x prev-y) (center (car (last rooms)))
             (cond ((= (random 2) 1)
                    (create-h-tunnel map prev-x new-x prev-y)
                    (create-v-tunnel map prev-y new-y new-x))
                   (t
                    (create-v-tunnel map prev-y new-y prev-x)
                    (create-h-tunnel map prev-x new-x new-y)))))
       (place-entities map new-room entities max-enemies-per-room)
       (if (null rooms)
           (setf rooms (list new-room))
           (push new-room (cdr (last rooms))))
       (incf num-rooms)))))
