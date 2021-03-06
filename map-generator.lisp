(in-package #:roguelike-2021)

(defgeneric create-room (map room index))
(defmethod create-room ((map game-map) (room rect) index)
  (map-tiles-loop (map tile
                       :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                       :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil :region index
                    :room t :corridor nil)))

(defgeneric create-h-tunnel (map x1 x2 y index))
(defmethod create-h-tunnel ((map game-map) x1 x2 y index)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                         :x-start start-x :x-end (1+ end-x)
                         :y-start y :y-end (1+ y))
      (unless (tile/room tile)
        (set-tile-slots tile :blocked nil :block-sight nil :region index
                        :corridor t)))))

(defgeneric create-v-tunnel (map y1 y2 x index))
(defmethod create-v-tunnel ((map game-map) y1 y2 x index)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                         :x-start x :x-end (1+ x)
                         :y-start start-y :y-end (1+ end-y))
      (unless (tile/room tile)
        (set-tile-slots tile :blocked nil :block-sight nil :region index
                        :corridor t)))))

(defun get-tile (map x y)
  (if (and (< x (1- (game-map/w map)))
           (> x 0)
           (< y (1- (game-map/h map)))
           (> y 0))
    (aref (game-map/tiles map) x y)
    nil))

(defgeneric place-entities (map room entities max-enemies-per-room max-items-per-room))
(defmethod place-entities ((map game-map) (room rect) entities
                                          max-enemies-per-room
                                          max-items-per-room)
  (let ((num-monsters (random max-enemies-per-room))
        (num-items    (random (1+ max-items-per-room))))
    (place-monsters room entities num-monsters)
    (place-items    room entities num-items)))

(defgeneric make-map (map max-rooms
                          room-min-size room-max-size
                          map-width map-height
                          player entities
                          max-enemies-per-room
                          max-items-per-room))

(defmethod make-map ((map game-map) max-rooms
                                   room-min-size room-max-size
                                   map-width map-height
                                   player entities
                                   max-enemies-per-room
                                   max-items-per-room)

 (do* ((rooms nil)
       (region-index 0)
       (num-rooms 0)
       (room-index 0 (1+ room-index))
       (new-room (random-rect room-min-size room-max-size
                              map-width map-height)
                 (random-rect room-min-size room-max-size
                              map-width map-height))
       (can-place-p t t))
      ((>= room-index max-rooms))
   (dolist (other-room rooms)
           (if (intersect new-room other-room)
             (setf can-place-p nil)))
   (when can-place-p
     (setf (rect/region new-room) region-index)
     (create-room map new-room region-index)
     (incf region-index)
     (setf (game-map/rooms map) (append (game-map/rooms map) (list new-room)))
     (multiple-value-bind (new-x new-y) (center new-room)
       (if (zerop num-rooms)
           (progn
            (setf (entity/x player) new-x
                  (entity/y player) new-y))
           (multiple-value-bind (prev-x prev-y) (center (car (last rooms)))
             (cond ((= (random 2) 1)
                    (create-h-tunnel map prev-x new-x prev-y region-index)
                    (create-v-tunnel map prev-y new-y new-x region-index)
                    (incf region-index))
                   (t
                    (create-v-tunnel map prev-y new-y prev-x region-index)
                    (create-h-tunnel map prev-x new-x new-y region-index)
                    (incf region-index)))))
       (place-entities map new-room entities max-enemies-per-room max-items-per-room)
       (when (> (random 100) 90)
         (place-spawner new-room entities (make-instance 'cloner :frequency 10
                                                         :clone-region (random region-index))))
       (if (null rooms)
           (setf rooms (list new-room))
           (push new-room (cdr (last rooms))))
       (incf num-rooms))))
 (dolist (door (find-doors map))
         (place-door entities (car door) (cdr door))))
