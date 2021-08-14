(in-package #:roguelike-2021)

(defgeneric create-room (map room index))
(defmethod create-room ((map game-map) (room rect) index)
  (map-tiles-loop (map tile
                       :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                       :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil :region index)))

(defgeneric create-h-tunnel (map x1 x2 y index))
(defmethod create-h-tunnel ((map game-map) x1 x2 y index)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                         :x-start start-x :x-end (1+ end-x)
                         :y-start y :y-end (1+ y))
      (when (not (tile/region tile))
        (set-tile-slots tile :blocked nil :block-sight nil :region index)))))

(defgeneric create-v-tunnel (map y1 y2 x index))
(defmethod create-v-tunnel ((map game-map) y1 y2 x index)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                         :x-start x :x-end (1+ x)
                         :y-start start-y :y-end (1+ end-y))
      (when (not (tile/region tile))
        (set-tile-slots tile :blocked nil :block-sight nil :region index)))))

(defparameter *cardinal-directions*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)))

(defun neighboring-regions (map x y)
  (let ((regions nil)
        (walls 0))
    (dolist (direction *cardinal-directions*)
        (when (and (> x 0) (< x (1- (game-map/w map)))
                   (> y 0) (< y (1- (game-map/h map))))
            (let ((tile (aref (game-map/tiles map)
                              (+ x (car direction))
                              (+ y (cdr direction)))))
              (when (and (tile/region tile) (not (member (tile/region tile) regions)))
                (setf regions (append (list (tile/region tile) regions))))
              (when (tile/blocked tile) (incf walls)))))
    (values regions walls)))

(defgeneric find-doors (map))
(defmethod find-doors ((map game-map))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (multiple-value-bind (regions walls) (neighboring-regions map x y)
      (when (and (> (length regions) 1)
                 (> walls 0))
        (setf (slot-value tile 'door) t)))))


(defgeneric place-entities (map room entities max-enemies-per-room max-items-per-room))
(defmethod place-entities ((map game-map) (room rect) entities
                                          max-enemies-per-room
                                          max-items-per-room)
  (let ((num-monsters (random max-enemies-per-room))
        (num-items    (random (1+ max-items-per-room))))
    (place-monsters room entities num-monsters)
    (place-items    room entities num-items)))

(defun place-monsters (room entities num-monsters)
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (rect/random room)
        (unless (entity-at entities x y)
          (let ((monster-rand (random 100)))
            (cond
              ((< monster-rand 70)
               (let* ((fighter-component (make-instance 'fighter :hp 10
                                                        :defense 0 :power 3))
                      (ai-component (make-instance 'tracking-monster :active-range 5)))
                 (nconc entities (list (make-instance 'entity :name "Orc"
                                                      :x x :y y :color (blt:green)
                                                      :char #\o :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component)))))

              ((< monster-rand 90)
               (let* ((fighter-component (make-instance 'fighter :hp 16
                                                        :defense 1 :power 4))
                      (ai-component (make-instance 'basic-monster :active-range 10)))
                 (nconc entities (list (make-instance 'entity :name "Troll"
                                                      :x x :y y :color (blt:yellow)
                                                      :char #\T :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component
                                                      :regenerating t)))))
              (t
               (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1
                                                        :power 4))
                      (ai-component (make-instance 'ranged-monster :active-range 10)))
                 (nconc entities (list (make-instance 'entity :name "Curious Light"
                                                      :x x :y y :color (blt:chartreuse)
                                                      :char #\c :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component
                                                      :regenerating t)))))))))))

(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (multiple-value-bind (x y) (rect/random room)
      (unless (entity-at entities x y)
        (let ((monster-rand (random 100)))
          (cond
            ((< monster-rand 70)
             (let* ((item-component (make-instance 'item :use-function #'heal
                                                   :use-args '(:heal-amount 4)))
                    (potion (make-instance 'entity :name "Healing Potion"
                                           :x x :y y :color (blt:purple)
                                           :item item-component
                                           :char #\! :blocks nil
                                           :render-order :item)))
               (nconc entities (list potion))))
            (t
             (format t "Monster spawner at ~A,~A~%" x y))))))))

(defun place-spawner (room entities spawner)
  (multiple-value-bind (x y) (rect/random room)
    (unless (entity-at entities x y)
      (setf (spawner/room spawner) room)
      (setf (spawner/region spawner) (rect/region room))
      (nconc entities (list (make-instance 'entity :x x :y y :color (blt:black)
                                           :spawner spawner
                                           :char #\space :blocks nil
                                           :render-order :item))))))

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
         (place-spawner new-room entities (make-instance 'spawner :frequency 10)))
       (if (null rooms)
           (setf rooms (list new-room))
           (push new-room (cdr (last rooms))))
       (incf num-rooms))))
 (find-doors map))
