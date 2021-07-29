(in-package #:roguelike-2021)

(defparameter *render-order*
  '(:corpse 1
    :item 2
    :actor 3))

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :lit-wall (blt:rgba 130 110 50)
                                :lit-ground (blt:rgba 200 180 50)))

(defun render-order-compare (entity-1 entity-2)
  (< (getf *render-order* (entity/render-order entity-1))
     (getf *render-order* (entity/render-order entity-2))))

(defun render-all (game-state player map stats-panel screen-width screen-height)
  (declare (ignore screen-width screen-height player))
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
      (let* ((tile (aref (game-map/tiles map) x y))
             (wall (tile/blocked tile))
             (visible (tile/visible tile))
             (explored (tile/explored tile)))

         (cond (visible
                 (if wall
                   (setf (blt:background-color) (getf *color-map* :lit-wall))
                   (setf (blt:background-color) (getf *color-map* :lit-ground)))
                (setf (blt:cell-char x y) #\Space))
               (explored
                 (if wall
                   (setf (blt:background-color) (getf *color-map* :dark-wall))
                   (setf (blt:background-color) (getf *color-map* :dark-ground)))
                (setf (blt:cell-char x y) #\Space))))))


  (mapc #'(lambda (entity) (draw entity map))
        (sort (game-state/entities game-state)  #'render-order-compare))
  (setf (blt:background-color) (blt:black)
          (blt:color) (blt:white))
  (render-panel stats-panel)

  (let ((entity-names (get-names-under-mouse (blt:mouse-x) (blt:mouse-y)
                                             (game-state/entities game-state)  map)))
    (when entity-names
      (setf (blt:color) (blt:yellow))
      (blt:print (1+ (panel/x stats-panel)) (1+ (panel/y stats-panel)) entity-names)))

  (blt:refresh))

(defun get-names-under-mouse (x y entities map)
  (when (and (< y (game-map/h map))
             (< x (game-map/w map)))
    (let ((names nil)
          (in-fov (tile/visible (aref (game-map/tiles map) x y))))
      (when in-fov
        (dolist (entity entities)
          (when ( and (= (entity/x entity) x)
                      (= (entity/y entity) y))
            (setf names (append names (list (entity/name entity)))))))
      (format nil "~{~A~^, ~}" names))))
