(in-package #:roguelike-2021)

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :lit-wall (blt:rgba 130 110 50)
                                :lit-ground (blt:rgba 200 180 50)))

(defun render-all (entities map)
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


  (mapc #'(lambda (entity) (draw entity map)) entities)
  (setf (blt:background-color) (blt:black))
  (blt:refresh))