(in-package #:roguelike-2021)

(defparameter *fov-distance* 5)

(defun reset-visibility (map)
  "Clear the visibility status on all tiles on our map"
  (map-tiles-loop (map tile)
    (setf (tile/visible tile) nil)))

(defun fov (map x y)
  (reset-visibility map))
