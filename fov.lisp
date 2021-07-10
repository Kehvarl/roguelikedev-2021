(in-package #:roguelike-2021)

(defparameter *fov-distance* 5)

(defun reset-visibility (map)
  "Clear the visibility status on all tiles on our map"
  (map-tiles-loop (map tile)
    (setf (tile/visible tile) nil)))

(defun degree-to-radian (degree)
  (* degree (/ pi 180)))

(defun diagonal-distance (x0 y0 x1 y1)
  "Define a squarish field of view"
  (let ((dx (- x0 x1))
        (dy (- y0 y1)))
    (max (abs dx) (abs dy))))

(defun lerp (start end time)
  "Linear Interpolation Between possible points"
  (+ start (* time (- end start))))

(defun fov (map x y)
  "Calculate the FoV for the entity at x,y and mark the appropriate tiles visible"
  (reset-visibility map)

  ;; loop over 360 degrees
  (dotimes (degree 360)
           (let* ((rad (degree-to-radian degree))
                  ;; Get the next point  on the outer-edge of the illluminated
                  ;; area, as well as the distance along the line to light.
                  (nx (round (+ (* (cos rad) *fov-distance*) x)))
                  (ny (round (+ (* (sin rad) *fov-distance*) y)))
                  (d (diagonal-distance x y nx ny)))
             (dotimes (tile d)
               (let ((tx (round (lerp x nx (/ tile d))))
                     (ty (round (lerp y ny (/ tile d)))))
                 ;; If we're past the edge of the map, stop tracing the line
                 (if (or (< tx 0) (> tx (game-map/w map)))
                   (return))
                 (if (or (< ty 0) (> ty (game-map/h map)))
                   (return))

                 ;; if the tile is a wall, mark it as lit and stop tracing the line
                 (when (tile/block-sight (aref (game-map/tiles map) tx ty))
                   (setf (tile/visible (aref (game-map/tiles map) tx ty)) t)
                   (return))
                 ;; Otherwise, mark the tile as lit and continue
                 (setf (tile/visible (aref (game-map/tiles map) tx ty)) t))))))
