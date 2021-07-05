;;;; rogulike 2021.lisp

(in-package #:roguelike-2021)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 6))

(defparameter *map* nil)

(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)))

(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
      (let* ((tile (aref (game-map/tiles map) x y))
             (wall (tile/blocked tile)))
        (if wall
          (setf (blt:background-color) (getf *color-map* :dark-wall))
          (setf (blt:background-color) (getf *color-map* :dark-ground)))
        (setf (blt:cell-char x y) #\Space))))

  (mapc #'draw entities)
  (blt:refresh))

(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:up (setf action (list :move (cons 0 -1))))
                  (:down (setf action (list :move (cons 0 1))))
                  (:left (setf action (list :move (cons -1 0))))
                  (:right (setf action (list :move (cons 1 0))))
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~AX~A" *screen-width* *screen-height*)
  (blt:set "window.title = Roguelike 2021"))

(defun main ()
  (blt:with-terminal
   (config)
   (setf *map* (make-instance 'game-map :w *map-width* :h *map-height*))
   (initialize-tiles *map*)
   (loop :with player = (make-instance 'entity
                                       :x (/ *screen-width* 2)
                                       :y (/ *screen-height* 2)
                                       :char #\@
                                       :color (blt:white))
     :and npc = (make-instance 'entity
                               :x (- (/ *screen-width* 2) 5)
                               :y (/ *screen-height* 2)
                               :char #\@
                               :color (blt:yellow))
     :with entities = (list player npc)

     :do
     (render-all entities *map*)
     (let* ((action (handle-keys))
            (move (getf action :move))
            (exit (getf action :quit)))
       (if exit
         (return))
       (when move
         (move player (car move) (cdr move)))))))
