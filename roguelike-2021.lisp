;;;; rogulike 2021.lisp

(in-package #:roguelike-2021)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 6))

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)
(defparameter *max-enemies-per-room* 6)

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

(defun game-tick (player entities map)
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
           (unless (blocked-p map destination-x destination-y)
             (let ((target (blocking-entity-at entities destination-x destination-y)))
               (cond (target
                      (format t "You kick the ~A.~%" (entity/name target)))
                     (t
                      (move player (car move) (cdr move))
                      (fov map (entity/x player) (entity/y player))))))))
    exit))

(defun main ()
  (blt:with-terminal
   (config)
   (let* ((player (make-instance 'entity
                                 :name "Player"
                                 :x (/ *screen-width* 2)
                                 :y (/ *screen-height* 2)
                                 :char #\@
                                 :color (blt:white)
                                 :blocks t))
          (entities (list player))
          (map (make-instance 'game-map :w *map-width* :h *map-height*)))
     (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
     (fov map (entity/x player) (entity/y player))
     (do ((exit nil (game-tick player entities map)))
       (exit)))))
