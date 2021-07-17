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

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~AX~A" *screen-width* *screen-height*)
  (blt:set "window.title = Roguelike 2021"))

(defun main ()
  (blt:with-terminal
   (config)
   (let* ((fighter-component (make-instance 'fighter
                                            :hp 30
                                            :defense 2
                                            :power 5))
          (player (make-instance 'entity
                                 :name "Player"
                                 :x (/ *screen-width* 2)
                                 :y (/ *screen-height* 2)
                                 :char #\@
                                 :color (blt:white)
                                 :blocks t
                                 :fighter fighter-component))
          (entities (list player))
          (map (make-instance 'game-map :w *map-width* :h *map-height*)))
     (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
     (fov map (entity/x player) (entity/y player))
     (do ((game-state :player-turn (game-tick player entities map game-state)))
       ((eql game-state :exit))))))
