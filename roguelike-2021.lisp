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
                                 :render-order :actor
                                 :fighter fighter-component))
          (entities (list player))
          (map (make-instance 'game-map :w *map-width* :h *map-height*))
          (stats-panel (make-panel 0 *map-height*
                                   *screen-width* (- *screen-height* *map-height*))))
     (make-bar "HP" stats-panel 1 1 15
               (fighter/hp fighter-component)
               (blt:rgba 0 128 0) (blt:rgba 100 100 100)
               :value-bind #'(lambda () (fighter/hp fighter-component))
               :max-bind #'(lambda () (fighter/max-hp fighter-component)))
     (make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
     (fov map (entity/x player) (entity/y player))
     (do ((game-state :player-turn (game-tick player entities map game-state stats-panel)))
       ((eql game-state :exit))))))
