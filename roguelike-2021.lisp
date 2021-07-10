;;;; rogulike 2021.lisp

(in-package #:roguelike-2021)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 6))

(defparameter *map* nil)

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
      (unless (blocked-p *map*
                         (+ (entity/x player) (car move))
                         (+ (entity/y player) (cdr move)))
        (move player (car move) (cdr move))))
    exit))

(defun main ()
  (blt:with-terminal
   (config)
   (setf *map* (make-instance 'game-map :w *map-width* :h *map-height*))
   (initialize-tiles *map*)
   (make-map *map*)
   (let* ((player (make-instance 'entity
                                 :x (/ *screen-width* 2)
                                 :y (/ *screen-height* 2)
                                 :char #\@
                                 :color (blt:white)))
          (npc (make-instance 'entity
                              :x (- (/ *screen-width* 2) 5)
                              :y (/ *screen-height* 2)
                              :char #\@
                              :color (blt:yellow)))
          (entities (list player npc)))
     (do ((exit nil (game-tick player entities *map*)))
       (exit)))))
