;;;; rogulike 2021.lisp

(in-package #:roguelike-2021)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)


(defclass entity()
  ((x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)))

(defmethod draw ((e entity))
  (with-slots (x y char color) e
   (setf (blt:color) color
         (blt:cell-char x y) char)))

(defmethod move ((e entity) dx dy)
  (incf (entity/x e) dx)
  (incf (entity/y e) dy))


(defun render-all (entities)
 (blt:clear)
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
   (loop :with player = (make-instance 'entity
                                       :x (/ *screen-width* 2)
                                       :y (/ *screen-height* 2)
                                       :char #\@
                                       :color (blt:white))
     :do
     (render-all (list player))
     (let* ((action (handle-keys))
            (move (getf action :move))
            (exit (getf action :quit)))
       (if exit
         (return))
       (when move
         (move player (car move) (cdr move)))))))
