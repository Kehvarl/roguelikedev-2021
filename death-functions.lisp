(in-package #:roguelike-2021)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))
  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks render-order ai name descriptor) monster
    (let ((message (format nil "~A is dead!~%" name))
          (dead-ai (make-instance 'dead-monster)))
      (setf char #\%
            color (blt:red)
            blocks nil
            render-order :corpse
            ai dead-ai
            (component/owner ai) monster
            descriptor "corpse")
      message)))
