(in-package #:roguelike-2021)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))
  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks render-order ai regenerating name descriptor) monster
    (let ((message (format nil "~A is dead!~%" name))
          (dead-ai (if regenerating
                     (make-instance 'dead-monster-regenerating)
                     (make-instance 'dead-monster))))
      (setf char #\%
            color (blt:red)
            blocks nil
            render-order :corpse
            ai dead-ai
            (component/owner ai) monster
            descriptor "corpse")
      message)))

(defun resurrect-monster (monster)
  (with-slots (char color blocks render-order ai regenerating name descriptor) monster
    (let ((message (format nil "~A rises!~%" name))
          (ai-component (make-instance 'basic-monster :active-range 10)))
      (setf char #\R
            color (blt:red)
            blocks t
            render-order :actor
            ai ai-component
            (component/owner ai) monster
            descriptor "Risen")
      message)))
