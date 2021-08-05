(in-package #:roguelike-2021)

(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))
  (values "You died!" :player-dead))

(defun kill-monster (monster)
  (with-slots (char color blocks render-order ai regenerating name descriptor) monster
    (let ((message (format nil "~A is dead!~%" name))
          (dead-ai (if regenerating
                     (make-instance 'dead-monster-regenerating :previous-ai ai
                                    :previous-name name :previous-char char)
                     (make-instance 'dead-monster :previous-ai ai
                                    :previous-name name :previous-char char))))
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
          (ai-component (dead-monster/previous-ai ai)))
      (setf char (if (dead-monster/previous-char ai)
                   (dead-monster/previous-char ai)
                   #\R)
            name (if (dead-monster/previous-name ai)
                    (dead-monster/previous-name ai)
                    #\z)
            color (blt:red)
            blocks t
            render-order :actor
            ai ai-component
            (component/owner ai) monster
            descriptor "Risen")
      message)))
