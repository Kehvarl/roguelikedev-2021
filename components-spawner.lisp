(in-package #:roguelike-2021)

(defclass spawner (component)
  ((room :accessor spawner/room)
   (frequency :initarg :frequency :accessor spawner/frequency)
   (tick :accessor spawner/tick :initform 0)))

(defgeneric spawn (component map entities))
(defmethod spawn ((component spawner) map entities)
  (with-slots (tick frequency) component
    (incf (spawner/tick component))
    (when (> frequency tick)
      (setf (spawner/tick component) 0)
      (format t "Spawner triggers"))))
