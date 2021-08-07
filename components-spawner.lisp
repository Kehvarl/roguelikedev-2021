(in-package #:roguelike-2021)

(defclass spawner (component)
  ((room :accessor spawner/room)
   (frequency :initarg :frequency :accessor spawner/frequency)
   (tick :accessor spawner/tick :initform 0)))

(defgeneric spawn (component map entities))
(defmethod spawn ((component spawner) map entities)
  (with-slots (room tick frequency) component
    (incf (spawner/tick component))
    (when (> tick frequency)
      (setf (spawner/tick component) 0)
      (place-items room entities 1))))
