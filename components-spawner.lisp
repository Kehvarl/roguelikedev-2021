(in-package #:roguelike-2021)

(defclass spawner (component)
  ((room :accessor spawner/room)
   (region :accessor spawner/region)
   (frequency :initarg :frequency :accessor spawner/frequency)
   (max-entities :initarg :max-entities :accessor spawner/max-entities :initform 5)
   (tick :accessor spawner/tick :initform 0)))

(defgeneric spawn (component map entities))
(defmethod spawn ((component spawner) map entities)
  (with-slots (room region tick frequency max-entities) component
    (incf (spawner/tick component))
    (when (> tick frequency)
      (setf (spawner/tick component) 0)
      (unless (> (entities-in-region map entities region) max-entities)
        (place-items room entities 1)))))
