(in-package #:roguelike-2021)

(defclass spawner (component)
  ((frequency :initarg :frequency :accessor spawner/frequency)))

(defgeneric spawn (component map entities))
