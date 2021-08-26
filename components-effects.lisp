(in-package #:roguelike-2021)

(defclass effect (component) ())

(defclass active-effects (component)
  ((capacity :initarg :capacity :accessor active-effects/capacity :initform 1)
   (effects :initarg :effects :accessor active-effects/effects :initform nil)))

(defgeneric add-effect (effects effect))
(defgeneric remove-effect (effects effect))
(defgeneric process-effects (effects))

(defmethod add-effect ((effects active-effects) (effect effect))
  (declare (ignore effects effect)))
