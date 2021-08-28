(in-package #:roguelike-2021)

(defclass effect (component)
  ((name :initarg :name :accessor effect/name :initform "")))

(defgeneric process-effect (effect entity))

(defmethod process-effect ((effect effect) (entity entity))
  (declare (ignore entity))
  (list :message (format nil "Default Effect")))

(defclass colorshift (effect)
  ((previous-color :initarg :previous-color :accessor colorshift/previous-color)
   (duration :initarg :duration :accessor colorshift/duration :initform nil)))

(defmethod process-effect ((effect colorshift) entity)
  (let ((results nil))
    (with-slots (previous-color duration) effect
      (when duration
        (decf duration)
        (when (<= duration 0)
          (setf (entity/color entity) previous-color)
          (remove-effect (entity/effects entity) effect)
          (append results (list :message (format nil "An Effect has ended")))))
      results)))


(defclass active-effects (component)
  ((capacity :initarg :capacity :accessor active-effects/capacity :initform 1)
   (effects :initarg :effects :accessor active-effects/effects :initform nil)))

(defgeneric add-effect (effects effect))
(defgeneric remove-effect (effects effect))
(defgeneric process-effects (effects owner))

(defmethod add-effect ((effects active-effects) (effect effect))
  (let ((results nil))
    (with-slots (effects capacity) effects
      (cond
        ((>= (length effects) capacity)
         (setf results (list :effect-added nil
                             :message "You cannot have any more effects active.")))
        (t (setf results (list :effect-added effect
                            :message (format nil
                                             "You are under the influence of ~A"
                                             (effect/name effect))
                            :message-color (blt:yellow)))))
      (setf effects (append effects (list effect))))))

(defmethod remove-effect ((effects active-effects) (effect effect))
  (let ((results nil))
    (with-slots (effects) effects
      (setf effects (remove-if #'(lambda (e)
                                         (eql e effect))
                               effects)
            results (list :efffect-removed effect
                          :message (format nil "~A no longer affects you."
                                           (effect/name effect))
                          :message-color (blt:yellow))))))

(defmethod process-effects ((effects active-effects) owner)
  (let ((results nil))
    (dolist (effect (active-effects/effects effects))
      (append results (process-effect effect owner)))
    results))
