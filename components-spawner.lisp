(in-package #:roguelike-2021)

(defclass spawner (component)
  ((room :accessor spawner/room)
   (region :initarg :region :accessor spawner/region)
   (frequency :initarg :frequency :accessor spawner/frequency)
   (max-entities :initarg :max-entities :accessor spawner/max-entities
                 :initform 5)
   (spawn-args :initarg :spawn-args :accessor spawner/spawn-args
               :initform nil)
   (tick :accessor spawner/tick :initform 0)))

(defgeneric spawn (component map entities))
(defmethod spawn ((component spawner) map entities)
  (with-slots (room region tick frequency max-entities spawn-args) component
    (incf (spawner/tick component))
    (when (> tick frequency)
      (setf (spawner/tick component) 0)
      (unless (> (length (entities-in-region map (remove-if
                                                  #'(lambda (e)
                                                            (entity/spawner e))
                                                  entities)
                                     region))
                max-entities)
        (if spawn-args
          (spawn-monster component spawn-args entities map)
          (place-items room entities 1))))))

(defun spawn-monster (spawner spawn-args entities map)
 (let* ((fighter-component (make-instance 'fighter :hp (getf spawn-args :hp)
                                          :defense (getf spawn-args :defense)
                                          :power (getf spawn-args :power)))
        (ai-component (make-instance 'basic-monster :active-range 5)))
   (multiple-value-bind (x y)(random-in-region map (spawner/region spawner))
     (unless (entity-at entities x y)
       (nconc entities (list (make-instance 'entity :name (getf spawn-args :name)
                                            :x x :y y
                                            :color (getf spawn-args :color)
                                            :char (getf spawn-args :char)
                                            :blocks t
                                            :render-order :actor
                                            :fighter fighter-component
                                            :ai ai-component)))))))

(defclass cloner (spawner)
  ((clone-region :initarg :clone-region :accessor cloner/clone-region)))

(defmethod spawn ((component cloner) map entities)
  (with-slots (region tick frequency max-entities spawn-args) component
    (incf (spawner/tick component))
    (when (> tick frequency)
      (setf (spawner/tick component) 0)
      (unless (> (length (entities-in-region map (remove-if
                                                  #'(lambda (e)
                                                            (entity/spawner e))
                                                  entities)
                                             region))
                 max-entities))
      (let ((clonable
             (entities-in-region map (remove-if-not #'entity/fighter
                                                    entities)
                                 (cloner/clone-region component))))
        (when (> (length clonable) 0)
          (let* ((clone (nth (random (length clonable)) clonable))
                 (fighter (entity/fighter clone))
                 (spawn-args '(:hp (fighter/hp fighter)
                               :defense (fighter/defense fighter)
                               :power (fighter/power fighter)
                               :name (entity/name clone)
                               :char (entity/char clone)
                               :color (entity/color clone))))

            (spawn-monster component
                           spawn-args
                           entities map)
            (format t "Cloning ~A ~AHP~%" (entity/name clone) (fighter/hp fighter))))))))
