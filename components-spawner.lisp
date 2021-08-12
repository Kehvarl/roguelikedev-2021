(in-package #:roguelike-2021)

(defclass spawner (component)
  ((room :accessor spawner/room)
   (region :accessor spawner/region)
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
      (unless (> (entities-in-region map (remove-if
                                          #'(lambda (e)
                                                    (entity/spawner e))
                                          entities)
                                     region)
                 max-entities)
        (break)
        (if spawn-args
          (spawn-monster (component/owner component) spawn-args entities)
          (place-items room entities 1))))))

(defun spawn-monster (monster spawn-args entities)
 (let* ((x (+ (- (random 10) 5) (entity/x monster)))
        (y (+ (- (random 10) 5) (entity/y monster)))
        (fighter-component (make-instance 'fighter :hp (getf spawn-args :hp)
                                          :defense (getf spawn-args :defense)
                                          :power (getf spawn-args :power)))
        (ai-component (make-instance 'basic-monster :active-range 5)))
   (unless (entity-at entities x y)
     (nconc entities (list (make-instance 'entity :name (getf spawn-args :name)
                                          :x x :y y
                                          :color (getf spawn-args :color)
                                          :char (getf spawn-args :char)
                                          :blocks t
                                          :render-order :actor
                                          :fighter fighter-component
                                          :ai ai-component))))))
