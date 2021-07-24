(in-package #:roguelike-2021)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map entities))
(defgeneric take-damage (component amount))
(defgeneric attack (component target))

(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map)
                                       (entity/x monster)
                                       (entity/y monster)))))
    (when in-sight

      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))

            ((> (fighter/hp (entity/fighter target)) 0)
             (attack (entity/fighter monster) target))))))

(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount))

(defmethod attack ((component fighter) (target entity))
  (let ((damage (- (fighter/power component)
                   (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (take-damage (entity/fighter target) damage)
       (format t "~A attacks ~A for ~A hit points.~%"
                (entity/name (component/owner component))
                (entity/name target)
                damage))
      (t
       (format t "~A attachs ~A but does no damage.~%"
               (entity/name (component/owner component))
               (entity/name target))))))
