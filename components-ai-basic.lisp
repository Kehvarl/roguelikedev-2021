(in-package #:roguelike-2021)

(defclass basic-monster (component)
  ((activation-range :initarg :active-range :accessor ai/active-range :initform 5)))

(defgeneric take-turn (component target map entities))
(defgeneric take-damage (component amount))
(defgeneric attack (component target))

(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-range (<= (distance-to monster target)
                       (ai/active-range (entity/ai monster))))
         (in-sight (tile/visible (aref (game-map/tiles map)
                                       (entity/x monster)
                                       (entity/y monster)))))

    (unless in-sight
      (when in-range
        (let ((direction (nth (random (length *all-directions*)) *all-directions*)))
          (move-safe monster (car direction) (cdr direction) map entities))))

    (when in-sight
       (cond ((>= (distance-to monster target) 2)
              (move-towards monster (entity/x target) (entity/y target) map entities))

             ((> (fighter/hp (entity/fighter target)) 0)
              (setf results (attack (entity/fighter monster) target)))))
    results))

(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount)
  (let ((results nil))
    (when (<= (fighter/hp component) 0)
      (setf results (list :dead (component/owner component))))
    results))

(defmethod attack ((component fighter) (target entity))
  (let ((results nil)
        (damage (- (fighter/power component)
                   (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (setf results (append (list :message
                                   (format nil "~A attacks ~A for ~A damage.~%"
                                           (entity/name (component/owner component))
                                           (entity/name target)
                                           damage))
                      (take-damage (entity/fighter target) damage))))
      (t
       (setf results (list :message (format nil "~A attachs ~A but does no damage.~%"
                                            (entity/name (component/owner component))
                                            (entity/name target))))))
    results))