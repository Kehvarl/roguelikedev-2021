(in-package #:roguelike-2021)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))

(defmethod initialize-instance :after ((fighter fighter) &rest initargs)
  (declare (ignore initargs))
  (with-slots (hp max-hp) fighter
    (unless max-hp
      (setf max-hp hp))))

(defgeneric take-damage (component amount))
(defgeneric attack (component target))

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

(defclass door (component)
  ((open :initarg :open :accessor door/open :initform nil)
   (locked :initarg :locked :accessor door/locked :initform nil)))

(defgeneric door/blocks (component))
(defmethod door/blocks ((component door))
  (not (door/open component)))
