(in-package #:roguelike-2021)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor figher/power)))

(defclass basic-monster (component) ())

(defgeneric take-turn (component))

(defmethod take-turn ((component basic-monster))
  (let ((monster (component/owner component)))
   (format t "The ~A wonders when it will get to move.~%"
           (entity/name monster))))
