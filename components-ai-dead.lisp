(in-package #:roguelike-2021)

(defparameter *decay-states*
  '(:decay-corpse (list "corpse" :decay-skeleton)
    :decay-skeleton ( list "skeleton" :decay-bones)
    :decay-bones (list "bones" :decay-remains)
    :decay-remains (list "scattered remains" nil)))

(defclass dead-monster (basic-monster)
  ((state :initarg :state :accessor dead-monster/state :initform :decay-corpse)
   (count :initarg :count :accessor dead-monster/count :initform 5)
   (next-state :initarg :next-state
               :accessor dead-monster/next-state
               :initform :decay-skeleton)))

(defmethod take-turn ((component dead-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-range (<= (distance-to monster target)
                       (* 2 (ai/active-range (entity/ai monster))))))
    (when in-range
      (setf results (decay component target map entities))

     results)))

(defmethod decay ((component dead-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component)))

    (with-slots (state count next-state) component
       (decf count)
       (when (and (not next-state)
                  (<= count 0))
           (setf results (list :decay monster)))

       (when (and next-state
                    (<= count 0))
           (let ((state-results (getf *decay-states* next-state))
                 (prev (describe-entity monster)))
             (setf count 5
                   (entity/descriptor monster) (nth 1 state-results)
                   state next-state
                   next-state (nth 2 state-results))

             (setf results (list :message
                                   (format nil "The ~A decays to a ~A" prev
                                             (describe-entity monster)))))))
    results))

(defclass dead-monster-regenerating (basic-monster)
  ((state :initarg :state :accessor dead-monster/state :initform :decay-corpse)
   (count :initarg :count :accessor dead-monster/count :initform 5)
   (next-state :initarg :next-state
               :accessor dead-monster/next-state
               :initform :decay-skeleton)))

(defmethod take-turn ((component dead-monster-regenerating) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (count (dead-monster/count component))
         (in-range (<= (distance-to monster target)
                       (* 2 (ai/active-range (entity/ai monster)))))
         (in-sight (tile/visible (aref (game-map/tiles map)
                                       (entity/x monster)
                                       (entity/y monster)))))
    (when in-range (decf count))

    (unless in-sight
      (when (and in-range
                 (<= count 0))
        (setf count 5)
        (format t "~A might regenerate" (describe-entity monster))
        (setf results (decay component target map entities))))

    (when (and in-sight
               (<= count 0))
      (setf count 5)
      (setf results (decay component target map entities)))))
