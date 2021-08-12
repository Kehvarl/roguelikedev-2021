(in-package #:roguelike-2021)

(defparameter *decay-states*
  '(:decay-corpse (list "corpse" :decay-skeleton)
    :decay-skeleton ( list "skeleton" :decay-bones)
    :decay-bones (list "bones" :decay-remains)
    :decay-remains (list "scattered remains" nil)
    :decay-hold (list nil :decay-hold)))

(defclass dead-monster (basic-monster)
  ((previous-ai :initarg :previous-ai :accessor dead-monster/previous-ai :initform nil)
   (previous-name :initarg :previous-name :accessor dead-monster/previous-name :initform nil)
   (previous-char :initarg :previous-char :accessor dead-monster/previous-char :initform nil)
   (state :initarg :state :accessor dead-monster/state :initform :decay-corpse)
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
      (decf (dead-monster/count component))
      (setf results (decay component target map entities))

     results)))

(defmethod decay ((component dead-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component)))

    (with-slots (state count next-state) component
       (when (and (not next-state)
                  (<= count 0))
           (setf results (list :decay monster)))

       (when (and next-state
                    (<= count 0))
           (let ((state-results (getf *decay-states* next-state))
                 (prev (describe-entity monster)))
             (if (and (not (entity/spawner (component/owner component)))
                      (< (random 100) 5))
               (progn
                (setf state-results (getf *decay-states* :decay-hold))
                (vermin-spawner component map)))

             (setf count 5
                  state next-state
                  next-state (nth 2 state-results))
             (if (nth 1 state-results)
               (setf (entity/descriptor monster) (nth 1 state-results)))

             (unless (eql state-results :decay-hold)
               (setf results (list :message
                              (format nil "The ~A decays to a ~A" prev
                               (describe-entity monster))))))))
    results))

(defgeneric vermin-spawner (component map))
(defmethod vermin-spawner ((component dead-monster) map)
  (let* ((monster (component/owner component))
         (region (region-at map (entity/x monster) (entity/y monster)))
         (spawner-component (make-instance 'spawner :frequency 10
                                           :max-entities 5
                                           :region region
                                           :spawn-args `(:hp 3 :defense 3
                                                         :power 3 :name "Rat"
                                                         :char #\r
                                                         :color ,(blt:gray)))))
    (setf (component/owner spawner-component) monster
          (entity/spawner monster) spawner-component)))



(defclass dead-monster-regenerating (dead-monster)
  ())


(defmethod take-turn ((component dead-monster-regenerating) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-range (<= (distance-to monster target)
                       (* 2 (ai/active-range (entity/ai monster)))))
         (in-sight (tile/visible (aref (game-map/tiles map)
                                       (entity/x monster)
                                       (entity/y monster)))))
    (when in-range
      (decf (dead-monster/count component))
      (if in-sight
        (setf results (decay component target map entities))
        (setf results (regenerate component))))
    results))

(defmethod regenerate ((component dead-monster-regenerating))
  (let* ((results nil)
         (monster (component/owner component))
         (new-hp (1+ (fighter/hp (entity/fighter monster)))))
    (setf (fighter/hp (entity/fighter monster)) new-hp)
    (if (>= new-hp 5)
      (setf results (list :message (resurrect-monster monster))))

    results))
