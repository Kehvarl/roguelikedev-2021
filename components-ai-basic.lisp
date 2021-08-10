(in-package #:roguelike-2021)

(defclass basic-monster (component)
  ((activation-range :initarg :active-range :accessor ai/active-range :initform 5)))

(defgeneric take-turn (component target map entities))

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

(defclass ranged-monster (basic-monster)
  ((target-range :initarg :target-range :accessor ranged-monster/target-range :initform 5)))

(defmethod take-turn ((component ranged-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-range (<= (distance-to monster target)
                       (ai/active-range (entity/ai monster))))
         (in-sight (tile/visible (aref (game-map/tiles map)
                                       (entity/x monster)
                                       (entity/y monster)))))

    (with-slots (target-range) component
      (unless in-sight
        (when in-range
          (let ((direction (nth (random (length *all-directions*)) *all-directions*)))
            (move-safe monster (car direction) (cdr direction) map entities))))

      (when in-sight
        (cond
          ((>= (distance-to monster target) target-range)
           (break)
           (move-towards monster (entity/x target) (entity/y target) map entities)))))
    results))

(defclass tracking-monster (basic-monster) ())

(defmethod take-turn ((component tracking-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-range (<= (distance-to monster target)
                       (ai/active-range (entity/ai monster)))))

    (when in-range


        (let ((tracks (track-around map (entity/x monster) (entity/y monster))))
          (unless tracks
            (let ((direction (nth (random (length *all-directions*))
                                  *all-directions*)))
              (move-safe monster (car direction) (cdr direction) map entities)))
          (when tracks
            (print tracks)
            (move-safe monster (car tracks) (cdr tracks) map entities))))

    results))

(defun track-around (map x y)
  (let ((result nil))
    (dolist (direction *all-directions*)
            (let ((track (tile/track (aref (game-map/tiles map)
                                           (+ (car direction) x)
                                           (+ (cdr direction) y)))))
             (when track
               (if result
                 (when (> (cdr result) track)
                   (setf result (cons direction track)))
                 (setf result (cons direction track))))))
    (car result)))
