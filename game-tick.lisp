(in-package #:roguelike-2021)

(deftype game-states () '(member :player-turn :enemy-turn :exit))

(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:up (setf action (list :move (cons 0 -1))))
                  (:down (setf action (list :move (cons 0 1))))
                  (:left (setf action (list :move (cons -1 0))))
                  (:right (setf action (list :move (cons 1 0))))
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))

(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
           (unless (blocked-p map destination-x destination-y)
             (let ((target (blocking-entity-at entities destination-x destination-y)))
               (cond (target
                      (format t "You kick the ~A.~%" (entity/name target)))
                     (t
                      (move player (car move) (cdr move))
                      (fov map (entity/x player) (entity/y player)))))
            (setf game-state :enemy-turn))))
    (when exit
      (setf game-state :exit)))

  (when (eql game-state :enemy-turn)
    (dolist (entity entities)
      (if (not (eql player entity))
          (format t "The ~A sits idly.~%" (entity/name entity))))
    (setf game-state :player-turn))

  game-state)
