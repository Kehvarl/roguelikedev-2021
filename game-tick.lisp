(in-package #:roguelike-2021)

(deftype game-states () '(member :player-turn :enemy-turn :exit))

(defun handle-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  ((or :up :i) (list :move (cons 0 -1)))
                  ((or :down :comma) (list :move (cons 0 1)))
                  ((or :left :j) (list :move (cons -1 0)))
                  ((or :right :l) (list :move (cons 1 0)))
                  (:u (list :move (cons -1 -1)))
                  (:o (list :move (cons 1 -1)))
                  (:m (list :move (cons -1 1)))
                  (:period (list :move (cons 1 1)))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))

(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((player-turn-results nil)
         (action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))
    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
           (unless (blocked-p map destination-x destination-y)
             (let ((target (blocking-entity-at entities destination-x destination-y)))
               (cond (target
                      (setf player-turn-results (attack (entity/fighter player) target)))
                     (t
                      (move player (car move) (cdr move))
                      (fov map (entity/x player) (entity/y player)))))
            (setf game-state :enemy-turn))))
    (when exit
      (setf game-state :exit))

    (let ((message (getf player-turn-results :message))
          (dead-entity (getf player-turn-results :dead)))
      (when message
        (format t message))
      (when dead-entity)))
      ;; Do something cool here

  (when (eql game-state :enemy-turn)
    (dolist (entity (remove-if-not #'entity/ai entities))
          (let* ((enemy-turn-results (take-turn (entity/ai entity))
                          player map entities)
                 (message (getf enemy-turn-results :message))
                 (dead-entity (getf enemy-turn-results :dead)))
            (when message
              (format t message))
            (when dead-entity)))
              ;; Do something with these corpses soon!
    (setf game-state :player-turn))

  game-state)
