(in-package #:roguelike-2021)

(defun handle-keys (game-state)
  (declare (ignore game-state))
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
                  (:g (list :pickup t))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))

(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move))
        (pickup (getf action :pickup)))
    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at (game-state/entities game-state)
                                            destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player)
                                                     target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player)))))
         (setf (game-state/state game-state) :enemy-turn))))

    (when pickup
      (dolist (entity (remove-if-not #'entity/item (game-state/entities game-state)))
        (when (and (= (entity/x entity) (entity/x player))
                   (= (entity/y entity) (entity/y player)))
          (setf player-turn-results (add-item (entity/inventory player)
                                              entity)))))
    (values player-turn-results game-state)))

(defun handle-player-results (game-state player player-turn-results log)
  (let ((message (getf player-turn-results :message))
        (message-color (if (getf player-turn-results :message-color)
                           (getf player-turn-results :message-color)
                           (blt:white)))
        (dead-entity (getf player-turn-results :dead))
        (item-added (getf player-turn-results :item-added)))
    (when message
      (add-message log message :color message-color))
    (when dead-entity
      (cond
        ((equal dead-entity player)
         (setf (values message (game-state/state game-state))
               (kill-player dead-entity)))
        (t
         (setf message (kill-monster dead-entity))))
      (add-message log message :color (blt:orange)))
    (when item-added
      (setf (game-state/entities game-state)
            (remove-if #'(lambda (entity)
                                 (and (eql entity item-added)
                                      (entity/item entity)))
                       (game-state/entities game-state))
            (game-state/state game-state) :enemy-turn)))
  game-state)

(defun enemy-turn (game-state player map log)
  (dolist (entity (remove-if-not #'entity/ai (game-state/entities game-state)))
    (let* ((enemy-turn-results (take-turn (entity/ai entity)
                                          player map (game-state/entities game-state)))
           (message (getf enemy-turn-results :message))
           (dead-entity (getf enemy-turn-results :dead)))
      (when message
        (add-message log message))
      (when dead-entity
        (cond
          ((equal dead-entity player)
           (setf (values message (game-state/state game-state))
                 (kill-player dead-entity)))
          (t
             (setf message (kill-monster dead-entity))))
        (add-message log message :color (blt:red)))))
  game-state)


(defun game-tick (player map game-state stats-panel log)
  (declare (type game-state game-state))
  (declare (type message-log log))

  (render-all game-state player map stats-panel *screen-width* *screen-height*)

  (let* ((player-turn-results nil)
         (action (handle-keys game-state))
         (exit (getf action :quit)))

    (when (eql (game-state/state game-state) :player-turn)
      (setf (values player-turn-results game-state)
            (player-turn game-state map player action)))

    (when exit
      (if (or (eql (game-state/state game-state) :show-inventory)
              (eql (game-state/state game-state) :drop-inventory))
        (setf (game-state/state game-state) (game-state/previous-state game-state))
        (setf (game-state/running game-state) nil)))

    (setf game-state (handle-player-results game-state player player-turn-results log)))

  (when (eql (game-state/state game-state)  :enemy-turn)
    (setf game-state (enemy-turn game-state player map log))
    (when (eql (game-state/state game-state) :player-dead)
      (return-from game-tick game-state))
    (setf (game-state/state game-state) :player-turn))

  game-state)
