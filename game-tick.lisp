(in-package #:roguelike-2021)

(defun player-turn (game-state map player action)
  (let ((player-turn-results nil)
        (move (getf action :move))
        (pickup (getf action :pickup))
        (show-inventory (getf action :show-inventory))
        (drop-inventory (getf action :drop-inventory)))
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

    (when show-inventory
      (with-slots (previous-state state) game-state
        (setf previous-state state
              state :show-inventory)))

    (when drop-inventory
      (with-slots (previous-state state) game-state
        (setf previous-state state
              state :drop-inventory)))

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
           (dead-entity (getf enemy-turn-results :dead))
           (decay (getf enemy-turn-results :decay)))
      (when message
        (add-message log message))
      (when decay
        (setf (game-state/entities game-state)
              (remove-if #'(lambda (e) (eql e decay))
               (game-state/entities game-state)))
        (setf message (format nil "~A decays away~%" (describe-entity decay)))
        (add-message log message :color (blt:orange)))
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
         (inventory-index (getf action :inventory-index))
         (exit (getf action :quit)))

    (when (eql (game-state/state game-state) :player-turn)
      (setf (values player-turn-results game-state)
            (player-turn game-state map player action)))

    (when exit
      (if (or (eql (game-state/state game-state) :show-inventory)
              (eql (game-state/state game-state) :drop-inventory))
        (setf (game-state/state game-state) (game-state/previous-state game-state))
        (setf (game-state/running game-state) nil)))

    (when (and inventory-index
               (not (eql (game-state/previous-state game-state) :player-dead))
               (< inventory-index (length (inventory/items (entity/inventory player)))))
      (let ((item (nth inventory-index (inventory/items (entity/inventory player)))))
        (cond
          ((eql (game-state/state game-state) :show-inventory)
           (let ((use-result (funcall (item/use-function (entity/item item))
                                     (entity/item item) player)))
             (setf player-turn-results use-result)
             (when (getf use-result :consumed)
                 (setf (game-state/state game-state) :enemy-turn)
                 (setf (inventory/items (entity/inventory player))
                       (remove-if #'(lambda (i)
                                            (eql i item))
                                  (inventory/items (entity/inventory player)))))))
          ((eql (game-state/state game-state) :drop-inventory)
           (let ((drop-result (drop-item (entity/inventory player) item)))
             (setf player-turn-results drop-result)
             (when (getf drop-result :item-dropped)
               (setf (game-state/state game-state) :enemy-turn)
               (setf (game-state/entities game-state)
                     (append (game-state/entities game-state)
                             (list (getf drop-result :item-dropped))))))))))


    (setf game-state (handle-player-results game-state player player-turn-results log)))

  (when (eql (game-state/state game-state)  :enemy-turn)
    (setf game-state (enemy-turn game-state player map log))
    (when (eql (game-state/state game-state) :player-dead)
      (return-from game-tick game-state))
    (setf (game-state/state game-state) :player-turn))

  game-state)
