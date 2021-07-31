(in-package #:roguelike-2021)

(defun handle-keys (game-state)
  (cond
    ((eql (game-state/state game-state) :player-turn)
     (handle-player-turn-keys))
    ((eql (game-state/state game-state) :player-dead)
     (handle-player-dead-keys))
    ((eql (game-state/state game-state) :show-inventory)
     (handle-inventory-keys))))

(defun handle-player-turn-keys ()
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
                  (:e (list :show-inventory t))

                  (:escape (list :quit t))
                  (:close (list :quit t)))))

(defun handle-player-dead-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  (:e (list :show-inventory t))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))

(defun handle-inventory-keys ()
  (when (blt:has-input-p)
    (let ((key (blt:read))
          (char-key (blt:character-input)))
      (when char-key
        (let ((index (- (char-code char-key) (char-code #\a))))
          (when (>= index 0)
            (return-from handle-inventory-keys (list :inventory-index index)))))
      (blt:key-case key
        (:escape (list :quit t))
        (:close (list :quit t))))))
