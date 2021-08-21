(in-package #:roguelike-2021)

(defstruct monster
  (chance 0)
  (entity ())
  (fighter ())
  (ai)
  (ai-args ()))

(defparameter *monsters-list*
  (list
   (make-monster
    :chance 70
    :entity (list
             :name "Orc"
             :color (blt:green)
             :char #\o
             :blocks t
             :render-order :actor)
    :fighter '(:hp 10 :defense 0 :power 3)
    :ai 'tracking-monster
    :ai-args '(:active-range 5))
   (make-monster
    :chance 20
    :entity (list
             :name "Troll"
             :color (blt:yellow)
             :char #\T
             :blocks t
             :regenerating t
             :render-order :actor)
    :fighter '(:hp 16 :defense 1 :power 4)
    :ai 'basic-monster
    :ai-args '(:active-range 5))
   (make-monster
    :chance 10
    :entity (list
             :name "Curious Light"
             :color (blt:cyan)
             :char #\c
             :blocks t
             :render-order :actor)
    :fighter '(:hp 10 :defense 1 :power 3)
    :ai 'ranged-monster
    :ai-args '(:active-range 5))))

(defun get-monster (monsters &key (chance nil))
  (let ((max_chance 0))
    (dolist (monster monsters)
      (incf max_chance (monster-chance monster)))
    (let ((select (if chance chance (random max_chance))))
      (dolist (monster monsters)
        (when (> (monster-chance monster) select)(return monster))
        (decf select (monster-chance monster))))))

(defun place-monster (entities x y monster)
  (let* ((ai-component (apply #'make-instance (monster-ai monster)
                          (monster-ai-args monster)))
         (fighter-component (apply #'make-instance 'fighter
                              (monster-fighter monster))))
    (nconc entities (list (apply #'make-instance 'entity
                            (append (monster-entity monster)
                                    (list
                                     :x x :y y
                                     :ai ai-component
                                     :fighter fighter-component)))))))

(defun place-monsters (room entities num-monsters)
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (rect/random room)
        (unless (entity-at entities x y)
          (place-monster
           entities x y (get-monster *monsters-list* :chance (random 100)))))))


(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (multiple-value-bind (x y) (rect/random room)
      (unless (entity-at entities x y)
        (let ((monster-rand (random 100)))
          (cond
            ((< monster-rand 70)
             (let* ((item-component (make-instance 'item :use-function #'heal
                                                   :use-args '(:heal-amount 4)))
                    (potion (make-instance 'entity :name "Healing Potion"
                                           :x x :y y :color (blt:purple)
                                           :item item-component
                                           :char #\! :blocks nil
                                           :render-order :item)))
               (nconc entities (list potion))))
            (t
             (format t "Monster spawner at ~A,~A~%" x y))))))))

(defun place-spawner (room entities spawner)
  (multiple-value-bind (x y) (rect/random room)
    (unless (entity-at entities x y)
      (setf (spawner/room spawner) room)
      (setf (spawner/region spawner) (rect/region room))
      (nconc entities (list (make-instance 'entity :x x :y y :color (blt:black)
                                           :spawner spawner
                                           :char #\space :blocks nil
                                           :render-order :item))))))

(defun place-door (entities x y)
  (let ((door-component (make-instance 'door :open nil :locked nil)))
    (nconc entities (list (make-instance 'entity :x x :y y :color (blt:yellow)
                                         :door door-component
                                         :char #\# :blocks nil
                                         :render-order :corpse)))))
