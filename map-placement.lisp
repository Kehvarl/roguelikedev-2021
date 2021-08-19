(in-package #:roguelike-2021)

(defun place-monsters (room entities num-monsters)
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (rect/random room)
        (unless (entity-at entities x y)
          (let ((monster-rand (random 100)))
            (cond
              ((< monster-rand 70)
               (let* ((fighter-component (make-instance 'fighter :hp 10
                                                        :defense 0 :power 3))
                      (ai-component (make-instance 'tracking-monster :active-range 5)))
                 (nconc entities (list (make-instance 'entity :name "Orc"
                                                      :x x :y y :color (blt:green)
                                                      :char #\o :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component)))))

              ((< monster-rand 90)
               (let* ((fighter-component (make-instance 'fighter :hp 16
                                                        :defense 1 :power 4))
                      (ai-component (make-instance 'basic-monster :active-range 10)))
                 (nconc entities (list (make-instance 'entity :name "Troll"
                                                      :x x :y y :color (blt:yellow)
                                                      :char #\T :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component
                                                      :regenerating t)))))
              (t
               (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1
                                                        :power 4))
                      (ai-component (make-instance 'ranged-monster :active-range 10)))
                 (nconc entities (list (make-instance 'entity :name "Curious Light"
                                                      :x x :y y :color (blt:chartreuse)
                                                      :char #\c :blocks t
                                                      :render-order :actor
                                                      :fighter fighter-component
                                                      :ai ai-component
                                                      :regenerating t)))))))))))

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