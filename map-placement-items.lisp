(in-package #:roguelike-2021)

(defstruct item-def
  (chance 0)
  (entity)
  (item))

(defparameter *items-list*
  (list
   (make-item-def
    :chance 50
    :entity (list
             :name "Purple Healing Potion"
             :color (blt:purple)
             :char #\!
             :blocks nil
             :render-order :item)
    :item (list
           :use-function #'heal
           :use-args '(:heal-amount 4)))
   (make-item-def
    :chance 50
    :entity (list
             :name "Blue Healing Potion"
             :color (blt:blue)
             :char #\!
             :blocks nil
             :render-order :item)
    :item (list
           :use-function #'heal
           :use-args '(:heal-amount 4)))
   (make-item-def
    :chance 50
    :entity (list
             :name "Green Potion"
             :color (blt:green)
             :char #\!
             :blocks nil
             :render-order :item)
    :item (list
           :use-function #'recolor
           :use-args (list :new-color (blt:green))))))


(defun get-item (items &key (chance nil))
  (let ((max_chance 0))
    (dolist (item items)
      (incf max_chance (item-def-chance item)))
    (let ((select (if chance chance (random max_chance))))
      (dolist (item items)
        (when (> (item-def-chance item) select)(return item))
        (decf select (item-def-chance item))))))

(defun place-item (entities x y item)
  (let* ((item-component (apply #'make-instance 'item (item-def-item item))))
    (nconc entities (list (apply 'make-instance 'entity
                            (append (item-def-entity item)
                                    (list
                                     :x x :y y
                                     :item item-component)))))))

(defun place-items (room entities num-items)
  (dotimes (item-index num-items)
    (multiple-value-bind (x y) (rect/random room)
      (unless (entity-at entities x y)
        (place-item entities x y (get-item *items-list*))))))
