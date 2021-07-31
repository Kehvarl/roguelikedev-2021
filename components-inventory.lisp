(in-package #:roguelike-2021)

(defclass inventory (component)
  ((capacity :initarg :capacity :accessor inventory/capacity)
   (items :initarg :items :accessor inventory/items :initform nil)))

(defgeneric add-item (inventory item))
(defgeneric drop-item (inventory item))

(defmethod add-item ((inventory inventory) (item entity))
  (let ((results nil))
    (with-slots (items capacity) inventory
      (cond
        ((>= (length items) capacity)
         (setf results (list :item-added nil
                             :message "You cannot carry more, your inventory is full.")))
        (t
         (setf results (list :item-added item
                             :message (format nil "You pick up the ~A" (entity/name item))
                             :message-color (blt:yellow)))
         (setf items (append items (list item))))))
    results))

(defmethod drop-item ((inventory inventory) (item entity))
  (let ((results nil))
    (with-slots (items) inventory
      (with-slots (x y) (component/owner inventory)
        (setf (entity/x item) x
              (entity/y item) y
              items (remove-if #'(lambda (i)
                                         (eql i item))
                               items)
              results (list :item-dropped item
                            :message (format nil "You dropped the ~A"
                                             (entity/name item))
                            :message-color (blt:yellow)))))))
