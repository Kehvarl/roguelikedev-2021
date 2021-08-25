(in-package #:roguelike-2021)

(defclass item (component)
  ((use-function :initarg :use-function :accessor item/use-function :initform nil)
   (use-args :initarg :use-args :accessor item/use-args :initform nil)))

(defun heal (item target)
  (let ((amount (getf (item/use-args item) :heal-amount)))
    (with-slots (hp max-hp) (entity/fighter target)
      (cond
        ((= hp max-hp)
         (list :consumed nil :message "You are already at full health"
               :message-color (blt:yellow)))
        (t
         (incf hp amount)
         (when (> hp max-hp)
           (setf hp max-hp))
         (list :consumed t :message "Your wounds start to feel better!"
               :message-color (blt:green)))))))

(defun recolor (item target)
  (let ((new-color (getf (item/use-args item) :new-color)))
    (setf (entity/color target) new-color)
    (list :consumed t :message "You feel strange..."
          :message-color (blt:green))))
