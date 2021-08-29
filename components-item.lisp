(in-package #:roguelike-2021)

(defclass item (component)
  ((use-function :initarg :use-function :accessor item/use-function :initform nil)
   (use-args :initarg :use-args :accessor item/use-args :initform nil)))

(defun heal (item target)
  (let ((amount (getf (item/use-args item) :heal-amount)))
    (gain-hp (entity/fighter target) amount)))

(defun heal-duration (item target)
  (let ((amount (getf (item/use-args item) :heal-amount))
        (duration (getf (item/use-args item) :duration))
        (message (getf (item/use-args item) :message)))
    (let ((effect (make-instance 'healeffect
                                 :heal-amount amount
                                 :duration duration)))
      (add-effect (entity/effects target) effect))
    (list :consumed t :message message :message-color (blt:green))))

(defun recolor (item target)
  (let ((new-color (getf (item/use-args item) :new-color)))
    (let ((effect (make-instance 'colorshift
                                 :previous-color (entity/color target)
                                 :duration 5)))
      (add-effect (entity/effects target) effect))
    (setf (entity/color target) new-color)
    (list :consumed t :message "You feel strange..."
          :message-color (blt:green))))
