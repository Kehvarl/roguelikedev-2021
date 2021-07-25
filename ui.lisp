(in-package #:roguelike-2021)

(defclass panel ()
  ((x :initarg :x :accessor panel/x)
   (y :initarg :x :accessor panel/y)
   (width :initarg :width :accessor panel/width)
   (height :initarg :height :accessor panel/height)
   (components :initarg :components :accessor panel/components :initform nil)))

(defmethod print-object ((object panel) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y width height) object
      (format stream "(~A,~A) ~Ax~A" x y width height))))

(defun make-panel (x y width height)
  (make-instance 'panel
                 :x x :y y :width width :height height))

(defclass panel-component ()
  ((panel :initarg :panel :accessor :panel-component/panel)
   (x :initarg :x :accessor panel-component/x)
   (y :initarg :y :accessor panel-component/y)))
