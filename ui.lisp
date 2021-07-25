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

(defclass bar (panel-component)
  ((name :initarg :name :accessor bar/name)
   (total-width :initarg :total-width :accessor bar/total-width)
   (value :initarg :value :accessor bar/value)
   (value-bind :initarg :value-bind)
   (maximum :initarg :maximum :accessor bar/maximum)
   (maximum-bind :initarg :maximum-bind)
   (color :initarg :color :accessor bar/color)
   (bg-color :initarg :bg-color :accessor bar/bg-color)))

(defmethod print-object ((object bar) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name total-width value maximum) object
      (format stream "~A ~Aw ~A/~A" name total-width value maximum))))

(defun make-bar (name panel x y total-width value color bg-color
                      &key (value-bind nil) (max-bind nil))
  (let ((bar (make-instance 'bar :name name :panel panel
                            :x x :y y :total-width total-width
                            :value value :maximum value
                            :value-bind value-bind
                            :max-bind max-bind
                            :color color :bg-color bg-color)))
    (setf (panel/components panel) (append (panel/components panel)
                                           (list bar)))))
