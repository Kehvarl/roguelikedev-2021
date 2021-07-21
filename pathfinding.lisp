(in-package #:roguelike-2021)

(defparameter *all-directions*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-pareng :accessor
                         node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (position parent) obj
      (format stream "~A, parent ~A" position parent))))

(defun node-equal (n1 n2)
  "Two nodes are equal if their positions are the same"
  (equal (node/position n1) (node/position n2)))

(defun node-compare (n1 n2)
  "Compares the F slots on two nodes and returns true if n1s F is less than n2s"
  (< (node/f n1) (node/f n2)))

(defun find-in-queue (queue n)
  "Finds the node N in the QUEUE by it's position.  Returns the last node if
there are multiple matches."
  (let ((node nil))
    (queues:map-queue #'(lambda (item)
                                (when (node-equal n item)
                                  (setf node item)))
                      queue)
    node))
