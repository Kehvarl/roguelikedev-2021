(in-package #:roguelike-2021)

(defclass tile ()
  ((blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
                :initform nil)
   (visible :initarg :visible
            :accessor tile/visible
            :initform nil)))

(defmethod initialize-instance :after ((tile tile) &rest initargs)
  (declare (ignore initargs))
  (with-slots (blocked block-sight) tile
    (if (null block-sight)
      (setf block-sight blocked))))

(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p)
                                       (block-sight nil block-sight-supplied-p))
  (if blocked-supplied-p
    (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
    (setf (slot-value tile 'block-sight) block-sight)))
