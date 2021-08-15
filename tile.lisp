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
            :initform nil)
   (explored :initarg :explored
             :accessor tile/explored
             :initform nil)
   (region :initarg :region
           :accessor tile/region
           :initform nil)
   (room :initarg :room
          :accessor tile/room
          :initform nil)
   (door :initarg :door
         :accessor tile/door
         :initform nil)
   (corridor :initarg :corridor
             :accessor tile/corridor
             :initform nil)
   (track :initarg :track
          :accessor tile/track
          :initform nil)))

(defmethod initialize-instance :after ((tile tile) &rest initargs)
  (declare (ignore initargs))
  (with-slots (blocked block-sight) tile
    (if (null block-sight)
      (setf block-sight blocked))))

(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p)
                                       (block-sight nil block-sight-supplied-p)
                                       (region nil region-supplied-p)
                                       (room nil room-supplied-p)
                                       (corridor nil corridor-supplied-p))
  (if blocked-supplied-p
    (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
    (setf (slot-value tile 'block-sight) block-sight))
  (if region-supplied-p
    (setf (slot-value tile 'region) region))
  (if room-supplied-p
    (setf (slot-value tile 'room) room))
  (if corridor-supplied-p
    (setf (slot-value tile 'corridor) corridor)))
