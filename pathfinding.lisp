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
   (distance-from-parent :initarg :distance-from-parent :accessor
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

(defun create-path (current-node)
  "Given a node, return a list of all parent nodes leading to it."
  (do ((path nil)
       (current current-node (node/parent current)))
    ((null current) (reverse path))
    (setf path (append path (list (node/position current))))))

(defun make-node (parent-node node-x node-y direction-from-parent)
  "Creates a NODE instance with the given PARENT, X, Y, and calculates
DISTANCE-FROM-PARENT"
  (let ((distance 10))
    (if (and (not (zerop (car direction-from-parent)))
             (not (zerop (cdr direction-from-parent))))
      (setf distance 14))
    (make-instance 'node :parent parent-node
                   :position (cons node-x node-y)
                   :distance-from-parent distance)))

(defun generate-node-cost (child current-node end-node)
  "Calculates and sets G, H, and F slots on child node."
  (with-slots (g h f position distance-from-parent) child
    (setf g (+ distance-from-parent (node/g current-node))
          h (+ (expt (- (car position) (car (node/position end-node))) 2)
               (expt (- (cdr position) (cdr (node/position end-node))) 2))
          f (+ g h))))

(defun update-open-queue (open-list child-node)
  "Uopdates an existing entry in OPEN-LIST if one exists that both matches
CHILD-NODE, and has a larger G value.  If there is no existing entry matching
CHILD-NODE, then it pushes CHILD-NODE onto OPEN-LIST"
  (let ((existing-child (find-in-queue open-list child-node)))
    (cond
      ((and existing-child (< (node/g child-node) (node/g existing-child)))
       (queues:queue-change open-list
                            (queues:queue-find open-list existing-child)
                            child-node))
      (t
       (queues:qpush open-list child-node)))))
