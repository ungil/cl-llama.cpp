(in-package :llama)

(defclass circular-buffer ()
  ((buffer :accessor buffer)
   (index :accessor index :initform 0)
   (empty :accessor empty :initarg :size)))

(defmethod initialize-instance :after ((cb circular-buffer) &key)
  (setf (slot-value cb 'buffer) (make-array (slot-value cb 'empty))))

(defmethod index-offset ((cb circular-buffer) i &optional (offset 1))
  (assert (< offset (length (buffer cb))))
  (mod (+ i offset) (length (buffer cb))))

(defmethod cb-clear ((cb circular-buffer))
  (setf (index cb) 0
        (empty cb) (length (buffer cb)))
  cb)

(defmethod cb-length ((cb circular-buffer))
  (- (length (buffer cb)) (empty cb)))

(defmethod cb-content ((cb circular-buffer))
  (if (zerop (empty cb))
      (loop for i = (index cb) then (index-offset cb i)
	    repeat (cb-length cb)
	    collect (elt (buffer cb) i))
      (subseq (buffer cb) 0 (index cb))))

(defmethod cb-push ((cb circular-buffer) value)
  (setf (elt (buffer cb) (index cb)) value)
  (setf (index cb) (index-offset cb (index cb)))
  (unless (zerop (empty cb)) (decf (empty cb)))
  cb)

(defmethod print-object ((cb circular-buffer) stream)
  (print-unreadable-object (cb stream :type t :identity t)
    (format stream "~A ~A ~A" (index cb) (buffer cb) (cb-content cb))))
