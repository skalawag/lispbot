(in-package #:lispbot)

(defun make-queue ()
  "Create an empty queue"
  (cons nil nil))

(defun enqueue (queue data)
  "Insert `data' at the end of the queue"
  (if (car queue)
      (setf (cdr queue)
            (setf (cddr queue) (cons data nil)))
      (setf (car queue)
            (setf (cdr queue) (cons data nil)))))

(defun dequeue (queue)
  "Return and remove the first element of the queue"
  (let ((data (pop (car queue))))
    (unless (car queue)
      (setf (cdr queue) nil))
    data))

(defun enqueue-front (queue data)
  "Insert `data' at the front of the queue"
  (if (car queue)
      (push data (car queue))
      (setf (car queue)
            (setf (cdr queue) (cons data nil)))))

(defun queue-empty-p (queue)
  "Return t if the queue is empty and nil otherwise"
  (null (car queue)))

(defun queue-front (queue)
  "Return the first element of the queue"
  (caar queue))

(defclass channel ()
  ((queue
    :initform (make-queue)
    :reader channel-queue)
   (lock
    :initform (bt:make-recursive-lock "channel lock")
    :reader channel-lock)
   (read-ok
    :initform (bt:make-condition-variable :name "channel read-ok")
    :reader channel-read-ok)))

(defgeneric channel-send (channel data))
(defgeneric channel-recv (channel))
(defgeneric channel-send-out-of-band (channel data))
(defgeneric channel-clear (channel))

(defmethod channel-send ((self channel) data)
  (with-slots (queue lock read-ok) self
    (bt:with-recursive-lock-held (lock)
      (enqueue queue data)
      (bt:condition-notify read-ok))))

(defmethod channel-recv ((self channel))
  (with-slots (queue lock read-ok) self
    (bt:with-recursive-lock-held (lock)
      (loop while (queue-empty-p queue)
            do (bt:condition-wait read-ok lock)
            finally (return (dequeue queue))))))

(defmethod channel-send-out-of-band ((self channel) data)
  (with-slots (queue lock read-ok) self
    (bt:with-recursive-lock-held (lock)
      (enqueue-front queue data)
      (bt:condition-notify read-ok))))

(defmethod channel-clear ((self channel))
  (bt:with-recursive-lock-held ((channel-lock self))
   (setf (slot-value self 'queue) (make-queue))))
