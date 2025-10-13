;;;; outputs/async.lisp - Asynchronous output implementation
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Async Output
;;; Buffers entries in memory and writes them using a background worker thread

(defclass async-output (output)
  ((underlying
    :initarg :underlying
    :reader async-output-underlying
    :type output
    :documentation "Underlying output that performs actual I/O")
   (queue
    :initarg :queue
    :accessor async-output-queue
    :documentation "Circular buffer storing pending log entries")
   (queue-size
    :initarg :queue-size
    :reader async-output-queue-size
    :type fixnum
    :documentation "Maximum number of entries in the queue")
   (head
    :initarg :head
    :accessor async-output-head
    :type fixnum
    :documentation "Read index for the queue")
   (tail
    :initarg :tail
    :accessor async-output-tail
    :type fixnum
    :documentation "Write index for the queue")
   (count
    :initarg :count
    :accessor async-output-count
    :type fixnum
    :documentation "Current number of queued entries")
   (lock
    :initarg :lock
    :reader async-output-lock
    :documentation "Lock protecting queue state")
   (not-empty
    :initarg :not-empty
    :reader async-output-not-empty
    :documentation "Condition variable signalled when queue is non-empty")
   (not-full
    :initarg :not-full
    :reader async-output-not-full
    :documentation "Condition variable signalled when queue has space")
   (drained
    :initarg :drained
    :reader async-output-drained
    :documentation "Condition variable signalled when queue becomes empty")
   (shutdown
    :initarg :shutdown
    :accessor async-output-shutdown
    :type boolean
    :documentation "Flag indicating the worker should terminate")
   (thread
    :initarg :thread
    :accessor async-output-thread
    :documentation "Background worker thread"))
  (:documentation "Output that performs logging asynchronously using a worker thread."))

(defun make-async-output (output &key (queue-size 1024) min-level)
  "Wrap OUTPUT in an asynchronous writer with a bounded queue of QUEUE-SIZE.
MIN-LEVEL defaults to OUTPUT's minimum level."
  (check-type output output)
  (let* ((size (max 1 queue-size))
         (lock (make-lock "llog/async-output"))
         (level (or (and min-level (if (integerp min-level)
                                       min-level
                                       (parse-level min-level)))
                    (output-min-level output)))
         (instance (make-instance 'async-output
                                  :encoder (output-encoder output)
                                  :min-level level
                                  :underlying output
                                  :queue (make-array size :initial-element nil)
                                  :queue-size size
                                  :head 0
                                  :tail 0
                                  :count 0
                                  :lock lock
                                  :not-empty (make-condition-variable)
                                  :not-full (make-condition-variable)
                                  :drained (make-condition-variable)
                                  :shutdown nil
                                  :thread nil)))
    (setf (async-output-thread instance)
          (make-thread (lambda () (async-output-loop instance))
                       :name "llog-async-output"))
    instance))

(defun async-output-loop (output)
  "Worker loop that drains queued entries."
  (loop
    (let (entry)
      (with-lock-held ((async-output-lock output))
        (loop while (and (zerop (async-output-count output))
                         (not (async-output-shutdown output)))
              do (condition-wait (async-output-not-empty output)
                                 (async-output-lock output)))
        (when (and (async-output-shutdown output)
                   (zerop (async-output-count output)))
          (condition-notify (async-output-drained output))
          (return))
        (let ((queue (async-output-queue output))
              (size (async-output-queue-size output)))
          (setf entry (aref queue (async-output-head output)))
          (setf (aref queue (async-output-head output)) nil)
          (setf (async-output-head output)
                (mod (1+ (async-output-head output)) size))
          (decf (async-output-count output))
          (condition-notify (async-output-not-full output))
          (when (zerop (async-output-count output))
            (condition-notify (async-output-drained output)))))
      (handler-case
          (write-entry (async-output-underlying output) entry)
        (cl:error (e)
          (format *error-output* "~&Async logging error: ~A~%" e)))))
  (flush-output (async-output-underlying output)))

(defmethod write-entry ((output async-output) (entry log-entry))
  (when (level>= (log-entry-level entry) (output-min-level output))
    (with-lock-held ((async-output-lock output))
      (loop while (and (>= (async-output-count output)
                           (async-output-queue-size output))
                       (not (async-output-shutdown output)))
            do (condition-wait (async-output-not-full output)
                               (async-output-lock output)))
      (unless (async-output-shutdown output)
        (let ((queue (async-output-queue output))
              (size (async-output-queue-size output)))
          (setf (aref queue (async-output-tail output)) entry)
          (setf (async-output-tail output)
                (mod (1+ (async-output-tail output)) size))
          (incf (async-output-count output))
          (condition-notify (async-output-not-empty output))))))
  (values))

(defmethod flush-output ((output async-output))
  (with-lock-held ((async-output-lock output))
    (loop while (> (async-output-count output) 0)
          do (condition-wait (async-output-drained output)
                             (async-output-lock output))))
  (flush-output (async-output-underlying output))
  (values))

(defmethod close-output ((output async-output))
  (with-lock-held ((async-output-lock output))
    (setf (async-output-shutdown output) t)
    (condition-notify (async-output-not-empty output))
    (condition-notify (async-output-not-full output)))
  (let ((thread (async-output-thread output)))
    (when thread
      (join-thread thread)
      (setf (async-output-thread output) nil)))
  (flush-output (async-output-underlying output))
  (close-output (async-output-underlying output))
  (values))
