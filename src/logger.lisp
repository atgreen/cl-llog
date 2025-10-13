;;;; logger.lisp - Logger class and core logging protocol
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Logger Class

(defclass logger ()
  ((name
    :initarg :name
    :initform ""
    :accessor logger-name
    :type cl:string
    :documentation "Logger name for identification")
   (level
    :initarg :level
    :initform +info+
    :accessor logger-level
    :type (integer 0 6)
    :documentation "Minimum log level for this logger")
   (outputs
    :initarg :outputs
    :initform nil
    :accessor logger-outputs
    :type list
    :documentation "List of output destinations")
   (context-fields
    :initarg :context-fields
    :initform nil
    :accessor logger-context-fields
    :type list
    :documentation "Fields that are added to every log entry")
   (lock
    :initarg :lock
    :initform (make-lock "llog/logger")
    :reader logger-lock
    :documentation "Lock protecting mutable logger state"))
  (:documentation "A logger that writes structured log entries to multiple outputs."))

;;; Global default logger

(defvar *logger* nil
  "The default global logger. Will be initialized on first use.")

(defun ensure-default-logger ()
  "Ensure *logger* is initialized with a default logger."
  (unless *logger*
    (setf *logger* (make-logger))))

;;; Constructor

(defun make-logger (&key (name "") (level :info) outputs)
  "Create a new logger with the specified configuration.

   NAME - logger name for identification
   LEVEL - minimum log level (keyword, string, or integer)
   OUTPUTS - list of output destinations"
  (let* ((level-int (if (integerp level)
                        level
                        (or (parse-level level)
                            +info+)))
         (output-list (when outputs (copy-list outputs))))
    (make-instance 'logger
                   :name name
                   :level level-int
                   :outputs (or output-list
                                (list (make-stream-output *standard-output*))))))

;;; Level Management

(defun set-level (logger level-designator)
  "Set the minimum log level for LOGGER."
  (let ((level (parse-level level-designator)))
    (unless level
      (cl:error "Invalid log level: ~S" level-designator))
    (with-lock-held ((logger-lock logger))
      (setf (slot-value logger 'level) level))
    (values level)))

(declaim (inline should-log-p))
(defun should-log-p (logger level)
  "Return T if a message at LEVEL should be logged by LOGGER."
  (declare (type logger logger)
           (type (integer 0 6) level)
           (optimize speed (safety 0)))
  (level>= level (logger-level logger)))

;;; Output Management

(defun add-output (logger output)
  "Add an output destination to LOGGER."
  (with-lock-held ((logger-lock logger))
    (push output (slot-value logger 'outputs)))
  logger)

(defun remove-output (logger output)
  "Remove an output destination from LOGGER."
    (with-lock-held ((logger-lock logger))
      (setf (slot-value logger 'outputs)
            (remove output (slot-value logger 'outputs))))
  logger)

;;; Core Logging Function

(defun log-entry (logger entry)
  "Log an ENTRY using LOGGER. Writes to all configured outputs."
  (declare (type logger logger)
           (type log-entry entry)
           (optimize speed))
  (let (context-fields logger-name outputs)
    (with-lock-held ((logger-lock logger))
      (setf context-fields (copy-list (logger-context-fields logger))
            logger-name (logger-name logger)
            outputs (copy-list (logger-outputs logger))))

    (when context-fields
      (setf (log-entry-fields entry)
            (append context-fields (log-entry-fields entry))))

    (when (and logger-name
               (string= (log-entry-logger-name entry) ""))
      (setf (log-entry-logger-name entry) logger-name))

    (dolist (output outputs)
      (handler-case
          (write-entry output entry)
        (cl:error (e)
          ;; Never let logging errors crash the application
          (format *error-output* "~&Logging error: ~A~%" e)))))

  (values))

;;; Contextual Logging

(defun with-fields (logger &rest keyword-args)
  "Create a child logger with additional context fields.
   Returns a new logger instance with the same configuration but additional fields."
  (if (null keyword-args)
      logger
      (let* ((additional (keywords-to-fields keyword-args))
             (lock (logger-lock logger))
             name level outputs context)
        (with-lock-held (lock)
          (setf name (logger-name logger)
                level (logger-level logger)
                outputs (logger-outputs logger)
                context (append (copy-list (logger-context-fields logger))
                                additional)))
        (make-instance 'logger
                       :name name
                       :level level
                       :outputs outputs
                       :context-fields context
                       :lock lock))))

(defun child-logger (parent-logger name)
  "Create a child logger that inherits configuration from PARENT-LOGGER."
  (let (level outputs context)
    (with-lock-held ((logger-lock parent-logger))
      (setf level (logger-level parent-logger)
            outputs (logger-outputs parent-logger)
            context (copy-list (logger-context-fields parent-logger))))
    (make-instance 'logger
                   :name name
                   :level level
                   :outputs outputs
                   :context-fields context
                   :lock (logger-lock parent-logger))))
