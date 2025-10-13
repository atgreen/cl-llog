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
    :documentation "Fields that are added to every log entry"))
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
  (let ((level-int (if (integerp level)
                       level
                       (or (parse-level level)
                           +info+))))
    (make-instance 'logger
                   :name name
                   :level level-int
                   :outputs (or outputs
                                (list (make-stream-output *standard-output*))))))

;;; Level Management

(defun set-level (logger level-designator)
  "Set the minimum log level for LOGGER."
  (let ((level (parse-level level-designator)))
    (unless level
      (cl:error "Invalid log level: ~S" level-designator))
    (setf (logger-level logger) level)))

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
  (push output (logger-outputs logger))
  logger)

(defun remove-output (logger output)
  "Remove an output destination from LOGGER."
  (setf (logger-outputs logger)
        (remove output (logger-outputs logger)))
  logger)

;;; Core Logging Function

(defun log-entry (logger entry)
  "Log an ENTRY using LOGGER. Writes to all configured outputs."
  (declare (type logger logger)
           (type log-entry entry)
           (optimize speed))

  ;; Add context fields to entry
  (when (logger-context-fields logger)
    (setf (log-entry-fields entry)
          (append (logger-context-fields logger)
                  (log-entry-fields entry))))

  ;; Set logger name if not already set
  (when (string= (log-entry-logger-name entry) "")
    (setf (log-entry-logger-name entry) (logger-name logger)))

  ;; Write to all outputs
  (dolist (output (logger-outputs logger))
    (handler-case
        (write-entry output entry)
      (cl:error (e)
        ;; Never let logging errors crash the application
        (format *error-output* "~&Logging error: ~A~%" e))))

  (values))

;;; Contextual Logging

(defun with-fields (logger &rest keyword-args)
  "Create a child logger with additional context fields.
   Returns a new logger instance with the same configuration but additional fields."
  (let ((new-logger (make-instance 'logger
                                   :name (logger-name logger)
                                   :level (logger-level logger)
                                   :outputs (logger-outputs logger)
                                   :context-fields (logger-context-fields logger))))
    (setf (logger-context-fields new-logger)
          (append (logger-context-fields new-logger)
                  (keywords-to-fields keyword-args)))
    new-logger))

(defun child-logger (parent-logger name)
  "Create a child logger that inherits configuration from PARENT-LOGGER."
  (make-instance 'logger
                 :name name
                 :level (logger-level parent-logger)
                 :outputs (logger-outputs parent-logger)
                 :context-fields (logger-context-fields parent-logger)))
