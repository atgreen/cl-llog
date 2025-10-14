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
   (pre-log-hooks
    :initarg :pre-log-hooks
    :initform nil
    :type list
    :documentation "Hooks called before logging (can modify or filter entries)")
   (post-log-hooks
    :initarg :post-log-hooks
    :initform nil
    :type list
    :documentation "Hooks called after logging (for notifications, metrics)")
   (error-hooks
    :initarg :error-hooks
    :initform nil
    :type list
    :documentation "Hooks called when logging errors occur")
   (sampling-configs
    :initarg :sampling-configs
    :initform (make-hash-table :test 'eql)
    :type hash-table
    :documentation "Sampling configuration per level (level -> sampling-config)")
   (rate-limiters
    :initarg :rate-limiters
    :initform (make-hash-table :test 'eql)
    :type hash-table
    :documentation "Rate limiters per level (level -> rate-limiter)")
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

;;; Forward Declarations for Sampling and Rate Limiting
;;; (These functions are defined in sampling.lisp and rate-limiting.lisp)

(declaim (ftype (function (t) t) should-sample-p))
(declaim (ftype (function (t t) t) record-sampling-decision))
(declaim (ftype (function (t t) t) get-sampling-config-for-level))
(declaim (ftype (function (t t t) t) set-sampling-config-for-level))
(declaim (ftype (function (t) t) try-take-token))
(declaim (ftype (function (t t) t) get-rate-limiter-for-level))
(declaim (ftype (function (t t t) t) set-rate-limiter-for-level))
(declaim (ftype (function (t) t) sampling-config-stats))
(declaim (ftype (function (t) t) rate-limiter-stats))
(declaim (ftype (function (t) t) rate-limiter-lock))
(declaim (ftype (function (t) t) rate-limiter-tokens))
(declaim (ftype (function (t) t) make-probabilistic-sampler))
(declaim (ftype (function (t) t) make-deterministic-sampler))
(declaim (ftype (function (t) t) make-rate-limiter-config))
(declaim (ftype (function (t) t) make-rate-limiter-per-minute))
(declaim (ftype (function (t) t) make-rate-limiter-per-hour))

;;; Core Logging Function

(defun log-entry (logger entry)
  "Log an ENTRY using LOGGER. Writes to all configured outputs.
   Calls pre-log hooks (which can modify or filter the entry),
   writes to outputs, then calls post-log hooks.
   If errors occur, error hooks are called."
  (declare (type logger logger)
           (type (or null log-entry) entry)
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

    ;; Apply sampling (filter out if not sampled)
    (let* ((level (log-entry-level entry))
           (sampling-config (get-sampling-config-for-level logger level)))
      (when sampling-config
        (let ((should-log (should-sample-p sampling-config)))
          (record-sampling-decision sampling-config should-log)
          (unless should-log
            (setf entry nil)))))

    ;; Apply rate limiting (filter out if rate limited)
    (when entry
      (let* ((level (log-entry-level entry))
             (rate-limiter (get-rate-limiter-for-level logger level)))
        (when (and rate-limiter (not (try-take-token rate-limiter)))
          (setf entry nil))))

    ;; Call pre-log hooks (can modify or filter entry)
    (when entry
      (setf entry (%call-pre-log-hooks logger entry)))

    ;; If pre-log hooks filtered the entry (returned nil), skip logging
    (when entry
      (let ((error-occurred nil))
        (dolist (output outputs)
          (handler-case
              (write-entry output entry)
            (cl:error (e)
              (setf error-occurred t)
              ;; Call error hooks
              (%call-error-hooks logger e entry)
              ;; Never let logging errors crash the application
              (format *error-output* "~&Logging error: ~A~%" e))))

        ;; Call post-log hooks if no errors occurred
        (unless error-occurred
          (%call-post-log-hooks logger entry)))))

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

;;; Sampling API

(defun set-sampling (logger level-designator probability-or-mode &optional n)
  "Configure sampling for LOGGER at LEVEL-DESIGNATOR.

   Probabilistic sampling:
     (set-sampling logger :debug 0.1)  ; 10% sample rate

   Deterministic sampling:
     (set-sampling logger :debug :every 100)  ; every 100th log

   Clear sampling:
     (set-sampling logger :debug nil)"
  (let ((level (parse-level level-designator)))
    (unless level
      (cl:error "Invalid log level: ~S" level-designator))
    (cond
      ;; Clear sampling
      ((null probability-or-mode)
       (set-sampling-config-for-level logger level nil))
      ;; Deterministic sampling: :every N
      ((and (eql probability-or-mode :every) n)
       (let ((config (make-deterministic-sampler n)))
         (set-sampling-config-for-level logger level config)))
      ;; Probabilistic sampling: probability
      ((numberp probability-or-mode)
       (let ((config (make-probabilistic-sampler probability-or-mode)))
         (set-sampling-config-for-level logger level config)))
      (t
       (cl:error "Invalid sampling configuration: ~S ~S" probability-or-mode n)))
    logger))

(defun clear-sampling (logger level-designator)
  "Clear sampling configuration for LOGGER at LEVEL-DESIGNATOR."
  (set-sampling logger level-designator nil))

(defun get-sampling-stats (logger level-designator)
  "Get sampling statistics for LOGGER at LEVEL-DESIGNATOR."
  (let* ((level (parse-level level-designator))
         (config (get-sampling-config-for-level logger level)))
    (sampling-config-stats config)))

;;; Rate Limiting API

(defun set-rate-limit (logger level-designator max-logs time-unit)
  "Configure rate limiting for LOGGER at LEVEL-DESIGNATOR.

   TIME-UNIT can be :per-second, :per-minute, or :per-hour.

   Examples:
     (set-rate-limit logger :error 100 :per-second)
     (set-rate-limit logger :warn 10 :per-minute)

   Clear rate limiting:
     (set-rate-limit logger :error nil nil)"
  (let ((level (parse-level level-designator)))
    (unless level
      (cl:error "Invalid log level: ~S" level-designator))
    (cond
      ;; Clear rate limiting
      ((null max-logs)
       (set-rate-limiter-for-level logger level nil))
      ;; Set rate limiting
      ((and (numberp max-logs) (plusp max-logs))
       (let ((limiter (ecase time-unit
                        (:per-second (make-rate-limiter-config max-logs))
                        (:per-minute (make-rate-limiter-per-minute max-logs))
                        (:per-hour (make-rate-limiter-per-hour max-logs)))))
         (set-rate-limiter-for-level logger level limiter)))
      (t
       (cl:error "Invalid rate limit: ~S ~S" max-logs time-unit)))
    logger))

(defun clear-rate-limit (logger level-designator)
  "Clear rate limiting configuration for LOGGER at LEVEL-DESIGNATOR."
  (set-rate-limit logger level-designator nil nil))

(defun get-rate-limit-stats (logger level-designator)
  "Get rate limiting statistics for LOGGER at LEVEL-DESIGNATOR."
  (let* ((level (parse-level level-designator))
         (limiter (get-rate-limiter-for-level logger level)))
    (rate-limiter-stats limiter)))

(defun rate-limited-p (logger level-designator)
  "Return T if LOGGER is currently rate limited at LEVEL-DESIGNATOR."
  (let* ((level (parse-level level-designator))
         (limiter (get-rate-limiter-for-level logger level)))
    (if (null limiter)
        nil
        (with-lock-held ((rate-limiter-lock limiter))
          (< (rate-limiter-tokens limiter) 1.0)))))
