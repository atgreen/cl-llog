;;;; api.lisp - User-facing logging API
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Sugared API
;;; Convenient logging macros with automatic field inference
;;;
(defmacro %log (level message &rest keyword-args)
  "Internal macro for logging with the sugared API."
  (let ((fields-form (when keyword-args
                       `(keywords-to-fields (list ,@keyword-args)))))
    `(progn
       (ensure-default-logger)
       (let ((logger *logger*))
         (when (should-log-p logger ,level)
           (let ((entry (make-log-entry
                         ,level
                         ,message
                         :logger-name (logger-name logger)
                         :fields ,(or fields-form nil))))
             (log-entry logger entry)))))))

(defmacro trace (message &rest keyword-args)
  "Log a message at TRACE level with optional keyword arguments as fields.
   Example: (llog:trace \"Debug details\" :value x :count n)"
  `(%log +trace+ ,message ,@keyword-args))

(defmacro debug (message &rest keyword-args)
  "Log a message at DEBUG level with optional keyword arguments as fields.
   Example: (llog:debug \"Variable state\" :x x :y y)"
  `(%log +debug+ ,message ,@keyword-args))

(defmacro info (message &rest keyword-args)
  "Log a message at INFO level with optional keyword arguments as fields.
   Example: (llog:info \"User logged in\" :user-id 123 :username \"alice\")"
  `(%log +info+ ,message ,@keyword-args))

(defmacro warn (message &rest keyword-args)
  "Log a message at WARN level with optional keyword arguments as fields.
   Example: (llog:warn \"Deprecated API used\" :api-version \"v1\")"
  `(%log +warn+ ,message ,@keyword-args))

(defmacro error (message &rest keyword-args)
  "Log a message at ERROR level with optional keyword arguments as fields.
   Example: (llog:error \"Operation failed\" :error-code 500)"
  `(%log +error+ ,message ,@keyword-args))

(defmacro fatal (message &rest keyword-args)
  "Log a message at FATAL level with optional keyword arguments as fields.
   Example: (llog:fatal \"System shutdown\" :reason \"out of memory\")"
  `(%log +fatal+ ,message ,@keyword-args))

(defmacro panic (message &rest keyword-args)
  "Log a message at PANIC level with optional keyword arguments as fields.
   Example: (llog:panic \"Critical error\" :stack-trace t)"
  `(%log +panic+ ,message ,@keyword-args))

;;; Typed API
;;; Zero-allocation logging with explicit field types

(defmacro %log-typed (level message &rest field-forms)
  "Internal macro for typed logging API."
  `(progn
     (ensure-default-logger)
     (let ((logger *logger*))
       (when (should-log-p logger ,level)
         (let ((entry (make-log-entry
                       ,level
                       ,message
                       :logger-name (logger-name logger)
                       :fields ,(if field-forms
                                    `(list ,@field-forms)
                                    nil))))
           (log-entry logger entry))))))

(defmacro trace-typed (message &rest field-forms)
  "Log a message at TRACE level with typed fields.
   Example: (llog:trace-typed \"Processing\" (llog:int \"item-id\" id))"
  `(%log-typed +trace+ ,message ,@field-forms))

(defmacro debug-typed (message &rest field-forms)
  "Log a message at DEBUG level with typed fields.
   Example: (llog:debug-typed \"State\" (llog:string \"status\" status))"
  `(%log-typed +debug+ ,message ,@field-forms))

(defmacro info-typed (message &rest field-forms)
  "Log a message at INFO level with typed fields.
   Example: (llog:info-typed \"Request\" (llog:int \"duration\" ms))"
  `(%log-typed +info+ ,message ,@field-forms))

(defmacro warn-typed (message &rest field-forms)
  "Log a message at WARN level with typed fields.
   Example: (llog:warn-typed \"Slow query\" (llog:float \"time\" 5.3))"
  `(%log-typed +warn+ ,message ,@field-forms))

(defmacro error-typed (message &rest field-forms)
  "Log a message at ERROR level with typed fields.
   Example: (llog:error-typed \"Failed\" (llog:error-field \"error\" err))"
  `(%log-typed +error+ ,message ,@field-forms))

(defmacro fatal-typed (message &rest field-forms)
  "Log a message at FATAL level with typed fields.
   Example: (llog:fatal-typed \"Shutdown\" (llog:int \"code\" code))"
  `(%log-typed +fatal+ ,message ,@field-forms))

(defmacro panic-typed (message &rest field-forms)
  "Log a message at PANIC level with typed fields.
   Example: (llog:panic-typed \"Crash\" (llog:string \"reason\" reason))"
  `(%log-typed +panic+ ,message ,@field-forms))

;;; Contextual Logging Macro

(defmacro with-context ((&rest keyword-args) &body body)
  "Execute BODY with additional context fields added to *logger*.
   Example: (llog:with-context (:request-id id) (process-request))"
  `(let ((*logger* (with-fields *logger* ,@keyword-args)))
     ,@body))
