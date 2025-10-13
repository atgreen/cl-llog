;;;; package.lisp - Package definitions for LLOG
;;;; SPDX-License-Identifier: MIT

(defpackage #:llog
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-thread
                #:join-thread
                #:make-condition-variable
                #:condition-wait
                #:condition-notify)
  (:shadow #:string #:float #:error #:trace #:debug #:warn)
  (:documentation "High-performance structured logging framework for Common Lisp")

  ;; Re-export useful CL symbols
  (:export #:make-logger
           #:*logger*
           #:log-entry
           #:make-log-entry
           #:log-entry-level
           #:log-entry-timestamp
           #:log-entry-message
           #:log-entry-logger-name
           #:log-entry-fields)

  ;; Log levels (constants)
  (:export #:+trace+
           #:+debug+
           #:+info+
           #:+warn+
           #:+error+
           #:+fatal+
           #:+panic+)

  ;; Sugared API - convenient logging macros
  (:export #:trace
           #:debug
           #:info
           #:warn
           #:error
           #:fatal
           #:panic)

  ;; Typed API - zero-allocation logging
  (:export #:trace-typed
           #:debug-typed
           #:info-typed
           #:warn-typed
           #:error-typed
           #:fatal-typed
           #:panic-typed)

  ;; Typed field constructors
  (:export #:string
           #:int
           #:float
           #:bool
           #:timestamp
           #:duration-ms
           #:error-field)

  ;; Contextual logging
  (:export #:with-fields
           #:with-context
           #:child-logger)

  ;; Logger configuration
  (:export #:logger-level
           #:set-level
           #:add-output
           #:remove-output)

  ;; Encoders
  (:export #:make-console-encoder
           #:make-json-encoder
           #:make-sexpr-encoder
           #:encoder)

  ;; Outputs
  (:export #:make-stream-output
           #:make-file-output
           #:make-async-output
           #:flush-output
           #:close-output
           #:output)

  ;; Hooks
  (:export #:add-hook
           #:remove-hook
           #:hook)

  ;; REPL helpers
  (:export #:show-recent
           #:grep-logs
           #:with-captured-logs)

  ;; Custom field types
  (:export #:define-field-type)

  ;; Log templates
  (:export #:define-log-template))
