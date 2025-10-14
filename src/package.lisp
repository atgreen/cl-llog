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
                #:condition-notify
                #:current-thread)
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
           #:error-field
           #:error-field-detailed)

  ;; Condition system integration
  (:export #:condition-info
           #:condition-info-p
           #:condition-info-type
           #:condition-info-message
           #:condition-info-backtrace
           #:condition-info-restarts
           #:condition-info-cause
           #:analyze-condition
           #:capture-backtrace
           #:capture-restarts
           #:condition-cause
           #:condition-chain)

  ;; Contextual logging
  (:export #:with-fields
           #:with-context
           #:child-logger)

  ;; Logger configuration
  (:export #:logger-level
           #:set-level
           #:add-output
           #:remove-output)

  ;; Hierarchical loggers
  (:export #:get-logger
           #:find-logger
           #:clear-logger-registry
           #:list-loggers
           #:set-logger-level-recursive
           #:root-logger
           #:set-root-level)

  ;; Encoders
  (:export #:make-console-encoder
           #:make-json-encoder
           #:make-sexpr-encoder
           #:make-pattern-encoder
           #:encoder)

  ;; Outputs
  (:export #:make-stream-output
           #:make-file-output
           #:make-async-output
           #:write-entry
           #:flush-output
           #:close-output
           #:output)

  ;; Hooks
  (:export #:hook
           #:hook-p
           #:hook-type
           #:hook-function
           #:hook-name
           #:hook-priority
           #:add-hook
           #:remove-hook
           #:clear-hooks
           #:list-hooks)

  ;; Sampling
  (:export #:set-sampling
           #:clear-sampling
           #:get-sampling-stats)

  ;; Rate Limiting
  (:export #:set-rate-limit
           #:clear-rate-limit
           #:get-rate-limit-stats
           #:rate-limited-p)

  ;; REPL helpers
  (:export #:show-recent
           #:grep-logs
           #:with-captured-logs
           #:enable-recent-logs
           #:disable-recent-logs)

  ;; Custom field types
  (:export #:define-field-type)

  ;; Log templates
  (:export #:define-log-template))
