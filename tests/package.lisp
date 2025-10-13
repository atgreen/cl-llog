;;;; tests/package.lisp - Test package definition for LLOG
;;;; SPDX-License-Identifier: MIT

(defpackage #:llog/tests
  (:use #:cl #:fiveam)
  ;; Use shadowing-import for symbols that conflict with CL
  (:shadowing-import-from #:llog
                          #:trace
                          #:debug
                          #:warn
                          #:error
                          #:string
                          #:float
                          #:trace-typed
                          #:debug-typed
                          #:warn-typed
                          #:error-typed
                          #:error-field)
  ;; Regular import for non-conflicting symbols
  (:import-from #:llog
                #:level-name
                #:level-keyword
                #:parse-level
                #:level>=
                #:make-log-entry
                #:log-entry-level
                #:log-entry-message
                #:log-entry-fields
                #:logger
                #:logger-level
                #:logger-outputs
                #:make-logger
                #:set-level
                #:should-log-p
                #:with-fields
                #:add-output
                #:remove-output
                #:int
                #:bool
                #:timestamp
                #:duration-ms
                #:keywords-to-fields
                #:make-console-encoder
                #:make-json-encoder
                #:make-sexpr-encoder
                #:make-stream-output
                #:make-file-output
                #:make-async-output
                #:flush-output
                #:close-output
                #:log-entry
                #:*logger*
                #:info
                #:fatal
                #:panic
                #:info-typed
                #:fatal-typed
                #:panic-typed
                #:+trace+
                #:+debug+
                #:+info+
                #:+warn+
                #:+error+))

(in-package #:llog/tests)

(def-suite :llog)
