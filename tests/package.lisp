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
                #:make-stream-output
                #:log-entry
                #:*logger*
                #:info
                #:fatal
                #:panic
                #:info-typed
                #:fatal-typed
                #:panic-typed))

(in-package #:llog/tests)

(def-suite :llog)
