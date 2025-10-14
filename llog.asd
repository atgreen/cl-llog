;;;; llog.asd - ASDF system definition for LLOG
;;;; SPDX-License-Identifier: MIT

(defsystem "llog"
  :version "0.1.0"
  :description "High-performance structured logging framework for Common Lisp"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :homepage "https://github.com/atgreen/cl-llog"
  :bug-tracker "https://github.com/atgreen/cl-llog/issues"
  :source-control (:git "https://github.com/atgreen/cl-llog.git")

  :depends-on ("bordeaux-threads"
               "cl-ppcre")

  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "levels")
                             (:file "entry")
                             (:file "fields")
                             (:file "buffer-pool")
                             (:file "logger")
                             (:file "hierarchy")
                             (:file "hooks")
                             (:file "sampling")
                             (:file "rate-limiting")
                             (:file "api")
                             (:file "conditions")
                             (:file "repl")

                             (:module "encoders"
                              :components ((:file "encoder")
                                          (:file "console")
                                          (:file "json")
                                          (:file "sexpr")
                                          (:file "pattern")))

                             (:module "outputs"
                              :components ((:file "output")
                                          (:file "stream")
                                          (:file "file")
                                          (:file "async"))))))

  :in-order-to ((test-op (test-op "llog/tests"))))


(defsystem "llog/tests"
  :description "Test suite for LLOG"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on ("llog"
               "fiveam")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "test-levels")
                             (:file "test-fields")
                             (:file "test-logger")
                             (:file "test-encoders")
                             (:file "test-buffers")
                             (:file "test-outputs")
                             (:file "test-concurrency")
                             (:file "test-api")
                             (:file "test-conditions")
                             (:file "test-hooks")
                             (:file "test-sampling")
                             (:file "test-rate-limiting")
                             (:file "test-hierarchy")
                             (:file "test-pattern")
                             (:file "test-repl"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :llog)))


(defsystem "llog/benchmarks"
  :description "Performance benchmarks for LLOG"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on ("llog"
               "trivial-benchmark")
  :components ((:module "benchmarks"
                :components ((:file "package")
                             (:file "basic")
                             (:file "allocations")
                             (:file "throughput")))))
