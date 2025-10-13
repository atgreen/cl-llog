;;;; llog.asd - ASDF system definition for LLOG
;;;; SPDX-License-Identifier: MIT

(defsystem "llog"
  :version "0.1.0"
  :description "High-performance structured logging framework for Common Lisp"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :homepage "https://github.com/yourusername/llog"
  :bug-tracker "https://github.com/yourusername/llog/issues"
  :source-control (:git "https://github.com/yourusername/llog.git")

  ;; Core has zero external dependencies
  :depends-on ("bordeaux-threads")

  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "levels")
                             (:file "entry")
                             (:file "fields")
                             (:file "buffer-pool")
                             (:file "logger")
                             (:file "api")

                             (:module "encoders"
                              :components ((:file "encoder")
                                          (:file "console")
                                          (:file "json")
                                          (:file "sexpr")))

                             (:module "outputs"
                              :components ((:file "output")
                                          (:file "stream")
                                          (:file "file"))))))

  :in-order-to ((test-op (test-op "llog/tests"))))


(defsystem "llog/tests"
  :description "Test suite for LLOG"
  :author "Your Name <your.email@example.com>"
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
                             (:file "test-api"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :llog)))


(defsystem "llog/benchmarks"
  :description "Performance benchmarks for LLOG"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :depends-on ("llog"
               "trivial-benchmark")
  :components ((:module "benchmarks"
                :components ((:file "package")
                             (:file "basic")
                             (:file "allocations")
                             (:file "throughput")))))
