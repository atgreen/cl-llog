;;;; llog-audit.asd - ASDF system definition for LLOG audit features
;;;; SPDX-License-Identifier: MIT

(defsystem "llog-audit"
  :version "0.1.0"
  :description "Tamper-evident audit trails for LLOG logging framework"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :homepage "https://github.com/atgreen/cl-llog"
  :bug-tracker "https://github.com/atgreen/cl-llog/issues"
  :source-control (:git "https://github.com/atgreen/cl-llog.git")

  :depends-on ("llog"           ; Core logging system
               "ironclad"       ; Cryptographic hashing (SHA-256, etc.)
               "cl-base64"      ; Base64 encoding for signatures
               "babel")         ; String encoding

  :components ((:module "src/audit"
                :serial t
                :components ((:file "package")
                             (:file "hash-chain")
                             (:file "checkpoints")
                             (:file "audit-output"))))

  :in-order-to ((test-op (test-op "llog-audit/tests"))))


(defsystem "llog-audit/tests"
  :description "Test suite for LLOG audit features"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on ("llog-audit"
               "fiveam")
  :components ((:module "tests/audit"
                :serial t
                :components ((:file "package")
                             (:file "test-hash-chain")
                             (:file "test-audit-output"))))
  :perform (test-op (o c) (symbol-call :fiveam :run! :llog-audit)))
