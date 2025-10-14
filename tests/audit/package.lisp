;;;; tests/audit/package.lisp - Test package for LLOG/AUDIT
;;;; SPDX-License-Identifier: MIT

(defpackage #:llog/audit/tests
  (:documentation "Test suite for the LLOG/AUDIT audit trail system.")
  (:use #:cl #:fiveam)
  (:import-from #:llog/audit
                #:hash-chain
                #:compute-entry-hash
                #:verify-chain
                #:checkpoint
                #:compute-merkle-root
                #:make-checkpoint-from-hashes
                #:verify-checkpoint
                #:make-audit-output
                #:audit-output-p)
  (:import-from #:llog
                #:make-logger
                #:info
                #:*logger*))

(in-package #:llog/audit/tests)

(def-suite :llog/audit)
