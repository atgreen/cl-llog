;;;; package.lisp - Package definition for LLOG-AUDIT
;;;; SPDX-License-Identifier: MIT

(defpackage #:llog-audit
  (:use #:cl)
  (:import-from #:llog
                #:log-entry
                #:log-entry-level
                #:log-entry-timestamp
                #:log-entry-message
                #:log-entry-logger-name
                #:log-entry-fields
                #:output
                #:write-entry
                #:close-output)
  (:documentation "Tamper-evident audit trail system for LLOG")

  ;; Audit output
  (:export #:make-audit-output
           #:audit-output
           #:audit-output-p)

  ;; Verification
  (:export #:verify-audit
           #:verify-audit-file
           #:audit-verification-result
           #:audit-verification-status)

  ;; Hash chain
  (:export #:hash-chain
           #:hash-chain-algorithm
           #:compute-entry-hash
           #:verify-chain)

  ;; Checkpoints
  (:export #:checkpoint
           #:checkpoint-record-count
           #:checkpoint-merkle-root
           #:checkpoint-timestamp
           #:make-checkpoint))
