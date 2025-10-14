;;;; test-hash-chain.lisp - Tests for hash chain implementation
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit/tests)

(in-suite :llog-audit)

;;; Hash Chain Tests

(def-test hash-chain-initialization ()
  "Test hash chain initialization."
  (let ((chain (llog-audit::init-hash-chain)))
    (is (llog-audit::hash-chain-p chain))
    (is (eql :sha256 (llog-audit::hash-chain-algorithm chain)))
    (is (null (llog-audit::hash-chain-previous-hash chain)))
    (is (zerop (llog-audit::hash-chain-entry-count chain)))))

(def-test compute-entry-hash-deterministic ()
  "Test that entry hash is deterministic."
  (let* ((entry (llog::%make-log-entry
                 :level llog:+info+
                 :timestamp 123456789
                 :message "Test message"
                 :logger-name "test"
                 :fields nil))
         (hash1 (compute-entry-hash entry nil))
         (hash2 (compute-entry-hash entry nil)))
    (is (stringp hash1))
    (is (string= hash1 hash2))
    (is (> (length hash1) 20))))  ; Base64 SHA-256 is ~44 chars

(def-test hash-chain-links-entries ()
  "Test that hash chain correctly links entries."
  (let* ((entry1 (llog::%make-log-entry
                  :level llog:+info+
                  :timestamp 123456789
                  :message "First"
                  :logger-name "test"
                  :fields nil))
         (entry2 (llog::%make-log-entry
                  :level llog:+info+
                  :timestamp 123456790
                  :message "Second"
                  :logger-name "test"
                  :fields nil))
         (hash1 (compute-entry-hash entry1 nil))
         (hash2 (compute-entry-hash entry2 hash1)))
    ;; Different entries produce different hashes
    (is (not (string= hash1 hash2)))

    ;; Same entry with different previous hash produces different hash
    (let ((hash2-alt (compute-entry-hash entry2 nil)))
      (is (not (string= hash2 hash2-alt))))))

(def-test chain-next-entry-increments-count ()
  "Test that chaining an entry increments the count."
  (let* ((chain (llog-audit::init-hash-chain))
         (entry (llog::%make-log-entry
                 :level llog:+info+
                 :timestamp 123456789
                 :message "Test"
                 :logger-name "test"
                 :fields nil)))
    (multiple-value-bind (new-chain new-hash)
        (llog-audit::chain-next-entry chain entry)
      (is (= 1 (llog-audit::hash-chain-entry-count new-chain)))
      (is (stringp new-hash))
      (is (string= new-hash (llog-audit::hash-chain-previous-hash new-chain))))))

;; TODO: Add more tests for verify-chain once we have entry metadata support
