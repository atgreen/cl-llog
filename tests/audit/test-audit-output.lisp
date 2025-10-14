;;;; test-audit-output.lisp - Tests for audit output
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit/tests)

(in-suite :llog-audit)

;;; Audit Output Tests

(def-test create-audit-output ()
  "Test creating an audit output."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (let ((output (make-audit-output temp-file)))
           (is (audit-output-p output))
           (is (llog-audit::audit-output-stream output))
           (is (= 1000 (llog-audit::audit-output-checkpoint-interval output)))
           (llog:close-output output))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;; TODO: Add more comprehensive tests once core functionality is working:
;; - Test writing entries
;; - Test hash chaining
;; - Test checkpoint generation
;; - Test verification
