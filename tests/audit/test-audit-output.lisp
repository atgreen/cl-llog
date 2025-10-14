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

(def-test write-entries-to-audit-output ()
  "Test writing multiple entries to audit output."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           (let ((output (make-audit-output temp-file)))
             ;; Write several entries
             (dotimes (i 5)
               (let ((entry (llog::%make-log-entry
                             :level llog:+info+
                             :timestamp (+ 1000000000 i)
                             :message (format nil "Test message ~D" i)
                             :logger-name "test.logger"
                             :fields nil)))
                 (llog:write-entry output entry)))
             (llog:close-output output))

           ;; Verify file was created and has content
           (is (probe-file temp-file))
           (let ((content (with-open-file (in temp-file)
                           (with-output-to-string (out)
                             (loop for line = (read-line in nil nil)
                                   while line
                                   do (write-line line out))))))
             ;; Should have header + 5 entries
             (is (> (length content) 100))
             ;; Should contain audit metadata
             (is (search "\"audit\"" content))
             (is (search "\"hash\"" content))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test audit-output-hash-chain ()
  "Test that hash chain is maintained correctly."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (let ((output (make-audit-output temp-file)))
           ;; Write entries and track hashes
           (let ((hashes nil))
             (dotimes (i 3)
               (let ((entry (llog::%make-log-entry
                             :level llog:+info+
                             :timestamp (+ 1000000000 i)
                             :message (format nil "Message ~D" i)
                             :logger-name "test"
                             :fields nil)))
                 (llog:write-entry output entry)
                 (push (llog-audit::hash-chain-previous-hash
                        (llog-audit::audit-output-chain output))
                       hashes)))

             ;; All hashes should be different
             (is (= 3 (length (remove-duplicates hashes :test #'string=))))

             ;; Entry count should be correct
             (is (= 3 (llog-audit::hash-chain-entry-count
                      (llog-audit::audit-output-chain output)))))

           (llog:close-output output))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test audit-output-checkpoint-generation ()
  "Test that checkpoints are generated at correct intervals."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           (let ((output (make-audit-output temp-file
                                           :checkpoint-interval 10)))
             ;; Write 25 entries (should trigger 2 checkpoints)
             (dotimes (i 25)
               (let ((entry (llog::%make-log-entry
                             :level llog:+info+
                             :timestamp (+ 1000000000 i)
                             :message (format nil "Entry ~D" i)
                             :logger-name "test"
                             :fields nil)))
                 (llog:write-entry output entry)))

             (llog:close-output output))

           ;; Read file and count checkpoints
           (let ((checkpoint-count 0))
             (with-open-file (in temp-file)
               (loop for line = (read-line in nil nil)
                     while line
                     when (search "\"checkpoint\"" line)
                     do (incf checkpoint-count)))
             ;; Should have 3 checkpoints: at 10, 20, and final (25)
             (is (= 3 checkpoint-count))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test audit-output-with-metadata ()
  "Test audit output with custom metadata."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           (let ((output (make-audit-output temp-file
                                           :metadata '(:system "test-system"
                                                      :version "1.0"))))
             (let ((entry (llog::%make-log-entry
                           :level llog:+info+
                           :timestamp 1000000000
                           :message "Test"
                           :logger-name "test"
                           :fields nil)))
               (llog:write-entry output entry))
             (llog:close-output output))

           ;; Verify metadata in file
           (let ((content (with-open-file (in temp-file)
                           (read-line in))))
             (is (search "test-system" content))
             (is (search "1.0" content))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test verify-valid-audit-file ()
  "Test verification of a valid audit file."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           ;; Create audit file with entries
           (let ((output (make-audit-output temp-file :checkpoint-interval 5)))
             (dotimes (i 12)
               (let ((entry (llog::%make-log-entry
                             :level llog:+info+
                             :timestamp (+ 1000000000 i)
                             :message (format nil "Entry ~D" i)
                             :logger-name "test"
                             :fields nil)))
                 (llog:write-entry output entry)))
             (llog:close-output output))

           ;; Verify the file
           (let ((result (llog-audit:verify-audit-file temp-file)))
             (is (llog-audit:verification-result-p result))
             (is (eql :valid (llog-audit:verification-result-status result)))
             (is (= 12 (llog-audit:verification-result-total-entries result)))
             ;; Should have 3 checkpoints: at 5, 10, and final (12)
             (is (= 3 (llog-audit:verification-result-checkpoints-verified result)))
             (is (null (llog-audit:verification-result-first-error result)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test detect-tampered-entry ()
  "Test that tampering with an entry is detected."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           ;; Create audit file
           (let ((output (make-audit-output temp-file)))
             (dotimes (i 5)
               (let ((entry (llog::%make-log-entry
                             :level llog:+info+
                             :timestamp (+ 1000000000 i)
                             :message (format nil "Entry ~D" i)
                             :logger-name "test"
                             :fields nil)))
                 (llog:write-entry output entry)))
             (llog:close-output output))

           ;; Tamper with the file - modify a message
           (let ((content (with-open-file (in temp-file)
                           (with-output-to-string (out)
                             (loop for line = (read-line in nil nil)
                                   while line
                                   do (write-line line out))))))
             (with-open-file (out temp-file
                                 :direction :output
                                 :if-exists :supersede)
               (write-string (cl-ppcre:regex-replace
                             "Entry 2"
                             content
                             "TAMPERED")
                            out)))

           ;; Verify should detect tampering
           (let ((result (llog-audit:verify-audit-file temp-file)))
             (is (eql :tampered (llog-audit:verification-result-status result)))
             (is (stringp (llog-audit:verification-result-first-error result)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(def-test verify-empty-file ()
  "Test verification of an empty file."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time))))
    (unwind-protect
         (progn
           ;; Create empty file
           (with-open-file (out temp-file
                               :direction :output
                               :if-exists :supersede)
             (declare (ignore out)))

           ;; Verify should report invalid
           (let ((result (llog-audit:verify-audit-file temp-file)))
             (is (eql :invalid (llog-audit:verification-result-status result)))
             (is (stringp (llog-audit:verification-result-first-error result)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;;; Signature Tests

(def-test create-signed-audit-output ()
  "Test creating an audit output with digital signatures."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time)))
        (temp-key-file (format nil "/tmp/llog-audit-key-~D.key"
                              (get-universal-time))))
    (unwind-protect
         ;; Generate a test Ed25519 private key (32 random bytes)
         (let ((private-key (ironclad:make-random-salt 32)))
             ;; Write key to file
             (with-open-file (out temp-key-file
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
               (write-sequence private-key out))

             ;; Create signed audit output
             (let ((output (make-audit-output temp-file
                                             :signing-key temp-key-file
                                             :signature-algorithm :ed25519
                                             :checkpoint-interval 5)))
               (is (audit-output-p output))
               (is (llog-audit::audit-output-signing-key output))
               (llog:close-output output)))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file))
      (when (probe-file temp-key-file)
        (delete-file temp-key-file)))))

(def-test verify-signed-audit-file ()
  "Test verification of signed audit logs."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time)))
        (temp-priv-key (format nil "/tmp/llog-audit-priv-~D.key"
                              (get-universal-time)))
        (temp-pub-key (format nil "/tmp/llog-audit-pub-~D.key"
                             (get-universal-time))))
    (unwind-protect
         ;; Generate Ed25519 key pair
         (let* ((private-key-bytes (ironclad:make-random-salt 32))
                  (private-key (ironclad:make-private-key :ed25519 :x private-key-bytes))
                  (public-key-bytes (ironclad:ed25519-key-y private-key)))

             ;; Write keys to files
             (with-open-file (out temp-priv-key
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
               (write-sequence private-key-bytes out))
             (with-open-file (out temp-pub-key
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
               (write-sequence public-key-bytes out))

             ;; Create signed audit file
             (let ((output (make-audit-output temp-file
                                             :signing-key temp-priv-key
                                             :checkpoint-interval 5)))
               (dotimes (i 12)
                 (let ((entry (llog::%make-log-entry
                               :level llog:+info+
                               :timestamp (+ 1000000000 i)
                               :message (format nil "Entry ~D" i)
                               :logger-name "test"
                               :fields nil)))
                   (llog:write-entry output entry)))
               (llog:close-output output))

             ;; Verify with public key
             (let ((result (llog-audit:verify-audit-file temp-file
                                                        :public-key temp-pub-key)))
               (is (eql :valid (llog-audit:verification-result-status result)))
               (is (= 12 (llog-audit:verification-result-total-entries result)))
               (is (= 3 (llog-audit:verification-result-checkpoints-verified result)))
               (is (null (llog-audit:verification-result-first-error result)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file))
      (when (probe-file temp-priv-key)
        (delete-file temp-priv-key))
      (when (probe-file temp-pub-key)
        (delete-file temp-pub-key)))))

(def-test detect-tampered-signature ()
  "Test that tampering with a signed checkpoint is detected."
  (let ((temp-file (format nil "/tmp/llog-audit-test-~D.log"
                           (get-universal-time)))
        (temp-priv-key (format nil "/tmp/llog-audit-priv-~D.key"
                              (get-universal-time)))
        (temp-pub-key (format nil "/tmp/llog-audit-pub-~D.key"
                             (get-universal-time))))
    (unwind-protect
         ;; Generate key pair
         (let* ((private-key-bytes (ironclad:make-random-salt 32))
                  (private-key (ironclad:make-private-key :ed25519 :x private-key-bytes))
                  (public-key-bytes (ironclad:ed25519-key-y private-key)))

             (with-open-file (out temp-priv-key
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
               (write-sequence private-key-bytes out))
             (with-open-file (out temp-pub-key
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
               (write-sequence public-key-bytes out))

             ;; Create signed audit file
             (let ((output (make-audit-output temp-file
                                             :signing-key temp-priv-key
                                             :checkpoint-interval 5)))
               (dotimes (i 6)
                 (let ((entry (llog::%make-log-entry
                               :level llog:+info+
                               :timestamp (+ 1000000000 i)
                               :message (format nil "Entry ~D" i)
                               :logger-name "test"
                               :fields nil)))
                   (llog:write-entry output entry)))
               (llog:close-output output))

             ;; Tamper with a checkpoint signature
             (let ((content (with-open-file (in temp-file)
                             (with-output-to-string (out)
                               (loop for line = (read-line in nil nil)
                                     while line
                                     do (write-line line out))))))
               (with-open-file (out temp-file
                                   :direction :output
                                   :if-exists :supersede)
                 (write-string (cl-ppcre:regex-replace
                               "signature\":\"[^\"]+\""
                               content
                               "signature\":\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\"")
                              out)))

             ;; Verify should detect tampering
             (let ((result (llog-audit:verify-audit-file temp-file
                                                        :public-key temp-pub-key)))
               (is (eql :tampered (llog-audit:verification-result-status result)))
               (is (search "signature" (llog-audit:verification-result-first-error result)))))
      ;; Cleanup
      (when (probe-file temp-file)
        (delete-file temp-file))
      (when (probe-file temp-priv-key)
        (delete-file temp-priv-key))
      (when (probe-file temp-pub-key)
        (delete-file temp-pub-key)))))
