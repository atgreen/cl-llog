;;;; audit-output.lisp - Tamper-evident audit log output
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit)

;;; Audit Output
;;;
;;; An output type that writes logs with hash chaining and periodic checkpoints.
;;; Each entry includes the hash of the previous entry.
;;; Every N entries, a checkpoint is written with a Merkle root.

(defstruct (audit-output (:constructor %make-audit-output))
  "Tamper-evident audit output with hash chaining."
  (stream nil :type stream)
  (encoder nil :type t)
  (chain nil :type hash-chain)
  (checkpoint-interval 1000 :type integer)
  (pending-hashes nil :type list)
  (metadata nil :type list)
  (signing-key nil :type (or null t)) ; lint:suppress and-or-simplification - expresses intent
  (signature-algorithm :ed25519 :type keyword))

(defun make-audit-output (file-path &key
                                      (encoder (llog:make-json-encoder))
                                      (algorithm :sha256)
                                      (checkpoint-interval 1000)
                                      (metadata nil)
                                      (if-exists :supersede)
                                      (signing-key nil)
                                      (signature-algorithm :ed25519))
  "Create an audit output that writes to FILE-PATH with hash chaining.

   ENCODER - Encoder for log entries (default: JSON)
   ALGORITHM - Hash algorithm (:sha256, :sha512, :sha3-256)
   CHECKPOINT-INTERVAL - Emit checkpoint every N records
   METADATA - Additional metadata to include in audit file
   IF-EXISTS - What to do if file exists (:supersede, :append, :error)
   SIGNING-KEY - Private key for signing checkpoints (Ed25519 or RSA)
                 Can be: pathname, octets, or ironclad key object
   SIGNATURE-ALGORITHM - Signature algorithm (:ed25519 or :rsa)"
  (let* ((stream (open file-path ; lint:suppress use-with-open-file - stream stored in struct
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create))
         (loaded-key (when signing-key (load-signing-key signing-key signature-algorithm)))
         (output (%make-audit-output
                  :stream stream
                  :encoder encoder
                  :chain (init-hash-chain algorithm)
                  :checkpoint-interval checkpoint-interval
                  :pending-hashes nil
                  :metadata metadata
                  :signing-key loaded-key
                  :signature-algorithm signature-algorithm)))
    ;; Write audit file header
    (format stream "{\"audit-version\":\"1.0\",~
                     \"algorithm\":~S,~
                     \"checkpoint-interval\":~D,~
                     \"created\":~D,~
                     \"metadata\":~A}~%"
            (string-downcase (symbol-name algorithm))
            checkpoint-interval
            (get-universal-time)
            (format-metadata-as-json metadata))
    output))

(defun format-metadata-as-json (metadata)
  "Format METADATA plist as JSON object."
  (if (null metadata)
      "null"
      (with-output-to-string (s)
        (write-char #\{ s)
        (loop for (key value) on metadata by #'cddr
              for first = t then nil
              unless first do (write-string "," s)
              do (format s "~S:~S"
                        (string-downcase (symbol-name key))
                        value))
        (write-char #\} s))))

(defun load-signing-key (key-spec algorithm)
  "Load a signing key from KEY-SPEC.
   KEY-SPEC can be:
   - Pathname: read raw key bytes from file
   - Octets: use directly as key
   - Ironclad key object: use directly"
  (cond
    ;; Already an ironclad key object - check if it has the make-message-signature method
    ((and (not (or (pathnamep key-spec) (stringp key-spec)))
          (not (typep key-spec '(vector (unsigned-byte 8)))))
     ;; Assume it's already a key object
     key-spec)

    ;; Pathname - read raw key bytes
    ((or (pathnamep key-spec) (stringp key-spec))
     (let ((key-bytes (with-open-file (in key-spec
                                          :direction :input
                                          :element-type '(unsigned-byte 8))
                       (let ((bytes (make-array (file-length in)
                                               :element-type '(unsigned-byte 8))))
                         (read-sequence bytes in)
                         bytes))))
       (ecase algorithm
         (:ed25519
          ;; Ed25519 keys are 32 bytes for private key, 64 for keypair
          (unless (or (= (length key-bytes) 32) (= (length key-bytes) 64))
            (error "Ed25519 key must be 32 or 64 bytes, got ~D" (length key-bytes)))
          (if (= (length key-bytes) 32)
              (ironclad:make-private-key :ed25519 :x key-bytes)
              ;; 64-byte format: private key (32) + public key (32)
              (ironclad:make-private-key :ed25519
                                        :x (subseq key-bytes 0 32))))
         (:rsa
          ;; For RSA, key-bytes should be in some standard format
          ;; This is simplified - in practice you'd parse PEM/DER
          (error "RSA key loading not yet implemented")))))

    ;; Octets - use directly
    ((typep key-spec '(vector (unsigned-byte 8)))
     (ecase algorithm
       (:ed25519
        (unless (or (= (length key-spec) 32) (= (length key-spec) 64))
          (error "Ed25519 key must be 32 or 64 bytes, got ~D" (length key-spec)))
        (if (= (length key-spec) 32)
            (ironclad:make-private-key :ed25519 :x key-spec)
            (ironclad:make-private-key :ed25519
                                      :x (subseq key-spec 0 32))))
       (:rsa
        (error "RSA key loading not yet implemented"))))

    (t
     (error "Invalid key specification: ~S" key-spec))))

(defun sign-data (data signing-key algorithm)
  "Sign DATA (string or octets) with SIGNING-KEY using ALGORITHM.
   Returns base64-encoded signature."
  (let ((data-octets (if (stringp data)
                        (babel:string-to-octets data)
                        data)))
    (ecase algorithm
      (:ed25519
       (let ((signature (ironclad:sign-message signing-key data-octets)))
         (cl-base64:usb8-array-to-base64-string signature)))
      (:rsa
       (error "RSA signing not yet implemented")))))

(defmethod llog:write-entry ((output audit-output) (entry llog:log-entry))
  "Write ENTRY to audit output with hash chaining."
  (multiple-value-bind (new-chain new-hash)
      (chain-next-entry (audit-output-chain output) entry)

    ;; Write entry as JSON with audit metadata
    (format (audit-output-stream output)
            "{\"entry\":~A,\"audit\":{\"hash\":~S,\"number\":~D}}~%"
            (string-right-trim '(#\Newline #\Return #\Space #\Tab)
                              (with-output-to-string (s)
                                (llog::encode-entry (audit-output-encoder output) s entry)))
            new-hash
            (hash-chain-entry-count new-chain))

    ;; Update chain
    (setf (audit-output-chain output) new-chain)

    ;; Track hash for checkpoint
    (push new-hash (audit-output-pending-hashes output))

    ;; Check if we should emit a checkpoint
    (when (>= (length (audit-output-pending-hashes output))
              (audit-output-checkpoint-interval output))
      (write-checkpoint output))

    (values)))

(defun write-checkpoint (output)
  "Write a checkpoint to OUTPUT and reset pending hashes."
  (let* ((hashes (reverse (audit-output-pending-hashes output)))
         (checkpoint (make-checkpoint-from-hashes
                      hashes
                      :metadata (audit-output-metadata output))))

    ;; Sign the checkpoint if we have a signing key
    (when (audit-output-signing-key output)
      (let ((signature (sign-data (checkpoint-merkle-root checkpoint)
                                  (audit-output-signing-key output)
                                  (audit-output-signature-algorithm output))))
        (setf (checkpoint-signature checkpoint) signature)))

    ;; Write checkpoint as special entry
    (if (checkpoint-signature checkpoint)
        (format (audit-output-stream output)
                "{\"checkpoint\":true,~
                  \"record-count\":~D,~
                  \"merkle-root\":~S,~
                  \"timestamp\":~D,~
                  \"signature\":~S,~
                  \"signature-algorithm\":~S}~%"
                (checkpoint-record-count checkpoint)
                (checkpoint-merkle-root checkpoint)
                (checkpoint-timestamp checkpoint)
                (checkpoint-signature checkpoint)
                (string-downcase (symbol-name (audit-output-signature-algorithm output))))
        (format (audit-output-stream output)
                "{\"checkpoint\":true,~
                  \"record-count\":~D,~
                  \"merkle-root\":~S,~
                  \"timestamp\":~D}~%"
                (checkpoint-record-count checkpoint)
                (checkpoint-merkle-root checkpoint)
                (checkpoint-timestamp checkpoint)))

    ;; Clear pending hashes
    (setf (audit-output-pending-hashes output) nil)

    checkpoint))

(defmethod llog:close-output ((output audit-output))
  "Close audit output, writing final checkpoint if needed."
  ;; Write final checkpoint if there are pending entries
  (when (audit-output-pending-hashes output)
    (write-checkpoint output))

  ;; Close the stream
  (close (audit-output-stream output))
  (values))

(defstruct verification-result
  "Result of audit log verification."
  (status :valid :type keyword)
  (total-entries 0 :type integer)
  (checkpoints-verified 0 :type integer)
  (first-error nil :type (or null string))
  (error-line nil :type (or null integer)))

(defun load-public-key (key-spec algorithm)
  "Load a public key from KEY-SPEC for signature verification.
   KEY-SPEC can be:
   - Pathname: read raw key bytes from file
   - Octets: use directly as key
   - Ironclad key object: use directly"
  (cond
    ;; Already an ironclad key object
    ((and (not (or (pathnamep key-spec) (stringp key-spec)))
          (not (typep key-spec '(vector (unsigned-byte 8)))))
     ;; Assume it's already a key object
     key-spec)

    ;; Pathname - read raw key bytes
    ((or (pathnamep key-spec) (stringp key-spec))
     (let ((key-bytes (with-open-file (in key-spec
                                          :direction :input
                                          :element-type '(unsigned-byte 8))
                       (let ((bytes (make-array (file-length in)
                                               :element-type '(unsigned-byte 8))))
                         (read-sequence bytes in)
                         bytes))))
       (ecase algorithm
         (:ed25519
          ;; Ed25519 public keys are 32 bytes
          (unless (= (length key-bytes) 32)
            (error "Ed25519 public key must be 32 bytes, got ~D" (length key-bytes)))
          (ironclad:make-public-key :ed25519 :y key-bytes))
         (:rsa
          (error "RSA key loading not yet implemented")))))

    ;; Octets - use directly
    ((typep key-spec '(vector (unsigned-byte 8)))
     (ecase algorithm
       (:ed25519
        (unless (= (length key-spec) 32)
          (error "Ed25519 public key must be 32 bytes, got ~D" (length key-spec)))
        (ironclad:make-public-key :ed25519 :y key-spec))
       (:rsa
        (error "RSA key loading not yet implemented"))))

    (t
     (error "Invalid key specification: ~S" key-spec))))

(defun verify-signature (data signature public-key algorithm)
  "Verify that SIGNATURE (base64 string) is valid for DATA using PUBLIC-KEY.
   Returns T if valid, NIL if invalid."
  (handler-case
      (let ((data-octets (if (stringp data)
                            (babel:string-to-octets data)
                            data))
            (signature-octets (cl-base64:base64-string-to-usb8-array signature)))
        (ecase algorithm
          (:ed25519
           (ironclad:verify-signature public-key data-octets signature-octets))
          (:rsa
           (error "RSA verification not yet implemented"))))
    (error ()
      ;; Any error in verification means invalid signature
      nil)))

(defun verify-audit-file (file-path &key public-key)
  "Verify the integrity of an audit log file.
   Returns a verification-result structure.

   FILE-PATH - Path to audit log file
   PUBLIC-KEY - Optional public key for signature verification
                (pathname, octets, or ironclad key object)

   STATUS values:
   - :VALID - All entries and checkpoints verified successfully
   - :TAMPERED - Hash chain or checkpoint verification failed
   - :INVALID - File format error or missing data"
  (with-open-file (in file-path :direction :input)
    (let ((result (make-verification-result))
          (line-number 0)
          (previous-hash nil)
          (pending-hashes nil)
          (algorithm :sha256)
          (loaded-public-key nil)
          (signature-algorithm :ed25519))

      ;; Load public key if provided
      (when public-key
        (setf loaded-public-key (load-public-key public-key signature-algorithm)))

      ;; Read and parse header
      (let ((header-line (read-line in nil nil)))
        (incf line-number)
        (unless header-line
          (setf (verification-result-status result) :invalid
                (verification-result-first-error result) "Empty file"
                (verification-result-error-line result) line-number)
          (return-from verify-audit-file result))

        ;; Extract algorithm from header
        (let ((algo-start (search "\"algorithm\":" header-line)))
          (when algo-start
            (let* ((value-start (+ algo-start 12))
                   (value-end (position #\" header-line :start (1+ value-start)))
                   (algo-string (subseq header-line (1+ value-start) value-end)))
              (setf algorithm (intern (string-upcase algo-string) :keyword))))))

      ;; Read and verify each entry
      (loop for line = (read-line in nil nil)
            while line
            do (incf line-number)
               (cond
                 ;; Checkpoint line
                 ((search "\"checkpoint\"" line)
                  (handler-case
                      (let* ((merkle-start (search "\"merkle-root\":" line))
                             (value-start (position #\" line :start (+ merkle-start 14)))
                             (value-end (position #\" line :start (1+ value-start)))
                             (stored-root (subseq line (1+ value-start) value-end))
                             (computed-root (compute-merkle-root
                                            (reverse pending-hashes)
                                            algorithm))
                             ;; Extract signature if present
                             (sig-start (search "\"signature\":" line))
                             (signature (when sig-start
                                         (let* ((sig-val-start (position #\" line :start (+ sig-start 12)))
                                                (sig-val-end (position #\" line :start (1+ sig-val-start))))
                                           (subseq line (1+ sig-val-start) sig-val-end)))))

                        ;; Verify Merkle root
                        (unless (string= stored-root computed-root)
                          (setf (verification-result-status result) :tampered
                                (verification-result-first-error result)
                                "Checkpoint Merkle root mismatch"
                                (verification-result-error-line result) line-number)
                          (return))

                         ;; Verify signature if present and we have a public key
                        (when (and signature loaded-public-key
                                   (not (verify-signature stored-root signature
                                                         loaded-public-key signature-algorithm)))
                          (setf (verification-result-status result) :tampered
                                (verification-result-first-error result)
                                "Checkpoint signature verification failed"
                                (verification-result-error-line result) line-number)
                          (return))

                        ;; Checkpoint verified
                        (incf (verification-result-checkpoints-verified result))
                        (setf pending-hashes nil))
                    (error (e)
                      (setf (verification-result-status result) :invalid
                            (verification-result-first-error result)
                            (format nil "Checkpoint parse error: ~A" e)
                            (verification-result-error-line result) line-number)
                      (return))))

                 ;; Regular entry line
                 ((search "\"entry\"" line)
                  (handler-case
                      (let* (;; Extract stored hash
                             (hash-start (search "\"hash\":" line))
                             (hash-value-start (position #\" line :start (+ hash-start 7)))
                             (hash-value-end (position #\" line :start (1+ hash-value-start)))
                             (stored-hash (subseq line (1+ hash-value-start) hash-value-end))

                             ;; Extract entry data for hash computation
                             (entry-start (search "{\"entry\":" line))
                             (entry-data-start (position #\{ line :start (+ entry-start 9)))
                             (audit-start (search ",\"audit\":" line))
                             (entry-json (subseq line entry-data-start audit-start))

                             ;; Parse entry fields
                             (ts-str (parse-json-field entry-json "ts"))
                             (level-str (parse-json-field entry-json "level"))
                             (logger-name (parse-json-field entry-json "logger"))
                             (msg (parse-json-field entry-json "msg"))

                             ;; Parse timestamp from ISO 8601 format or unix timestamp
                             (timestamp (parse-timestamp ts-str))

                             ;; Parse level from string name
                             (level (level-from-name level-str))

                             ;; Create temporary entry for hash computation
                             (entry (llog::%make-log-entry
                                    :level level
                                    :timestamp timestamp
                                    :message msg
                                    :logger-name logger-name
                                    :fields nil))

                             ;; Compute expected hash
                             (computed-hash (compute-entry-hash entry previous-hash algorithm)))

                        (if (string= stored-hash computed-hash)
                            (progn
                              (incf (verification-result-total-entries result))
                              (push stored-hash pending-hashes)
                              (setf previous-hash stored-hash))
                            (progn
                              (setf (verification-result-status result) :tampered
                                    (verification-result-first-error result)
                                    (format nil "Hash mismatch at entry ~D"
                                            (verification-result-total-entries result))
                                    (verification-result-error-line result) line-number)
                              (return))))
                    (error (e)
                      (setf (verification-result-status result) :invalid
                            (verification-result-first-error result)
                            (format nil "Entry parse error: ~A" e)
                            (verification-result-error-line result) line-number)
                      (return))))

                 ;; Unknown line (header, blank line, etc.) - ignore
                 (otherwise
                  nil)))

      result)))

(defun parse-json-field (json-string field-name)
  "Simple JSON field parser for verification.
   Returns the value of FIELD-NAME from JSON-STRING."
  (let* ((field-pattern (format nil "\"~A\":" field-name))
         (start (search field-pattern json-string)))
    (when start
      (let ((value-start (+ start (length field-pattern))))
        ;; Skip whitespace
        (loop while (and (< value-start (length json-string))
                        (member (char json-string value-start) '(#\Space #\Tab)))
              do (incf value-start))

        (if (char= (char json-string value-start) #\")
            ;; String value
            (let ((value-end (position #\" json-string :start (1+ value-start))))
              (subseq json-string (1+ value-start) value-end))
            ;; Number value
            (let ((value-end (or (position #\, json-string :start value-start)
                                (position #\} json-string :start value-start))))
              (string-trim '(#\Space #\Tab)
                          (subseq json-string value-start value-end))))))))

(defun parse-timestamp (ts-string)
  "Parse timestamp from ISO 8601 string or unix timestamp.
   Returns universal time integer."
  (if (and ts-string
           ;; Check if it's all digits (unix timestamp)
           (every #'digit-char-p ts-string))
      ;; Unix timestamp (number)
      (parse-integer ts-string)
      ;; ISO 8601 format - just extract year/month/day/time for now
      ;; Format: "1931-09-09T21:46:40"
      ;; This is a simplified parser - a full ISO 8601 parser would be more robust
      ;; Note: Uses local timezone to match the encoder's behavior
      (if ts-string
          (let* ((year (parse-integer ts-string :start 0 :end 4))
                 (month (parse-integer ts-string :start 5 :end 7))
                 (day (parse-integer ts-string :start 8 :end 10))
                 (hour (parse-integer ts-string :start 11 :end 13))
                 (minute (parse-integer ts-string :start 14 :end 16))
                 (second (parse-integer ts-string :start 17 :end 19)))
            (encode-universal-time second minute hour day month year))
          0)))

(defun level-from-name (level-name)
  "Convert level name string to level integer.
   Maps: trace→0, debug→1, info→2, warn→3, error→4, fatal→5, panic→6"
  (cond
    ((string= level-name "trace") 0)
    ((string= level-name "debug") 1)
    ((string= level-name "info") 2)
    ((string= level-name "warn") 3)
    ((string= level-name "error") 4)
    ((string= level-name "fatal") 5)
    ((string= level-name "panic") 6)
    (t (error "Unknown level name: ~A" level-name))))
