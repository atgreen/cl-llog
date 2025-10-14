;;;; audit-output.lisp - Tamper-evident audit log output
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit)

;;; Audit Output
;;;
;;; An output type that writes logs with hash chaining and periodic checkpoints.
;;; Each entry includes the hash of the previous entry.
;;; Every N entries, a checkpoint is written with a Merkle root.

(defstruct audit-output
  "Tamper-evident audit output with hash chaining."
  (stream nil :type stream)
  (encoder nil :type t)
  (chain nil :type hash-chain)
  (checkpoint-interval 1000 :type integer)
  (pending-hashes nil :type list)
  (metadata nil :type list))

(defun make-audit-output (file-path &key
                                      (encoder (llog:make-json-encoder))
                                      (algorithm :sha256)
                                      (checkpoint-interval 1000)
                                      (metadata nil)
                                      (if-exists :supersede))
  "Create an audit output that writes to FILE-PATH with hash chaining.

   ENCODER - Encoder for log entries (default: JSON)
   ALGORITHM - Hash algorithm (:sha256, :sha512, :sha3-256)
   CHECKPOINT-INTERVAL - Emit checkpoint every N records
   METADATA - Additional metadata to include in audit file
   IF-EXISTS - What to do if file exists (:supersede, :append, :error)"
  (let* ((stream (open file-path ; lint:suppress use-with-open-file - stream stored in struct
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create))
         (output (make-audit-output
                  :stream stream
                  :encoder encoder
                  :chain (init-hash-chain algorithm)
                  :checkpoint-interval checkpoint-interval
                  :pending-hashes nil
                  :metadata metadata)))
    ;; Write audit file header
    (format stream "{\"audit-version\":\"1.0\",~
                     \"algorithm\":~S,~
                     \"checkpoint-interval\":~D,~
                     \"created\":~D,~
                     \"metadata\":~S}~%"
            (string-downcase (symbol-name algorithm))
            checkpoint-interval
            (get-universal-time)
            metadata)
    output))

(defmethod llog:write-entry ((output audit-output) (entry llog:log-entry))
  "Write ENTRY to audit output with hash chaining."
  (let* ((chain (audit-output-chain output))
         (new-chain new-hash)
         (values (chain-next-entry chain entry)))

    ;; Store hash in entry metadata
    (setf (getf (llog::log-entry-metadata entry) :hash) new-hash
          (getf (llog::log-entry-metadata entry) :entry-number)
          (hash-chain-entry-count new-chain))

    ;; Write entry with hash
    (llog::encode-entry (audit-output-encoder output)
                        (audit-output-stream output)
                        entry)
    (terpri (audit-output-stream output))

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

    ;; Write checkpoint as special entry
    (format (audit-output-stream output)
            "{\"checkpoint\":true,~
              \"record-count\":~D,~
              \"merkle-root\":~S,~
              \"timestamp\":~D}~%"
            (checkpoint-record-count checkpoint)
            (checkpoint-merkle-root checkpoint)
            (checkpoint-timestamp checkpoint))

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

(defun verify-audit-file (file-path)
  "Verify the integrity of an audit log file.
   Returns a verification result structure."
  (declare (ignore file-path))
  ;; TODO: Implement full verification
  ;; 1. Read header
  ;; 2. Verify hash chain
  ;; 3. Verify checkpoints
  ;; 4. Check signatures (if present)
  (error "Audit verification not yet implemented"))
