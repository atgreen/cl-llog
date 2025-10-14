;;;; checkpoints.lisp - Periodic signed checkpoints for audit logs
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit)

;;; Checkpoints
;;;
;;; Every N records, emit a signed checkpoint containing:
;;; - Record count
;;; - Merkle root (hash of all hashes in this batch)
;;; - Timestamp
;;; - Digital signature (optional)
;;;
;;; This provides:
;;; 1. Faster verification (verify checkpoints instead of every record)
;;; 2. Non-repudiation (if signed with private key)
;;; 3. Tamper detection even if attacker recomputes hashes

(defstruct checkpoint
  "Represents a signed checkpoint in the audit trail."
  (record-count 0 :type integer)
  (merkle-root nil :type (or null string))
  (timestamp 0 :type integer)
  (signature nil :type (or null string))
  (metadata nil :type list))

(defun compute-merkle-root (hashes &optional (algorithm :sha256))
  "Compute the Merkle root of HASHES.
   This is a simple hash-of-hashes for now.
   A full Merkle tree implementation would be more efficient for large batches."
  (if (null hashes)
      nil
      (let ((digest (ironclad:make-digest algorithm)))
        (dolist (hash hashes)
          (ironclad:update-digest digest
                                   (cl-base64:base64-string-to-usb8-array hash)))
        (cl-base64:usb8-array-to-base64-string
         (ironclad:produce-digest digest)))))

(defun make-checkpoint-from-hashes (hashes &key metadata)
  "Create a checkpoint from a list of entry HASHES."
  (make-checkpoint
   :record-count (length hashes)
   :merkle-root (compute-merkle-root hashes)
   :timestamp (get-universal-time)
   :metadata metadata))

(defun verify-checkpoint (checkpoint hashes &optional (algorithm :sha256))
  "Verify that CHECKPOINT matches the given HASHES.
   Returns T if valid, NIL if tampered."
  (let ((computed-root (compute-merkle-root hashes algorithm)))
    (string= (checkpoint-merkle-root checkpoint) computed-root)))

(defun format-checkpoint (checkpoint stream)
  "Format CHECKPOINT as a human-readable string to STREAM."
  (format stream "~&[CHECKPOINT]~%")
  (format stream "  Records: ~D~%" (checkpoint-record-count checkpoint))
  (format stream "  Merkle Root: ~A~%" (checkpoint-merkle-root checkpoint))
  (format stream "  Timestamp: ~A~%"
          (llog::format-timestamp (checkpoint-timestamp checkpoint)))
  (when (checkpoint-signature checkpoint)
    (format stream "  Signature: ~A~%" (checkpoint-signature checkpoint)))
  (when (checkpoint-metadata checkpoint)
    (format stream "  Metadata: ~S~%" (checkpoint-metadata checkpoint))))
