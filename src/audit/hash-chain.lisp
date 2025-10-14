;;;; hash-chain.lisp - Hash chain implementation for tamper detection
;;;; SPDX-License-Identifier: MIT

(in-package #:llog-audit)

;;; Hash Chain Structure
;;;
;;; Each log entry includes the hash of the previous entry:
;;; Entry 1: data + hash(header)
;;; Entry 2: data + hash(entry1)
;;; Entry 3: data + hash(entry2)
;;; ...
;;;
;;; Any modification breaks the chain, making tampering evident.

(defstruct hash-chain
  "Represents a cryptographic hash chain."
  (algorithm :sha256 :type keyword)
  (previous-hash nil :type (or null string))
  (entry-count 0 :type integer))

(defun hash-algorithm-digest-length (algorithm)
  "Return the digest length in bytes for ALGORITHM."
  (ecase algorithm
    (:sha256 32)
    (:sha512 64)
    (:sha3-256 32)))

(defun compute-entry-hash (entry previous-hash &optional (algorithm :sha256))
  "Compute the hash of ENTRY chained with PREVIOUS-HASH.
   Returns a base64-encoded string."
  (let ((digest (ironclad:make-digest algorithm)))
    ;; Include previous hash in the computation
    (when previous-hash
      (ironclad:update-digest digest
                               (cl-base64:base64-string-to-usb8-array previous-hash)))

    ;; Hash the log entry data
    (ironclad:update-digest digest
                             (babel:string-to-octets
                              (format nil "~A:~A:~A:~A"
                                      (llog:log-entry-timestamp entry)
                                      (llog:log-entry-level entry)
                                      (llog:log-entry-logger-name entry)
                                      (llog:log-entry-message entry))))

    ;; Hash the fields
    (dolist (field (llog:log-entry-fields entry))
      (ironclad:update-digest digest
                               (babel:string-to-octets
                                (format nil "~A:~A"
                                        (llog::field-name field)
                                        (prin1-to-string (llog::field-value field))))))

    ;; Return base64-encoded hash
    (cl-base64:usb8-array-to-base64-string
     (ironclad:produce-digest digest))))

(defun verify-chain (entries &optional (algorithm :sha256))
  "Verify the hash chain integrity of ENTRIES.
   Returns (values STATUS TAMPERED-INDEX) where:
   - STATUS is :VALID or :TAMPERED
   - TAMPERED-INDEX is the index where tampering was detected (or NIL)"
  (if (null entries)
      (values :valid nil)
      (let ((previous-hash nil))
        (loop for entry in entries
              for index from 0
              for stored-hash = (getf (llog::log-entry-metadata entry) :hash)
              for computed-hash = (compute-entry-hash entry previous-hash algorithm)
              do (unless (string= stored-hash computed-hash)
                   (return-from verify-chain (values :tampered index)))
                 (setf previous-hash stored-hash)
              finally (return (values :valid nil))))))

(defun init-hash-chain (&optional (algorithm :sha256))
  "Initialize a new hash chain."
  (make-hash-chain :algorithm algorithm
                   :previous-hash nil
                   :entry-count 0))

(defun chain-next-entry (chain entry)
  "Add ENTRY to CHAIN and return updated chain with new hash."
  (let* ((new-hash (compute-entry-hash entry
                                        (hash-chain-previous-hash chain)
                                        (hash-chain-algorithm chain)))
         (new-chain (copy-hash-chain chain)))
    (setf (hash-chain-previous-hash new-chain) new-hash
          (hash-chain-entry-count new-chain) (1+ (hash-chain-entry-count chain)))
    (values new-chain new-hash)))
