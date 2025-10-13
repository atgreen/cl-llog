;;;; encoders/encoder.lisp - Base encoder protocol
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Encoder Protocol
;;; All encoders implement these methods

(defclass encoder ()
  ()
  (:documentation "Base class for all encoders. Encoders transform log entries into formatted output."))

(defgeneric encode-entry (encoder stream entry)
  (:documentation "Encode a log ENTRY to STREAM using ENCODER.
   This is the main entry point for encoding."))

(defgeneric encode-field (encoder stream field)
  (:documentation "Encode a single FIELD to STREAM using ENCODER.
   Can be specialized for specific encoder and field type combinations."))

;;; Default field encoding (fallback for :any type)

(defmethod encode-field ((encoder encoder) stream (field field))
  "Default field encoding - calls prin1 on the value."
  (prin1 (field-value field) stream))
