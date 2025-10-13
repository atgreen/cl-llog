;;;; encoders/json.lisp - JSON encoder
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; JSON Encoder
;;; Produces JSON output for log aggregation systems

(defclass json-encoder (encoder)
  ()
  (:documentation "Encoder that produces JSON output."))

(defun make-json-encoder ()
  "Create a JSON encoder."
  (make-instance 'json-encoder))

;;; TODO: Implement JSON encoding
;;; This is a stub for now - will be implemented in Phase 2
