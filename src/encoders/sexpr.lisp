;;;; encoders/sexpr.lisp - S-expression encoder
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; S-Expression Encoder
;;; Produces Lisp-native S-expression output

(defclass sexpr-encoder (encoder)
  ()
  (:documentation "Encoder that produces S-expression output."))

(defun make-sexpr-encoder ()
  "Create an S-expression encoder."
  (make-instance 'sexpr-encoder))

;;; TODO: Implement S-expression encoding
;;; This is a stub for now - will be implemented in Phase 2
