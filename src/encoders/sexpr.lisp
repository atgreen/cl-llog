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

(declaim (inline %field-keyword))
(defun %field-keyword (field)
  "Convert FIELD name to a keyword symbol."
  (intern (string-upcase (field-name field)) :keyword))

(defun %sexpr-field-value (field)
  "Return the S-expression value to emit for FIELD."
  (case (field-type field)
    (:error
     (let ((condition (field-value field)))
       (list :type (type-of condition)
             :message (princ-to-string condition))))
    (otherwise
     (field-value field))))

(defmethod encode-entry ((encoder sexpr-encoder) stream (entry log-entry))
  "Encode ENTRY as a keyword/value property list S-expression."
  (let* ((base (list :level (level-keyword (log-entry-level entry))
                     :ts (log-entry-timestamp entry)
                     :msg (log-entry-message entry)))
         (with-logger (if (or (null (log-entry-logger-name entry))
                              (string= (log-entry-logger-name entry) ""))
                          base
                          (nconc base (list :logger (log-entry-logger-name entry)))))
         (field-plist (loop for field in (log-entry-fields entry)
                            nconc (list (%field-keyword field)
                                        (%sexpr-field-value field)))))
    (prin1 (nconc with-logger field-plist) stream)
    (terpri stream)))
