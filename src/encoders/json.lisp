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

(declaim (inline %write-json-string))
(defun %write-json-string (stream string)
  "Write STRING to STREAM with JSON escaping."
  (declare (type cl:string string))
  (write-char #\" stream)
  (loop for ch across string
        for code = (char-code ch)
        do (case ch
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Backspace (write-string "\\b" stream))
             (#\FormFeed (write-string "\\f" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (if (< code 32)
                  (format stream "\\u~4,'0X" code)
                  (write-char ch stream)))))
  (write-char #\" stream))

(defun %write-json-number (stream number)
  "Write NUMBER to STREAM using JSON-compatible notation."
  (let* ((raw (write-to-string number :readably nil :pretty nil))
         (json (substitute #\e #\d (substitute #\E #\D raw))))
    (write-string json stream)))

(defun %format-timestamp-string (timestamp)
  "Return an ISO8601-like string for internal TIMESTAMP."
  (with-output-to-string (buffer)
    (format-timestamp timestamp buffer)))

(defun %encode-json-condition (stream condition)
  "Serialize CONDITION to a JSON object."
  (write-char #\{ stream)
  (%write-json-string stream "type")
  (write-char #\: stream)
  (%write-json-string stream (prin1-to-string (type-of condition)))
  (write-char #\, stream)
  (%write-json-string stream "message")
  (write-char #\: stream)
  (%write-json-string stream (princ-to-string condition))
  (write-char #\} stream))

(defmethod encode-entry ((encoder json-encoder) stream (entry log-entry))
  "Encode ENTRY as a single JSON object."
  (write-char #\{ stream)
  (let ((first t))
    (labels ((add-field (thunk)
               (if first
                   (setf first nil)
                   (write-char #\, stream))
               (funcall thunk))
             (add-string (key value)
               (add-field (lambda ()
                            (%write-json-string stream key)
                            (write-char #\: stream)
                            (%write-json-string stream value))))
             (add-raw (key thunk)
               (add-field (lambda ()
                            (%write-json-string stream key)
                            (write-char #\: stream)
                            (funcall thunk)))))
      (add-string "level"
                  (string-downcase (level-name (log-entry-level entry))))
      (add-string "ts"
                  (%format-timestamp-string (log-entry-timestamp entry)))
      (when (and (log-entry-logger-name entry)
                 (plusp (length (log-entry-logger-name entry))))
        (add-string "logger" (log-entry-logger-name entry)))
      (add-string "msg" (log-entry-message entry))
      (dolist (field (log-entry-fields entry))
        (add-raw (field-name field)
                 (lambda ()
                   (encode-field encoder stream field))))))
  (write-char #\} stream)
  (terpri stream))

(defmethod encode-field ((encoder json-encoder) stream (field field))
  "Encode FIELD value in JSON form."
  (case (field-type field)
    (:string
     (%write-json-string stream (field-value field)))
    (:int
     (%write-json-number stream (field-value field)))
    (:float
     (%write-json-number stream (field-value field)))
    (:duration-ms
     (%write-json-number stream (field-value field)))
    (:bool
     (write-string (if (field-value field) "true" "false") stream))
    (:timestamp
     (%write-json-string stream
                         (%format-timestamp-string (field-value field))))
    (:error
     (%encode-json-condition stream (field-value field)))
    (otherwise
     (%write-json-string stream (prin1-to-string (field-value field)))))
  (values))
