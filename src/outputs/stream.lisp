;;;; outputs/stream.lisp - Stream output
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Stream Output
;;; Writes log entries to a Lisp stream

(defclass stream-output (output)
  ((stream
    :initarg :stream
    :accessor output-stream
    :documentation "The stream to write to"))
  (:documentation "Output that writes to a Common Lisp stream."))

(defun make-stream-output (stream &key (encoder (make-console-encoder))
                                       (min-level +trace+))
  "Create a stream output that writes to STREAM.

   STREAM - a Common Lisp output stream
   ENCODER - encoder to use for formatting (default: console encoder)
   MIN-LEVEL - minimum log level for this output"
  (let ((level (if (integerp min-level)
                   min-level
                   (or (parse-level min-level) +trace+))))
    (make-instance 'stream-output
                   :stream stream
                   :encoder encoder
                   :min-level level)))

(defmethod write-entry ((output stream-output) (entry log-entry))
  "Write a log entry to the stream."
  ;; Check if this entry meets the minimum level
  (when (level>= (log-entry-level entry) (output-min-level output))
    (encode-entry (output-encoder output)
                  (output-stream output)
                  entry)
    (force-output (output-stream output))))

(defmethod flush-output ((output stream-output))
  "Flush the stream."
  (force-output (output-stream output)))
