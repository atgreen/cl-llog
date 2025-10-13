;;;; outputs/output.lisp - Base output protocol
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Output Protocol
;;; All outputs implement these methods

(defclass output ()
  ((encoder
    :initarg :encoder
    :accessor output-encoder
    :documentation "The encoder used to format log entries")
   (min-level
    :initarg :min-level
    :initform +trace+
    :accessor output-min-level
    :type (integer 0 6)
    :documentation "Minimum level for this output"))
  (:documentation "Base class for all output destinations."))

(defgeneric write-entry (output entry)
  (:documentation "Write a log ENTRY to OUTPUT.
   This is the main entry point for writing log entries."))

(defgeneric flush-output (output)
  (:documentation "Flush any buffered data in OUTPUT.
   Default implementation does nothing."))

(defmethod flush-output ((output output))
  "Default flush implementation - does nothing"
  (values))

(defgeneric close-output (output)
  (:documentation "Close OUTPUT and release any resources it holds."))

(defmethod close-output ((output output))
  "Default close implementation - does nothing"
  (values))
