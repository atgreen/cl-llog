;;;; entry.lisp - Log entry structure
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Log Entry Structure
;;; Using defstruct for performance - fixed layout, fast access

(defstruct (log-entry (:constructor %make-log-entry)
                      (:copier nil))
  "A log entry representing a single log event."
  (level 0 :type (integer 0 6))
  (timestamp 0 :type unsigned-byte)
  (message "" :type cl:string)
  (logger-name "" :type cl:string)
  (fields nil :type list))

(declaim (inline make-log-entry))
(defun make-log-entry (level message &key logger-name fields)
  "Create a log entry with the specified LEVEL and MESSAGE.
   LOGGER-NAME is the name of the logger that created this entry.
   FIELDS is a list of field structures."
  (declare (type (integer 0 6) level)
           (type cl:string message)
           (optimize speed))
  (%make-log-entry
   :level level
   :timestamp (get-universal-time)
   :message message
   :logger-name (or logger-name "")
   :fields fields))

;;; Timestamp utilities

(defun timestamp-to-universal (universal-time)
  "Identity function for compatibility - timestamps are now stored as universal time."
  (declare (type unsigned-byte universal-time)
           (optimize speed (safety 1)))
  universal-time)

(defun format-timestamp (timestamp &optional (stream *standard-output*))
  "Format a timestamp (universal time) to ISO8601-ish format."
  (declare (type unsigned-byte timestamp)
           (optimize speed))
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time timestamp)
    (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))
