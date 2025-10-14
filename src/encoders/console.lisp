;;;; encoders/console.lisp - Human-readable console encoder
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Console Encoder
;;; Produces human-readable output suitable for terminals

(defclass console-encoder (encoder)
  ((colors
    :initarg :colors
    :initform nil
    :accessor encoder-colors-p
    :type boolean
    :documentation "Whether to use ANSI colors"))
  (:documentation "Encoder that produces human-readable console output."))

(defun make-console-encoder (&key (colors nil))
  "Create a console encoder. If COLORS is T, uses ANSI color codes."
  (make-instance 'console-encoder :colors colors))

;;; ANSI Color Codes

(defparameter *ansi-reset* "[0m"
  "ANSI code to reset colors")

(defparameter *level-colors*
  `((,+trace+ . "[38;5;240m")    ; Dark gray
    (,+debug+ . "[38;5;246m")     ; Light gray
    (,+info+ . "[38;5;12m")       ; Bright blue
    (,+warn+ . "[38;5;11m")       ; Bright yellow
    (,+error+ . "[38;5;9m")       ; Bright red
    (,+fatal+ . "[38;5;1m")       ; Dark red
    (,+panic+ . "[38;5;5m"))      ; Magenta
  "ANSI color codes for each log level")

(defun level-color (level)
  "Get the ANSI color code for a log level."
  (rest (assoc level *level-colors*)))

;;; Console Encoding

(defmethod encode-entry ((encoder console-encoder) stream (entry log-entry))
  "Encode a log entry in human-readable console format."
  (let ((use-colors (encoder-colors-p encoder)))
    ;; Timestamp
    (when use-colors
      (write-string "[38;5;10m" stream))
    (format-timestamp (log-entry-timestamp entry) stream)
    (when use-colors
      (write-string *ansi-reset* stream))

    (write-char #\Space stream)

    ;; Level
    (write-char #\[ stream)
    (when use-colors
      (write-string (level-color (log-entry-level entry)) stream))
    (write-string (level-name (log-entry-level entry)) stream)
    (when use-colors
      (write-string *ansi-reset* stream))
    (write-char #\] stream)

    (write-char #\Space stream)

    ;; Logger name (if present)
    (unless (string= (log-entry-logger-name entry) "")
      (write-string (log-entry-logger-name entry) stream)
      (write-string ": " stream))

    ;; Message
    (write-string (log-entry-message entry) stream)

    ;; Fields (if any)
    (when (log-entry-fields entry)
      (terpri stream)
      (dolist (field (log-entry-fields entry))
        (write-string "  " stream)
        (write-string (field-name field) stream)
        (write-string ": " stream)
        (encode-field-console encoder stream field)
        (terpri stream)))

    ;; Final newline if no fields
    (unless (log-entry-fields entry)
      (terpri stream))))

(defgeneric encode-field-console (encoder stream field)
  (:documentation "Encode a field for console output"))

(defmethod encode-field-console ((encoder console-encoder) stream (field field))
  "Default field encoding for console"
  (case (field-type field)
    (:string
     (cl:write-string (field-value field) stream))
    (:int
     (format stream "~D" (field-value field)))
    (:float
     (format stream "~,3F" (field-value field)))
    (:duration-ms
     (format stream "~,3F ms" (field-value field)))
    (:bool
     (write-string (if (field-value field) "true" "false") stream))
    (:timestamp
     (format-timestamp (field-value field) stream))
    (:error
     (let ((condition (field-value field)))
       (format stream "~A (~A)"
               (princ-to-string condition)
               (type-of condition))))
    (:error-detailed
     (let ((info (field-value field)))
       (format stream "~A (~A)"
               (condition-info-message info)
               (condition-info-type info))
       (when (condition-info-backtrace info)
         (terpri stream)
         (write-string "    Backtrace:" stream)
         (dolist (frame (condition-info-backtrace info))
           (terpri stream)
           (write-string "      " stream)
           (write-string frame stream)))
       (when (condition-info-restarts info)
         (terpri stream)
         (write-string "    Restarts:" stream)
         (dolist (restart (condition-info-restarts info))
           (terpri stream)
           (format stream "      ~A: ~A"
                   (getf restart :name)
                   (getf restart :description))))
       (when (condition-info-cause info)
         (terpri stream)
         (write-string "    Caused by: " stream)
         (format stream "~A (~A)"
                 (princ-to-string (condition-info-cause info))
                 (type-of (condition-info-cause info))))))
    (otherwise
     (prin1 (field-value field) stream))))
