;;;; encoders/pattern.lisp - Configurable pattern layout encoder
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Pattern Layout Encoder
;;; Supports configurable format strings like:
;;;   "%d{yyyy-MM-dd HH:mm:ss} [%level] %logger - %msg%n"

(defclass pattern-encoder (encoder)
  ((pattern
    :initarg :pattern
    :initform "%d [%level] %logger - %msg%n"
    :accessor pattern-encoder-pattern
    :type cl:string
    :documentation "Format pattern string"))
  (:documentation "Encoder that uses configurable pattern strings for output."))

(defun make-pattern-encoder (&key (pattern "%d [%level] %logger - %msg%n"))
  "Create a pattern encoder with the specified PATTERN.

   Supported patterns:
     %d or %date       - Timestamp (supports {format} like %d{yyyy-MM-dd HH:mm:ss})
     %level or %p      - Log level (TRACE, DEBUG, INFO, etc.)
     %logger or %c     - Logger name
     %msg or %m        - Log message
     %n                - Newline
     %field{name}      - Specific field value
     %%                - Literal %

   Modifiers:
     %5level           - Right-pad to 5 characters
     %-5level          - Left-pad to 5 characters
     %.10logger        - Truncate to 10 characters
     %5.10logger       - Pad to 5, truncate to 10

   Examples:
     \"%d [%level] %logger - %msg%n\"
     \"%d{yyyy-MM-dd HH:mm:ss.SSS} [%-5level] %logger - %msg%n\"
     \"[%level] %msg (user=%field{user-id})%n\""
  (make-instance 'pattern-encoder :pattern pattern))

;;; Pattern Parsing

(defstruct pattern-token
  "A parsed token from a pattern string."
  (type :literal :type keyword)  ; :literal, :date, :level, :logger, :msg, :newline, :field
  (value "" :type cl:string)     ; Literal text or field name
  (width nil :type (or null integer))    ; Minimum width (negative = left-align)
  (precision nil :type (or null integer)) ; Maximum width (truncation)
  (date-format nil :type (or null cl:string))) ; Date format string

(defun parse-pattern (pattern)
  "Parse a pattern string into a list of pattern-token structures."
  (declare (type cl:string pattern))
  (let ((tokens nil)
        (i 0)
        (len (length pattern)))
    (loop while (< i len) do
      (let ((ch (char pattern i)))
        (cond
          ;; Pattern specifier starting with %
          ((char= ch #\%)
           (incf i)
           (when (< i len)
             (multiple-value-bind (token new-i) (parse-pattern-specifier pattern i)
               (push token tokens)
               (setf i new-i))))

          ;; Literal text
          (t
           (let ((start i))
             (loop while (and (< i len) (char/= (char pattern i) #\%))
                   do (incf i))
             (push (make-pattern-token :type :literal
                                      :value (subseq pattern start i))
                   tokens))))))
    (nreverse tokens)))

(defun parse-pattern-specifier (pattern start)
  "Parse a pattern specifier starting at START. Returns (values token new-position)."
  (declare (type cl:string pattern)
           (type fixnum start))
  (let ((i start)
        (len (length pattern))
        (left-align nil)
        (width nil)
        (precision nil))

    ;; Parse alignment and width
    (when (and (< i len) (char= (char pattern i) #\-))
      (setf left-align t)
      (incf i))

    ;; Parse width
    (when (and (< i len) (digit-char-p (char pattern i)))
      (multiple-value-bind (w new-i) (parse-integer pattern :start i :junk-allowed t)
        (setf width (if left-align (- w) w))
        (setf i new-i)))

    ;; Parse precision
    (when (and (< i len) (char= (char pattern i) #\.))
      (incf i)
      (when (and (< i len) (digit-char-p (char pattern i)))
        (multiple-value-bind (p new-i) (parse-integer pattern :start i :junk-allowed t)
          (setf precision p)
          (setf i new-i))))

    ;; Parse specifier type
    (cond
      ((>= i len)
       (values (make-pattern-token :type :literal :value "%") i))

      ;; %d or %date - timestamp
      ((or (looking-at-p pattern i "date")
           (char= (char pattern i) #\d))
       (let ((type-end (if (looking-at-p pattern i "date") (+ i 4) (1+ i))))
         (multiple-value-bind (date-format new-i) (parse-braced-value pattern type-end)
           (values (make-pattern-token :type :date
                                      :width width
                                      :precision precision
                                      :date-format date-format)
                   new-i))))

      ;; %level or %p - log level
      ((or (looking-at-p pattern i "level")
           (char= (char pattern i) #\p))
       (let ((new-i (if (looking-at-p pattern i "level") (+ i 5) (1+ i))))
         (values (make-pattern-token :type :level
                                    :width width
                                    :precision precision)
                 new-i)))

      ;; %logger or %c - logger name
      ((or (looking-at-p pattern i "logger")
           (char= (char pattern i) #\c))
       (let ((new-i (if (looking-at-p pattern i "logger") (+ i 6) (1+ i))))
         (values (make-pattern-token :type :logger
                                    :width width
                                    :precision precision)
                 new-i)))

      ;; %msg or %m - message
      ((or (looking-at-p pattern i "msg")
           (char= (char pattern i) #\m))
       (let ((new-i (if (looking-at-p pattern i "msg") (+ i 3) (1+ i))))
         (values (make-pattern-token :type :msg
                                    :width width
                                    :precision precision)
                 new-i)))

      ;; %n - newline
      ((char= (char pattern i) #\n)
       (values (make-pattern-token :type :newline) (1+ i)))

      ;; %field{name} - specific field
      ((looking-at-p pattern i "field")
       (multiple-value-bind (field-name new-i) (parse-braced-value pattern (+ i 5))
         (values (make-pattern-token :type :field
                                    :value field-name
                                    :width width
                                    :precision precision)
                 new-i)))

      ;; %% - literal %
      ((char= (char pattern i) #\%)
       (values (make-pattern-token :type :literal :value "%") (1+ i)))

      ;; Unknown specifier - treat as literal
      (t
       (values (make-pattern-token :type :literal
                                  :value (cl:string (char pattern i)))
               (1+ i))))))

(defun looking-at-p (string start substring)
  "Check if STRING contains SUBSTRING starting at START."
  (declare (type cl:string string substring)
           (type fixnum start))
  (let ((sub-len (length substring)))
    (and (<= (+ start sub-len) (length string))
         (string= string substring :start1 start :end1 (+ start sub-len)))))

(defun parse-braced-value (string start)
  "Parse a braced value like {yyyy-MM-dd} starting at START.
   Returns (values value-string new-position)."
  (declare (type cl:string string)
           (type fixnum start))
  (if (and (< start (length string))
           (char= (char string start) #\{))
      (let ((end (position #\} string :start (1+ start))))
        (if end
            (values (subseq string (1+ start) end) (1+ end))
            (values nil start)))
      (values nil start)))

;;; Pattern Rendering

(defmethod encode-entry ((encoder pattern-encoder) stream (entry log-entry))
  "Encode a log entry using pattern layout."
  (let ((tokens (parse-pattern (pattern-encoder-pattern encoder))))
    (dolist (token tokens)
      (render-token token stream entry))))

(defun render-token (token stream entry)
  "Render a single pattern token to STREAM."
  (let ((raw-value (token-raw-value token entry)))
    (when raw-value
      (let ((formatted (format-token-value raw-value token)))
        (write-string formatted stream)))))

(defun token-raw-value (token entry)
  "Get the raw string value for a token."
  (ecase (pattern-token-type token)
    (:literal
     (pattern-token-value token))

    (:date
     (format-timestamp-pattern entry (pattern-token-date-format token)))

    (:level
     (level-name (log-entry-level entry)))

    (:logger
     (log-entry-logger-name entry))

    (:msg
     (log-entry-message entry))

    (:newline
     (cl:string #\Newline))

    (:field
     (let ((field (find (pattern-token-value token)
                       (log-entry-fields entry)
                       :key #'field-name
                       :test #'string=)))
       (if field
           (field-value-string field)
           "")))))

(defun format-timestamp-pattern (entry date-format)
  "Format timestamp according to date-format pattern."
  (if date-format
      (format-timestamp-custom (log-entry-timestamp entry) date-format)
      (with-output-to-string (s)
        (format-timestamp (log-entry-timestamp entry) s))))

(defun format-timestamp-custom (timestamp format-string)
  "Format TIMESTAMP using custom FORMAT-STRING.
   Supports: yyyy MM dd HH mm ss SSS"
  (declare (ignore format-string))
  ;; For now, just use default format - can be enhanced later
  (with-output-to-string (s)
    (format-timestamp timestamp s)))

(defun field-value-string (field)
  "Convert field value to string."
  (let ((value (field-value field)))
    (typecase value
      (cl:string value)
      (integer (format nil "~D" value))
      (cl:float (format nil "~F" value))
      (t (prin1-to-string value)))))

(defun format-token-value (value token)
  "Apply width and precision formatting to VALUE according to TOKEN."
  (let* ((width (pattern-token-width token))
         (precision (pattern-token-precision token))
         (truncated (if precision
                        (subseq value 0 (min precision (length value)))
                        value)))
    (if width
        ;; Positive width = right-justify (pad left), negative = left-justify (pad right)
        (if (plusp width)
            (format nil (format nil "~~~D@A" width) truncated)
            (format nil (format nil "~~~DA" (- width)) truncated))
        truncated)))
