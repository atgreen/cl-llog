;;;; repl.lisp - REPL integration helpers
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Global Circular Buffer for Recent Logs

(defvar *recent-logs* nil
  "Circular buffer storing recent log entries for REPL inspection.")

(defvar *recent-logs-size* 100
  "Number of recent log entries to keep in memory.")

(defvar *recent-logs-index* 0
  "Current write position in the circular buffer.")

(defvar *recent-logs-count* 0
  "Number of entries written (for tracking buffer fill).")

(defvar *recent-logs-lock* (make-lock "llog/recent-logs")
  "Lock protecting the recent logs buffer.")

(defun initialize-recent-logs (&optional (size 100))
  "Initialize or resize the recent logs circular buffer."
  (with-lock-held (*recent-logs-lock*)
    (setf *recent-logs-size* size
          *recent-logs* (make-array size :initial-element nil)
          *recent-logs-index* 0
          *recent-logs-count* 0)))

(defun %record-recent-log (logger entry)
  "Internal function to record a log entry in the recent buffer."
  (declare (ignore logger))
  (with-lock-held (*recent-logs-lock*)
    (unless *recent-logs*
      (initialize-recent-logs))
    (setf (aref *recent-logs* *recent-logs-index*) (copy-log-entry entry)
          *recent-logs-index* (mod (1+ *recent-logs-index*) *recent-logs-size*))
    (when (< *recent-logs-count* *recent-logs-size*)
      (incf *recent-logs-count*))))

(defun enable-recent-logs (&optional (logger *logger*) (size 100))
  "Enable recording of recent log entries for LOGGER.
   SIZE determines how many entries to keep (default: 100)."
  (ensure-default-logger)
  (initialize-recent-logs size)
  (add-hook logger :post-log #'%record-recent-log
            :name 'record-recent-logs
            :priority 100))

(defun disable-recent-logs (&optional (logger *logger*))
  "Disable recording of recent log entries for LOGGER."
  (ensure-default-logger)
  (remove-hook logger :post-log :name 'record-recent-logs))

;;; Show Recent Logs

(defun show-recent (&key (count 10) level logger-name pattern (stream *standard-output*))
  "Display recent log entries from the circular buffer.

   COUNT - Number of entries to show (default: 10, :all for all)
   LEVEL - Only show entries at or above this level (keyword)
   LOGGER-NAME - Only show entries from this logger (string)
   PATTERN - Only show entries matching this regex pattern (string)
   STREAM - Output stream (default: *standard-output*)

   Examples:
     (show-recent)                          ; Last 10 entries
     (show-recent :count :all)              ; All buffered entries
     (show-recent :level :error)            ; Only errors
     (show-recent :logger-name \"app.db\")    ; Only from app.db logger
     (show-recent :pattern \"failed\")        ; Only entries matching 'failed'"
  (with-lock-held (*recent-logs-lock*)
    (unless *recent-logs*
      (format stream "~&No recent logs available. Call (enable-recent-logs) first.~%")
      (return-from show-recent nil))

    (let* ((entries (collect-recent-entries))
           (level-int (when level (parse-level level)))
           (scanner (when pattern (cl-ppcre:create-scanner pattern :case-insensitive-mode t)))
           (filtered (remove-if-not
                      (lambda (entry)
                        (and (or (null level-int)
                                 (>= (log-entry-level entry) level-int))
                             (or (null logger-name)
                                 (search logger-name (log-entry-logger-name entry)))
                             (or (null scanner)
                                 (cl-ppcre:scan scanner (log-entry-message entry)))))
                      entries))
           (to-show (if (eql count :all)
                        filtered
                        (subseq filtered
                                (max 0 (- (length filtered) count))))))

      (if (null to-show)
          (format stream "~&No matching log entries found.~%")
          (progn
            (format stream "~&Recent logs (~D entr~:@p):~%" (length to-show))
            (format stream "~&~80,,,'-<~>~%")
            (dolist (entry to-show)
              (format-log-entry stream entry))
            (format stream "~&~80,,,'-<~>~%")))
      (length to-show))))

(defun collect-recent-entries ()
  "Collect all entries from the circular buffer in chronological order."
  (let ((result nil)
        (start (if (< *recent-logs-count* *recent-logs-size*)
                   0
                   *recent-logs-index*)))
    (loop for i from 0 below *recent-logs-count*
          for pos = (mod (+ start i) *recent-logs-size*)
          for entry = (aref *recent-logs* pos)
          when entry
            do (push entry result))
    (nreverse result)))

(defun format-log-entry (stream entry)
  "Format a log entry for human-readable display."
  (format stream "~&[~A] ~A ~A~@[ - ~A~]~%"
          (format-timestamp (log-entry-timestamp entry))
          (level-keyword (log-entry-level entry))
          (log-entry-logger-name entry)
          (log-entry-message entry))
  (dolist (field (log-entry-fields entry))
    (format stream "  ~A: ~A~%"
            (field-name field)
            (format-field-value field))))

(defun format-field-value (field)
  "Format field value for display."
  (case (field-type field)
    (:error
     (let ((condition (field-value field)))
       (if (typep condition 'cl:error)
           (format nil "~A" condition)
           (prin1-to-string condition))))
    (:timestamp
     (format-timestamp (field-value field)))
    (t (prin1-to-string (field-value field)))))

;;; Grep Logs

(defun grep-logs (pattern &key level logger-name (stream *standard-output*))
  "Search recent log entries for PATTERN (regex string).

   PATTERN - Regular expression to match against log messages
   LEVEL - Only search entries at or above this level (keyword)
   LOGGER-NAME - Only search entries from this logger (string)
   STREAM - Output stream (default: *standard-output*)

   Returns a list of matching log entries.

   Examples:
     (grep-logs \"error\")                    ; Find all logs containing 'error'
     (grep-logs \"user.*login\" :level :info) ; Find login messages at INFO+
     (grep-logs \"query\" :logger-name \"app.db\") ; Find queries from db logger"
  (with-lock-held (*recent-logs-lock*)
    (unless *recent-logs*
      (format stream "~&No recent logs available. Call (enable-recent-logs) first.~%")
      (return-from grep-logs nil))

    (let* ((entries (collect-recent-entries))
           (level-int (when level (parse-level level)))
           (scanner (cl-ppcre:create-scanner pattern :case-insensitive-mode t))
           (matches (remove-if-not
                     (lambda (entry)
                       (and (cl-ppcre:scan scanner (log-entry-message entry))
                            (or (null level-int)
                                (>= (log-entry-level entry) level-int))
                            (or (null logger-name)
                                (search logger-name (log-entry-logger-name entry)))))
                     entries)))

      (if (null matches)
          (format stream "~&No matching log entries found.~%")
          (progn
            (format stream "~&Found ~D matching entr~:@p:~%" (length matches))
            (format stream "~&~80,,,'-<~>~%")
            (dolist (entry matches)
              (format-log-entry stream entry))
            (format stream "~&~80,,,'-<~>~%")))
      matches)))

;;; Capture Logs for Testing

(defstruct captured-output
  "Output that captures log entries to a list."
  (entries nil :type list)
  (lock (make-lock "llog/captured") :type t))

(defmethod write-entry ((output captured-output) (entry log-entry))
  "Capture log entry to the captured-output list."
  (with-lock-held ((captured-output-lock output))
    (push (copy-log-entry entry) (captured-output-entries output)))
  (values))

(defmethod close-output ((output captured-output))
  "No-op for captured output."
  (values))

(defmacro with-captured-logs ((&optional (logger-var '*logger*)
                                &key level)
                               &body body)
  "Execute BODY with logs captured and returned as a list.

   LOGGER-VAR - Logger to capture from (default: *logger*)
   LEVEL - Minimum log level to capture (default: current level)

   Returns two values:
     1. The result of evaluating BODY
     2. List of captured log entries (chronological order)

   Examples:
     (with-captured-logs ()
       (info \"Test message\"))
     => NIL, (#<LOG-ENTRY ...>)

     (multiple-value-bind (result logs)
         (with-captured-logs (*logger* :level :warn)
           (warn \"Warning\")
           (info \"Info\")  ; Not captured (below :warn)
           42)
       (values result (length logs)))
     => 42, 1"
  (let ((output-var (gensym "OUTPUT"))
        (original-outputs-var (gensym "ORIGINAL-OUTPUTS"))
        (original-level-var (gensym "ORIGINAL-LEVEL")))
    `(let* ((,output-var (make-captured-output))
            (,original-outputs-var (logger-outputs ,logger-var))
            (,original-level-var (logger-level ,logger-var)))
       (unwind-protect
            (progn
              ;; Replace outputs with captured output
              (with-lock-held ((logger-lock ,logger-var))
                (setf (slot-value ,logger-var 'outputs) (list ,output-var))
                ,@(when level
                    `((setf (slot-value ,logger-var 'level)
                            (or (parse-level ,level) ,original-level-var)))))
              ;; Execute body and return result with captured entries
              (values (progn ,@body) ; lint:suppress redundant-progn - needed for macro body splicing
                      (nreverse (captured-output-entries ,output-var))))
         ;; Restore original outputs and level
         (with-lock-held ((logger-lock ,logger-var))
           (setf (slot-value ,logger-var 'outputs) ,original-outputs-var
                 (slot-value ,logger-var 'level) ,original-level-var))))))

;;; Custom Field Type Definition

(defmacro define-field-type (name (value-var value-type) &body options)
  "Define a custom field type constructor.

   NAME - Name of the field type (symbol)
   VALUE-VAR - Variable name for the value parameter
   VALUE-TYPE - Type declaration for the value (for optimization)
   OPTIONS - Property list with optional keys:
     :documentation - Docstring for the constructor
     :coercion - Expression to coerce the value (defaults to value-var)
     :inline - Whether to inline the constructor (default: T)

   Examples:
     ;; Simple custom type
     (define-field-type uuid (value cl:string)
       :documentation \"Create a UUID field.\")

     ;; Type with coercion
     (define-field-type percentage (value real)
       :documentation \"Create a percentage field (0-100).\"
       :coercion (max 0.0 (min 100.0 (cl:float value 1.0))))

     ;; Type with validation
     (define-field-type email (value cl:string)
       :documentation \"Create an email field.\"
       :coercion (progn
                   (unless (find #\\@ value)
                     (cl:error \"Invalid email: ~A\" value))
                   value))

   Usage:
     (info-typed \"User registered\"
       (uuid \"user-id\" user-uuid)
       (email \"email\" user-email)
       (percentage \"completion\" 75.5))"
  (let* ((options-plist (loop for (key val) on options by #'cddr
                             collect key collect val))
         (documentation (getf options-plist :documentation
                              (format nil "Create a ~A field." name)))
         (coercion (getf options-plist :coercion value-var))
         (inline (getf options-plist :inline t))
         (type-keyword (intern (cl:string name) :keyword))
         (llog-symbol (intern (cl:string name) :llog)))

    `(progn
       ,@(when inline
           `((declaim (inline ,llog-symbol))))
       (defun ,llog-symbol (field-name ,value-var) ; lint:suppress lambda-list-invalid
         ,documentation
         (declare (type cl:string field-name)
                  (type ,value-type ,value-var)
                  (optimize speed (safety 1)))
         (make-field field-name ,coercion ,type-keyword))

       ;; Export the symbol from llog package
       (export ',llog-symbol (find-package :llog))

       ',llog-symbol)))

;;; Helper: Copy log entry for storage

(defun copy-log-entry (entry)
  "Create a copy of a log entry for storage."
  (%make-log-entry
   :level (log-entry-level entry)
   :timestamp (log-entry-timestamp entry)
   :message (log-entry-message entry)
   :logger-name (log-entry-logger-name entry)
   :fields (copy-list (log-entry-fields entry))))
