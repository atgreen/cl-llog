;;;; hooks.lisp - Example hooks demonstrating the hook system
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Example 1: Metrics Counter Hook
;;; Counts log entries by level for monitoring

(defun make-metrics-counter ()
  "Create a metrics counter that tracks log volume by level.
   Returns a hook function and a function to retrieve statistics."
  (let ((counts (make-hash-table :test 'eql)))
    ;; Initialize counters
    (loop for level from 0 to 6
          do (setf (gethash level counts) 0))

    (values
     ;; Hook function (post-log)
     (lambda (logger entry)
       (declare (ignore logger))
       (let ((level (llog:log-entry-level entry)))
         (incf (gethash level counts 0))))

     ;; Statistics retrieval function
     (lambda ()
       (list :trace (gethash +trace+ counts 0)
             :debug (gethash +debug+ counts 0)
             :info (gethash +info+ counts 0)
             :warn (gethash +warn+ counts 0)
             :error (gethash +error+ counts 0)
             :fatal (gethash +fatal+ counts 0)
             :panic (gethash +panic+ counts 0))))))

;; Usage:
;; (multiple-value-bind (hook get-stats) (make-metrics-counter)
;;   (add-hook *logger* :post-log hook :name 'metrics-counter)
;;   ;; ... log some messages ...
;;   (funcall get-stats))


;;; Example 2: Level Filter Hook
;;; Filters out log entries below a certain level (pre-log hook)

(defun make-level-filter (min-level)
  "Create a pre-log hook that filters entries below MIN-LEVEL.
   Returns NIL to filter out the entry, or the entry to keep it."
  (let ((min-level-int (if (integerp min-level)
                           min-level
                           (parse-level min-level))))
    (lambda (logger entry)
      (declare (ignore logger))
      (if (>= (llog:log-entry-level entry) min-level-int)
          entry
          nil))))

;; Usage:
;; (add-hook *logger* :pre-log (make-level-filter :warn)
;;           :name 'warn-and-above :priority 10)


;;; Example 3: Field Redaction Hook
;;; Redacts sensitive fields like passwords, API keys, etc.

(defun make-field-redactor (sensitive-keys)
  "Create a pre-log hook that redacts sensitive field values.
   SENSITIVE-KEYS is a list of field keys to redact (as strings)."
  (lambda (logger entry)
    (declare (ignore logger))
    (let ((fields (llog:log-entry-fields entry)))
      (setf (llog:log-entry-fields entry)
            (loop for field in fields
                  if (member (field-key field) sensitive-keys :test #'string=)
                    collect (make-field (field-key field) :string "***REDACTED***")
                  else
                    collect field))
      entry)))

;; Usage:
;; (add-hook *logger* :pre-log
;;           (make-field-redactor '("password" "api-key" "secret"))
;;           :name 'redactor :priority 5)


;;; Example 4: Timestamp Filter Hook
;;; Adds a custom timestamp field to every log entry

(defun make-timestamp-enricher (&optional (field-name "enriched-ts"))
  "Create a pre-log hook that adds a custom timestamp field."
  (lambda (logger entry)
    (declare (ignore logger))
    (let ((ts (get-universal-time)))
      (push (timestamp field-name ts) (llog:log-entry-fields entry))
      entry)))

;; Usage:
;; (add-hook *logger* :pre-log (make-timestamp-enricher "processed-at")
;;           :name 'timestamp-enricher)


;;; Example 5: Error Notification Hook
;;; Sends notifications for ERROR/FATAL/PANIC levels

(defun make-error-notifier (callback)
  "Create a post-log hook that calls CALLBACK for ERROR/FATAL/PANIC entries.
   CALLBACK is a function (lambda (level message) ...) that sends notifications."
  (lambda (logger entry)
    (declare (ignore logger))
    (let ((level (llog:log-entry-level entry)))
      (when (>= level +error+)
        (funcall callback
                 (level-keyword level)
                 (llog:log-entry-message entry))))))

;; Usage:
;; (add-hook *logger* :post-log
;;           (make-error-notifier
;;             (lambda (level msg)
;;               (format t "~&ALERT: [~A] ~A~%" level msg)))
;;           :name 'error-alerter)


;;; Example 6: Sampling Hook
;;; Only logs every Nth entry (useful for high-volume debug logs)

(defun make-sampling-hook (sample-rate)
  "Create a pre-log hook that only keeps 1 out of every SAMPLE-RATE entries.
   SAMPLE-RATE=10 means keep 1 out of 10 entries."
  (let ((counter 0))
    (lambda (logger entry)
      (declare (ignore logger))
      (if (zerop (mod (incf counter) sample-rate))
          entry
          nil))))

;; Usage:
;; (add-hook *logger* :pre-log (make-sampling-hook 10)
;;           :name 'sampler :priority 20)


;;; Example 7: Performance Profiler Hook
;;; Tracks timing between log calls

(defun make-performance-profiler ()
  "Create hooks that measure time between log calls.
   Returns a pre-log hook and a function to retrieve statistics."
  (let ((last-time (get-internal-real-time))
        (intervals nil))
    (values
     ;; Pre-log hook
     (lambda (logger entry)
       (declare (ignore logger))
       (let* ((now (get-internal-real-time))
              (delta (/ (- now last-time) internal-time-units-per-second)))
         (push delta intervals)
         (setf last-time now)
         entry))

     ;; Statistics function
     (lambda ()
       (if intervals
           (let* ((sorted (sort (copy-list intervals) #'<))
                  (len (length sorted))
                  (avg (/ (reduce #'+ sorted) len))
                  (median (nth (floor len 2) sorted)))
             (list :count len
                   :avg-interval avg
                   :median-interval median
                   :min (first sorted)
                   :max (first (last sorted))))
           (list :count 0))))))

;; Usage:
;; (multiple-value-bind (hook get-stats) (make-performance-profiler)
;;   (add-hook *logger* :pre-log hook :name 'profiler)
;;   ;; ... log some messages ...
;;   (funcall get-stats))


;;; Example 8: Context Enrichment Hook
;;; Adds common context fields automatically (hostname, thread, etc.)

(defun make-context-enricher ()
  "Create a pre-log hook that adds system context to every entry."
  (let ((hostname (machine-instance))
        (lisp-impl (lisp-implementation-type)))
    (lambda (logger entry)
      (declare (ignore logger))
      (setf (llog:log-entry-fields entry)
            (append (list (string "hostname" hostname)
                          (string "lisp" lisp-impl)
                          (string "thread"
                                  (format nil "~A" (current-thread))))
                    (llog:log-entry-fields entry)))
      entry)))

;; Usage:
;; (add-hook *logger* :pre-log (make-context-enricher)
;;           :name 'context-enricher :priority 1)


;;; Example 9: Circular Buffer Hook
;;; Keeps the last N log entries in memory for debugging

(defun make-circular-buffer (size)
  "Create a post-log hook that maintains a circular buffer of recent entries.
   Returns the hook function and a function to retrieve the buffer contents."
  (let ((buffer (make-array size :initial-element nil))
        (index 0)
        (count 0))
    (values
     ;; Post-log hook
     (lambda (logger entry)
       (declare (ignore logger))
       (setf (aref buffer index) (copy-log-entry entry))
       (setf index (mod (1+ index) size))
       (when (< count size)
         (incf count)))

     ;; Retrieval function
     (lambda ()
       (let ((result nil)
             (start (if (< count size) 0 index)))
         (loop for i from 0 below count
               for pos = (mod (+ start i) size)
               do (push (aref buffer pos) result))
         (nreverse result))))))

;; Usage:
;; (multiple-value-bind (hook get-recent) (make-circular-buffer 100)
;;   (add-hook *logger* :post-log hook :name 'circular-buffer)
;;   ;; ... log some messages ...
;;   (funcall get-recent))


;;; Example 10: Structured Error Reporter
;;; Formats and reports errors to an external service (e.g., Sentry)

(defun make-sentry-reporter (dsn)
  "Create an error hook that reports to Sentry.
   DSN is the Sentry Data Source Name.
   This is a simplified example - real implementation would use HTTP client."
  (lambda (logger error entry)
    (declare (ignore logger))
    ;; In a real implementation, this would:
    ;; 1. Extract error details
    ;; 2. Format as Sentry event JSON
    ;; 3. Send HTTP POST to Sentry
    (format *error-output*
            "~&[SENTRY ~A] Level: ~A, Error: ~A, Message: ~A~%"
            dsn
            (level-keyword (llog:log-entry-level entry))
            error
            (llog:log-entry-message entry))))

;; Usage:
;; (add-hook *logger* :error
;;           (make-sentry-reporter "https://xxx@sentry.io/123")
;;           :name 'sentry)
