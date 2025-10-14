;;;; rate-limiting.lisp - Token bucket rate limiting for log volume control
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Token Bucket Rate Limiter

(defstruct rate-limiter
  "Token bucket rate limiter for controlling log throughput."
  (capacity 100 :type (integer 1 *))          ; Maximum tokens
  (tokens 100.0 :type single-float)           ; Current token count
  (refill-rate 100.0 :type single-float)      ; Tokens per second
  (last-refill 0.0 :type single-float)        ; Last refill timestamp
  (lock (make-lock "llog/rate-limiter"))
  (total-attempts 0 :type (integer 0 *))      ; Total log attempts
  (allowed-logs 0 :type (integer 0 *))        ; Logs that passed
  (dropped-logs 0 :type (integer 0 *)))       ; Logs that were dropped

;;; Time Utilities

(declaim (inline get-monotonic-time))
(defun get-monotonic-time ()
  "Get current monotonic time in seconds as a float.
   Uses get-internal-real-time for portability."
  (coerce (/ (get-internal-real-time) internal-time-units-per-second)
          'single-float))

;;; Token Bucket Operations

(defun refill-tokens (limiter now)
  "Refill tokens based on elapsed time since last refill.
   MUST be called with limiter lock held."
  (declare (type rate-limiter limiter)
           (type single-float now)
           (optimize speed (safety 1)))
  (let* ((last-refill (rate-limiter-last-refill limiter))
         (elapsed (- now last-refill))
         (tokens-to-add (* elapsed (rate-limiter-refill-rate limiter)))
         (current-tokens (rate-limiter-tokens limiter))
         (capacity (cl:float (rate-limiter-capacity limiter) 1.0f0))
         (new-tokens (min capacity (+ current-tokens tokens-to-add))))
    (setf (rate-limiter-tokens limiter) new-tokens
          (rate-limiter-last-refill limiter) now)))

(defun try-take-token (limiter)
  "Attempt to take a token from the limiter.
   Returns T if token was available, NIL if rate limited."
  (declare (type rate-limiter limiter)
           (optimize speed (safety 1)))
  (with-lock-held ((rate-limiter-lock limiter))
    (let ((now (get-monotonic-time)))
      ;; Refill tokens based on elapsed time
      (refill-tokens limiter now)

      ;; Try to take a token
      (incf (rate-limiter-total-attempts limiter))
      (let ((tokens (rate-limiter-tokens limiter)))
        (if (>= tokens 1.0)
            (progn
              (setf (rate-limiter-tokens limiter) (- tokens 1.0))
              (incf (rate-limiter-allowed-logs limiter))
              t)
            (progn
              (incf (rate-limiter-dropped-logs limiter))
              nil))))))

;;; Statistics

(defun rate-limiter-stats (limiter)
  "Return rate limiter statistics as a plist."
  (declare (type (or null rate-limiter) limiter))
  (if (null limiter)
      '(:total 0 :allowed 0 :dropped 0 :rate 1.0 :current-tokens 0)
      (with-lock-held ((rate-limiter-lock limiter))
        (list :total (rate-limiter-total-attempts limiter)
              :allowed (rate-limiter-allowed-logs limiter)
              :dropped (rate-limiter-dropped-logs limiter)
              :rate (if (zerop (rate-limiter-total-attempts limiter))
                        1.0
                        (/ (rate-limiter-allowed-logs limiter)
                           (rate-limiter-total-attempts limiter)))
              :current-tokens (floor (rate-limiter-tokens limiter))
              :capacity (rate-limiter-capacity limiter)
              :refill-rate (rate-limiter-refill-rate limiter)))))

(defun reset-rate-limiter-stats (limiter)
  "Reset rate limiter statistics to zero."
  (declare (type rate-limiter limiter))
  (with-lock-held ((rate-limiter-lock limiter))
    (setf (rate-limiter-total-attempts limiter) 0
          (rate-limiter-allowed-logs limiter) 0
          (rate-limiter-dropped-logs limiter) 0)))

(defun reset-rate-limiter-tokens (limiter)
  "Reset token bucket to full capacity."
  (declare (type rate-limiter limiter))
  (with-lock-held ((rate-limiter-lock limiter))
    (setf (rate-limiter-tokens limiter)
          (coerce (rate-limiter-capacity limiter) 'single-float)
          (rate-limiter-last-refill limiter) (get-monotonic-time))))

;;; API for Logger Integration

(defun make-rate-limiter-config (max-per-second)
  "Create a rate limiter that allows MAX-PER-SECOND logs per second."
  (unless (and (numberp max-per-second) (plusp max-per-second))
    (cl:error "max-per-second must be a positive number, got: ~A" max-per-second))
  (let* ((rate (cl:float max-per-second 1.0f0))
         (capacity (ceiling max-per-second)))
    (make-rate-limiter :capacity capacity
                       :tokens (cl:float capacity 1.0f0)
                       :refill-rate rate
                       :last-refill (get-monotonic-time))))

(defun make-rate-limiter-per-minute (max-per-minute)
  "Create a rate limiter that allows MAX-PER-MINUTE logs per minute."
  (make-rate-limiter-config (/ max-per-minute 60.0)))

(defun make-rate-limiter-per-hour (max-per-hour)
  "Create a rate limiter that allows MAX-PER-HOUR logs per hour."
  (make-rate-limiter-config (/ max-per-hour 3600.0)))

;;; Rate Limiting Hook Factory

(defun make-rate-limiting-hook (logger)
  "Create a pre-log hook that applies rate limiting from LOGGER.
   Returns a function suitable for use with add-hook."
  (lambda (lg log-entry)
    (declare (ignore lg))
    (let* ((level (log-entry-level log-entry))
           (limiter (get-rate-limiter-for-level logger level)))
      (if (null limiter)
          log-entry  ; No rate limiting configured
          (if (try-take-token limiter)
              log-entry    ; Token available, allow log
              nil)))))     ; No token, drop log

;;; Helper for accessing logger rate limiter config
;;; (These functions will be implemented in logger.lisp integration)

(defun get-rate-limiter-for-level (logger level)
  "Get the rate limiter for LOGGER at LEVEL.
   Returns nil if no rate limiting is configured."
  (declare (type logger logger)
           (type (integer 0 6) level))
  (with-lock-held ((logger-lock logger))
    (gethash level (slot-value logger 'rate-limiters))))

(defun set-rate-limiter-for-level (logger level limiter)
  "Set the rate limiter for LOGGER at LEVEL."
  (declare (type logger logger)
           (type (integer 0 6) level)
           (type (or null rate-limiter) limiter))
  (with-lock-held ((logger-lock logger))
    (if limiter
        (setf (gethash level (slot-value logger 'rate-limiters)) limiter)
        (remhash level (slot-value logger 'rate-limiters)))))
