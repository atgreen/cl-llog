;;;; sampling.lisp - Log sampling for volume control
;;;; SPDX-License-Identifier: MIT

(in-package #:llog)

;;; Sampling Configuration

(defstruct sampling-config
  "Configuration for log sampling at a specific level."
  (mode nil :type (member nil :probabilistic :deterministic))
  (rate 1.0 :type (or (real 0.0 1.0) (integer 1 *)))
  (counter 0 :type (integer 0 *))
  (lock (make-lock "llog/sampling"))
  (total-logs 0 :type (integer 0 *))
  (sampled-logs 0 :type (integer 0 *))
  (dropped-logs 0 :type (integer 0 *)))

;;; Sampling Decision

(declaim (inline should-sample-probabilistic-p))
(defun should-sample-probabilistic-p (config)
  "Return T if a log should be sampled based on probabilistic sampling."
  (declare (type sampling-config config)
           (optimize speed (safety 1)))
  (let ((probability (sampling-config-rate config)))
    (< (random 1.0) probability)))

(defun should-sample-deterministic-p (config)
  "Return T if a log should be sampled based on deterministic (every Nth) sampling."
  (declare (type sampling-config config)
           (optimize speed (safety 1)))
  (with-lock-held ((sampling-config-lock config))
    (let* ((n (sampling-config-rate config))
           (counter (sampling-config-counter config))
           (should-log (zerop (mod counter n))))
      (setf (sampling-config-counter config) (1+ counter))
      should-log)))

(defun should-sample-p (config)
  "Return T if a log entry should be sampled according to CONFIG."
  (declare (type (or null sampling-config) config))
  (cond
    ((null config) t)  ; No sampling configured, always log
    ((eql (sampling-config-mode config) :probabilistic)
     (should-sample-probabilistic-p config))
    ((eql (sampling-config-mode config) :deterministic)
     (should-sample-deterministic-p config))
    (t t)))  ; Unknown mode, default to logging

;;; Statistics Tracking

(defun record-sampling-decision (config sampled-p)
  "Record whether a log was sampled or dropped for statistics."
  (declare (type sampling-config config))
  (with-lock-held ((sampling-config-lock config))
    (incf (sampling-config-total-logs config))
    (if sampled-p
        (incf (sampling-config-sampled-logs config))
        (incf (sampling-config-dropped-logs config)))))

(defun sampling-config-stats (config)
  "Return sampling statistics as a plist."
  (declare (type (or null sampling-config) config))
  (if (null config)
      '(:total 0 :sampled 0 :dropped 0 :rate 1.0)
      (with-lock-held ((sampling-config-lock config))
        (list :total (sampling-config-total-logs config)
              :sampled (sampling-config-sampled-logs config)
              :dropped (sampling-config-dropped-logs config)
              :rate (if (zerop (sampling-config-total-logs config))
                        1.0
                        (/ (sampling-config-sampled-logs config)
                           (sampling-config-total-logs config)))))))

(defun reset-sampling-stats (config)
  "Reset sampling statistics to zero."
  (declare (type sampling-config config))
  (with-lock-held ((sampling-config-lock config))
    (setf (sampling-config-total-logs config) 0
          (sampling-config-sampled-logs config) 0
          (sampling-config-dropped-logs config) 0
          (sampling-config-counter config) 0)))

;;; API for Logger Integration

(defun make-probabilistic-sampler (probability)
  "Create a sampling config for probabilistic sampling.
   PROBABILITY should be between 0.0 and 1.0 (e.g., 0.1 for 10% sample rate)."
  (unless (<= 0.0 probability 1.0)
    (cl:error "Probability must be between 0.0 and 1.0, got: ~A" probability))
  (make-sampling-config :mode :probabilistic
                        :rate (coerce probability 'single-float)))

(defun make-deterministic-sampler (n)
  "Create a sampling config for deterministic sampling (every Nth log).
   N must be a positive integer."
  (unless (and (integerp n) (plusp n))
    (cl:error "N must be a positive integer, got: ~A" n))
  (make-sampling-config :mode :deterministic
                        :rate n
                        :counter 0))

;;; Sampling Hook Factory

(defun make-sampling-hook (logger)
  "Create a pre-log hook that applies sampling configuration from LOGGER.
   Returns a function suitable for use with add-hook."
  (lambda (lg log-entry)
    (declare (ignore lg))
    (let* ((level (log-entry-level log-entry))
           (config (get-sampling-config-for-level logger level)))
      (if (null config)
          log-entry  ; No sampling configured
          (let ((should-log (should-sample-p config)))
            (record-sampling-decision config should-log)
            (if should-log
                log-entry
                nil))))))  ; Return nil to filter out

;;; Helper for accessing logger sampling config
;;; (These functions will be implemented in logger.lisp integration)

(defun get-sampling-config-for-level (logger level)
  "Get the sampling configuration for LOGGER at LEVEL.
   Returns nil if no sampling is configured."
  (declare (type logger logger)
           (type (integer 0 6) level))
  (with-lock-held ((logger-lock logger))
    (gethash level (slot-value logger 'sampling-configs))))

(defun set-sampling-config-for-level (logger level config)
  "Set the sampling configuration for LOGGER at LEVEL."
  (declare (type logger logger)
           (type (integer 0 6) level)
           (type (or null sampling-config) config))
  (with-lock-held ((logger-lock logger))
    (if config
        (setf (gethash level (slot-value logger 'sampling-configs)) config)
        (remhash level (slot-value logger 'sampling-configs)))))
