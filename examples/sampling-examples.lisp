;;;; sampling-examples.lisp - Real-world examples of sampling and rate limiting
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; This file demonstrates practical usage patterns for sampling and rate
;;;; limiting in production scenarios.

(defpackage #:llog/examples/sampling
  (:use #:cl)
  (:documentation "Examples for LLOG sampling and rate limiting"))

(in-package #:llog/examples/sampling)

;;; Example 1: High-Traffic Web Service
;;;
;;; A web service handles 10,000 requests/second. We want to:
;;; - Log all errors
;;; - Sample 1% of info logs (100 logs/sec instead of 10K)
;;; - Rate limit warnings to prevent storms

(defun setup-web-service-logger ()
  "Configure logger for high-traffic web service."
  (let ((logger (llog:make-logger :name "web-service")))
    ;; Sample only 1% of INFO logs
    (llog:set-sampling logger :info 0.01)

    ;; Rate limit WARN to 50 per second (prevents warning storms)
    (llog:set-rate-limit logger :warn 50 :per-second)

    ;; ERROR and above: no sampling, no rate limiting (always log)
    logger))

(defun handle-web-request (request logger)
  "Example request handler with appropriate logging."
  ;; INFO: Will be sampled at 1%
  (llog:info logger "Request received"
             :method (request-method request)
             :path (request-path request)
             :ip (request-ip request))

  (handler-case
      (process-request request)

    (validation-error (e)
      ;; WARN: Rate limited to 50/sec
      (llog:warn logger "Validation failed"
                 :error (llog:error-field "error" e)
                 :request-id (request-id request)))

    (error (e)
      ;; ERROR: Always logged (no sampling/rate limiting)
      (llog:error logger "Request processing failed"
                  :error (llog:error-field-detailed "error" e :backtrace t)
                  :request-id (request-id request)))))

;;; Example 2: Batch Processing System
;;;
;;; Processing millions of records. We want to:
;;; - Log progress every 1000 records (deterministic sampling)
;;; - Rate limit errors to avoid log flooding on bad data

(defun setup-batch-logger ()
  "Configure logger for batch processing."
  (let ((logger (llog:make-logger :name "batch-processor")))
    ;; Log every 1000th DEBUG message (progress tracking)
    (llog:set-sampling logger :debug :every 1000)

    ;; Rate limit errors to 10 per minute
    ;; (prevents floods from consistently bad data)
    (llog:set-rate-limit logger :error 10 :per-minute)

    logger))

(defun process-batch (records logger)
  "Process batch of records with appropriate logging."
  (loop for record in records
        for index from 0
        do
           ;; DEBUG: Logged every 1000th iteration
           (llog:debug logger "Processing record"
                       :index index
                       :record-id (record-id record))

           (handler-case
               (process-record record)
             (error (e)
               ;; ERROR: Rate limited to 10/minute
               (llog:error logger "Record processing failed"
                           :record-id (record-id record)
                           :error (llog:error-field "error" e))))))

;;; Example 3: Development vs Production Configuration
;;;
;;; Different sampling strategies for different environments.

(defun setup-logger-for-environment (env)
  "Configure logger based on environment."
  (let ((logger (llog:make-logger)))
    (ecase env
      (:development
       ;; Development: Log everything, no sampling
       ;; (Helpful for debugging)
       logger)

      (:staging
       ;; Staging: Light sampling, some rate limiting
       (llog:set-sampling logger :debug 0.5)      ; 50% of DEBUG
       (llog:set-sampling logger :info 0.1)       ; 10% of INFO
       (llog:set-rate-limit logger :warn 100 :per-second)
       logger)

      (:production
       ;; Production: Heavy sampling, aggressive rate limiting
       (llog:set-sampling logger :debug 0.01)     ; 1% of DEBUG
       (llog:set-sampling logger :info 0.05)      ; 5% of INFO
       (llog:set-rate-limit logger :warn 50 :per-second)
       (llog:set-rate-limit logger :error 20 :per-second)
       logger))))

;;; Example 4: Per-Component Sampling
;;;
;;; Different components have different logging needs.

(defun setup-multi-component-logging ()
  "Set up loggers for different system components."
  (list
   ;; Database logger: Log every 100th query
   (let ((db-logger (llog:make-logger :name "database")))
     (llog:set-sampling db-logger :debug :every 100)
     (cons :database db-logger))

   ;; Cache logger: Sample 10% (high frequency)
   (let ((cache-logger (llog:make-logger :name "cache")))
     (llog:set-sampling cache-logger :debug 0.1)
     (cons :cache cache-logger))

   ;; Auth logger: No sampling (security-critical)
   (let ((auth-logger (llog:make-logger :name "auth")))
     (cons :auth auth-logger))))

;;; Example 5: Dynamic Sampling Based on Load
;;;
;;; Adjust sampling rate based on system load.

(defun adjust-sampling-for-load (logger load-level)
  "Dynamically adjust sampling based on system load."
  (cond
    ((< load-level 0.5)
     ;; Low load: Detailed logging
     (llog:set-sampling logger :debug 1.0)   ; 100%
     (llog:set-sampling logger :info 1.0))   ; 100%

    ((< load-level 0.8)
     ;; Medium load: Some sampling
     (llog:set-sampling logger :debug 0.1)   ; 10%
     (llog:set-sampling logger :info 0.5))   ; 50%

    (t
     ;; High load: Aggressive sampling
     (llog:set-sampling logger :debug 0.01)  ; 1%
     (llog:set-sampling logger :info 0.1)))  ; 10%

  (format t "Adjusted sampling for load level: ~,2F~%" load-level))

;;; Example 6: Monitoring Sampling Effectiveness
;;;
;;; Track and report on sampling statistics.

(defun report-sampling-stats (logger)
  "Generate a report of sampling effectiveness."
  (format t "~%Sampling Statistics Report~%")
  (format t "==========================~%")

  (dolist (level '(:debug :info :warn :error))
    (let ((stats (llog:get-sampling-stats logger level)))
      (when (plusp (getf stats :total))
        (format t "~%~A:~%" level)
        (format t "  Total attempts:  ~:D~%" (getf stats :total))
        (format t "  Logged:          ~:D~%" (getf stats :sampled))
        (format t "  Dropped:         ~:D~%" (getf stats :dropped))
        (format t "  Effective rate:  ~,2F%~%"
                (* 100 (getf stats :rate)))))))

(defun report-rate-limiting-stats (logger)
  "Generate a report of rate limiting effectiveness."
  (format t "~%Rate Limiting Statistics Report~%")
  (format t "================================~%")

  (dolist (level '(:debug :info :warn :error))
    (let ((stats (llog:get-rate-limit-stats logger level)))
      (when (plusp (getf stats :total))
        (format t "~%~A:~%" level)
        (format t "  Total attempts:   ~:D~%" (getf stats :total))
        (format t "  Allowed:          ~:D~%" (getf stats :allowed))
        (format t "  Dropped:          ~:D~%" (getf stats :dropped))
        (format t "  Effective rate:   ~,2F%~%"
                (* 100 (getf stats :rate)))
        (format t "  Current tokens:   ~D / ~D~%"
                (getf stats :current-tokens)
                (getf stats :capacity))
        (format t "  Refill rate:      ~,1F tokens/sec~%"
                (getf stats :refill-rate))))))

;;; Example 7: Cost-Based Sampling
;;;
;;; Sample expensive operations differently than cheap ones.

(defun log-with-cost-based-sampling (logger operation cost)
  "Log operation with sampling rate based on operation cost."
  (let ((sample-rate (cond
                       ((< cost 0.01) 0.01)    ; Cheap ops: 1% sample
                       ((< cost 0.1) 0.1)      ; Medium ops: 10% sample
                       (t 1.0))))              ; Expensive ops: 100%

    (when (< (random 1.0) sample-rate)
      (llog:info logger "Operation completed"
                 :operation operation
                 :cost-seconds cost))))

;;; Example 8: Combining Sampling and Rate Limiting
;;;
;;; Use both techniques together for maximum control.

(defun setup-combined-controls (logger)
  "Set up both sampling and rate limiting."
  ;; Sample DEBUG aggressively (keep 1%)
  (llog:set-sampling logger :debug 0.01)

  ;; Also rate limit DEBUG to 100/sec as absolute ceiling
  (llog:set-rate-limit logger :debug 100 :per-second)

  ;; Result: Maximum 100 DEBUG logs/sec, each representing ~100 events
  ;; (10,000 events -> 100 sampled -> max 100/sec logged)

  logger)

;;; Example 9: Temporary Sampling Disable
;;;
;;; Temporarily disable sampling for debugging.

(defun with-full-logging (logger function)
  "Execute FUNCTION with sampling temporarily disabled."
  ;; Save current sampling configs
  (let ((debug-stats (llog:get-sampling-stats logger :debug))
        (info-stats (llog:get-sampling-stats logger :info)))

    ;; Clear sampling
    (llog:clear-sampling logger :debug)
    (llog:clear-sampling logger :info)

    (unwind-protect
         (funcall function)

      ;; Restore sampling (Note: This is simplified; production code
      ;; would need to restore actual sampling config, not just stats)
      (when (< (getf debug-stats :rate) 1.0)
        (llog:info logger "Restoring debug sampling"))
      (when (< (getf info-stats :rate) 1.0)
        (llog:info logger "Restoring info sampling")))))

;;; Example 10: Sampling for Cost Reduction
;;;
;;; Calculate cost savings from sampling.

(defun calculate-log-cost-savings (logger events-per-day cost-per-million-logs)
  "Calculate cost savings from sampling."
  (format t "~%Log Cost Analysis~%")
  (format t "==================~%")

  (let ((total-cost 0)
        (sampled-cost 0))

    (dolist (level '(:debug :info :warn :error))
      (let* ((stats (llog:get-sampling-stats logger level))
             (rate (getf stats :rate))
             (level-events (* events-per-day rate))
             (level-cost (* (/ level-events 1000000.0) cost-per-million-logs)))

        (incf total-cost (* (/ events-per-day 1000000.0) cost-per-million-logs))
        (incf sampled-cost level-cost)

        (format t "~%~A: ~,2F%% logged (~,0F -> ~,0F events/day)~%"
                level
                (* 100 rate)
                events-per-day
                level-events)))

    (format t "~%Cost without sampling: $~,2F/day~%" total-cost)
    (format t "Cost with sampling:    $~,2F/day~%" sampled-cost)
    (format t "Savings:               $~,2F/day (~,1F%)~%"
            (- total-cost sampled-cost)
            (* 100 (/ (- total-cost sampled-cost) total-cost)))))

;;; Utility Functions (stubs for examples)

(defun request-method (request)
  "Extract HTTP method from request."
  (getf request :method))

(defun request-path (request)
  "Extract URL path from request."
  (getf request :path))

(defun request-ip (request)
  "Extract client IP from request."
  (getf request :ip))

(defun request-id (request)
  "Extract request ID from request."
  (getf request :id))

(defun process-request (request)
  "Process HTTP request (stub)."
  (declare (ignore request))
  nil)

(defun record-id (record)
  "Extract ID from record."
  (getf record :id))

(defun process-record (record)
  "Process record (stub)."
  (declare (ignore record))
  nil)

(define-condition validation-error (error) ())
