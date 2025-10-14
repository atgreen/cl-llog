;;;; test-rate-limiting.lisp - Tests for rate limiting functionality
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Basic Rate Limiting Tests

(def-test rate-limiting-drops-excess ()
  "Test that rate limiting drops logs when limit is exceeded."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Allow 10 logs per second
    (llog:set-rate-limit logger :info 10 :per-second)

    ;; Try to log 100 times immediately (should only get ~10)
    (dotimes (i 100)
      (info "Test message"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (<= line-count 12) "Expected at most 12 lines (10 + burst tolerance), got ~A" line-count))))

(def-test rate-limiting-allows-within-limit ()
  "Test that logs within rate limit are all allowed."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Allow 1000 logs per second (way more than we'll use)
    (llog:set-rate-limit logger :info 1000 :per-second)

    (dotimes (i 50)
      (info "Test message"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (= 50 line-count)))))

;;; Per-Level Rate Limiting Tests

(def-test rate-limiting-per-level ()
  "Test that rate limiting works independently per level."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Different limits for different levels
    (llog:set-rate-limit logger :debug 5 :per-second)
    (llog:set-rate-limit logger :info 10 :per-second)

    ;; Try to exceed both
    (dotimes (i 50)
      (debug "Debug message")
      (info "Info message"))

    (let* ((output (get-output-stream-string stream))
           (debug-count (count-substring "DEBUG" output))
           (info-count (count-substring "INFO" output)))
      ;; DEBUG should have ~5, INFO should have ~10 (allow some variance)
      (is (<= debug-count 7))
      (is (<= info-count 12)))))

;;; Time Unit Tests

(def-test rate-limiting-per-minute ()
  "Test rate limiting with per-minute configuration."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Allow 60 per minute = 1 per second
    (llog:set-rate-limit logger :info 60 :per-minute)

    ;; Try to log 10 immediately
    (dotimes (i 10)
      (info "Test message"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      ;; Should get ~1-2 logs (1/sec rate with some burst tolerance)
      (is (<= line-count 3) "Expected at most 3 lines, got ~A" line-count))))

(def-test rate-limiting-per-hour ()
  "Test rate limiting with per-hour configuration."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Allow 3600 per hour = 1 per second
    (llog:set-rate-limit logger :info 3600 :per-hour)

    (dotimes (i 10)
      (info "Test message"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (<= line-count 3) "Expected at most 3 lines, got ~A" line-count))))

;;; Clear Rate Limit Tests

(def-test clear-rate-limit-restores-full-logging ()
  "Test that clearing rate limit restores unlimited logging."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Set very restrictive limit
    (llog:set-rate-limit logger :info 1 :per-second)

    (dotimes (i 10)
      (info "Limited"))

    (let ((limited-count (count #\Newline (get-output-stream-string stream))))
      (is (<= limited-count 2) "Should be rate limited"))

    ;; Clear rate limit
    (llog:clear-rate-limit logger :info)

    (dotimes (i 10)
      (info "Unlimited"))

    (let ((total-count (count #\Newline (get-output-stream-string stream))))
      ;; Should have limited logs + 10 unlimited logs
      (is (>= total-count 10) "Should have at least 10 new unlimited logs"))))

;;; Statistics Tests

(def-test rate-limiting-statistics ()
  "Test that rate limiting statistics are tracked correctly."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Set low limit
    (llog:set-rate-limit logger :info 5 :per-second)

    ;; Try to log 50 times
    (dotimes (i 50)
      (info "Test"))

    ;; Check statistics
    (let ((stats (llog:get-rate-limit-stats logger :info)))
      (is (= 50 (getf stats :total)))
      (is (plusp (getf stats :allowed)))
      (is (plusp (getf stats :dropped)))
      (is (= 50 (+ (getf stats :allowed) (getf stats :dropped)))))))

;;; Rate Limited Predicate Tests

(def-test rate-limited-p-returns-true-when-limited ()
  "Test that rate-limited-p correctly reports rate limiting state."
  (let* ((logger (llog:make-logger))
         (llog:*logger* logger))
    ;; Set very low limit
    (llog:set-rate-limit logger :info 1 :per-second)

    ;; Exhaust the bucket
    (dotimes (i 10)
      (info "Test"))

    ;; Should now be rate limited
    (is-true (llog:rate-limited-p logger :info))))

(def-test rate-limited-p-returns-false-when-not-configured ()
  "Test that rate-limited-p returns false when no limit configured."
  (let ((logger (llog:make-logger)))
    (is-false (llog:rate-limited-p logger :info))))

;;; Token Refill Tests

(def-test rate-limiting-refills-over-time ()
  "Test that token bucket refills over time."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Allow 10 per second
    (llog:set-rate-limit logger :info 10 :per-second)

    ;; Exhaust the bucket
    (dotimes (i 15)
      (info "Burst"))

    ;; Wait for refill (0.5 seconds = ~5 tokens)
    (sleep 0.5)

    ;; Should be able to log a few more after refill
    (dotimes (i 10)
      (info "After refill"))

    ;; Check total output - should have initial burst (<=12) + refilled logs (>0)
    (let* ((output (get-output-stream-string stream))
           (total-count (count #\Newline output)))
      ;; With 10/sec limit: initial burst gets ~10, then after 0.5s refill we get ~5 more
      ;; So expect >10 but not all 25 (15+10)
      (is (> total-count 10) "Should have logged more than just initial burst")
      (is (< total-count 20) "But shouldn't have logged everything"))))

;;; Error Handling Tests

(def-test rate-limiting-invalid-rate ()
  "Test that invalid rates are rejected."
  (let ((logger (llog:make-logger)))
    (signals cl:error
      (llog:set-rate-limit logger :info -1 :per-second))
    (signals cl:error
      (llog:set-rate-limit logger :info 0 :per-second))))

(def-test rate-limiting-invalid-time-unit ()
  "Test that invalid time units are rejected."
  (let ((logger (llog:make-logger)))
    (signals cl:error
      (llog:set-rate-limit logger :info 10 :per-day))))

(def-test rate-limiting-invalid-level ()
  "Test that invalid levels are rejected."
  (let ((logger (llog:make-logger)))
    (signals cl:error
      (llog:set-rate-limit logger :invalid-level 10 :per-second))))

;;; Helper function (shared with test-sampling.lisp)

(defun count-substring (substring string)
  "Count occurrences of SUBSTRING in STRING."
  (loop with start = 0
        for pos = (search substring string :start2 start)
        while pos
        count pos
        do (setf start (1+ pos))))
