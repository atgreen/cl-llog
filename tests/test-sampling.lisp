;;;; test-sampling.lisp - Tests for sampling functionality
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Probabilistic Sampling Tests

(def-test probabilistic-sampling-basic ()
  "Test that probabilistic sampling reduces log volume."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Set 50% sampling rate
    (llog:set-sampling logger :info 0.5)

    ;; Log 1000 times (no fields to avoid extra newlines from console encoder)
    (dotimes (i 1000)
      (info "Test message"))

    ;; Check that approximately 50% were logged (allow 20% variance)
    (let* ((output (get-output-stream-string stream))
           (line-count (count #\Newline output)))
      (is (< 400 line-count 600) "Expected 400-600 lines, got ~A" line-count))))

(def-test probabilistic-sampling-zero ()
  "Test that 0.0 sampling rate drops all logs."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:set-sampling logger :info 0.0)

    (dotimes (i 100)
      (info "Should not appear"))

    (is (string= "" (get-output-stream-string stream)))))

(def-test probabilistic-sampling-one ()
  "Test that 1.0 sampling rate logs everything."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:set-sampling logger :info 1.0)

    (dotimes (i 100)
      (info "Should appear"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (= 100 line-count)))))

;;; Deterministic Sampling Tests

(def-test deterministic-sampling-every-nth ()
  "Test that deterministic sampling logs every Nth entry."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Log every 10th entry
    (llog:set-sampling logger :info :every 10)

    ;; No fields to avoid extra newlines from console encoder
    (dotimes (i 100)
      (info "Test message"))

    ;; Should log exactly 10 times (0, 10, 20, ..., 90)
    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (= 10 line-count)))))

(def-test deterministic-sampling-every-1 ()
  "Test that every 1 logs everything."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    (llog:set-sampling logger :info :every 1)

    (dotimes (i 50)
      (info "Should appear"))

    (let ((line-count (count #\Newline (get-output-stream-string stream))))
      (is (= 50 line-count)))))

;;; Per-Level Sampling Tests

(def-test sampling-per-level ()
  "Test that sampling works independently per level."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :level :debug
                                   :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Sample DEBUG heavily, INFO lightly
    (llog:set-sampling logger :debug :every 100)
    (llog:set-sampling logger :info :every 10)

    ;; Generate 100 logs of each level
    (dotimes (i 100)
      (debug "Debug message")
      (info "Info message"))

    ;; DEBUG should appear 1 time, INFO 10 times
    (let* ((output (get-output-stream-string stream))
           (debug-count (count-substring "DEBUG" output))
           (info-count (count-substring "INFO" output)))
      (is (= 1 debug-count))
      (is (= 10 info-count)))))

;;; Clear Sampling Tests

(def-test clear-sampling-restores-full-logging ()
  "Test that clearing sampling restores 100% logging."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Set restrictive sampling
    (llog:set-sampling logger :info :every 100)

    ;; With :every 100, counter starts at 0, so first log (i=0) will match
    ;; Only 10 iterations means only counter=0 will match (mod 0 100 = 0)
    (dotimes (i 10)
      (info "Sampled"))

    ;; Clear sampling
    (llog:clear-sampling logger :info)

    ;; Now log without sampling
    (dotimes (i 10)
      (info "Full logging"))

    ;; Check total output: 1 from sampling + 10 from full logging = 11
    (let* ((output (get-output-stream-string stream))
           (total-count (count #\Newline output)))
      (is (= 11 total-count) "Should have 1 sampled + 10 full = 11 total"))))

;;; Statistics Tests

(def-test sampling-statistics ()
  "Test that sampling statistics are tracked correctly."
  (let* ((stream (make-string-output-stream))
         (logger (llog:make-logger :outputs (list (llog:make-stream-output stream))))
         (llog:*logger* logger))
    ;; Set 50% sampling
    (llog:set-sampling logger :info 0.5)

    ;; Log 100 times
    (dotimes (i 100)
      (info "Test"))

    ;; Check statistics
    (let ((stats (llog:get-sampling-stats logger :info)))
      (is (= 100 (getf stats :total)))
      (is (plusp (getf stats :sampled)))
      (is (plusp (getf stats :dropped)))
      (is (= 100 (+ (getf stats :sampled) (getf stats :dropped)))))))

;;; Error Handling Tests

(def-test sampling-invalid-probability ()
  "Test that invalid probabilities are rejected."
  (let ((logger (llog:make-logger)))
    (signals cl:error
      (llog:set-sampling logger :info 1.5))
    (signals cl:error
      (llog:set-sampling logger :info -0.1))))

(def-test sampling-invalid-level ()
  "Test that invalid levels are rejected."
  (let ((logger (llog:make-logger)))
    (signals cl:error
      (llog:set-sampling logger :invalid-level 0.5))))

;;; Helper function

(defun count-substring (substring string)
  "Count occurrences of SUBSTRING in STRING."
  (loop with start = 0
        for pos = (search substring string :start2 start)
        while pos
        count pos
        do (setf start (1+ pos))))
