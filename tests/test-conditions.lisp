;;;; tests/test-conditions.lisp - Tests for condition system integration
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (c) 2025 Anthony Green <green@moxielogic.com>

(in-package #:llog/tests)

(def-suite* :condition-integration
  :description "Test suite for condition system integration"
  :in :llog)

;;; Backtrace Capture Tests

(def-test backtrace-capture-basic ()
  "Test that backtrace can be captured."
  (let ((backtrace (llog::capture-backtrace 10)))
    (is (listp backtrace))
    (is (> (length backtrace) 0))
    (is (every #'stringp backtrace))))

(def-test backtrace-capture-limit ()
  "Test that backtrace respects frame limit."
  (let ((short (llog::capture-backtrace 5))
        (long (llog::capture-backtrace 20)))
    (is (listp short))
    (is (listp long))
    (is (<= (length short) 5))
    ;; Long may have fewer than 20 frames if stack is shallow
    (is (>= (length long) (length short)))))

;;; Restart Information Tests

(def-test capture-restarts-basic ()
  "Test that restart information can be captured."
  (block test
    (restart-case
        (let ((restarts (llog::capture-restarts)))
          (is (listp restarts))
          (is (> (length restarts) 0))
          ;; Should include at least the abort restart
          (is (member 'abort (mapcar (lambda (r) (getf r :name)) restarts))))
      (test-restart ()
        :report "Test restart for testing"
        (return-from test t)))))

(def-test capture-restarts-names ()
  "Test that custom restart names are captured."
  (restart-case
      (let ((restarts (llog::capture-restarts)))
        (let ((names (mapcar (lambda (r) (getf r :name)) restarts)))
          (is (member 'custom-restart names))
          (is (member 'another-restart names))))
    (custom-restart ()
      :report "Custom restart"
      nil)
    (another-restart ()
      :report "Another custom restart"
      nil)))

;;; Condition Analysis Tests

(def-test analyze-condition-basic ()
  "Test basic condition analysis."
  (handler-case
      (cl:error "Test error")
    (cl:error (condition)
      (let ((info (llog:analyze-condition condition)))
        (is (llog:condition-info-p info))
        (is (eq (llog:condition-info-type info) 'simple-error))
        (is (string= (llog:condition-info-message info) "Test error"))
        ;; Default options should capture backtrace
        (is (not (null (llog:condition-info-backtrace info))))
        (is (listp (llog:condition-info-backtrace info)))
        ;; Default options should capture restarts
        (is (not (null (llog:condition-info-restarts info))))))))

(def-test analyze-condition-no-backtrace ()
  "Test that backtrace can be disabled."
  (handler-case
      (cl:error "Test error")
    (cl:error (condition)
      (let ((info (llog:analyze-condition condition :backtrace nil)))
        (is (llog:condition-info-p info))
        (is (null (llog:condition-info-backtrace info)))))))

(def-test analyze-condition-no-restarts ()
  "Test that restart capture can be disabled."
  (handler-case
      (cl:error "Test error")
    (cl:error (condition)
      (let ((info (llog:analyze-condition condition :restarts nil)))
        (is (llog:condition-info-p info))
        (is (null (llog:condition-info-restarts info)))))))

;;; Condition Cause Tests

(define-condition test-condition-with-cause (cl:error)
  ((cause :initarg :cause :reader test-condition-cause))
  (:report (lambda (condition stream)
             (format stream "Test condition with cause"))))

(def-test condition-cause-extraction ()
  "Test that condition causes can be extracted."
  (let* ((original (make-condition 'simple-error :format-control "Original error"))
         (wrapper (make-condition 'test-condition-with-cause :cause original)))
    (let ((extracted (llog:condition-cause wrapper)))
      (is (eq extracted original)))))

(def-test condition-cause-none ()
  "Test handling of conditions without causes."
  (let ((condition (make-condition 'simple-error :format-control "No cause")))
    (is (null (llog:condition-cause condition)))))

(def-test condition-chain-single ()
  "Test condition chain with single condition."
  (let ((condition (make-condition 'simple-error :format-control "Single")))
    (let ((chain (llog:condition-chain condition)))
      (is (listp chain))
      (is (= (length chain) 1))
      (is (eq (first chain) condition)))))

(def-test condition-chain-nested ()
  "Test condition chain with nested conditions."
  (let* ((c1 (make-condition 'simple-error :format-control "First"))
         (c2 (make-condition 'test-condition-with-cause :cause c1)))
    (let ((chain (llog:condition-chain c2)))
      (is (listp chain))
      (is (= (length chain) 2))
      (is (eq (first chain) c2))
      (is (eq (second chain) c1)))))

;;; Error Field Tests

(def-test error-field-detailed-basic ()
  "Test creating detailed error fields."
  (handler-case
      (cl:error "Test error")
    (cl:error (condition)
      (let ((field (llog:error-field-detailed "error" condition)))
        (is (typep field 'llog::field))
        (is (string= (llog::field-name field) "error"))
        (is (eq (llog::field-type field) :error-detailed))
        (is (llog:condition-info-p (llog::field-value field)))))))

(def-test error-field-detailed-options ()
  "Test that error field respects options."
  (handler-case
      (cl:error "Test error")
    (cl:error (condition)
      (let* ((field (llog:error-field-detailed "error" condition
                                               :backtrace nil
                                               :restarts t
                                               :chain nil))
             (info (llog::field-value field)))
        (is (null (llog:condition-info-backtrace info)))
        (is (not (null (llog:condition-info-restarts info))))
        (is (null (llog:condition-info-cause info)))))))

;;; Encoder Integration Tests

(def-test json-encoder-condition-info ()
  "Test that JSON encoder handles condition-info."
  (handler-case
      (cl:error "Test JSON error")
    (cl:error (condition)
      (let* ((stream (make-string-output-stream))
             (encoder (llog:make-json-encoder))
             (field (llog:error-field-detailed "error" condition :backtrace t :restarts t))
             (entry (llog::make-log-entry llog:+error+
                                         "Test message"
                                         :logger-name ""
                                         :fields (list field))))
        (llog::encode-entry encoder stream entry)
        (let ((output (get-output-stream-string stream)))
          (is (search "\"error\"" output))
          (is (search "\"type\"" output))
          (is (search "\"message\"" output))
          (is (search "\"backtrace\"" output)))))))

(def-test console-encoder-condition-info ()
  "Test that console encoder handles condition-info."
  (handler-case
      (cl:error "Test console error")
    (cl:error (condition)
      (let* ((stream (make-string-output-stream))
             (encoder (llog:make-console-encoder))
             (field (llog:error-field-detailed "error" condition :backtrace t))
             (entry (llog::make-log-entry llog:+error+
                                         "Test message"
                                         :logger-name ""
                                         :fields (list field))))
        (llog::encode-field-console encoder stream field)
        (let ((output (get-output-stream-string stream)))
          (is (search "Test console error" output))
          (is (search "SIMPLE-ERROR" output))
          (is (search "Backtrace:" output)))))))

(def-test sexpr-encoder-condition-info ()
  "Test that S-expression encoder handles condition-info."
  (handler-case
      (cl:error "Test sexpr error")
    (cl:error (condition)
      (let* ((stream (make-string-output-stream))
             (encoder (llog:make-sexpr-encoder))
             (field (llog:error-field-detailed "error" condition :backtrace t))
             (entry (llog::make-log-entry llog:+error+
                                         "Test message"
                                         :logger-name ""
                                         :fields (list field))))
        (llog::encode-entry encoder stream entry)
        (let* ((output (get-output-stream-string stream))
               (sexpr (read-from-string output)))
          (is (listp sexpr))
          (is (getf sexpr :error))
          (let ((error-data (getf sexpr :error)))
            (is (listp error-data))
            (is (getf error-data :type))
            (is (getf error-data :message))
            (is (getf error-data :backtrace))))))))

;;; Full Logging Integration Tests

(def-test log-with-detailed-error ()
  "Test full logging with detailed error information."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream :encoder (llog:make-json-encoder)))
         (llog:*logger* (llog:make-logger :outputs (list output))))
    (handler-case
        (cl:error "Integration test error")
      (cl:error (condition)
        (llog:error-typed "Error occurred"
          (llog:error-field-detailed "error" condition :backtrace t :restarts t))))
    (llog:flush-output output)
    (let ((output-str (get-output-stream-string stream)))
      (is (search "\"error\"" output-str))
      (is (search "\"backtrace\"" output-str))
      (is (search "Integration test error" output-str)))))

(def-test log-typed-with-detailed-error ()
  "Test typed API logging with detailed error information."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream :encoder (llog:make-json-encoder)))
         (llog:*logger* (llog:make-logger :outputs (list output))))
    (handler-case
        (cl:error "Typed test error")
      (cl:error (condition)
        (llog:error-typed "Error occurred"
          (llog:error-field-detailed "error" condition :backtrace t))))
    (llog:flush-output output)
    (let ((output-str (get-output-stream-string stream)))
      (is (search "\"error\"" output-str))
      (is (search "\"backtrace\"" output-str))
      (is (search "Typed test error" output-str)))))
