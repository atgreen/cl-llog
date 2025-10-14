;;;; test-pattern.lisp - Tests for pattern layout encoder
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Basic Pattern Rendering

(def-test pattern-encoder-basic ()
  "Test basic pattern encoder functionality."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "[%level] %msg%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test message"))
    (let ((output (get-output-stream-string stream)))
      (is (search "[INFO]" output))
      (is (search "Test message" output)))))

(def-test pattern-encoder-default-pattern ()
  "Test that pattern encoder uses default pattern when none provided."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      ;; Default pattern: "%d [%level] %logger - %msg%n"
      (is (search "[INFO]" output))
      (is (search "Test" output)))))

;;; Pattern Specifiers

(def-test pattern-level-specifier ()
  "Test %level and %p pattern specifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%level %p%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (warn "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "WARN WARN" output)))))

(def-test pattern-logger-specifier ()
  "Test %logger and %c pattern specifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%logger %c%n"))
         (logger (make-logger :name "test.logger"
                             :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "test.logger test.logger" output)))))

(def-test pattern-msg-specifier ()
  "Test %msg and %m pattern specifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%msg|%m%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Hello"))
    (let ((output (get-output-stream-string stream)))
      (is (search "Hello|Hello" output)))))

(def-test pattern-date-specifier ()
  "Test %d and %date pattern specifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%d%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      ;; Should contain ISO8601-ish timestamp
      (is (> (length output) 10))
      (is (search "T" output)))))  ; Contains T separator

(def-test pattern-newline-specifier ()
  "Test %n newline specifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "line1%nline2%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let* ((output (get-output-stream-string stream))
           (lines (count #\Newline output)))
      (is (= 2 lines)))))

(def-test pattern-literal-percent ()
  "Test %% literal percent specifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "100%% complete%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "100% complete" output)))))

(def-test pattern-literal-text ()
  "Test literal text in patterns."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "LOG: %msg END%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "LOG: Test END" output)))))

;;; Field Specifier

(def-test pattern-field-specifier ()
  "Test %field{name} pattern specifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%msg user=%field{user-id}%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info-typed "Login" (int "user-id" 42)))
    (let ((output (get-output-stream-string stream)))
      (is (search "Login user=42" output)))))

(def-test pattern-field-missing ()
  "Test %field{name} with missing field."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%msg user=%field{user-id}%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Login"))  ; No user-id field
    (let ((output (get-output-stream-string stream)))
      (is (search "Login user=" output)))))

(def-test pattern-field-types ()
  "Test %field{} with different field types."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder
                   :pattern "int=%field{int} float=%field{float} str=%field{str}%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info-typed "Test"
            (int "int" 42)
            (llog:float "float" 3.14)
            (llog:string "str" "hello")))
    (let ((output (get-output-stream-string stream)))
      (is (search "int=42" output))
      (is (search "float=3.1" output))  ; Format varies
      (is (search "str=hello" output)))))

;;; Width and Precision Modifiers

(def-test pattern-width-right-pad ()
  "Test right-padding with width modifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "[%10level]%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      ;; INFO is 4 chars, should be padded to 10
      (is (search "[      INFO]" output)))))

(def-test pattern-width-left-pad ()
  "Test left-padding with negative width modifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "[%-10level]%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "[INFO      ]" output)))))

(def-test pattern-precision-truncate ()
  "Test truncation with precision modifier."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%.3msg%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Testing"))
    (let ((output (get-output-stream-string stream)))
      (is (search "Tes" output))
      (is (not (search "Testing" output))))))

(def-test pattern-width-and-precision ()
  "Test combined width and precision modifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%5.3msg%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Hi"))
    (let ((output (get-output-stream-string stream)))
      ;; "Hi" truncated to 3 (still "Hi"), then padded to 5
      (is (search "   Hi" output)))))

;;; Date Format Patterns

(def-test pattern-date-with-format ()
  "Test %d{format} date format specification."
  (let* ((stream (make-string-output-stream))
         ;; For now, format string is ignored, but test that braces are parsed
         (encoder (llog:make-pattern-encoder :pattern "%d{yyyy-MM-dd}%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      ;; Should contain a timestamp (format implementation pending)
      (is (> (length output) 5)))))

;;; Edge Cases

(def-test pattern-empty ()
  "Test empty pattern."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern ""))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (string= "" output)))))

(def-test pattern-only-literals ()
  "Test pattern with only literal text."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "constant text%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "constant text" output)))))

(def-test pattern-unknown-specifier ()
  "Test unknown pattern specifier becomes literal."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%unknown%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      ;; Unknown specifiers should be treated as literals
      (is (> (length output) 0)))))

(def-test pattern-percent-at-end ()
  "Test pattern ending with lone percent."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder :pattern "%msg%"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info "Test"))
    (let ((output (get-output-stream-string stream)))
      (is (search "Test" output)))))

;;; Complex Patterns

(def-test pattern-complex-log4j-style ()
  "Test complex Log4j-style pattern."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder
                   :pattern "%d [%-5level] %logger - %msg%n"))
         (logger (make-logger :name "app.db"
                             :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (warn "Connection failed"))
    (let ((output (get-output-stream-string stream)))
      (is (search "[WARN " output))
      (is (search "app.db" output))
      (is (search "Connection failed" output)))))

(def-test pattern-with-fields ()
  "Test pattern with multiple field references."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder
                   :pattern "%msg [user=%field{user} req=%field{request-id}]%n"))
         (logger (make-logger :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info-typed "Request completed"
            (llog:string "user" "alice")
            (llog:string "request-id" "req-123")))
    (let ((output (get-output-stream-string stream)))
      (is (search "Request completed [user=alice req=req-123]" output)))))

(def-test pattern-all-specifiers ()
  "Test pattern using all available specifiers."
  (let* ((stream (make-string-output-stream))
         (encoder (llog:make-pattern-encoder
                   :pattern "%d %level %logger %msg %field{extra}%n"))
         (logger (make-logger :name "test"
                             :outputs (list (llog:make-stream-output stream :encoder encoder)))))
    (let ((llog:*logger* logger))
      (info-typed "Test" (llog:string "extra" "data")))
    (let ((output (get-output-stream-string stream)))
      (is (search "INFO" output))
      (is (search "test" output))
      (is (search "Test" output))
      (is (search "data" output)))))

;;; Multiple Loggers with Different Patterns

(def-test pattern-multiple-encoders ()
  "Test different loggers with different pattern encoders."
  (let* ((stream1 (make-string-output-stream))
         (stream2 (make-string-output-stream))
         (encoder1 (llog:make-pattern-encoder :pattern "[%level] %msg%n"))
         (encoder2 (llog:make-pattern-encoder :pattern "%level: %msg%n"))
         (logger1 (make-logger :outputs (list (llog:make-stream-output stream1 :encoder encoder1))))
         (logger2 (make-logger :outputs (list (llog:make-stream-output stream2 :encoder encoder2)))))

    (let ((llog:*logger* logger1))
      (info "From logger 1"))

    (let ((llog:*logger* logger2))
      (info "From logger 2"))

    (let ((output1 (get-output-stream-string stream1))
          (output2 (get-output-stream-string stream2)))
      (is (search "[INFO] From logger 1" output1))
      (is (search "INFO: From logger 2" output2)))))
