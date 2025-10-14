;;;; test-repl.lisp - Tests for REPL integration features
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Test Recent Logs Buffer

(def-test enable-recent-logs-initializes-buffer ()
  "Test that enabling recent logs creates the buffer."
  (let ((logger (llog:make-logger)))
    (llog:enable-recent-logs logger 50)
    (is (not (null llog::*recent-logs*))) ; lint:suppress not-null - FiveAM 'is' requires test expression
    (is (= 50 llog::*recent-logs-size*))
    (llog:disable-recent-logs logger)))

(def-test recent-logs-records-entries ()
  "Test that recent logs hook records log entries."
  (let ((logger (llog:make-logger)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "Test message 1")
      (warn "Test message 2")
      (error "Test message 3"))

    (is (= 3 llog::*recent-logs-count*))
    (llog:disable-recent-logs logger)))

(def-test recent-logs-circular-buffer-wraps ()
  "Test that circular buffer wraps around when full."
  (let ((logger (llog:make-logger)))
    (llog:enable-recent-logs logger 5)

    (let ((llog:*logger* logger))
      (dotimes (i 10)
        (info (format nil "Message ~D" i))))

    ;; Should only keep last 5 entries
    (is (= 5 llog::*recent-logs-count*))
    (llog:disable-recent-logs logger)))

;;; Test show-recent

(def-test show-recent-displays-entries ()
  "Test that show-recent displays recent log entries."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "First message")
      (warn "Second message")
      (error "Third message"))

    (let ((count (llog:show-recent :stream output)))
      (is (= 3 count))
      (let ((result (get-output-stream-string output)))
        (is (search "First message" result))
        (is (search "Second message" result))
        (is (search "Third message" result))))

    (llog:disable-recent-logs logger)))

(def-test show-recent-filters-by-level ()
  "Test that show-recent can filter by log level."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "Info message")
      (warn "Warn message")
      (error "Error message"))

    (let ((count (llog:show-recent :level :warn :stream output)))
      (is (= 2 count))  ; warn and error
      (let ((result (get-output-stream-string output)))
        (is (not (search "Info message" result)))
        (is (search "Warn message" result))
        (is (search "Error message" result))))

    (llog:disable-recent-logs logger)))

(def-test show-recent-filters-by-logger-name ()
  "Test that show-recent can filter by logger name."
  (let ((output (make-string-output-stream)))
    (llog::clear-logger-registry)
    (let ((logger-a (llog:get-logger "app.a"))
          (logger-b (llog:get-logger "app.b")))
      (llog:enable-recent-logs logger-a 10)

      (let ((llog:*logger* logger-a))
        (info "From logger A"))
      (let ((llog:*logger* logger-b))
        (info "From logger B"))

      (let ((count (llog:show-recent :logger-name "app.a" :stream output)))
        (is (= 1 count))
        (let ((result (get-output-stream-string output)))
          (is (search "From logger A" result))
          (is (not (search "From logger B" result)))))

      (llog:disable-recent-logs logger-a))))

(def-test show-recent-filters-by-pattern ()
  "Test that show-recent can filter by regex pattern."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "User login successful")
      (info "Database query executed")
      (info "User logout successful"))

    (let ((count (llog:show-recent :pattern "user" :stream output)))
      (is (= 2 count))
      (let ((result (get-output-stream-string output)))
        (is (search "User login" result))
        (is (not (search "Database query" result)))
        (is (search "User logout" result))))

    (llog:disable-recent-logs logger)))

(def-test show-recent-limits-count ()
  "Test that show-recent can limit the number of entries shown."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (dotimes (i 5)
        (info (format nil "Message ~D" i))))

    (let ((count (llog:show-recent :count 2 :stream output)))
      (is (= 2 count)))

    (llog:disable-recent-logs logger)))

(def-test show-recent-shows-all-with-all-keyword ()
  "Test that show-recent shows all entries with :count :all."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (dotimes (i 7)
        (info (format nil "Message ~D" i))))

    (let ((count (llog:show-recent :count :all :stream output)))
      (is (= 7 count)))

    (llog:disable-recent-logs logger)))

;;; Test grep-logs

(def-test grep-logs-finds-matching-entries ()
  "Test that grep-logs finds entries matching a pattern."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "Processing order 12345")
      (info "User authentication failed")
      (info "Processing order 67890"))

    (let ((matches (llog:grep-logs "order" :stream output)))
      (is (= 2 (length matches)))
      (is (search "order 12345" (log-entry-message (first matches))))
      (is (search "order 67890" (log-entry-message (second matches)))))

    (llog:disable-recent-logs logger)))

(def-test grep-logs-with-level-filter ()
  "Test that grep-logs can filter by level."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "Info with error keyword")
      (error "Error with error keyword"))

    (let ((matches (llog:grep-logs "error" :level :error :stream output)))
      (is (= 1 (length matches)))
      (is (>= (log-entry-level (first matches)) llog:+error+)))

    (llog:disable-recent-logs logger)))

(def-test grep-logs-with-logger-name-filter ()
  "Test that grep-logs can filter by logger name."
  (let ((output (make-string-output-stream)))
    (llog::clear-logger-registry)
    (let ((logger-db (llog:get-logger "app.db"))
          (logger-api (llog:get-logger "app.api")))
      (llog:enable-recent-logs logger-db 10)

      (let ((llog:*logger* logger-db))
        (info "Query executed"))
      (let ((llog:*logger* logger-api))
        (info "Query received"))

      (let ((matches (llog:grep-logs "query" :logger-name "app.db" :stream output)))
        (is (= 1 (length matches)))
        (is (search "app.db" (log-entry-logger-name (first matches)))))

      (llog:disable-recent-logs logger-db))))

(def-test grep-logs-regex-patterns ()
  "Test that grep-logs supports regex patterns."
  (let ((logger (llog:make-logger))
        (output (make-string-output-stream)))
    (llog:enable-recent-logs logger 10)

    (let ((llog:*logger* logger))
      (info "User 123 logged in")
      (info "User ABC logged in")
      (info "System started"))

    (let ((matches (llog:grep-logs "User \\d+" :stream output)))
      (is (= 1 (length matches)))
      (is (search "User 123" (log-entry-message (first matches)))))

    (llog:disable-recent-logs logger)))

;;; Test with-captured-logs

(def-test with-captured-logs-captures-entries ()
  "Test that with-captured-logs captures log entries."
  (let ((logger (llog:make-logger)))
    (multiple-value-bind (result logs)
        (llog:with-captured-logs (logger)
          (let ((llog:*logger* logger))
            (info "First")
            (warn "Second")
            (error "Third"))
          42)
      (is (= 42 result))
      (is (= 3 (length logs)))
      (is (search "First" (log-entry-message (first logs))))
      (is (search "Second" (log-entry-message (second logs))))
      (is (search "Third" (log-entry-message (third logs)))))))

(def-test with-captured-logs-filters-by-level ()
  "Test that with-captured-logs respects level filtering."
  (let ((logger (llog:make-logger :level :info)))
    (multiple-value-bind (result logs)
        (llog:with-captured-logs (logger :level :warn)
          (let ((llog:*logger* logger))
            (info "Info message")
            (warn "Warn message")
            (error "Error message"))
          'done)
      (is (eql 'done result))
      (is (= 2 (length logs)))  ; Only warn and error
      (is (search "Warn" (log-entry-message (first logs))))
      (is (search "Error" (log-entry-message (second logs)))))))

(def-test with-captured-logs-restores-outputs ()
  "Test that with-captured-logs restores original outputs."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream))
         (logger (llog:make-logger :outputs (list output))))
    (llog:with-captured-logs (logger)
      (let ((llog:*logger* logger))
        (info "During capture")))

    ;; After with-captured-logs, original output should be restored
    (let ((llog:*logger* logger))
      (info "After capture"))

    (let ((result (get-output-stream-string stream)))
      (is (not (search "During capture" result)))
      (is (search "After capture" result)))))

(def-test with-captured-logs-restores-level ()
  "Test that with-captured-logs restores original level."
  (let ((logger (llog:make-logger :level :info)))
    (llog:with-captured-logs (logger :level :debug)
      (is (= llog:+debug+ (logger-level logger))))

    ;; After with-captured-logs, original level should be restored
    (is (= llog:+info+ (logger-level logger)))))

(def-test with-captured-logs-handles-errors ()
  "Test that with-captured-logs restores state even on error."
  (let* ((stream (make-string-output-stream))
         (output (llog:make-stream-output stream))
         (logger (llog:make-logger :outputs (list output))))

    (signals cl:error
      (llog:with-captured-logs (logger)
        (let ((llog:*logger* logger))
          (info "Before error"))
        (cl:error "Test error")))

    ;; Original outputs should be restored despite error
    (is (equal (list output) (logger-outputs logger)))))

;;; Test define-field-type

(def-test define-field-type-creates-constructor ()
  "Test that define-field-type creates a field constructor."
  (llog:define-field-type test-uuid (value cl:string)
    :documentation "Test UUID field type.")

  (let ((field (funcall (symbol-function (find-symbol "TEST-UUID" :llog))
                        "id" "550e8400-e29b-41d4-a716-446655440000")))
    (is (typep field 'llog::field))
    (is (string= "id" (llog::field-name field)))
    (is (string= "550e8400-e29b-41d4-a716-446655440000" (llog::field-value field)))
    (is (eql :test-uuid (llog::field-type field)))))

(def-test define-field-type-with-coercion ()
  "Test that define-field-type supports value coercion."
  (llog:define-field-type test-percentage (value real)
    :documentation "Percentage field (0-100)."
    :coercion (max 0.0 (min 100.0 (cl:float value 1.0))))

  (let* ((constructor (symbol-function (find-symbol "TEST-PERCENTAGE" :llog)))
         (field1 (funcall constructor "completion" 75.5))
         (field2 (funcall constructor "overflow" 150.0))
         (field3 (funcall constructor "underflow" -10.0)))
    (is (= 75.5 (llog::field-value field1)))
    (is (= 100.0 (llog::field-value field2)))
    (is (zerop (llog::field-value field3)))))

(def-test define-field-type-exports-symbol ()
  "Test that define-field-type exports the constructor."
  (llog:define-field-type test-exported (value cl:string))

  ;; Symbol should be accessible from outside package
  (is (find-symbol "TEST-EXPORTED" :llog))
  (is (eql :external (nth-value 1 (find-symbol "TEST-EXPORTED" :llog)))))

(def-test define-field-type-inline-by-default ()
  "Test that define-field-type declares functions inline by default."
  (llog:define-field-type test-inlined (value cl:string))

  ;; Check that inline declaration exists
  (let ((sym (find-symbol "TEST-INLINED" :llog)))
    (is (sb-c::info :function :inlinep sym))))

(def-test define-field-type-usage-in-logging ()
  "Test that custom field types work with logging."
  (llog:define-field-type test-currency (value real)
    :documentation "Currency amount in dollars."
    :coercion (cl:float value 1.0d0))

  (let* ((logger (llog:make-logger))
         (constructor (symbol-function (find-symbol "TEST-CURRENCY" :llog))))
    (multiple-value-bind (result logs)
        (llog:with-captured-logs (logger)
          (let ((llog:*logger* logger))
            (llog:info-typed "Payment received"
              (funcall constructor "amount" 99.99d0))))
      (is (= 1 (length logs)))
      (let* ((entry (first logs))
             (fields (log-entry-fields entry))
             (amount-field (first fields)))
        (is (= 99.99d0 (llog::field-value amount-field)))
        (is (eql :test-currency (llog::field-type amount-field)))))))
