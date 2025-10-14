# LLOG User Guide

A practical guide to structured logging with LLOG, from basic usage to production deployments.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Structured Logging](#structured-logging)
3. [Configuring Outputs](#configuring-outputs)
4. [Production Patterns](#production-patterns)
5. [Error Logging](#error-logging)
6. [Advanced Features](#advanced-features)
7. [Testing with LLOG](#testing-with-llog)
8. [Deployment Guide](#deployment-guide)
9. [Extension Modules](#extension-modules)
   - [REPL Integration (llog/repl)](#repl-integration-llogrepl)
   - [Audit Logs (llog/audit)](#audit-logs-llogaudit)

---

## Getting Started

### Installation

```bash
# With ocicl
ocicl install llog

# Or add to your system dependencies
:depends-on ("llog")
```

### Your First Logger

```lisp
(asdf:load-system :llog)

;; The simplest possible logging
(llog:info "Application started")
;; Output: 2025-10-14T06:45:12 [INFO] Application started

;; With context
(llog:info "User logged in" :user-id 12345 :username "alice")
;; Output: 2025-10-14T06:45:13 [INFO] User logged in
;;   user-id: 12345
;;   username: "alice"
```

The global `llog:*logger*` is automatically created when you first log. It writes to `*standard-output*` with colored console output.

### Understanding Log Levels

LLOG has seven log levels, from most verbose to most critical:

```lisp
(llog:trace "Entering function foo")  ; Finest details
(llog:debug "Variable x = 42")        ; Debug information
(llog:info "Request processed")       ; Informational
(llog:warn "Cache miss")              ; Warnings
(llog:error "Operation failed")       ; Errors
(llog:fatal "Database unavailable")   ; Fatal errors
(llog:panic "Critical failure")       ; Panic level
```

Set the minimum level to filter logs:

```lisp
(llog:set-level llog:*logger* :info)  ; Only INFO and above
(llog:debug "This won't be logged")
(llog:info "This will be logged")
```

---

## Structured Logging

Structured logging attaches key-value pairs (fields) to log entries, making them machine-parseable and queryable.

### Adding Fields (Sugared API)

The sugared API automatically infers field types:

```lisp
(llog:info "Order processed"
  :order-id 54321           ; Integer field
  :customer "alice"         ; String field
  :amount 99.99             ; Float field
  :priority t               ; Boolean field
  :processing-time 123)     ; Integer (milliseconds)
```

**JSON Output:**
```json
{"level":"info","ts":"2025-10-14T06:45:15","msg":"Order processed","order-id":54321,"customer":"alice","amount":99.99,"priority":true,"processing-time":123}
```

### Typed API for Performance

For hot code paths, use the typed API for 92% allocation reduction:

```lisp
(llog:info-typed "Order processed"
  (llog:int "order-id" order-id)
  (llog:string "customer" customer-name)
  (llog:float "amount" amount)
  (llog:bool "priority" is-priority)
  (llog:duration-ms "processing-time" elapsed-ms))
```

**When to Use Typed API:**
- Inside loops processing thousands of items
- High-frequency events (>10K/sec)
- Performance-critical paths
- When profiling shows logging allocations

**When to Use Sugared API:**
- Application startup/shutdown
- Infrequent events (user actions, errors)
- Development/debugging
- When readability matters more than performance

### Contextual Logging

Add fields that apply to multiple log entries:

```lisp
(defun handle-request (request)
  ;; Create logger with request context
  (let ((logger (llog:with-fields llog:*logger*
                  :request-id (request-id request)
                  :user-id (request-user-id request))))

    (llog:info logger "Request received")
    (llog:debug logger "Validating input")
    (process-request request)
    (llog:info logger "Request complete")))
;; All three logs include :request-id and :user-id
```

Or use the macro form:

```lisp
(llog:with-context (:request-id req-id :user-id user-id)
  (llog:info "Processing request")
  (process-data)
  (llog:info "Request complete"))
```

### Timing Operations

Use `duration-ms` for timing:

```lisp
(let ((start (get-internal-real-time)))
  (expensive-operation)
  (let ((elapsed (/ (- (get-internal-real-time) start)
                   (/ internal-time-units-per-second 1000.0))))
    (llog:info-typed "Operation complete"
      (llog:duration-ms "elapsed" elapsed))))
```

---

## Configuring Outputs

By default, logs go to `*standard-output*` with console formatting. You can add multiple outputs and change encoders.

### Multiple Outputs

Send logs to both console and file:

```lisp
(defvar *logger*
  (llog:make-logger
    :level :info
    :outputs (list
              ;; Human-readable console
              (llog:make-stream-output *standard-output*
                :encoder (llog:make-console-encoder :colors t))

              ;; Machine-readable JSON file
              (llog:make-file-output "logs/app.log"
                :encoder (llog:make-json-encoder)))))
```

### Separate Error Logs

Use per-output level filtering:

```lisp
(defvar *logger*
  (llog:make-logger
    :outputs (list
              ;; All logs
              (llog:make-file-output "logs/app.log"
                :min-level :info)

              ;; Errors only
              (llog:make-file-output "logs/errors.log"
                :min-level :error
                :encoder (llog:make-json-encoder)))))
```

### JSON for Log Aggregation

For Elasticsearch, Splunk, or other log aggregators:

```lisp
(defvar *json-logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "logs/structured.jsonl"
                :encoder (llog:make-json-encoder)))))

(llog:info *json-logger* "User action"
  :action "purchase"
  :user-id 123
  :amount 49.99)
```

Each log is one JSON object per line (JSON Lines format), perfect for streaming ingestion.

---

## Production Patterns

### High-Throughput Logging

For applications logging millions of events:

```lisp
(defvar *high-volume-logger*
  (llog:make-logger
    :outputs (list
              ;; Async + block buffering = maximum throughput
              (llog:make-async-output
                (llog:make-file-output "logs/events.log"
                  :encoder (llog:make-json-encoder)
                  :buffer-mode :block      ; Batch writes
                  :buffer-size 65536)      ; 64KB buffer
                :queue-size 8192))))       ; 8K entry queue

;; This can handle 1M+ logs/second
```

**Performance Characteristics:**
- No buffering (`:none`): ~50K logs/sec
- Line buffering (`:line`): ~200K logs/sec  (default)
- Block buffering (`:block`): ~1M+ logs/sec
- Async wrapper: Removes I/O latency from hot path

### File Buffering Strategies

Choose based on your durability requirements:

```lisp
;; Audit logs: No data loss (slowest)
(llog:make-file-output "audit.log"
  :buffer-mode :none)  ; Every log flushed immediately

;; Application logs: Good balance (default)
(llog:make-file-output "app.log"
  :buffer-mode :line)  ; Flush after each entry

;; Debug logs: Maximum performance
(llog:make-file-output "debug.log"
  :buffer-mode :block
  :buffer-size 32768)  ; Flush every 32KB
```

### Sampling High-Volume Events

Reduce log volume for debug/trace events:

```lisp
(defvar *logger* (llog:make-logger :level :debug))

;; Sample 1% of DEBUG logs
(llog:set-sampling *logger* :debug 0.01)

;; Or sample deterministically: every 1000th
(llog:set-sampling *logger* :debug :every 1000)

;; In a hot loop:
(loop for item in million-items
      do (process-item item)
         (llog:debug "Item processed" :id (item-id item)))
;; Only ~1000 logs written instead of 1 million
```

### Rate Limiting

Prevent log storms:

```lisp
;; Limit errors to 100/second
(llog:set-rate-limit *logger* :error 100 :per-second)

;; Now if errors spike, you won't overwhelm log storage
(dotimes (i 10000)
  (llog:error "Something went wrong" :iteration i))
;; Only 100 logs written per second
```

### Hierarchical Loggers

Organize logging by module:

```lisp
;; Configure root level
(llog:set-level (llog:root-logger) :warn)

;; Enable debug for specific subsystem
(defvar *db-logger* (llog:get-logger "myapp.database"))
(llog:set-level *db-logger* :debug)

;; Use in modules
(in-package :myapp/database)
(defvar *logger* (llog:get-logger "myapp.database"))

(defun connect (url)
  (llog:debug *logger* "Connecting" :url url)
  ...)
```

---

## Error Logging

### Basic Error Logging

```lisp
(handler-case
    (risky-operation)
  (error (e)
    (llog:error "Operation failed"
      :error-type (type-of e)
      :error-message (princ-to-string e))))
```

### Rich Error Context

Capture backtraces and restart information:

```lisp
(handler-case
    (process-payment order)
  (payment-error (err)
    (llog:error-typed "Payment failed"
      (llog:int "order-id" (order-id order))
      (llog:float "amount" (order-amount order))
      (llog:error-field-detailed "error" err
        :backtrace t
        :restarts t))))
```

**Output includes:**
- Error type and message
- Full backtrace (SBCL/CCL)
- Available restarts
- All fields formatted appropriately per encoder

### Nested Errors

Follow condition chains to root cause:

```lisp
(handler-case
    (handler-case
        (connect-database url)
      (network-timeout (e)
        (error 'database-connection-error
          :cause e
          :url url)))
  (error (e)
    (llog:error-typed "Database unavailable"
      (llog:error-field-detailed "error" e :chain t))))
;; Logs both the connection error AND the network timeout
```

### Error Hooks

Alert on critical errors:

```lisp
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (declare (ignore logger))
    (when (>= (llog:log-entry-level entry) llog:+fatal+)
      (send-pagerduty-alert
        (format nil "FATAL: ~A" (llog:log-entry-message entry)))))
  :name 'fatal-alerter)
```

---

## Advanced Features

### Pre-log Hooks for Field Redaction

```lisp
;; Redact sensitive fields
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (setf (llog:log-entry-fields entry)
          (loop for field in (llog:log-entry-fields entry)
                for key = (llog:field-name field)
                if (member key '("password" "ssn" "credit-card")
                          :test #'string=)
                  collect (llog:string key "***REDACTED***")
                else
                  collect field))
    entry)
  :name 'redact-sensitive-data
  :priority 1)  ; Run early

(llog:info "User login" :username "alice" :password "secret123")
;; Logs: User login username="alice" password="***REDACTED***"
```

### Context Enrichment

Add system context automatically:

```lisp
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (setf (llog:log-entry-fields entry)
          (append (llog:log-entry-fields entry)
                  (list (llog:string "hostname" (machine-instance))
                        ;; Note: Process ID access is implementation-specific
                        ;; SBCL: (sb-posix:getpid), CCL: (ccl::getpid)
                        ;; For portability, consider using UIOP or omitting
                        (llog:string "thread" (bt:thread-name
                                               (bt:current-thread))))))
    entry)
  :name 'add-system-context
  :priority 10)
```

### Metrics Collection

Track log counts by level:

```lisp
(defvar *log-metrics* (make-hash-table))

(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (declare (ignore logger))
    (let ((level (llog:level-keyword (llog:log-entry-level entry))))
      (incf (gethash level *log-metrics* 0))))
  :name 'metrics-collector)

;; Query metrics
(maphash (lambda (level count)
           (format t "~A: ~D~%" level count))
         *log-metrics*)
```

### Custom Encoders

Create a custom CSV encoder:

```lisp
(defclass csv-encoder (llog:encoder) ())

(defmethod llog:encode-entry ((encoder csv-encoder)
                               stream
                               (entry llog:log-entry))
  (format stream "~A,~A,~S~%"
          (llog:log-entry-timestamp entry)
          (llog:level-name (llog:log-entry-level entry))
          (llog:log-entry-message entry)))

;; Use it
(defvar *csv-logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "logs.csv"
                :encoder (make-instance 'csv-encoder)))))
```

---

## Testing with LLOG

### Capturing Logs in Tests

```lisp
(use-package :fiveam)

(test user-registration
  "Test user registration logging"
  (multiple-value-bind (user logs)
      (llog:with-captured-logs ()
        (register-user "alice" "alice@example.com"))

    ;; Assert on behavior
    (is (not (null user)))

    ;; Assert on logs
    (is (= 2 (length logs)))
    (is (search "User registered"
                (llog:log-entry-message (first logs))))
    (is (eql :info
             (llog:log-entry-level (first logs))))))
```

### Testing Error Logging

```lisp
(test error-handling-logs-correctly
  "Verify errors are logged with details"
  (multiple-value-bind (result logs)
      (llog:with-captured-logs (*logger* :level :error)
        (handler-case
            (dangerous-operation)
          (error (e) :error-handled)))

    (is (= 1 (length logs)))
    (is (>= (llog:log-entry-level (first logs))
            llog:+error+))
    (is (find "error" (llog:log-entry-fields (first logs))
              :key #'llog:field-name
              :test #'string=))))
```

### REPL Development Workflow

```lisp
;; At REPL: Enable recent logs
(llog:enable-recent-logs *logger* 500)

;; Develop your feature
(develop-feature)

;; Review what happened
(llog:show-recent)

;; Search for problems
(llog:grep-logs "error|failed" :level :warn)

;; Inspect specific entries
(let ((errors (llog:grep-logs "timeout")))
  (dolist (entry errors)
    (inspect entry)))
```

---

## Deployment Guide

### Configuration Management

```lisp
(defun configure-logging (config)
  "Configure logging from application config."
  (let ((level (getf config :log-level :info))
        (outputs (getf config :log-outputs)))

    (setf llog:*logger*
          (llog:make-logger
            :level level
            :outputs (mapcar #'make-output-from-config outputs)))))

(defun make-output-from-config (output-config)
  (ecase (getf output-config :type)
    (:file
     (llog:make-file-output
       (getf output-config :path)
       :encoder (make-encoder (getf output-config :format :json))
       :buffer-mode (getf output-config :buffer-mode :line)))

    (:async-file
     (llog:make-async-output
       (llog:make-file-output
         (getf output-config :path)
         :encoder (make-encoder (getf output-config :format :json))
         :buffer-mode (getf output-config :buffer-mode :block))))

    (:console
     (llog:make-stream-output *standard-output*
       :encoder (llog:make-console-encoder
                  :colors (getf output-config :colors t))))))
```

### Production Checklist

**Before Deployment:**

1. ✅ Set appropriate log level (`:info` or `:warn` for production)
2. ✅ Use JSON encoder for log aggregation
3. ✅ Enable async output for high-volume logging
4. ✅ Configure file rotation (external tool like logrotate)
5. ✅ Set up sampling for debug/trace logs
6. ✅ Add rate limiting for error logs
7. ✅ Implement sensitive data redaction hooks
8. ✅ Test log capture in CI/CD
9. ✅ Configure alerts for fatal errors
10. ✅ Document log structure for ops team

**Example Production Configuration:**

```lisp
(defvar *production-logger*
  (llog:make-logger
    :level :info
    :outputs (list
              ;; Structured logs for aggregation
              (llog:make-async-output
                (llog:make-file-output "/var/log/myapp/app.jsonl"
                  :encoder (llog:make-json-encoder)
                  :buffer-mode :block
                  :buffer-size 32768)
                :queue-size 4096)

              ;; Error logs separate
              (llog:make-file-output "/var/log/myapp/errors.jsonl"
                :encoder (llog:make-json-encoder)
                :min-level :error))))

;; Sampling for DEBUG (if enabled dynamically)
(llog:set-sampling *production-logger* :debug 0.01)

;; Rate limiting for errors
(llog:set-rate-limit *production-logger* :error 1000 :per-minute)

;; Redact sensitive data
(llog:add-hook *production-logger* :pre-log #'redact-sensitive-fields)

;; Alert on fatal errors
(llog:add-hook *production-logger* :post-log #'alert-on-fatal)
```

### Monitoring

Check logger health:

```lisp
(defun check-logger-health (logger)
  "Check if logger is healthy."
  (let ((issues nil))

    ;; Check rate limiting
    (dolist (level '(:error :warn))
      (when (llog:rate-limited-p logger level)
        (push (format nil "~A logs are rate limited" level) issues)))

    ;; Check sampling stats
    (let ((stats (llog:get-sampling-stats logger :debug)))
      (when stats
        (let ((rate (getf stats :rate)))
          (when (and rate (< rate 0.01))
            (push "Very aggressive sampling (>99% dropped)" issues)))))

    (if issues
        (values :unhealthy issues)
        (values :healthy nil))))
```

### Troubleshooting

**Logs not appearing:**
- Check log level: `(llog:logger-level llog:*logger*)`
- Check outputs: `(llog:logger-outputs llog:*logger*)`
- Check file permissions

**Performance issues:**
- Use typed API in hot paths
- Enable async output
- Use block buffering
- Add sampling for high-volume events

**Memory issues:**
- Disable recent logs in production
- Check async queue size
- Verify file handles are closed

---

## Extension Modules

LLOG provides optional extension modules for specialized functionality.

### REPL Integration (llog/repl)

The REPL extension adds interactive development features for working with logs at the REPL.

#### Installation

```lisp
(asdf:load-system :llog/repl)
```

**Dependencies:** `cl-ppcre` (for regex search)

#### Features

**Recent Logs Buffer:**

Keep recent log entries in memory for quick inspection:

```lisp
;; Enable with 500-entry circular buffer
(llog:enable-recent-logs *logger* 500)  ; See: enable-recent-logs

;; View last 10 entries
(llog:show-recent)  ; See: show-recent

;; View all buffered entries
(llog:show-recent :count :all)

;; Filter by level
(llog:show-recent :level :error)

;; Filter by pattern (case-insensitive regex)
(llog:show-recent :pattern "user" :count 20)

;; Disable when done
(llog:disable-recent-logs *logger*)  ; See: disable-recent-logs
```

- [`enable-recent-logs`](api/repl.md#enable-recent-logs) - Enable recent log tracking
- [`show-recent`](api/repl.md#show-recent) - Display recent entries with filtering
- [`disable-recent-logs`](api/repl.md#disable-recent-logs) - Disable tracking

**Grep-Style Search:**

Search through recent logs with regex patterns:

```lisp
;; Find all logs containing "error"
(llog:grep-logs "error")  ; See: grep-logs

;; Find login events at INFO or above
(llog:grep-logs "login" :level :info)

;; Find logs from specific logger
(llog:grep-logs "query" :logger-name "app.db")

;; Use regex patterns
(llog:grep-logs "User \\d+" :level :info)
;; Returns: list of matching log-entry structures

;; Programmatic use
(let ((errors (llog:grep-logs "failed|timeout")))
  (format t "Found ~D error-related logs~%" (length errors)))
```

- [`grep-logs`](api/repl.md#grep-logs) - Search recent logs with regex patterns

**Log Capture for Testing:**

Capture log output without writing to disk:

```lisp
(fiveam:test user-registration-test
  (multiple-value-bind (result logs)
      (llog:with-captured-logs ()  ; See: with-captured-logs
        (register-user "alice" "alice@example.com"))

    ;; Assert on logs
    (is (= 2 (length logs)))
    (is (search "User registered"
                (llog:log-entry-message (first logs))))
    (is (eql :info (llog:log-entry-level (first logs))))))

;; Capture only specific levels
(llog:with-captured-logs (*logger* :level :error)
  (llog:info "Not captured")
  (llog:error "This is captured"))
```

- [`with-captured-logs`](api/repl.md#with-captured-logs) - Capture logs for testing

**Custom Field Types:**

Define domain-specific field types with validation:

```lisp
;; Simple type
(llog:define-field-type uuid (value string)  ; See: define-field-type
  :documentation "UUID field")

;; Type with validation
(llog:define-field-type email (value string)
  :documentation "Email address"
  :coercion (progn
              (unless (find #\@ value)
                (error "Invalid email: ~A" value))
              value))

;; Type with coercion
(llog:define-field-type percentage (value real)
  :documentation "Percentage (0-100)"
  :coercion (max 0.0 (min 100.0 (float value 1.0))))

;; Use custom types
(llog:info-typed "User registered"
  (llog:uuid "user-id" "550e8400...")
  (llog:email "email" "alice@example.com")
  (llog:percentage "completion" 75.5))
```

- [`define-field-type`](api/repl.md#define-field-type) - Define custom field types with validation

**Use Cases:**

- **Interactive Development**: Review logs at REPL without tailing files
- **Debugging**: Search recent logs for specific patterns
- **Testing**: Capture and assert on log output in tests
- **Type Safety**: Define custom field types for domain consistency

**Thread Safety:** All REPL operations are thread-safe with automatic locking.

**Performance:** Zero overhead when disabled; circular buffer maintains constant memory.

See [docs/api/repl.md](api/repl.md) for complete API documentation.

---

### Audit Logs (llog/audit)

The audit extension provides cryptographically-secured, tamper-evident audit trails for compliance requirements.

#### Installation

```lisp
(asdf:load-system :llog/audit)
```

**Dependencies:** `ironclad` (cryptography), `cl-base64`, `babel`

#### What is Tamper-Evident?

**Tamper-evident** means the system can **detect** unauthorized modifications, but does not **prevent** them. If someone modifies a log entry, the hash chain breaks and verification reveals the tampering.

**True tamper-proofing** requires additional measures:
- Write-once media (WORM drives)
- Immediate external replication
- Hardware security modules (HSM)
- OS-level protections (append-only files)

#### Basic Usage

```lisp
;; Create audit output with hash chaining
(llog:add-output *logger*
  (llog/audit:make-audit-output "audit.log"  ; See: make-audit-output
    :algorithm :sha256
    :checkpoint-interval 1000
    :metadata '(:system "payment-service" :version "1.0")))

;; Log normally - hashes computed automatically
(llog:info "Payment processed"
  :user-id 123
  :amount 99.99
  :transaction-id "txn-abc")

;; Verify integrity
(let ((result (llog/audit:verify-audit-file "audit.log")))  ; See: verify-audit-file
  (case (llog/audit:verification-result-status result)
    (:valid (format t "Audit log is valid!~%"))
    (:tampered (format t "ALERT: Tampering detected!~%"))))
```

**API Functions:**
- [`make-audit-output`](../src/audit/README.md#basic-audit-logging-synchronous) - Create audit output with hash chaining
- [`verify-audit-file`](../src/audit/README.md#basic-audit-logging-synchronous) - Verify audit log integrity
- `verification-result-status` - Get verification status (`:valid` or `:tampered`)

#### Digital Signatures

Add Ed25519 signatures for non-repudiation:

```lisp
;; Generate key pair (one-time setup)
(multiple-value-bind (private-key public-key)
    (ironclad:generate-key-pair :ed25519)
  ;; Save keys securely...
  )

;; Create signed audit output
(llog:add-output *logger*
  (llog/audit:make-audit-output "audit.log"  ; See: digital signatures
    :signing-key "audit-private.key"
    :checkpoint-interval 1000))

;; Verify with public key
(let ((result (llog/audit:verify-audit-file "audit.log"
                                            :public-key "audit-public.key")))
  (format t "Status: ~A~%" (llog/audit:verification-result-status result))
  (format t "Entries: ~D~%" (llog/audit:verification-result-total-entries result))
  (format t "Checkpoints: ~D~%"
          (llog/audit:verification-result-checkpoints-verified result)))
```

**API Functions:**
- [`make-audit-output`](../src/audit/README.md#digital-signatures) with `:signing-key` - Create signed audit output
- [`verify-audit-file`](../src/audit/README.md#digital-signatures) with `:public-key` - Verify signatures
- `verification-result-total-entries` - Get total entry count
- `verification-result-checkpoints-verified` - Get verified checkpoint count

#### Async Composition

Signature computation is synchronous but can be offloaded to background thread:

```lisp
;; Synchronous: signatures block logging thread
(llog/audit:make-audit-output "audit.log" :signing-key "private.key")

;; Async: signatures happen in background
(llog:make-async-output  ; See: make-async-output
  (llog/audit:make-audit-output "audit.log" :signing-key "private.key"))
```

**API Functions:**
- [`make-async-output`](../src/audit/README.md#async-composition-pattern) - Wrap audit output for background processing
- See [Async Composition Pattern](../src/audit/README.md#async-composition-pattern) for details

#### Compliance Use Cases

Designed for compliance requirements mandating tamper detection:
- **SOC 2**: Security audit trails
- **ISO 27001**: Information security logging
- **SOX**: Financial transaction logs
- **HIPAA**: Healthcare access logs
- **PCI DSS**: Payment system audit trails

**Test Status:** 41/41 tests passing (100%) - Production-ready

See [src/audit/README.md](../src/audit/README.md) for complete documentation including file format specification, key management, and verification procedures.

---

## Next Steps

- **API Reference**: See [docs/api/](api/) for complete API documentation
- **Examples**: Check `examples/` directory for complete working examples
- **Buffer Pool**: Read [buffer-pool.md](buffer-pool.md) for performance internals

---

**Questions or Issues?** File an issue at [GitHub Issues](https://github.com/atgreen/cl-llog/issues)
