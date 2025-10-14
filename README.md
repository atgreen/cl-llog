# llog

**High-Performance Structured Logging for Common Lisp**

llog is a modern, high-performance structured logging framework for
Common Lisp.

## Features

- **Dual API**: Ergonomic sugared API and zero-allocation typed API
- **Structured Logging**: First-class support for key-value fields with type preservation
- **Multiple Encoders**: JSON, S-expressions, and colored console output
- **Thread-Safe**: Concurrent logging with bordeaux-threads locks
- **Contextual Logging**: Attach fields that flow through call chains
- **Leveled Logging**: TRACE, DEBUG, INFO, WARN, ERROR, FATAL, PANIC
- **Field Types**: String, int, float, bool, timestamp, duration, error/condition
- **Multiple Outputs**: Stream and file outputs with per-output configuration
- **Async Logging**: `make-async-output` queues entries and flushes via a background worker thread
- **Buffer Pool**: Thread-local caching with 92% allocation reduction (typed API vs sugared API)
- **File Buffering**: Configurable buffering strategies (:none, :line, :block)
- **Condition System Integration**: Enhanced error fields with backtrace capture, restart information, and condition chains
- **Hook System**: Extensible hooks for pre-log filtering/modification, post-log notifications, and error handling
- **Sampling and Rate Limiting**: Control log volume with probabilistic/deterministic sampling and token bucket rate limiting
- **REPL Integration**: Interactive development features including recent log buffer, grep search, log capture for testing, and custom field types
- **Hierarchical Loggers**: Named logger hierarchy with inheritance and pattern-based configuration
- **Pattern Layouts**: Customizable log formats with pattern strings
- **Tamper-Evident Audit Logs** (optional extension): Cryptographic hash chaining with Ed25519 digital signatures for compliance requirements (SOC 2, ISO 27001, SOX, HIPAA, PCI DSS)

## Quick Start

```lisp
(asdf:load-system :llog)

;; Create a logger
(defvar *logger* (llog:make-logger))

;; Simple logging
(llog:info "Application started")
(llog:warn "Configuration missing" :key "database.url")

;; Structured logging with fields
(llog:info "User logged in"
  :user-id 12345
  :username "alice"
  :ip-address "192.168.1.1")

;; Typed API for performance-critical paths
(llog:info-typed "Order processed"
  (llog:int "order-id" order-id)
  (llog:string "status" "completed")
  (llog:float "amount" 99.99))

;; Contextual logging
(let ((logger (llog:with-fields *logger*
                :request-id (uuid:make-v4-uuid))))
  (llog:info logger "Processing request")
  (process-request)
  (llog:info logger "Request completed"))

;; Switch output backend (console -> JSON file)
(let* ((file-output (llog:make-file-output "app.log" :encoder (llog:make-json-encoder)))
       (logger (llog:make-logger :outputs (list file-output))))
  (llog:info logger "Logs now flow to app.log"))

;; Fan-out to multiple backends (console + async JSON file)
(let* ((console (llog:make-stream-output *standard-output*
                                         :encoder (llog:make-console-encoder :colors t)))
       (file (llog:make-file-output "structured.log"
                                    :encoder (llog:make-json-encoder)))
       (async (llog:make-async-output file :queue-size 2048))
       (logger (llog:make-logger :outputs (list console async))))
  (llog:info logger "This entry appears on stdout and in structured.log"))
```

## Installation

### With ocicl

```bash
ocicl install llog
```

### Manual

```bash
git clone https://github.com/atgreen/cl-llog.git
cd llog
```

In your Lisp REPL:

```lisp
(asdf:load-system :llog)
```

## Configuring Outputs & Encoders

Every logger ships with a console output by default. Swap or add backends using the output helpers:

```lisp
;; Replace console output with JSON file logging
(setf llog:*logger*
      (llog:make-logger
        :outputs (list (llog:make-file-output "logs/app.json"
                                             :encoder (llog:make-json-encoder)))))

;; Add a human-readable console output alongside JSON
(llog:add-output llog:*logger*
                 (llog:make-stream-output *standard-output*
                                         :encoder (llog:make-console-encoder :colors t)))

;; Remove an output when it is no longer needed
(llog:remove-output llog:*logger*
                    (first (llog:logger-outputs llog:*logger*)))

;; Wrap a file output in the async writer to decouple I/O latency
(llog:add-output llog:*logger*
                 (llog:make-async-output
                   (llog:make-file-output "logs/structured.log"
                                          :encoder (llog:make-json-encoder))
                   :queue-size 4096))
```

Encoders are pluggable—pass `:encoder` to any output to switch formats (console, JSON, S-expression, pattern layout when available).

## Condition System Integration

LLOG provides first-class support for the Common Lisp condition system, automatically capturing rich debugging information when logging errors. This goes beyond simple error messages to include backtraces, active restarts, and condition chains.

### Basic Error Logging

Use `error-field-detailed` to capture comprehensive condition information:

```lisp
(handler-case
    (/ 1 0)
  (error (condition)
    (llog:error "Division error occurred"
                :error (llog:error-field-detailed "error" condition
                                                  :backtrace t
                                                  :restarts t))))
```

**JSON Output:**
```json
{
  "level": "error",
  "ts": "2025-10-13T19:56:26",
  "msg": "Division error occurred",
  "error": {
    "type": "DIVISION-BY-ZERO",
    "message": "arithmetic error DIVISION-BY-ZERO signalled",
    "backtrace": [
      "(/ 1 0)",
      "(HANDLER-CASE ...)",
      "..."
    ],
    "restarts": [
      {"name": "RETRY", "description": "Retry division"},
      {"name": "ABORT", "description": "Abort operation"}
    ]
  }
}
```

**Console Output:**
```
2025-10-13T19:56:26 [ERROR] Division error occurred
  error: arithmetic error DIVISION-BY-ZERO signalled (DIVISION-BY-ZERO)
    Backtrace:
      (/ 1 0)
      (HANDLER-CASE ...)
      ...
    Restarts:
      RETRY: Retry division
      ABORT: Abort operation
```

### Features

- **Automatic Backtrace Capture**: Implementation-specific support for SBCL and CCL with graceful fallback
- **Restart Information**: Documents available recovery options at the error site
- **Condition Chains**: Follows nested/wrapped conditions to root causes
- **Zero Overhead**: Backtrace capture only happens when explicitly enabled
- **All Encoders**: Works seamlessly with JSON, console, and S-expression outputs

### API

```lisp
;; Full control over what's captured
(llog:error-field-detailed "error" condition
  :backtrace t    ; Capture stack frames (default: T)
  :restarts nil   ; Capture restart info (default: NIL)
  :chain nil)     ; Follow condition chain (default: NIL)

;; Use with typed API for zero-allocation logging
(llog:error-typed "Database connection failed"
  (llog:error-field-detailed "db-error" condition :backtrace t))

;; Direct condition analysis for custom handling
(let ((info (llog:analyze-condition condition
              :backtrace t
              :restarts t
              :chain t)))
  ;; info is a condition-info structure
  (llog:condition-info-backtrace info)  ; => list of frame strings
  (llog:condition-info-restarts info)   ; => list of restart plists
  (llog:condition-info-cause info))     ; => parent condition or nil
```

### Use Cases

**Production Error Logging**:
```lisp
(handler-case
    (process-payment order)
  (payment-error (err)
    (llog:error "Payment processing failed"
                :order-id (order-id order)
                :amount (order-amount order)
                :error (llog:error-field-detailed "error" err :backtrace t))))
```

**Development Debugging**:
```lisp
;; Capture everything during development
(handler-case
    (load-config "config.lisp")
  (error (err)
    (llog:debug "Configuration load failed"
                :config-file "config.lisp"
                :error (llog:error-field-detailed "error" err
                         :backtrace t
                         :restarts t
                         :chain t))))
```

**Nested Error Handling**:
```lisp
;; Automatically follows condition chains
(handler-case
    (handler-case
        (open-database connection-string)
      (network-error (err)
        (error 'database-connection-error :cause err)))
  (error (err)
    (llog:error "Database unavailable"
                :error (llog:error-field-detailed "error" err :chain t))))
;; The logged error will include both the outer and inner conditions
```

## Hook System

LLOG provides a powerful hook system for extending logger behavior without modifying core code. Hooks can filter entries, transform fields, collect metrics, send notifications, or integrate with external services.

### Hook Types

- **`:pre-log`** - Called before logging; can modify or filter entries
- **`:post-log`** - Called after successful logging; for metrics, notifications
- **`:error`** - Called when logging errors occur; for error reporting

### Basic Usage

```lisp
;; Add a metrics counter
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (declare (ignore logger))
    (when (>= (llog:log-entry-level entry) llog:+error+)
      (incf *error-count*)))
  :name 'error-counter
  :priority 50)

;; Filter entries in production
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (if (< (llog:log-entry-level entry) llog:+info+)
        nil  ; Filter out TRACE and DEBUG
        entry))
  :name 'production-filter
  :priority 10)

;; Redact sensitive fields
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    ;; Modify entry fields to redact passwords
    (setf (llog:log-entry-fields entry)
          (loop for field in (llog:log-entry-fields entry)
                if (string= "password" (llog::field-key field))
                  collect (llog:string "password" "***REDACTED***")
                else
                  collect field))
    entry)
  :name 'redactor
  :priority 5)

;; Remove hooks
(llog:remove-hook *logger* :pre-log :name 'production-filter)
(llog:clear-hooks *logger* :post-log)  ; Clear all post-log hooks
```

### Features

- **Priority-Based Execution**: Lower priority numbers run first (default: 50)
- **Error Isolation**: Hook failures don't crash logging
- **Entry Filtering**: Pre-log hooks can return NIL to skip logging
- **Entry Modification**: Pre-log hooks can transform fields
- **Multiple Hooks**: Chain multiple hooks of the same type

### Example Hooks

See `examples/hooks.lisp` for 10 complete hook examples:
- Metrics counter (track logs by level)
- Sampling (log every Nth entry)
- Field redaction (PII protection)
- Context enrichment (add hostname, thread info)
- Performance profiling (track timing between logs)
- Error alerting (send notifications for critical errors)
- Circular buffer (keep recent logs in memory)

### Hook API

```lisp
;; Add a hook
(add-hook logger type function &key name priority)

;; Remove hooks
(remove-hook logger type &key name function)
(clear-hooks logger &optional type)

;; List hooks
(list-hooks logger &optional type)

;; Hook properties
(hook-type hook)      ; => :pre-log | :post-log | :error
(hook-function hook)  ; => #<FUNCTION ...>
(hook-name hook)      ; => name or NIL
(hook-priority hook)  ; => integer (default 50)
```

## Sampling and Rate Limiting

LLOG provides built-in sampling and rate limiting to control log volume in high-throughput applications.

### Sampling

Reduce log volume by logging only a subset of events:

```lisp
;; Probabilistic sampling - log 10% of DEBUG messages
(llog:set-sampling *logger* :debug 0.1)

;; Deterministic sampling - log every 100th INFO message
(llog:set-sampling *logger* :info :every 100)

;; Clear sampling
(llog:clear-sampling *logger* :debug)

;; Get statistics
(llog:get-sampling-stats *logger* :debug)
;; => (:total 10000 :sampled 1000 :dropped 9000 :rate 0.1)
```

### Rate Limiting

Prevent log storms using token bucket rate limiting:

```lisp
;; Allow maximum 100 ERROR logs per second
(llog:set-rate-limit *logger* :error 100 :per-second)

;; Allow maximum 10 WARN logs per minute
(llog:set-rate-limit *logger* :warn 10 :per-minute)

;; Clear rate limit
(llog:clear-rate-limit *logger* :error)

;; Check if currently rate limited
(llog:rate-limited-p *logger* :error)  ; => T or NIL

;; Get statistics
(llog:get-rate-limit-stats *logger* :error)
;; => (:total 500 :allowed 100 :dropped 400 :rate 0.2
;;     :current-tokens 5 :capacity 100 :refill-rate 100.0)
```

### Use Cases

- **High-traffic services**: Sample 1% of INFO logs, rate limit WARNs
- **Batch processing**: Log every 1000th record processed
- **Production debugging**: Temporarily increase sampling for investigation
- **Cost control**: Reduce log storage costs by 90%+ in high-volume scenarios

See `examples/sampling-examples.lisp` for 10 real-world examples including cost analysis, dynamic sampling, and monitoring strategies.

## REPL Integration

llog provides powerful REPL integration features for interactive development and debugging, making it easy to inspect, search, and capture log entries during development.

### Recent Logs Buffer

Keep recent log entries in memory for quick inspection:

```lisp
;; Enable recent logs tracking (keeps last 100 entries by default)
(llog:enable-recent-logs *logger*)

;; Generate some logs
(llog:info "User logged in" :user-id 123 :username "alice")
(llog:warn "Cache miss" :key "user:123")
(llog:error "Database timeout" :query "SELECT * FROM users")

;; View recent entries
(llog:show-recent)
;; Displays last 10 entries with timestamps and fields

;; Show all buffered entries
(llog:show-recent :count :all)

;; Filter by log level
(llog:show-recent :level :error)  ; Only ERROR and above

;; Filter by logger name
(llog:show-recent :logger-name "app.db")

;; Filter by regex pattern (case-insensitive)
(llog:show-recent :pattern "user")  ; Matches "User", "user", "USERNAME", etc.

;; Disable when done
(llog:disable-recent-logs *logger*)
```

### Searching Logs

Search through recent entries with regex patterns:

```lisp
;; Find all logs containing "error" (case-insensitive)
(llog:grep-logs "error")

;; Find login events at INFO level or above
(llog:grep-logs "login" :level :info)

;; Find database queries from specific logger
(llog:grep-logs "query" :logger-name "app.db")

;; Regex patterns supported
(llog:grep-logs "User \\d+")  ; Find entries like "User 123"

;; Returns list of matching log entries for programmatic use
(let ((errors (llog:grep-logs "failed")))
  (format t "Found ~D errors~%" (length errors)))
```

### Capturing Logs for Testing

Capture log output in tests without writing to disk:

```lisp
(fiveam:test my-feature-test
  (multiple-value-bind (result logs)
      (llog:with-captured-logs (*logger*)
        ;; Code under test
        (my-function)
        (process-data))

    ;; Assert on the captured logs
    (is (= 3 (length logs)))
    (is (search "Processing started"
                (llog:log-entry-message (first logs))))
    (is (>= (llog:log-entry-level (second logs))
            llog:+warn+))))

;; Capture only specific levels
(llog:with-captured-logs (*logger* :level :error)
  ;; Only ERROR and above are captured
  (llog:info "Not captured")
  (llog:error "This is captured"))
```

### Custom Field Types

Define custom field types with validation and coercion:

```lisp
;; Simple custom type
(llog:define-field-type uuid (value string)
  :documentation "UUID field type")

;; Type with validation
(llog:define-field-type email (value string)
  :documentation "Email field type"
  :coercion (progn
              (unless (find #\@ value)
                (error "Invalid email: ~A" value))
              value))

;; Type with coercion
(llog:define-field-type percentage (value real)
  :documentation "Percentage field (0-100)"
  :coercion (max 0.0 (min 100.0 (float value 1.0))))

;; Use custom types in logging
(llog:info-typed "User registered"
  (llog:uuid "user-id" "550e8400-e29b-41d4-a716-446655440000")
  (llog:email "email" "alice@example.com")
  (llog:percentage "completion" 75.5))
```

### Features

- **Circular Buffer**: Automatically wraps when full, always keeping most recent entries
- **Thread-Safe**: All REPL operations are protected with locks
- **Zero Overhead**: No performance impact when disabled
- **Flexible Filtering**: Combine level, logger name, and regex patterns
- **Test Integration**: Works seamlessly with FiveAM and other test frameworks
- **Custom Types**: Extend the type system with domain-specific field types

### Use Cases

**Interactive Development**:
```lisp
;; Enable during development session
(llog:enable-recent-logs *logger* 500)

;; Work on your feature, then review what happened
(llog:show-recent :pattern "database" :level :warn)
```

**Debugging Production Issues**:
```lisp
;; Temporarily enable in production to capture issue
(llog:enable-recent-logs *logger* 1000)

;; Let it run for a bit, then search for problems
(llog:grep-logs "timeout|failed|error" :level :error)
```

**Test-Driven Development**:
```lisp
(test api-error-handling
  "Verify proper error logging"
  (multiple-value-bind (result logs)
      (llog:with-captured-logs ()
        (handler-case (api-call-that-fails)
          (error (e) nil)))
    (is (= 1 (length logs)))
    (is (search "API call failed" (llog:log-entry-message (first logs))))))
```

## Tamper-Evident Audit Logs

**llog/audit** is an optional extension that provides cryptographically-secured audit trails with hash chaining and digital signatures. It's designed for compliance requirements where tamper detection is mandatory.

### What is Tamper-Evident?

**Tamper-evident** means the system can **detect** unauthorized modifications, but does not **prevent** them. If someone modifies a log entry, the hash chain breaks and verification reveals the tampering.

**True tamper-proofing** requires additional measures (WORM drives, external replication, HSM, OS-level protections).

### Architecture

The audit system uses:
1. **Hash Chaining**: Each entry includes the hash of the previous entry
2. **Periodic Checkpoints**: Merkle roots computed every N records
3. **Digital Signatures**: Optional Ed25519 signatures on checkpoints
4. **Verification Tool**: Detects breaks in the chain

### Installation

```lisp
;; Load the audit extension (separate from core llog)
(asdf:load-system :llog/audit)
```

### Basic Usage

```lisp
;; Create an audit output with hash chaining
(llog:add-output *logger*
  (llog/audit:make-audit-output "audit.log"
    :algorithm :sha256
    :checkpoint-interval 1000
    :metadata '(:system "payment-service" :version "1.0")))

;; Log normally - hashes computed automatically
(llog:info "Payment processed"
  :user-id 123
  :amount 99.99
  :transaction-id "txn-abc")

;; Verify audit log integrity
(let ((result (llog/audit:verify-audit-file "audit.log")))
  (case (llog/audit:verification-result-status result)
    (:valid (format t "Audit log is valid!~%"))
    (:tampered (format t "ALERT: Audit log has been tampered!~%"))))
```

### Digital Signatures

Add Ed25519 signatures for non-repudiation:

```lisp
;; Generate Ed25519 key pair (one-time setup)
(multiple-value-bind (private-key public-key)
    (ironclad:generate-key-pair :ed25519)
  ;; Save keys securely...
  )

;; Create signed audit output (synchronous)
(llog:add-output *logger*
  (llog/audit:make-audit-output "audit.log"
    :signing-key "audit-private.key"
    :checkpoint-interval 1000))

;; Or wrap with async output for background signing
(llog:add-output *logger*
  (llog:make-async-output
    (llog/audit:make-audit-output "audit.log"
      :signing-key "audit-private.key")))

;; Verify signatures
(let ((result (llog/audit:verify-audit-file "audit.log"
                                            :public-key "audit-public.key")))
  (if (eq :valid (llog/audit:verification-result-status result))
      (format t "Audit log verified! ~D entries, ~D checkpoints~%"
              (llog/audit:verification-result-total-entries result)
              (llog/audit:verification-result-checkpoints-verified result))
      (format t "Verification failed: ~A~%"
              (llog/audit:verification-result-first-error result))))
```

### Async Composition Pattern

Signature computation is synchronous by default, but expensive cryptographic operations can be offloaded to a background thread using llog's standard async output wrapper:

```lisp
;; Synchronous: signatures block the logging thread
(llog/audit:make-audit-output "audit.log" :signing-key "private.key")

;; Async: signatures happen in background thread
(llog:make-async-output
  (llog/audit:make-audit-output "audit.log" :signing-key "private.key"))
```

This composition pattern keeps the audit implementation simple while giving users control over performance tradeoffs.

### Compliance Use Cases

This feature is designed for compliance requirements that mandate tamper detection:
- **SOC 2**: Security audit trails
- **ISO 27001**: Information security logging
- **SOX**: Financial transaction logs
- **HIPAA**: Healthcare access logs
- **PCI DSS**: Payment system audit trails

### Test Status

**41/41 tests passing (100%)** - Production-ready implementation

### Documentation

See [src/audit/README.md](src/audit/README.md) for:
- Detailed API documentation
- File format specification
- Key generation examples
- Verification procedures
- Performance considerations

## Documentation

- [Buffer Pool System](docs/buffer-pool.md) - Thread-local caching and memory management
- [Buffer Pool Design](docs/buffer-pool-design.md) - Implementation details
- [API Documentation](docs/api.md) - Coming soon
- [User Guide](docs/guide.md) - Coming soon

## Comparison with Other Loggers

| Feature | zap | zerolog | log4cl | llog |
|---------|-----|---------|--------|------|
| Zero-allocation | ✓ | ✓ | ✗ | ✓ (92-94% reduction) |
| Structured logging | ✓ | ✓ | ✓ | ✓ |
| Dual API | ✓ | ✗ | ✗ | ✓ |
| Buffer pooling | ✓ | ✓ | ✗ | ✓ |
| Thread-local caching | ✓ | ✓ | ✗ | ✓ |
| Async logging | ✓ | ✗ | ✗ | ✓ |
| Hierarchical loggers | ✗ | ✗ | ✓ | ✓ |
| Pattern layouts | ✗ | ✗ | ✓ | ✓ |
| Thread-safe | ✓ | ✓ | ✓ | ✓ |
| JSON output | ✓ | ✓ | ✓ | ✓ |
| Hook system | ✗ | ✓ | ✗ | ✓ |
| Sampling/Rate limiting | ✗ | ✗ | ✗ | ✓ |
| REPL integration | N/A | N/A | ✓ | ✓ |
| Condition system | N/A | N/A | ✗ | ✓ |
| Tamper-evident audit logs | ✗ | ✗ | ✗ | ✓ |

## Development

### Running Tests

```lisp
(asdf:test-system :llog)
```

**Current Test Status**: 723 checks, 100% pass rate (all passing)

Test coverage includes:
- Log levels and filtering
- Field constructors and type preservation
- Logger lifecycle and configuration
- All three encoders (console, JSON, S-expression)
- Pattern layouts with format strings
- Sugared and typed APIs
- Contextual logging with fields
- Thread-safe concurrent logging (6 concurrency test suites)
- Buffer pool operations and thread-local caching
- File output with multiple buffering modes
- Async output with background worker thread
- Condition system integration (backtrace capture, restart info, condition chains)
- Hook system (pre-log, post-log, error hooks with priority and isolation)
- Sampling (probabilistic and deterministic)
- Rate limiting (token bucket with time-based refill)
- Hierarchical logger system
- REPL integration (recent logs buffer, grep search, log capture, custom field types)
- Tamper-evident audit logs (hash chaining, checkpoints, verification, Ed25519 signatures) - 41/41 tests

### Running Benchmarks

```lisp
(load "benchmarks/allocation-bench.lisp")
(llog/benchmarks:run-allocation-benchmarks 1000)
(llog/benchmarks:run-performance-benchmarks 10000)
(llog/benchmarks:compare-apis 1000)
```

**Allocation Benchmarks** (SBCL, 1000 iterations):
- Sugared API: 25.69 KB per call
- Typed API (stream): 2.04 KB per call (92% reduction)
- Typed API (file, block-buffered): 1.51 KB per call (94% reduction)

**Performance Benchmarks** (SBCL):
- Throughput: 333,333 logs/second
- Per-call latency: ~3μs (typed API)

## Author and License

llog was written by [Anthony Green](https://github.com/atgreen), and
is distributed under the terms of the MIT license.

## Acknowledgments

llog combines the best of both worlds:

**Performance-oriented design** inspired by Go logging libraries:
- [uber-go/zap](https://github.com/uber-go/zap) - Dual API design, zero-allocation goals
- [rs/zerolog](https://github.com/rs/zerolog) - Performance focus
- [sirupsen/logrus](https://github.com/sirupsen/logrus) - Structured logging

**REPL-friendly features** inspired by Common Lisp traditions:
- [log4cl](https://github.com/sharplispers/log4cl) - Hierarchical logging, editor integration, configuration management

## Contact

- Issues: [GitHub Issues](https://github.com/atgreen/cl-llog/issues)
