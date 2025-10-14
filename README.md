# LLOG

**High-Performance Structured Logging for Common Lisp**

LLOG is a modern, high-performance structured logging framework for Common Lisp with rich features for both development and production use.

## Features

- **Dual API**: Ergonomic sugared API and zero-allocation typed API
- **Structured Logging**: First-class support for key-value fields with type preservation
- **Multiple Encoders**: JSON, S-expressions, colored console output, and pattern layouts
- **Thread-Safe**: Concurrent logging with bordeaux-threads locks
- **Contextual Logging**: Attach fields that flow through call chains
- **Leveled Logging**: TRACE, DEBUG, INFO, WARN, ERROR, FATAL, PANIC
- **Multiple Outputs**: Stream and file outputs with per-output configuration
- **Async Logging**: Background worker threads for non-blocking I/O
- **Buffer Pool**: Thread-local caching with 92% allocation reduction (typed API vs sugared API)
- **File Buffering**: Configurable buffering strategies (`:none`, `:line`, `:block`)
- **Condition System Integration**: Rich error logging with backtrace capture, restarts, and condition chains
- **Hook System**: Extensible hooks for filtering, metrics, and notifications
- **Sampling and Rate Limiting**: Control log volume in high-throughput scenarios
- **REPL Integration**: Recent log buffer, grep search, log capture for testing
- **Hierarchical Loggers**: Named logger hierarchy with inheritance
- **Tamper-Evident Audit Logs** (optional): Cryptographic hash chaining with Ed25519 signatures for compliance

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

;; Typed API for performance-critical paths (zero-allocation)
(llog:info-typed "Order processed"
  (llog:int "order-id" order-id)
  (llog:string "status" "completed")
  (llog:float "amount" 99.99))

;; Error logging with automatic backtrace capture
(handler-case
    (risky-operation)
  (error (e)
    (llog:error "Operation failed"
                :error (llog:error-field-detailed "error" e :backtrace t))))

;; Multiple outputs: console + async JSON file
(let* ((console (llog:make-stream-output *standard-output*
                                         :encoder (llog:make-console-encoder :colors t)))
       (file (llog:make-file-output "app.log"
                                    :encoder (llog:make-json-encoder)
                                    :buffer-mode :block))
       (async (llog:make-async-output file :queue-size 4096))
       (logger (llog:make-logger :outputs (list console async))))
  (llog:info logger "Logs to both console and file"))
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

## Key Features

### Structured Logging

Log structured data with typed fields for easy parsing and analysis:

```lisp
(llog:info-typed "Payment processed"
  (llog:int "user-id" 12345)
  (llog:float "amount" 99.99)
  (llog:duration-ms "processing-time" 45)
  (llog:timestamp "completed-at"))
```

See the [Fields API](docs/api/fields.md) for all field types.

### Output Configuration

Multiple outputs with different encoders and buffering strategies:

```lisp
;; High-throughput: block buffering for performance
(llog:make-file-output "app.log"
  :encoder (llog:make-json-encoder)
  :buffer-mode :block
  :buffer-size 32768)

;; Critical logs: no buffering for durability
(llog:make-file-output "audit.log"
  :encoder (llog:make-json-encoder)
  :buffer-mode :none)

;; Background processing: async output
(llog:make-async-output
  (llog:make-file-output "background.log")
  :queue-size 4096)
```

See the [Outputs API](docs/api/outputs.md) for details on buffering modes and performance.

### Condition System Integration

Rich error logging with automatic backtrace capture:

```lisp
(handler-case
    (process-payment order)
  (payment-error (err)
    (llog:error "Payment failed"
                :order-id (order-id order)
                :error (llog:error-field-detailed "error" err
                         :backtrace t
                         :restarts t
                         :chain t))))
```

**Output includes**: error type, message, stack frames, available restarts, and condition chains.

See the [Condition System API](docs/api/conditions.md) for full details.

### Hooks

Extend logger behavior with hooks for filtering, metrics, and notifications:

```lisp
;; Filter sensitive data
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (redact-passwords entry)))

;; Track error metrics
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (when (>= (llog:log-entry-level entry) llog:+error+)
      (incf *error-count*))))
```

See the [Hooks API](docs/api/hooks.md) and `examples/hooks.lisp` for 10 complete examples.

### Sampling and Rate Limiting

Control log volume in high-throughput applications:

```lisp
;; Sample 1% of DEBUG logs
(llog:set-sampling *logger* :debug 0.01)

;; Rate limit errors to 100/second
(llog:set-rate-limit *logger* :error 100 :per-second)
```

See the [Sampling & Rate Limiting API](docs/api/sampling.md) for strategies and examples.

### REPL Integration

Interactive features for development:

```lisp
;; Keep recent logs in memory
(llog:enable-recent-logs *logger*)
(llog:show-recent :pattern "error" :level :warn)

;; Search with regex
(llog:grep-logs "User \\d+")

;; Capture logs in tests
(multiple-value-bind (result logs)
    (llog:with-captured-logs ()
      (my-function))
  (assert (= 3 (length logs))))
```

See the [REPL Integration API](docs/api/repl.md) for full details.

### Hierarchical Loggers

Organize loggers by module with inheritance:

```lisp
(defvar *app* (llog:get-logger "myapp" :level :info))
(defvar *db* (llog:get-logger "myapp.db" :level :debug))
(defvar *cache* (llog:get-logger "myapp.cache"))  ; Inherits :info from parent
```

See the [Hierarchical Loggers API](docs/api/hierarchy.md) for details.

### Tamper-Evident Audit Logs

Optional extension for compliance requirements (SOC 2, ISO 27001, SOX, HIPAA, PCI DSS):

```lisp
(asdf:load-system :llog/audit)

;; Create audit output with hash chaining and digital signatures
(llog:add-output *logger*
  (llog/audit:make-audit-output "audit.log"
    :signing-key "private.key"
    :checkpoint-interval 1000))

;; Verify integrity
(llog/audit:verify-audit-file "audit.log" :public-key "public.key")
```

See [src/audit/README.md](src/audit/README.md) for full documentation and compliance use cases.

## Documentation

### Getting Started

New to LLOG? Start with the **[User Guide](docs/guide.md)** - a comprehensive tutorial covering:
- Getting started with structured logging
- Output configuration and best practices
- Production patterns (high-throughput, buffering, sampling, rate limiting)
- Error logging with backtraces and condition integration
- Advanced features (hooks, custom encoders, hierarchical loggers)
- Testing strategies with log capture
- Deployment guide and production checklist

### API Reference

Complete API documentation is available in [docs/api/](docs/api/):

**Core APIs:**
- **[Core](docs/api/core.md)** - Loggers, log levels, logging functions, contextual logging
- **[Fields](docs/api/fields.md)** - Typed field constructors (string, int, float, bool, timestamp, duration, error)
- **[Outputs](docs/api/outputs.md)** - Stream, file (with buffering modes), and async outputs
- **[Encoders](docs/api/encoders.md)** - Console, JSON, S-expression, and pattern layouts

**Advanced Features:**
- **[Hooks](docs/api/hooks.md)** - Pre-log, post-log, and error hooks
- **[Sampling & Rate Limiting](docs/api/sampling.md)** - Volume control strategies
- **[Hierarchical Loggers](docs/api/hierarchy.md)** - Named hierarchy with inheritance
- **[Condition System](docs/api/conditions.md)** - Rich error logging with backtraces
- **[REPL Integration](docs/api/repl.md)** - Interactive development features

### Implementation Details

- [Buffer Pool System](docs/buffer-pool.md) - Thread-local caching and memory management
- [Buffer Pool Design](docs/buffer-pool-design.md) - Design decisions and benchmarks

## Test Status

**723/723 tests passing (100%)** - Production-ready implementation

## Performance

- **Typed API**: 92% allocation reduction vs sugared API
- **Async Logging**: Non-blocking I/O with bounded queues
- **Block Buffering**: 1M+ logs/second for high-throughput scenarios
- **Buffer Pool**: Thread-local caching eliminates allocation overhead

See the [User Guide](docs/guide.md#production-patterns) for performance tuning strategies.

## Author and License

LLOG was written by [Anthony Green](https://github.com/atgreen), and is distributed under the terms of the MIT license.

## Acknowledgments

LLOG combines the best of both worlds:

**Performance-oriented design** inspired by Go logging libraries:
- [uber-go/zap](https://github.com/uber-go/zap) - Dual API design, zero-allocation goals
- [rs/zerolog](https://github.com/rs/zerolog) - Performance focus
- [sirupsen/logrus](https://github.com/sirupsen/logrus) - Structured logging

**REPL-friendly features** inspired by Common Lisp traditions:
- [log4cl](https://github.com/sharplispers/log4cl) - Hierarchical logging, configuration management

## Contact

- Issues: [GitHub Issues](https://github.com/atgreen/cl-llog/issues)
