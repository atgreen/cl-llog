# LLOG

**High-Performance Structured Logging for Common Lisp**

LLOG is a modern, high-performance structured logging framework for
Common Lisp, inspired by the best practices from the Go ecosystem
(zap, zerolog, logrus).

## Status

🚧 **Under Active Development** - v0.1.0

## Performance Highlights

- **333K logs/second** throughput (SBCL, typed API)
- **92-94% allocation reduction** (typed API vs sugared API)
- **2KB per log call** with typed API
- **Thread-local buffer caching** with >95% hit rate
- **100% test pass rate** (477 checks)

## Features

### Implemented ✓

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

### In Progress 🚧

- Hierarchical logger naming (package.function.method auto-detection)
- Pattern layout encoder (configurable format strings)
- Daily rolling file appenders

### Planned 📋

- Hook system for extensibility (Phase 3.5)
- Condition system integration with backtrace capture (Phase 3.4)
- Sampling and rate limiting (Phase 3.6)
- REPL integration helpers: show-recent, grep-logs, with-captured-logs (Phase 3.7)
- Log templates: define-log-template macro (Phase 3.7)
- Custom field types: define-field-type macro (Phase 3.7)
- Compile-time log elimination (Phase 3.7)
- Configuration save/restore
- Editor integration (Emacs/VSCode support)

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
git clone https://github.com/yourusername/llog.git
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

## Documentation

- [Buffer Pool System](docs/buffer-pool.md) - Thread-local caching and memory management
- [Buffer Pool Design](docs/buffer-pool-design.md) - Implementation details
- [API Documentation](docs/api.md) - Coming soon
- [User Guide](docs/guide.md) - Coming soon

## Comparison with Other Loggers

| Feature | zap | zerolog | log4cl | LLOG |
|---------|-----|---------|--------|------|
| Zero-allocation | ✓ | ✓ | ✗ | ✓ (92-94% reduction) |
| Structured logging | ✓ | ✓ | ✓ | ✓ |
| Dual API | ✓ | ✗ | ✗ | ✓ |
| Buffer pooling | ✓ | ✓ | ✗ | ✓ |
| Thread-local caching | ✓ | ✓ | ✗ | ✓ |
| Async logging | ✓ | ✗ | ✗ | ✓ |
| Hierarchical loggers | ✗ | ✗ | ✓ | ✓ (planned) |
| Pattern layouts | ✗ | ✗ | ✓ | ✓ (planned) |
| Thread-safe | ✓ | ✓ | ✓ | ✓ |
| JSON output | ✓ | ✓ | ✓ | ✓ |
| Hook system | ✗ | ✓ | ✗ | ✓ (planned) |
| REPL integration | N/A | N/A | ✓ | ✓ (planned) |
| Condition system | N/A | N/A | ✗ | ✓ (planned) |

## Development

### Running Tests

```lisp
(asdf:test-system :llog)
```

**Current Test Status**: 477 checks, 100% pass rate

Test coverage includes:
- Log levels and filtering
- Field constructors and type preservation
- Logger lifecycle and configuration
- All three encoders (console, JSON, S-expression)
- Sugared and typed APIs
- Contextual logging with fields
- Thread-safe concurrent logging (6 concurrency test suites)
- Buffer pool operations and thread-local caching
- File output with multiple buffering modes
- Async output with background worker thread

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

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

LLOG combines the best of both worlds:

**Performance-oriented design** inspired by Go logging libraries:
- [uber-go/zap](https://github.com/uber-go/zap) - Dual API design, zero-allocation goals
- [rs/zerolog](https://github.com/rs/zerolog) - Performance focus
- [sirupsen/logrus](https://github.com/sirupsen/logrus) - Structured logging

**REPL-friendly features** inspired by Common Lisp traditions:
- [log4cl](https://github.com/sharplispers/log4cl) - Hierarchical logging, editor integration, configuration management

## Contact

- Issues: [GitHub Issues](https://github.com/yourusername/llog/issues)
- Discussions: [GitHub Discussions](https://github.com/yourusername/llog/discussions)
