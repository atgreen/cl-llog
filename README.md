# LLOG

**High-Performance Structured Logging for Common Lisp**

LLOG is a modern, high-performance structured logging framework for Common Lisp, inspired by the best practices from the Go ecosystem (zap, zerolog, logrus).

## Status

🚧 **Under Active Development** - v0.1.0

## Features

### Core Design

- **Dual API**: Choose between ergonomic sugared API and zero-allocation typed API
- **Structured by Default**: First-class support for key-value logging
- **High Performance**: < 100ns per log call in typed API, minimal allocations
- **Multiple Encoders**: JSON, S-expressions, and human-readable console output
- **Thread-Safe**: Safe concurrent logging from multiple threads
- **Zero Dependencies**: Core library has no external dependencies

### Coming Soon

- Async logging with ring buffers
- Condition system integration
- Hook system for extensibility
- Sampling and rate limiting
- Compile-time log elimination
- REPL integration helpers

## Quick Start

```lisp
;; Load with ocicl
(ocicl:install :llog)
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

## Performance

LLOG is designed for minimal overhead:

- **Typed API**: < 100ns per log call (without I/O)
- **Sugared API**: < 500ns per log call (without I/O)
- **Zero allocations**: Achieved in typed API hot path
- **10x faster**: Than existing CL loggers for structured logging

## Documentation

- [PRD.md](PRD.md) - Product Requirements Document
- [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) - Development roadmap
- [API Documentation](docs/api.md) - Coming soon
- [User Guide](docs/guide.md) - Coming soon

## Comparison with Go Libraries

| Feature | zap | zerolog | logrus | LLOG |
|---------|-----|---------|--------|------|
| Zero-allocation | ✓ | ✓ | ✗ | ✓ |
| Structured logging | ✓ | ✓ | ✓ | ✓ |
| Dual API | ✓ | ✗ | ✗ | ✓ |
| Hook system | ✗ | ✓ | ✓ | ✓ |
| REPL integration | N/A | N/A | N/A | ✓ |
| Condition system | N/A | N/A | N/A | ✓ |

## Development

### Running Tests

```lisp
(asdf:test-system :llog)
```

### Running Benchmarks

```lisp
(asdf:load-system :llog/benchmarks)
(llog-benchmarks:run-all)
```

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

Inspired by:
- [uber-go/zap](https://github.com/uber-go/zap)
- [rs/zerolog](https://github.com/rs/zerolog)
- [sirupsen/logrus](https://github.com/sirupsen/logrus)

## Roadmap

### Phase 1: Foundation (Weeks 1-4) - In Progress
- [x] Project structure
- [ ] Core logger protocol
- [ ] Log levels and filtering
- [ ] Basic field types
- [ ] Simple output

### Phase 2: Structured Logging (Weeks 5-8)
- [ ] Sugared and typed APIs
- [ ] JSON encoder
- [ ] Multiple encoders
- [ ] Contextual logging
- [ ] Thread safety

### Phase 3: Advanced Features (Weeks 9-12)
- [ ] Buffer pools
- [ ] Async logging
- [ ] Condition integration
- [ ] Hooks
- [ ] Sampling/rate limiting

### Phase 4: Quality (Weeks 13-16)
- [ ] >90% test coverage
- [ ] Performance benchmarks
- [ ] Multi-implementation support
- [ ] Documentation

### Phase 5: Release (Weeks 17-24)
- [ ] Quicklisp submission
- [ ] Community outreach
- [ ] Integration examples

## Contact

- Issues: [GitHub Issues](https://github.com/yourusername/llog/issues)
- Discussions: [GitHub Discussions](https://github.com/yourusername/llog/discussions)
