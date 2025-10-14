# LLOG API Reference

Complete API reference for the LLOG structured logging framework.

## Core API

- **[Core](core.md)** - Loggers, log levels, log entries, and basic operations
- **[Fields](fields.md)** - Typed field constructors for structured logging
- **[Outputs](outputs.md)** - Stream, file, and async output targets
- **[Encoders](encoders.md)** - Console, JSON, S-expression, and pattern layout encoders

## Advanced Features

- **[Hooks](hooks.md)** - Pre-log, post-log, and error hooks for extending behavior
- **[Sampling & Rate Limiting](sampling.md)** - Control log volume in high-throughput scenarios
- **[Hierarchical Loggers](hierarchy.md)** - Named logger hierarchy with inheritance
- **[Condition System](conditions.md)** - Error logging with backtrace and restart capture

## Extension Modules

Optional extensions that add specialized functionality:

- **[REPL Integration](repl.md)** - Interactive development features (llog/repl system)
  - Recent logs buffer with search and filtering
  - Log capture for testing
  - Custom field types with validation
  - Grep-style log searching

- **[Audit Logs](../../src/audit/README.md)** - Tamper-evident audit trails (llog/audit system)
  - Cryptographic hash chaining
  - Ed25519 digital signatures
  - Compliance-ready (SOC 2, ISO 27001, HIPAA, PCI DSS)
  - Verification tools

## Quick Reference

### Creating Loggers

```lisp
(make-logger &key name level outputs)  ; Create a logger
*logger*                                ; Global default logger
(root-logger)                           ; Root of logger hierarchy
(get-logger name &key level outputs)    ; Get/create named logger
```

### Logging Functions (Sugared API)

```lisp
(trace message &rest key-value-pairs)  ; TRACE level
(debug message &rest key-value-pairs)  ; DEBUG level
(info message &rest key-value-pairs)   ; INFO level
(warn message &rest key-value-pairs)   ; WARN level
(error message &rest key-value-pairs)  ; ERROR level
(fatal message &rest key-value-pairs)  ; FATAL level
(panic message &rest key-value-pairs)  ; PANIC level
```

### Typed API (Zero-Allocation)

```lisp
(trace-typed message &rest fields)     ; TRACE level
(debug-typed message &rest fields)     ; DEBUG level
(info-typed message &rest fields)      ; INFO level
(warn-typed message &rest fields)      ; WARN level
(error-typed message &rest fields)     ; ERROR level
(fatal-typed message &rest fields)     ; FATAL level
(panic-typed message &rest fields)     ; PANIC level
```

### Field Constructors

```lisp
(string key value)                     ; String field
(int key value)                        ; Integer field
(float key value)                      ; Float field (double-float)
(bool key value)                       ; Boolean field
(timestamp key universal-time)         ; Timestamp field
(duration-ms key milliseconds)         ; Duration field
(error-field key condition)            ; Simple error field
(error-field-detailed key condition    ; Detailed error with backtrace
  &key backtrace restarts chain)
```

### Outputs

```lisp
(make-stream-output stream &key encoder min-level)
(make-file-output path &key encoder min-level buffer-mode buffer-size)
(make-async-output output &key queue-size)
```

### Encoders

```lisp
(make-console-encoder &key colors time-format)
(make-json-encoder)
(make-sexpr-encoder)
(make-pattern-encoder pattern)
```

## Documentation Conventions

Throughout this API reference:

- **Required arguments** are shown without default values
- **Optional arguments** use `&optional` or `&key` with defaults shown when applicable
- **Return values** are documented for each function
- **Examples** demonstrate practical usage
- **Thread-safety** is noted where relevant

## See Also

- [README.md](../../README.md) - Main project documentation with examples
- [Buffer Pool](../buffer-pool.md) - Memory management internals
- [Buffer Pool Design](../buffer-pool-design.md) - Design decisions
