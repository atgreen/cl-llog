# Product Requirements Document: LLOG

## A Best-in-Class Logging Framework for Common Lisp

**Version:** 1.0
**Date:** 2025-10-13
**Status:** Draft

---

## Executive Summary

LLOG is a high-performance, structured logging framework for Common Lisp inspired by the best practices from the Go ecosystem (zap, zerolog, logrus). It aims to provide zero-allocation logging, structured output, and a developer-friendly API while leveraging Common Lisp's unique strengths including the condition system, macros, and REPL-driven development.

**Key Differentiators:**
- **Performance First:** Zero-allocation structured logging for hot paths
- **Lisp-Native:** Deep integration with Common Lisp condition system and REPL workflows
- **Dual APIs:** High-performance typed API and ergonomic sugared API
- **Structured by Default:** First-class support for structured logging with S-expression and JSON output

---

## Problem Statement

Current Common Lisp logging solutions have several limitations:

1. **Performance Overhead:** Existing libraries use reflection and dynamic formatting, causing unnecessary allocations in hot code paths
2. **Poor Structured Logging:** Most libraries focus on string-based messages rather than machine-parseable structured data
3. **Limited Observability:** Lack of integration with modern observability tools and JSON-based log aggregators
4. **Weak Context Management:** Difficult to maintain contextual information across call chains
5. **Suboptimal Developer Experience:** APIs that don't leverage Lisp's macro system or REPL workflow

---

## Goals and Non-Goals

### Goals

1. **Performance:** Match or exceed the performance characteristics of Go's zap library
2. **Zero Allocations:** Provide a zero-allocation logging path for performance-critical code
3. **Structured Logging:** Make structured logging the default, with key-value fields as first-class citizens
4. **Multiple Output Formats:** Support JSON, S-expressions, and human-readable formats out of the box
5. **Context Propagation:** Easy attachment of contextual fields that flow through the call stack
6. **Condition System Integration:** Seamlessly log Common Lisp conditions with full context
7. **Thread Safety:** Safe concurrent logging from multiple threads
8. **Extensibility:** Hook system for custom formatters, outputs, and middleware
9. **REPL-Friendly:** Excellent developer experience during interactive development

### Non-Goals

1. **Log Rotation:** Delegate to OS-level tools (logrotate) or separate libraries
2. **Remote Transport:** Focus on local logging; integration with remote systems via hooks
3. **Query/Search:** Analysis should be done by external tools (ELK, Loki, etc.)
4. **Backwards Compatibility:** Not attempting to replace or be compatible with existing CL logging APIs

---

## User Personas

### Performance-Conscious Backend Developer
- Needs minimal overhead logging in high-throughput services
- Wants structured logs for aggregation and analysis
- Values compile-time optimization and zero allocations

### Library Author
- Needs logging that doesn't force dependencies on users
- Wants configurable output that library consumers can control
- Values clean, non-intrusive API

### DevOps Engineer
- Needs JSON output for log aggregation (ELK, Loki, Datadog)
- Wants structured fields for filtering and alerting
- Values consistent log formats across services

### Application Developer
- Needs easy-to-use logging during development
- Wants readable console output in REPL
- Values debugging capabilities and stack traces

---

## Feature Requirements

### FR-1: Dual API Design

**Priority:** P0 (Critical)

Provide two complementary APIs inspired by zap:

1. **Sugared Logger (`slog`):** Developer-friendly API with dynamic typing
   - Natural Lisp-style keyword arguments
   - Automatic type inference
   - Printf-style formatting support
   - 4-10x faster than traditional logging

2. **Typed Logger (`log`):** Zero-allocation API for performance-critical paths
   - Strongly-typed field functions
   - Compile-time optimization
   - No reflection or boxing
   - Minimal/zero allocations

**Example:**
```lisp
;; Sugared API - ergonomic for most use cases
(llog:info "User logged in"
  :user-id 12345
  :username "alice"
  :ip-address "192.168.1.1"
  :duration-ms 145)

;; Typed API - zero allocations for hot paths
(llog:info-typed "User logged in"
  (llog:int "user-id" 12345)
  (llog:string "username" "alice")
  (llog:string "ip-address" "192.168.1.1")
  (llog:int "duration-ms" 145))
```

### FR-2: Leveled Logging

**Priority:** P0 (Critical)

Support standard log levels with semantic meaning:

- **TRACE:** Fine-grained debugging (disabled by default)
- **DEBUG:** Detailed debugging information
- **INFO:** General informational messages
- **WARN:** Warning messages for potentially harmful situations
- **ERROR:** Error events that might still allow the application to continue
- **FATAL:** Severe errors that lead to application termination
- **PANIC:** Errors that invoke the debugger/abort

**Requirements:**
- Runtime level filtering with minimal overhead
- Compile-time elimination of log statements via reader conditionals
- Per-logger level configuration
- Dynamic level changes without restart

**Conditional Logging:**
Enable efficient conditional execution based on log level:

```lisp
;; Check if logging is enabled before expensive computation
(when (llog:debug-p)
  (let ((expensive-data (compute-expensive-debug-info)))
    (llog:debug "Debug info" :data expensive-data)))

;; Macro form that returns T/NIL
(if (llog:trace-p *logger*)
    (detailed-trace)
    (simple-trace))
```

### FR-3: Structured Fields

**Priority:** P0 (Critical)

First-class support for structured key-value logging:

**Supported Field Types:**
- Integers (fixnum, bignum)
- Floats (single, double)
- Strings
- Symbols/Keywords
- Booleans
- Timestamps
- Durations
- Errors/Conditions
- Arrays/Sequences
- Hash tables
- Custom objects (via protocol)

**Field Features:**
- Type preservation through to output
- Efficient serialization
- Custom field types via extension protocol

```lisp
(llog:info "Database query completed"
  :query-id #x1A2B3C
  :table 'users
  :duration (llog:duration-ms 45.3)
  :rows-returned 150
  :cached-p nil
  :query-params #("alice" 30))
```

### FR-4: Contextual Logging

**Priority:** P0 (Critical)

Support context propagation with accumulated fields:

```lisp
;; Create logger with base context
(defvar *request-logger*
  (llog:with-fields *logger*
    :service "api"
    :version "1.2.3"))

;; Add request-specific context
(let ((logger (llog:with-fields *request-logger*
                :request-id (uuid:make-v4-uuid)
                :user-id user-id)))
  ;; All logs within scope include these fields
  (llog:info logger "Request started")
  (process-request)
  (llog:info logger "Request completed"))
```

**Requirements:**
- Efficient context chaining
- Thread-local contexts via dynamic variables
- Minimal copying overhead
- Context inheritance

### FR-5: Output Formats

**Priority:** P0 (Critical)

**Built-in Encoders:**

1. **JSON Encoder** (for production/aggregation)
```json
{"level":"info","ts":"2025-10-13T08:04:23.123Z","msg":"User logged in","user-id":12345,"username":"alice"}
```

2. **S-Expression Encoder** (Lisp-native structured format)
```lisp
(:level :info :ts 3912847463123 :msg "User logged in" :user-id 12345 :username "alice")
```

3. **Console Encoder** (human-readable for development)
```
2025-10-13T08:04:23.123Z [INFO] User logged in
  user-id: 12345
  username: alice
  ip-address: 192.168.1.1
```

4. **Colored Console Encoder** (REPL-friendly with ANSI colors)
```
[38;5;10m2025-10-13T08:04:23.123Z[0m [38;5;12mINFO[0m User logged in
  user-id: 12345
  username: alice
```

5. **Pattern Layout Encoder** (configurable format strings)
```lisp
;; Create encoder with custom pattern
(llog:make-pattern-encoder
  :pattern "%d{ISO8601} [%p] %c{2}: %m%n"
  :patterns '(("%d" . timestamp)
              ("%p" . level)
              ("%c" . logger-name)
              ("%m" . message)
              ("%n" . newline)
              ("%t" . thread-name)
              ("%F" . source-file)
              ("%L" . line-number)))

;; Example output:
;; 2025-10-13T08:04:23.123Z [INFO] api.handler: User logged in
```

**Pattern Directives:**
- `%d{format}` - Timestamp with optional format
- `%p` - Log level
- `%c{precision}` - Logger category name, optionally shortened
- `%m` - Log message
- `%n` - Newline
- `%t` - Thread name
- `%F` - Source file name
- `%L` - Line number
- `%M` - Method/function name
- `%%` - Literal percent sign

### FR-6: Condition System Integration

**Priority:** P1 (High)

Deep integration with Common Lisp's condition system:

```lisp
;; Automatic condition logging
(handler-bind
  ((error (lambda (c)
            (llog:error "Operation failed"
              :error c  ;; Automatically extracts condition info
              :backtrace t))))
  (risky-operation))

;; Log with condition context
(llog:with-condition-context (c)
  (llog:error "Failed to process request"
    :condition-type (type-of c)
    :restart-names (mapcar #'restart-name (compute-restarts))))
```

**Features:**
- Automatic condition field extraction
- Optional backtrace capture
- Restart information logging
- Condition chain traversal

### FR-7: Performance Optimizations

**Priority:** P0 (Critical)

**Compile-Time Optimizations:**
- Log level elimination via reader conditionals
- Inline expansion of logging macros
- Type inference for zero-boxing paths

```lisp
;; Feature flags for compile-time log elimination
#+llog-no-debug (llog:debug "Expensive debug calculation: ~A" (slow-fn))
;; Expands to nothing when feature is set

;; Level-based reader conditionals
#.(if (>= llog:*compile-time-level* llog:+debug+)
    '(llog:debug "Debug message")
    'nil)
```

**Runtime Optimizations:**
- Zero-allocation logging path
- Pre-allocated buffer pools
- Lazy field evaluation
- Lock-free ring buffers for async logging

**Performance Targets:**
- Typed API: < 100ns per log call (without I/O)
- Sugared API: < 500ns per log call (without I/O)
- Zero allocations in typed API hot path
- < 2 allocations per call in sugared API

### FR-8: Multiple Outputs

**Priority:** P1 (High)

Support multiple concurrent outputs:

```lisp
(llog:make-logger
  :outputs (list
    (llog:make-file-output "/var/log/app.json"
                          :encoder :json)
    (llog:make-stream-output *standard-output*
                            :encoder :console
                            :min-level :warn)
    (llog:make-syslog-output :facility :local0)
    ;; Daily rolling file appender
    (llog:make-daily-file-output "/var/log/app.%Y%m%d.log"
                                 :encoder :json)))
```

**Output Types:**

1. **Stream Output** - Write to any CL stream
2. **File Output** - Simple file output with optional buffering
3. **Daily Rolling File Output** - Automatic daily log rotation
   - Pattern-based filenames with date/time substitution
   - Automatic rollover when date changes
   - Optional backup file naming
   - Configurable retention policy

```lisp
;; Daily rolling with custom pattern
(llog:make-daily-file-output
  "/var/log/app.%Y%m%d.log"  ; Rolls daily
  :backup-pattern "app.%Y%m%d-%H%M.log.bak"  ; Optional backup naming
  :max-backups 7  ; Keep last 7 days
  :encoder (llog:make-json-encoder))

;; Weekly rolling
(llog:make-daily-file-output
  "/var/log/app.%Y-W%U.log"  ; Rolls weekly (%U = week number)
  :encoder :json)
```

**Features:**
- Multiple simultaneous outputs
- Per-output encoding and filtering
- Async writes via background threads
- Buffering strategies (unbuffered, line-buffered, block-buffered)
- Automatic log rotation based on time or size

### FR-9: Sampling and Rate Limiting

**Priority:** P2 (Medium)

Control log volume in high-throughput scenarios:

```lisp
;; Sample 1% of debug logs
(llog:debug "Detailed trace" :sample-rate 0.01)

;; Rate limit to 10 per second
(llog:warn "Rate limited warning"
  :rate-limit (llog:make-rate-limiter :rate 10 :per :second))

;; First N occurrences
(llog:info "Initialization step" :first 100)
```

### FR-10: Hooks and Middleware

**Priority:** P2 (Medium)

Extensibility via hooks inspired by logrus:

```lisp
;; Add custom hook
(llog:add-hook *logger*
  (lambda (entry)
    (when (>= (llog:entry-level entry) :error)
      (send-to-sentry entry))))

;; Middleware for field transformation
(llog:add-middleware *logger*
  (lambda (entry)
    (llog:add-field entry :hostname (machine-instance))))
```

**Hook Types:**
- Pre-log hooks (modify/filter entries)
- Post-log hooks (notifications, alerts)
- Error hooks (handle logging failures)

### FR-11: Typed Logger Macro DSL

**Priority:** P1 (High)

Leverage Lisp macros for performance and ergonomics:

```lisp
;; Define reusable log templates
(llog:define-log-template user-action (action user-id)
  (:level :info)
  (:message "User action performed")
  (:fields
    (string "action" ,action)
    (int "user-id" ,user-id)
    (timestamp "ts")))

;; Usage
(user-action "login" 12345)

;; Expands to optimized typed logging call
```

**Expression Logging:**

Quick debugging macro that logs both expression and its value:

```lisp
;; Log expression and result
(llog:expr (+ x y))
;; Outputs: "(+ x y) => 42"

;; With custom level
(llog:expr-debug (complex-calculation x y z))
;; Outputs: "(complex-calculation x y z) => #<RESULT ...>"

;; Multiple expressions
(llog:exprs x y (sqrt z))
;; Outputs:
;;   x => 10
;;   y => 20
;;   (sqrt z) => 5.0
```

**Implementation:**
```lisp
(defmacro llog:expr (expr &optional (level :debug))
  `(let ((result ,expr))
     (llog:log ,level "~A => ~S" ',expr result)
     result))
```

### FR-12: REPL Integration

**Priority:** P1 (High)

Excellent REPL experience:

```lisp
;; Interactive log level changes
(llog:set-level *logger* :debug)

;; Temporary log capture for testing
(llog:with-captured-logs (logs)
  (my-function)
  ;; Inspect logs in REPL
  logs)

;; Pretty-print recent logs
(llog:show-recent 10)

;; Search logs interactively
(llog:grep-logs :user-id 12345)
```

**Configuration Save/Restore:**

Save and restore logging configurations for debugging sessions:

```lisp
;; Save current configuration with a name
(llog:save :debugging-auth-bug)
(llog:save :performance-testing)

;; List saved configurations
(llog:list-configs)
;; => (:debugging-auth-bug :performance-testing :last-session)

;; Restore a saved configuration
(llog:restore :debugging-auth-bug)

;; Configurations persist across sessions (saved to ~/.llog-configs or similar)
```

**"Narrowing in Reverse" Workflow:**

A powerful debugging technique unique to hierarchical loggers:

```lisp
;; 1. Start with broad DEBUG logging
(llog:set-level :debug)  ; Root logger to DEBUG
(run-system)  ; Floods REPL with logs

;; 2. Selectively turn OFF categories you don't need
(llog:set-level "database" :off)
(llog:set-level "network.http" :off)
(llog:set-level "ui.render" :off)
(run-system)  ; Much cleaner output, focused on relevant areas

;; 3. Save this focused configuration
(llog:save :bug-1234-investigation)

;; 4. Later (or on another machine), restore it
(llog:restore :bug-1234-investigation)

;; 5. Reset when done
(llog:reset-all-levels)  ; Back to defaults
```

**Interactive Configuration Display:**

```lisp
;; Show current logging configuration as a tree
(llog:config)
;; Output:
;; ROOT [INFO]
;;   ├─ myapp [DEBUG]
;;   │  ├─ myapp.api [DEBUG]
;;   │  │  ├─ myapp.api.auth [OFF]
;;   │  │  └─ myapp.api.handler [TRACE]
;;   │  └─ myapp.database [OFF]
;;   └─ third-party [WARN]
```

### FR-13: Hierarchical Logger Naming

**Priority:** P1 (High)

Automatic logger hierarchy based on code context:

**Automatic Category Detection:**

Logger categories are automatically determined from the lexical context:

```lisp
(in-package :myapp)

(defun process-request ()
  (llog:info "Processing")  ; Logger: "myapp.process-request"
  (labels ((helper ()
             (llog:debug "Helper")))  ; Logger: "myapp.process-request.helper"
    (helper)))

(defmethod handle ((req http-request))
  (llog:info "Handling"))  ; Logger: "myapp.handle.http-request"

(defmethod handle :before ((req authenticated-request))
  (llog:debug "Auth check"))  ; Logger: "myapp.handle.before.authenticated-request"
```

**Hierarchy Rules:**

1. Start with package name
2. Add function/generic function name
3. For methods, add qualifiers (:before, :after, :around)
4. Add non-T specializers
5. For LABELS/FLET, add local function name

**Benefits:**

- **Granular Control:** Set levels per-function, per-method, per-specializer
- **Easy Filtering:** Turn off entire subsystems: `(llog:set-level "myapp.database" :off)`
- **Method-Specific Debugging:** `(llog:set-level "myapp.handle.after" :debug)`
- **Natural Organization:** Reflects code structure without manual naming

**API:**

```lisp
;; Automatic naming (default)
(llog:info "Message")  ; Uses context-derived name

;; Explicit logger name
(llog:info "myapp.custom.logger" "Message")

;; Get current logger name
(llog:current-logger-name)  ; => "myapp.my-function"

;; Set level by category pattern
(llog:set-level "myapp.api.*" :debug)  ; All API loggers
(llog:set-level "*.handle.before.*" :off)  ; All :before methods named 'handle'

;; Query logger hierarchy
(llog:list-loggers :pattern "myapp.api*")
;; => ("myapp.api" "myapp.api.handler" "myapp.api.auth")

;; Get parent logger
(llog:parent-logger "myapp.api.handler")  ; => "myapp.api"
```

**Implementation Notes:**

Use `sb-debug:*debug-name*` or similar implementation-specific APIs to determine the enclosing function/method at macro-expansion time.

---

## Technical Requirements

### TR-1: Thread Safety

**Priority:** P0 (Critical)

- All logging operations must be thread-safe
- Support concurrent logging from multiple threads
- Lock-free data structures where possible
- Configurable async vs sync logging

### TR-2: Memory Management

**Priority:** P0 (Critical)

- Bounded memory usage with configurable limits
- Buffer pooling to reduce GC pressure
- Explicit buffer size configuration
- Graceful degradation on memory pressure

### TR-3: Error Handling

**Priority:** P0 (Critical)

- Logging should never crash the application
- Graceful handling of I/O errors
- Configurable error handlers
- Failover to stderr on output failure

### TR-4: Portability

**Priority:** P1 (High)

**Supported Implementations:**
- SBCL (primary target)
- CCL
- ECL
- ABCL
- LispWorks (commercial)
- Allegro CL (commercial)

**OS Support:**
- Linux (primary)
- macOS
- Windows
- BSD variants

### TR-5: Dependencies

**Priority:** P1 (High)

**Minimize external dependencies:**
- Core: Zero required dependencies
- Optional: JSON library (for JSON encoder)
- Optional: Local-time (for timestamps)
- Optional: Bordeaux-threads (for threading)

**Design for dependency injection:**
- Users can provide their own JSON encoder
- Pluggable timestamp sources
- Optional components via ASDF features

### TR-6: Testing

**Priority:** P0 (Critical)

- Unit tests with > 90% coverage
- Performance benchmarks
- Stress tests for concurrency
- Compliance tests for output formats
- Memory leak detection

### TR-7: Documentation

**Priority:** P1 (High)

- Comprehensive API documentation
- Migration guides from other CL loggers
- Performance tuning guide
- Examples for common use cases
- Comparison with Go libraries

### TR-8: Editor Integration Support

**Priority:** P2 (Medium)

Design the API to support deep editor integration (Emacs/Vim/VSCode):

**Required APIs:**

```lisp
;; Enumerate all loggers and their levels
(llog:list-all-loggers)
;; => (("myapp" :debug) ("myapp.api" :info) ...)

;; Get logger hierarchy as tree
(llog:logger-tree &optional root)

;; Programmatic level changes with notifications
(llog:set-level "myapp.api" :debug
                :notify-fn (lambda (logger old-level new-level)
                            (format t "~A: ~A -> ~A" logger old-level new-level)))

;; Query logger configuration
(llog:logger-info "myapp.api")
;; => (:name "myapp.api" :level :info :effective-level :debug
;;     :parent "myapp" :children ("myapp.api.handler" "myapp.api.auth"))

;; Source location tracking
(llog:info "Message" :source-location (list :file "api.lisp" :line 42))
```

**Log Entry Metadata:**

All log entries should track:
- Source file and line number (when available)
- Logger category name
- Thread ID
- Timestamp

**Editor Feature Support:**

1. **Interactive Level Control:** Click on logger names to change levels
2. **Source Navigation:** Click on log messages to jump to source
3. **Visual Hierarchy:** Display logger tree with current levels
4. **Log Colorization:** Level-based syntax highlighting
5. **Quick Actions:** Context menus for common operations

**Future Extensions:**

- Emacs package (log4slime equivalent)
- VS Code extension
- Vim plugin
- REPL protocol for editor communication

---

## API Design Examples

### Basic Usage

```lisp
;;;; Quick Start
(ql:quickload :llog)

;; Create logger
(defvar *logger* (llog:make-logger))

;; Simple logging
(llog:info "Application started")
(llog:warn "Configuration file not found, using defaults")
(llog:error "Failed to connect to database" :error err)

;;;; Sugared API
(llog:info "User created"
  :user-id 12345
  :username "alice"
  :email "alice@example.com"
  :created-at (get-universal-time))

;;;; Typed API (zero allocations)
(llog:info-typed "Order processed"
  (llog:int "order-id" order-id)
  (llog:string "status" "completed")
  (llog:float "amount" 99.99)
  (llog:duration-ms "processing-time" elapsed))

;;;; Contextual Logging
(defun handle-request (request-id user-id)
  (let ((logger (llog:with-fields *logger*
                  :request-id request-id
                  :user-id user-id)))
    (llog:info logger "Request started")
    (process-request)
    (llog:info logger "Request completed")))

;;;; Configuration
(defvar *logger*
  (llog:make-logger
    :min-level :info
    :outputs (list
      (llog:make-file-output "/var/log/app.log"
        :encoder (llog:make-json-encoder)
        :buffer-size 8192)
      (llog:make-stream-output *standard-output*
        :encoder (llog:make-console-encoder :colors t)
        :min-level :warn))))

;;;; Structured Error Logging
(handler-case
  (/ 1 0)
  (error (e)
    (llog:error "Calculation failed"
      :error e
      :backtrace (llog:capture-backtrace)
      :input-value 1
      :operation 'division)))

;;;; Performance Critical Path
(defun hot-path (x y)
  ;; Compile-time elimination when LLOG-NO-DEBUG feature set
  #-llog-no-debug
  (llog:debug-typed "Hot path values"
    (llog:int "x" x)
    (llog:int "y" y))

  ;; Zero allocation logging
  (when (> x threshold)
    (llog:warn-typed "Threshold exceeded"
      (llog:int "value" x)
      (llog:int "threshold" threshold))))

;;;; Custom Fields
(llog:define-field-type point (x y)
  (:encoder (encoder stream field-name value)
    (llog:encode-object encoder stream field-name
      (list (cons "x" (point-x value))
            (cons "y" (point-y value))))))

(llog:info "Location updated"
  :position (llog:point "location" my-point))
```

### Advanced Usage

```lisp
;;;; Logger Hierarchy
(defvar *root-logger* (llog:make-logger :level :info))
(defvar *db-logger* (llog:child-logger *root-logger* :name "database"))
(defvar *api-logger* (llog:child-logger *root-logger* :name "api"))

;;;; Dynamic Contexts
(llog:with-context (:request-id (uuid:make-v4-uuid)
                    :tenant-id tenant-id)
  ;; All logs in this scope inherit context
  (handle-api-request))

;;;; Sampling
(dotimes (i 10000)
  ;; Only log 1% of iterations
  (llog:debug "Loop iteration" :i i :sample-rate 0.01))

;;;; Hooks
(llog:add-hook *logger* :error
  (lambda (entry)
    (sentry:report-error
      :message (llog:entry-message entry)
      :extra (llog:entry-fields entry))))

;;;; Custom Encoder
(llog:define-encoder my-custom-encoder ()
  ((format :reader encoder-format
           :initarg :format
           :initform :default))

  (:method encode-entry ((encoder my-custom-encoder) stream entry)
    (format stream "~A | ~A | ~A~%"
      (llog:entry-timestamp entry)
      (llog:entry-level entry)
      (llog:entry-message entry))))

;;;; Testing Support
(llog:with-captured-logs (logs :min-level :info)
  (my-function-that-logs)
  ;; Assert on logged entries
  (assert (= 3 (length logs)))
  (assert (equal :info (llog:entry-level (first logs)))))
```

---

## Performance Requirements

### Benchmarks

**Comparison Targets:**
- log4cl (existing CL logger)
- Go zap (industry standard)
- Go zerolog (zero allocation)

**Performance Criteria:**

| Operation | Target | Baseline (log4cl) |
|-----------|--------|-------------------|
| Typed log with 5 fields (no I/O) | < 100ns | ~2000ns |
| Sugared log with 5 fields (no I/O) | < 500ns | ~2500ns |
| JSON encoding (5 fields) | < 200ns | ~1000ns |
| Context creation (5 fields) | < 50ns | ~500ns |
| Allocations per typed log | 0-1 | 5-10 |
| Allocations per sugared log | 1-3 | 10-15 |

**Throughput Targets:**
- Single-threaded: > 1M logs/sec (without I/O)
- Multi-threaded (8 cores): > 5M logs/sec (without I/O)
- With JSON encoding: > 500K logs/sec
- With file I/O: Limited by disk throughput

---

## Success Metrics

### Adoption Metrics
- 100+ GitHub stars in first 6 months
- 10+ projects using in production within 1 year
- Listed in Awesome-CL list
- Mentioned in State of CL survey

### Performance Metrics
- 10x faster than log4cl for structured logging
- Performance within 2x of Go's zap library
- Zero allocations in typed API confirmed via benchmarks
- Memory usage < 1MB for typical application

### Quality Metrics
- Zero critical bugs in issue tracker
- > 90% test coverage
- All supported CL implementations passing CI
- Documentation completeness score > 85%

### Community Metrics
- 5+ external contributors
- 50+ GitHub issues/discussions
- Active responses to issues < 48 hours
- Community-contributed hooks/plugins

---

## Milestones and Timeline

### Phase 1: Core Infrastructure (Months 1-2)

**Deliverables:**
- [ ] Core logger protocol and structure
- [ ] Level-based filtering
- [ ] Basic field types (string, int, float)
- [ ] Memory-efficient entry structure
- [ ] Simple stream output

**Success Criteria:**
- Basic logging works on SBCL
- Unit tests pass
- Benchmark framework established

### Phase 2: Structured Logging (Months 2-3)

**Deliverables:**
- [ ] Complete field type system
- [ ] JSON encoder
- [ ] S-expression encoder
- [ ] Console encoder with colors
- [ ] Pattern layout encoder (FR-5)
- [ ] Contextual logging (with-fields)
- [ ] Sugared and typed APIs
- [ ] Hierarchical logger naming (FR-13)
- [ ] Conditional logging checks (FR-2)

**Success Criteria:**
- All encoders produce valid output
- Pattern layouts work with custom formats
- Automatic logger naming from package/function context
- Performance within 5x of targets
- Thread-safe logging confirmed

### Phase 3: Advanced Features (Months 3-4)

**Deliverables:**
- [ ] Hook system
- [ ] Multiple outputs (FR-8)
- [ ] Daily rolling file appenders (FR-8)
- [ ] Async logging with ring buffers
- [ ] Sampling and rate limiting
- [ ] Condition system integration
- [ ] Buffer pooling
- [ ] Expression logging macro (FR-11)
- [ ] Configuration save/restore (FR-12)

**Success Criteria:**
- Zero allocations in typed API path
- Daily log rotation working reliably
- Config save/restore persists across sessions
- Performance targets met
- Works on CCL and ECL

### Phase 4: Polish and Documentation (Month 5)

**Deliverables:**
- [ ] Comprehensive documentation
- [ ] Migration guide from log4cl
- [ ] Performance tuning guide
- [ ] Example applications
- [ ] Comparison benchmarks with Go libraries
- [ ] "Narrowing in reverse" workflow documentation (FR-12)
- [ ] Editor integration API documentation (TR-8)
- [ ] Logger hierarchy visualization tools

**Success Criteria:**
- Documentation completeness > 90%
- All examples run without errors
- Performance comparison published
- Migration guide covers common log4cl patterns
- Editor integration APIs are well-documented

### Phase 5: Community Release (Month 6)

**Deliverables:**
- [ ] Quicklisp submission
- [ ] Blog post announcement
- [ ] Presentation at CL event/meetup
- [ ] Reddit/HN launch
- [ ] Integration examples with popular CL frameworks

**Success Criteria:**
- Quicklisp accepted
- 50+ GitHub stars
- 3+ production users
- Positive community feedback

---

## Open Questions

1. **Naming:** Is "LLOG" the right name? Alternatives: cl-zap, fast-log, structured-log?

2. **JSON Library:** Bundle a minimal JSON encoder or depend on existing library (jonathan, jzon)?

3. **Timestamp Format:** ISO8601 strings vs Unix timestamps vs Universal Time? Make configurable?

4. **Global vs Local:** Should there be a global `*logger*` variable or always explicit?

5. **Lazy Evaluation:** Should field values be lazily evaluated to avoid computation when level filtered?

6. **Circular References:** How to handle circular structures in custom objects?

7. **Integration:** Should we provide built-in integrations with popular frameworks (Hunchentoot, Clack, etc.)?

8. **Async Default:** Should async logging be the default or opt-in?

9. **Hierarchical Naming Portability:** How to handle automatic logger naming across different CL implementations that may have different introspection capabilities?

10. **Logger Name Caching:** Should logger names be computed at macroexpansion time or runtime? Trade-offs between flexibility and performance?

11. **Configuration Storage:** Where to store saved configurations? Home directory, XDG_CONFIG_HOME, or configurable?

12. **Pattern vs Fixed Layouts:** Should pattern layouts be the primary interface with presets for JSON/console, or keep them separate?

---

## Appendix A: Comparison with Go Libraries

| Feature | zap | zerolog | logrus | log4cl | LLOG |
|---------|-----|---------|--------|--------|------|
| Zero-allocation | ✓ | ✓ | ✗ | ✗ | ✓ |
| Structured logging | ✓ | ✓ | ✓ | ✓ | ✓ |
| Leveled logging | ✓ | ✓ | ✓ | ✓ | ✓ |
| JSON output | ✓ | ✓ | ✓ | ✓ | ✓ |
| Dual API (typed/sugared) | ✓ | ✗ | ✗ | ✗ | ✓ |
| Hook system | ✗ | ✓ | ✓ | ✗ | ✓ |
| Sampling | ✓ | ✓ | ✗ | ✗ | ✓ |
| Context propagation | ✓ | ✓ | ✓ | ✓ | ✓ |
| Hierarchical loggers | ✗ | ✗ | ✗ | ✓ | ✓ |
| Auto logger naming | ✗ | ✗ | ✗ | ✓ | ✓ |
| Pattern layouts | ✗ | ✗ | ✗ | ✓ | ✓ |
| Config save/restore | ✗ | ✗ | ✗ | ✓ | ✓ |
| Daily log rotation | ✗ | ✗ | ✗ | ✓ | ✓ |
| Expression logging | ✗ | ✗ | ✗ | ✓ | ✓ |
| REPL integration | N/A | N/A | N/A | ✓ | ✓ |
| Editor integration | N/A | N/A | N/A | ✓ | ✓ |
| Condition system | N/A | N/A | N/A | ✗ | ✓ |
| Compile-time elim | ✗ | ✗ | ✗ | ✗ | ✓ |

---

## Appendix B: References

**Go Logging Libraries:**
- [uber-go/zap](https://github.com/uber-go/zap)
- [rs/zerolog](https://github.com/rs/zerolog)
- [sirupsen/logrus](https://github.com/sirupsen/logrus)

**Common Lisp Logging:**
- [log4cl](https://github.com/sharplispers/log4cl)
- [cl-syslog](https://github.com/mhsjlw/cl-syslog)
- [vom](https://github.com/orthecreedence/vom)

**Observability Standards:**
- OpenTelemetry Logging Specification
- Syslog RFC 5424
- JSON Logging Best Practices

---

## Appendix C: Example Output Formats

### JSON Output
```json
{
  "level": "error",
  "ts": "2025-10-13T08:04:23.123456Z",
  "logger": "api.handler",
  "msg": "Failed to process request",
  "request_id": "550e8400-e29b-41d4-a716-446655440000",
  "user_id": 12345,
  "error": "Database connection timeout",
  "error_type": "DB-TIMEOUT-ERROR",
  "duration_ms": 5000.234,
  "retry_count": 3,
  "stacktrace": "..."
}
```

### S-Expression Output
```lisp
(:level :error
 :ts 3912847463123456
 :logger "api.handler"
 :msg "Failed to process request"
 :request-id #<UUID 550e8400-e29b-41d4-a716-446655440000>
 :user-id 12345
 :error #<DB-TIMEOUT-ERROR>
 :error-type DB-TIMEOUT-ERROR
 :duration-ms 5000.234d0
 :retry-count 3
 :stacktrace "...")
```

### Colored Console Output
```
2025-10-13T08:04:23.123 [ERROR] api.handler: Failed to process request
  request_id: 550e8400-e29b-41d4-a716-446655440000
  user_id: 12345
  error: Database connection timeout (DB-TIMEOUT-ERROR)
  duration_ms: 5000.234
  retry_count: 3
```

---

**Document Status:** Ready for Review
**Next Steps:** Team review, stakeholder feedback, technical feasibility validation
