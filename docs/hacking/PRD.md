# LLOG Product Requirements Document (PRD)

**Product:** LLOG - High-Performance Structured Logging for Common Lisp
**Version:** 1.0
**Status:** In Development (v0.1.0)
**Last Updated:** 2025-10-13

---

## Executive Summary

LLOG is a modern, high-performance structured logging framework for Common Lisp, inspired by best practices from the Go ecosystem (zap, zerolog, logrus) while maintaining REPL-friendly features from the Common Lisp tradition (log4cl).

**Current Implementation Status:** Phase 3 complete
- ✅ Phase 1: Foundation (100%)
- ✅ Phase 2: Structured Logging (100%)
- ✅ Phase 3: Advanced Features (100% - 7/7 sub-phases complete)
- 🚧 Phase 4: Quality and Performance (in progress)
- 📋 Phase 5: Release and Community (planned)

---

## Product Vision

**Mission:** Provide the fastest, most ergonomic structured logging solution for Common Lisp that doesn't compromise on performance or developer experience.

**Key Differentiators:**
1. **Dual API Design**: Ergonomic sugared API for convenience, zero-allocation typed API for performance
2. **First-Class Condition Integration**: Automatic backtrace capture, restart information, and condition chains
3. **Buffer Pool Architecture**: Thread-local caching achieving 92-94% allocation reduction
4. **Zero External Dependencies**: Core functionality requires only bordeaux-threads

---

## Target Users

### Primary Personas

**1. The Performance-Conscious Developer**
- Writing high-throughput services or real-time systems
- Needs logging that doesn't impact application performance
- Values allocation profiling and optimization capabilities
- **Key Need:** Zero-allocation logging in hot paths

**2. The Production Engineer**
- Running Common Lisp applications in production
- Needs structured logs for monitoring and debugging
- Values JSON output for log aggregation systems
- **Key Need:** Reliable, structured logging with condition capture

**3. The REPL-Driven Developer**
- Developing interactively in the REPL
- Needs immediate feedback and debugging information
- Values colored output and readable formatting
- **Key Need:** Ergonomic API with rich error information

---

## Current Feature Set

### Core Features (Implemented ✅)

#### FR-1: Dual API Design
**Status:** ✅ Complete
- Sugared API with automatic type inference
- Typed API with explicit field types
- Both APIs fully functional and tested

#### FR-2: Multiple Log Levels
**Status:** ✅ Complete
- TRACE, DEBUG, INFO, WARN, ERROR, FATAL, PANIC
- Per-logger level configuration
- Per-output level filtering

#### FR-3: Structured Logging
**Status:** ✅ Complete
- Key-value field pairs
- Type-preserving field system
- Support for: string, int, float, bool, timestamp, duration, error/condition

#### FR-4: Multiple Encoders
**Status:** ✅ Complete
- JSON encoder (machine-readable)
- Console encoder (human-readable with ANSI colors)
- S-expression encoder (Lisp-native)
- Extensible encoder protocol

#### FR-5: Multiple Outputs
**Status:** ✅ Complete
- Stream output (stdout, stderr, custom streams)
- File output with three buffering modes (:none, :line, :block)
- Multiple simultaneous outputs (fan-out)
- Per-output configuration

#### FR-6: Thread Safety
**Status:** ✅ Complete
- Thread-safe logger operations
- Per-output locks
- Concurrent logging tested
- 6 concurrency test suites

#### FR-7: Contextual Logging
**Status:** ✅ Complete
- `with-fields` for persistent context
- `with-context` macro for dynamic binding
- Field inheritance through logger hierarchy

#### FR-8: Buffer Pool System
**Status:** ✅ Complete
- Global buffer pool with configurable size
- Thread-local buffer caching
- >95% cache hit rate
- 92-94% allocation reduction vs sugared API

#### FR-9: Async Logging
**Status:** ✅ Complete
- Background worker thread
- Bounded queue with backpressure
- Graceful shutdown
- Wraps any output type

#### FR-10: Condition System Integration
**Status:** ✅ Complete
- Automatic backtrace capture (SBCL, CCL)
- Restart information extraction
- Condition chain traversal
- `error-field-detailed` constructor
- Full encoder integration

### Advanced Features

#### FR-11: Hook System
**Status:** ✅ Complete (Phase 3.5)
- Pre-log hooks for entry modification/filtering
- Post-log hooks for metrics and notifications
- Error hooks for logging error handling
- Priority-based execution (lower priority runs first)
- Error isolation (hook failures don't crash logging)
- Comprehensive API: add-hook, remove-hook, clear-hooks, list-hooks
- 10 example hooks (metrics, sampling, redaction, Sentry, etc.)

#### FR-12: Sampling and Rate Limiting
**Status:** ✅ Complete (Phase 3.6)
- Probabilistic sampling (0.0-1.0 sample rate)
- Deterministic sampling (every Nth entry)
- Token bucket rate limiting (per-second, per-minute, per-hour)
- Per-level configuration
- Statistics tracking and monitoring APIs
- Complete API: set-sampling, set-rate-limit, clear-*, get-*-stats, rate-limited-p
- 20 comprehensive tests
- 10 real-world examples in examples/sampling-examples.lisp

#### FR-13: REPL Integration
**Status:** ✅ Complete (Phase 3.7)
- Recent logs circular buffer with enable/disable control
- `show-recent` with filtering by level, logger name, and regex patterns (case-insensitive)
- `grep-logs` for searching log entries with regex support
- `with-captured-logs` macro for testing log output
- `define-field-type` macro for custom field types with validation/coercion
- Thread-safe circular buffer implementation
- Hook-based recording via post-log integration
- 17 comprehensive tests
- Full coverage of buffer operations, filtering, capture, and custom types

### Planned Features 📋

#### FR-14: Pattern Layout Encoder
**Status:** 📋 Planned (Phase 3.7)
- Configurable format strings
- Custom field ordering
- Template-based formatting

#### FR-15: Hierarchical Logger Naming
**Status:** 📋 Planned (Phase 3.7)
- Package-based logger naming
- Auto-detection of package/function/method
- Hierarchical filtering

---

## Non-Functional Requirements

### Performance Requirements

#### NFR-1: Throughput
**Target:** >300K logs/second (single-threaded, typed API, SBCL)
**Current:** ✅ 333K logs/second achieved

#### NFR-2: Allocation Reduction
**Target:** >90% reduction in typed API vs sugared API
**Current:** ✅ 92-94% achieved

#### NFR-3: Latency
**Target:** <5μs per log call (typed API, no I/O)
**Current:** ✅ ~3μs achieved

### Quality Requirements

#### NFR-4: Test Coverage
**Target:** >90% code coverage
**Current:** ✅ 100% pass rate (723/723 tests), coverage analysis pending

#### NFR-5: Code Quality
**Target:** Zero linting issues
**Current:** ✅ Zero issues (ocicl lint)

#### NFR-6: Portability
**Target:** Tests pass on SBCL, CCL, ECL, ABCL
**Current:** ⚠️ SBCL only (other implementations pending Phase 4)

### Usability Requirements

#### NFR-7: Documentation
**Target:** Complete API docs, user guide, examples
**Current:** ⚠️ Partial (README complete, full docs pending Phase 4)

#### NFR-8: Installation
**Target:** One-command install via Quicklisp
**Current:** 📋 Manual installation (Quicklisp submission pending Phase 5)

---

## Success Metrics

### Technical Metrics
- ✅ Throughput: 333K logs/second (target: >300K)
- ✅ Allocation reduction: 92-94% (target: >90%)
- ✅ Test pass rate: 100% (723/723 checks, target: 100%)
- ✅ Linting: 0 issues (target: 0)
- ⚠️ Code coverage: TBD (target: >90%)

### Adoption Metrics (Post-Launch)
- GitHub Stars: Target 50+
- Quicklisp Downloads: Target 100+/month
- Production Users: Target 3+
- Community Activity: Active issue tracker and PRs

---

## Release Plan

### v0.1.0 (Current)
- Core logging complete
- Buffer pool system
- Async logging
- Condition integration
- Hook system
- Sampling and rate limiting
- REPL integration
- Hierarchical loggers
- Pattern layouts
- 100% test pass rate (723/723)

### v1.0.0 (Target: Phase 5)
- All Phase 3 features complete
- Comprehensive documentation
- Multi-implementation testing
- Quicklisp submission
- Performance benchmarks vs log4cl

### v1.1.0 (Future)
- Additional encoders
- Advanced REPL features
- Extended condition analysis
- Performance optimizations

---

## Future Roadmap: Advanced Features

The following sections outline ambitious features beyond v1.0, organized by theme and priority. These represent the cutting edge of logging capabilities, drawing inspiration from modern observability practices, compliance requirements, and performance engineering.

### Phase 6: Production Integration (High Priority)

Production-grade integrations for enterprise observability stacks and compliance requirements.

#### FR-16: Production-Grade Network Sinks
**Priority:** High | **Effort:** Large | **Target:** v1.2.0

Native support for industry-standard log aggregation systems with production-ready reliability.

**Capabilities:**
- **Loki Integration**: Native Prometheus Loki HTTP API with label support
- **Journald Output**: Direct systemd journal integration for Linux deployments
- **Syslog Support**: RFC 5424 syslog protocol (TCP/UDP/TLS)
- **GELF Output**: Graylog Extended Log Format for Graylog/Elasticsearch

**Reliability Features:**
- Batching with configurable size/time windows
- Exponential backoff with jitter for retries
- Circuit breaker states (closed/open/half-open)
- Connection pooling and keepalive
- Metrics for monitoring sink health

**API Design:**
```lisp
;; Loki sink with labels
(llog:add-output *logger*
  (llog:make-loki-output "http://loki:3100"
    :labels '(:app "orders" :env "prod")
    :batch-size 1000
    :batch-timeout 5.0
    :retry-attempts 3))

;; Journald for systemd environments
(llog:add-output *logger*
  (llog:make-journald-output
    :identifier "my-app"
    :facility :user))

;; Syslog with TLS
(llog:add-output *logger*
  (llog:make-syslog-output "syslog.company.com"
    :port 6514
    :protocol :tls
    :facility :local0))

;; GELF for Graylog
(llog:add-output *logger*
  (llog:make-gelf-output "graylog:12201"
    :compression :gzip
    :additional-fields '(:datacenter "us-west")))
```

**Use Cases:**
- Kubernetes deployments shipping to Loki
- Linux services integrating with journald
- Enterprise syslog aggregation
- Graylog/ELK stack integration

**Success Metrics:**
- 99.9% delivery reliability under normal conditions
- <100ms p99 latency for batch operations
- Graceful degradation during network failures

---

#### FR-17: OpenTelemetry Integration
**Priority:** High | **Effort:** Large | **Target:** v1.2.0

First-class OpenTelemetry Logs support with automatic trace correlation.

**Capabilities:**
- OTLP/HTTP and OTLP/gRPC exporters
- Automatic trace-id/span-id propagation from context
- Resource and instrumentation scope attributes
- Severity number mapping
- Full OTLP LogRecord specification support

**Trace Correlation:**
- Automatic extraction from `*llog-context*`
- Manual trace context injection
- Span event emission as logs
- Distributed tracing breadcrumb trails

**API Design:**
```lisp
;; Configure OTLP exporter
(llog:add-output *logger*
  (llog:make-otlp-output
    :endpoint "https://otel-collector:4318"
    :protocol :http
    :headers '(("Authorization" . "Bearer token123"))
    :resource-attributes '(:service.name "my-service"
                           :service.version "1.0.0"
                           :deployment.environment "production")))

;; Automatic trace correlation
(let ((*llog-context*
        (llog:with-fields *logger*
          :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
          :span-id "00f067aa0ba902b7")))
  (llog:info "Processing request")
  ;; Trace/span IDs automatically attached to OTLP logs
  )

;; Emit span event as log
(llog:span-event "cache.hit"
  :attributes '(:cache.key "user:123" :latency-ms 5))
```

**Integration Benefits:**
- Unified observability (traces + logs)
- Jump from trace to logs in one click
- Correlate errors across distributed systems
- Vendor-neutral observability

**Success Metrics:**
- Zero trace-id loss during propagation
- <50ms p99 export latency
- Compatible with major OTLP collectors (OpenTelemetry, Grafana Agent, Datadog Agent)

---

#### FR-18: Redaction Policies
**Priority:** High | **Effort:** Medium | **Target:** v1.2.0

Centralized, auditable redaction for PII/PHI compliance (GDPR, HIPAA, CCPA).

**Capabilities:**
- **Key-Based Redaction**: Redact specific field keys
- **Regex Pattern Matching**: Detect credit cards, SSNs, emails, etc.
- **Redaction Strategies**: mask, hash, drop, tokenize
- **Policy Auditing**: Log which policy caused redaction
- **Performance**: Compiled regex + caching for hot paths

**API Design:**
```lisp
;; Define redaction policy
(llog:define-redaction-policy 'pii
  :keys '("password" "ssn" "credit-card" "api-key")
  :regexes '(("\\b\\d{16}\\b" . :credit-card)           ; Visa/MC/Amex
             ("\\b\\d{3}-\\d{2}-\\d{4}\\b" . :ssn)       ; SSN
             ("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b" . :email))
  :strategy :mask
  :mask-char #\*
  :preserve-length t
  :policy-id "PII-2025-01")

;; Apply policy to logger
(llog:set-redaction-policy *logger* 'pii)

;; Redaction happens automatically
(llog:info "User registered"
  :username "alice"
  :email "alice@example.com"     ; Redacted: a****@e******.***
  :password "hunter2"             ; Redacted: *******
  :ssn "123-45-6789")             ; Redacted: ***-**-****

;; Custom tokenization
(llog:define-redaction-policy 'tokenized-pii
  :keys '("ssn")
  :strategy :tokenize
  :tokenizer (lambda (value) (format nil "TOKEN-~A" (sxhash value))))

;; Audit redaction events
(llog:enable-redaction-audit *logger* "audit.log")
;; Logs: {"policy":"PII-2025-01","field":"ssn","strategy":"mask","timestamp":"..."}
```

**Compliance Features:**
- Policy versioning and change tracking
- Redaction audit trail
- Zero retention of raw sensitive data
- Deterministic hashing for correlation

**Use Cases:**
- GDPR compliance for EU data
- HIPAA compliance for healthcare logs
- PCI DSS compliance for payment systems
- Zero-trust security architectures

**Success Metrics:**
- 100% redaction accuracy (no leaks)
- <5% performance overhead
- Audit trail integrity

---

### Phase 7: Advanced Analysis & Debugging (Medium-High Priority)

Tools for deep debugging, performance analysis, and operational visibility.

#### FR-19: Chrome Trace / Perfetto Export
**Priority:** Medium-High | **Effort:** Medium | **Target:** v1.3.0

Export logs and spans as Chromium Trace Event format for flame graph visualization in chrome://tracing or Perfetto.

**Capabilities:**
- Automatic span duration tracking
- Begin/End event pairs with matching IDs
- Instant events and flow events
- Thread/process categorization
- Metadata and args support

**API Design:**
```lisp
;; Enable trace export
(llog:enable-trace-export *logger* "trace.json")

;; Span-style logging
(with-log-span (*logger* "db.query"
                 :id (uuid:make-v4-uuid)
                 :attrs '(:sql "SELECT * FROM users" :rows 1000))
  (llog:info "Fetching users")
  (query-database))
;; Emits: {"name":"db.query","ph":"B","ts":123,"tid":1,"args":{"sql":"..."}}
;;        {"name":"db.query","ph":"E","ts":456,"tid":1}

;; Batch export
(llog:flush-trace-export *logger*)
;; Creates trace.json viewable in chrome://tracing
```

**Visualization:**
- Timeline view of all operations
- Flame graph for nested spans
- Identify slow operations visually
- Correlate logs with performance

**Use Cases:**
- Performance profiling
- Request tracing in web services
- Batch processing analysis
- Concurrency debugging

**Tools:**
- chrome://tracing (built into Chrome)
- Perfetto UI (https://ui.perfetto.dev)
- SpeedScope (https://speedscope.app)

---

#### FR-20: Black Box Buffer
**Priority:** High | **Effort:** Medium | **Target:** v1.3.0

Per-thread ring buffers that keep recent DEBUG/TRACE logs in memory, automatically dumping on ERROR/FATAL.

**Capabilities:**
- Thread-local circular buffers
- Configurable size per thread
- Automatic dump on error conditions
- Manual dump for investigation
- Structured attachment to error logs

**API Design:**
```lisp
;; Enable black box recording
(llog:enable-blackbox *logger*
  :size 2000              ; Keep last 2000 entries per thread
  :levels '(:trace :debug) ; Only buffer trace/debug
  :trigger-levels '(:error :fatal :panic))

;; Normal logging - DEBUG entries buffered but not written
(llog:debug "Processing item" :id 1)
(llog:debug "Processing item" :id 2)
;; ... 2000 more debug logs ...

;; Error triggers automatic dump
(llog:error "Failed to process batch" :error err)
;; Automatically attaches last 2000 debug entries as:
;; "blackbox": [
;;   {"level":"debug","ts":"...","msg":"Processing item","id":1},
;;   ...
;; ]

;; Manual dump for investigation
(llog:dump-blackbox *logger* *standard-output*)

;; Query blackbox state
(llog:blackbox-stats *logger* :thread (bt:current-thread))
;; => (:size 2000 :used 347 :overwritten 0 :dumps 2)
```

**Use Cases:**
- Production debugging without verbose logging
- Root cause analysis with full context
- Intermittent bug investigation
- Cost-effective debug logging (memory-only until error)

**Performance:**
- Negligible overhead (memory write only)
- Thread-local = no locking
- Circular buffer = constant memory

---

#### FR-21: Enhanced REPL Integration & Emacs Mode
**Priority:** Medium | **Effort:** Medium | **Target:** v1.3.0

Deep REPL integration with live log tailing, filtering, and an Emacs minor mode for rich interaction.

**REPL Features:**
```lisp
;; Live tail with filtering
(llog:tail *logger*
  :follow t
  :filter '(and (>= level :warn)
                (field= :component "api")))
;; Prints logs in real-time, press Ctrl-C to stop

;; Interactive narrowing
(llog:tail *logger*
  :follow t
  :interactive t)
;; Press 'f' to add filter, 'l' to change level, 'q' to quit

;; Query recent logs
(llog:recent *logger* :last 100 :level :error)
;; Returns list of log-entry structures

;; Send selection to REPL for analysis
(let ((errors (llog:grep-logs *logger*
                :pattern "timeout"
                :since (- (get-universal-time) 3600))))
  (format t "Found ~D timeout errors in last hour~%" (length errors)))
```

**Emacs Minor Mode:**
- `M-x llog-mode` activates in REPL buffer
- Syntax highlighting for log levels (colors)
- `C-c l f` - Add filter expression
- `C-c l l` - Change log level
- `C-c l s` - Search logs
- `C-c l j` - Jump to source location (if :source-location enabled)
- `C-c l c` - Clear recent logs
- `C-c l e` - Export selection to file

**Installation:**
```elisp
;; ~/.emacs.d/init.el
(add-to-list 'load-path "~/quicklisp/local-projects/llog/emacs")
(require 'llog-mode)
(add-hook 'sly-mrepl-mode-hook #'llog-mode)
```

**Use Cases:**
- Interactive development with live log feedback
- REPL-driven debugging
- Quick log analysis without external tools
- Seamless editor integration

---

#### FR-22: llogctl CLI
**Priority:** Medium | **Effort:** Medium | **Target:** v1.3.0

Standalone command-line tool for offline log analysis, transformation, and audit verification.

**Capabilities:**
- **grep**: Filter logs by level, field, regex
- **convert**: Transform between formats (JSON ↔ S-expr ↔ mmap)
- **verify-audit**: Check Merkle chain integrity (FR-24)
- **stats**: Compute statistics (counts, percentiles, top-K)
- **coalesce**: Merge duplicate entries (FR-26)
- **export**: Convert to other formats (CSV, SQL, Parquet)

**Command Examples:**
```bash
# Filter warnings from API component
llogctl grep --level>=warn --field component=api app.json > api-warns.json

# Convert mmap to JSON
llogctl convert --input llog.wal --output out.json --format json

# Verify audit log integrity
llogctl verify-audit audit.log
# Output: ✓ 10,000 records verified, chain intact, checkpoint signature valid

# Statistics
llogctl stats app.json --group-by level
# Output:
# TRACE: 5,000,000 (50%)
# DEBUG: 3,000,000 (30%)
# INFO:  1,500,000 (15%)
# WARN:    400,000 (4%)
# ERROR:   100,000 (1%)

# Coalesce repeated messages
llogctl coalesce app.json --window 5s --key msg,component > deduped.json
# Reduced 1M entries to 10K (-99%)

# Export to CSV for Excel analysis
llogctl export app.json --format csv --fields ts,level,msg,user-id > logs.csv

# Top-K analysis
llogctl stats app.json --top 10 --by user-id
# user-id=123: 50,000 logs
# user-id=456: 45,000 logs
# ...
```

**Implementation:**
- Standalone binary (no Lisp runtime required)
- Streaming parser (handles multi-GB files)
- Parallel processing for large files
- Rich terminal output with progress bars

**Use Cases:**
- Post-mortem analysis
- Log archival and compression
- Compliance audit verification
- Cost analysis (identify noisy logs)

---

### Phase 8: High-Performance Extensions (Medium Priority)

Features focused on extreme performance, zero-overhead, and resource efficiency.

#### FR-23: Compiled Filter DSL
**Priority:** Medium | **Effort:** Medium | **Target:** v1.4.0

Small s-expression filter language that compiles to native predicates for zero-overhead filtering.

**Capabilities:**
- Compile-time filter compilation
- Runtime filter updates (with recompilation)
- Boolean logic (and, or, not)
- Level comparisons
- Field matching (equality, regex, ranges)
- Function calls for custom predicates

**API Design:**
```lisp
;; Define and compile filter
(llog:set-filter *logger*
  '(and (>= level :info)
        (not (field= :component "healthcheck"))
        (or (starts-with msg "Order")
            (field-matches :user-id "^admin-"))))

;; Filter compiles to:
;; (lambda (entry)
;;   (and (>= (log-entry-level entry) +info+)
;;        (not (equal (field-value entry :component) "healthcheck"))
;;        (or (string-prefix-p "Order" (log-entry-message entry))
;;            (ppcre:scan "^admin-" (field-value entry :user-id)))))

;; Clear filter
(llog:clear-filter *logger*)

;; Temporary filter override
(with-filter *logger* '(= level :debug)
  (llog:debug "This will be logged")
  (llog:info "This will NOT be logged"))

;; Pre-compile filters for hot paths
(defparameter *production-filter*
  (llog:compile-filter
    '(and (>= level :info)
          (not (field= :component "healthcheck")))))

(llog:set-filter *logger* *production-filter*)
```

**Filter DSL:**
```lisp
;; Level comparisons
(>= level :warn)
(< level :error)

;; Field operations
(field= :key "value")
(field-exists :key)
(field-null :key)
(field-in :status '("active" "pending"))
(field> :latency 1000)

;; String operations
(starts-with msg "prefix")
(ends-with msg "suffix")
(contains msg "substring")
(field-matches :email ".*@example\\.com")

;; Boolean logic
(and expr1 expr2 ...)
(or expr1 expr2 ...)
(not expr)

;; Custom predicates
(call #'my-predicate entry)
```

**Performance:**
- Compiled to native code (SBCL compiler)
- Zero runtime overhead (vs hand-written predicate)
- Filter changes trigger recompilation
- Cache compiled filters

**Use Cases:**
- Production filtering without performance cost
- Complex filtering logic
- Dynamic filter updates without restarts
- Per-environment filter configurations

---

#### FR-24: Mmap'd Append-Only Writer (WAL)
**Priority:** Medium | **Effort:** Large | **Target:** v1.4.0

Memory-mapped write-ahead log for extreme write performance with deferred encoding.

**Capabilities:**
- Memory-mapped file I/O
- Fsync policies (:never, :interval, :on-error, :always)
- Binary format (not human-readable)
- Separate replay tool converts to JSON/S-expr
- Multi-GB file support

**API Design:**
```lisp
;; Create mmap output
(llog:add-output *logger*
  (llog:make-mmap-output "llog.wal"
    :fsync :interval
    :fsync-interval-ms 100
    :initial-size (* 1024 1024 1024)  ; 1GB
    :max-size (* 10 1024 1024 1024))) ; 10GB

;; Logging is extremely fast (just memory write)
(time
  (dotimes (i 1000000)
    (llog:info "Event" :id i)))
;; => 0.5 seconds (2M logs/sec)

;; Convert WAL to JSON later
$ llogctl convert --input llog.wal --output logs.json --format json

;; Replay WAL programmatically
(llog:replay-wal "llog.wal"
  (lambda (entry)
    (when (>= (log-entry-level entry) +error+)
      (send-alert entry))))
```

**Fsync Policies:**
- `:never` - No fsync (fastest, risk of data loss)
- `:interval` - Fsync every N ms (balanced)
- `:on-error` - Fsync only ERROR+ (errors durable)
- `:always` - Fsync every write (slowest, most durable)

**Format:**
- Binary serialization (faster than JSON)
- Fixed-size record headers
- Variable-length payloads
- CRC32 checksums per record
- File format version for compatibility

**Use Cases:**
- Ultra-high-throughput logging (>1M logs/sec)
- Batch processing with deferred analysis
- Embedded systems with limited I/O
- Temporary local storage before shipping to remote

**Performance Targets:**
- >1M logs/second (SBCL, fsync :never)
- <1KB allocation per log
- Graceful degradation when mmap full

---

#### FR-25: Intent-Aware Sampling
**Priority:** Medium | **Effort:** Medium | **Target:** v1.4.0

Advanced sampling that goes beyond global rates to support per-key, burst protection, and multi-dimensional sampling.

**Current Sampling (FR-12):**
- Probabilistic: Sample X% of logs
- Deterministic: Sample every Nth log

**Enhanced Sampling:**
```lisp
;; Per-key sampling (e.g., by user-id)
(llog:enable-sampling *logger*
  :rules '((:level :debug
            :by :user-id
            :rate 0.01            ; 1% of debug logs
            :head-sampling 10)))  ; Always log first 10 per user

;; Burst protection for warnings
(llog:enable-sampling *logger*
  :rules '((:level :warn
            :burst 100            ; Allow first 100
            :per-seconds 10       ; Then 1 per 10 seconds
            :strategy :burst)))

;; Multi-dimensional sampling
(llog:enable-sampling *logger*
  :rules '((:level :info
            :when (field= :component "api")
            :by (:user-id :endpoint)
            :rate 0.1)))

;; Adaptive sampling based on volume
(llog:enable-adaptive-sampling *logger*
  :target-rate 1000  ; Target 1000 logs/sec
  :adjust-interval 60) ; Adjust every 60 seconds

;; Always-sample rules (exceptions)
(llog:add-sampling-rule *logger*
  '(:always :when (>= level :error)))

(llog:add-sampling-rule *logger*
  '(:always :when (field-matches :trace-id "debug-.*")))
```

**Sampling Strategies:**
- **Probabilistic**: Random sampling per event
- **Deterministic**: Every Nth event
- **Per-Key**: Sample rate per unique key value
- **Head Sampling**: Always log first N, then sample
- **Tail Sampling**: Decide after seeing outcome (complex traces)
- **Burst**: Allow burst, then throttle
- **Adaptive**: Adjust rate to maintain target throughput

**Use Cases:**
- High-cardinality logging (per-user debugging)
- Cost control with user-specific rates
- Retain errors while sampling success logs
- Debug-trace mode without flooding logs

**Monitoring:**
```lisp
(llog:get-sampling-stats *logger*)
;; => ((:rule "debug-by-user"
;;      :total 1000000
;;      :sampled 10000
;;      :dropped 990000
;;      :rate 0.01
;;      :cardinality 5000)  ; 5000 unique user-ids
;;     ...)
```

---

### Phase 9: Specialized Features (Lower Priority / Niche)

Advanced features for specific use cases: compliance, debugging, performance analysis.

#### FR-26: Tamper-Evident Audit Trails (Merkle Chain)
**Priority:** Medium | **Effort:** Large | **Target:** v1.5.0

Cryptographic append-only audit logs with hash chaining and periodic signed checkpoints for compliance and forensics.

**Important:** This feature makes logs **tamper-evident**, not tamper-proof. It detects unauthorized modifications after the fact but does not prevent them. An attacker with filesystem access can still modify logs, but the hash chain will reveal the tampering during verification. True tamper-proofing requires additional measures like write-once media, immediate external replication, or hardware security modules.

**Capabilities:**
- SHA-256 hash chaining (each record hashes previous)
- Periodic checkpoints with digital signatures
- Verification tool detects tampering
- Append-only guarantees
- Compliance-ready (SOC2, ISO 27001, etc.)

**API Design:**
```lisp
;; Create audit output
(llog:add-output *logger*
  (llog:make-audit-output "audit.log"
    :hash-algorithm :sha256
    :checkpoint-every 1000  ; Sign every 1000 records
    :signer *pgp-signer*    ; Or :keypair for Ed25519
    :metadata '(:system "payment-service"
                :version "1.0"
                :jurisdiction "EU")))

;; Normal logging
(llog:info "Payment processed"
  :user-id 123
  :amount 99.99
  :transaction-id "txn-abc")

;; Verify audit log integrity
(llog:verify-audit "audit.log")
;; => (:status :valid
;;     :records 10000
;;     :checkpoints 10
;;     :chain-intact t
;;     :signatures-valid t
;;     :first-timestamp "2025-10-13T10:00:00Z"
;;     :last-timestamp "2025-10-13T11:30:00Z")

;; Detect tampering
;; If someone modifies record #5000:
(llog:verify-audit "audit.log")
;; => (:status :tampered
;;     :tampered-record 5001  ; Detection at next record
;;     :expected-hash "abc123..."
;;     :actual-hash "def456...")

;; Export for compliance officer
(llog:export-audit "audit.log" "audit-report.pdf"
  :format :pdf
  :include-checksums t
  :include-signatures t)
```

**File Format:**
```
[Header: version, hash-algo, checkpoint-interval, metadata]
[Record 1: timestamp, level, message, fields, hash(header)]
[Record 2: timestamp, level, message, fields, hash(record1)]
...
[Record 1000: ...]
[Checkpoint 1: timestamp, record-count, merkle-root, signature]
[Record 1001: ..., hash(checkpoint1)]
...
```

**Verification:**
- Forward hash chain verification
- Checkpoint signature validation
- Merkle tree root verification
- Timestamp monotonicity check

**Use Cases:**
- Financial transaction logs (SOX compliance)
- Healthcare access logs (HIPAA)
- Security audit trails (SOC2)
- Legal evidence preservation
- Forensic investigation

**Compliance Benefits:**
- Tamper detection
- Non-repudiation (signatures)
- Append-only guarantees
- Export to compliance formats

---

#### FR-27: Chrome-Style Rate Coalescing / Deduplication
**Priority:** Low | **Effort:** Medium | **Target:** v1.5.0

Automatically collapse repeated messages into single entries with occurrence counts and time windows.

**Capabilities:**
- Coalesce identical messages
- Custom key functions (by message + fields)
- Time-window based coalescing
- Count and timestamp tracking
- Automatic emission after window closes

**API Design:**
```lisp
;; Enable coalescing
(with-log-coalescing (*logger*
                      :window 5.0  ; 5 second window
                      :key (lambda (entry)
                             (values (log-entry-message entry)
                                     (field-value entry :host))))
  ;; Repeated logs within 5 seconds are coalesced
  (dotimes (i 100)
    (llog:warn "Retrying connection" :host "db-1")
    (sleep 0.01)))

;; Emits single log:
;; {"level":"warn","msg":"Retrying connection","host":"db-1",
;;  "count":100,"window":"5s","first_seen":"...","last_seen":"..."}

;; Global coalescing
(llog:enable-coalescing *logger*
  :window 5.0
  :key (lambda (entry)
         (values (log-entry-message entry)
                 (log-entry-level entry))))

;; Fine-grained control
(llog:enable-coalescing *logger*
  :window 5.0
  :max-count 1000  ; Force emit after 1000 occurrences
  :strategy :sliding-window  ; vs :fixed-window
  :key-fn #'my-key-extractor)
```

**Strategies:**
- **Fixed Window**: Emit at end of fixed time windows
- **Sliding Window**: Emit N seconds after first occurrence
- **Hybrid**: Emit on count threshold OR time threshold

**Use Cases:**
- Retry loops (collapse 1000 retries into 1 log)
- Health check logs (reduce noise)
- Rate-limited API warnings
- Repeated error conditions

**Benefits:**
- 90-99% log volume reduction for repeated messages
- Storage cost reduction
- Improved signal-to-noise ratio
- Easier log analysis

---

#### FR-28: Slow-Sink Detector & Adaptive Routing
**Priority:** Low | **Effort:** Medium | **Target:** v1.5.0

Detect underperforming outputs and automatically route logs to fallback sinks or apply backpressure.

**Capabilities:**
- Per-output latency monitoring
- Threshold-based degradation detection
- Automatic fallback routing
- Adaptive level adjustment
- Metrics and alerting

**API Design:**
```lisp
;; Enable sink health monitoring
(llog:enable-sink-healthchecks *logger*
  :threshold-ms 50       ; Flag if write takes >50ms
  :window 60             ; Over 60 second window
  :unhealthy-percent 0.1 ; 10% of writes exceed threshold
  :on-degrade :drop-debug)  ; Drop DEBUG logs if degraded

;; Advanced routing
(llog:enable-sink-healthchecks *logger*
  :threshold-ms 50
  :on-degrade :fallback
  :fallback-output (llog:make-file-output "/tmp/fallback.log"))

;; Custom degradation handler
(llog:enable-sink-healthchecks *logger*
  :threshold-ms 50
  :on-degrade (lambda (logger output stats)
                (llog:warn "Sink degraded" :output output :stats stats)
                ;; Maybe page ops? Switch to backup?
                ))

;; Query sink health
(llog:sink-health *logger*)
;; => ((:output #<FILE-OUTPUT>
;;      :status :healthy
;;      :p50-latency-ms 2
;;      :p99-latency-ms 15
;;      :error-rate 0.0)
;;     (:output #<LOKI-OUTPUT>
;;      :status :degraded
;;      :p50-latency-ms 80
;;      :p99-latency-ms 500
;;      :error-rate 0.02))
```

**Degradation Actions:**
- `:drop-debug` - Drop DEBUG logs, keep WARN+
- `:drop-info` - Drop DEBUG+INFO, keep WARN+
- `:fallback` - Route to fallback output
- `:buffer` - Buffer in memory (risky)
- `:alert` - Call alerting function
- `:circuit-break` - Stop sending, retry later

**Use Cases:**
- Network sink failures (Loki down)
- Disk full scenarios
- Slow database logging
- Prevent logging from crashing app

**Monitoring:**
```lisp
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (let ((health (llog:sink-health logger)))
      (when (some (lambda (h) (eq (getf h :status) :degraded)) health)
        (send-alert "Logging sinks degraded" health)))))
```

---

#### FR-29: Message Templates with Named Holes (Serilog-Style)
**Priority:** Low | **Effort:** Medium | **Target:** v1.5.0

Pre-compiled message templates with named placeholders for faster formatting and better structure.

**Capabilities:**
- Compile-time template parsing
- Named placeholder extraction
- Automatic field generation
- Reduced string allocations
- Localization support (i18n)

**API Design:**
```lisp
;; Define and compile templates
(llog:deftemplate user-purchase
  "User {user} purchased {qty} {item} for {price}"
  :fields '(:user :qty :item :price))

;; Use template
(llog:info user-purchase
  :user "alice"
  :qty 2
  :item "widgets"
  :price 19.99)
;; Message: "User alice purchased 2 widgets for 19.99"
;; Fields: {:user "alice" :qty 2 :item "widgets" :price 19.99}

;; Compile at runtime
(defparameter *payment-template*
  (llog:compile-template
    "Payment {status} for order {order-id} amount {amount}"))

(llog:info *payment-template*
  :status "approved"
  :order-id "ORD-123"
  :amount 99.99)

;; Localization support
(llog:deftemplate user-login
  "User {user} logged in"
  :locale :en)

(llog:deftemplate user-login
  "Utilisateur {user} connecté"
  :locale :fr)

(let ((*locale* :fr))
  (llog:info user-login :user "alice"))
;; Message: "Utilisateur alice connecté"

;; Type hints for better encoding
(llog:deftemplate order-shipped
  "Order {order-id:string} shipped at {shipped-at:timestamp} weight {weight:float}kg"
  :types '(:order-id :string
           :shipped-at :timestamp
           :weight :float))
```

**Benefits:**
- 30-50% faster than `format` for common templates
- Fewer allocations (pre-compiled structure)
- Automatic field extraction for structured logging
- Type hints for better encoding
- I18n support for global apps

**Use Cases:**
- High-volume templated logging
- Multi-language applications
- Consistent message formats
- Performance-critical paths

---

#### FR-30: Field Schemas & Validation
**Priority:** Low | **Effort:** Medium | **Target:** v1.6.0

Define contracts for log fields to catch errors early and enable compact encodings.

**Capabilities:**
- Declare field types and constraints
- Runtime validation (optional)
- Compile-time checks (when possible)
- Compact binary encodings (CBOR, Protocol Buffers)
- Auto-generate field documentation

**API Design:**
```lisp
;; Define schema
(llog:defschema user-event
  (:strict t  ; Reject unknown fields
   :validate t) ; Validate at runtime
  (:user-id :type :int :required t :range (1 . 999999999))
  (:username :type :string :required t :max-length 50)
  (:email :type :string :required nil :format :email)
  (:ip :type :string :required nil :format :ipv4)
  (:age :type :int :required nil :range (0 . 150)))

;; Use schema
(llog:info "User registered"
  :schema 'user-event
  :user-id 123
  :username "alice"
  :email "alice@example.com"
  :ip "192.168.1.1"
  :age 30)
;; ✓ Valid

;; Validation errors
(llog:info "User registered"
  :schema 'user-event
  :user-id "not-an-int"  ; Wrong type
  :username "alice")
;; ERROR: Schema validation failed for user-event:
;;   Field :user-id: expected :int, got "not-an-int"
;;   Missing required field :user-id

;; Compile-time checks (typed API)
(llog:info-typed "User registered"
  (llog:schema 'user-event
    (llog:int "user-id" 123)
    (llog:string "username" "alice")
    (llog:string "email" "alice@example.com")))

;; Compact encoding with schemas
(llog:add-output *logger*
  (llog:make-cbor-output "logs.cbor"
    :use-schemas t))  ; CBOR encodes field names as integers

;; Auto-generate docs
(llog:schema-to-markdown 'user-event "docs/schemas.md")
```

**Benefits:**
- Catch bugs before logs reach production
- Self-documenting field contracts
- Compact binary formats (50-80% smaller)
- API compatibility checking

**Use Cases:**
- Strict schema enforcement
- Binary log formats (CBOR, Protobuf)
- Cross-team logging contracts
- Log format evolution

---

#### FR-31: Transaction Logs with Rollback/Commit
**Priority:** Low | **Effort:** Medium | **Target:** v1.6.0

Buffer logs within a scope and emit only on commit, discarding on rollback/error.

**Capabilities:**
- Transactional log scoping
- Manual commit/rollback
- Automatic rollback on unwind
- Nested transactions
- Conditional emission

**API Design:**
```lisp
;; Basic transaction
(llog:with-log-transaction (*logger*)
  (llog:debug "Step 1: Validating input")
  (llog:debug "Step 2: Processing")
  (if (success-p)
      (llog:commit-logs)    ; Emit all logs
      (llog:rollback-logs)))  ; Discard all logs

;; Automatic rollback on error
(llog:with-log-transaction (*logger* :auto-commit nil)
  (llog:debug "Attempting payment")
  (process-payment order)
  (llog:debug "Payment successful")
  (llog:commit-logs))
;; If process-payment signals error, logs are discarded

;; Nested transactions
(llog:with-log-transaction (*logger*)
  (llog:debug "Outer: Starting batch")
  (dotimes (i 10)
    (llog:with-log-transaction (*logger*)
      (llog:debug "Inner: Processing item" :id i)
      (when (process-item i)
        (llog:commit-logs))))  ; Commit inner
  (llog:commit-logs))  ; Commit outer

;; Conditional emission
(llog:with-log-transaction (*logger*
                            :commit-on :error
                            :level :debug)
  ;; Only emit DEBUG logs if an error occurs
  (llog:debug "Step 1")
  (llog:debug "Step 2")
  (llog:error "Failed!"))  ; Triggers commit of all DEBUG logs
```

**Use Cases:**
- Noisy retry loops (only log if all retries fail)
- Speculative operations (only log if chosen path)
- Batch processing (only log failed batches)
- Reduce log noise in success cases

**Performance:**
- Buffered in memory (very fast)
- Zero I/O until commit
- Discarded logs = zero cost

---

#### FR-32: Condition/Restart Timeline Capture
**Priority:** Low | **Effort:** Medium | **Target:** v1.6.0

Capture the full restart decision tree as structured breadcrumbs during condition handling.

**Capabilities:**
- Record invoke-restart events
- Tie restarts to originating condition
- Timeline visualization
- Interactive replay in REPL
- Debugging complex condition flows

**API Design:**
```lisp
;; Enable restart timeline capture
(llog:with-condition-timeline (*logger*)
  (handler-bind
      ((network-error
        (lambda (err)
          (llog:info "Network error, invoking RETRY restart")
          (invoke-restart 'retry))))
    (with-retries (3)
      (fetch-data))))

;; Logs structured timeline:
;; {"level":"warn","msg":"Condition signaled",
;;  "condition":{"type":"NETWORK-ERROR","message":"Connection timeout"},
;;  "restarts":["RETRY","USE-CACHED-VALUE","ABORT"],
;;  "timeline-id":"uuid-123"}
;;
;; {"level":"info","msg":"Restart invoked",
;;  "restart":"RETRY","timeline-id":"uuid-123"}
;;
;; ... retry happens ...
;;
;; {"level":"info","msg":"Condition resolved",
;;  "timeline-id":"uuid-123","outcome":"success"}

;; Query timeline
(llog:get-condition-timeline "uuid-123")
;; => ((:event :condition-signaled
;;      :condition #<NETWORK-ERROR>
;;      :restarts (RETRY USE-CACHED-VALUE ABORT))
;;     (:event :restart-invoked
;;      :restart RETRY)
;;     (:event :condition-signaled  ; Second attempt
;;      :condition #<NETWORK-ERROR>
;;      :restarts ...)
;;     (:event :restart-invoked
;;      :restart RETRY)
;;     (:event :condition-resolved
;;      :outcome :success))

;; Visualize in REPL
(llog:visualize-timeline "uuid-123")
;; Output:
;; NETWORK-ERROR (attempt 1)
;; ├─ RETRY → NETWORK-ERROR (attempt 2)
;; │  └─ RETRY → Success
;; Available restarts: [RETRY] [USE-CACHED-VALUE] [ABORT]
```

**Use Cases:**
- Debugging complex restart hierarchies
- Understanding retry behavior
- Root cause analysis for failed conditions
- Interactive condition debugging

**Lisp-Specific:**
- Unique to Common Lisp condition system
- Showcases advanced condition handling
- Useful for teaching restarts

---

## Prioritization Summary

### Must-Have for Adoption (v1.2.0)
1. **FR-16**: Production sinks (Loki, journald, syslog, GELF)
2. **FR-17**: OpenTelemetry integration
3. **FR-18**: Redaction policies

### High Value for Debugging (v1.3.0)
4. **FR-20**: Black box buffer
5. **FR-21**: Enhanced REPL + Emacs mode
6. **FR-22**: llogctl CLI

### Performance Engineering (v1.4.0)
7. **FR-19**: Chrome Trace / Perfetto export
8. **FR-23**: Compiled filter DSL
9. **FR-24**: Mmap'd WAL
10. **FR-25**: Intent-aware sampling

### Specialized/Niche (v1.5.0+)
11. **FR-26**: Tamper-evident audit trails
12. **FR-27**: Rate coalescing
13. **FR-28**: Slow-sink detector
14. **FR-29**: Message templates
15. **FR-30**: Field schemas
16. **FR-31**: Transaction logs
17. **FR-32**: Condition/restart timeline

---

## Competitive Analysis

### vs log4cl
**Advantages:**
- ✅ 10-100x faster (typed API)
- ✅ 92-94% less allocation
- ✅ First-class condition integration
- ✅ Buffer pool architecture

**Trade-offs:**
- ⚠️ Hierarchical naming (planned, not yet implemented)
- ⚠️ Pattern layouts (planned, not yet implemented)

### vs Go Libraries (zap, zerolog)
**Parity:**
- ✅ Dual API design (like zap)
- ✅ Zero-allocation path (near-zero: 92-94% reduction)
- ✅ Structured logging
- ✅ Buffer pooling

**Advantages:**
- ✅ REPL integration (planned)
- ✅ Condition system integration (unique to CL)
- ✅ S-expression encoder (Lisp-native)

---

## Risk Assessment

### Technical Risks

**Risk:** Portability issues across implementations
**Mitigation:** ✅ Bordeaux-threads abstraction, implementation-specific features documented
**Status:** Low (foundation in place, testing pending)

**Risk:** Zero-allocation proving impossible
**Mitigation:** ✅ Focus on minimal allocations (92-94% achieved)
**Status:** ✅ Resolved (target exceeded)

**Risk:** Performance targets too ambitious
**Mitigation:** ✅ Early benchmarking, iterative optimization
**Status:** ✅ Resolved (targets met/exceeded)

### Adoption Risks

**Risk:** Community adoption challenges
**Mitigation:** Excellent documentation, easy migration, real-world examples, performance demonstrations
**Status:** ⚠️ Documentation in progress

**Risk:** Quicklisp acceptance delays
**Mitigation:** Follow submission guidelines, clean git history, proper ASDF system
**Status:** 📋 Planned for Phase 5

---

## Dependencies

### Runtime Dependencies
- **bordeaux-threads** (required for threading)
- **fiveam** (development/testing only)

### Development Tools
- SBCL (primary development platform)
- ocicl (linting and dependency management)
- git (version control)

---

## Open Questions

1. **Pattern Layout Priority:** How critical is pattern layout encoder for v1.0?
   - **Status:** Deferred to Phase 3.7 (not blocking v1.0)

2. **Hierarchical Naming:** Required for v1.0 or acceptable for v1.1?
   - **Status:** Planned for Phase 3.7 (nice-to-have for v1.0)

3. **Multi-Implementation Testing:** Which implementations are must-have vs nice-to-have?
   - **Status:** SBCL required, CCL high priority, ECL/ABCL medium priority

---

## Changelog

### 2025-10-14
- Updated status: Phase 3.7 (REPL Integration) complete
- Updated status: Phase 3 complete (100% - 7/7 sub-phases)
- Added FR-13: REPL Integration implementation details
- Updated metrics: 100% test pass rate (723/723 checks)
- REPL features: show-recent, grep-logs, with-captured-logs, define-field-type
- Thread-safe circular buffer for recent logs with hook-based recording
- Case-insensitive regex pattern matching via cl-ppcre
- Custom field type system with validation and coercion
- 17 comprehensive tests in test-repl.lisp
- Added dependency: cl-ppcre for regex support
- Updated README with lowercase "llog" branding
- Added comprehensive REPL Integration documentation section
- Updated comparison table: REPL integration marked as ✓
- Clarified FR-26: tamper-evident vs tamper-proof distinction

### 2025-10-13 (Evening)
- Updated status: Phase 3.6 (Sampling and Rate Limiting) complete
- Added FR-12: Sampling and Rate Limiting implementation details
- Updated metrics: 97% test pass rate (561/574, +30 tests)
- Updated Phase 3 progress: 6/7 sub-phases complete (86%)
- Sampling features: probabilistic (0.0-1.0) and deterministic (every Nth)
- Rate limiting features: token bucket with per-second/minute/hour configuration
- 20 comprehensive tests in test-sampling.lisp and test-rate-limiting.lisp
- 10 real-world examples in examples/sampling-examples.lisp

### 2025-10-13 (Later)
- Updated status: Phase 3.5 (Hooks and Extensibility) complete
- Added FR-11: Hook System implementation details
- Updated metrics: 100% test pass rate (544/544, +10 tests)
- Updated Phase 3 progress: 5/7 sub-phases complete (71%)
- Hook system features: pre-log, post-log, error hooks with priority and isolation
- 10 example hooks created in examples/hooks.lisp

### 2025-10-13
- Updated status: Phase 3.1 (Memory Management) complete
- Updated status: Phase 3.4 (Condition Integration) complete
- Added FR-10: Condition System Integration
- Updated metrics: 100% test pass rate (534/534)
- Updated metrics: Zero linting issues
- Updated allocation reduction: 92-94% achieved

### 2025-10-13 (Earlier)
- Updated status: Phase 3.3 (Async Logging) complete
- Updated status: Phase 2 complete with formal concurrency tests
- Added NFR-5: Code quality requirement

### 2025-10-12
- Updated status: Phase 3.2 (File Output) complete
- Added buffer pool performance metrics
- Updated throughput: 333K logs/second

### 2025-10-11
- Initial PRD draft
- Phase 1 and 2 complete
- Core feature set defined

---

**Document Owner:** LLOG Development Team
**Review Cycle:** After each phase completion
**Next Review:** Phase 4 completion
