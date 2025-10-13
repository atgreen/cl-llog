# LLOG Implementation Plan

**Project:** LLOG - Best-in-Class Logging Framework for Common Lisp
**Timeline:** 6 months
**Last Updated:** 2025-10-13
**Current Status:** Phase 2 Complete, Phase 3 In Progress (Async logging landed)

---

## Overview

This document provides a detailed, phase-by-phase plan for implementing LLOG. Each phase builds on the previous one, with clear deliverables and success criteria.

---

## Phase 1: Foundation (Weeks 1-4)

**Goal:** Establish core infrastructure and basic logging capability

### 1.1 Project Setup (Week 1)

**Tasks:**
- [ ] Set up project structure (ASDF system, directory layout, README)
- [ ] Create initial directory structure:
  ```
  llog/
  ├── src/
  │   ├── package.lisp
  │   ├── logger.lisp
  │   ├── levels.lisp
  │   ├── entry.lisp
  │   ├── fields.lisp
  │   ├── encoders/
  │   ├── outputs/
  │   └── api.lisp
  ├── tests/
  │   ├── package.lisp
  │   └── test-*.lisp
  ├── benchmarks/
  ├── examples/
  ├── docs/
  ├── llog.asd
  ├── llog-tests.asd
  └── README.md
  ```
- [x] Initialize git repository
- [x] Set up initial ASDF system definition
- [x] Create package definitions

**Deliverables:**
- Loadable ASDF system
- Basic project documentation

### 1.2 Core Abstractions (Week 2)

**Tasks:**
- [x] Design core logger protocol and class hierarchy
  - `logger` class with slots: name, level, outputs, context-fields
  - Protocol methods: `log-entry`, `should-log-p`, `add-output`, etc.
- [x] Implement log levels and level filtering system
  - Define constants: `+trace+`, `+debug+`, `+info+`, `+warn+`, `+error+`, `+fatal+`, `+panic+`
  - Level comparison and filtering functions
  - Per-logger level configuration
- [x] Create log entry structure (efficient, minimal allocation)
  - Struct or class with slots: level, timestamp, message, fields, logger-name
  - Consider using structure for performance (fixed layout, faster access)
  - Optimize for minimal allocation during creation

**Design Considerations:**
```lisp
;; Logger structure
(defclass logger ()
  ((name :initarg :name :reader logger-name)
   (level :initarg :level :accessor logger-level)
   (outputs :initarg :outputs :accessor logger-outputs)
   (context-fields :initform nil :accessor logger-context-fields)
   (hooks :initform nil :accessor logger-hooks)))

;; Entry structure (use struct for performance)
(defstruct (log-entry (:constructor make-log-entry-internal))
  (level 0 :type fixnum)
  (timestamp 0 :type unsigned-byte)
  (message "" :type string)
  (logger-name "" :type string)
  (fields nil :type list))

;; Level constants
(defconstant +trace+ 0)
(defconstant +debug+ 1)
(defconstant +info+ 2)
(defconstant +warn+ 3)
(defconstant +error+ 4)
(defconstant +fatal+ 5)
(defconstant +panic+ 6)
```

**Deliverables:**
- Working logger class
- Level system with filtering
- Log entry representation

### 1.3 Basic Fields (Week 3)

**Tasks:**
- [x] Implement basic field types (string, int, float, boolean, timestamp)
  - Create field representation (name + value + type)
  - Implement type-specific constructors
- [x] Build field type protocol for extensibility
  - Generic function `encode-field` for custom types
  - Protocol for field serialization

**Design:**
```lisp
;; Field representation
(defstruct field
  (name "" :type string)
  (value nil)
  (type nil :type symbol))

;; Field constructors for typed API
(defun string-field (name value)
  (make-field :name name :value value :type :string))

(defun int-field (name value)
  (make-field :name name :value value :type :int))

;; Generic protocol
(defgeneric encode-field (encoder stream field)
  (:documentation "Encode a field to stream using encoder"))
```

**Deliverables:**
- Field type system
- Basic field constructors
- Extensibility protocol

### 1.4 Simple Output (Week 4)

**Tasks:**
- [x] Implement simple stream output (stdout/stderr)
  - `stream-output` class
  - Basic `write-entry` method
- [x] Implement console encoder (human-readable, plain text)
  - Simple text format: `[LEVEL] message field1=value1 field2=value2`

**Design:**
```lisp
(defclass output ()
  ((encoder :initarg :encoder :reader output-encoder)
   (min-level :initarg :min-level :initform +trace+ :reader output-min-level)))

(defclass stream-output (output)
  ((stream :initarg :stream :reader output-stream)))

(defgeneric write-entry (output entry)
  (:documentation "Write a log entry to the output"))
```

**Deliverables:**
- Working stream output
- Basic console encoder
- End-to-end logging: `(llog:info "Hello")` prints to stdout

**Success Criteria for Phase 1:** ✓ **COMPLETE**
- ✓ Can create a logger
- ✓ Can log simple messages to stdout
- ✓ Levels work and filter correctly
- ✓ Basic fields can be attached
- ✓ Unit tests pass on SBCL (45 tests, 100% pass rate)
- ✓ FiveAM test framework integrated
- ✓ Comprehensive test coverage for Phase 1 features

---

## Phase 2: Structured Logging (Weeks 5-8)

**Goal:** Complete structured logging with multiple encoders and APIs

### 2.1 Sugared API (Week 5)

**Tasks:**
- [x] Create sugared API macros (info, warn, error, etc.)
  - Macros that accept message + keyword arguments
  - Automatic field construction from keyword args
  - Optimize for common cases

**Design:**
```lisp
(defmacro info (message &rest keyword-args)
  "Log at INFO level with keyword arguments converted to fields"
  (let ((fields (loop for (key value) on keyword-args by #'cddr
                     collect `(make-field
                               :name ,(string-downcase (symbol-name key))
                               :value ,value
                               :type (infer-type ,value)))))
    `(when (should-log-p *logger* +info+)
       (log-entry *logger*
                  (make-log-entry-internal
                   :level +info+
                   :message ,message
                   :fields (list ,@fields))))))

;; Usage: (llog:info "User login" :user-id 123 :username "alice")
```

**Deliverables:**
- Complete sugared API (trace, debug, info, warn, error, fatal, panic)
- Automatic type inference
- Keyword argument handling

### 2.2 Typed API (Week 5)

**Tasks:**
- [x] Create typed API macros (info-typed, warn-typed, etc.)
  - Explicit field type functions
  - Zero-allocation optimization

**Design:**
```lisp
;; Field constructor functions
(declaim (inline string int float bool timestamp))

(defun string (name value)
  (declare (optimize speed (safety 0))
           (type string name value))
  (make-field :name name :value value :type :string))

(defun int (name value)
  (declare (optimize speed (safety 0))
           (type string name)
           (type integer value))
  (make-field :name name :value value :type :int))

;; Typed logging macro
(defmacro info-typed (message &rest field-forms)
  `(when (should-log-p *logger* +info+)
     (log-entry *logger*
                (make-log-entry-internal
                 :level +info+
                 :message ,message
                 :fields (list ,@field-forms)))))

;; Usage: (llog:info-typed "User login"
;;          (llog:int "user-id" 123)
;;          (llog:string "username" "alice"))
```

**Deliverables:**
- Complete typed API
- Inline declarations for zero allocation
- Performance comparable to manual logging

### 2.3 JSON Encoder (Week 6)

**Tasks:**
- [x] Implement JSON encoder with proper escaping
  - Manual JSON writing (no library dependencies)
  - Proper string escaping and quoting
  - Type-specific encoding
  - Number formatting (CL to JSON notation)
- [ ] Zero-allocation optimizations (deferred to Phase 3)

**Design:**
```lisp
(defclass json-encoder (encoder)
  ((pretty :initarg :pretty :initform nil :reader encoder-pretty-p)
   (timestamp-format :initarg :timestamp-format :initform :iso8601)))

(defmethod encode-entry ((encoder json-encoder) stream entry)
  (write-char #\{ stream)
  (write-string "\"level\":\"" stream)
  (write-string (level-name (log-entry-level entry)) stream)
  (write-string "\",\"ts\":\"" stream)
  (encode-timestamp stream (log-entry-timestamp entry) encoder)
  (write-string "\",\"msg\":\"" stream)
  (write-json-escaped-string stream (log-entry-message entry))
  (write-char #\" stream)
  ;; Encode fields
  (dolist (field (log-entry-fields entry))
    (write-char #\, stream)
    (encode-field encoder stream field))
  (write-char #\} stream)
  (write-char #\Newline stream))
```

**Deliverables:**
- JSON encoder producing valid JSON
- Proper escaping and formatting
- Performance benchmarks

### 2.4 Additional Encoders (Week 6)

**Tasks:**
- [x] Implement S-expression encoder
  - Native Lisp format (plist-based)
  - Can be read back with `read`
  - Type preservation for all field types
- [x] Implement colored console encoder (ANSI colors for REPL)
  - ANSI escape codes for colors
  - Level-specific colors
  - Pretty formatting with indentation
  - Field display with proper alignment

**Deliverables:**
- ✓ S-expression encoder (make-sexpr-encoder)
- ✓ Colored console encoder (make-console-encoder)
- ✓ Encoder selection API

### 2.5 Contextual Logging (Week 7)

**Tasks:**
- [x] Build contextual logging system (with-fields, child loggers)
  - `with-fields` function to add persistent fields
  - `child-logger` function for hierarchical loggers
  - Efficient field inheritance (shared lock, copied fields)

**Design:**
```lisp
(defun with-fields (logger &rest keyword-args)
  "Create a child logger with additional context fields"
  (let ((child (copy-logger logger)))
    (setf (logger-context-fields child)
          (append (logger-context-fields logger)
                  (keywords-to-fields keyword-args)))
    child))

;; Dynamic context
(defmacro with-context ((&rest keyword-args) &body body)
  "Execute body with additional context in *logger*"
  `(let ((*logger* (with-fields *logger* ,@keyword-args)))
     ,@body))

;; Usage:
;; (let ((logger (llog:with-fields *logger*
;;                 :request-id (uuid:make-v4-uuid))))
;;   (llog:info logger "Processing request"))
```

**Deliverables:**
- `with-fields` function
- `with-context` macro
- Field inheritance working

### 2.6 Thread Safety (Week 8)

**Tasks:**
- [x] Implement thread-safe logging with locks
  - Protect shared state (logger config, outputs)
  - Per-logger locks using bordeaux-threads
  - Thread-safe set-level, add-output, remove-output
  - Thread-safe log-entry with snapshot of outputs/context
- [ ] Lock-free optimizations (deferred to Phase 3)
- [ ] Per-thread buffers (deferred to Phase 3)

**Design Considerations:**
- Using `bt:with-lock-held` for protecting logger state
- Lock-free algorithms deferred to Phase 3
- Thread-local storage for buffers deferred to Phase 3

**Deliverables:**
- ✓ Thread-safe logger operations
- ✓ Concurrent logging tested (manual verification)
- ✓ Formal concurrency tests (test-concurrency.lisp with 6 test suites)
- [ ] Performance benchmarks under contention (deferred to Phase 4)

**Success Criteria for Phase 2:** ✓ **COMPLETE**
- ✓ Both sugared and typed APIs work
- ✓ JSON, S-expr, and console encoders produce correct output
- ✓ Context fields propagate correctly
- ✓ Thread-safe logging confirmed (using locks)
- ✓ Stream outputs protected with per-output locks
- ✓ Formal concurrency tests implemented (467 checks, 100% pass rate)
- [ ] Performance benchmarks (deferred to Phase 4)

---

## Phase 3: Advanced Features (Weeks 9-12)

**Goal:** Add performance optimizations and advanced capabilities

### 3.1 Memory Management (Week 9)

**Tasks:**
- [ ] Create buffer pool system for memory efficiency
  - Pre-allocated buffer pool
  - Thread-local buffer caches
  - Automatic size management
- [ ] Optimize for zero allocations in typed API path
  - Stack allocation where possible
  - Reuse buffers
  - Profile and eliminate allocations

**Design:**
```lisp
(defvar *buffer-pool* (make-buffer-pool :size 32 :buffer-size 8192))

(defstruct buffer-pool
  (buffers nil :type list)
  (lock (bt:make-lock))
  (buffer-size 8192 :type fixnum))

(defun acquire-buffer (pool)
  (bt:with-lock-held ((buffer-pool-lock pool))
    (or (pop (buffer-pool-buffers pool))
        (make-array (buffer-pool-buffer-size pool)
                    :element-type 'character
                    :fill-pointer 0))))

(defun release-buffer (pool buffer)
  (setf (fill-pointer buffer) 0)
  (bt:with-lock-held ((buffer-pool-lock pool))
    (push buffer (buffer-pool-buffers pool))))
```

**Deliverables:**
- Buffer pool implementation
- Zero-allocation typed API
- Allocation profiling results

### 3.2 File and Multiple Outputs (Week 9)

**Tasks:**
- [x] Implement file output with buffering strategies
  - Unbuffered (:none), line-buffered (:line), block-buffered (:block) modes
  - Automatic flushing based on mode
  - Automatic directory creation
  - Error handling for I/O failures
  - Per-file locks for thread safety
- [x] Build multiple output support (fan-out to multiple sinks)
  - Parallel writes to multiple outputs (via logger-outputs list)
  - Per-output level filtering (output-min-level)
  - Error isolation (handler-case around each output)
  - Generic flush-output and close-output methods

**Deliverables:**
- ✓ File output with buffering (make-file-output)
- ✓ Multi-output support (add-output, remove-output)
- ✓ Per-output level filtering

### 3.3 Async Logging (Week 10)

**Status:** ✅ Complete

**Implemented:**
- `make-async-output` wraps any output with a bounded queue (default 1024 entries)
- Background worker thread drains entries using bordeaux-threads condition variables
- Backpressure handled by blocking producers when queue is full
- Graceful shutdown via `close-output`, ensuring buffers flush before the worker joins
- Tests cover basic async fan-out (`tests/test-outputs.lisp`) and flush semantics

**Follow-ups:**
- [ ] Throughput benchmarks under contention (planned for Phase 4)
- [ ] Drop/delay metrics for observability (optional enhancement)

### 3.4 Condition Integration (Week 10)

**Tasks:**
- [ ] Add condition system integration (auto-capture conditions, backtraces)
  - Automatic condition field extraction
  - Backtrace capture (SBCL, CCL, etc.) with graceful fallback
  - Restart information and active restart list
  - Condition chain traversal / nested causes
  - Tests covering SBCL + CCL behaviour

**Design:**
```lisp
(defmethod encode-field ((encoder t) stream (field field))
  (when (subtypep (type-of (field-value field)) 'condition)
    ;; Special handling for conditions
    (let ((condition (field-value field)))
      (encode-object encoder stream field
        (list (cons "type" (type-of condition))
              (cons "message" (princ-to-string condition))
              (cons "backtrace" (capture-backtrace condition)))))))

(defun capture-backtrace (condition)
  #+sbcl (sb-debug:print-backtrace :stream nil :count 20)
  #+ccl (ccl:print-call-history :detailed t :count 20)
  #-(or sbcl ccl) "Backtrace not available")
```

**Deliverables:**
- Condition field type
- Backtrace capture
- Restart information extraction

### 3.5 Hooks and Extensibility (Week 11)

**Tasks:**
- [ ] Implement hook system (pre-log, post-log, error hooks)
  - Hook registration API (:pre-log, :post-log, :error)
  - Hook execution with error isolation + logging of failures
  - Example hooks (Sentry, metrics counter)
  - Middleware support for field transforms

**Design:**
```lisp
(defclass hook ()
  ((type :initarg :type :reader hook-type)
   (function :initarg :function :reader hook-function)
   (min-level :initarg :min-level :reader hook-min-level)))

(defmethod execute-hooks ((logger logger) (entry log-entry) hook-type)
  (dolist (hook (logger-hooks logger))
    (when (and (eq (hook-type hook) hook-type)
               (>= (log-entry-level entry) (hook-min-level hook)))
      (handler-case
          (funcall (hook-function hook) entry)
        (error (e)
          (format *error-output* "Hook error: ~A~%" e))))))

;; Usage:
;; (llog:add-hook *logger* :post-log
;;   (lambda (entry)
;;     (when (>= (log-entry-level entry) +error+)
;;       (send-to-sentry entry)))
;;   :min-level +error+)
```

**Deliverables:**
- Hook system
- Example hooks (Sentry integration, etc.)

### 3.6 Sampling and Rate Limiting (Week 11)

**Tasks:**
- [ ] Build sampling support (sample-rate parameter)
  - Probabilistic sampling
  - Deterministic sampling (every Nth)
- [ ] Implement rate limiting functionality
  - Token bucket algorithm
  - Per-logger and per-call-site limits
  - Configuration knobs via API and REPL helpers

**Deliverables:**
- Sampling implementation
- Rate limiting
- Configuration API

### 3.7 Advanced API Features (Week 12)

**Tasks:**
- [ ] Implement custom field type protocol and define-field-type macro
- [ ] Create log template system (define-log-template macro)
- [ ] Create compile-time log elimination (reader conditionals, features)
- [ ] Add REPL integration helpers (show-recent, grep-logs, with-captured-logs)
- [ ] Pattern layout encoder (configurable format directives)
- [ ] Hierarchical logger naming & filters (auto detection of package/function/method)
- [ ] Configuration save/restore & interactive config tree

**Deliverables:**
- Custom field types working
- Log template system
- Compile-time elimination
- REPL helpers

**Success Criteria for Phase 3:**
- Zero allocations achieved in typed API
- Async logging working with high throughput
- Hooks and extensibility demonstrated
- All advanced features functional
- Pattern encoder, hierarchical naming, and config persistence available

---

## Phase 4: Quality and Performance (Weeks 13-16)

**Goal:** Achieve production-ready quality and performance

### 4.1 Comprehensive Testing (Weeks 13-14)

**Tasks:**
- [ ] Build comprehensive unit tests (target >90% coverage)
  - Test all API functions
  - Edge cases and error conditions
  - Mock outputs for verification
- [ ] Create performance benchmarks (vs log4cl, measure allocations)
  - Benchmark suite with multiple scenarios
  - Allocation counting
  - Comparison with log4cl, cl-log
- [ ] Add concurrency stress tests (multi-threaded logging)
  - Heavy concurrent load tests
  - Race condition detection
  - Memory leak tests

**Deliverables:**
- >90% test coverage
- Benchmark suite
- Stress tests passing

### 4.2 Performance Optimization (Week 14)

**Tasks:**
- [ ] Profile and optimize hot paths for performance targets
  - SBCL profiling (sb-sprof)
  - Identify bottlenecks
  - Optimize critical paths
- [ ] Add error handling and graceful degradation
  - Handle all I/O errors
  - Fallback to stderr on failures
  - Never crash the application

**Deliverables:**
- Performance targets met
- Robust error handling
- Profiling reports

### 4.3 Portability (Week 15)

**Tasks:**
- [ ] Test portability across CL implementations
  - SBCL (primary)
  - CCL
  - ECL
  - ABCL
  - LispWorks (if available)
- [ ] Implement CI pipeline (test on SBCL, CCL, ECL, ABCL)
  - GitHub Actions workflow
  - Test matrix for implementations
  - Automated benchmarks

**Deliverables:**
- Tests passing on SBCL, CCL, ECL, ABCL
- CI pipeline running
- Portability issues documented

### 4.4 Documentation (Week 16)

**Tasks:**
- [ ] Write API reference documentation
  - Docstrings for all public APIs
  - Generated API docs
- [ ] Create user guide with examples
  - Getting started
  - Common patterns
  - Best practices
- [ ] Write migration guide from log4cl
- [ ] Create performance tuning guide
- [ ] Build example applications demonstrating usage patterns
  - Include pattern encoder example, condition logging demo, rolling files

**Deliverables:**
- Complete documentation
- Examples for all major features
- Migration guide

**Success Criteria for Phase 4:**
- >90% test coverage achieved
- Performance targets met or exceeded
- Tests pass on 4+ CL implementations
- Complete documentation

---

## Phase 5: Release and Community (Weeks 17-24)

**Goal:** Public release and community adoption

### 5.1 Prepare Release (Weeks 17-18)

**Tasks:**
- [ ] Final API review and stabilization
- [ ] Prepare Quicklisp submission
  - Ensure ASDF system is correct
  - License file (MIT)
  - Clean git history
- [ ] Create release notes
- [ ] Tag version 1.0.0

**Deliverables:**
- Stable 1.0.0 release
- Quicklisp submission ready

### 5.2 Community Outreach (Weeks 19-20)

**Tasks:**
- [ ] Write announcement blog post
- [ ] Submit to Quicklisp
- [ ] Post to /r/lisp, /r/Common_Lisp
- [ ] Share on Hacker News
- [ ] Present at online Lisp meetup
- [ ] Create comparison benchmarks vs Go libraries

**Deliverables:**
- Public announcement
- Community engagement
- Benchmark comparisons published

### 5.3 Integration Examples (Weeks 21-22)

**Tasks:**
- [ ] Integration with Hunchentoot
- [ ] Integration with Clack
- [ ] Integration with cl-async
- [ ] Integration with SBCL's native threads
- [ ] Example: structured logging in web service
- [ ] Example: high-performance logging in computation
- [ ] Example: daily rolling file output configuration
- [ ] Example: pattern layout configuration in production/service

**Deliverables:**
- Integration guides
- Example applications
- Framework-specific helpers

### 5.4 Iteration and Improvement (Weeks 23-24)

**Tasks:**
- [ ] Gather community feedback
- [ ] Fix reported bugs
- [ ] Consider feature requests
- [ ] Improve documentation based on questions
- [ ] Release patch versions as needed

**Deliverables:**
- Responsive to community
- Bug fixes released
- Growing adoption

**Success Criteria for Phase 5:**
- Quicklisp acceptance
- 50+ GitHub stars
- 3+ production users
- Positive community feedback
- Active issue tracker

---

## Key Technical Decisions

### 1. Core Data Structures

**Decision: Use structs for log entries, classes for loggers**

Rationale:
- Structs have fixed layouts → faster field access
- Log entries are immutable after creation → struct is perfect
- Loggers are mutable and need extensibility → class is appropriate

### 2. Field Representation

**Decision: Tagged union approach with type field**

```lisp
(defstruct field
  name    ; string
  value   ; any
  type)   ; symbol indicating type
```

Rationale:
- Simple and fast
- Easy to extend
- Encoder can dispatch on type
- Small memory footprint

### 3. Threading Model

**Decision: Lock-based for logger state, lock-free for hot path**

Rationale:
- Logger configuration is infrequent → locks acceptable
- Log writing is hot path → lock-free ring buffer
- Thread-local buffers eliminate most contention

### 4. Buffer Management

**Decision: Pool of reusable character arrays**

Rationale:
- Reduces GC pressure
- Thread-local caches eliminate contention
- Arrays are fast in all implementations

### 5. Async Implementation

**Decision: Single background thread with lock-free ring buffer**

Rationale:
- Simple and maintainable
- Lock-free ring buffer gives good performance
- Single writer avoids coordination overhead
- Can extend to multiple threads later if needed

### 6. JSON Encoding

**Decision: Manual JSON writing, no external dependencies**

Rationale:
- Full control over performance
- Zero allocations possible
- No dependency on JSON library
- Can optimize for common cases

### 7. Timestamp Handling

**Decision: Use `get-internal-real-time` by default, allow custom sources**

Rationale:
- Portable across implementations
- Fast (no system call on most implementations)
- Sufficient precision for logging
- Can plug in local-time for ISO8601

---

## Risk Mitigation

### Risk 1: Performance Targets Too Ambitious

**Mitigation:**
- Early benchmarking in Phase 1
- Iterative optimization in Phase 4
- Fallback: Adjust targets based on empirical data
- Go libraries benefit from compiler optimizations; CL has different strengths

### Risk 2: Portability Issues

**Mitigation:**
- Test on multiple implementations early
- Use Bordeaux-threads for threading abstraction
- Document implementation-specific features
- Provide implementation-specific optimizations where needed

### Risk 3: Zero-Allocation Proving Impossible

**Mitigation:**
- Focus on minimal allocations rather than absolute zero
- Use compiler-specific features (SBCL's dynamic-extent)
- Document allocation characteristics clearly
- "Near-zero" allocations may be acceptable

### Risk 4: Community Adoption Challenges

**Mitigation:**
- Excellent documentation
- Easy migration from log4cl
- Real-world examples
- Active community engagement
- Performance benchmarks to demonstrate value

### Risk 5: Scope Creep

**Mitigation:**
- Stick to PRD features
- Defer nice-to-have features to v1.1
- Clear phase boundaries
- Regular scope review

---

## Dependencies

### Required for Development
- SBCL (primary development platform)
- Quicklisp (for dev dependencies)
- FiveAM or Parachute (testing)
- ASDF (build system)

### Optional Runtime Dependencies
- Bordeaux-threads (threading abstraction)
- Local-time (ISO8601 timestamps)
- Implementation-specific: SBCL, CCL, ECL, ABCL

### Zero Required Dependencies for Core
The core logging functionality should work with zero external dependencies.

---

## Measurement and Metrics

### Performance Metrics (Tracked Weekly)

1. **Latency:**
   - Sugared API latency (ns per call, no I/O)
   - Typed API latency (ns per call, no I/O)
   - With JSON encoding
   - With file I/O

2. **Allocations:**
   - Bytes allocated per log call
   - Number of allocations per call
   - Total heap usage

3. **Throughput:**
   - Logs per second (single-threaded)
   - Logs per second (multi-threaded, 8 cores)
   - With async logging

### Quality Metrics (Tracked Weekly)

1. **Test Coverage:** Target >90%
2. **Open Issues:** Track count and severity
3. **Documentation Coverage:** % of public API documented
4. **Portability:** # of implementations passing tests

### Community Metrics (Post-Launch)

1. **GitHub Stars**
2. **Quicklisp Downloads**
3. **Issue/PR Activity**
4. **Community Contributions**

---

## Next Steps

### Immediate Actions (Week 1)

1. Set up project structure
2. Initialize git repository
3. Create initial ASDF system
4. Define package structure
5. Start Phase 1.1 tasks

### Review Points

- **End of Phase 1:** Core infrastructure review
- **End of Phase 2:** API design review
- **End of Phase 3:** Feature completeness review
- **End of Phase 4:** Release readiness review

---

## Appendix: Code Organization

### Suggested Module Structure

```lisp
;; package.lisp - Package definitions
(defpackage #:llog
  (:use #:cl)
  (:export
   ;; Core API
   #:make-logger #:*logger*
   ;; Sugared API
   #:trace #:debug #:info #:warn #:error #:fatal #:panic
   ;; Typed API
   #:trace-typed #:debug-typed #:info-typed
   ;; Field constructors
   #:string #:int #:float #:bool #:timestamp
   ;; Context
   #:with-fields #:with-context
   ;; Configuration
   #:make-json-encoder #:make-console-encoder
   #:make-file-output #:make-stream-output))

;; levels.lisp - Log level definitions
;; entry.lisp - Log entry structure
;; fields.lisp - Field types and protocol
;; logger.lisp - Logger class and core logic
;; api.lisp - Public API macros
;; encoders/json.lisp - JSON encoder
;; encoders/console.lisp - Console encoder
;; encoders/sexpr.lisp - S-expression encoder
;; outputs/stream.lisp - Stream output
;; outputs/file.lisp - File output
;; outputs/async.lisp - Async wrapper
;; utils.lisp - Utilities
;; conditions.lisp - Condition integration
;; hooks.lisp - Hook system
```

---

**Document Status:** Ready for Implementation
**Next Review:** End of Week 4 (Phase 1 completion)
