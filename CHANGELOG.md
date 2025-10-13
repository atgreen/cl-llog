# Changelog

All notable changes to LLOG will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added - Phase 3.1: Memory Management
- **Buffer pool system** with three-tier architecture (thread-local, global pool, on-demand)
- **Thread-local buffer caching** using weak-key hash table for automatic cleanup
- **Allocation benchmarks** in `benchmarks/allocation-bench.lisp`
- **Comprehensive buffer pool tests** (6 new test suites)
- **Buffer pool documentation** in `docs/buffer-pool.md`
- Support for `current-thread` from bordeaux-threads

### Performance - Phase 3.1
- **92% allocation reduction**: Typed API vs sugared API (25.69 KB → 2.04 KB per call)
- **94% allocation reduction**: File output with block buffering (1.51 KB per call)
- **333,333 logs/second** throughput on SBCL
- **>95% cache hit rate** for thread-local buffer cache
- **~3μs per log call** latency (typed API, no I/O)

### Fixed - Phase 2 Completion
- Thread-safe stream output with per-output locks
- Timestamp handling switched from `get-internal-real-time` to `get-universal-time`
- Exported missing log-entry symbols
- Fixed concurrent test timing issues with proper synchronization
- Fixed bordeaux-threads API usage (condition-notify)

### Added - Phase 2: Structured Logging
- **Async logging** with `make-async-output` and background worker thread
- **File output** with configurable buffering (:none, :line, :block)
- **Multiple output support** with fan-out to multiple sinks
- **Formal concurrency tests** (6 test suites covering all threading scenarios)
- **Thread safety** with per-logger and per-output locks
- **Contextual logging** with `with-fields` and `with-context`
- **Three encoders**: JSON, S-expression, colored console
- **Dual API**: Sugared (keyword args) and typed (explicit types)
- **Field types**: string, int, float, bool, timestamp, duration-ms, error-field

### Testing - Phase 2 Complete
- **477 checks, 100% pass rate**
- Test coverage for all encoders, APIs, concurrent operations, buffer pool
- Tests for async output, file output, thread safety
- Buffer pool operation tests (reuse, max-size, thread isolation, auto-resize)

### Added - Phase 1: Foundation
- Core logger protocol with levels (TRACE, DEBUG, INFO, WARN, ERROR, FATAL, PANIC)
- Log entry structure with efficient field representation
- Stream output with basic console encoder
- Package structure with proper symbol shadowing
- FiveAM test framework integration
- MIT license

## Release Strategy

### v0.1.0 - Foundation (Target: Q1 2025)
- Phases 1-2 complete
- Core logging functionality
- Basic performance optimizations
- Initial documentation

### v0.2.0 - Advanced Features (Target: Q2 2025)
- Phase 3 complete (condition integration, hooks, sampling)
- REPL helpers and templates
- Enhanced performance

### v1.0.0 - Production Ready (Target: Q3 2025)
- Phase 4 complete (comprehensive testing, benchmarks)
- Multi-implementation support (SBCL, CCL, ECL, ABCL)
- Complete documentation
- Quicklisp submission

## Version History

### [0.1.0-dev] - 2025-01-13
- Initial development release
- Phases 1-2 complete, Phase 3.1 complete
- 477 tests passing
- 92-94% allocation reduction achieved

---

## Notes

### Breaking Changes Policy
During 0.x development, breaking changes may occur between minor versions.
Starting from 1.0.0, semantic versioning will be strictly followed.

### Performance Tracking
Key metrics tracked across versions:
- Allocation per log call (bytes)
- Throughput (logs/second)
- Latency (microseconds)
- Test coverage (%)
- Cache hit rate (%)

### Benchmark Platform
Unless otherwise noted, all benchmarks run on:
- SBCL 2.5.9
- Linux x64
- Modern multi-core CPU
