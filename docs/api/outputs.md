# Outputs API

Output destinations for log entries. LLOG supports stream output (stdout/stderr), file output with configurable buffering, and async output for background writing.

## Table of Contents

- [Stream Output](#stream-output)
- [File Output](#file-output)
  - [Buffering Modes](#buffering-modes)
- [Async Output](#async-output)
- [Base Output Protocol](#base-output-protocol)

## Stream Output

Write log entries to any Common Lisp stream (typically `*standard-output*` or `*error-output*`).

### `make-stream-output`

```lisp
(make-stream-output stream &key encoder min-level)
```

Create an output that writes encoded log entries to a stream.

**Arguments:**
- `stream` - A Common Lisp output stream
- `encoder` - Encoder instance (default: console encoder)
- `min-level` - Minimum log level (default: `+trace+`)

**Returns:** A `stream-output` instance

**Example:**
```lisp
;; Console output (default)
(llog:make-stream-output *standard-output*)

;; Error stream output
(llog:make-stream-output *error-output*
  :encoder (llog:make-console-encoder :colors nil)
  :min-level :error)

;; JSON to stdout
(llog:make-stream-output *standard-output*
  :encoder (llog:make-json-encoder))

;; Custom stream
(with-open-file (stream "debug.log"
                        :direction :output
                        :if-exists :append)
  (let ((output (llog:make-stream-output stream)))
    (llog:write-entry output entry)))
```

**Thread-Safety:** Safe if the underlying stream is thread-safe. Most Common Lisp implementations provide thread-safe standard streams.

---

## File Output

Write log entries to a file with configurable buffering strategies for performance tuning.

### `make-file-output`

```lisp
(make-file-output path &key encoder min-level buffer-mode buffer-size)
```

Create an output that appends encoded log entries to a file.

**Arguments:**
- `path` - File path (string or pathname)
- `encoder` - Encoder instance (default: JSON encoder)
- `min-level` - Minimum log level (default: `+trace+`)
- `buffer-mode` - Buffering strategy: `:none`, `:line`, or `:block` (default: `:line`)
- `buffer-size` - Buffer size in bytes for `:block` mode (default: `8192`)

**Returns:** A `file-output` instance

**Example:**
```lisp
;; Basic file output (line buffering)
(llog:make-file-output "app.log")

;; JSON with custom encoder
(llog:make-file-output "structured.log"
  :encoder (llog:make-json-encoder))

;; Only log errors
(llog:make-file-output "error.log"
  :min-level :error)

;; High-throughput: block buffering
(llog:make-file-output "high-volume.log"
  :buffer-mode :block
  :buffer-size 16384)

;; No buffering: immediate flush
(llog:make-file-output "critical.log"
  :buffer-mode :none)
```

**Thread-Safety:** Yes - internal locking protects file stream and buffer state

**File Creation:** Automatically creates parent directories if they don't exist

---

### Buffering Modes

File output supports three buffering strategies to balance performance vs data durability:

#### `:none` - No Buffering

```lisp
(llog:make-file-output "audit.log" :buffer-mode :none)
```

**Behavior:**
- Every log entry is immediately written to disk with `force-output`
- Guarantees maximum data durability
- Slowest performance

**Use Cases:**
- Critical audit logs
- System logs where data loss is unacceptable
- Low-volume logging where performance doesn't matter
- Debugging when you need immediate visibility

**Example:**
```lisp
;; Audit log - never lose an entry
(defvar *audit-logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "audit.log"
                :buffer-mode :none
                :encoder (llog:make-json-encoder)))))
```

---

#### `:line` - Line Buffering (Default)

```lisp
(llog:make-file-output "app.log" :buffer-mode :line)
```

**Behavior:**
- Writes are buffered in memory
- Flushes to disk after each newline character
- Good balance of performance and durability
- Most log entries end with newlines, so this flushes per-entry

**Use Cases:**
- General application logging (default choice)
- Development logging
- Production logs where occasional data loss is acceptable
- Structured logs (JSON, S-expr) where each entry is one line

**Example:**
```lisp
;; Standard app logging (default behavior)
(defvar *app-logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "app.log"
                :encoder (llog:make-json-encoder)))))
;; :buffer-mode defaults to :line
```

---

#### `:block` - Block Buffering

```lisp
(llog:make-file-output "high-volume.log"
  :buffer-mode :block
  :buffer-size 16384)
```

**Behavior:**
- Writes are buffered in memory
- Flushes to disk only when buffer reaches `buffer-size` bytes
- Maximum performance, lowest I/O overhead
- Data may be lost if process crashes before flush

**Use Cases:**
- High-throughput logging (millions of logs/second)
- Debug logging in production
- Trace-level logging
- Scenarios where recent log loss is acceptable

**Buffer Sizing:**
- Default: 8192 bytes (8KB)
- Larger buffers = fewer disk writes = better performance
- Larger buffers = more data loss on crash
- Common sizes: 8KB (default), 16KB, 32KB, 64KB

**Example:**
```lisp
;; High-volume debug logging
(defvar *debug-logger*
  (llog:make-logger
    :level :debug
    :outputs (list
              (llog:make-file-output "debug.log"
                :buffer-mode :block
                :buffer-size 32768))))  ; 32KB buffer

;; Force flush periodically
(defun periodic-flush-task ()
  (loop
    (sleep 5)  ; Every 5 seconds
    (llog:flush-output (first (llog:logger-outputs *debug-logger*)))))
```

**Explicit Flushing:**
You can manually flush block-buffered output:

```lisp
(let ((file-output (llog:make-file-output "data.log"
                                          :buffer-mode :block)))
  ;; Log entries (buffered)
  (dotimes (i 1000)
    (llog:info "Record" :id i))

  ;; Force flush to disk
  (llog:flush-output file-output))
```

---

### Performance Comparison

Based on typical workloads:

| Mode     | Throughput       | Latency (p99) | Data Loss Risk |
|----------|------------------|---------------|----------------|
| `:none`  | ~50K logs/sec    | ~50μs         | None           |
| `:line`  | ~200K logs/sec   | ~10μs         | Minimal        |
| `:block` | ~1M+ logs/sec    | ~3μs          | Moderate       |

**Recommendation:** Use `:line` (default) unless you have specific requirements:
- Use `:none` for audit/critical logs
- Use `:block` for high-volume trace/debug logs

---

## Async Output

Wrap any output in an async writer to decouple logging from I/O latency. Logging calls return immediately while a background thread handles writes.

### `make-async-output`

```lisp
(make-async-output output &key queue-size min-level)
```

Wrap an output in an asynchronous writer with a bounded queue.

**Arguments:**
- `output` - Underlying output to wrap (stream-output, file-output, etc.)
- `queue-size` - Maximum number of buffered entries (default: `1024`)
- `min-level` - Minimum log level (default: inherits from wrapped output)

**Returns:** An `async-output` instance

**Example:**
```lisp
;; Basic async wrapper
(llog:make-async-output
  (llog:make-file-output "app.log"))

;; High-volume async logging
(llog:make-async-output
  (llog:make-file-output "logs/events.log"
    :encoder (llog:make-json-encoder))
  :queue-size 4096)

;; Async + block buffering for maximum throughput
(llog:make-async-output
  (llog:make-file-output "trace.log"
    :buffer-mode :block
    :buffer-size 32768)
  :queue-size 8192)

;; Multiple outputs: console (sync) + file (async)
(defvar *logger*
  (llog:make-logger
    :outputs (list
              ;; Synchronous console for immediate feedback
              (llog:make-stream-output *standard-output*
                :encoder (llog:make-console-encoder))
              ;; Async file for performance
              (llog:make-async-output
                (llog:make-file-output "app.log"
                  :encoder (llog:make-json-encoder))))))
```

---

### Async Behavior

**Queue Mechanics:**
- Log entries are added to an in-memory circular buffer
- Background thread drains the queue and writes to underlying output
- If queue fills up, logging calls block until space is available
- On shutdown, queue is drained before thread terminates

**Blocking vs Non-Blocking:**
- Currently blocking if queue is full (prevents data loss)
- Future: May add non-blocking mode with drop semantics

**Thread Naming:**
- Worker thread is named `"llog-async-output"` for easy identification in debuggers

**Error Handling:**
- Errors in background thread are printed to `*error-output*`
- Logging calls don't fail due to I/O errors
- Background thread continues processing remaining entries

**Example with Monitoring:**
```lisp
(defvar *async-output*
  (llog:make-async-output
    (llog:make-file-output "app.log")
    :queue-size 2048))

;; Monitor queue depth
(defun check-queue-health ()
  (let ((count (llog::async-output-count *async-output*))
        (size (llog::async-output-queue-size *async-output*)))
    (when (> (/ count size) 0.8)
      (warn "Async log queue ~D% full" (* 100 (/ count size))))))
```

---

### Flushing and Shutdown

#### `flush-output`

```lisp
(flush-output output)
```

Flush buffered log entries to the underlying destination.

**Behavior:**
- **File Output (`:block` mode):** Writes buffer contents to disk
- **File Output (`:line` or `:none`):** Calls `force-output` on stream
- **Async Output:** Blocks until queue is empty and underlying output is flushed
- **Stream Output:** Calls `force-output` on stream

**Example:**
```lisp
;; Flush before important operation
(llog:info "Starting critical section")
(llog:flush-output (first (llog:logger-outputs *logger*)))
(critical-operation)

;; Periodic flush for block-buffered output
(loop
  (log-batch)
  (llog:flush-output *file-output*)
  (sleep 10))
```

---

#### `close-output`

```lisp
(close-output output)
```

Close an output, flushing any buffered data and releasing resources.

**Behavior:**
- **File Output:** Flushes buffer (if `:block` mode), closes file stream
- **Async Output:** Signals shutdown, waits for worker thread, closes underlying output
- **Stream Output:** Closes stream (if it's a file stream)

**Example:**
```lisp
(let ((output (llog:make-file-output "temp.log")))
  (unwind-protect
       (progn
         (llog:add-output *logger* output)
         (llog:info "Temporary logging"))
    (llog:close-output output)))

;; Async shutdown
(let ((async (llog:make-async-output
              (llog:make-file-output "app.log"))))
  (llog:add-output *logger* async)
  ;; ... logging happens ...
  (llog:close-output async))  ; Waits for queue to drain
```

---

## Base Output Protocol

All outputs implement this protocol. You can create custom outputs by implementing these methods.

### `output` (class)

Base class for all output destinations.

**Slots:**
- `encoder` - Encoder for formatting log entries
- `min-level` - Minimum log level to write

---

### `write-entry`

```lisp
(write-entry output entry)
```

Write a log entry to the output. This is the core method all outputs must implement.

**Arguments:**
- `output` - The output instance
- `entry` - A `log-entry` instance

**Returns:** No meaningful value

**Example (custom output):**
```lisp
(defclass database-output (llog:output)
  ((connection :initarg :connection)))

(defmethod llog:write-entry ((output database-output) (entry llog:log-entry))
  (let ((conn (slot-value output 'connection)))
    (execute-sql conn
                 "INSERT INTO logs (level, message, timestamp) VALUES (?, ?, ?)"
                 (llog:log-entry-level entry)
                 (llog:log-entry-message entry)
                 (llog:log-entry-timestamp entry))))
```

---

### `output-min-level`

```lisp
(output-min-level output)
```

Get the minimum log level for an output.

**Returns:** Log level integer (0-6)

---

### `output-encoder`

```lisp
(output-encoder output)
```

Get the encoder for an output.

**Returns:** Encoder instance

---

## Performance Tips

1. **Use async output for I/O-bound logging:**
   ```lisp
   (llog:make-async-output (llog:make-file-output "app.log"))
   ```

2. **Use block buffering for high-volume logs:**
   ```lisp
   (llog:make-file-output "trace.log"
     :buffer-mode :block
     :buffer-size 32768)
   ```

3. **Combine async + block for maximum throughput:**
   ```lisp
   (llog:make-async-output
     (llog:make-file-output "events.log"
       :buffer-mode :block
       :buffer-size 65536)
     :queue-size 8192)
   ```

4. **Use per-output min-level filtering:**
   ```lisp
   ;; Console: only warnings and errors
   (llog:make-stream-output *standard-output* :min-level :warn)
   ;; File: everything
   (llog:make-file-output "debug.log" :min-level :trace)
   ```

5. **Separate outputs by concern:**
   ```lisp
   (llog:make-logger
     :outputs (list
               ;; Human-readable console
               (llog:make-stream-output *standard-output*
                 :encoder (llog:make-console-encoder))
               ;; Structured file
               (llog:make-file-output "app.log"
                 :encoder (llog:make-json-encoder))
               ;; Error-only file
               (llog:make-file-output "errors.log"
                 :encoder (llog:make-json-encoder)
                 :min-level :error)))
   ```

---

## See Also

- [Core API](core.md) - Logger creation and management
- [Encoders API](encoders.md) - Output formats (console, JSON, S-expression)
- [Fields API](fields.md) - Structured field constructors
