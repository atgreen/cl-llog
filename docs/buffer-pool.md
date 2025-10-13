# Buffer Pool System

## Overview

LLOG includes a high-performance buffer pool system designed to minimize memory allocations during logging operations. The system uses a combination of global pooling and thread-local caching to provide fast, low-contention buffer access.

## Architecture

### Three-Tier Design

1. **Thread-Local Cache**: Each thread maintains one cached buffer in thread-local storage
2. **Global Pool**: A shared pool of pre-allocated buffers (default: 32 buffers)
3. **On-Demand Allocation**: New buffers created when pool is exhausted

### Thread-Local Caching

```
Thread 1: [Buffer] ──┐
Thread 2: [Buffer] ──┼──> Global Pool: [Buffer, Buffer, Buffer, ...]
Thread 3: [Buffer] ──┘         ↓
                            New buffers allocated when needed
```

- Each thread caches exactly one buffer
- Uses weak-key hash table to prevent memory leaks from dead threads
- Zero lock contention for thread-local hits
- Falls back to global pool when thread cache is full

### Buffer Structure

```lisp
(defstruct char-buffer
  (string string :type (simple-array character (*)))
  (length length :type fixnum))
```

- **string**: Growable character array
- **length**: Current number of characters used

## API Reference

### Buffer Acquisition and Release

#### `acquire-char-buffer &optional pool`

Obtain a character buffer, preferring thread-local cache.

```lisp
(let ((buffer (acquire-char-buffer)))
  ;; Use buffer
  (release-char-buffer buffer))
```

**Return**: A cleared `char-buffer` ready for use

**Thread-Safety**: Safe for concurrent calls from multiple threads

#### `release-char-buffer buffer &optional pool`

Return a buffer for reuse, caching in thread-local storage first.

```lisp
(unwind-protect
    (progn
      (let ((buffer (acquire-char-buffer)))
        ;; Use buffer
        ))
  (release-char-buffer buffer))
```

**Arguments**:
- `buffer`: The buffer to release

**Behavior**:
1. Clears the buffer
2. If thread has no cached buffer, caches it
3. Otherwise, returns to global pool (if pool not full)

### Buffer Operations

#### `char-buffer-push-char buffer char`

Append a single character to the buffer.

```lisp
(char-buffer-push-char buffer #\A)
```

**Auto-resize**: Buffer grows automatically if needed

#### `char-buffer-push-string buffer string`

Append a string to the buffer.

```lisp
(char-buffer-push-string buffer "Hello, World!")
```

**Performance**: Efficient bulk copy operation

#### `char-buffer-push-buffer target source`

Append contents of one buffer to another.

```lisp
(char-buffer-push-buffer main-buffer temp-buffer)
```

**Use Case**: Combining buffers for hierarchical encoding

#### `char-buffer-clear buffer`

Reset buffer to empty state without deallocating.

```lisp
(char-buffer-clear buffer)
```

**Performance**: O(1) operation, just resets length

#### `char-buffer-write-to-stream buffer stream`

Write buffer contents to a stream.

```lisp
(char-buffer-write-to-stream buffer *standard-output*)
```

**Efficient**: Uses `write-string` with `:start` and `:end`

#### `char-buffer-last-char buffer`

Get the last character in the buffer.

```lisp
(let ((last (char-buffer-last-char buffer)))
  (when (and last (char= last #\Newline))
    ;; Handle newline
    ))
```

**Return**: Character or `NIL` if buffer is empty

### Pool Configuration

#### `make-buffer-pool &key buffer-size max-buffers`

Create a custom buffer pool.

```lisp
(defparameter *my-pool*
  (make-buffer-pool :buffer-size 4096
                   :max-buffers 16))

(let ((buffer (acquire-char-buffer *my-pool*)))
  ;; Use buffer
  (release-char-buffer buffer *my-pool*))
```

**Parameters**:
- `buffer-size`: Initial capacity of each buffer (default: 8192)
- `max-buffers`: Maximum buffers to cache in pool (default: 32)

**Use Case**: Custom pools for specific subsystems

## Usage Patterns

### Basic Pattern

```lisp
(let ((buffer (acquire-char-buffer)))
  (unwind-protect
       (progn
         (char-buffer-push-string buffer "Hello")
         (char-buffer-write-to-stream buffer *standard-output*))
    (release-char-buffer buffer)))
```

**Always** use `unwind-protect` to ensure buffer is released

### JSON Encoding Pattern

```lisp
(defun encode-json-object (obj)
  (let ((buffer (acquire-char-buffer)))
    (unwind-protect
         (progn
           (char-buffer-push-char buffer #\{)
           (encode-fields buffer obj)
           (char-buffer-push-char buffer #\})
           (char-buffer-write-to-stream buffer *output*))
      (release-char-buffer buffer))))
```

### File Output Pattern

LLOG's file output automatically uses the buffer pool:

```lisp
(let ((output (make-file-output "/var/log/app.log"
                                :encoder (make-json-encoder)
                                :buffer-mode :block)))
  ;; Logging automatically uses buffer pool
  (llog:info "Message"))
```

No manual buffer management needed - handled internally

## Performance Characteristics

### Allocation Benchmarks

From `benchmarks/allocation-bench.lisp` (1000 iterations):

| Configuration | Total Allocation | Per-Call | Reduction |
|--------------|------------------|----------|-----------|
| Sugared API (stream) | 25.69 KB | 25.69 KB | baseline |
| Typed API (stream) | 2.04 KB | 2.04 KB | 92% |
| Typed API (file, block) | 1.51 KB | 1.51 KB | 94% |

### Throughput

- **Typed API**: ~333,000 logs/second (SBCL on modern hardware)

### Thread-Local Cache Hit Rate

With typical logging patterns:
- **Hit rate**: >95% (buffer cached in thread-local storage)
- **Lock contention**: Near-zero for common case
- **Fallback**: Global pool lock only when thread cache misses

## Implementation Details

### Thread-Local Storage

```lisp
(defstruct thread-local-cache
  (lock (make-lock "llog/thread-cache") :type t)
  (cache (make-hash-table :test 'eq :weakness :key) :type hash-table))
```

**Weak Keys**: Dead thread objects are garbage collected automatically

**Lock**: Protects hash table updates (not buffer access)

### Buffer Growth Strategy

When buffer needs to grow:
1. Calculate new size: `max(needed, current * 2)`
2. Allocate new array
3. Copy existing contents
4. Replace buffer's internal array

**Amortized O(1)**: Doubling strategy ensures good performance

### Pool Overflow Behavior

When global pool is full:
- New buffers are still created on demand
- They're used and then garbage collected
- Pool size limit prevents unbounded growth

## Best Practices

### 1. Always Release Buffers

```lisp
;; ✓ Good
(let ((buffer (acquire-char-buffer)))
  (unwind-protect
       (use-buffer buffer)
    (release-char-buffer buffer)))

;; ✗ Bad - buffer leaks
(let ((buffer (acquire-char-buffer)))
  (use-buffer buffer))
```

### 2. Don't Hold Buffers Across Blocking Operations

```lisp
;; ✗ Bad - holds buffer during I/O
(let ((buffer (acquire-char-buffer)))
  (unwind-protect
       (progn
         (encode-to-buffer buffer data)
         (write-to-network buffer))  ; Blocks!
    (release-char-buffer buffer)))

;; ✓ Good - release before blocking
(let* ((buffer (acquire-char-buffer))
       (data (unwind-protect
                  (encode-to-buffer buffer data)
                (release-char-buffer buffer))))
  (write-to-network data))
```

### 3. Clear Buffers Before Release

The API does this automatically, but if implementing custom patterns:

```lisp
(unwind-protect
     (use-buffer buffer)
  (char-buffer-clear buffer)
  (release-char-buffer buffer))
```

### 4. Consider Custom Pools for Special Cases

```lisp
;; Large buffers for specific subsystem
(defparameter *large-buffer-pool*
  (make-buffer-pool :buffer-size 65536
                   :max-buffers 4))

(defun process-large-data (data)
  (let ((buffer (acquire-char-buffer *large-buffer-pool*)))
    (unwind-protect
         (encode-large-data buffer data)
      (release-char-buffer buffer *large-buffer-pool*))))
```

## Debugging and Monitoring

### Check Pool Statistics

```lisp
(with-lock-held ((buffer-pool-lock *char-buffer-pool*))
  (format t "Buffers in pool: ~D~%"
          (length (buffer-pool-buffers *char-buffer-pool*))))
```

### Monitor Thread Cache

```lisp
(let ((cache (thread-local-cache-cache *thread-buffer-cache*)))
  (format t "Cached buffers: ~D~%"
          (hash-table-count cache)))
```

### Allocation Profiling (SBCL)

```lisp
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  ;; Your logging code here
  )
```

## Future Enhancements

Potential optimizations for future versions:

1. **Lock-Free Ring Buffer**: For even lower contention
2. **Per-Thread Pool**: Multiple buffers per thread
3. **Size Classes**: Different buffer sizes for different use cases
4. **NUMA-Aware Allocation**: For multi-socket systems
5. **Dynamic Pool Sizing**: Adjust based on usage patterns

## See Also

- [JSON Encoder](json-encoder.md) - Uses buffer pool internally
- [File Output](file-output.md) - Block buffering with buffer pool
- [Performance Tuning](performance-tuning.md) - Overall performance guide
