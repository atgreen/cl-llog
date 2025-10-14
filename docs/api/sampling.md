# Sampling and Rate Limiting API

Control log volume in high-throughput applications using probabilistic/deterministic sampling and token bucket rate limiting.

## Sampling

Reduce log volume by logging only a subset of events.

### `set-sampling`

```lisp
(set-sampling logger level-designator probability-or-mode &optional n)
```

Configure sampling for a specific log level.

**Probabilistic Sampling:**
```lisp
(llog:set-sampling *logger* :debug 0.1)  ; Log 10% of DEBUG messages
(llog:set-sampling *logger* :trace 0.01) ; Log 1% of TRACE messages
```

**Deterministic Sampling:**
```lisp
(llog:set-sampling *logger* :info :every 100)   ; Log every 100th INFO
(llog:set-sampling *logger* :debug :every 1000) ; Log every 1000th DEBUG
```

**Clear Sampling:**
```lisp
(llog:set-sampling *logger* :debug nil)
```

**Example:**
```lisp
;; High-volume endpoint
(defvar *api-logger* (llog:make-logger))
(llog:set-sampling *api-logger* :debug 0.01)  ; 1% sampling

(dotimes (i 10000)
  (llog:debug *api-logger* "Request processed" :request-id i))
;; Only ~100 logs written
```

---

### `clear-sampling`

```lisp
(clear-sampling logger level-designator)
```

Clear sampling configuration for a level.

---

### `get-sampling-stats`

```lisp
(get-sampling-stats logger level-designator)
```

Get sampling statistics.

**Returns:** Plist with `:total`, `:sampled`, `:dropped`, `:rate`

**Example:**
```lisp
(llog:get-sampling-stats *logger* :debug)
;; => (:total 10000 :sampled 1000 :dropped 9000 :rate 0.1)
```

---

## Rate Limiting

Prevent log storms using token bucket rate limiting.

### `set-rate-limit`

```lisp
(set-rate-limit logger level-designator max-logs time-unit)
```

Configure rate limiting for a specific log level.

**Time Units:**
- `:per-second` - Maximum logs per second
- `:per-minute` - Maximum logs per minute
- `:per-hour` - Maximum logs per hour

**Example:**
```lisp
;; Allow max 100 ERROR logs per second
(llog:set-rate-limit *logger* :error 100 :per-second)

;; Allow max 10 WARN logs per minute
(llog:set-rate-limit *logger* :warn 10 :per-minute)

;; Allow max 1000 INFO logs per hour
(llog:set-rate-limit *logger* :info 1000 :per-hour)

;; Clear rate limit
(llog:set-rate-limit *logger* :error nil nil)
```

---

### `clear-rate-limit`

```lisp
(clear-rate-limit logger level-designator)
```

Clear rate limiting for a level.

---

### `rate-limited-p`

```lisp
(rate-limited-p logger level-designator)
```

Check if logger is currently rate limited at a level.

**Returns:** `T` if rate limited, `NIL` otherwise

**Example:**
```lisp
(when (llog:rate-limited-p *logger* :error)
  (warn "Error logging is currently rate limited"))
```

---

### `get-rate-limit-stats`

```lisp
(get-rate-limit-stats logger level-designator)
```

Get rate limiting statistics.

**Returns:** Plist with `:total`, `:allowed`, `:dropped`, `:rate`, `:current-tokens`, `:capacity`, `:refill-rate`

**Example:**
```lisp
(llog:get-rate-limit-stats *logger* :error)
;; => (:total 500 :allowed 100 :dropped 400 :rate 0.2
;;     :current-tokens 5 :capacity 100 :refill-rate 100.0)
```

---

## Use Cases

**High-Traffic Services:**
```lisp
;; Sample 1% of INFO, rate limit WARNs
(llog:set-sampling *logger* :info 0.01)
(llog:set-rate-limit *logger* :warn 100 :per-minute)
```

**Batch Processing:**
```lisp
;; Log progress every 1000 records
(llog:set-sampling *logger* :info :every 1000)

(loop for record in records
      for i from 1
      do (process-record record)
         (llog:info "Processed record" :id i :total (length records)))
```

**Production Debugging:**
```lisp
;; Temporarily increase sampling for investigation
(llog:set-sampling *logger* :debug 0.5)  ; 50% sampling
(investigate-issue)
(llog:set-sampling *logger* :debug 0.01) ; Back to 1%
```

**Cost Control:**
```lisp
;; Reduce log storage costs by 90%+
(llog:set-sampling *logger* :debug 0.01)  ; 1%
(llog:set-sampling *logger* :trace 0.001) ; 0.1%
```

---

## Token Bucket Algorithm

Rate limiting uses a token bucket:
- Bucket holds tokens (capacity = max-logs)
- Each log consumes one token
- Tokens refill at constant rate
- If bucket empty, log is dropped

This provides smooth rate limiting with burst tolerance.

---

## See Also

- [Hooks API](hooks.md) - Alternative: implement custom sampling with hooks
- [Core API](core.md) - Logger configuration
- [Examples](../../examples/sampling-examples.lisp) - 10 real-world examples
