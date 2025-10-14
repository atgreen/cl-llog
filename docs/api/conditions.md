# Condition System Integration API

Rich error logging with automatic backtrace capture, restart information, and condition chain following. Integrates deeply with Common Lisp's condition system.

## Functions

### `error-field-detailed`

```lisp
(error-field-detailed name condition &key backtrace restarts chain)
```

Create a detailed error field from a condition.

**Arguments:**
- `name` - Field name (string)
- `condition` - Common Lisp condition object
- `backtrace` - Capture stack frames (boolean, default: `T`)
- `restarts` - Capture available restarts (boolean, default: `NIL`)
- `chain` - Follow condition chain to root cause (boolean, default: `NIL`)

**Returns:** Field with rich error information

**Example:**
```lisp
(handler-case
    (process-payment order)
  (payment-error (err)
    (llog:error-typed "Payment failed"
      (llog:int "order-id" (order-id order))
      (llog:error-field-detailed "error" err
        :backtrace t
        :restarts t))))
```

**See Also:** [Fields API](fields.md#error-field-detailed) for detailed documentation and examples.

---

### `analyze-condition`

```lisp
(analyze-condition condition &key backtrace restarts chain)
```

Analyze a condition and return structured information.

**Arguments:**
- `condition` - Condition to analyze
- `backtrace` - Capture backtrace (boolean, default: `T`)
- `restarts` - Capture restarts (boolean, default: `T`)
- `chain` - Follow chain (boolean, default: `T`)

**Returns:** `condition-info` structure

**Example:**
```lisp
(handler-case
    (risky-operation)
  (error (err)
    (let ((info (llog:analyze-condition err
                  :backtrace t
                  :restarts t
                  :chain t)))
      (format t "Error type: ~A~%" (llog:condition-info-type info))
      (format t "Message: ~A~%" (llog:condition-info-message info))
      (format t "Frames: ~D~%" (length (llog:condition-info-backtrace info))))))
```

---

### `capture-backtrace`

```lisp
(capture-backtrace &optional count)
```

Capture a backtrace with up to `count` frames (default: 20).

**Returns:** List of frame strings

**Implementation Support:**
- **SBCL:** Full support via `sb-debug`
- **CCL:** Full support via `ccl::map-call-frames`
- **Other:** Returns `("Backtrace not available on this implementation")`

**Example:**
```lisp
(let ((frames (llog:capture-backtrace 10)))
  (format t "Stack:~%~{  ~A~%~}" frames))
```

---

### `capture-restarts`

```lisp
(capture-restarts)
```

Capture information about currently available restarts.

**Returns:** List of plists with `:name` and `:description`

**Example:**
```lisp
(let ((restarts (llog:capture-restarts)))
  (format t "Available restarts:~%")
  (loop for r in restarts
        do (format t "  ~A: ~A~%"
                   (getf r :name)
                   (getf r :description))))
```

---

### `condition-cause`

```lisp
(condition-cause condition)
```

Extract the cause of a condition (wrapped/nested condition).

**Returns:** Parent condition or `NIL`

**Note:** Looks for common slot names: `CAUSE`, `PARENT`, `WRAPPED-CONDITION`, `ORIGINAL-CONDITION`

---

### `condition-chain`

```lisp
(condition-chain condition)
```

Get the full chain of conditions from outermost to root cause.

**Returns:** List of conditions

**Example:**
```lisp
(handler-case
    (handler-case
        (connect-database)
      (network-error (e)
        (error 'database-error :cause e)))
  (error (e)
    (let ((chain (llog:condition-chain e)))
      (format t "Error chain (~D conditions):~%" (length chain))
      (loop for c in chain
            do (format t "  ~A~%" (type-of c))))))
```

---

## Structures

### `condition-info`

```lisp
(defstruct condition-info
  type         ; Symbol - condition type
  message      ; String - condition message
  backtrace    ; List - stack frame strings
  restarts     ; List - restart plists
  cause)       ; Condition - parent/cause
```

**Accessors:**
- `condition-info-type`
- `condition-info-message`
- `condition-info-backtrace`
- `condition-info-restarts`
- `condition-info-cause`

---

## Output Formats

### JSON
```json
{
  "level": "error",
  "msg": "Operation failed",
  "error": {
    "type": "PAYMENT-ERROR",
    "message": "Card declined",
    "backtrace": [
      "(PROCESS-PAYMENT #<ORDER>)",
      "(HANDLE-CHECKOUT ...)"
    ],
    "restarts": [
      {"name": "RETRY", "description": "Retry payment"},
      {"name": "ABORT", "description": "Abort operation"}
    ]
  }
}
```

### Console
```
2025-10-14T12:34:56 [ERROR] Operation failed
  error: Card declined (PAYMENT-ERROR)
    Backtrace:
      (PROCESS-PAYMENT #<ORDER>)
      (HANDLE-CHECKOUT ...)
    Restarts:
      RETRY: Retry payment
      ABORT: Abort operation
```

---

## Use Cases

### Production Error Logging
```lisp
(handler-case
    (critical-operation)
  (error (e)
    (llog:error-typed "Operation failed"
      (llog:error-field-detailed "error" e :backtrace t))))
```

### Development Debugging
```lisp
(handler-case
    (experimental-feature)
  (error (e)
    (llog:debug-typed "Feature error"
      (llog:error-field-detailed "error" e
        :backtrace t
        :restarts t
        :chain t))))
```

### Error Recovery
```lisp
(handler-bind
    ((network-error
      (lambda (e)
        (llog:warn-typed "Network error, retrying"
          (llog:error-field-detailed "error" e :backtrace nil))
        (invoke-restart 'retry))))
  (fetch-data url))
```

---

## Performance

**Zero Overhead When Disabled:** Backtrace capture only occurs when explicitly requested.

**Backtrace Cost:** ~100μs on SBCL for 20 frames

**Recommendation:** Enable backtrace in development, optionally in production for ERROR/FATAL levels.

---

## See Also

- [Fields API](fields.md#error-field-detailed) - `error-field-detailed` documentation
- [Core API](core.md) - Logging functions
