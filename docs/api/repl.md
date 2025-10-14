# REPL Integration API

Interactive development features: recent log buffer, search, log capture for testing, and custom field types.

## Recent Logs Buffer

Keep recent log entries in memory for quick inspection during development.

### `enable-recent-logs`

```lisp
(enable-recent-logs logger &optional capacity)
```

Enable recent log tracking with circular buffer.

**Arguments:**
- `logger` - Logger to track
- `capacity` - Buffer size (default: 100 entries)

**Example:**
```lisp
(llog:enable-recent-logs *logger* 500)

;; Generate logs
(llog:info "User logged in" :user-id 123)
(llog:warn "Cache miss" :key "user:123")
(llog:error "Database timeout")

;; View them later
(llog:show-recent)
```

---

### `disable-recent-logs`

```lisp
(disable-recent-logs logger)
```

Disable recent log tracking and free buffer.

---

### `show-recent`

```lisp
(show-recent &key logger count level logger-name pattern)
```

Display recent log entries with optional filtering.

**Arguments:**
- `logger` - Logger to show (default: `*logger*`)
- `count` - Number of entries to show (default: 10, or `:all` for everything)
- `level` - Minimum log level filter (default: nil = all levels)
- `logger-name` - Filter by logger name (string)
- `pattern` - Filter by regex pattern, case-insensitive (string)

**Example:**
```lisp
;; Show last 10 entries
(llog:show-recent)

;; Show all buffered entries
(llog:show-recent :count :all)

;; Show only errors
(llog:show-recent :level :error)

;; Show specific logger
(llog:show-recent :logger-name "app.db")

;; Search by pattern (case-insensitive)
(llog:show-recent :pattern "user")  ; Matches "User", "user", "USERNAME"

;; Combine filters
(llog:show-recent :level :warn :pattern "timeout" :count 20)
```

---

### `grep-logs`

```lisp
(grep-logs pattern &key logger level logger-name)
```

Search recent logs with regex pattern.

**Arguments:**
- `pattern` - Regex pattern to search (case-insensitive)
- `logger` - Logger to search (default: `*logger*`)
- `level` - Minimum log level filter
- `logger-name` - Filter by logger name

**Returns:** List of matching `log-entry` structures

**Example:**
```lisp
;; Find all logs containing "error"
(llog:grep-logs "error")

;; Find login events at INFO level or above
(llog:grep-logs "login" :level :info)

;; Find database queries
(llog:grep-logs "query" :logger-name "app.db")

;; Regex patterns
(llog:grep-logs "User \\d+")  ; "User 123", "User 456", etc.

;; Programmatic use
(let ((errors (llog:grep-logs "failed")))
  (format t "Found ~D errors~%" (length errors))
  (dolist (entry errors)
    (format t "  ~A~%" (llog:log-entry-message entry))))
```

---

## Log Capture for Testing

Capture log output in tests without writing to files.

### `with-captured-logs`

```lisp
(with-captured-logs (&optional logger &key level) &body body)
```

Execute body with logs captured and returned as a list.

**Arguments:**
- `logger` - Logger to capture from (default: `*logger*`)
- `level` - Minimum log level to capture (default: current level)

**Returns:** Two values:
1. Result of body
2. List of captured `log-entry` structures

**Example:**
```lisp
;; Basic capture
(multiple-value-bind (result logs)
    (llog:with-captured-logs ()
      (llog:info "Processing started")
      (process-data)
      (llog:info "Processing complete")
      42)  ; Return value
  (format t "Result: ~A~%" result)     ; => 42
  (format t "Captured ~D logs~%" (length logs)))  ; => 2

;; In tests
(fiveam:test my-feature-test
  (multiple-value-bind (result logs)
      (llog:with-captured-logs (*logger*)
        (my-function))
    (fiveam:is (= 3 (length logs)))
    (fiveam:is (search "Processing started"
                       (llog:log-entry-message (first logs))))))

;; Capture only errors
(multiple-value-bind (result logs)
    (llog:with-captured-logs (*logger* :level :error)
      (llog:info "Not captured")
      (llog:error "This is captured"))
  (format t "~D error(s)~%" (length logs)))  ; => 1
```

---

## Custom Field Types

Define domain-specific field types with validation and coercion.

### `define-field-type`

```lisp
(define-field-type type-name (value value-type) &key documentation coercion inline)
```

Define a custom field type constructor.

**Arguments:**
- `type-name` - Symbol for the type
- `value` - Parameter name for the value
- `value-type` - Type constraint for the value
- `documentation` - Documentation string (keyword arg)
- `coercion` - Form to coerce/validate value (keyword arg)
- `inline` - Whether to inline constructor (keyword arg, default: `T`)

**Example:**
```lisp
;; Simple type
(llog:define-field-type uuid (value string)
  :documentation "UUID field type")

(llog:info-typed "User created"
  (llog:uuid "user-id" "550e8400-e29b-41d4-a716-446655440000"))

;; Type with validation
(llog:define-field-type email (value string)
  :documentation "Email address"
  :coercion (progn
              (unless (find #\@ value)
                (error "Invalid email: ~A" value))
              value))

;; Type with coercion
(llog:define-field-type percentage (value real)
  :documentation "Percentage (0-100)"
  :coercion (max 0.0 (min 100.0 (float value 1.0))))

;; Currency type (stores as cents)
(llog:define-field-type currency (value real)
  :documentation "Currency amount"
  :coercion (round (* value 100)))

;; Use custom types
(llog:info-typed "Transaction"
  (llog:email "customer" "alice@example.com")
  (llog:percentage "discount" 15.0)
  (llog:currency "amount" 99.99))
```

**Generated Constructor:**
`define-field-type` creates and exports a constructor function:
```lisp
(llog:email "addr" "alice@example.com")
;; => #S(FIELD :NAME "addr" :VALUE "alice@example.com" :TYPE :EMAIL)
```

---

## Thread-Safety

All REPL operations are thread-safe:
- Recent log buffer uses locks
- Capture uses temporary outputs with locks
- Multiple threads can call `show-recent` concurrently

---

## Performance

**Zero Overhead When Disabled:** No performance impact if recent logs are not enabled.

**Circular Buffer:** Recent logs buffer wraps automatically, maintaining constant memory usage.

---

## Use Cases

### Interactive Development
```lisp
;; At REPL
(llog:enable-recent-logs *logger* 500)

;; Work on feature
(develop-feature)

;; Review what happened
(llog:show-recent :pattern "database" :level :warn)
```

### Debugging Production Issues
```lisp
;; Temporarily enable in production
(llog:enable-recent-logs *production-logger* 1000)

;; Let it run for a bit
(sleep 60)

;; Search for problems
(llog:grep-logs "timeout|failed|error" :level :error)

;; Disable when done
(llog:disable-recent-logs *production-logger*)
```

### Test-Driven Development
```lisp
(fiveam:test api-error-handling
  "Verify proper error logging"
  (multiple-value-bind (result logs)
      (llog:with-captured-logs ()
        (handler-case (api-call-that-fails)
          (error (e) nil)))
    (fiveam:is (= 1 (length logs)))
    (fiveam:is (>= (llog:log-entry-level (first logs))
                   llog:+error+))))
```

---

## See Also

- [Fields API](fields.md#custom-field-types) - More on custom field types
- [Core API](core.md) - Logger configuration
