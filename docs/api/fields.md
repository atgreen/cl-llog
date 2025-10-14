# Fields API

Typed field constructors for structured logging. Fields are key-value pairs with explicit type information that enable zero-allocation logging through the typed API.

## Table of Contents

- [Field Constructors](#field-constructors)
- [Error Fields](#error-fields)
- [Custom Field Types](#custom-field-types)
- [Field Protocol](#field-protocol)

## Field Constructors

All field constructors follow the pattern `(constructor-name key value)` and return a field structure that can be passed to the typed logging API.

### `string`

```lisp
(string name value)
```

Create a string field.

**Arguments:**
- `name` - Field name (string)
- `value` - String value

**Example:**
```lisp
(llog:info-typed "User action"
  (llog:string "username" "alice")
  (llog:string "action" "login")
  (llog:string "ip-address" "192.168.1.1"))
```

---

### `int`

```lisp
(int name value)
```

Create an integer field.

**Arguments:**
- `name` - Field name (string)
- `value` - Integer value

**Example:**
```lisp
(llog:info-typed "Request processed"
  (llog:int "user-id" 12345)
  (llog:int "status-code" 200)
  (llog:int "response-bytes" 4096))
```

---

### `float`

```lisp
(float name value)
```

Create a floating-point field. Value is coerced to `double-float` for consistency.

**Arguments:**
- `name` - Field name (string)
- `value` - Real number (will be coerced to double-float)

**Example:**
```lisp
(llog:info-typed "Measurement recorded"
  (llog:float "temperature" 98.6)
  (llog:float "humidity" 0.65)
  (llog:float "score" 0.95))
```

---

### `bool`

```lisp
(bool name value)
```

Create a boolean field.

**Arguments:**
- `name` - Field name (string)
- `value` - Boolean value (`t` or `nil`)

**Example:**
```lisp
(llog:info-typed "User session"
  (llog:string "username" "alice")
  (llog:bool "authenticated" t)
  (llog:bool "admin" nil)
  (llog:bool "mfa-enabled" t))
```

---

### `timestamp`

```lisp
(timestamp name &optional value)
```

Create a timestamp field. If value is not provided, uses the current time.

**Arguments:**
- `name` - Field name (string)
- `value` - Universal time (unsigned-byte, optional - defaults to current time)

**Example:**
```lisp
;; Current time
(llog:info-typed "Event occurred"
  (llog:timestamp "event-time"))

;; Specific time
(llog:info-typed "Scheduled task"
  (llog:timestamp "scheduled-at" scheduled-time)
  (llog:timestamp "executed-at" (get-universal-time)))

;; Event logging with timestamps
(let ((start-time (get-universal-time)))
  (process-data)
  (llog:info-typed "Processing complete"
    (llog:timestamp "started-at" start-time)
    (llog:timestamp "completed-at")))
```

---

### `duration-ms`

```lisp
(duration-ms name value)
```

Create a duration field in milliseconds. Value is coerced to `double-float`.

**Arguments:**
- `name` - Field name (string)
- `value` - Duration in milliseconds (real number)

**Example:**
```lisp
;; Timing operations
(let ((start (get-internal-real-time)))
  (expensive-operation)
  (let ((elapsed (/ (- (get-internal-real-time) start)
                   (/ internal-time-units-per-second 1000.0))))
    (llog:info-typed "Operation complete"
      (llog:duration-ms "elapsed" elapsed))))

;; Multiple duration fields
(llog:info-typed "Request breakdown"
  (llog:duration-ms "db-query" 45.2)
  (llog:duration-ms "rendering" 12.8)
  (llog:duration-ms "total" 58.0))
```

---

## Error Fields

### `error-field`

```lisp
(error-field name condition)
```

Create a simple error field from a condition. Captures the error type and message.

**Arguments:**
- `name` - Field name (string)
- `condition` - A Common Lisp condition

**Example:**
```lisp
(handler-case
    (/ 1 0)
  (error (e)
    (llog:error-typed "Operation failed"
      (llog:error-field "error" e))))

;; JSON output:
;; {"level":"error","msg":"Operation failed",
;;  "error":{"type":"DIVISION-BY-ZERO",
;;           "message":"arithmetic error DIVISION-BY-ZERO signalled"}}
```

---

### `error-field-detailed`

```lisp
(error-field-detailed name condition &key backtrace restarts chain)
```

Create a detailed error field with optional backtrace, restart information, and condition chain.

**Arguments:**
- `name` - Field name (string)
- `condition` - A Common Lisp condition
- `backtrace` - Capture stack frames (boolean, default: `t`)
- `restarts` - Capture available restarts (boolean, default: `nil`)
- `chain` - Follow condition chain to root cause (boolean, default: `nil`)

**Returns:** A field with rich error information

**Implementation Support:**
- **Backtrace:** Full support on SBCL and CCL, graceful fallback on other implementations
- **Restarts:** Portable across all implementations
- **Chain:** Requires conditions to have a `cause` slot (custom error hierarchies)

**Example:**
```lisp
;; Basic error with backtrace
(handler-case
    (process-payment order)
  (payment-error (err)
    (llog:error-typed "Payment failed"
      (llog:int "order-id" (order-id order))
      (llog:error-field-detailed "error" err :backtrace t))))

;; Capture restarts for debugging
(handler-case
    (load-config "config.lisp")
  (error (err)
    (llog:debug-typed "Config load failed"
      (llog:string "config-file" "config.lisp")
      (llog:error-field-detailed "error" err
        :backtrace t
        :restarts t))))

;; Follow condition chain
(handler-case
    (handler-case
        (connect-to-database url)
      (network-error (err)
        (error 'database-connection-error :cause err)))
  (error (err)
    (llog:error-typed "Database unavailable"
      (llog:error-field-detailed "error" err :chain t))))
```

**JSON Output:**
```json
{
  "level": "error",
  "msg": "Payment failed",
  "order-id": 12345,
  "error": {
    "type": "PAYMENT-ERROR",
    "message": "Card declined",
    "backtrace": [
      "(PROCESS-PAYMENT #<ORDER>)",
      "(HANDLE-CHECKOUT ...)",
      "..."
    ],
    "restarts": [
      {"name": "RETRY-PAYMENT", "description": "Retry with different card"},
      {"name": "CANCEL-ORDER", "description": "Cancel the order"}
    ]
  }
}
```

**Console Output:**
```
2025-10-14T12:34:56 [ERROR] Payment failed
  order-id: 12345
  error: Card declined (PAYMENT-ERROR)
    Backtrace:
      (PROCESS-PAYMENT #<ORDER>)
      (HANDLE-CHECKOUT ...)
      ...
    Restarts:
      RETRY-PAYMENT: Retry with different card
      CANCEL-ORDER: Cancel the order
```

**See Also:** [Conditions API](conditions.md) for more details on condition system integration.

---

## Custom Field Types

Define custom field types with validation and coercion using the REPL integration features.

### `define-field-type`

```lisp
(define-field-type type-name (value value-type) &key documentation coercion inline)
```

Define a custom field type with optional coercion and validation.

**Arguments:**
- `type-name` - Symbol naming the new field type
- `value` - Parameter name for the value
- `value-type` - Type constraint for the value
- `documentation` - Documentation string (keyword)
- `coercion` - Form to coerce/validate value (keyword)
- `inline` - Whether to inline the constructor (keyword, default: `t`)

**Example:**
```lisp
;; Simple custom type
(llog:define-field-type uuid (value string)
  :documentation "UUID field type")

;; Type with validation
(llog:define-field-type email (value string)
  :documentation "Email address field"
  :coercion (progn
              (unless (find #\@ value)
                (error "Invalid email: ~A" value))
              value))

;; Type with coercion
(llog:define-field-type percentage (value real)
  :documentation "Percentage value (0-100)"
  :coercion (max 0.0 (min 100.0 (float value 1.0))))

;; Currency type
(llog:define-field-type currency (value real)
  :documentation "Currency amount in cents"
  :coercion (round (* value 100)))

;; Use custom types
(llog:info-typed "User registered"
  (llog:uuid "user-id" "550e8400-e29b-41d4-a716-446655440000")
  (llog:email "email" "alice@example.com")
  (llog:percentage "completion" 75.5)
  (llog:currency "balance" 123.45))
```

**Generated Constructor:**
Each `define-field-type` creates a constructor function:
```lisp
(llog:uuid "id" "550e8400-...")  ; Returns (field "id" "550e8400-..." :uuid)
```

**Exports:** The constructor function is automatically exported from the `llog` package.

**See Also:** [REPL API](repl.md) for more on custom field types.

---

## Field Protocol

### Field Structure

```lisp
(defstruct field
  name    ; String
  value   ; Any Lisp value
  type)   ; Symbol indicating field type
```

Fields are lightweight structures holding the key-value pair and type information. Encoders use this information to format output appropriately.

---

### `field-name`

```lisp
(field-name field)
```

Get the name of a field.

**Returns:** String

---

### `field-value`

```lisp
(field-value field)
```

Get the value of a field.

**Returns:** Any Lisp value

---

### `field-type`

```lisp
(field-type field)
```

Get the type of a field.

**Returns:** Keyword symbol (`:string`, `:int`, `:float`, `:bool`, `:timestamp`, `:duration-ms`, `:error`, etc.)

---

### `encode-field` (Generic Function)

```lisp
(encode-field encoder stream field)
```

Encode a field to a stream using an encoder. This is a generic function that can be specialized for custom encoders or field types.

**Example (custom encoder):**
```lisp
(defmethod llog:encode-field ((encoder my-custom-encoder)
                              stream
                              (field llog:field))
  (format stream "~A=~A " (llog:field-name field) (llog:field-value field)))
```

---

## Type Inference (Sugared API)

When using the sugared API, field types are automatically inferred from values:

```lisp
(llog:info "User action"
  :username "alice"      ; => (string "username" "alice")
  :user-id 12345         ; => (int "user-id" 12345)
  :score 0.95            ; => (float "score" 0.95)
  :admin t               ; => (bool "admin" t)
  :active nil)           ; => (bool "active" nil)
```

**Type Inference Rules:**
- `string` → `:string` field
- `integer` → `:int` field
- `float` → `:float` field
- `t` or `nil` → `:bool` field
- `condition` → `:error` field
- Other types → `:any` field (uses `princ-to-string`)

**Limitation:** Universal time integers are inferred as `:int`, not `:timestamp`. Use typed API for explicit timestamp fields.

---

## Performance Notes

1. **Field constructors are inlined** for zero-allocation logging
2. **Use typed API for hot paths** - 92% allocation reduction
3. **String concatenation happens at encode time**, not field creation
4. **Field type symbols are compared with `EQL`** (fast)

**Allocation Comparison (1000 iterations, SBCL):**
```lisp
;; Sugared API
(llog:info "User action" :user-id 123 :username "alice")
;; => 25.69 KB per call

;; Typed API
(llog:info-typed "User action"
  (llog:int "user-id" 123)
  (llog:string "username" "alice"))
;; => 2.04 KB per call (92% reduction)
```

---

## See Also

- [Core API](core.md) - Logging functions that use fields
- [Conditions API](conditions.md) - Detailed error field documentation
- [REPL API](repl.md) - Custom field type definition
- [Encoders API](encoders.md) - How fields are formatted
