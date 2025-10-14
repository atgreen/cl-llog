# Encoders API

Encoders transform log entries into formatted output. LLOG provides console (human-readable with colors), JSON (machine-readable structured), S-expression (Lisp-native), and pattern layout (customizable) encoders.

## Table of Contents

- [Console Encoder](#console-encoder)
- [JSON Encoder](#json-encoder)
- [S-Expression Encoder](#s-expression-encoder)
- [Pattern Encoder](#pattern-encoder)
- [Custom Encoders](#custom-encoders)

## Console Encoder

Human-readable output with optional ANSI color coding.

### `make-console-encoder`

```lisp
(make-console-encoder &key colors time-format)
```

Create a console encoder for human-readable output.

**Arguments:**
- `colors` - Enable ANSI color codes (boolean, default: `t`)
- `time-format` - Time format function (default: ISO8601-ish format)

**Output Format:**
```
YYYY-MM-DDTHH:MM:SS [LEVEL] message
  field1: value1
  field2: value2
```

**Example:**
```lisp
;; With colors (default)
(llog:make-console-encoder)

;; Without colors
(llog:make-console-encoder :colors nil)

;; Use in logger
(defvar *logger*
  (llog:make-logger
    :outputs (list
              (llog:make-stream-output *standard-output*
                :encoder (llog:make-console-encoder :colors t)))))

(llog:info "User logged in" :user-id 123 :username "alice")
;; Output (colored):
;; 2025-10-14T06:30:15 [INFO] User logged in
;;   user-id: 123
;;   username: "alice"
```

**Color Scheme:**
- TRACE: Gray
- DEBUG: Blue
- INFO: Green
- WARN: Yellow
- ERROR: Red
- FATAL: Red + Bold
- PANIC: Red + Bold + Blinking

**Condition Error Fields:** Console encoder renders backtraces and restarts in readable indented format.

---

## JSON Encoder

Machine-readable JSON Lines format (one JSON object per line).

### `make-json-encoder`

```lisp
(make-json-encoder)
```

Create a JSON encoder for structured machine-readable output.

**Output Format:**
```json
{"level":"info","ts":"2025-10-14T06:30:15","msg":"message","field1":value1,"field2":"value2"}
```

**Example:**
```lisp
(defvar *logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "app.log"
                :encoder (llog:make-json-encoder)))))

(llog:info "User logged in" :user-id 123 :username "alice")
;; Output:
;; {"level":"info","ts":"2025-10-14T06:30:15","msg":"User logged in","user-id":123,"username":"alice"}
```

**Field Type Encoding:**
- Strings: JSON strings with escaping
- Integers: JSON numbers
- Floats: JSON numbers
- Booleans: JSON `true`/`false`
- Timestamps: ISO8601 strings
- Duration-ms: Numbers (milliseconds)
- Errors: Nested objects with type, message, optional backtrace/restarts

**Use Cases:**
- Log aggregation systems (ELK, Splunk)
- Structured log analysis
- Machine parsing
- Log shipping

---

## S-Expression Encoder

Lisp-native S-expression format.

### `make-sexpr-encoder`

```lisp
(make-sexpr-encoder)
```

Create an S-expression encoder for Lisp-readable output.

**Output Format:**
```lisp
(:level :info :ts 3918734415 :msg "message" :field1 value1 :field2 "value2")
```

**Example:**
```lisp
(defvar *logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "app.sexp"
                :encoder (llog:make-sexpr-encoder)))))

(llog:info "User logged in" :user-id 123 :username "alice")
;; Output:
;; (:level :info :ts 3918734415 :msg "User logged in" :user-id 123 :username "alice")
```

**Use Cases:**
- Lisp applications reading logs
- REPL-based log analysis
- Preserve Lisp type information
- Easy `READ`ing into Lisp data structures

---

## Pattern Encoder

Customizable output format using pattern strings.

### `make-pattern-encoder`

```lisp
(make-pattern-encoder pattern &key time-format)
```

Create a pattern-based encoder with customizable format.

**Arguments:**
- `pattern` - Format pattern string with placeholders
- `time-format` - Time format function (optional)

**Pattern Placeholders:**
- `%t` - Timestamp
- `%l` - Log level (uppercase)
- `%m` - Message
- `%n` - Logger name
- `%f{key}` - Field value by key
- `%%` - Literal `%`

**Example:**
```lisp
;; Apache-like format
(llog:make-pattern-encoder "%t %l [%n] %m")

;; With specific fields
(llog:make-pattern-encoder "%t - %f{user-id} - %m")

;; Use in logger
(defvar *logger*
  (llog:make-logger
    :outputs (list
              (llog:make-stream-output *standard-output*
                :encoder (llog:make-pattern-encoder
                          "[%l] %n: %m")))))

(llog:info "Request processed" :duration-ms 45)
;; Output:
;; [INFO] app: Request processed
```

**Use Cases:**
- Legacy log format compatibility
- Custom log formats for specific tools
- Simplified output for debugging

---

## Custom Encoders

Create custom encoders by subclassing `encoder` and implementing the protocol.

### Encoder Protocol

```lisp
(defclass my-encoder (llog:encoder) ())

(defmethod llog:encode-entry ((encoder my-encoder) stream entry)
  ;; Write formatted entry to stream
  ...)

(defmethod llog:encode-field ((encoder my-encoder) stream field)
  ;; Write formatted field to stream
  ...)
```

**Example (CSV Encoder):**
```lisp
(defclass csv-encoder (llog:encoder) ())

(defmethod llog:encode-entry ((encoder csv-encoder) stream (entry llog:log-entry))
  (format stream "~A,~A,\"~A\"~%"
          (llog:log-entry-timestamp entry)
          (llog:level-name (llog:log-entry-level entry))
          (llog:log-entry-message entry)))

;; Use it
(defvar *csv-logger*
  (llog:make-logger
    :outputs (list
              (llog:make-file-output "logs.csv"
                :encoder (make-instance 'csv-encoder)))))
```

### Buffer-Based Encoding (Performance)

Override `encode-entry-into-buffer` for zero-allocation encoding:

```lisp
(defmethod llog:encode-entry-into-buffer ((encoder my-encoder) entry buffer)
  (llog:char-buffer-clear buffer)
  ;; Use char-buffer-push-* functions
  (llog:char-buffer-push-string buffer "...")
  buffer)
```

---

## See Also

- [Outputs API](outputs.md) - Output destinations that use encoders
- [Fields API](fields.md) - Field types that encoders format
- [Core API](core.md) - Logger configuration
