# Core API

Core logger functionality including logger creation, log levels, and basic logging operations.

## Table of Contents

- [Loggers](#loggers)
- [Log Levels](#log-levels)
- [Logging Functions](#logging-functions)
  - [Sugared API](#sugared-api)
  - [Typed API](#typed-api)
- [Contextual Logging](#contextual-logging)
- [Output Management](#output-management)

## Loggers

### `*logger*`

```lisp
*logger*  ; Special variable
```

The default global logger. Automatically initialized on first use with a console output. All sugared API macros use this logger unless you create your own.

**Example:**
```lisp
;; Use default logger
(llog:info "Using global logger")

;; Replace global logger
(setf llog:*logger* (llog:make-logger :level :warn))
```

---

### `make-logger`

```lisp
(make-logger &key name level outputs)
```

Create a new logger with the specified configuration.

**Arguments:**
- `name` - Logger name for identification (string, default: `""`)
- `level` - Minimum log level (keyword, string, or integer, default: `:info`)
- `outputs` - List of output destinations (default: console output to `*standard-output*`)

**Returns:** A `logger` instance

**Example:**
```lisp
;; Simple logger
(defvar *my-logger* (llog:make-logger))

;; Logger with custom level
(defvar *debug-logger* (llog:make-logger :level :debug))

;; Logger with multiple outputs
(defvar *app-logger*
  (llog:make-logger
    :name "app"
    :level :info
    :outputs (list
              (llog:make-stream-output *standard-output*)
              (llog:make-file-output "app.log"))))
```

---

### `set-level`

```lisp
(set-level logger level-designator)
```

Set the minimum log level for a logger. Messages below this level will be filtered out.

**Arguments:**
- `logger` - The logger to configure
- `level-designator` - Log level (`:trace`, `:debug`, `:info`, `:warn`, `:error`, `:fatal`, `:panic`, or integer 0-6)

**Returns:** The level integer

**Thread-Safe:** Yes

**Example:**
```lisp
(llog:set-level *logger* :debug)    ; Allow DEBUG and above
(llog:set-level *logger* :error)    ; Only ERROR and above
(llog:set-level *logger* 2)         ; INFO level (integer form)
```

---

### `with-fields`

```lisp
(with-fields logger &rest keyword-args)
```

Create a child logger with additional context fields. The context fields are automatically added to every log entry made with the child logger.

**Arguments:**
- `logger` - Parent logger
- `keyword-args` - Key-value pairs for context fields

**Returns:** A new logger instance with additional context

**Thread-Safe:** Yes (creates new logger instance)

**Example:**
```lisp
;; Add request ID to all logs
(let ((logger (llog:with-fields *logger* :request-id "req-12345")))
  (llog:info logger "Processing request")
  (llog:info logger "Request complete"))
;; Both logs will include :request-id field

;; Nested context
(let* ((logger1 (llog:with-fields *logger* :user-id 42))
       (logger2 (llog:with-fields logger1 :session-id "abc")))
  (llog:info logger2 "Action performed"))
;; Log includes both :user-id and :session-id
```

---

### `child-logger`

```lisp
(child-logger parent-logger name)
```

Create a child logger that inherits configuration from the parent logger. Unlike `with-fields`, this creates a logger with a different name but the same level, outputs, and context fields.

**Arguments:**
- `parent-logger` - Logger to inherit from
- `name` - Name for the child logger

**Returns:** A new logger instance

**Example:**
```lisp
(defvar *db-logger* (llog:child-logger *logger* "database"))
(llog:info *db-logger* "Connection established")
```

---

## Log Levels

LLOG provides seven log levels, from most verbose to most severe:

### Level Constants

```lisp
llog:+trace+   ; 0 - Finest-grained debugging information
llog:+debug+   ; 1 - Detailed debugging information
llog:+info+    ; 2 - General informational messages
llog:+warn+    ; 3 - Warning messages for potentially harmful situations
llog:+error+   ; 4 - Error events that might still allow continued execution
llog:+fatal+   ; 5 - Severe errors that lead to application termination
llog:+panic+   ; 6 - Critical errors that should invoke the debugger
```

### Level Functions

#### `level-name`

```lisp
(level-name level)
```

Return the string name for a log level integer.

**Example:**
```lisp
(llog:level-name llog:+info+)  ; => "INFO"
(llog:level-name 4)            ; => "ERROR"
```

---

#### `level-keyword`

```lisp
(level-keyword level)
```

Return the keyword name for a log level integer.

**Example:**
```lisp
(llog:level-keyword llog:+info+)  ; => :INFO
(llog:level-keyword 4)            ; => :ERROR
```

---

#### `parse-level`

```lisp
(parse-level level-designator)
```

Parse a level designator (keyword, string, or integer) to a level integer. Returns `NIL` if invalid.

**Example:**
```lisp
(llog:parse-level :info)     ; => 2
(llog:parse-level "DEBUG")   ; => 1
(llog:parse-level 5)         ; => 5
(llog:parse-level :invalid)  ; => NIL
```

---

#### `level>=`

```lisp
(level>= level1 level2)
```

Return `T` if level1 is greater than or equal to level2.

**Example:**
```lisp
(llog:level>= llog:+error+ llog:+warn+)  ; => T
(llog:level>= llog:+debug+ llog:+info+)  ; => NIL
```

---

## Logging Functions

LLOG provides two APIs for logging: the **Sugared API** (convenient) and the **Typed API** (zero-allocation).

### Sugared API

The sugared API automatically infers field types from keyword arguments. Easy to use but allocates memory.

#### `trace`, `debug`, `info`, `warn`, `error`, `fatal`, `panic`

```lisp
(trace message &rest keyword-args)
(debug message &rest keyword-args)
(info message &rest keyword-args)
(warn message &rest keyword-args)
(error message &rest keyword-args)
(fatal message &rest keyword-args)
(panic message &rest keyword-args)
```

Log a message at the respective level with optional keyword arguments as fields.

**Arguments:**
- `message` - Log message (string)
- `keyword-args` - Key-value pairs for structured fields

**Field Type Inference:**
- Strings remain strings
- Numbers become int or float fields
- Booleans become bool fields
- Universal time integers become timestamp fields

**Example:**
```lisp
;; Simple message
(llog:info "Application started")

;; With structured fields
(llog:info "User logged in"
  :user-id 12345
  :username "alice"
  :admin t)

;; Multiple types
(llog:warn "Slow query"
  :duration-ms 1523
  :query "SELECT * FROM users"
  :rows-returned 10000)

;; Error logging
(llog:error "Operation failed"
  :error-code 500
  :retry-count 3)
```

---

### Typed API

The typed API requires explicit field constructors but achieves 92-94% allocation reduction through buffer pooling. Use for performance-critical code paths.

#### `trace-typed`, `debug-typed`, `info-typed`, `warn-typed`, `error-typed`, `fatal-typed`, `panic-typed`

```lisp
(trace-typed message &rest field-forms)
(debug-typed message &rest field-forms)
(info-typed message &rest field-forms)
(warn-typed message &rest field-forms)
(error-typed message &rest field-forms)
(fatal-typed message &rest field-forms)
(panic-typed message &rest field-forms)
```

Log a message at the respective level with explicitly typed fields.

**Arguments:**
- `message` - Log message (string)
- `field-forms` - Field constructor calls (see [Fields API](fields.md))

**Example:**
```lisp
;; Basic typed logging
(llog:info-typed "User logged in"
  (llog:int "user-id" 12345)
  (llog:string "username" "alice")
  (llog:bool "admin" t))

;; Performance-critical path
(llog:debug-typed "Processing record"
  (llog:int "record-id" id)
  (llog:float "score" score)
  (llog:duration-ms "elapsed" elapsed-ms))

;; Error with condition
(handler-case
    (risky-operation)
  (error (e)
    (llog:error-typed "Operation failed"
      (llog:int "attempt" attempt-num)
      (llog:error-field "error" e))))
```

**Performance:** Use typed API in hot paths. The buffer pool achieves 92% allocation reduction vs sugared API.

---

## Contextual Logging

### `with-context`

```lisp
(with-context (&rest keyword-args) &body body)
```

Execute body with additional context fields added to `*logger*`. Automatically creates a child logger with the fields and binds it to `*logger*` within the body.

**Arguments:**
- `keyword-args` - Key-value pairs for context fields
- `body` - Forms to execute with contextual logger

**Returns:** Result of body

**Example:**
```lisp
;; Add request context
(llog:with-context (:request-id (uuid:make-v4-uuid)
                    :user-id user-id)
  (process-request)
  (llog:info "Request processed")
  (llog:info "Response sent"))
;; All logs include :request-id and :user-id

;; Nested contexts
(llog:with-context (:transaction-id txn-id)
  (llog:info "Transaction started")
  (llog:with-context (:step "validation")
    (llog:info "Validating input"))
  (llog:with-context (:step "processing")
    (llog:info "Processing data")))
```

---

## Output Management

### `add-output`

```lisp
(add-output logger output)
```

Add an output destination to a logger. The logger will write to all registered outputs.

**Arguments:**
- `logger` - Logger to configure
- `output` - Output instance (from `make-stream-output`, `make-file-output`, etc.)

**Returns:** The logger

**Thread-Safe:** Yes

**Example:**
```lisp
;; Add JSON file output
(llog:add-output *logger*
  (llog:make-file-output "app.log"
    :encoder (llog:make-json-encoder)))

;; Add multiple outputs
(llog:add-output *logger* (llog:make-file-output "debug.log"))
(llog:add-output *logger* (llog:make-file-output "error.log"))
```

---

### `remove-output`

```lisp
(remove-output logger output)
```

Remove an output destination from a logger.

**Arguments:**
- `logger` - Logger to configure
- `output` - Output instance to remove

**Returns:** The logger

**Thread-Safe:** Yes

**Example:**
```lisp
(let ((file-output (llog:make-file-output "temp.log")))
  (llog:add-output *logger* file-output)
  ;; ... use logger ...
  (llog:remove-output *logger* file-output))
```

---

### `logger-outputs`

```lisp
(logger-outputs logger)
```

Get the list of output destinations for a logger.

**Returns:** List of output instances

**Example:**
```lisp
(llog:logger-outputs *logger*)
;; => (#<STREAM-OUTPUT> #<FILE-OUTPUT>)

;; Iterate outputs
(dolist (output (llog:logger-outputs *logger*))
  (format t "Output: ~A~%" output))
```

---

## See Also

- [Fields API](fields.md) - Field constructors for structured logging
- [Outputs API](outputs.md) - Output destinations (stream, file, async)
- [Encoders API](encoders.md) - Output formats (console, JSON, S-expression)
- [Hooks API](hooks.md) - Extend logger behavior with hooks
