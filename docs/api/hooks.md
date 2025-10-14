# Hooks API

Extensible hooks for filtering, transforming, and monitoring log entries. Hooks allow you to extend logger behavior without modifying core code.

## Hook Types

- **`:pre-log`** - Called before logging; can modify or filter entries
- **`:post-log`** - Called after successful logging; for metrics, notifications
- **`:error`** - Called when logging errors occur

## Functions

### `add-hook`

```lisp
(add-hook logger type function &key name priority)
```

Add a hook to a logger.

**Arguments:**
- `logger` - Logger to add hook to
- `type` - Hook type (`:pre-log`, `:post-log`, or `:error`)
- `function` - Hook function (see signatures below)
- `name` - Optional name for the hook (symbol)
- `priority` - Execution priority (integer, default: 50, lower runs first)

**Hook Function Signatures:**
```lisp
;; Pre-log hook: (logger entry) => entry or NIL
(lambda (logger entry) ...)

;; Post-log hook: (logger entry) => ignored
(lambda (logger entry) ...)

;; Error hook: (logger error entry) => ignored
(lambda (logger error entry) ...)
```

**Example:**
```lisp
;; Filter sensitive data
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    ;; Redact password fields
    (setf (llog:log-entry-fields entry)
          (loop for field in (llog:log-entry-fields entry)
                if (string= "password" (llog::field-key field))
                  collect (llog:string "password" "***REDACTED***")
                else
                  collect field))
    entry)
  :name 'redact-passwords
  :priority 5)

;; Count errors
(defvar *error-count* 0)
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (declare (ignore logger))
    (when (>= (llog:log-entry-level entry) llog:+error+)
      (incf *error-count*)))
  :name 'error-counter)

;; Alert on critical errors
(llog:add-hook *logger* :error
  (lambda (logger error entry)
    (declare (ignore logger entry))
    (send-alert (format nil "Logging error: ~A" error)))
  :name 'alert-on-error)
```

**Pre-log Hook Filtering:**
Return `NIL` to filter out the entry (prevent logging):
```lisp
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (if (< (llog:log-entry-level entry) llog:+info+)
        nil  ; Filter out TRACE and DEBUG
        entry))
  :name 'production-filter)
```

---

### `remove-hook`

```lisp
(remove-hook logger type &key name function)
```

Remove hook(s) from a logger. Specify either `:name` or `:function`.

**Example:**
```lisp
(llog:remove-hook *logger* :pre-log :name 'redact-passwords)
(llog:remove-hook *logger* :post-log :function my-hook-fn)
```

---

### `clear-hooks`

```lisp
(clear-hooks logger &optional type)
```

Remove all hooks from a logger. If type is specified, only clears that type.

**Example:**
```lisp
(llog:clear-hooks *logger* :pre-log)  ; Clear all pre-log hooks
(llog:clear-hooks *logger*)           ; Clear all hooks
```

---

### `list-hooks`

```lisp
(list-hooks logger &optional type)
```

List hooks registered on a logger.

**Returns:** List of hook structures

**Example:**
```lisp
(llog:list-hooks *logger* :pre-log)
;; => (#<HOOK redact-passwords priority=5> #<HOOK validator priority=10>)
```

---

## Hook Execution

**Priority Order:** Lower priority numbers run first (default: 50)

**Error Isolation:** Hook failures don't crash logging. Errors are caught and printed to `*error-output*`.

**Pre-log Chain:** Hooks run in priority order. Entry is passed through the chain. If any hook returns `NIL`, logging is skipped.

**Thread-Safety:** Hooks are called within logger's lock. Keep hooks fast and non-blocking.

---

## Common Patterns

### Metrics Collection
```lisp
(defvar *log-counts* (make-hash-table))
(llog:add-hook *logger* :post-log
  (lambda (logger entry)
    (declare (ignore logger))
    (let ((level (llog:log-entry-level entry)))
      (incf (gethash level *log-counts* 0)))))
```

### Sampling
```lisp
(defvar *sample-counter* 0)
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (if (zerop (mod (incf *sample-counter*) 100))
        entry  ; Log every 100th
        nil))  ; Filter others
  :priority 1)
```

### Context Enrichment
```lisp
(llog:add-hook *logger* :pre-log
  (lambda (logger entry)
    (declare (ignore logger))
    (setf (llog:log-entry-fields entry)
          (append (llog:log-entry-fields entry)
                  (list (llog:string "hostname" (machine-instance))
                        (llog:int "thread-id" (bt:current-thread-id)))))
    entry)
  :priority 10)
```

---

## See Also

- [Sampling API](sampling.md) - Built-in sampling and rate limiting
- [Core API](core.md) - Logger creation
- [Examples](../../examples/hooks.lisp) - 10 complete hook examples
