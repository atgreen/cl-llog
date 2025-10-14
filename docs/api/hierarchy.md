# Hierarchical Loggers API

Named logger hierarchy with inheritance. Loggers form a tree structure where children inherit configuration from parents.

## Functions

### `root-logger`

```lisp
(root-logger)
```

Get the root logger. All loggers inherit from root unless explicitly configured otherwise.

**Example:**
```lisp
(defvar *root* (llog:root-logger))
(llog:set-level *root* :warn)  ; All loggers default to WARN
```

---

### `get-logger`

```lisp
(get-logger name &key level outputs)
```

Get or create a named logger. Names use dot notation for hierarchy.

**Arguments:**
- `name` - Logger name (string, use dots for hierarchy: "app.db.queries")
- `level` - Explicit log level (optional, inherits from parent if not specified)
- `outputs` - Explicit outputs (optional, inherits from parent if not specified)

**Hierarchy Rules:**
- `"app"` → child of root
- `"app.db"` → child of `"app"`
- `"app.db.queries"` → child of `"app.db"`

**Example:**
```lisp
;; Create hierarchy
(defvar *app-logger* (llog:get-logger "app" :level :info))
(defvar *db-logger* (llog:get-logger "app.db" :level :debug))
(defvar *query-logger* (llog:get-logger "app.db.queries"))

;; Query logger inherits DEBUG level from db logger
(llog:info *query-logger* "Query executed")
```

---

### `find-logger`

```lisp
(find-logger name)
```

Find a logger by name without creating it.

**Returns:** Logger or `NIL` if not found

---

## Inheritance

Children inherit from parents:
- **Level:** Child uses parent's level if not explicitly set
- **Outputs:** Child uses parent's outputs if not explicitly set
- **Context Fields:** Children can add fields but don't inherit parent's context fields

### Override Inheritance

```lisp
;; Parent has INFO level
(defvar *app* (llog:get-logger "app" :level :info))

;; Child overrides to DEBUG
(defvar *debug-module* (llog:get-logger "app.debug" :level :debug))

;; Sibling inherits INFO from parent
(defvar *other-module* (llog:get-logger "app.other"))
```

---

## Patterns

### Module-Based Logging
```lisp
(defpackage :myapp/database
  (:use :cl)
  (:export :connect :query))

(in-package :myapp/database)

(defvar *logger* (llog:get-logger "myapp.database"))

(defun connect (url)
  (llog:info *logger* "Connecting to database" :url url)
  ...)

(defun query (sql)
  (llog:debug *logger* "Executing query" :sql sql)
  ...)
```

### Granular Control
```lisp
;; Configure root: only errors by default
(llog:set-level (llog:root-logger) :error)

;; Enable INFO for entire app
(llog:set-level (llog:get-logger "myapp") :info)

;; Enable DEBUG for specific subsystem
(llog:set-level (llog:get-logger "myapp.auth") :debug)

;; Enable TRACE for troubleshooting
(llog:set-level (llog:get-logger "myapp.auth.ldap") :trace)
```

### Dynamic Configuration
```lisp
(defun set-debug-mode (enabled)
  "Enable/disable debug logging for app."
  (llog:set-level (llog:get-logger "myapp")
                 (if enabled :debug :info)))

(defun configure-logging (config)
  "Configure logging from config file."
  (loop for (name level) in config
        do (llog:set-level (llog:get-logger name) level)))

;; Usage
(configure-logging '(("myapp" :info)
                    ("myapp.database" :debug)
                    ("myapp.cache" :warn)))
```

---

## Logger Registry

Loggers are stored in a global registry. Use `get-logger` to access them by name.

**Thread-Safety:** Logger creation and hierarchy operations are thread-safe.

---

## See Also

- [Core API](core.md) - Basic logger creation
- [Pattern Encoder](encoders.md#pattern-encoder) - Use `%n` for logger name in output
