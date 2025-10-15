# Hooks vs Generic Functions: Design Analysis

This document analyzes the trade-offs between LLOG's current hooks implementation and a hypothetical generic function-based approach.

## Current Implementation: Explicit Hooks

### Architecture

**File: `src/hooks.lisp`** (lines 1-188)

```lisp
(defstruct hook
  (type nil :type (member :pre-log :post-log :error))
  (function nil :type function)
  (name nil :type (or null symbol cl:string))
  (priority 50 :type integer))

;; API
(add-hook logger :pre-log #'my-filter :name 'security-filter :priority 10)
(remove-hook logger :pre-log :name 'security-filter)
(clear-hooks logger :pre-log)
(list-hooks logger)
```

**Logger slots:**
```lisp
(defclass logger ()
  ((pre-log-hooks :initform nil :type list)
   (post-log-hooks :initform nil :type list)
   (error-hooks :initform nil :type list)
   ...))
```

**Execution flow:**
```lisp
;; In log-entry function (src/logger.lisp:150-209)
1. Call pre-log hooks (can modify/filter entry)
2. Write to outputs
3. Call post-log hooks (notifications, metrics)
4. On error: call error hooks
```

### Key Features

1. **Per-logger hooks**: Each logger instance has its own hook list
2. **Three hook types**: :pre-log, :post-log, :error
3. **Priority ordering**: Hooks sorted by numeric priority (lower = earlier)
4. **Named hooks**: Optional names for easy removal
5. **Error isolation**: Hook failures don't break logging
6. **Dynamic modification**: Add/remove hooks at runtime
7. **Introspection**: `list-hooks` shows all attached hooks

## Alternative: Generic Function Approach

### Design Options

There are several ways to use generic functions for hooks. Let's explore the main approaches.

### Option 1: Generic Functions with EQL Specializers

```lisp
;;; Generic function protocol
(defgeneric pre-log-hook (logger entry)
  (:documentation "Called before logging. Return entry or nil to filter.")
  (:method (logger entry)
    ;; Default: no-op, return entry as-is
    entry))

(defgeneric post-log-hook (logger entry)
  (:documentation "Called after successful logging.")
  (:method (logger entry)
    ;; Default: no-op
    (declare (ignore logger entry))
    (values)))

(defgeneric error-hook (logger error entry)
  (:documentation "Called when logging errors occur.")
  (:method (logger error entry)
    ;; Default: no-op
    (declare (ignore logger error entry))
    (values)))

;;; Usage: Define methods for specific loggers
(defmethod pre-log-hook ((logger (eql *my-logger*)) entry)
  ;; Custom filtering for *my-logger*
  (when (>= (log-entry-level entry) +warn+)
    entry))

;;; Usage: Define methods for logger subclasses
(defclass audit-logger (logger) ())

(defmethod post-log-hook ((logger audit-logger) entry)
  ;; All audit-logger instances get this hook
  (send-to-siem entry))
```

### Option 2: Generic Functions with Method Combination

```lisp
;;; Use :around/:before/:after method combination
(defgeneric process-log-entry (logger entry)
  (:method (logger entry)
    ;; Primary method: actual logging
    (dolist (output (logger-outputs logger))
      (write-entry output entry))))

;;; Hook as :before method
(defmethod process-log-entry :before ((logger audit-logger) entry)
  (validate-entry entry))

;;; Hook as :after method
(defmethod process-log-entry :after ((logger audit-logger) entry)
  (send-metric 'log-entry-count 1))

;;; Hook as :around method (can filter)
(defmethod process-log-entry :around ((logger (eql *dev-logger*)) entry)
  (when (in-development-mode-p)
    (call-next-method)))
```

### Option 3: Hybrid Approach

```lisp
;;; Keep hooks list, but use generic functions for execution
(defgeneric execute-pre-log-hooks (logger entry hooks)
  (:method (logger entry hooks)
    ;; Default implementation (current behavior)
    (let ((result entry))
      (dolist (hook hooks result)
        (when result
          (setf result (funcall (hook-function hook) logger result)))))))

;;; Allow specialization
(defmethod execute-pre-log-hooks :around ((logger audit-logger) entry hooks)
  ;; Add audit-specific pre-processing
  (log-audit-trail logger entry)
  (call-next-method))
```

## Detailed Comparison

### 1. Runtime Performance

| Aspect | Current Hooks | Generic Functions |
|--------|---------------|-------------------|
| **Dispatch overhead** | Direct `funcall` (~5-10ns) | Generic function dispatch (~20-50ns) |
| **Multiple hooks** | Simple `dolist` iteration | Multiple method calls (slower) |
| **Per-instance behavior** | Lock + list copy (~50-100ns) | EQL specializer lookup (varies) |
| **Typical overhead** | **~100-200ns** for hook execution | **~500-1000ns** for multiple methods |

**Verdict**: Current hooks are **faster** (2-5x) for the common case of per-logger hooks.

**Why**:
- Generic functions optimize for type-based dispatch
- Per-instance specialization (EQL) is slow
- Multiple method calls have overhead
- Hook list iteration is simpler

### 2. API Simplicity

#### Current Hooks

```lisp
;; Add hook
(add-hook *logger* :pre-log
          (lambda (logger entry)
            (redact-sensitive-fields entry))
          :name 'redactor
          :priority 10)

;; Remove hook
(remove-hook *logger* :pre-log :name 'redactor)

;; List hooks
(list-hooks *logger* :pre-log)
```

**Pros**:
- ✅ Explicit, clear intent
- ✅ Self-documenting API
- ✅ Easy to understand for newcomers
- ✅ Consistent with event-driven patterns

**Cons**:
- ❌ Not "idiomatic" Common Lisp
- ❌ Adds API surface area
- ❌ Doesn't leverage CLOS

#### Generic Functions

```lisp
;; Add "hook"
(defmethod pre-log-hook ((logger (eql *logger*)) entry)
  (redact-sensitive-fields entry))

;; Remove "hook"
(remove-method #'pre-log-hook
               (find-method #'pre-log-hook '()
                           (list (find-class 'eql-specializer) 'log-entry)))

;; List "hooks"
(closer-mop:generic-function-methods #'pre-log-hook)
```

**Pros**:
- ✅ Idiomatic Common Lisp
- ✅ Familiar to CLOS programmers
- ✅ Leverages existing infrastructure

**Cons**:
- ❌ `remove-method` is cumbersome
- ❌ Introspection requires MOP
- ❌ Less obvious for logging use case
- ❌ No built-in priority mechanism

**Verdict**: Current hooks have a **better API** for this use case.

### 3. Dynamic Modification

#### Current Hooks

```lisp
;; Add at runtime
(add-hook logger :post-log #'send-alert)

;; Remove by name
(remove-hook logger :post-log :name 'alert-sender)

;; Remove by function
(remove-hook logger :post-log :function #'send-alert)

;; Clear all
(clear-hooks logger :post-log)
```

**Pros**:
- ✅ Easy to add/remove
- ✅ Can remove by name or function
- ✅ Thread-safe (locks around modifications)
- ✅ Clear semantics

#### Generic Functions

```lisp
;; Add at runtime
(defmethod post-log-hook ((logger (eql logger)) entry)
  (send-alert entry))

;; Remove (awkward)
(let ((method (find-method #'post-log-hook '()
                          (list (intern-eql-specializer logger)
                                (find-class 'log-entry)))))
  (remove-method #'post-log-hook method))

;; Clear all (even more awkward)
(dolist (method (generic-function-methods #'post-log-hook))
  (when (eql-specializer-for-logger-p method logger)
    (remove-method #'post-log-hook method)))
```

**Cons**:
- ❌ `remove-method` requires finding the method first
- ❌ No built-in filtering (name, priority)
- ❌ Thread-safety not guaranteed (implementation-dependent)
- ❌ Complex code for simple operations

**Verdict**: Current hooks are **much easier** to modify dynamically.

### 4. Ordering Control

#### Current Hooks

```lisp
;; Priority 10 runs before 20
(add-hook logger :pre-log #'validate :priority 10)
(add-hook logger :pre-log #'redact :priority 20)
(add-hook logger :pre-log #'enrich :priority 30)
```

**Pros**:
- ✅ Explicit numeric priority
- ✅ Stable sort preserves insertion order
- ✅ Easy to understand execution order
- ✅ Can insert between existing hooks

#### Generic Functions

```lisp
;; No built-in ordering mechanism
;; Would need custom method combination

(define-method-combination priority-hooks ()
  ((methods priority-hooks (:priority *)))
  `(progn
     ,@(stable-sort methods #'< :key (lambda (m) (method-priority m)))))

;; Then use it:
(defgeneric pre-log-hook (logger entry)
  (:method-combination priority-hooks))

;; Add with priority (non-standard)
(defmethod pre-log-hook priority-hooks ((logger logger) entry)
  (:priority 10)
  (validate entry))
```

**Cons**:
- ❌ No built-in priority mechanism
- ❌ Requires custom method combination
- ❌ Non-standard syntax
- ❌ Complex to implement correctly

**Verdict**: Current hooks have **superior ordering** control.

### 5. Error Handling

#### Current Hooks

```lisp
(defun %call-pre-log-hooks (logger entry)
  (dolist (hook hooks result)
    (handler-case
        (setf result (funcall (hook-function hook) logger result))
      (cl:error (e)
        (format *error-output*
                "~&LLOG: Pre-log hook ~A error: ~A~%"
                (or (hook-name hook) "(unnamed)")
                e)))))
```

**Pros**:
- ✅ Built-in error isolation
- ✅ Named hooks make debugging easier
- ✅ Errors don't break logging
- ✅ Can identify which hook failed

#### Generic Functions

```lisp
;; No built-in error isolation
;; Would need to wrap method calls

(defmethod pre-log-hook :around (logger entry)
  (handler-case
      (call-next-method)
    (cl:error (e)
      ;; How do we know which method failed?
      (format *error-output* "Hook error: ~A~%" e)
      entry)))  ; Return original entry
```

**Cons**:
- ❌ No built-in error isolation
- ❌ Hard to identify failing method
- ❌ Requires manual wrapping
- ❌ `:around` affects all methods

**Verdict**: Current hooks have **better error handling**.

### 6. Introspection

#### Current Hooks

```lisp
(list-hooks logger :pre-log)
;; => (#S(HOOK :TYPE :PRE-LOG :NAME VALIDATE :PRIORITY 10 ...)
;;     #S(HOOK :TYPE :PRE-LOG :NAME REDACT :PRIORITY 20 ...)
;;     ...)

;; Easy to inspect
(loop for hook in (list-hooks logger :pre-log)
      collect (hook-name hook))
;; => (VALIDATE REDACT ENRICH)
```

**Pros**:
- ✅ Simple, direct introspection
- ✅ Shows names, priorities, types
- ✅ Per-logger view
- ✅ No MOP required

#### Generic Functions

```lisp
(closer-mop:generic-function-methods #'pre-log-hook)
;; => (#<STANDARD-METHOD PRE-LOG-HOOK (#<EQL-SPECIALIZER {1234567890}>
;;                                      LOG-ENTRY) {ABCDEF}>
;;     ...)

;; Complex to filter by logger
(remove-if-not
  (lambda (method)
    (and (eql-specializer-p (first (method-specializers method)))
         (eql (eql-specializer-object (first ...)) logger)))
  (generic-function-methods #'pre-log-hook))
```

**Cons**:
- ❌ Requires MOP (closer-mop)
- ❌ Complex filtering needed
- ❌ No semantic information (names, priorities)
- ❌ Hard to understand output

**Verdict**: Current hooks have **much better introspection**.

### 7. Per-Instance vs Global Behavior

#### Current Hooks

```lisp
;; Per-instance hooks (easy)
(add-hook *logger1* :pre-log #'redact)
(add-hook *logger2* :pre-log #'validate)

;; Each logger has its own hooks
(list-hooks *logger1*)  ; => (redact)
(list-hooks *logger2*)  ; => (validate)
```

**Pros**:
- ✅ Natural per-instance behavior
- ✅ No global state pollution
- ✅ Clear ownership

#### Generic Functions

```lisp
;; Per-instance (awkward with EQL specializers)
(defmethod pre-log-hook ((logger (eql *logger1*)) entry)
  (redact entry))

(defmethod pre-log-hook ((logger (eql *logger2*)) entry)
  (validate entry))

;; Global behavior (natural)
(defmethod pre-log-hook ((logger logger) entry)
  ;; Applies to ALL loggers
  (standard-processing entry))

;; Per-subclass (natural)
(defclass audit-logger (logger) ())

(defmethod pre-log-hook ((logger audit-logger) entry)
  ;; Applies to all audit-logger instances
  (audit-processing entry))
```

**Pros**:
- ✅ Global hooks are natural
- ✅ Subclass-based dispatch is powerful

**Cons**:
- ❌ Per-instance is awkward
- ❌ EQL specializers are slow
- ❌ Global state in methods

**Verdict**: **Depends on use case**. Current hooks are better for per-instance; generic functions better for subclass/global.

### 8. Composability

#### Current Hooks

```lisp
;; Easy to compose hooks
(defun make-filtering-hook (predicate)
  (lambda (logger entry)
    (when (funcall predicate entry)
      entry)))

(add-hook logger :pre-log (make-filtering-hook #'important-p))

;; Easy to chain
(add-hook logger :pre-log #'validate :priority 10)
(add-hook logger :pre-log #'redact :priority 20)
(add-hook logger :pre-log #'enrich :priority 30)
```

**Pros**:
- ✅ First-class functions easy to compose
- ✅ Closures capture state
- ✅ Higher-order hooks straightforward

#### Generic Functions

```lisp
;; Composition requires helper methods
(defmethod pre-log-hook :around ((logger logger) entry)
  (let ((filtered (call-next-method)))
    (when (and filtered (important-p filtered))
      filtered)))

;; Or separate methods
(defmethod pre-log-hook ((logger logger) entry)
  (validate entry))

(defmethod pre-log-hook :after ((logger logger) entry)
  (redact entry))
```

**Pros**:
- ✅ Method combination provides structure
- ✅ Multiple dispatch allows complex patterns

**Cons**:
- ❌ Harder to compose dynamically
- ❌ Order controlled by method combination, not priority
- ❌ Can't easily create hooks from closures

**Verdict**: Current hooks are **easier to compose**.

### 9. Common Lisp Idioms

#### Current Hooks

**Pros**:
- ✅ Uses `defstruct` (simple, fast)
- ✅ First-class functions
- ✅ Clear, explicit API

**Cons**:
- ❌ Doesn't leverage CLOS
- ❌ Reinvents method dispatch
- ❌ Not "the Lisp way"

#### Generic Functions

**Pros**:
- ✅ Uses CLOS (idiomatic)
- ✅ Leverages existing infrastructure
- ✅ Familiar pattern

**Cons**:
- ❌ Overcomplicates simple task
- ❌ Not all Lispers are CLOS experts
- ❌ MOP required for full control

**Verdict**: **Philosophical difference**. Generic functions are more idiomatic, but hooks are more practical.

## Real-World Examples

### Example 1: Security Filtering

#### Current Hooks

```lisp
(defun redact-sensitive-fields (logger entry)
  "Remove credit card numbers from log messages."
  (setf (log-entry-message entry)
        (regex-replace-all "\\d{4}-\\d{4}-\\d{4}-\\d{4}"
                          (log-entry-message entry)
                          "****-****-****-****"))
  entry)

(add-hook *production-logger* :pre-log #'redact-sensitive-fields
          :name 'security-redactor
          :priority 5)  ; Run early

;; Easy to disable in development
(when (production-mode-p)
  (add-hook *logger* :pre-log #'redact-sensitive-fields))
```

#### Generic Functions

```lisp
(defmethod pre-log-hook ((logger production-logger) entry)
  "Remove credit card numbers from log messages."
  (setf (log-entry-message entry)
        (regex-replace-all "\\d{4}-\\d{4}-\\d{4}-\\d{4}"
                          (log-entry-message entry)
                          "****-****-****-****"))
  (call-next-method))

;; Harder to enable/disable dynamically
;; Would need conditional logic in method:
(defmethod pre-log-hook ((logger production-logger) entry)
  (when (production-mode-p)
    (setf (log-entry-message entry) ...))
  (call-next-method))
```

**Winner**: Current hooks (easier to add/remove dynamically)

### Example 2: Metrics Collection

#### Current Hooks

```lisp
(defvar *log-counter* 0)

(add-hook *logger* :post-log
          (lambda (logger entry)
            (incf *log-counter*)
            (when (zerop (mod *log-counter* 1000))
              (send-metric 'log-count *log-counter*)))
          :name 'metrics-collector)

;; Easy to remove
(remove-hook *logger* :post-log :name 'metrics-collector)
```

#### Generic Functions

```lisp
(defvar *log-counter* 0)

(defmethod post-log-hook :after ((logger logger) entry)
  (incf *log-counter*)
  (when (zerop (mod *log-counter* 1000))
    (send-metric 'log-count *log-counter*)))

;; Remove is awkward
(remove-method #'post-log-hook
               (find-method #'post-log-hook '(:after)
                           (list (find-class 'logger) (find-class 'log-entry))))
```

**Winner**: Current hooks (easier to manage)

### Example 3: Logger Subclass Behavior

#### Current Hooks

```lisp
(defclass audit-logger (logger) ())

(defmethod initialize-instance :after ((logger audit-logger) &key)
  ;; Add audit hook to this specific logger
  (add-hook logger :post-log #'send-to-audit-trail))
```

#### Generic Functions

```lisp
(defclass audit-logger (logger) ())

(defmethod post-log-hook ((logger audit-logger) entry)
  ;; Automatically applies to all audit-logger instances
  (send-to-audit-trail entry)
  (call-next-method))
```

**Winner**: Generic functions (more natural for subclass behavior)

## Performance Benchmarks

### Theoretical Analysis

**Current hooks** (per log call with 3 hooks):
```
Lock acquisition:      ~50ns
List copy:             ~20ns
Loop setup:            ~10ns
Hook 1 funcall:        ~10ns + user code
Hook 2 funcall:        ~10ns + user code
Hook 3 funcall:        ~10ns + user code
Total overhead:        ~110ns + user code
```

**Generic functions** (per log call with 3 methods):
```
GF dispatch:           ~30ns
Method 1 call:         ~20ns + user code
Method 2 call:         ~20ns + user code
Method 3 call:         ~20ns + user code
Total overhead:        ~90ns + user code
```

**But**: Generic functions require EQL specializers for per-instance behavior:
```
EQL dispatch:          ~100-500ns (depends on implementation)
Method call:           ~20ns + user code
Total per-instance:    ~120-520ns + user code
```

### Key Insight

**For per-instance hooks** (LLOG's primary use case):
- Current hooks: ~110ns overhead
- Generic functions: ~120-520ns overhead

**For subclass hooks** (less common):
- Current hooks: ~110ns overhead (same)
- Generic functions: ~90ns overhead (slightly faster)

## Recommendations

### Stick with Current Hooks ✅

**Reasons:**

1. **Better API**: Clear, explicit, self-documenting
2. **Better performance**: Faster for per-instance use case (~2-5x)
3. **Better dynamic modification**: Easy add/remove/clear
4. **Better ordering**: Explicit priority control
5. **Better error handling**: Named hooks, isolation built-in
6. **Better introspection**: Simple, no MOP required
7. **Better composability**: First-class functions, closures
8. **Practical**: Solves the problem simply and efficiently

### When Generic Functions Would Be Better

Generic functions would be superior if:

1. **Most hooks are subclass-based**: Behavior tied to logger types, not instances
2. **Global hooks dominate**: One set of hooks for all loggers
3. **Method combination is needed**: Complex :before/:after/:around patterns
4. **Type-based dispatch**: Hooks based on entry type, level, etc.
5. **Heavy CLOS usage**: Project already deeply invested in CLOS patterns

**None of these apply strongly to LLOG.**

### Hybrid Approach (Not Recommended)

You *could* support both:

```lisp
;; Keep current hooks
(add-hook logger :pre-log #'validate)

;; Also call generic function
(defgeneric user-pre-log-hook (logger entry)
  (:method (logger entry) entry))

;; In log-entry:
(setf entry (%call-pre-log-hooks logger entry))     ; Current hooks
(setf entry (user-pre-log-hook logger entry))       ; Generic function
```

**But this adds complexity with little benefit.**

### Minor Improvements to Current Implementation

Instead of switching to generic functions, consider:

1. **Hook composition helpers**:
```lisp
(defun compose-hooks (&rest hooks)
  "Compose multiple hook functions into one."
  (lambda (logger entry)
    (reduce (lambda (entry hook)
              (funcall hook logger entry))
            hooks
            :initial-value entry)))
```

2. **Conditional hooks**:
```lisp
(defun make-conditional-hook (predicate hook-fn)
  "Only call hook-fn when predicate returns true."
  (lambda (logger entry)
    (if (funcall predicate logger entry)
        (funcall hook-fn logger entry)
        entry)))
```

3. **Hook metadata**:
```lisp
(defstruct hook
  ...
  (metadata nil :type list)  ; Arbitrary key-value pairs
  (enabled t :type boolean)) ; Enable/disable without removing
```

## Conclusion

### Key Takeaways

1. **Current hooks are well-designed** for LLOG's use case
2. **Generic functions would be slower** (2-5x overhead)
3. **Generic functions would be harder to use** for dynamic modification
4. **Generic functions lack critical features** (priority, error isolation, names)
5. **Hooks are more practical** even if less "idiomatic"

### The Bottom Line

**Keep the current hooks implementation.**

It's:
- ✅ Faster
- ✅ Simpler to use
- ✅ More flexible
- ✅ Better suited to logging's event-driven nature
- ✅ Easier to understand and debug

Generic functions are a powerful tool, but they're not the right tool for this job. Sometimes the straightforward approach is the best approach.

### Design Wisdom

> "Use the right tool for the job, not the most sophisticated tool."

LLOG's hooks are:
- Simple enough to understand in 5 minutes
- Powerful enough for all realistic use cases
- Fast enough to be negligible overhead
- Flexible enough for runtime modification

That's good design. Don't overcomplicate it.

## References

- LLOG hooks implementation: `src/hooks.lisp`
- Logger integration: `src/logger.lisp:150-209`
- CLOS specification: ANSI Common Lisp, Chapter 7
- Closer-MOP documentation: https://github.com/pcostanza/closer-mop
- Practical Common Lisp, Chapter 21: Programming in the Large: Packages and Symbols
