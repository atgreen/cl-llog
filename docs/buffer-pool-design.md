# Buffer Pool & Zero-Allocation Strategy (Phase 3.1)

**Date:** 2025-10-13  
**Author:** Codex (with async outputs complete)

## 1. Goals

1. Eliminate avoidable allocations on the typed logging hot path (target: 0 conses per call on SBCL when logging at enabled levels without I/O).
2. Provide reusable buffers for encoders (JSON, S-expression, console) to avoid `with-output-to-string` churn and temporary string creation.
3. Support both synchronous and async outputs without changing their external API.
4. Maintain portability across SBCL, CCL, ECL (avoid implementation-specific APIs except where feature-gated).

## 2. Key Observations

- `make-log-entry` currently allocates:
  - the `log-entry` struct (required),
  - a new list for fields (`list` in typed API macro),
  - temporary strings for encoder output (e.g., `with-output-to-string`).
- Typed fields (`llog:int` etc.) already return structs; main remaining allocations are the fields list and encoder buffers.
- JSON encoder builds a full string before writing to outputs; block-buffered file output buffers again, meaning double buffering.
- Async output enqueues whole `log-entry` structs; reusing them between calls would complicate concurrency and should be avoided.

## 3. Proposed Architecture

### 3.1 Buffer Pool Component

```lisp
(defstruct (byte-buffer (:constructor make-byte-buffer (vector fill-pointer)))
  vector   ; simple-array (unsigned-byte 8) or character
  fill-pointer)

(defstruct buffer-pool
  (lock (bt:make-lock "llog/buffer-pool"))
  (buffers '())          ; stack of reusable buffers
  (element-type 'character)
  (buffer-size 8192)
  (max-buffers 32))
```

- Global pool for process-wide reuse: `*encoder-buffer-pool*` (character streams) and optional `*json-byte-pool*` (when we move to octet encoding).
- Thread-local cache (`*thread-buffer*`) via `sb-ext:*dynamic-extent*` or simple special binding to avoid lock contention.
- Acquisition API:
  ```lisp
  (defun acquire-buffer (&optional (pool *encoder-buffer-pool*)) ...)
  (defun release-buffer (buffer &optional pool) ...)
  ```

### 3.2 Encoder Integration

- Replace `with-output-to-string` in `encode-entry` with buffer usage:
  - Acquire buffer → encode → write directly to stream/output.
  - For outputs needing a string (e.g., block-buffer file output), supply vector+length to new helper `write-buffer`.
- JSON encoder to emit directly into provided stream; if we still need a temporary string (e.g., underlying output expects string), wrap buffer in `make-string-output-stream` backed by existing vector.
- Provide utility `with-encoder-buffer ((stream buffer) &body body)` macro.

### 3.3 Field List Allocation

- Move typed API macros from `(list ,@field-forms)` to use pre-allocated simple vector or stack allocation:
  - `make-log-entry` accepts `fields` as simple-vector plus count.
  - Convert `log-entry` slot to store simple vector; encoders iterate using `dotimes`.
  - For sugared API, continue building list (non-zero allocation acceptable).
- Introduce `with-field-array` macro used by typed macros:
  ```lisp
  (with-field-array (fields count 8)
    (setf (svref fields count) field-form) ...)
  ```
  - Backed by dynamically-sized stack array (via `locally` + `declare (dynamic-extent ...)`) on SBCL; fallback to simple vector + `make-array` with `adjustable nil` for portability.

### 3.4 Output Changes

- `file-output` block buffer currently uses `make-string-output-stream`; swap to pool-backed buffer.
- `async-output` remains unchanged; it enqueues entries, not buffer data.
- Provide `flush-output` override to ensure pooled buffers are returned after write.

## 4. API Changes

| Area | Current | Proposed |
|------|---------|----------|
| `log-entry` fields slot | `list` | simple-vector + count (new slot `fields-count` or reuse `length` info) |
| `make-log-entry` signature | `(level message &key logger-name fields)` | add `fields-count` keyword (default `(length fields)` for backward compatibility) |
| Encoders | Expect list of fields | Update to iterate using vector access helpers |
| Typed macros | Construct lists | Use `with-field-array`, returning vector+count |

Backward compatibility: keep accepting lists (coerce to vector when needed) so existing APIs/tests continue to work during transition.

## 5. Concurrency & Lifecycle

- Pools are optional; if acquisition fails (e.g., pool empty and hitting max), fall back to allocating fresh buffers.
- `release-buffer` invoked via `unwind-protect` to avoid leaks on errors.
- Thread-local fast path: store buffer in special `*thread-encoder-buffer*` for reuse by same thread without locking.

## 6. Testing Strategy

1. **Unit tests**
   - Verify encoders produce identical output before/after change.
   - Confirm `make-log-entry` accepts both vector+count and list inputs.
2. **Allocation smoke test**
   - SBCL `room` / `sb-ext:gc-run-stats` or `sb-profile` to ensure typed API logs allocate 0 bytes per call.
3. **Pool reuse**
   - Assert buffers are returned on `flush-output`.
4. **Regression tests**
   - Async output interactions ensure no double release.

## 7. Rollout Plan

1. Implement buffer structs + pool (`src/buffer-pool.lisp` new module). Wire into ASDF.
2. Update `log-entry` fields representation (vector+count) with backward-compatible path.
3. Refactor typed API macros to use new helper.
4. Migrate encoders and outputs to consume vectors.
5. Update tests and add allocation assertions (gated behind `#+sbcl`).
6. Document configuration knobs (pool size, buffer length) in README/PLAN.

## 8. Open Questions

- Should the pool size be configurable at runtime (env vars / dynamic variables)?
- Do we need separate pools per encoder type (JSON vs console) due to different size profiles?
- Investigate using `flexi-streams` or direct octet arrays for JSON to avoid character conversion costs.

---

**Next Step:** Implement buffer pool module and refactor typed API to consume vector-based fields (Phase 3.1 step 2).
