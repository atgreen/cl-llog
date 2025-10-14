# llog-audit: Tamper-Evident Audit Trails

**Status:** 🚧 **Under Development** - Basic skeleton in place

This is an optional extension to the llog logging framework that provides **tamper-evident** (not tamper-proof) audit trails using cryptographic hash chaining.

## What is Tamper-Evident?

**Tamper-evident** means the system can **detect** unauthorized modifications after the fact, but does not **prevent** them. If someone modifies a log entry, the hash chain will break and verification will reveal the tampering.

**True tamper-proofing** requires additional measures:
- Write-once media (WORM drives)
- Immediate external replication
- Hardware security modules (HSM)
- OS-level protections (append-only files)

## Architecture

The audit system uses:
1. **Hash Chaining**: Each entry includes hash of previous entry
2. **Periodic Checkpoints**: Merkle roots with optional signatures every N records
3. **Verification Tool**: Detects breaks in the chain

## Dependencies

The `llog-audit` system adds these dependencies (isolated from core `llog`):
- **ironclad**: Cryptographic hashing (SHA-256, SHA-512, SHA3)
- **cl-base64**: Base64 encoding for hashes
- **babel**: String encoding (transitive dependency)

## Installation

```lisp
;; Load the audit extension
(ql:quickload :llog-audit)
```

## Usage

```lisp
;; Create an audit output
(llog:add-output *logger*
  (llog-audit:make-audit-output "audit.log"
    :algorithm :sha256
    :checkpoint-interval 1000
    :metadata '(:system "payment-service" :version "1.0")))

;; Log normally - hashes are computed automatically
(llog:info "Payment processed"
  :user-id 123
  :amount 99.99
  :transaction-id "txn-abc")

;; Verify audit log integrity (TODO: not yet implemented)
(llog-audit:verify-audit-file "audit.log")
;; => (:status :valid :records 10000 :checkpoints 10)
```

## File Format

```
{header: version, algorithm, checkpoint-interval, metadata}
{entry: timestamp, level, message, fields, hash}
{entry: timestamp, level, message, fields, hash}
...
{checkpoint: record-count, merkle-root, signature}
{entry: timestamp, level, message, fields, hash}
...
```

## TODO

- [ ] Complete verification implementation
- [ ] Add digital signature support
- [ ] Add CLI tool for offline verification
- [ ] Performance testing with large files
- [ ] Support for streaming verification
- [ ] Export to compliance formats (PDF, CSV)

## Compliance Use Cases

This feature is designed for compliance requirements that mandate tamper detection:
- **SOC 2**: Security audit trails
- **ISO 27001**: Information security logging
- **SOX**: Financial transaction logs
- **HIPAA**: Healthcare access logs
- **PCI DSS**: Payment system audit trails

## Status

This is a skeleton implementation created on 2025-10-14. The hash chain implementation is functional, but verification, signatures, and full integration testing are still needed.
