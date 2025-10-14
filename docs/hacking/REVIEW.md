# LLOG Project Review – 14 Oct 2025

## Overview

I reviewed the current `llog` codebase to evaluate the structure of the logging library and how well it aligns with idiomatic Common Lisp practices. The project is well organised and feature rich, but several choices skew toward porting patterns from Go rather than embracing Lisp-specific extensibility.

## Strengths

- Clear ASDF layout keeps core code, encoders, outputs, tests, and benchmarks separated, which simplifies both consumption and extension.
- User-facing API centres on `make-logger`, `*logger*`, and the sugared/typed macros, so the public surface is easy to discover.
- Concurrency support, buffering, and async outputs demonstrate a thoughtful, production-oriented design.

## Pain Points

- `src/package.lisp` shadows common CL symbols (`string`, `float`, `error`, `trace`, `debug`, `warn`), forcing callers to manage symbol conflicts rather than offering a friendly import path.
- Typed field helpers rely on a single `field` struct with manual type tags, pushing users toward pre-defined constructors instead of extending behaviour via CLOS methods.
- Core functions frequently manipulate slots with `slot-value` while holding locks, bypassing accessor methods and method combinations that are idiomatic in Lisp.
- Several exports lack implementations (`show-recent`, `grep-logs`, `with-captured-logs`, `define-field-type`), creating a misleading public API.
- Aggressive `(optimize speed (safety 0))` declarations appear throughout hot paths, removing safety checks for library consumers by default.

## Recommendations

1. Revisit package exports: either rename conflicting symbols (e.g. `log-string`), or introduce a `llog-user` package that wraps the API without shadowing core Common Lisp functions.
2. Replace manual field tagging with a generic protocol—let callers specialise `encode-field` or introduce a `coerce-field` generic so new types integrate through CLOS rather than bespoke structs.
3. Provide macro front-ends that delay work until level checks pass (using `once-only` patterns or compiler macros) and lean on method combinations for hook chains instead of bespoke `%call-*` functions.
4. Implement the missing helper exports and deliver a `define-field-type` macro that generates the necessary generics, showcasing idiomatic macro usage.
5. Move unsafe optimisation settings into compiler macros or document how callers can opt into lowered safety, keeping the default definitions safe for general use.

These changes would keep the current performance focus while making the library feel more naturally Lispy to users.
