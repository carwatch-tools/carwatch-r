# Port status

This repository is a native R implementation in active parity development. It is not yet a
behavior-complete replacement for `carwatch-python` 1.0.0.

Implemented and locally tested:

- Conventional MIT package metadata, roxygen documentation, namespace
  declarations, and a clean local `R CMD check --no-manual`.
- Python 1.0.0 development-oracle fixture generator plus versioned canonical
  results, issue-report, source-audit, merge, compliance, and feature fixtures.
- Direct CSV/ZIP loading and deterministic one-folder-per-participant source
  selection, including nested archive members, hidden files, duplicate logical
  exports, mismatch exclusions, and source audits.
- Canonical three-header Study Results read/write, flat Study Manager imports,
  display-only protection, canonical variable ordering, and typed restoration.
- Registration-aware standard conversion with raw-event de-duplication,
  registration-epoch source provenance, strict ambiguous/cyclic/backwards
  protocol-order checks, manifest validation, re-registration reporting, and
  canonical schedule reconstruction.
- Two-pass conversion reports with deterministic issue IDs, stale-decision
  rejection, scope drops, re-registration overrides, collection-date mapping,
  scan-time ordering, manual-diary patches, and relative/absolute fallback
  schedules.
- Compliance summaries, saliva merging with physical-tube and positional swap
  correction, response features, synthetic data, and static plots.

Required before claiming Python 1.0 parity:

- Exact Python 1.0.0 conversion-report identity/message/context equivalence,
  including occurrence suffixes and all supersession cases.
- Full Python merge metadata/index-level semantics and the complete
  synthetic-data configuration surface.
- Shiny/DT timeline and conversion-report editor.
- Translation of the remaining Python behavioral suite, versioned differential
  fixtures for each path, and CI on macOS, Linux, and Windows.
