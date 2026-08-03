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
- Registration-aware standard conversion with raw-event de-duplication and
  provenance, re-registration reporting, two-pass issue-report IDs, validated
  decisions, scope-drop decisions, compliance summaries, saliva merging,
  response features, synthetic data, and static plots.

Required before claiming Python 1.0 parity:

- Full registration-epoch reconstruction across ambiguous cohort order,
  manifests, re-registration reassignment, and missing registrations.
- Complete conversion decision semantics: every issue context, cross-language
  issue-ID equivalence, supersession, manual-diary patches, and all change
  actions.
- Python merge metadata/index-level semantics and the complete synthetic-data
  configuration surface.
- Shiny/DT timeline and conversion-report editor.
- Translation of the remaining Python behavioral suite and differential CI on
  macOS, Linux, and Windows.
