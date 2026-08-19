# Port status

CARWatch for R 1.0.0 is the behavior-complete native R migration of
`carwatch-python` 1.0.0. Parity means identical semantic results, validation
outcomes, issue-report content, provenance, and three-header Study Results CSV
interchange. It does not mean identical pandas, Matplotlib, or Jupyter object
types.

## Migrated behavior

- CSV, ZIP, nested-member, and participant-folder raw-log import with source
  selection, de-duplication, exclusions, and source audits.
- Registration-epoch reconstruction, cohort protocol resolution, manifests,
  re-registration overrides, canonical schedules, and strict DST handling.
- All 14 conversion issue codes, deterministic identities and occurrence
  suffixes, two-pass decisions, supersession, stale-decision rejection, scope
  drops, schedule fallbacks, and manual-diary patches.
- Complete/display Study Results boundaries, flat Study Manager import, typed
  restoration, and lossless Python-generated three-header CSV round-trips.
- Physical-ID and positional saliva merging, swap correction, arbitrary
  laboratory metadata, compliance, sampling anomalies, response features, and
  static quality-control plots.
- Registration-aware synthetic studies and configurable anomalies.
- Interactive participant/day timelines and conversion-report decisions through
  optional Shiny and DT components, including keyboard row navigation and
  refreshes that isolate unavailable manual-diary decisions.

## R-native representations

- Complete results use a `carwatch_results` S3 tibble with a reversible
  `(day, sample, variable)` column specification instead of pandas MultiIndex
  columns.
- Additional pandas index levels become validated R columns and are classified
  globally as participant, participant-day, or sample metadata.
- Static plots return ggplot2 objects; interactive notebook widgets are Shiny
  applications and DT tables.
- Python exceptions and warnings become stable `carwatch_*` R condition classes.

## Parity evidence

- The 216 named Python 1.0.0 test functions are pinned by module, count, and
  test-name hash and mapped to focused `testthat` contract suites.
- Versioned fixtures cover canonical results, raw-log source audits, exact
  conversion reports, saliva merging, compliance, and response features.
- CI regenerates fixtures using the pinned Python v1.0.0 package, rejects drift,
  runs `R CMD check --as-cran` on R 4.3 and current R across Linux, macOS, and
  Windows, runs coverage, and builds the pkgdown site.
