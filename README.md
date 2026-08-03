# CARWatch for R

[![R-CMD-check](https://github.com/carwatch-tools/carwatch-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carwatch-tools/carwatch-r/actions/workflows/R-CMD-check.yaml)
[![R 4.3+](https://img.shields.io/badge/R-4.3%2B-276DC3)](https://cran.r-project.org/)
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE.md)

CARWatch reconstructs app-recorded saliva-sampling studies and combines their
sampling-quality information with laboratory biomarkers. Raw log events remain
immutable evidence, registrations define protocol structure, corrections are
second-pass decisions, and output retains timing and source provenance.

This is a native R port of Python carwatch 1.0.0. It is usable for the workflow
below but is not yet behavior-complete. See [PORT_STATUS.md](PORT_STATUS.md)
for the explicit parity boundary.

- Import CSV logs, ZIP archives, and participant-folder exports.
- Reconstruct canonical study days, sampling positions, and timing compliance.
- Create and reload auditable conversion-issue reports.
- Read and write the Python-compatible three-header Study Results CSV format.
- Import flat Study Manager exports.
- Merge laboratory values by physical tube ID or protocol position.
- Compute cortisol response features and static quality-control plots.

## Installation

CARWatch requires R 4.3 or newer. Install R from [CRAN](https://cran.r-project.org/)
or, on macOS, with Homebrew:

~~~sh
brew install r
~~~

The initial release is GitHub-only. After the repository is published:

~~~r
install.packages("remotes")
remotes::install_github("carwatch-tools/carwatch-r")
~~~

To install this checkout locally:

~~~sh
R CMD INSTALL .
~~~

~~~r
install.packages("remotes")
remotes::install_local(".")
library(carwatch)
~~~

## Typical workflow

Sample IDs and filenames are opaque audit values. Registration metadata and its
ordered saliva_ids define the protocol positions.

### 1. Load raw CARWatch logs

For one folder per participant, provide the participant IDs explicitly. Folders
can contain nested CSV exports and ZIP archives.

~~~r
participant_dirs <- c(
  vp01 = "data/carwatch/vp01",
  vp02 = "data/carwatch/vp02"
)
imported <- read_raw_logs_from_participant_dirs(
  participant_dirs,
  create_report = TRUE
)
raw_logs <- imported$raw_logs
source_audit <- imported$source_audit
~~~

The source audit records every selected or excluded source, selection reason,
and event count. When exact sources are already known:

~~~r
raw_logs <- read_raw_logs(c(
  "data/carwatch/vp01.csv",
  "data/carwatch/vp02.zip"
))
~~~

### 2. Reconstruct study days and create an advisory report

The initial conversion reconstructs registrations, canonical days, scheduled
samples, and compliance fields. Warning mode returns usable output plus
unresolved anomalies.

~~~r
initial <- convert_raw_logs(
  raw_logs,
  errors = "warn",
  create_report = TRUE
)
initial$report$summary
write.csv(initial$report$issues, "conversion_issues.csv", row.names = FALSE)
~~~

The prefilled accept values are recommendations. They do not modify raw events
or execute corrections until the report is supplied to a second pass.

### 3. Review and apply decisions

Review the CSV in a spreadsheet, preserving its issue IDs and context, then
reload it for strict conversion.

~~~r
decisions <- read_conversion_report("conversion_issues.csv")
final <- convert_raw_logs(
  raw_logs,
  errors = "raise",
  create_report = TRUE,
  issue_decisions = decisions
)
study_results <- final$results
~~~

The implemented report workflow validates supported decisions and scope-drop
decisions. Full Python-equivalent supersession, manual-diary patches, and all
change actions remain parity work; consult [PORT_STATUS.md](PORT_STATUS.md)
before using those paths.

Inspect protocol reconstruction where needed:

~~~r
summarize_protocol(raw_logs)
extract_registration_schedule(raw_logs)
~~~

### 4. Inspect and save complete Study Results

The carwatch_results object is a tibble with a reversible day, sample, variable
column specification. It represents the Python-compatible complete wide result
layout.

~~~r
study_days <- as_study_days(study_results)
sample_events <- as_sample_events(study_results)
write_study_results(study_results, "study_results.csv")

analysis_results <- drop_non_compliant_samples(study_results)
summarize_compliance(study_results)
find_non_compliant_samples(study_results)
~~~

Study days include collection dates, awakening information, registration
context, and day compliance. Sample events include actual and expected times,
sample positions, recorded physical IDs, deviations, and sample compliance.

### 5. Restore results or import a Study Manager export

~~~r
study_results <- read_study_results("study_results.csv")
display_results <- read_study_results("study_results.csv", simple = TRUE)
study_results <- read_study_manager_export("study_manager_export.csv")
~~~

Use simple results only for display. They intentionally cannot be merged,
analysed, or plotted because required provenance is absent.

### 6. Load laboratory saliva data

For physical-ID matching, use a long CSV with participant, sample, and one
biomarker column:

~~~csv
participant,sample,cortisol
vp01,tube-a,8.2
vp01,tube-b,12.6
~~~

~~~r
saliva <- read_saliva("cortisol.csv", saliva_type = "cortisol")
~~~

For position-based laboratory data:

~~~r
saliva_by_position <- data.frame(
  participant = c("vp01", "vp01"),
  day = c("D1", "D1"),
  sample_position = c(1L, 2L),
  cortisol = c(8.2, 12.6)
)
~~~

### 7. Merge sampling and laboratory data

Recorded physical IDs correct documented swaps by default. Set correct_swaps to
FALSE to match scheduled IDs instead.

~~~r
merged_results <- merge_saliva(
  study_results,
  saliva,
  match_on = "sample",
  correct_swaps = TRUE
)

merged_results <- merge_saliva(
  study_results,
  saliva_by_position,
  match_on = "position"
)
~~~

The result remains complete canonical Study Results and includes laboratory
availability, recorded-sample, and swap-correction provenance.

### 8. Compute features and quality-control plots

The CARWatch feature adapter groups by participant and day, orders samples by
sample_position, and uses actual sampling times.

~~~r
cortisol_features <- compute_features_from_carwatch(
  merged_results,
  saliva_type = "cortisol"
)

plot_sampling_timeline(study_results, participant = "vp01", day = "D1")
plot_compliance_overview(study_results)
plot_timing_deviation(study_results)
plot_saliva_curve(merged_results, value = "cortisol")
~~~

For generic long-format saliva data, use compute_features, auc, max_value,
initial_value, max_increase, or slope. Interactive Shiny/DT tools are planned
and not part of this release.

## Synthetic data

Generate deterministic local example data:

~~~r
path <- generate_synthetic_study_data(
  "carwatch-example",
  n_participants = 4,
  random_state = 42
)
~~~

The generated directory contains raw logs and a cortisol CSV suitable for the
workflow above.

## Citation

Report the package version used in an analysis. For research using CARWatch,
cite:

> Richer, R., Abel, L., Küderle, A., Eskofier, B. M., & Rohleder, N. (2023).
> CARWatch — A smartphone application for improving the accuracy of cortisol
> awakening response sampling. Psychoneuroendocrinology, 151, 106073.
> https://doi.org/10.1016/j.psyneuen.2023.106073

## Development

~~~sh
R CMD INSTALL .
Rscript -e 'testthat::test_local(reporter = "summary")'
R CMD build .
R CMD check --no-manual carwatch_*.tar.gz
~~~

Python 1.0.0 is a development-only oracle. Regenerate checked-in fixtures from
the adjacent Python checkout:

~~~sh
uv run python ../carwatch-r/tools/generate_python_fixtures.py
~~~

Do not claim full Python parity until [PORT_STATUS.md](PORT_STATUS.md) is
complete.

## License

CARWatch for R is available under the [MIT License](LICENSE.md).
