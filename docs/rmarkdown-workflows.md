# R Markdown workflows

The package contains four end-to-end walkthroughs and nine focused gallery
workflows. Together they mirror the executable Python example catalogue using
R-native tibbles, `ggplot2`, Shiny, and DT.

## End-to-end walkthroughs

- [`examples/01-log-processing.Rmd`](../examples/01-log-processing.Rmd): raw
  log import, source audit, advisory conversion, submitted decisions, manual
  diary, complete Study Results, and timing quality control.
- [`examples/02-saliva-analysis.Rmd`](../examples/02-saliva-analysis.Rmd):
  completed conversion, position-based cortisol merge, compliance filtering,
  response features, and static plots.
- [`examples/03-plotting.Rmd`](../examples/03-plotting.Rmd): individual timing,
  cohort compliance, deviations, and cortisol-response plots.
- [`examples/04-synthetic-study-interactive-log-processing.Rmd`](../examples/04-synthetic-study-interactive-log-processing.Rmd):
  synthetic input generation and nonblocking construction of the Shiny/DT
  conversion editor and interactive timeline.

## Focused gallery

The [gallery index](../examples/gallery/README.md) links all focused examples:

1. Load participant folders and inspect source provenance.
2. Resolve conversion issues through a CSV report.
3. Resolve conversion issues interactively.
4. Assess and filter sampling compliance.
5. Inspect individual sampling timing.
6. Merge positional saliva measurements.
7. Compute cortisol response features.
8. Reconstruct a multi-registration protocol with an ordered manifest.
9. Generate deterministic local example data.

## Install prerequisites

Install R 4.3 or newer. On macOS, `brew install r` installs R. Install this
package, the renderer, and the optional dependencies used by the interactive
examples:

```r
install.packages(c("remotes", "rmarkdown", "shiny", "DT"))
remotes::install_github("carwatch-tools/carwatch-r")
```

For a local checkout:

```r
install.packages("remotes")
remotes::install_local(".")
```

`rmarkdown` installs `knitr`. HTML rendering requires Pandoc; RStudio bundles
it, and standalone installations can use the Pandoc package from its official
distribution.

## Render an example

Render one document from the package root:

```r
rmarkdown::render("examples/01-log-processing.Rmd")
```

Render and validate all 13 documents:

```sh
Rscript tools/render_examples.R
```

Interactive documents use `launch = FALSE` while rendering. This constructs
and validates each `shiny.appobj` without starting a blocking server. Run the
explicit launch chunks in an interactive R session to open the applications.

Each document creates its data below `tempdir()` and prints that path. The
source logs, advisory report, submitted report, manual diary, Study Results,
and analysis CSV files are retained there for inspection during the R session.
Change `study_dir` in the setup chunk to a controlled project directory when
the artifacts must persist.

## Adapt the log-processing workflow

Replace the synthetic-data chunk with an explicit named mapping. Names are
participant IDs. Values are folders containing their raw CARWatch exports.

```r
participant_dirs <- c(
  vp01 = "data/raw/vp01",
  vp02 = "data/raw/vp02"
)
imported <- carwatch::read_raw_logs_from_participant_dirs(
  participant_dirs,
  create_report = TRUE
)
```

Run the first conversion with `errors = "warn"` and `create_report = TRUE`.
Save `advisory$report$issues` unchanged. Review only `user_decision` and
`user_decision_value`; `issue_id`, message, details, and protocol context are
the stable audit identity. Load the edited CSV through
`read_conversion_report()` and run the second conversion with `errors =
"raise"`.

Do not alter raw log events to correct data. Record corrections through the
second-pass decisions and manual diary. Sample IDs and filenames remain opaque;
protocol position comes from registration metadata.

## Adapt the saliva-analysis workflow

Use `match_on = "sample"` when laboratory rows contain the physical tube ID:

```r
saliva <- carwatch::read_saliva("data/cortisol.csv", saliva_type = "cortisol")
merged <- carwatch::merge_saliva(results, saliva, match_on = "sample")
```

Use `match_on = "position"` only when the laboratory table explicitly has
`participant`, `day`, and `sample_position`. Keep the returned
`carwatch_results` object complete. `simple = TRUE` is a display-only import
and cannot be used for merging, compliance, features, or plots.

## Reproducibility boundary

Archive all of the following with the analysis: raw exports, source audit,
first-pass issue report, edited decision report, manual diary, complete Study
Results CSV, laboratory input, package version, and rendered R Markdown HTML.
This preserves the evidence and every correction needed to reproduce the final
analysis.
