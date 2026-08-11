# CARWatch tutorials

This page is for people using CARWatch for a study. It explains how to run the
executable R Markdown tutorials. Package checks and continuous integration are
documented separately in [Development](development.md).

## Before you start

Install R 4.3 or newer, then install RStudio Desktop. R is the language that
runs CARWatch; RStudio is the desktop application used to edit, run, and render
the tutorials. Install R first, then RStudio:

1. Download [R](https://cran.r-project.org/).
2. Download [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
3. Start RStudio and paste the following into its Console:

```r
install.packages("remotes")
remotes::install_github("carwatch-tools/carwatch-r")
```

To run the tutorials themselves, download or clone this repository and open
`examples/examples.Rproj` in RStudio. Install the packages needed to render the
tutorials and use the interactive apps:

```r
install.packages(c("pkgload", "rmarkdown", "shiny", "DT"))
```

Open an `.Rmd` file and use **Run All** to work through it step by step, or use
**Knit** to create an HTML report. RStudio includes the renderer needed for
knitting. You do not need to use `R CMD INSTALL .` to run a tutorial from this
repository; its setup loads the local package code. When you change package
code, rerun the setup chunk before rerunning the affected tutorial chunks.

## Choose a tutorial

| If you want to… | Start here |
| --- | --- |
| Import app logs, review issues, submit decisions, and save Study Results | [`01-log-processing.Rmd`](../examples/01-log-processing.Rmd) |
| Merge cortisol data, assess compliance, and compute response features | [`02-saliva-analysis.Rmd`](../examples/02-saliva-analysis.Rmd) |
| Create sampling timelines, compliance, deviation, and saliva-response plots | [`03-plotting.Rmd`](../examples/03-plotting.Rmd) |
| Generate synthetic data and try the Shiny tools | [`04-synthetic-study-interactive-log-processing.Rmd`](../examples/04-synthetic-study-interactive-log-processing.Rmd) |

The [focused gallery](../examples/gallery/README.md) contains shorter examples
for one task at a time, including spreadsheet-based issue review,
multi-registration protocols, saliva merging, and feature calculation.

## Use your own study data

The tutorials create temporary example data, so they can run without any input
files. For real work, replace the example paths with your own study folder and
write outputs to a controlled study directory. The log-processing tutorial
shows the complete two-pass workflow; the saliva tutorial shows both matching
by tube ID and matching by sample position.

Keep the original app exports, the file-import log, the first and final
decision reports, any manual diary, the complete Study Results CSV, laboratory
input, package version, and rendered HTML. Together they document how the
final analysis was created.

## Interactive tutorials

The rendered documents prepare the Shiny apps but do not open them
automatically. Run the explicitly marked launch chunk in an interactive R
session to open the timeline or conversion-decision editor. In the editor,
select an issue, apply a decision, then use **Refresh remaining issues** to
update the conversion. `Done` returns the complete decision table.
