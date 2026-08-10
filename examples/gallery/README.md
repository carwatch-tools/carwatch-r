# CARWatch R example gallery

The gallery mirrors the focused Python example workflows with R-native APIs.
Every `.Rmd` file is executable and creates its inputs below `tempdir()`.

## Research workflow

1. [Load participant folders and inspect source provenance](01-research-workflow/01-load-carwatch-logs.Rmd)
2. [Resolve conversion issues through a CSV report](01-research-workflow/02-resolve-conversion-issues-in-spreadsheet.Rmd)
3. [Resolve conversion issues interactively](01-research-workflow/03-resolve-conversion-issues-interactively.Rmd)
4. [Assess and filter sampling compliance](01-research-workflow/04-assess-and-filter-sampling-compliance.Rmd)
5. [Inspect individual sampling timing](01-research-workflow/05-inspect-individual-sampling-timing.Rmd)
6. [Merge positional saliva measurements](01-research-workflow/06-merge-saliva-measurements.Rmd)
7. [Compute cortisol response features](01-research-workflow/07-compute-cortisol-features.Rmd)

## Protocol variants

- [Reconstruct a multi-registration protocol](02-protocol-variants/01-reconstruct-multi-registration-protocol.Rmd)

## Example data

- [Generate local CARWatch example data](03-example-data/01-generate-local-example-data.Rmd)

Render the complete catalogue from the package root:

```sh
Rscript tools/render_examples.R
```
