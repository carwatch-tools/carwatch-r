## Resubmission

This is a resubmission. In this version I have:

- replaced the permanently redirected RStudio Desktop URL in the README and
  tutorial vignette with the current Posit documentation URL.

## Test environments

- Local: macOS 26.6, R 4.6.1
- GitHub Actions: macOS, Ubuntu, and Windows with R 4.3 and R-release
- GitHub Actions: Ubuntu with R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new submission.

## New submission

This is the first CRAN submission of `carwatch`.

The package reads only files supplied explicitly by the user. Examples and
tests use bundled or temporary data and do not write to the user's home
directory. Interactive Shiny components are optional and are never started by
examples or package checks.

The words flagged by the spell checker are intentional:

- "CARWatch" is the name of the software.
- "auditable" and "barcode" are standard terms describing the package's
  processing workflow.
- "et al." is part of the bibliographic citation.
