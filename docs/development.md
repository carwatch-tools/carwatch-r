# Development

This page is for CARWatch package contributors and maintainers. It is not
needed to analyse a study or run the user tutorials. For those, see
[CARWatch tutorials](tutorials.md).

## Local package checks

Run these commands from the package root after changing package code:

```sh
R CMD INSTALL .
Rscript -e 'testthat::test_local(reporter = "summary")'
R CMD build .
R CMD check --no-manual carwatch_*.tar.gz
```

## Validate all R Markdown examples

The following command renders all end-to-end and gallery examples in temporary
directories. It is a development and CI check, not a normal user workflow:

```sh
Rscript tools/render_examples.R
```

Interactive examples use `launch = FALSE` during rendering. The script checks
that each Shiny app can be constructed without starting a server.

## Regenerate README figures

The README plot images are generated from a deterministic synthetic study:

```sh
Rscript tools/generate_readme_figures.R
```

The script loads the current package source and replaces the four PNG files in
`docs/images/`. Inspect the generated figures before committing them.

## Cross-implementation checks

CARWatch is validated against `carwatch-python` 1.0.0 during development.
Regenerate checked-in fixtures from the adjacent Python checkout when a
supported semantic contract changes:

```sh
cd ../carwatch-python
uv run python ../carwatch-r/tools/generate_python_fixtures.py
uv run python ../carwatch-r/tools/check_python_test_inventory.py ../carwatch-r/tests
```

The inventory gate maps the reference tests to R contract suites.

## Continuous integration

CI checks R 4.3 and the current R release on Linux, macOS, and Windows. It
runs package checks, tests, coverage, fixture validation, pkgdown, and Shiny
smoke tests.
