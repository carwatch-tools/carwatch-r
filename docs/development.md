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

## Release workflow

The `Release` GitHub Actions workflow separates CRAN preparation from the
public GitHub release.

Before submitting a version:

1. Set the same release version in `DESCRIPTION` and `CITATION.cff`.
2. Start `NEWS.md` with `# carwatch <version>` and update
   `cran-comments.md`.
3. Push the changes to `main` and wait for the normal package checks.
4. In GitHub, open **Actions > Release > Run workflow**. The workflow checks
   the package on macOS, Linux, and Windows, then provides
   `carwatch-source-package` as a downloadable source archive.
5. Upload that `.tar.gz` archive to win-builder and then to CRAN. CRAN still
   requires the maintainer to confirm the submission by email.

After CRAN accepts the version, create and push its tag:

```sh
git tag -a v1.0.1 -m "carwatch 1.0.1"
git push origin v1.0.1
```

Replace `1.0.1` with the version in `DESCRIPTION`. A tag must be exactly
`v<version>`. Pushing it reruns the release checks, builds the source archive,
creates the GitHub Release, and attaches the archive. If any check fails, no
GitHub Release is published.

CRAN submission itself is intentionally not performed by the workflow. It is
a reviewed submission with an email-confirmation step, whereas the GitHub
release can be reproduced safely from the accepted tag.
