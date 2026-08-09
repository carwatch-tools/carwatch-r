test_that("Python 1.0.0 canonical result fixtures round-trip semantically", {
  root <- testthat::test_path("..", "..", "inst", "extdata", "parity", "v1.0.0")
  testthat::skip_if_not(fs::file_exists(fs::path(root, "manifest.json")))
  manifest <- jsonlite::read_json(fs::path(root, "manifest.json"), simplifyVector = TRUE)
  expect_identical(manifest$oracle_version, "1.0.0")
  results <- read_study_results(fs::path(root, "results.csv"))
  restored <- tempfile(fileext = ".csv")
  write_study_results(results, restored)
  reread <- read_study_results(restored)
  expect_equal(as_sample_events(reread), as_sample_events(results))
  expect_equal(as_study_days(reread), as_study_days(results))
})

test_that("parity fixture source audit and laboratory merge are accepted", {
  root <- testthat::test_path("..", "..", "inst", "extdata", "parity", "v1.0.0")
  testthat::skip_if_not(fs::file_exists(fs::path(root, "results.csv")))
  results <- read_study_results(fs::path(root, "results.csv"))
  saliva <- read_saliva(fs::path(root, "saliva.csv"))
  merged <- merge_saliva(results, saliva)
  expect_true(all(c("cortisol", "lab_value_available", "sampling_event_recorded") %in% names(as_sample_events(merged))))
})

test_that("raw fixture reconstructs the Python canonical schema", {
  root <- testthat::test_path("..", "..", "inst", "extdata", "parity", "v1.0.0")
  testthat::skip_if_not(fs::dir_exists(fs::path(root, "raw")))
  expected <- read_study_results(fs::path(root, "results.csv"))
  imported <- read_raw_logs_from_participant_dirs(
    setNames(fs::path(root, "raw", "VP01"), "VP01"),
    create_report = TRUE
  )
  actual <- convert_raw_logs(imported$raw_logs, errors = "warn", create_report = TRUE)
  expect_equal(unname(attr(actual$results, "column_spec")$variable), unname(attr(expected, "column_spec")$variable))
  expect_equal(unname(as.data.frame(as_sample_events(actual$results)[c("participant", "day", "sample", "sample_position", "recorded_sample")])), unname(as.data.frame(as_sample_events(expected)[c("participant", "day", "sample", "sample_position", "recorded_sample")])))
  expect_equal(actual$report$summary$issue_count, 0L)
  expect_true(all(imported$source_audit$status == "selected"))
})

test_that("conversion reports match Python 1.0.0 field for field", {
  root <- testthat::test_path("..", "..", "inst", "extdata", "parity", "v1.0.0")
  manifest <- jsonlite::read_json(fs::path(root, "manifest.json"), simplifyVector = TRUE)
  expect_gte(manifest$fixture_schema, 2)
  for (scenario in manifest$conversion_scenarios) {
    scenario_root <- fs::path(root, "conversion_scenarios", scenario)
    raw <- read_raw_logs_from_participant_dirs(stats::setNames(fs::path(scenario_root, "raw", "p1"), "p1"))
    converted <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
    expected <- readr::read_csv(
      fs::path(scenario_root, "issues.csv"),
      col_types = readr::cols(.default = readr::col_character()),
      na = character(), show_col_types = FALSE
    )
    actual <- converted$report$issues
    for (column in names(expected)) actual[[column]] <- as.character(actual[[column]])
    actual[is.na(actual)] <- ""
    expect_identical(actual[names(expected)], expected, info = scenario)
    expected_summary <- jsonlite::read_json(fs::path(scenario_root, "summary.json"), simplifyVector = TRUE)
    expect_equal(unname(unlist(converted$report$summary[names(expected_summary)])), unname(unlist(expected_summary)), info = scenario)
    expected_results <- read_study_results(fs::path(scenario_root, "results.csv"))
    actual_spec <- attr(converted$results, "column_spec")[c("day", "sample", "variable")]
    expected_spec <- attr(expected_results, "column_spec")[c("day", "sample", "variable")]
    for (column in names(actual_spec)) {
      actual_spec[[column]] <- unname(actual_spec[[column]])
      expected_spec[[column]] <- unname(expected_spec[[column]])
    }
    expect_identical(actual_spec, expected_spec, info = scenario)
  }
})
