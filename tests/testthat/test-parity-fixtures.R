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
