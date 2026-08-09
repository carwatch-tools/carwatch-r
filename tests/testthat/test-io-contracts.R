test_that("saliva CSV reader covers biomarker and identifier validation", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("participant,sample,cortisol", " 01 , tube-a ,1.5", "02,tube-b,"), path)
  saliva <- read_saliva(path)
  expect_identical(saliva$participant, c("01", "02"))
  expect_identical(saliva$sample, c("tube-a", "tube-b"))
  expect_equal(saliva$cortisol, c(1.5, NA_real_))
  other <- tempfile(fileext = ".csv")
  writeLines(c("participant,sample,amylase", "01,tube-a,2.5"), other)
  expect_equal(read_saliva(other, "amylase")$amylase, 2.5)

  invalid_columns <- tempfile(fileext = ".csv"); writeLines(c("participant,sample,cortisol,extra", "01,a,1,x"), invalid_columns)
  expect_error(read_saliva(invalid_columns), "exactly")
  missing_id <- tempfile(fileext = ".csv"); writeLines(c("participant,sample,cortisol", ",a,1"), missing_id)
  expect_error(read_saliva(missing_id), "must not contain missing")
  non_numeric <- tempfile(fileext = ".csv"); writeLines(c("participant,sample,cortisol", "01,a,high"), non_numeric)
  expect_error(read_saliva(non_numeric), "non-numeric")
  duplicate <- tempfile(fileext = ".csv"); writeLines(c("participant,sample,cortisol", "01,a,1", "01,a,2"), duplicate)
  expect_error(read_saliva(duplicate), "duplicate")
  empty <- tempfile(fileext = ".csv"); writeLines("participant,sample,cortisol", empty)
  expect_error(read_saliva(empty), "does not contain")
  wrong_extension <- tempfile(fileext = ".txt"); writeLines(c("participant,sample,cortisol", "01,a,1"), wrong_extension)
  expect_error(read_saliva(wrong_extension), "csv")
  expect_error(read_saliva(path, ""), "non-empty")
})

test_that("manual diary reader accepts optional samples and rejects malformed keys", {
  valid <- tempfile(fileext = ".csv")
  writeLines(c("participant,day,date,awakening_time,sampling_time_1,sampling_time_2", "01,D1,2025-05-15,06:00,06:01,06:31"), valid)
  diary <- read_manual_diary(valid)
  expect_identical(diary$awakening_time, "2025-05-15 06:00")
  no_samples <- tempfile(fileext = ".csv")
  writeLines(c("participant,day,date,awakening_time", "01,D1,2025-05-15,06:00"), no_samples)
  expect_equal(nrow(read_manual_diary(no_samples)), 1L)
  gap <- tempfile(fileext = ".csv")
  writeLines(c("participant,day,date,awakening_time,sampling_time_2", "01,D1,2025-05-15,06:00,06:31"), gap)
  expect_error(read_manual_diary(gap), "contiguous")
  duplicate <- tempfile(fileext = ".csv")
  writeLines(c("participant,day,date,awakening_time", "01,D1,2025-05-15,06:00", "01,D1,2025-05-15,06:01"), duplicate)
  expect_error(read_manual_diary(duplicate), "duplicate")
  bad_day <- tempfile(fileext = ".csv")
  writeLines(c("participant,day,date,awakening_time", "01,day1,2025-05-15,06:00"), bad_day)
  expect_error(read_manual_diary(bad_day), "canonical")
})

test_that("Study Results enforce display and typed-value boundaries", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("day,D1", "sample,day", "variable,possible_reregistration", "participant,", "01,maybe"), path)
  expect_error(read_study_results(path), "boolean")
  flat <- tempfile(fileext = ".csv")
  writeLines(c("Participant ID,date_D1,sampling_time_D1_S1", "01,2025-05-15,06:00:00"), flat)
  expect_error(read_study_results(flat), "header")
  expect_error(read_study_results(path, simple = "yes"), "simple")
})

test_that("source audit summary validates its schema", {
  audit <- tibble::tibble(participant = c("p1", "p1", "p2"), status = c("selected", "excluded", "selected"), raw_event_count = c(3L, NA_integer_, 4L))
  summary <- summarize_source_audit(audit)
  expect_identical(summary$raw_log_import, c(2L, 7L, 2L))
  expect_error(summarize_source_audit(tibble::tibble(participant = "p1")), "missing required")
})

test_that("documented public migration surface is exported", {
  expected <- c(
    "read_raw_logs", "read_raw_logs_from_participant_dirs", "convert_raw_logs", "read_conversion_report", "write_conversion_report",
    "read_study_results", "write_study_results", "read_study_manager_export", "read_manual_diary", "read_saliva",
    "merge_saliva", "summarize_compliance", "find_non_compliant_samples", "drop_non_compliant_samples", "find_sampling_anomalies",
    "compute_features", "compute_features_from_carwatch", "plot_sampling_timeline", "interactive_sampling_timeline", "conversion_report_editor"
  )
  exports <- getNamespaceExports("carwatch")
  expect_true(all(expected %in% exports))
  for (name in expected) expect_true(is.function(getExportedValue("carwatch", name)), info = name)
})
