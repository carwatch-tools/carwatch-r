test_that("canonical results round-trip through the three-header CSV", {
  long <- tibble::tibble(
    participant = c("vp01", "vp01", "vp01", "vp01"),
    day = c("D1", "D1", "D1", "D1"),
    sample = c("day", "day", "tube-x", "tube-x"),
    variable = c("date", "awakening_time", "sample_position", "cortisol"),
    value = list(
      as.POSIXct("2026-02-01", tz = "Europe/Berlin"),
      as.POSIXct("2026-02-01 07:00:00", tz = "Europe/Berlin"),
      1L,
      12.5
    )
  )
  original <- carwatch:::.from_long_results(long)
  path <- tempfile(fileext = ".csv")
  write_study_results(original, path)
  restored <- read_study_results(path)
  expect_s3_class(restored, "carwatch_results")
  expect_equal(as_sample_events(restored)$cortisol, 12.5)
  expect_equal(as_sample_events(restored)$sample_position, 1L)
})

test_that("synthetic logs complete the canonical conversion", {
  path <- tempfile("carwatch-synthetic-")
  generate_synthetic_study_data(path, n_participants = 2, random_state = 1)
  folders <- setNames(file.path(path, "logs", c("VP_01", "VP_02")), c("VP_01", "VP_02"))
  imported <- read_raw_logs_from_participant_dirs(folders)
  converted <- convert_raw_logs(imported, errors = "warn", create_report = TRUE)
  expect_s3_class(converted$results, "carwatch_results")
  expect_equal(nrow(as_sample_events(converted$results)), 16)
  expect_equal(nrow(converted$report$issues), 0)
})

test_that("position-indexed merging preserves the canonical representation", {
  path <- tempfile("carwatch-synthetic-")
  generate_synthetic_study_data(path, n_participants = 1, random_state = 1)
  result <- convert_raw_logs(read_raw_logs_from_participant_dirs(setNames(file.path(path, "logs", "VP_01"), "VP_01")), errors = "warn")
  samples <- as_sample_events(result)
  saliva <- dplyr::transmute(samples, participant, day, sample_position, cortisol = seq_len(dplyr::n()))
  merged <- merge_saliva(result, saliva, match_on = "position")
  expect_s3_class(merged, "carwatch_results")
  expect_false(anyNA(as_sample_events(merged)$cortisol))
  expect_true(all(c("cortisol_auc_g", "cortisol_auc_i") %in% names(compute_features_from_carwatch(merged))))
})

test_that("accepted diary decisions patch a missing awakening time", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = c("vp01", "vp01"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:05:00"), tz = timezone),
    action = c("study_metadata", "barcode_scanned"),
    payload = list(
      list(study_name = "study", saliva_ids = "s1", saliva_times = 0, study_days = 1),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "x", day_expected = 1, day_scanned = 1)
    ),
    source_file = c("one.csv", "one.csv")
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  decisions$user_decision[decisions$code == "missing_awakening_time"] <- "accept"
  diary <- tibble::tibble(participant = "vp01", day = "D1", awakening_time = "2025-05-15 06:00:00")
  patched <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions, manual_diary = diary)
  expect_false(is.na(as_study_days(patched$results)$awakening_time[[1]]))
  expect_identical(as_sample_events(patched$results)$sampling_time_source[[1]], "app")
})

test_that("accept applies a proposed sample drop", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 4),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:01:00", "2025-05-15 06:02:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned", "barcode_scanned"),
    payload = list(
      list(study_name = "study", saliva_ids = "s1", saliva_times = 0, study_days = 1),
      list(id = 0),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "a", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "b", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 4)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  expect_true(any(initial$report$issues$code == "duplicate_scheduled_sample_events"))
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = initial$report$issues)
  expect_equal(nrow(as_sample_events(final$results)), 0L)
})

test_that("a change decision sorts a complete scan series by time", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 6),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:10:00", "2025-05-15 06:20:00", "2025-05-15 06:30:00", "2025-05-15 06:40:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", rep("barcode_scanned", 4)),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2", "s3", "s4"), saliva_times = c(0, 30, 45, 60), study_days = 1),
      list(id = 0),
      list(sample_expected = "s2", sample_scanned = "s2", barcode_value = "002", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s3", sample_scanned = "s3", barcode_value = "003", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s4", sample_scanned = "s4", barcode_value = "004", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 6)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  decisions$user_decision[decisions$code == "non_increasing_sampling_times"] <- "change"
  decisions$user_decision_value[decisions$code == "non_increasing_sampling_times"] <- "sort_samples_by_time"
  final <- convert_raw_logs(raw, errors = "raise", create_report = TRUE, issue_decisions = decisions)
  samples <- as_sample_events(final$results)
  expect_true(all(diff(as.numeric(samples$sampling_time)) > 0))
  expect_equal(samples$barcode, c("002", "003", "004", "001"))
  expect_equal(samples$recorded_sample, c("s2", "s3", "s4", "s1"))
  expect_identical(final$report$issues$resolution_status[final$report$issues$code == "non_increasing_sampling_times"], "resolved")
})
