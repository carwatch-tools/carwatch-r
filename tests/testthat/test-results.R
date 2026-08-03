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
