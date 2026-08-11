test_that("synthetic defaults match Study Manager conventions", {
  root <- tempfile("carwatch-defaults-")
  generate_synthetic_study_data(root, n_participants = 1, validate = FALSE)
  folders <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  schedule <- extract_registration_schedule(read_raw_logs_from_participant_dirs(folders))
  expect_identical(unique(schedule$study_name), "Study1")
  expect_identical(unique(schedule$scheduled_sample), paste0("S", 1:4))
  expect_identical(unique(schedule$day), paste0("D", 1:4))
})

test_that("synthetic issue and compliance ratios are exact and patchable", {
  root <- tempfile("carwatch-counts-")
  generate_synthetic_study_data(root, n_participants = 2, random_state = 11, non_compliant_sample_ratio = 5 / 32, missing_awakening_time_ratio = 2 / 8, missing_sampling_time_ratio = 3 / 32)
  folders <- stats::setNames(file.path(root, "logs", c("VP_01", "VP_02")), c("VP_01", "VP_02"))
  raw <- read_raw_logs_from_participant_dirs(folders)
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  counts <- table(advisory$report$issues$code)
  expect_identical(unname(counts["missing_awakening_time"]), 2L)
  expect_identical(unname(counts["missing_scheduled_sample_event"]), 3L)
  final <- convert_raw_logs(raw, errors = "raise", issue_decisions = read_conversion_report(file.path(root, "issue_decisions.csv")), manual_diary = read_manual_diary(file.path(root, "manual_diary.csv")))
  samples <- as_sample_events(final)
  expect_identical(sum(samples$sample_compliant %in% FALSE), 5L)
  expect_identical(sum(samples$sampling_time_source == "manual_diary", na.rm = TRUE), 3L)
})

test_that("synthetic relative and absolute samples use Python-compatible compliance ranges", {
  root <- tempfile("carwatch-mixed-compliance-")
  generate_synthetic_study_data(
    root,
    n_participants = 1,
    random_state = 42,
    non_compliant_sample_ratio = 1,
    missing_awakening_time_ratio = 0,
    missing_sampling_time_ratio = 0,
    study_config = list(
      study_days = 1,
      saliva_distances = c(0, 30),
      saliva_alarm_times = c("12:00", "17:00")
    )
  )
  folder <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  samples <- as_sample_events(convert_raw_logs(read_raw_logs_from_participant_dirs(folder), errors = "raise"))

  expect_identical(samples$schedule_type, c("relative", "relative", "absolute", "absolute"))
  expect_true(all(samples$sample_compliant %in% FALSE))
  expect_true(all(abs(samples$time_deviation_min[samples$schedule_type == "relative"]) > 5))
  expect_true(all(abs(samples$time_deviation_min[samples$schedule_type == "absolute"]) > 15))
})

test_that("synthetic anomaly selection does not perturb generated diary times", {
  complete <- tempfile("carwatch-complete-")
  missing <- tempfile("carwatch-missing-")
  common <- list(
    n_participants = 1,
    random_state = 17,
    non_compliant_sample_ratio = 0,
    missing_awakening_time_ratio = 0,
    validate = FALSE
  )
  do.call(generate_synthetic_study_data, c(list(output_dir = complete, missing_sampling_time_ratio = 0), common))
  do.call(generate_synthetic_study_data, c(list(output_dir = missing, missing_sampling_time_ratio = 1 / 16), common))

  expect_identical(readLines(file.path(complete, "manual_diary.csv")), readLines(file.path(missing, "manual_diary.csv")))
})

test_that("synthetic barcodes match the deterministic Python contract", {
  root <- tempfile("carwatch-barcodes-")
  generate_synthetic_study_data(
    root,
    n_participants = 1,
    random_state = 42,
    non_compliant_sample_ratio = 0,
    missing_awakening_time_ratio = 0,
    missing_sampling_time_ratio = 0,
    validate = FALSE
  )
  folder <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  raw <- read_raw_logs_from_participant_dirs(folder)
  scans <- raw[raw$action == "barcode_scanned", , drop = FALSE]
  barcodes <- vapply(scans$payload, function(payload) as.character(payload$barcode_value), character(1))
  positions <- vapply(scans$payload, function(payload) as.integer(payload$id) + 1L, integer(1))

  expect_identical(unique(barcodes[positions == 1L]), "3220932455")
  expect_identical(vapply(split(barcodes, positions), function(values) length(unique(values)), integer(1)), stats::setNames(rep(1L, 4), as.character(1:4)))
})

test_that("synthetic aliases and multiple registrations preserve opaque IDs", {
  root <- tempfile("carwatch-registrations-")
  generate_synthetic_study_data(root, n_participants = 1, validate = FALSE, study_config = list(N = "QR example", SS = "T0", T = c(0, 30), A = "12:00", E = 1, FD = 0, FM = 1, registrations = list(list(N = "Study1", D = 1, registration_date = "2026-05-01", saliva_ids = c("baseline", "rise", "noon")), list(N = "Study2", D = 1, registration_date = "2026-05-08", saliva_ids = c("tube-x", "tube-y", "tube-z")))))
  folder <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  schedule <- extract_registration_schedule(read_raw_logs_from_participant_dirs(folder))
  expect_identical(unique(schedule$study_name), c("Study1", "Study2"))
  expect_identical(split(schedule$scheduled_sample, schedule$registration), list(`1` = c("baseline", "rise", "noon"), `2` = c("tube-x", "tube-y", "tube-z")))
})

test_that("decoded Study Manager QR configurations are accepted", {
  root <- tempfile("carwatch-qr-")
  generate_synthetic_study_data(root, validate = FALSE, study_config = "CARWATCH;N:QRExample;D:2;NP:1;SS:S1;T:0,30,15,15;A:;E:0;FD:1;FM:0;V:1.0.0")
  folder <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  schedule <- extract_registration_schedule(read_raw_logs_from_participant_dirs(folder))
  expect_identical(unique(schedule$day), c("D1", "D2"))
  expect_identical(unique(schedule$scheduled_sample), paste0("S", 1:4))
})

test_that("synthetic configuration preserves Python-compatible schedule and metadata fields", {
  root <- tempfile("carwatch-python-config-")
  generate_synthetic_study_data(
    root,
    n_participants = 1,
    validate = FALSE,
    study_config = list(
      filename_token = "file token",
      study_days = 1,
      saliva_distances = c(0L, 30L),
      saliva_alarm_times = 1200L,
      has_evening_sample = TRUE,
      check_duplicates = TRUE,
      enable_manual_scan = TRUE
    )
  )
  folder <- stats::setNames(file.path(root, "logs", "VP_01"), "VP_01")
  raw <- read_raw_logs_from_participant_dirs(folder)
  metadata <- raw$payload[[which(raw$action == "study_metadata")[[1]]]]

  expect_identical(metadata$saliva_absolute_times, "12:00")
  expect_true(metadata$check_duplicates)
  expect_true(metadata$has_evening_salivette)
  expect_true(metadata$enable_manual_scan)
  expect_true(any(grepl("carwatch_file-token_", basename(raw$source_file), fixed = TRUE)))
  expect_error(generate_synthetic_study_data(tempfile("bad-time-"), validate = FALSE, study_config = list(A = 2460L)), "Invalid fixed")
  expect_error(generate_synthetic_study_data(tempfile("bad-seed-"), validate = FALSE, random_state = -1), "non-negative")
})

test_that("synthetic cortisol, reproducibility, and overwrite contracts hold", {
  first <- tempfile("carwatch-first-"); second <- tempfile("carwatch-second-")
  generate_synthetic_study_data(first, n_participants = 1, random_state = 42, create_cortisol_data = TRUE)
  generate_synthetic_study_data(second, n_participants = 1, random_state = 42, create_cortisol_data = TRUE)
  expect_identical(readLines(file.path(first, "manual_diary.csv")), readLines(file.path(second, "manual_diary.csv")))
  expect_identical(readLines(file.path(first, "cortisol.csv")), readLines(file.path(second, "cortisol.csv")))
  cortisol <- readr::read_csv(file.path(first, "cortisol.csv"), show_col_types = FALSE)
  profile <- dplyr::summarise(dplyr::group_by(cortisol, .data$sample_position), cortisol = mean(.data$cortisol), .groups = "drop")
  expect_gt(profile$cortisol[[3]], profile$cortisol[[2]])
  expect_gt(profile$cortisol[[2]], profile$cortisol[[1]])
  expect_lt(profile$cortisol[[4]], profile$cortisol[[3]])
  expect_error(generate_synthetic_study_data(first, n_participants = 1), "overwrite")
  expect_silent(generate_synthetic_study_data(first, n_participants = 1, overwrite = TRUE, validate = FALSE))
  expect_error(generate_synthetic_study_data(tempfile("invalid-"), study_config = list(non_compliant_sample_count = 1)), "function parameters")
})
