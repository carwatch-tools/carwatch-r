.conversion_raw <- function(timestamp, action, payload, participant = "p1", source_file = "one.csv", tz = "Europe/Berlin") {
  tibble::tibble(
    participant = if (length(participant) == 1L) rep(participant, length(timestamp)) else participant,
    timestamp = as.POSIXct(timestamp, tz = tz),
    action = action,
    payload = payload,
    source_file = if (length(source_file) == 1L) rep(source_file, length(timestamp)) else source_file
  )
}

.metadata <- function(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1L, saliva_absolute_times = character()) {
  list(study_name = study_name, saliva_ids = saliva_ids, saliva_times = saliva_times, saliva_absolute_times = saliva_absolute_times, study_days = study_days)
}

.scan <- function(expected, recorded = expected, day = 1L, barcode = expected) {
  list(sample_expected = expected, sample_scanned = recorded, barcode_value = barcode, day_expected = day, day_scanned = day)
}

test_that("study name and multi-day metadata define canonical registrations", {
  raw <- .conversion_raw(
    c("2025-05-15 05:00:00", "2025-05-16 05:00:00"),
    c("study_metadata", "study_metadata"),
    list(.metadata("first", "opaque", 0, 2L), .metadata("second", "opaque", 0, 1L))
  )
  protocol <- summarize_protocol(raw, errors = "warn")
  expect_identical(protocol$study_name, c("first", "second"))
  schedule <- extract_registration_schedule(raw, errors = "warn")
  expect_identical(unique(schedule$day), c("D1", "D2", "D3"))
  expect_identical(schedule$scheduled_sample, rep("opaque", 3))
})

test_that("repeated metadata before collection remains one registration", {
  raw <- .conversion_raw(
    c("2025-05-15 05:00:00", "2025-05-15 05:01:00", "2025-05-15 06:00:00"),
    c("study_metadata", "study_metadata", "barcode_scanned"),
    list(.metadata(saliva_ids = "s1", saliva_times = 0), .metadata(saliva_ids = "s1", saliva_times = 0), .scan("s1")),
    source_file = c("first.csv", "copy.csv", "scan.csv")
  )
  converted <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  expect_identical(unique(as_study_days(converted$results)$registration), 1L)
  expect_false(any(converted$report$issues$code == "possible_reregistration"))
})

test_that("registration input anomalies use the public issue codes", {
  raw <- .conversion_raw(
    c("2025-05-15 04:00:00", "2025-05-15 05:00:00", "2025-05-15 05:30:00", "2025-05-15 06:00:00"),
    c("barcode_scanned", "study_metadata", "study_metadata", "barcode_scanned"),
    list(.scan("s1"), list(study_name = "invalid"), .metadata(saliva_ids = "s1", saliva_times = 0), .scan("s1", day = 2L))
  )
  converted <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  expect_true(all(c("scan_before_registration_metadata", "invalid_study_metadata", "invalid_day_expected") %in% converted$report$issues$code))
  expect_false(any(!converted$report$issues$code %in% carwatch:::.conversion_issue_codes))
})

test_that("protocol manifests validate configurations and retain absent slots", {
  raw <- .conversion_raw("2025-05-15 05:00:00", "study_metadata", list(.metadata("first", "a", 0)))
  manifest <- list(.metadata("first", "a", 0), .metadata("second", "b", 0))
  converted <- suppressWarnings(convert_raw_logs(raw, protocol_manifest = manifest, errors = "warn", create_report = TRUE))
  missing <- converted$report$issues[converted$report$issues$code == "manifest_registration_missing", , drop = FALSE]
  expect_identical(missing$registration, 2L)
  expect_identical(unique(as_study_days(converted$results)$day), c("D1", "D2"))
  expect_error(
    extract_registration_schedule(raw, protocol_manifest = list(.metadata("unknown", "x", 0))),
    "not present in the protocol manifest"
  )
})

test_that("conversion report CSV round-trips and advisory accepts require submission", {
  raw <- .conversion_raw("2025-05-15 05:00:00", "study_metadata", list(.metadata(saliva_ids = "s1", saliva_times = 0)))
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  expect_true(all(advisory$report$issues$resolution_status == "unresolved"))
  path <- tempfile(fileext = ".csv")
  readr::write_csv(advisory$report$issues, path, na = "")
  decisions <- read_conversion_report(path)
  diary <- tibble::tibble(participant = "p1", day = "D1", awakening_time = "2025-05-15 06:00:00", sampling_time_1 = "2025-05-15 06:01:00")
  final <- convert_raw_logs(raw, errors = "raise", create_report = TRUE, issue_decisions = decisions, manual_diary = diary)
  expect_true(all(final$report$issues$resolution_status == "resolved"))
  expect_identical(as_sample_events(final$results)$sampling_time_source, "manual_diary")
})

test_that("scope decisions clear only their declared canonical scope", {
  raw <- .conversion_raw(
    c("2025-05-15 05:00:00", "2025-05-15 06:00:00"),
    c("study_metadata", "barcode_scanned"),
    list(.metadata(saliva_ids = c("s1", "s2"), saliva_times = c(0, 30)), .scan("s1"))
  )
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  decisions <- advisory$report$issues
  decisions$user_decision <- "keep"
  target <- decisions$code == "missing_scheduled_sample_event"
  decisions$user_decision[target] <- "drop_sample"
  final <- convert_raw_logs(raw, errors = "raise", issue_decisions = decisions)
  samples <- as_sample_events(final)
  expect_false(is.na(samples$sampling_time[samples$sample == "s1"]))
  expect_true(is.na(samples$sampling_time[samples$sample == "s2"]))
})

test_that("registered absolute times support use_default patches", {
  raw <- .conversion_raw(
    c("2025-05-15 05:00:00", "2025-05-15 06:00:00"), c("study_metadata", "spontaneous_awakening"),
    list(.metadata(saliva_ids = "noon", saliva_times = numeric(), saliva_absolute_times = "12:00"), list(id = 0))
  )
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  decisions <- advisory$report$issues
  decisions$user_decision <- "keep"
  target <- decisions$code == "missing_scheduled_sample_event"
  decisions$user_decision[target] <- "change"
  decisions$user_decision_value[target] <- "use_default"
  final <- convert_raw_logs(raw, errors = "raise", issue_decisions = decisions)
  expect_identical(format(as_sample_events(final)$sampling_time, "%Y-%m-%d %H:%M"), "2025-05-15 12:00")
})

test_that("manual patches reject nonexistent and ambiguous local times", {
  raw <- .conversion_raw(
    "2025-03-30 00:00:00", "study_metadata",
    list(.metadata(saliva_ids = "s1", saliva_times = 0))
  )
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  decisions <- advisory$report$issues
  decisions$user_decision <- "keep"
  target <- decisions$code == "missing_awakening_time"
  decisions$user_decision[target] <- "change"
  decisions$user_decision_value[target] <- "2025-03-30 02:30"
  expect_error(convert_raw_logs(raw, errors = "raise", issue_decisions = decisions), class = "carwatch_schema_error")
})

test_that("invalid and stale decisions fail before producing analysis output", {
  raw <- .conversion_raw("2025-05-15 05:00:00", "study_metadata", list(.metadata(saliva_ids = "s1", saliva_times = 0)))
  advisory <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  invalid <- advisory$report$issues
  invalid$user_decision[[1]] <- "unsupported"
  expect_error(convert_raw_logs(raw, issue_decisions = invalid), "unsupported")
  stale <- advisory$report$issues
  stale$user_decision <- "keep"
  stale$issue_id[[1]] <- "stale-issue-1"
  expect_error(convert_raw_logs(raw, issue_decisions = stale), "not reproduced")
})

test_that("metadata-free conversion remains unsupported", {
  raw <- .conversion_raw("2025-05-15 06:00:00", "barcode_scanned", list(.scan("s1")))
  expect_error(convert_raw_logs(raw, errors = "warn", create_report = TRUE), "requires usable `study_metadata`")
})
