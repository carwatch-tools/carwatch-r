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

test_that("accept retains the earliest duplicate scan", {
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
  expect_identical(initial$report$issues$proposed_action[initial$report$issues$code == "duplicate_scheduled_sample_events"], "keep_earliest_scan")
  expect_true(is.na(as_sample_events(initial$results)$sampling_time[[1]]))
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = initial$report$issues)
  sample <- as_sample_events(final$results)
  expect_identical(sample$barcode[[1]], "a")
  expect_equal(final$report$summary$recorded_sample_event_count, 1L)
})

test_that("accept reassigns a safe duplicate scan to its recorded sample", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 4),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:10:00", "2025-05-15 06:20:00", "2025-05-15 06:30:00"), tz = timezone),
    action = c("study_metadata", rep("barcode_scanned", 3)),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s1", sample_scanned = "s2", barcode_value = "002", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s1", sample_scanned = "s2", barcode_value = "unused", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 4)
  )
  raw <- raw[-4, ]
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  expect_identical(initial$report$issues$proposed_action[initial$report$issues$code == "duplicate_scheduled_sample_events"], "reassign_to_recorded_sample")
  decisions <- initial$report$issues
  decisions$user_decision[decisions$code != "duplicate_scheduled_sample_events"] <- "keep"
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions)
  samples <- as_sample_events(final$results)
  expect_equal(samples$barcode, c("001", "002"))
  expect_equal(samples$recorded_sample, c("s1", "s2"))
})

test_that("expected-sample override moves an invalid scan only after decision", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 3),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:10:00", "2025-05-15 06:20:00"), tz = timezone),
    action = c("study_metadata", rep("barcode_scanned", 2)),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1),
      list(sample_expected = "invalid", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s2", sample_scanned = "s2", barcode_value = "002", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 3)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  issue <- initial$report$issues$code == "expected_sample_not_in_active_metadata"
  expect_identical(initial$report$issues$proposed_action[issue], "override_expected_sample")
  expect_true(is.na(as_sample_events(initial$results)$sampling_time[as_sample_events(initial$results)$sample == "s1"]))
  decisions <- initial$report$issues
  decisions$user_decision[!issue] <- "keep"
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions)
  samples <- as_sample_events(final$results)
  expect_equal(samples$barcode, c("001", "002"))
  expect_equal(samples$recorded_sample, c("s1", "s2"))
})

test_that("collection-date decisions choose the canonical day date", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 4),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 07:00:00", "2025-05-16 08:00:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", rep("barcode_scanned", 2)),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = numeric(), saliva_absolute_times = c("07:00", "08:00"), study_days = 1),
      list(id = 0),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s2", sample_scanned = "s2", barcode_value = "002", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 4)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  issue <- initial$report$issues$code == "multiple_collection_dates"
  expect_identical(initial$report$issues$proposed_action[issue], "use_earliest_collection_date")
  expect_true(is.na(as_study_days(initial$results)$date[[1]]))
  decisions <- initial$report$issues
  decisions$user_decision[!issue] <- "keep"
  decisions$user_decision[issue] <- "change"
  decisions$user_decision_value[issue] <- "use_latest_collection_date"
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions)
  expect_identical(as.character(as.Date(as_study_days(final$results)$date[[1]])), "2025-05-16")
  scheduled <- as_sample_events(final$results)$scheduled_sampling_time
  expect_identical(format(scheduled, "%Y-%m-%d %H:%M"), c("2025-05-16 07:00", "2025-05-16 08:00"))
})

test_that("an explicit awakening-time decision does not require a diary", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = c("vp01", "vp01"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:20:00"), tz = timezone),
    action = c("study_metadata", "barcode_scanned"),
    payload = list(
      list(study_name = "study", saliva_ids = "s1", saliva_times = 0, study_days = 1),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1)
    ),
    source_file = c("one.csv", "one.csv")
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  awakening <- decisions$code == "missing_awakening_time"
  decisions$user_decision[awakening] <- "change"
  decisions$user_decision_value[awakening] <- "2025-05-15 06:00"
  final <- convert_raw_logs(raw, errors = "raise", create_report = TRUE, issue_decisions = decisions)
  days <- as_study_days(final$results)
  expect_identical(format(days$awakening_time[[1]], "%Y-%m-%d %H:%M"), "2025-05-15 06:00")
  expect_identical(days$awakening_type[[1]], "decision")
})

test_that("use_default uses the supplied fallback schedule only when needed", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 3),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:10:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned"),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2"), study_days = 1),
      list(id = 0),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 3)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  missing <- decisions$code == "missing_scheduled_sample_event"
  decisions$user_decision[missing] <- "change"
  decisions$user_decision_value[missing] <- "use_default"
  final <- convert_raw_logs(raw, errors = "raise", create_report = TRUE, issue_decisions = decisions, sampling_schedule = c(0, 45))
  samples <- as_sample_events(final$results)
  second <- samples[samples$sample == "s2", , drop = FALSE]
  expect_identical(format(second$sampling_time[[1]], "%Y-%m-%d %H:%M"), "2025-05-15 06:45")
  expect_identical(second$sampling_time_source[[1]], "schedule")
})

test_that("registration schedule rejects ambiguous cohort order unless warned", {
  raw <- tibble::tibble(
    participant = c("p1", "p1", "p2", "p2"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-16 05:00:00", "2025-05-16 06:00:00"), tz = "Europe/Berlin"),
    action = "study_metadata",
    payload = list(
      list(saliva_ids = "first"), list(saliva_ids = "second"),
      list(saliva_ids = "second"), list(saliva_ids = "first")
    ),
    source_file = c("p1-first.csv", "p1-second.csv", "p2-second.csv", "p2-first.csv")
  )
  expect_error(extract_registration_schedule(raw), "ambiguous")
  expect_warning(schedule <- extract_registration_schedule(raw, errors = "warn"), "ambiguous")
  expect_identical(unique(schedule$day), c("D1", "D2"))
})

test_that("registration-order violations retain the canonical schedule and audit sources", {
  raw <- tibble::tibble(
    participant = rep("p1", 3),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-16 05:00:00", "2025-05-17 05:00:00"), tz = "Europe/Berlin"),
    action = "study_metadata",
    payload = list(
      list(study_name = "first", saliva_ids = "first-1"),
      list(study_name = "second", saliva_ids = "second-1"),
      list(study_name = "first", saliva_ids = "first-1")
    ),
    source_file = c("first.csv", "second.csv", "first-again.csv")
  )
  expect_error(extract_registration_schedule(raw), "do not create an additional")
  expect_warning(schedule <- extract_registration_schedule(raw, errors = "warn"), "do not create an additional")
  expect_identical(unique(schedule$day), c("D1", "D2"))
  converted <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, check_compliance = FALSE)
  issue <- converted$report$issues[converted$report$issues$code == "registration_order_violation", , drop = FALSE]
  expect_equal(nrow(issue), 1L)
  details <- jsonlite::fromJSON(issue$details[[1]], simplifyVector = FALSE)
  expect_identical(details$current$metadata_source_files, "first-again.csv")
  expect_identical(details$previous$metadata_source_files, "second.csv")
})

test_that("saliva merges correct swaps for physical and positional matching", {
  samples <- c("B1", "B2", "B3", "B4")
  long <- dplyr::bind_rows(lapply(seq_along(samples), function(position) {
    tibble::tibble(
      participant = "p1", day = "D1", sample = samples[[position]],
      variable = c("sampling_time", "recorded_sample", "sample_position"),
      value = list(
        as.POSIXct(sprintf("2025-05-15 06:%02d:00", position), tz = "Europe/Berlin"),
        c("B1", "B3", "B2", "B4")[[position]], position
      )
    )
  }))
  results <- carwatch:::.from_long_results(long)
  physical <- merge_saliva(results, tibble::tibble(participant = "p1", sample = samples, cortisol = c(1, 2, 3, 4)))
  positional <- merge_saliva(results, tibble::tibble(participant = "p1", day = "D1", sample_position = seq_along(samples), cortisol = c(1, 2, 3, 4)), match_on = "position")
  expect_equal(as_sample_events(physical)$cortisol, c(1, 3, 2, 4))
  expect_equal(as_sample_events(positional)$cortisol, c(1, 3, 2, 4))
  expect_true(all(as_sample_events(positional)$mismatch_corrected[c(2, 3)]))
})

test_that("saliva merges validate physical tubes and missing app events", {
  samples <- c("B1", "B2")
  long <- dplyr::bind_rows(lapply(seq_along(samples), function(position) tibble::tibble(
    participant = "p1", day = "D1", sample = samples[[position]],
    variable = c("sampling_time", "recorded_sample", "sample_position"),
    value = list(as.POSIXct(sprintf("2025-05-15 06:%02d:00", position), tz = "Europe/Berlin"), "B1", position)
  )))
  results <- carwatch:::.from_long_results(long)
  saliva <- tibble::tibble(participant = "p1", sample = samples, cortisol = c(1, 2))
  expect_error(merge_saliva(results, saliva), "same physical saliva tube")
  long$value[[4]] <- as.POSIXct(NA)
  long$value[[5]] <- "B2"
  results <- carwatch:::.from_long_results(long)
  expect_error(merge_saliva(results, saliva, missing_carwatch_data = "raise"), "scheduled_sample=B2")
  bad <- saliva; bad$cortisol[[1]] <- "invalid"
  expect_error(merge_saliva(results, bad), "numeric")
})

test_that("Study Manager imports retain natural per-day sample positions", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "Participant ID,date_D1,sampling_time_D1_S10,sample_barcode_D1_S10,sample_scanned_D1_S10,sampling_time_D1_S2,sample_barcode_D1_S2,sample_scanned_D1_S2,sampling_time_D1_S1,sample_barcode_D1_S1,sample_scanned_D1_S1",
    "p1,2025-05-15,08:00:00,c10,S10,07:00:00,c2,S2,06:00:00,c1,S1"
  ), path)
  samples <- as_sample_events(read_study_manager_export(path))
  expect_identical(samples$sample, c("S1", "S2", "S10"))
  expect_identical(samples$sample_position, c(1L, 2L, 3L))
})

test_that("a collection-date mapping reassigns scans by sample position", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 6),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:10:00", "2025-05-16 06:20:00", "2025-05-17 05:00:00", "2025-05-17 06:00:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned", "barcode_scanned", "study_metadata", "spontaneous_awakening"),
    payload = list(
      list(study_name = "source", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1),
      list(id = 0),
      list(sample_expected = "s1", sample_scanned = "s1", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(sample_expected = "s2", sample_scanned = "s2", barcode_value = "002", day_expected = 1, day_scanned = 1),
      list(study_name = "target", saliva_ids = c("t1", "t2"), saliva_times = c(0, 30), study_days = 1),
      list(id = 0)
    ),
    source_file = rep("one.csv", 6)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  mapping_issue <- decisions$code == "multiple_collection_dates"
  decisions$user_decision[!mapping_issue] <- "keep"
  decisions$user_decision[mapping_issue] <- "change"
  decisions$user_decision_value[mapping_issue] <- '{"2025-05-15":"D1","2025-05-16":"D2"}'
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions)
  samples <- as_sample_events(final$results)
  expect_identical(samples$barcode[samples$day == "D1" & samples$sample == "s1"], "001")
  expect_true(is.na(samples$sampling_time[samples$day == "D1" & samples$sample == "s2"]))
  expect_identical(samples$barcode[samples$day == "D2" & samples$sample == "t2"], "002")
})

test_that("protocol order follows observed registration precedence", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = c("vp01", "vp01", "vp02", "vp02", "vp02", "vp02"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-16 05:00:00", "2025-05-16 06:00:00", "2025-05-17 05:00:00", "2025-05-17 06:00:00"), tz = timezone),
    action = c("study_metadata", "barcode_scanned", "study_metadata", "barcode_scanned", "study_metadata", "barcode_scanned"),
    payload = list(
      list(study_name = "second", saliva_ids = "b", saliva_times = 0, study_days = 1),
      list(sample_expected = "b", sample_scanned = "b", barcode_value = "b-01", day_expected = 1, day_scanned = 1),
      list(study_name = "first", saliva_ids = "a", saliva_times = 0, study_days = 1),
      list(sample_expected = "a", sample_scanned = "a", barcode_value = "a-01", day_expected = 1, day_scanned = 1),
      list(study_name = "second", saliva_ids = "b", saliva_times = 0, study_days = 1),
      list(sample_expected = "b", sample_scanned = "b", barcode_value = "b-02", day_expected = 1, day_scanned = 1)
    ),
    source_file = rep("one.csv", 6)
  )
  converted <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  schedule <- extract_registration_schedule(raw)
  expect_identical(schedule$study_name[match("D1", schedule$day)], "first")
  samples <- as_sample_events(converted$results)
  expect_identical(samples$day[samples$participant == "vp01" & samples$sample == "b" & samples$barcode == "b-01"], "D2")
})

test_that("ambiguous and cyclic cohort protocol orders are reported", {
  timezone <- "Europe/Berlin"
  metadata <- function(name, sample) list(study_name = name, saliva_ids = sample, saliva_times = 0, study_days = 1)
  ambiguous <- tibble::tibble(
    participant = c("vp01", "vp01", "vp02", "vp02"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-16 05:00:00", "2025-05-16 06:00:00"), tz = timezone),
    action = rep("study_metadata", 4),
    payload = list(metadata("a", "a"), metadata("b", "b"), metadata("b", "b"), metadata("a", "a")),
    source_file = rep("one.csv", 4)
  )
  ambiguous_report <- convert_raw_logs(ambiguous, errors = "warn", create_report = TRUE)$report
  expect_true(any(ambiguous_report$issues$code == "ambiguous_protocol_order"))
  cyclic <- tibble::tibble(
    participant = rep(c("vp01", "vp02", "vp03"), each = 3),
    timestamp = as.POSIXct(rep(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 07:00:00"), 3), tz = timezone),
    action = rep("study_metadata", 9),
    payload = list(metadata("a", "a"), metadata("b", "b"), metadata("c", "c"), metadata("b", "b"), metadata("c", "c"), metadata("a", "a"), metadata("c", "c"), metadata("a", "a"), metadata("b", "b")),
    source_file = rep("one.csv", 9)
  )
  cyclic_report <- convert_raw_logs(cyclic, errors = "warn", create_report = TRUE)$report
  expect_true(any(cyclic_report$issues$code == "cyclic_protocol_order"))
})

test_that("manifest registrations missing for a participant are reported", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = c("vp01", "vp01"),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00"), tz = timezone),
    action = c("study_metadata", "barcode_scanned"),
    payload = list(
      list(study_name = "first", saliva_ids = "a", saliva_times = 0, study_days = 1),
      list(sample_expected = "a", sample_scanned = "a", barcode_value = "001", day_expected = 1, day_scanned = 1)
    ),
    source_file = c("one.csv", "one.csv")
  )
  manifest <- list(
    list(study_name = "first", saliva_ids = "a", saliva_times = 0, study_days = 1),
    list(study_name = "second", saliva_ids = "b", saliva_times = 0, study_days = 1)
  )
  report <- convert_raw_logs(raw, protocol_manifest = manifest, errors = "warn", create_report = TRUE)$report
  missing <- report$issues[report$issues$code == "manifest_registration_missing", , drop = FALSE]
  expect_equal(nrow(missing), 1L)
  expect_identical(missing$participant[[1]], "vp01")
})

test_that("re-registration overrides remap the later epoch by sample position", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 7),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:10:00", "2025-05-15 06:15:00", "2025-05-15 06:20:00", "2025-05-16 05:00:00", "2025-05-16 06:00:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned", "study_metadata", "barcode_scanned", "study_metadata", "spontaneous_awakening"),
    payload = list(
      list(study_name = "source", saliva_ids = c("old-1", "old-2"), saliva_times = c(0, 30), study_days = 1),
      list(id = 0),
      list(sample_expected = "old-1", sample_scanned = "old-1", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(study_name = "source", saliva_ids = c("old-1", "old-2"), saliva_times = c(0, 30), study_days = 1),
      list(sample_expected = "old-2", sample_scanned = "old-2", barcode_value = "002", day_expected = 1, day_scanned = 1),
      list(study_name = "target", saliva_ids = c("new-1", "new-2"), saliva_times = c(0, 30), study_days = 1),
      list(id = 0)
    ),
    source_file = rep("one.csv", 7)
  )
  initial <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  target <- decisions$code == "possible_reregistration"
  decisions$user_decision[!target] <- "keep"
  decisions$user_decision[target] <- "change"
  decisions$user_decision_value[target] <- '{"registration":2}'
  final <- convert_raw_logs(raw, errors = "warn", create_report = TRUE, issue_decisions = decisions)
  samples <- as_sample_events(final$results)
  expect_identical(samples$barcode[samples$day == "D1" & samples$sample == "old-1"], "001")
  expect_identical(samples$barcode[samples$day == "D2" & samples$sample == "new-2"], "002")
  expect_identical(samples$recorded_sample[samples$day == "D2" & samples$sample == "new-2"], "new-2")
})

test_that("registration source provenance is kept per canonical epoch", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("vp01", 6),
    timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:10:00", "2025-05-16 05:00:00", "2025-05-16 06:00:00", "2025-05-16 06:10:00"), tz = timezone),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned", "study_metadata", "spontaneous_awakening", "barcode_scanned"),
    payload = list(
      list(study_name = "first", saliva_ids = "a", saliva_times = 0, study_days = 1), list(id = 0), list(sample_expected = "a", sample_scanned = "a", barcode_value = "001", day_expected = 1, day_scanned = 1),
      list(study_name = "second", saliva_ids = "b", saliva_times = 0, study_days = 1), list(id = 0), list(sample_expected = "b", sample_scanned = "b", barcode_value = "002", day_expected = 1, day_scanned = 1)
    ),
    source_file = c("first.csv", "first.csv", "first.csv", "second.csv", "second.csv", "second.csv")
  )
  days <- as_study_days(convert_raw_logs(raw, errors = "warn"))
  expect_identical(days$registration_sources[days$day == "D1"], "first.csv")
  expect_identical(days$registration_sources[days$day == "D2"], "second.csv")
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
