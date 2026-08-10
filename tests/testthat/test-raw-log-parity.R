.raw_log_path <- function(lines) {
  root <- tempfile("carwatch-log-"); dir.create(root)
  path <- file.path(root, "carwatch_demo_01_20250515.csv")
  writeLines(lines, path)
  path
}

test_that("conversion issue identity excludes wording and proposed action", {
  reports <- list(carwatch:::.new_conversion_report(tibble::tibble(participant = character(), source_file = character())), carwatch:::.new_conversion_report(tibble::tibble(participant = character(), source_file = character())))
  first <- carwatch:::.report_add_issue(reports[[1]], code = "missing_scheduled_sample_event", participant = "01", registration = 1L, day = "D1", sample_position = 1L, sample_id = "tube-x", message = "First message.", details = list(source = "same"), proposed_action = "first_action")
  second <- carwatch:::.report_add_issue(reports[[2]], code = "missing_scheduled_sample_event", participant = "01", registration = 1L, day = "D1", sample_position = 1L, sample_id = "tube-x", message = "Second message.", details = list(source = "same"), proposed_action = "second_action")
  expect_identical(first$issue_id, second$issue_id)
  expect_identical(carwatch:::.report_add_issue(second$report, code = "example", message = "First line.\nSecond line.", proposed_action = "keep")$report$issues$message[[2]], "First line. Second line.")
})

test_that("issue IDs use the Python RFC 3339 identity encoding", {
  # Computed by carwatch-python 1.0.0 ConversionReport.add_issue() with the
  # same context. This guards the otherwise easy-to-miss +02:00 versus +0200
  # serialization difference in cross-language editable reports.
  stamp <- as.POSIXct("2025-05-15 06:10:00", tz = "Europe/Berlin")
  expect_identical(
    carwatch:::.stable_issue_id(
      "p1", code = "scan_before_registration_metadata",
      details = list(timestamp = stamp, source_file = "one.csv", payload = list(sample_expected = "s1"))
    ),
    "dc76536f1f39c731"
  )
})

test_that("raw-log reader accepts current, multiline, and legacy exports", {
  current <- read_raw_logs(.raw_log_path(c(
    '1747282410799;local;spontaneous_awakening;{"id":0}',
    '1747282435999;local;barcode_scanned;{"id":0,"saliva_id":100,"barcode_value":"0010101","day_scanned":1,"day_expected":1,"sample_scanned":"B1","sample_expected":"B1"}'
  )))
  expect_identical(current$participant, c("01", "01"))
  expect_identical(current$payload[[2]]$barcode_value, "0010101")
  multiline <- read_raw_logs(.raw_log_path(c('1776429093567;local;spontaneous_awakening;{', '  "id" : -1', '}')))
  expect_equal(multiline$payload[[1]]$id, -1)
  legacy <- read_raw_logs(.raw_log_path('1747282410799;spontaneous_awakening;{"id":0}'))
  expect_identical(legacy$action, "spontaneous_awakening")
})

test_that("raw-log reader exposes invalid JSON modes and ignores hidden ZIP members", {
  path <- .raw_log_path('1747282410799;local;spontaneous_awakening;{invalid}')
  expect_error(read_raw_logs(path), "Invalid JSON")
  expect_warning(warned <- read_raw_logs(path, errors = "warn"), "Invalid JSON")
  expect_identical(warned$payload[[1]]$`_invalid_json`, "{invalid}")
  root <- tempfile("carwatch-zip-"); dir.create(root)
  visible <- file.path(root, "carwatch_demo_01_20250515.csv")
  hidden <- file.path(root, ".hidden.csv")
  writeLines('1747282410799;local;spontaneous_awakening;{"id":0}', visible)
  writeLines('1747282410799;local;spontaneous_awakening;{"id":99}', hidden)
  archive <- file.path(root, "logs.zip")
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(root); utils::zip(archive, c(basename(visible), basename(hidden)), flags = "-q")
  loaded <- read_raw_logs(archive)
  expect_equal(loaded$payload[[1]]$id, 0)
})

test_that("raw-log source ordering is advisory and converter preserves swaps", {
  unordered <- .raw_log_path(c('1747282435999;local;timer_set;{"id":1}', '1747282410799;local;timer_set;{"id":0}'))
  expect_warning(logs <- read_raw_logs(unordered, errors = "raise"), "will be sorted during conversion")
  expect_equal(logs$timestamp_ms, sort(logs$timestamp_ms))
  raw <- tibble::tibble(
    participant = rep("01", 4), timestamp = as.POSIXct(c("2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:01:00", "2025-05-15 06:31:00"), tz = "Europe/Berlin"),
    action = c("study_metadata", "spontaneous_awakening", "barcode_scanned", "barcode_scanned"),
    payload = list(list(study_name = "opaque", saliva_ids = c("tube-x", "17", "baseline"), saliva_times = c(0, 30, 30)), list(id = 0), list(sample_expected = "tube-x", sample_scanned = "tube-x", barcode_value = "one", day_expected = 1), list(sample_expected = "17", sample_scanned = "baseline", barcode_value = "two", day_expected = 1)),
    source_file = "one.csv"
  )
  converted <- convert_raw_logs(raw, errors = "warn")
  samples <- as_sample_events(converted)
  expect_equal(samples$sample_position, c(1L, 2L, 3L))
  expect_identical(samples$recorded_sample[samples$sample == "17"], "baseline")
  expect_identical(as_study_days(converted)$study_name, "opaque")
})

test_that("conversion-report supersession follows the Python decision matrix", {
  row <- function(code, decision, value = "", participant = "01", day = "D1", sample = NA_character_) tibble::tibble(code = code, user_decision = decision, user_decision_value = value, participant = participant, day = day, sample_id = sample)
  expect_true(carwatch:::.decision_supersedes(row("ambiguous_protocol_order", "accept", participant = "__cohort__", day = NA_character_), row("missing_scheduled_sample_event", "", sample = "S1")))
  expect_true(carwatch:::.decision_supersedes(row("expected_sample_not_in_active_metadata", "accept", sample = "bad"), row("duplicate_scheduled_sample_events", "", sample = "S1")))
  expect_true(carwatch:::.decision_supersedes(row("multiple_collection_dates", "change", '{"2025-05-15":"D1"}'), row("missing_scheduled_sample_event", "", sample = "S1")))
  expect_false(carwatch:::.decision_supersedes(row("multiple_collection_dates", "change", "use_latest_collection_date"), row("missing_scheduled_sample_event", "", sample = "S1")))
  expect_true(carwatch:::.decision_supersedes(row("possible_reregistration", "change", '{"registration":2}'), row("missing_awakening_time", "")))
  expect_false(carwatch:::.decision_supersedes(row("possible_reregistration", "keep"), row("missing_awakening_time", "")))
})

test_that("conversion reports retain complete issue-specific context", {
  timezone <- "Europe/Berlin"
  raw <- tibble::tibble(
    participant = rep("p1", 8),
    timestamp = as.POSIXct(c(
      "2025-05-15 05:00:00", "2025-05-15 05:10:00", "2025-05-15 05:20:00", "2025-05-15 05:30:00",
      "2025-05-16 05:00:00", "2025-05-16 05:10:00", "2025-05-16 05:20:00", "2025-05-16 05:30:00"
    ), tz = timezone),
    action = c("barcode_scanned", "study_metadata", "barcode_scanned", "barcode_scanned", "study_metadata", "barcode_scanned", "barcode_scanned", "barcode_scanned"),
    payload = list(
      list(sample_expected = "a", sample_scanned = "a", day_expected = 1),
      list(study_name = "study", saliva_ids = c("a", "b", "c"), saliva_times = c(0, 30, 60), study_days = 1),
      list(sample_expected = "outside", sample_scanned = "b", barcode_value = "one", day_expected = 1),
      list(sample_expected = "outside-day", sample_scanned = "a", barcode_value = "two", day_expected = 2),
      list(study_name = "study", saliva_ids = c("a", "b", "c"), saliva_times = c(0, 30, 60), study_days = 1),
      list(sample_expected = "a", sample_scanned = "a", barcode_value = "three", day_expected = 1),
      list(sample_expected = "a", sample_scanned = "a", barcode_value = "four", day_expected = 1),
      list(sample_expected = "b", sample_scanned = "b", barcode_value = "five", day_expected = 1)
    ),
    source_file = rep("one.csv", 8)
  )
  converted <- convert_raw_logs(raw, errors = "warn", create_report = TRUE)
  issues <- converted$report$issues
  expect_true(all(c(
    "scan_before_registration_metadata", "expected_sample_not_in_active_metadata",
    "invalid_day_expected", "possible_reregistration",
    "duplicate_scheduled_sample_events", "missing_awakening_time",
    "missing_scheduled_sample_event"
  ) %in% issues$code))
  expected <- issues[issues$code == "expected_sample_not_in_active_metadata", , drop = FALSE]
  expect_match(expected$message[[1]], "study_name='study'")
  expect_match(expected$proposed_action_description[[1]], "exact registered ID")
  expect_true(all(c("scheduled_sample", "active_saliva_ids", "timestamp", "source_file") %in% names(jsonlite::fromJSON(expected$details[[1]], simplifyVector = FALSE))))
  duplicate <- issues[issues$code == "duplicate_scheduled_sample_events", , drop = FALSE]
  expect_true(all(c("occurrences", "retained_occurrence", "other_occurrences", "supplied_fields") %in% names(jsonlite::fromJSON(duplicate$details[[1]], simplifyVector = FALSE))))
  missing <- issues[issues$code == "missing_scheduled_sample_event", , drop = FALSE]
  expect_true(all(c("registered_at", "source_files", "saliva_times", "saliva_absolute_times") %in% names(jsonlite::fromJSON(missing$details[[1]], simplifyVector = FALSE))))
})

test_that("conversion summaries count raw evidence and participant schedules", {
  raw <- tibble::tibble(
    participant = c("p1", "p1", "p2", "p2", "p2"),
    timestamp = as.POSIXct(c(
      "2025-05-15 05:00:00", "2025-05-15 06:00:00",
      "2025-05-15 05:00:00", "2025-05-15 06:00:00", "2025-05-15 06:00:00"
    ), tz = "Europe/Berlin"),
    action = c("study_metadata", "barcode_scanned", "study_metadata", "barcode_scanned", "barcode_scanned"),
    payload = list(
      list(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1),
      list(sample_expected = "s1", sample_scanned = "s1", day_expected = 1),
      list(study_name = "study", saliva_ids = c("s1", "s2"), saliva_times = c(0, 30), study_days = 1),
      list(sample_expected = "s1", sample_scanned = "s1", day_expected = 1),
      list(sample_expected = "s1", sample_scanned = "s1", day_expected = 1)
    ),
    source_file = c("p1.csv", "p1.csv", "p2.csv", "p2.csv", "p2-copy.csv")
  )
  converted <- suppressWarnings(convert_raw_logs(raw, errors = "warn", create_report = TRUE))
  expect_identical(converted$report$summary$input_event_count, 5L)
  expect_identical(converted$report$summary$input_participant_count, 2L)
  expect_identical(converted$report$summary$input_source_file_count, 3L)
  expect_identical(converted$report$summary$expected_sample_position_count, 4L)
})

test_that("warning and strict modes expose unresolved conversion issues", {
  raw <- tibble::tibble(
    participant = "p1",
    timestamp = as.POSIXct("2025-05-15 05:00:00", tz = "Europe/Berlin"),
    action = "study_metadata",
    payload = list(list(study_name = "study", saliva_ids = "s1", saliva_times = 0, study_days = 1)),
    source_file = "one.csv"
  )
  expect_warning(
    advisory <- convert_raw_logs(raw, errors = "warn", create_report = TRUE),
    class = "carwatch_conversion_warning"
  )
  decisions <- advisory$report$issues
  decisions$user_decision <- ""
  expect_error(
    convert_raw_logs(raw, errors = "raise", issue_decisions = decisions),
    class = "carwatch_schema_error"
  )
})

test_that("obsolete proposed actions are rejected at the report boundary", {
  decisions <- tibble::tibble(
    participant = "p1", issue_id = "issue-1", code = "missing_scheduled_sample_event",
    user_decision = "keep", user_decision_value = "", proposed_action = "patch_sampling_time"
  )
  expect_error(carwatch:::.normalize_issue_decisions(decisions), "obsolete proposed actions")
})
