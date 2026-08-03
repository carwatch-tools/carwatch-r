.payload_value <- function(payload, name, default = NULL) {
  if (is.null(payload) || !is.list(payload) || is.null(payload[[name]])) default else payload[[name]]
}

.payload_sequence <- function(value) {
  if (is.null(value)) return(character())
  if (is.character(value) && length(value) == 1L) {
    text <- trimws(value)
    if (startsWith(text, "[")) return(trimws(unlist(strsplit(gsub("[\\[\\]']", "", text), ",", fixed = TRUE))))
  }
  trimws(as.character(unlist(value, use.names = FALSE)))
}

.registration_config <- function(payload) {
  ids <- .payload_sequence(.payload_value(payload, "saliva_ids"))
  if (!length(ids)) return(NULL)
  days <- suppressWarnings(as.integer(.payload_value(payload, "study_days", 1L)))
  if (is.na(days) || days < 1L) .carwatch_abort("Study metadata contains invalid `study_days`.", "carwatch_schema_error")
  list(
    study_name = as.character(.payload_value(payload, "study_name", "")),
    saliva_ids = ids,
    saliva_times = suppressWarnings(as.numeric(.payload_sequence(.payload_value(payload, "saliva_times")))),
    saliva_absolute_times = .payload_sequence(.payload_value(payload, "saliva_absolute_times")),
    study_days = days
  )
}

.registration_key <- function(config) digest::digest(list(config$study_name, config$saliva_ids, config$saliva_times, config$saliva_absolute_times, config$study_days), algo = "sha1", serialize = TRUE)

.issue_id <- function(code, participant = NA_character_, day = NA_character_, sample_id = NA_character_, details = list()) .stable_issue_id(participant, day = day, sample_id = sample_id, code = code, details = details)

.deduplicate_raw_events <- function(raw_logs) {
  ordered <- dplyr::arrange(raw_logs, .data$participant, .data$timestamp, .data$source_file)
  stamp <- if ("timestamp_ms" %in% names(ordered)) as.character(ordered$timestamp_ms) else format(ordered$timestamp, "%Y-%m-%dT%H:%M:%OS%z")
  key <- paste(ordered$participant, stamp, ordered$action, vapply(ordered$payload, .canonical_json, character(1)), sep = "")
  pieces <- split(seq_len(nrow(ordered)), key, drop = TRUE)
  dplyr::bind_rows(lapply(pieces, function(index) {
    row <- ordered[index[[1]], , drop = FALSE]
    row$source_file <- paste(sort(unique(ordered$source_file[index])), collapse = ";")
    row
  })) |>
    dplyr::arrange(.data$participant, .data$timestamp, .data$source_file)
}

.protocol_from_logs <- function(raw_logs, protocol_manifest = NULL) {
  if (!is.null(protocol_manifest)) {
    configs <- lapply(protocol_manifest, .registration_config)
    if (any(vapply(configs, is.null, logical(1)))) .carwatch_abort("Every protocol manifest entry requires non-empty `saliva_ids`.", "carwatch_schema_error")
    return(configs)
  }
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  configs <- lapply(metadata$payload, .registration_config)
  configs <- configs[!vapply(configs, is.null, logical(1))]
  if (!length(configs)) .carwatch_abort("Raw-log conversion requires usable `study_metadata` registration events.", "carwatch_schema_error")
  keys <- vapply(configs, .registration_key, character(1)); configs[!duplicated(keys)]
}

.registration_schedule <- function(raw_logs, protocol_manifest = NULL) {
  protocol <- .protocol_from_logs(raw_logs, protocol_manifest)
  rows <- list(); day_number <- 0L
  for (registration in seq_along(protocol)) {
    config <- protocol[[registration]]
    for (registration_day in seq_len(config$study_days)) {
      day_number <- day_number + 1L
      for (position in seq_along(config$saliva_ids)) {
        type <- if (position <= length(config$saliva_times)) "relative" else "absolute"
        interval <- if (type == "relative") config$saliva_times[[position]] else NA_real_
        absolute_position <- position - length(config$saliva_times)
        absolute <- if (type == "absolute" && absolute_position <= length(config$saliva_absolute_times)) config$saliva_absolute_times[[absolute_position]] else NA_character_
        rows[[length(rows) + 1L]] <- tibble::tibble(registration = registration, study_name = config$study_name, registration_day = registration_day, day = paste0("D", day_number), scheduled_sample = config$saliva_ids[[position]], sample_position = position, schedule_type = type, expected_interval_min = interval, absolute_clock = absolute)
      }
    }
  }
  dplyr::bind_rows(rows)
}

#' Extract the registration-aware protocol schedule
#' @param raw_logs Immutable events returned by [read_raw_logs()].
#' @param protocol_manifest Optional ordered registration configuration.
#' @param errors Unresolved-issue handling: "raise", "warn", or legacy "error".
#' @return A tibble with registration-aware sample positions.
#' @export
extract_registration_schedule <- function(raw_logs, protocol_manifest = NULL, errors = c("error", "warn")) {
  errors <- match.arg(errors)
  .require_columns(raw_logs, c("participant", "timestamp", "action", "payload", "source_file"), "Raw logs")
  .registration_schedule(raw_logs, protocol_manifest)
}

#' Summarize the reconstructed study protocol
#' @param raw_logs Immutable events returned by [read_raw_logs()].
#' @param protocol_manifest Optional ordered registration configuration.
#' @param errors Unresolved-issue handling.
#' @return A registration-level tibble.
#' @export
summarize_protocol <- function(raw_logs, protocol_manifest = NULL, errors = c("error", "warn")) {
  schedule <- extract_registration_schedule(raw_logs, protocol_manifest, errors)
  dplyr::summarise(dplyr::group_by(schedule, .data$registration, .data$study_name), canonical_days = list(unique(.data$day)), study_days = dplyr::n_distinct(.data$day), saliva_ids = list(unique(.data$scheduled_sample)), sample_positions = dplyr::n_distinct(.data$sample_position), .groups = "drop")
}

.coerce_event_day <- function(event, schedule, current_registration) {
  expected <- suppressWarnings(as.integer(.payload_value(event$payload[[1]], "day_expected", 1L)))
  row <- dplyr::filter(schedule, .data$registration == current_registration, .data$registration_day == expected)
  if (!nrow(row)) NA_character_ else row$day[[1]]
}

.day_timestamp <- function(day_date, clock, tz) {
  if (is.na(day_date) || is.na(clock) || !nzchar(clock)) return(as.POSIXct(NA))
  .parse_local_time(sprintf("%s %s:00", format(day_date, "%Y-%m-%d"), clock), tz, "scheduled sampling time")
}

.conversion_report <- function(issues, result) {
  list(
    results = result,
    report = list(
      summary = list(input_event_count = NA_integer_, output_participant_count = nrow(result), canonical_day_count = length(unique(attr(result, "column_spec")$day)), issue_count = nrow(issues)),
      issues = issues
    )
  )
}

.replace_long_value <- function(long, participant, day, sample, variable, value) {
  index <- which(long$participant == participant & long$day == day & long$sample == sample & long$variable == variable)
  if (length(index) != 1L) .carwatch_abort("Conversion patch could not resolve one canonical result value.", "carwatch_schema_error")
  long$value[[index]] <- value
  long
}

.manual_timestamp <- function(manual_diary, participant, day, column, timezone) {
  if (is.null(manual_diary)) .carwatch_abort("A conversion decision requires a manual diary.", "carwatch_value_error")
  .require_columns(manual_diary, c("participant", "day", column), "Manual diary")
  row <- manual_diary[manual_diary$participant == participant & manual_diary$day == day, , drop = FALSE]
  if (nrow(row) != 1L || is.na(row[[column]][[1]])) .carwatch_abort(sprintf("Manual diary has no %s value for participant=%s, day=%s.", column, participant, day), "carwatch_value_error")
  value <- row[[column]][[1]]
  if (inherits(value, "POSIXt")) return(value)
  value <- as.character(value)
  if (nchar(value) == 16L) value <- paste0(value, ":00")
  .parse_local_time(value, timezone, sprintf("manual diary %s", column))
}

.scheduled_patch_time <- function(long, schedule, participant, day, sample, timezone, fallback = NULL) {
  expected <- schedule[schedule$day == day & schedule$scheduled_sample == sample, , drop = FALSE]
  if (nrow(expected) != 1L) .carwatch_abort("Schedule patch does not resolve one expected sample.", "carwatch_value_error")
  row <- expected[1, , drop = FALSE]
  date_index <- which(long$participant == participant & long$day == day & long$sample == "day" & long$variable == "date")
  awakening_index <- which(long$participant == participant & long$day == day & long$sample == "day" & long$variable == "awakening_time")
  collection_date <- long$value[[date_index[[1]]]]
  awakening <- long$value[[awakening_index[[1]]]]
  if (row$schedule_type[[1]] == "absolute") return(.day_timestamp(collection_date, row$absolute_clock[[1]], timezone))
  prior <- schedule[schedule$day == day & schedule$sample_position <= row$sample_position[[1]] & schedule$schedule_type == "relative", , drop = FALSE]
  offsets <- sum(prior$expected_interval_min, na.rm = TRUE)
  if (is.na(awakening)) .carwatch_abort("Cannot apply a relative schedule patch without an awakening time.", "carwatch_value_error")
  awakening + offsets * 60
}

.apply_conversion_patches <- function(long, report, schedule, manual_diary, sampling_schedule, timezone) {
  selected <- .decision_rows(report)
  if (!nrow(selected)) return(long)
  for (index in seq_len(nrow(selected))) {
    item <- selected[index, , drop = FALSE]
    participant <- item$participant[[1]]; day <- item$day[[1]]; sample <- item$sample_id[[1]]
    action <- item$user_decision[[1]]; value <- item$user_decision_value[[1]]
    if (item$code[[1]] == "missing_awakening_time" && ((action == "accept" && item$proposed_action[[1]] == "use_manual_diary_awakening_time") || (action == "change" && value %in% c("use_manual_diary_awakening_time", "manual_diary")))) {
      timestamp <- .manual_timestamp(manual_diary, participant, day, "awakening_time", timezone)
      long <- .replace_long_value(long, participant, day, "day", "awakening_time", timestamp)
      long <- .replace_long_value(long, participant, day, "day", "date", as.POSIXct(as.Date(timestamp), tz = timezone))
      long <- .replace_long_value(long, participant, day, "day", "awakening_type", "manual_diary")
    }
    sample_patch <- item$code[[1]] == "missing_scheduled_sample_event" && ((action == "accept" && item$proposed_action[[1]] == "use_manual_diary_sampling_time") || (action == "change" && value %in% c("use_manual_diary_sampling_time", "use_default")))
    if (sample_patch) {
      timestamp <- if (action == "change" && value == "use_default") .scheduled_patch_time(long, schedule, participant, day, sample, timezone, sampling_schedule) else .manual_timestamp(manual_diary, participant, day, paste0("sampling_time_", item$sample_position[[1]]), timezone)
      long <- .replace_long_value(long, participant, day, sample, "sampling_time", timestamp)
      long <- .replace_long_value(long, participant, day, sample, "sampling_time_source", if (action == "change" && value == "use_default") "schedule" else "manual_diary")
    }
  }
  long
}

.append_conversion_compliance <- function(long, participants, checker) {
  long <- long[!long$variable %in% c("actual_interval_min", "time_deviation_min", "sample_compliant", "expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count", "non_compliant_samples", "day_compliant"), , drop = FALSE]
  result <- .from_long_results(long, participants)
  samples <- as_sample_events(result)
  if (!nrow(samples)) return(long)
  samples$scheduled_sample <- samples$sample
  samples <- .evaluate_sampling_compliance(samples, checker)
  additions <- lapply(c("actual_interval_min", "time_deviation_min", "sample_compliant"), function(variable) tibble::tibble(participant = samples$participant, day = samples$day, sample = samples$sample, variable = variable, value = as.list(samples[[variable]])))
  day_values <- lapply(split(samples, interaction(samples$participant, samples$day, drop = TRUE)), function(group) {
    failed <- group$sample[group$sample_compliant %in% FALSE]
    tibble::tibble(participant = group$participant[[1]], day = group$day[[1]], sample = "day", variable = c("expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count", "non_compliant_samples", "day_compliant"), value = list(nrow(group), sum(!is.na(group$recorded_sample)), sum(!is.na(group$sample_compliant)), sum(group$sample_compliant %in% TRUE), if (length(failed)) paste(failed, collapse = ";") else NA_character_, all(group$sample_compliant %in% TRUE) && !anyNA(group$sample_compliant)))
  })
  dplyr::bind_rows(long, dplyr::bind_rows(additions), dplyr::bind_rows(day_values))
}

#' Convert immutable CARWatch raw logs to canonical Study Results
#'
#' The first pass only reports issues. Decisions passed through `issue_decisions`
#' are the only allowed route for corrections; raw event rows are never changed.
#'
#' @param raw_logs Immutable events returned by [read_raw_logs()].
#' @param protocol_manifest Optional ordered registration configuration.
#' @param errors Unresolved-issue handling: "raise", "warn", or legacy "error".
#' @param create_report Return a list containing results and the issue report.
#' @param issue_decisions A prior editable issue report with decisions.
#' @param sampling_schedule Optional fallback schedule.
#' @param manual_diary Optional normalized manual diary.
#' @param check_compliance Whether to calculate timing compliance.
#' @param compliance_checker Timing tolerance configuration.
#' @return Canonical results, or a results/report list.
#' @export
convert_raw_logs <- function(raw_logs, protocol_manifest = NULL, errors = c("error", "warn"), create_report = FALSE, issue_decisions = NULL, sampling_schedule = NULL, manual_diary = NULL, check_compliance = TRUE, compliance_checker = new_sampling_compliance_checker()) {
  errors <- match.arg(errors, c("raise", "warn", "error")); if (identical(errors, "error")) errors <- "raise"
  .assert_scalar_logical(create_report, "create_report"); .assert_scalar_logical(check_compliance, "check_compliance")
  .require_columns(raw_logs, c("participant", "timestamp", "action", "payload", "source_file"), "Raw logs")
  raw_logs <- .deduplicate_raw_events(raw_logs)
  decision_frame <- if (is.null(issue_decisions)) NULL else .normalize_issue_decisions(issue_decisions)
  report_state <- .new_conversion_report(raw_logs, decision_frame)
  schedule <- .registration_schedule(raw_logs, protocol_manifest)
  participants <- sort(unique(.as_character_id(raw_logs$participant, "Raw-log participant IDs")))
  timezone <- attr(raw_logs$timestamp, "tzone") %||% "Europe/Berlin"
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  issues <- list(); records <- list()
  for (participant in participants) {
    events <- dplyr::arrange(dplyr::filter(raw_logs, .data$participant == .env$participant), .data$timestamp)
    registrations <- lapply(events$payload[events$action == "study_metadata"], .registration_config)
    registrations <- registrations[!vapply(registrations, is.null, logical(1))]
    if (!length(registrations)) {
      issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("missing_registration_metadata", participant), code = "missing_registration_metadata", participant = participant, day = NA_character_, sample_id = NA_character_, proposed_action = "", user_decision = "", resolution_status = "unresolved")
      next
    }
    active_registration <- NA_integer_
    awakening_day_counter <- 0L
    current_config_key <- .registration_key(registrations[[1]])
    registration_sources <- unique(events$source_file[events$action == "study_metadata"])
    collection_started <- FALSE
    metadata_seen <- FALSE
    for (i in seq_len(nrow(events))) {
      event <- events[i, , drop = FALSE]
      if (event$action[[1]] == "study_metadata") {
        config <- .registration_config(event$payload[[1]])
        if (!is.null(config)) {
          new_key <- .registration_key(config)
          if (identical(new_key, current_config_key) && collection_started) issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("possible_reregistration", participant, details = list(timestamp = format(event$timestamp[[1]], "%Y-%m-%dT%H:%M:%S%z"))), code = "possible_reregistration", participant = participant, day = NA_character_, sample_id = NA_character_, proposed_action = "reuse_registration", user_decision = "", resolution_status = "unresolved")
          current_config_key <- new_key
          position <- which(vapply(.protocol_from_logs(raw_logs, protocol_manifest), function(item) identical(.registration_key(item), current_config_key), logical(1)))
          if (length(position)) active_registration <- position[[1]]
          metadata_seen <- TRUE
        } else issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("invalid_study_metadata", participant, details = list(source_file = event$source_file[[1]])), code = "invalid_study_metadata", participant = participant, day = NA_character_, sample_id = NA_character_, proposed_action = "ignore_metadata_event", user_decision = "", resolution_status = "unresolved")
        next
      }
      if (!event$action[[1]] %in% c("barcode_scanned", "spontaneous_awakening", "alarm")) next
      if (event$action[[1]] == "barcode_scanned" && !metadata_seen) {
        issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("scan_before_registration_metadata", participant, details = list(source_file = event$source_file[[1]])), code = "scan_before_registration_metadata", participant = participant, day = NA_character_, sample_id = NA_character_, proposed_action = "drop_sample", user_decision = "", resolution_status = "unresolved")
        next
      }
      if (event$action[[1]] == "barcode_scanned") collection_started <- TRUE
      if (event$action[[1]] %in% c("spontaneous_awakening", "alarm")) {
        awakening_day_counter <- awakening_day_counter + 1L
        day_row <- dplyr::filter(schedule, .data$registration == .env$active_registration, .data$registration_day == .env$awakening_day_counter)
        day <- if (nrow(day_row)) day_row$day[[1]] else NA_character_
      } else {
        expected_day <- suppressWarnings(as.integer(.payload_value(event$payload[[1]], "day_expected", 1L)))
        active_rows <- schedule[schedule$registration == active_registration, , drop = FALSE]
        if (is.na(expected_day) || !expected_day %in% active_rows$registration_day) {
          issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("invalid_day_expected", participant, details = list(day_expected = expected_day, source_file = event$source_file[[1]])), code = "invalid_day_expected", participant = participant, day = NA_character_, sample_id = NA_character_, proposed_action = "drop_sample", user_decision = "", resolution_status = "unresolved")
          next
        }
        expected_sample <- as.character(.payload_value(event$payload[[1]], "sample_expected", ""))
        if (!expected_sample %in% active_rows$scheduled_sample) {
          issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("expected_sample_not_in_active_metadata", participant, details = list(sample_expected = expected_sample, source_file = event$source_file[[1]])), code = "expected_sample_not_in_active_metadata", participant = participant, day = NA_character_, sample_id = expected_sample, proposed_action = "override_expected_sample", user_decision = "", resolution_status = "unresolved")
          next
        }
        day <- .coerce_event_day(event, schedule, active_registration)
      }
      if (is.na(day)) next
      records[[length(records) + 1L]] <- tibble::tibble(participant = participant, day = day, action = event$action[[1]], timestamp = event$timestamp[[1]], payload = list(event$payload[[1]]), source_file = event$source_file[[1]], registration = active_registration, registration_sources = paste(registration_sources, collapse = ";"))
    }
  }
  event_records <- if (length(records)) dplyr::bind_rows(records) else tibble::tibble(participant = character(), day = character(), action = character(), timestamp = as.POSIXct(character()), payload = list(), source_file = character(), registration = integer(), registration_sources = character())
  output <- list()
  for (participant in participants) for (day in unique(schedule$day)) {
    positions <- dplyr::filter(schedule, .data$day == .env$day)
    events <- dplyr::filter(event_records, .data$participant == .env$participant, .data$day == .env$day)
    awakening <- dplyr::filter(events, .data$action %in% c("spontaneous_awakening", "alarm"))
    awakening_time <- if (nrow(awakening)) awakening$timestamp[[1]] else as.POSIXct(NA)
    collection_date <- if (nrow(events)) as.Date(events$timestamp[[1]]) else as.Date(NA)
    scan_dates <- unique(as.Date(events$timestamp[events$action == "barcode_scanned"]))
    if (length(scan_dates) > 1L) issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("multiple_collection_dates", participant, day, details = list(dates = sort(as.character(scan_dates)))), code = "multiple_collection_dates", participant = participant, day = day, sample_id = NA_character_, proposed_action = "drop_day", user_decision = "", resolution_status = "unresolved")
    if (!nrow(awakening)) issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("missing_awakening_time", participant, day), code = "missing_awakening_time", participant = participant, day = day, sample_id = NA_character_, proposed_action = "use_manual_diary_awakening_time", user_decision = "", resolution_status = "unresolved")
    for (j in seq_len(nrow(positions))) {
      position <- positions[j, ]
      expected_match <- vapply(events$payload, function(payload) identical(as.character(.payload_value(payload, "sample_expected", "")), as.character(position$scheduled_sample)), logical(1))
      scan <- events[events$action == "barcode_scanned" & expected_match, , drop = FALSE]
      if (nrow(scan) > 1L) issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("duplicate_scheduled_sample_events", participant, day, position$scheduled_sample, details = list(event_count = nrow(scan))), code = "duplicate_scheduled_sample_events", participant = participant, day = day, sample_id = position$scheduled_sample, proposed_action = "drop_sample", user_decision = "", resolution_status = "unresolved")
      payload <- if (nrow(scan)) scan$payload[[1]] else list()
      sampling_time <- if (nrow(scan)) scan$timestamp[[1]] else as.POSIXct(NA)
      if (!nrow(scan)) issues[[length(issues) + 1L]] <- tibble::tibble(issue_id = .issue_id("missing_scheduled_sample_event", participant, day, position$scheduled_sample), code = "missing_scheduled_sample_event", participant = participant, day = day, sample_id = position$scheduled_sample, proposed_action = "use_manual_diary_sampling_time", user_decision = "", resolution_status = "unresolved")
      scheduled <- if (position$schedule_type == "absolute") .day_timestamp(collection_date, position$absolute_clock, timezone) else as.POSIXct(NA)
      recorded_sample <- .payload_value(payload, "sample_scanned", NA_character_)
      mismatch <- if (!is.na(recorded_sample) && recorded_sample != position$scheduled_sample) paste0(position$scheduled_sample, "->", recorded_sample) else NA_character_
      output[[length(output) + 1L]] <- tibble::tibble(participant = participant, day = day, sample = position$scheduled_sample, variable = c("sampling_time", "barcode", "recorded_sample", "sampling_time_source", "sample_position", "day_expected", "day_scanned", "schedule_type", "expected_interval_min", "scheduled_sampling_time", "awakening_time", "awakening_type", "mismatch_summary", "registration", "study_name", "registration_day", "registration_sources", "possible_reregistration", "date"), value = list(sampling_time, .payload_value(payload, "barcode_value", NA_character_), recorded_sample, if (nrow(scan)) "app" else NA_character_, position$sample_position, .payload_value(payload, "day_expected", NA_integer_), .payload_value(payload, "day_scanned", NA_integer_), position$schedule_type, position$expected_interval_min, scheduled, awakening_time, if (nrow(awakening)) awakening$action[[1]] else NA_character_, mismatch, position$registration, position$study_name, position$registration_day, if (nrow(events)) events$registration_sources[[1]] else NA_character_, FALSE, as.POSIXct(collection_date)))
    }
  }
  long <- dplyr::bind_rows(output)
  long <- dplyr::mutate(long, sample = ifelse(.data$variable %in% c("date", "awakening_time", "awakening_type", "mismatch_summary", "registration", "study_name", "registration_day", "registration_sources", "possible_reregistration"), "day", .data$sample))
  long <- dplyr::slice_head(dplyr::group_by(long, .data$participant, .data$day, .data$sample, .data$variable), n = 1L) |>
    dplyr::ungroup()
  results <- .from_long_results(long, participants)
  samples <- as_sample_events(results); days <- as_study_days(results)
  if (nrow(samples)) for (group in split(samples, interaction(samples$participant, samples$day, drop = TRUE, lex.order = TRUE))) {
    ordered <- group[order(group$sample_position), , drop = FALSE]
    recorded <- ordered[!is.na(ordered$sampling_time), , drop = FALSE]
    if (nrow(recorded) > 1L && any(diff(as.numeric(recorded$sampling_time)) <= 0)) {
      issues[[length(issues) + 1L]] <- tibble::tibble(
        issue_id = .issue_id("non_increasing_sampling_times", recorded$participant[[1]], recorded$day[[1]], details = list(sample_positions = recorded$sample_position, timestamps = format(recorded$sampling_time, "%Y-%m-%dT%H:%M:%S%z"))),
        code = "non_increasing_sampling_times", participant = recorded$participant[[1]], day = recorded$day[[1]], sample_id = NA_character_,
        proposed_action = "drop_day", user_decision = "", resolution_status = "unresolved"
      )
    }
  }
  if (check_compliance && nrow(samples)) {
    samples$scheduled_sample <- samples$sample
    if (!"awakening_time" %in% names(samples)) samples <- dplyr::left_join(samples, dplyr::select(days, dplyr::all_of(c("participant", "day", "awakening_time"))), by = c("participant", "day"))
    samples <- .evaluate_sampling_compliance(samples, compliance_checker)
    add <- dplyr::bind_rows(lapply(c("actual_interval_min", "time_deviation_min", "sample_compliant"), function(current_variable) {
      tibble::tibble(participant = samples$participant, day = samples$day, sample = samples$sample, variable = current_variable, value = as.list(samples[[current_variable]]))
    }))
    long <- dplyr::bind_rows(long, add); results <- .from_long_results(long, participants)
    day_additions <- lapply(split(samples, interaction(samples$participant, samples$day, drop = TRUE, lex.order = TRUE)), function(group) {
      non_compliant <- group$sample[group$sample_compliant %in% FALSE]
      tibble::tibble(participant = group$participant[[1]], day = group$day[[1]], sample = "day", variable = c("expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count", "non_compliant_samples", "day_compliant"), value = list(nrow(group), sum(!is.na(group$recorded_sample)), sum(!is.na(group$sample_compliant)), sum(group$sample_compliant %in% TRUE), if (length(non_compliant)) paste(non_compliant, collapse = ";") else NA_character_, all(group$sample_compliant %in% TRUE) && !anyNA(group$sample_compliant)))
    })
    long <- dplyr::bind_rows(long, dplyr::bind_rows(day_additions)); results <- .from_long_results(long, participants)
  }
  issue_frame <- if (length(issues)) dplyr::bind_rows(issues) else tibble::tibble(issue_id = character(), code = character(), participant = character(), day = character(), sample_id = character(), proposed_action = character(), user_decision = character(), resolution_status = character())
  # Reissue the discovered anomalies through the stable report model. The
  # default `accept` cells are advisory; only rows supplied through
  # `issue_decisions` count as executable decisions.
  if (nrow(issue_frame)) for (row in seq_len(nrow(issue_frame))) {
    item <- issue_frame[row, , drop = FALSE]
    context <- schedule[schedule$day == item$day[[1]] & schedule$scheduled_sample == item$sample_id[[1]], , drop = FALSE]
    added <- .report_add_issue(report_state, code = item$code[[1]], message = item$code[[1]], participant = item$participant[[1]], day = item$day[[1]], sample_id = item$sample_id[[1]], registration = if (nrow(context)) context$registration[[1]] else NA_integer_, registration_day = if (nrow(context)) context$registration_day[[1]] else NA_integer_, sample_position = if (nrow(context)) context$sample_position[[1]] else NA_integer_, proposed_action = item$proposed_action[[1]])
    report_state <- added$report
  }
  if (!is.null(decision_frame) && nrow(report_state$issues)) {
    long <- .apply_conversion_patches(long, report_state, schedule, manual_diary, sampling_schedule, timezone)
    if (check_compliance) long <- .append_conversion_compliance(long, participants, compliance_checker)
    results <- .from_long_results(long, participants)
    selected <- report_state$issues[report_state$issues$resolution_status == "resolved", c("participant", "day", "sample_id", "user_decision", "proposed_action"), drop = FALSE]
    if (nrow(selected)) {
      selected$effective_action <- ifelse(selected$user_decision == "accept" & selected$proposed_action %in% c("drop_participant", "drop_day", "drop_sample"), selected$proposed_action, selected$user_decision)
      remove_participants <- selected$participant[selected$effective_action == "drop_participant"]
      remove_days <- selected[selected$effective_action == "drop_day", c("participant", "day"), drop = FALSE]
      remove_samples <- selected[selected$effective_action == "drop_sample", c("participant", "day", "sample_id"), drop = FALSE]
      keep <- !long$participant %in% remove_participants
      if (nrow(remove_days)) keep <- keep & !paste(long$participant, long$day, sep = "\r") %in% paste(remove_days$participant, remove_days$day, sep = "\r")
      if (nrow(remove_samples)) keep <- keep & !(long$sample != "day" & paste(long$participant, long$day, long$sample, sep = "\r") %in% paste(remove_samples$participant, remove_samples$day, remove_samples$sample_id, sep = "\r"))
      long <- long[keep, , drop = FALSE]
      results <- .from_long_results(long, setdiff(participants, remove_participants))
    }
  }
  if (errors == "raise" && nrow(report_state$issues) && is.null(issue_decisions)) .carwatch_abort("Raw-log conversion encountered unresolved issues. Run with `errors = 'warn'` and `create_report = TRUE` to review them.", "carwatch_schema_error")
  if (create_report) return(list(results = results, report = .report_finalize(report_state, results, schedule, sum(event_records$action == "barcode_scanned"))))
  results
}

#' Read a conversion-issue report CSV
#' @param path CSV path.
#' @return A validated editable issue tibble.
#' @export
read_conversion_report <- function(path) {
  path <- .assert_file(path, "csv")
  data <- readr::read_csv(path, show_col_types = FALSE)
  .normalize_issue_decisions(data)
}
