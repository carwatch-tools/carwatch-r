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

# Report messages are an audit interface shared with the Python release.
# Keep this separate from JSON serialization: Python f-string ``!r`` uses
# single-quoted representations, whereas JSON correctly uses double quotes.
.python_repr <- function(value) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) return("None")
  if (inherits(value, "POSIXt")) return(paste0("'", .json_safe(value), "'"))
  if (is.character(value)) return(paste0("[", paste(paste0("'", gsub("'", "\\\\'", value, fixed = TRUE), "'"), collapse = ", "), "]"))
  if (is.numeric(value) || is.integer(value)) return(paste0("[", paste(as.character(value), collapse = ", "), "]"))
  if (is.logical(value)) return(paste0("[", paste(ifelse(value, "True", "False"), collapse = ", "), "]"))
  if (is.list(value)) {
    if (is.null(names(value))) return(paste0("[", paste(vapply(value, .python_repr, character(1)), collapse = ", "), "]"))
    return(paste0("{", paste(sprintf("'%s': %s", names(value), vapply(value, .python_repr, character(1))), collapse = ", "), "}"))
  }
  paste0("'", as.character(value), "'")
}

.python_scalar_repr <- function(value) {
  result <- .python_repr(value)
  if (is.character(value) && length(value) == 1L) return(sub("^\\[(.*)\\]$", "\\1", result))
  if ((is.numeric(value) || is.integer(value) || is.logical(value)) && length(value) == 1L) return(sub("^\\[(.*)\\]$", "\\1", result))
  result
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
    keys <- vapply(configs, .registration_key, character(1))
    if (anyDuplicated(keys)) .carwatch_abort("Protocol manifest contains duplicate registration configurations.", "carwatch_schema_error")
    metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
    observed <- lapply(metadata$payload, .registration_config)
    observed <- observed[!vapply(observed, is.null, logical(1))]
    unknown <- setdiff(vapply(observed, .registration_key, character(1)), keys)
    if (length(unknown)) .carwatch_abort("Raw logs contain registration configurations not present in the protocol manifest.", "carwatch_schema_error")
    return(configs)
  }
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  configs <- lapply(metadata$payload, .registration_config)
  configs <- configs[!vapply(configs, is.null, logical(1))]
  if (!length(configs)) .carwatch_abort("Raw-log conversion requires usable `study_metadata` registration events.", "carwatch_schema_error")
  keys <- vapply(configs, .registration_key, character(1))
  config_by_key <- configs[!duplicated(keys)]
  names(config_by_key) <- keys[!duplicated(keys)]
  observed <- dplyr::arrange(metadata, .data$participant, .data$timestamp, .data$source_file)
  observed$config_key <- vapply(observed$payload, function(payload) {
    config <- .registration_config(payload)
    if (is.null(config)) NA_character_ else .registration_key(config)
  }, character(1))
  sequences <- lapply(split(observed$config_key, observed$participant, drop = TRUE), function(values) unique(values[!is.na(values)]))
  first_seen <- vapply(names(config_by_key), function(key) min(observed$timestamp[observed$config_key == key]), as.POSIXct(NA))
  position <- lapply(names(config_by_key), function(key) unlist(lapply(sequences, function(sequence) match(key, sequence)), use.names = FALSE))
  names(position) <- names(config_by_key)
  precedes <- matrix(0L, nrow = length(config_by_key), ncol = length(config_by_key), dimnames = list(names(config_by_key), names(config_by_key)))
  for (sequence in sequences) if (length(sequence) > 1L) for (left in seq_along(sequence)) if (left < length(sequence)) for (right in (left + 1L):length(sequence)) precedes[sequence[[left]], sequence[[right]]] <- precedes[sequence[[left]], sequence[[right]]] + 1L
  edges <- matrix(FALSE, nrow = length(config_by_key), ncol = length(config_by_key), dimnames = dimnames(precedes))
  for (left in seq_len(nrow(precedes))) for (right in seq_len(ncol(precedes))) if (left != right && precedes[left, right] > precedes[right, left]) edges[left, right] <- TRUE
  sort_key <- function(key) {
    positions <- position[[key]]
    c(if (length(positions)) stats::median(positions, na.rm = TRUE) else Inf, as.numeric(first_seen[[key]]), match(key, names(config_by_key)))
  }
  remaining <- names(config_by_key)
  ordered <- character()
  while (length(remaining)) {
    incoming <- vapply(remaining, function(key) any(edges[remaining, key, drop = TRUE]), logical(1))
    available <- remaining[!incoming]
    if (!length(available)) available <- remaining
    sort_matrix <- vapply(available, sort_key, numeric(3))
    candidate <- available[[order(sort_matrix[1, ], sort_matrix[2, ], sort_matrix[3, ])[1L]]]
    ordered <- c(ordered, candidate)
    remaining <- setdiff(remaining, candidate)
  }
  unname(config_by_key[ordered])
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

.protocol_order_issues <- function(raw_logs, protocol_manifest = NULL) {
  if (!is.null(protocol_manifest)) return(list())
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  configs <- lapply(metadata$payload, .registration_config)
  keys <- vapply(configs, function(config) if (is.null(config)) NA_character_ else .registration_key(config), character(1))
  valid <- !is.na(keys)
  keys <- keys[valid]
  participants <- metadata$participant[valid]
  sequences <- lapply(split(keys, participants, drop = TRUE), unique)
  unique_keys <- unique(keys)
  if (length(unique_keys) < 2L) return(list())
  precedes <- matrix(0L, nrow = length(unique_keys), ncol = length(unique_keys), dimnames = list(unique_keys, unique_keys))
  for (sequence in sequences) if (length(sequence) > 1L) for (left in seq_along(sequence)) if (left < length(sequence)) for (right in (left + 1L):length(sequence)) precedes[sequence[[left]], sequence[[right]]] <- precedes[sequence[[left]], sequence[[right]]] + 1L
  issues <- list()
  for (left in seq_len(nrow(precedes) - 1L)) for (right in (left + 1L):ncol(precedes)) if (precedes[left, right] == precedes[right, left] && precedes[left, right] > 0L) {
    left_config <- configs[[match(rownames(precedes)[[left]], keys)]]
    right_config <- configs[[match(rownames(precedes)[[right]], keys)]]
    details <- list(left_saliva_ids = left_config$saliva_ids, right_saliva_ids = right_config$saliva_ids, left_before_count = precedes[left, right], right_before_count = precedes[right, left])
    issues[[length(issues) + 1L]] <- .new_conversion_issue(
      "ambiguous_protocol_order",
      sprintf("Protocol registration order is ambiguous between sample sets %s and %s. Supply an ordered protocol manifest.", .python_repr(left_config$saliva_ids), .python_repr(right_config$saliva_ids)),
      details = details,
      proposed_action = "use_deterministic_protocol_order",
      proposed_action_description = "The tied transition is omitted from cohort precedence; remaining evidence and deterministic first-observed order are used. Supply a manifest to fix the order explicitly."
    )
  }
  edges <- precedes > t(precedes)
  diag(edges) <- FALSE
  remaining <- unique_keys
  cyclic <- FALSE
  while (length(remaining)) {
    incoming <- vapply(remaining, function(key) any(edges[remaining, key, drop = TRUE]), logical(1))
    available <- remaining[!incoming]
    if (!length(available)) {
      cyclic <- TRUE
      break
    }
    remaining <- setdiff(remaining, available[[1]])
  }
  if (cyclic) {
    sequences_details <- lapply(split(keys, participants, drop = TRUE), function(sequence) lapply(unique(sequence), function(key) configs[[match(key, keys)]]$saliva_ids))
    issues[[length(issues) + 1L]] <- .new_conversion_issue(
    "cyclic_protocol_order",
    "Protocol registration transitions contain a cycle. Supply an ordered protocol manifest and inspect conflicting participants.",
    details = list(participant_sequences = sequences_details),
    proposed_action = "use_deterministic_protocol_order",
    proposed_action_description = "A deterministic median-position and first-observed fallback order is used. Participant-specific violations remain reported. Supply a manifest to resolve the conflict."
    )
  }
  issues
}

.registration_order_violation_issues <- function(raw_logs, protocol_manifest = NULL) {
  protocol <- .protocol_from_logs(raw_logs, protocol_manifest)
  protocol_keys <- vapply(protocol, .registration_key, character(1))
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  if (!nrow(metadata)) return(list())
  metadata <- dplyr::arrange(metadata, .data$participant, .data$timestamp, .data$source_file)
  issues <- list()
  for (participant in unique(as.character(metadata$participant))) {
    events <- metadata[metadata$participant == participant, , drop = FALSE]
    observed <- lapply(events$payload, .registration_config)
    keys <- vapply(observed, function(config) if (is.null(config)) NA_character_ else .registration_key(config), character(1))
    registration <- match(keys, protocol_keys)
    valid <- !is.na(registration)
    if (sum(valid) < 2L) next
    previous_index <- NA_integer_
    for (index in which(valid)) {
      if (!is.na(previous_index) && registration[[index]] < registration[[previous_index]]) {
        current <- observed[[index]]
        previous <- observed[[previous_index]]
        details <- list(
          observed_registrations = as.integer(registration[valid]),
          previous = list(
            registration = as.integer(registration[[previous_index]]),
            study_name = previous$study_name,
            saliva_ids = previous$saliva_ids,
            timestamp = events$timestamp[[previous_index]],
            metadata_source_files = sort(unique(unlist(strsplit(events$source_file[[previous_index]], ";", fixed = TRUE), use.names = FALSE)))
          ),
          current = list(
            registration = as.integer(registration[[index]]),
            study_name = current$study_name,
            saliva_ids = current$saliva_ids,
            timestamp = events$timestamp[[index]],
            metadata_source_files = sort(unique(unlist(strsplit(events$source_file[[index]], ";", fixed = TRUE), use.names = FALSE)))
          )
        )
        issues[[length(issues) + 1L]] <- .new_conversion_issue(
          "registration_order_violation",
          paste0(
            "Participant registration sequence violates the canonical protocol order: participant=", shQuote(participant),
            ", observed_registrations=", .python_repr(as.integer(registration[valid])), ". Registration ", registration[[index]],
            " [study_name=", shQuote(current$study_name), "; saliva_ids=", .python_repr(current$saliva_ids),
            "; timestamp=", .python_repr(events$timestamp[[index]]), "; metadata_sources=", .python_repr(details$current$metadata_source_files),
            "] was recorded again after registration ", registration[[previous_index]],
            " [study_name=", shQuote(previous$study_name), "; saliva_ids=", .python_repr(previous$saliva_ids),
            "; timestamp=", .python_repr(events$timestamp[[previous_index]]), "; metadata_sources=", .python_repr(details$previous$metadata_source_files),
            "]. Expected canonical registration numbers to increase chronologically. Proposed action: retain the cohort-derived canonical order, map this metadata event to the existing registration ", registration[[index]], ", and do not create an additional canonical study day."
          ),
          participant,
          registration = registration[[index]],
          details = details,
          proposed_action = "reuse_registration",
          proposed_action_description = "The cohort-derived canonical registration order is retained; this metadata event does not create another canonical study day."
        )
      }
      previous_index <- index
    }
  }
  issues
}

.protocol_issue_message <- function(issue) {
  code <- issue$code[[1]]
  if (code == "ambiguous_protocol_order") {
    return("Cohort registration order is ambiguous; provide `protocol_manifest` to define the canonical order.")
  }
  if (code == "cyclic_protocol_order") {
    return("Cohort registration order contains a cycle; provide `protocol_manifest` to define the canonical order.")
  }
  if (code == "registration_order_violation") {
    return("A metadata registration was recorded after a later canonical registration. Retain the cohort-derived canonical order and do not create an additional canonical study day.")
  }
  code
}

.issue_description <- function(code) {
  switch(code,
    invalid_study_metadata = "Ignore this unusable metadata event.",
    scan_before_registration_metadata = "Drop the scan because no usable registration is active.",
    invalid_day_expected = "Drop the scan because its expected day is outside the active registration.",
    multiple_collection_dates = "Use the earliest collection date or explicitly map every collection date to a canonical day.",
    possible_reregistration = "Reuse the active registration, or change to a compatible canonical registration.",
    missing_awakening_time = "Use the matching manual-diary awakening time or provide an explicit local timestamp.",
    expected_sample_not_in_active_metadata = "Override with an exact registered sample ID or drop the scan.",
    duplicate_scheduled_sample_events = "Keep the earliest scan or reassign a safe recorded sample.",
    non_increasing_sampling_times = "Drop the day or sort a complete scan series by time.",
    missing_scheduled_sample_event = "Use the matching manual-diary sampling time or a schedule fallback.",
    ""
  )
}

.manifest_registration_missing_issues <- function(raw_logs, protocol_manifest) {
  if (is.null(protocol_manifest)) return(list())
  protocol <- .protocol_from_logs(raw_logs, protocol_manifest)
  protocol_keys <- vapply(protocol, .registration_key, character(1))
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  observed <- lapply(metadata$payload, .registration_config)
  observed_keys <- vapply(observed, function(config) if (is.null(config)) NA_character_ else .registration_key(config), character(1))
  issues <- list()
  for (participant in sort(unique(as.character(raw_logs$participant)))) {
    participant_keys <- unique(observed_keys[metadata$participant == participant])
    for (registration in which(!protocol_keys %in% participant_keys)) {
      issues[[length(issues) + 1L]] <- .new_conversion_issue(
        "manifest_registration_missing",
        sprintf("A manifest-defined registration is absent from participant logs: registration=%s, study_name=%s.", registration, shQuote(protocol[[registration]]$study_name)),
        participant,
        registration = registration,
        details = list(saliva_ids = protocol[[registration]]$saliva_ids, canonical_days = unique(.registration_schedule(raw_logs, protocol_manifest)$day[.registration_schedule(raw_logs, protocol_manifest)$registration == registration])),
        proposed_action = "keep_missing_registration",
        proposed_action_description = "Expected schedule rows are created with missing registration timestamp and source provenance."
      )
    }
  }
  issues
}

#' Extract the registration-aware protocol schedule
#' @param raw_logs Immutable events returned by [read_raw_logs()].
#' @param protocol_manifest Optional ordered registration configuration.
#' @param errors Unresolved-issue handling: "raise", "warn", or legacy "error".
#' @return A tibble with registration-aware sample positions.
#' @export
extract_registration_schedule <- function(raw_logs, protocol_manifest = NULL, errors = c("raise", "warn", "error")) {
  errors <- match.arg(errors); if (identical(errors, "error")) errors <- "raise"
  .require_columns(raw_logs, c("participant", "timestamp", "action", "payload", "source_file"), "Raw logs")
  raw_logs <- .deduplicate_raw_events(raw_logs)
  issues <- c(.protocol_order_issues(raw_logs, protocol_manifest), .registration_order_violation_issues(raw_logs, protocol_manifest))
  if (length(issues)) {
    messages <- vapply(issues, .protocol_issue_message, character(1))
    if (errors == "raise") .carwatch_abort(messages[[1]], "carwatch_schema_error")
    warning(paste(messages, collapse = " "), call. = FALSE)
  }
  .registration_schedule(raw_logs, protocol_manifest)
}

#' Summarize the reconstructed study protocol
#' @param raw_logs Immutable events returned by [read_raw_logs()].
#' @param protocol_manifest Optional ordered registration configuration.
#' @param errors Unresolved-issue handling.
#' @return A registration-level tibble.
#' @export
summarize_protocol <- function(raw_logs, protocol_manifest = NULL, errors = c("raise", "warn", "error")) {
  schedule <- extract_registration_schedule(raw_logs, protocol_manifest, errors)
  dplyr::summarise(dplyr::group_by(schedule, .data$registration, .data$study_name), canonical_days = list(unique(.data$day)), study_days = dplyr::n_distinct(.data$day), saliva_ids = list(unique(.data$scheduled_sample)), sample_positions = dplyr::n_distinct(.data$sample_position), .groups = "drop")
}

.coerce_event_day <- function(event, schedule, current_registration) {
  expected <- suppressWarnings(as.integer(.payload_value(event$payload[[1]], "day_expected", 1L)))
  row <- dplyr::filter(schedule, .data$registration == current_registration, .data$registration_day == expected)
  if (!nrow(row)) NA_character_ else row$day[[1]]
}

.reregistration_override_target <- function(decisions, participant, protocol) {
  if (is.null(decisions) || !nrow(decisions)) return(NA_integer_)
  rows <- decisions[decisions$participant == participant & decisions$code == "possible_reregistration" & decisions$user_decision == "change", , drop = FALSE]
  if (!nrow(rows)) return(NA_integer_)
  directive <- tryCatch(jsonlite::fromJSON(rows$user_decision_value[[1]], simplifyVector = FALSE), error = function(error) NULL)
  valid <- is.list(directive) && identical(names(directive), "registration") && length(directive$registration) == 1L
  if (!valid) .carwatch_abort("Registration overrides must be JSON objects containing exactly one `registration` field.", "carwatch_value_error")
  target <- directive$registration
  if (is.character(target)) {
    target <- trimws(target)
    matches <- which(vapply(protocol, function(config) identical(config$study_name, target), logical(1)))
    if (length(matches) != 1L) .carwatch_abort("Registration override references an unknown or ambiguous study name.", "carwatch_value_error")
    return(as.integer(matches[[1]]))
  }
  target <- suppressWarnings(as.integer(target))
  if (is.na(target) || target < 1L || target > length(protocol)) .carwatch_abort("Registration override targets must be a positive canonical registration number or exact study name.", "carwatch_value_error")
  target
}

.map_registration_sample <- function(protocol, source_registration, target_registration, sample) {
  if (is.na(source_registration) || is.na(target_registration) || !nzchar(sample)) return(sample)
  source <- protocol[[source_registration]]$saliva_ids
  target <- protocol[[target_registration]]$saliva_ids
  position <- match(sample, source)
  if (is.na(position)) return(sample)
  if (length(source) != length(target)) .carwatch_abort("Registration override requires source and target schedules with the same number of sample positions.", "carwatch_value_error")
  target[[position]]
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

.missing_long_value <- function(value) {
  if (inherits(value, "POSIXt")) return(as.POSIXct(NA, tz = attr(value, "tzone") %||% "UTC"))
  if (inherits(value, "Date")) return(as.Date(NA))
  if (is.integer(value)) return(NA_integer_)
  if (is.double(value)) return(NA_real_)
  if (is.logical(value)) return(NA)
  NA_character_
}

.blank_long_rows <- function(long, rows) {
  for (row in rows) long$value[[row]] <- .missing_long_value(long$value[[row]])
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

.fallback_schedule_value <- function(fallback, day, sample, position) {
  if (is.null(fallback)) .carwatch_abort("Cannot patch sampling time because registration metadata contains no usable schedule and no fallback `sampling_schedule` was supplied.", "carwatch_value_error")
  values <- fallback
  if (is.list(values) && !is.null(names(values)) && day %in% names(values)) values <- values[[day]]
  if (is.list(values) && !is.null(names(values)) && sample %in% names(values)) return(values[[sample]])
  if (length(values) < position || is.character(values) && length(values) == 1L) .carwatch_abort(sprintf("Fallback sampling_schedule must provide a value for %s/%s.", day, sample), "carwatch_value_error")
  values[[position]]
}

.scheduled_patch_time <- function(long, schedule, participant, day, sample, timezone, fallback = NULL) {
  expected <- schedule[schedule$day == day & schedule$scheduled_sample == sample, , drop = FALSE]
  if (nrow(expected) != 1L) .carwatch_abort("Schedule patch does not resolve one expected sample.", "carwatch_value_error")
  row <- expected[1, , drop = FALSE]
  date_index <- which(long$participant == participant & long$day == day & long$sample == "day" & long$variable == "date")
  awakening_index <- which(long$participant == participant & long$day == day & long$sample == "day" & long$variable == "awakening_time")
  collection_date <- long$value[[date_index[[1]]]]
  awakening <- long$value[[awakening_index[[1]]]]
  if (row$schedule_type[[1]] == "relative" && !is.na(row$expected_interval_min[[1]])) {
    prior <- schedule[schedule$day == day & schedule$sample_position <= row$sample_position[[1]] & schedule$schedule_type == "relative", , drop = FALSE]
    offsets <- sum(prior$expected_interval_min, na.rm = TRUE)
    if (is.na(awakening)) .carwatch_abort("Cannot apply a relative schedule patch without an awakening time.", "carwatch_value_error")
    return(awakening + offsets * 60)
  }
  if (row$schedule_type[[1]] == "absolute" && !is.na(row$absolute_clock[[1]]) && nzchar(row$absolute_clock[[1]])) return(.day_timestamp(collection_date, row$absolute_clock[[1]], timezone))
  fallback_value <- .fallback_schedule_value(fallback, day, sample, row$sample_position[[1]])
  offset <- suppressWarnings(as.numeric(fallback_value))
  if (!is.na(offset)) {
    if (is.na(awakening)) .carwatch_abort("Cannot apply a relative fallback schedule without an awakening time.", "carwatch_value_error")
    return(awakening + offset * 60)
  }
  .day_timestamp(collection_date, as.character(fallback_value), timezone)
}

.sort_sample_events_by_time <- function(long, schedule, participant, day) {
  positions <- schedule[schedule$day == day, , drop = FALSE]
  positions <- positions[order(positions$sample_position), , drop = FALSE]
  expected_samples <- as.character(positions$scheduled_sample)
  sampling_rows <- long[
    long$participant == participant & long$day == day &
      long$sample %in% expected_samples & long$variable == "sampling_time",
    , drop = FALSE
  ]
  sampling_rows <- sampling_rows[match(expected_samples, sampling_rows$sample), , drop = FALSE]
  times <- suppressWarnings(as.numeric(as.POSIXct(do.call(c, sampling_rows$value))))
  complete <- nrow(sampling_rows) == length(expected_samples) &&
    !anyNA(match(expected_samples, sampling_rows$sample)) && !anyNA(times)
  if (!complete) {
    .carwatch_abort(
      sprintf(
        "Cannot sort scan events by time because the sampling series is incomplete: participant=%s, day=%s, expected_sample_positions=%s, recorded_sample_positions=%s.",
        shQuote(participant), shQuote(day),
        paste(positions$sample_position, collapse = ","),
        paste(positions$sample_position[!is.na(times)], collapse = ",")
      ),
      "carwatch_value_error"
    )
  }
  source_order <- order(times, method = "radix")
  if (any(diff(times[source_order]) <= 0)) {
    .carwatch_abort(
      sprintf("Sorting scan events by time did not create a strictly increasing sampling series: participant=%s, day=%s.", shQuote(participant), shQuote(day)),
      "carwatch_value_error"
    )
  }
  event_variables <- c("sampling_time", "barcode", "recorded_sample", "sampling_time_source", "day_expected", "day_scanned")
  for (variable in event_variables) {
    rows <- which(
      long$participant == participant & long$day == day &
        long$sample %in% expected_samples & long$variable == variable
    )
    target_rows <- rows[match(expected_samples, long$sample[rows])]
    if (anyNA(target_rows)) next
    long$value[target_rows] <- long$value[target_rows[source_order]]
  }
  long
}

.barcode_scan_rows <- function(event_records, participant, day, sample) {
  candidates <- event_records[
    event_records$participant == participant & event_records$day == day &
      event_records$action == "barcode_scanned",
    , drop = FALSE
  ]
  if (!nrow(candidates)) return(candidates)
  expected <- vapply(seq_len(nrow(candidates)), function(index) {
    override <- candidates$scheduled_sample_override[[index]]
    value <- if (!is.na(override) && nzchar(override)) override else as.character(.payload_value(candidates$payload[[index]], "sample_expected", ""))
    identical(value, as.character(sample))
  }, logical(1))
  candidates[expected, , drop = FALSE]
}

.write_scan_event <- function(long, event, participant, day, sample) {
  payload <- event$payload[[1]]
  values <- list(
    sampling_time = event$timestamp[[1]],
    barcode = .payload_value(payload, "barcode_value", NA_character_),
    recorded_sample = if ("recorded_sample_override" %in% names(event) && !is.na(event$recorded_sample_override[[1]]) && nzchar(event$recorded_sample_override[[1]])) event$recorded_sample_override[[1]] else .payload_value(payload, "sample_scanned", NA_character_),
    sampling_time_source = "app",
    day_expected = .payload_value(payload, "day_expected", NA_integer_),
    day_scanned = .payload_value(payload, "day_scanned", NA_integer_)
  )
  for (variable in names(values)) long <- .replace_long_value(long, participant, day, sample, variable, values[[variable]])
  long
}

.apply_duplicate_scan_decision <- function(long, event_records, schedule, item) {
  participant <- item$participant[[1]]
  day <- item$day[[1]]
  sample <- item$sample_id[[1]]
  scans <- .barcode_scan_rows(event_records, participant, day, sample)
  scans <- scans[order(scans$timestamp), , drop = FALSE]
  if (item$user_decision[[1]] != "accept" || nrow(scans) < 2L) return(long)
  if (item$proposed_action[[1]] == "keep_earliest_scan") return(.write_scan_event(long, scans[1, , drop = FALSE], participant, day, sample))
  if (item$proposed_action[[1]] != "reassign_to_recorded_sample" || nrow(scans) != 2L) return(long)
  recorded <- vapply(scans$payload, function(payload) as.character(.payload_value(payload, "sample_scanned", NA_character_)), character(1))
  correct <- which(recorded == sample)
  mismatched <- which(recorded != sample & !is.na(recorded))
  day_samples <- schedule$scheduled_sample[schedule$day == day]
  target <- if (length(mismatched) == 1L) recorded[[mismatched]] else NA_character_
  target_scans <- if (!is.na(target)) .barcode_scan_rows(event_records, participant, day, target) else event_records[0, , drop = FALSE]
  if (length(correct) != 1L || length(mismatched) != 1L || !target %in% day_samples || nrow(target_scans)) return(long)
  long <- .write_scan_event(long, scans[correct, , drop = FALSE], participant, day, sample)
  .write_scan_event(long, scans[mismatched, , drop = FALSE], participant, day, target)
}

.apply_expected_sample_override <- function(long, event_records, schedule, item) {
  participant <- item$participant[[1]]
  day <- item$day[[1]]
  scans <- .barcode_scan_rows(event_records, participant, day, item$sample_id[[1]])
  if (!nrow(scans)) return(long)
  scan <- scans[order(scans$timestamp), , drop = FALSE][1, , drop = FALSE]
  action <- item$user_decision[[1]]
  target <- if (action == "accept" && item$proposed_action[[1]] == "override_expected_sample") {
    as.character(.payload_value(scan$payload[[1]], "sample_scanned", NA_character_))
  } else if (action == "override_expected_sample") {
    item$user_decision_value[[1]]
  } else {
    return(long)
  }
  available <- schedule$scheduled_sample[schedule$day == day]
  if (!nzchar(target) || !target %in% available) {
    .carwatch_abort(
      sprintf("Cannot override an expected sample with an ID outside the active registration: participant=%s, day=%s, requested_sample=%s.", shQuote(participant), shQuote(day), shQuote(target)),
      "carwatch_value_error"
    )
  }
  existing_time <- long$value[[which(long$participant == participant & long$day == day & long$sample == target & long$variable == "sampling_time")]]
  if (!is.na(existing_time)) {
    .carwatch_abort(
      sprintf("Cannot override an expected sample onto an occupied canonical position: participant=%s, day=%s, requested_sample=%s.", shQuote(participant), shQuote(day), shQuote(target)),
      "carwatch_value_error"
    )
  }
  .write_scan_event(long, scan, participant, day, target)
}

.apply_collection_date_decision <- function(long, event_records, schedule, item, timezone) {
  participant <- item$participant[[1]]
  day <- item$day[[1]]
  scans <- event_records[
    event_records$participant == participant & event_records$day == day &
      event_records$action == "barcode_scanned",
    , drop = FALSE
  ]
  scan_dates <- sort(unique(as.Date(scans$timestamp, tz = timezone)))
  if (length(scan_dates) < 2L) return(long)
  action <- item$user_decision[[1]]
  value <- item$user_decision_value[[1]]
  selected_date <- if (action == "accept") {
    scan_dates[[1]]
  } else if (action == "change" && value == "use_earliest_collection_date") {
    scan_dates[[1]]
  } else if (action == "change" && value == "use_latest_collection_date") {
    scan_dates[[length(scan_dates)]]
  } else {
    return(long)
  }
  long <- .replace_long_value(long, participant, day, "day", "date", as.POSIXct(selected_date, tz = timezone))
  positions <- schedule[schedule$day == day & schedule$schedule_type == "absolute", , drop = FALSE]
  for (index in seq_len(nrow(positions))) {
    position <- positions[index, , drop = FALSE]
    timestamp <- .day_timestamp(selected_date, position$absolute_clock[[1]], timezone)
    long <- .replace_long_value(long, participant, day, position$scheduled_sample[[1]], "scheduled_sampling_time", timestamp)
  }
  long
}

.clear_scan_event <- function(long, participant, day, sample) {
  values <- list(
    sampling_time = as.POSIXct(NA), barcode = NA_character_,
    recorded_sample = NA_character_, sampling_time_source = NA_character_,
    day_expected = NA_integer_, day_scanned = NA_integer_
  )
  for (variable in names(values)) long <- .replace_long_value(long, participant, day, sample, variable, values[[variable]])
  long
}

.apply_collection_date_mapping <- function(long, event_records, schedule, item, timezone) {
  if (item$user_decision[[1]] != "change") return(long)
  participant <- item$participant[[1]]
  source_day <- item$day[[1]]
  scans <- event_records[
    event_records$participant == participant & event_records$day == source_day &
      event_records$action == "barcode_scanned",
    , drop = FALSE
  ]
  scan_dates <- sort(unique(as.character(as.Date(scans$timestamp, tz = timezone))))
  value <- item$user_decision_value[[1]]
  if (value %in% c("use_earliest_collection_date", "use_latest_collection_date")) return(long)
  mapping <- tryCatch(jsonlite::fromJSON(value, simplifyVector = FALSE), error = function(error) NULL)
  valid_mapping <- is.list(mapping) && !is.null(names(mapping)) && all(vapply(mapping, function(entry) length(entry) == 1L && is.atomic(entry), logical(1)))
  if (!valid_mapping) .carwatch_abort("Collection-date reassignment must be a JSON date-to-day object.", "carwatch_value_error")
  mapping <- vapply(mapping, as.character, character(1))
  if (!setequal(names(mapping), scan_dates) || anyDuplicated(names(mapping))) .carwatch_abort(sprintf("Collection-date reassignment must define every reported date exactly once: expected_dates=%s, supplied_dates=%s.", paste(scan_dates, collapse = ","), paste(names(mapping), collapse = ",")), "carwatch_value_error")
  if (anyDuplicated(unname(mapping))) .carwatch_abort("Collection-date reassignment requires distinct canonical target days.", "carwatch_value_error")
  available_days <- unique(schedule$day)
  if (length(setdiff(unname(mapping), available_days))) .carwatch_abort("Collection-date reassignment references an unknown canonical day.", "carwatch_value_error")
  source_positions <- schedule[schedule$day == source_day, c("scheduled_sample", "sample_position"), drop = FALSE]
  source_positions <- source_positions[order(source_positions$sample_position), , drop = FALSE]
  for (sample in source_positions$scheduled_sample) long <- .clear_scan_event(long, participant, source_day, sample)
  for (collection_date in names(mapping)) {
    target_day <- mapping[[collection_date]]
    target_positions <- schedule[schedule$day == target_day, c("scheduled_sample", "sample_position", "schedule_type", "absolute_clock"), drop = FALSE]
    target_positions <- target_positions[order(target_positions$sample_position), , drop = FALSE]
    if (nrow(source_positions) != nrow(target_positions)) .carwatch_abort(sprintf("Collection-date reassignment requires compatible source and target sampling schedules: source_day=%s, target_day=%s.", source_day, target_day), "carwatch_value_error")
    date_scans <- scans[as.character(as.Date(scans$timestamp, tz = timezone)) == collection_date, , drop = FALSE]
    for (index in seq_len(nrow(date_scans))) {
      event <- date_scans[index, , drop = FALSE]
      source_sample <- as.character(.payload_value(event$payload[[1]], "sample_expected", ""))
      source_position <- source_positions$sample_position[match(source_sample, source_positions$scheduled_sample)]
      if (is.na(source_position)) .carwatch_abort(sprintf("Collection-date reassignment cannot resolve scheduled sample %s.", shQuote(source_sample)), "carwatch_value_error")
      target_sample <- target_positions$scheduled_sample[match(source_position, target_positions$sample_position)]
      long <- .write_scan_event(long, event, participant, target_day, target_sample)
    }
    long <- .replace_long_value(long, participant, target_day, "day", "date", as.POSIXct(as.Date(collection_date), tz = timezone))
    absolute <- target_positions[target_positions$schedule_type == "absolute", , drop = FALSE]
    for (index in seq_len(nrow(absolute))) {
      position <- absolute[index, , drop = FALSE]
      long <- .replace_long_value(long, participant, target_day, position$scheduled_sample[[1]], "scheduled_sampling_time", .day_timestamp(as.Date(collection_date), position$absolute_clock[[1]], timezone))
    }
  }
  long
}

.apply_conversion_patches <- function(long, report, schedule, manual_diary, sampling_schedule, timezone, event_records = NULL) {
  selected <- .decision_rows(report)
  if (!nrow(selected)) return(long)
  for (index in seq_len(nrow(selected))) {
    item <- selected[index, , drop = FALSE]
    participant <- item$participant[[1]]; day <- item$day[[1]]; sample <- item$sample_id[[1]]
    action <- item$user_decision[[1]]; value <- item$user_decision_value[[1]]
    awakening_patch <- item$code[[1]] == "missing_awakening_time" && (
      (action == "accept" && item$proposed_action[[1]] == "use_manual_diary_awakening_time") ||
        action == "change"
    )
    if (awakening_patch) {
      from_diary <- action == "accept" || value %in% c("use_manual_diary_awakening_time", "manual_diary")
      timestamp <- if (from_diary) .manual_timestamp(manual_diary, participant, day, "awakening_time", timezone) else .parse_local_time(if (nchar(value) == 16L) paste0(value, ":00") else value, timezone, "conversion decision awakening time")
      long <- .replace_long_value(long, participant, day, "day", "awakening_time", timestamp)
      long <- .replace_long_value(long, participant, day, "day", "date", as.POSIXct(as.Date(timestamp), tz = timezone))
      long <- .replace_long_value(long, participant, day, "day", "awakening_type", if (from_diary) "manual_diary" else "manual_override")
    }
    sample_patch <- item$code[[1]] == "missing_scheduled_sample_event" && ((action == "accept" && item$proposed_action[[1]] == "use_manual_diary_sampling_time") || (action == "change" && value %in% c("use_manual_diary_sampling_time", "use_default")))
    if (sample_patch) {
      timestamp <- if (action == "change" && value == "use_default") .scheduled_patch_time(long, schedule, participant, day, sample, timezone, sampling_schedule) else .manual_timestamp(manual_diary, participant, day, paste0("sampling_time_", item$sample_position[[1]]), timezone)
      long <- .replace_long_value(long, participant, day, sample, "sampling_time", timestamp)
      long <- .replace_long_value(long, participant, day, sample, "sampling_time_source", if (action == "change" && value == "use_default") "schedule" else "manual_diary")
    }
    if (item$code[[1]] == "non_increasing_sampling_times" && action == "change") {
      long <- .sort_sample_events_by_time(long, schedule, participant, day)
    }
    if (item$code[[1]] == "duplicate_scheduled_sample_events" && !is.null(event_records)) {
      long <- .apply_duplicate_scan_decision(long, event_records, schedule, item)
    }
    if (item$code[[1]] == "expected_sample_not_in_active_metadata" && !is.null(event_records)) {
      long <- .apply_expected_sample_override(long, event_records, schedule, item)
    }
    if (item$code[[1]] == "multiple_collection_dates" && !is.null(event_records)) {
      long <- .apply_collection_date_decision(long, event_records, schedule, item, timezone)
      long <- .apply_collection_date_mapping(long, event_records, schedule, item, timezone)
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
  additions <- lapply(c("actual_interval_min", "time_deviation_min", "sample_compliant"), function(current_variable) tibble::tibble(participant = samples$participant, day = samples$day, sample = samples$sample, variable = current_variable, value = as.list(samples[[current_variable]])))
  day_values <- lapply(split(samples, interaction(samples$participant, samples$day, drop = TRUE)), function(group) {
    failed <- group$sample[group$sample_compliant %in% FALSE]
    tibble::tibble(participant = group$participant[[1]], day = group$day[[1]], sample = "day", variable = c("expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count", "non_compliant_samples", "day_compliant"), value = list(nrow(group), sum(!is.na(group$recorded_sample)), sum(!is.na(group$sample_compliant)), sum(group$sample_compliant %in% TRUE), if (length(failed)) paste(failed, collapse = ";") else NA_character_, all(group$sample_compliant %in% TRUE) && !anyNA(group$sample_compliant)))
  })
  dplyr::bind_rows(long, dplyr::bind_rows(additions), dplyr::bind_rows(day_values))
}

.new_conversion_issue <- function(code, message, participant = "__cohort__", registration = NA_integer_, registration_day = NA_integer_, day = NA_character_, sample_position = NA_integer_, sample_id = NA_character_, details = list(), proposed_action = "", proposed_action_description = "") {
  tibble::tibble(
    code = code, message = message, participant = participant,
    registration = as.integer(registration), registration_day = as.integer(registration_day),
    day = day, sample_position = as.integer(sample_position), sample_id = sample_id,
    details = list(details), proposed_action = proposed_action,
    proposed_action_description = proposed_action_description
  )
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
#' @param sampling_schedule Optional fallback schedule for `use_default` sample
#'   decisions. Supply a position-indexed vector, a named sample list, or a
#'   named day list containing either form.
#' @param manual_diary Optional normalized manual diary.
#' @param check_compliance Whether to calculate timing compliance.
#' @param compliance_checker Timing tolerance configuration.
#' @return Canonical results, or a results/report list.
#' @export
convert_raw_logs <- function(raw_logs, protocol_manifest = NULL, errors = c("raise", "warn", "error"), create_report = FALSE, issue_decisions = NULL, sampling_schedule = NULL, manual_diary = NULL, check_compliance = TRUE, compliance_checker = new_sampling_compliance_checker()) {
  errors <- match.arg(errors); if (identical(errors, "error")) errors <- "raise"
  .assert_scalar_logical(create_report, "create_report"); .assert_scalar_logical(check_compliance, "check_compliance")
  .require_columns(raw_logs, c("participant", "timestamp", "action", "payload", "source_file"), "Raw logs")
  report_state <- .new_conversion_report(raw_logs, issue_decisions)
  raw_logs <- .deduplicate_raw_events(raw_logs)
  decision_frame <- if (is.null(issue_decisions)) NULL else .normalize_issue_decisions(issue_decisions)
  report_state$decisions <- if (is.null(decision_frame)) report_state$decisions else decision_frame
  schedule <- .registration_schedule(raw_logs, protocol_manifest)
  protocol <- .protocol_from_logs(raw_logs, protocol_manifest)
  participants <- sort(unique(.as_character_id(raw_logs$participant, "Raw-log participant IDs")))
  timezone <- attr(raw_logs$timestamp, "tzone") %||% "Europe/Berlin"
  metadata <- raw_logs[raw_logs$action == "study_metadata", , drop = FALSE]
  issues <- c(
    .protocol_order_issues(raw_logs, protocol_manifest),
    .registration_order_violation_issues(raw_logs, protocol_manifest),
    .manifest_registration_missing_issues(raw_logs, protocol_manifest)
  ); records <- list(); registration_context <- list()
  for (participant in participants) {
    events <- dplyr::arrange(dplyr::filter(raw_logs, .data$participant == .env$participant), .data$timestamp)
    registrations <- lapply(events$payload[events$action == "study_metadata"], .registration_config)
    registrations <- registrations[!vapply(registrations, is.null, logical(1))]
    if (!length(registrations)) {
      # Python records one issue for each scan rather than introducing an
      # additional, non-public "missing registration" issue code.
      scans <- events[events$action == "barcode_scanned", , drop = FALSE]
      for (scan_index in seq_len(nrow(scans))) {
        scan <- scans[scan_index, , drop = FALSE]
        issues[[length(issues) + 1L]] <- .new_conversion_issue(
          "scan_before_registration_metadata",
          sprintf("Barcode scan occurs before usable registration metadata: participant=%s, source_file=%s.", shQuote(participant), shQuote(scan$source_file[[1]])),
          participant,
          details = list(timestamp = scan$timestamp[[1]], source_file = scan$source_file[[1]], payload = scan$payload[[1]]),
          proposed_action = "drop_sample",
          proposed_action_description = "The scan is excluded because no active sampling configuration can resolve it."
        )
      }
      next
    }
    active_registration <- NA_integer_
    awakening_day_counter <- 0L
    current_config_key <- .registration_key(registrations[[1]])
    registration_sources <- list()
    collection_started <- FALSE
    metadata_seen <- FALSE
    active_source_registration <- NA_integer_
    active_registered_at <- as.POSIXct(NA, tz = timezone)
    active_metadata_sources <- character()
    override_target <- .reregistration_override_target(decision_frame, participant, protocol)
    for (i in seq_len(nrow(events))) {
      event <- events[i, , drop = FALSE]
      if (event$action[[1]] == "study_metadata") {
        config <- .registration_config(event$payload[[1]])
        if (!is.null(config)) {
          new_key <- .registration_key(config)
          event_sources <- sort(unique(unlist(strsplit(event$source_file[[1]], ";", fixed = TRUE), use.names = FALSE)))
          if (is.na(active_registered_at)) {
            active_registered_at <- event$timestamp[[1]]
            active_metadata_sources <- event_sources
          }
          override_applied <- FALSE
          if (identical(new_key, current_config_key) && collection_started) {
            issues[[length(issues) + 1L]] <- .new_conversion_issue(
              "possible_reregistration",
              sprintf("Identical study metadata was written after collection started: study_name=%s.", shQuote(config$study_name)),
              participant,
              details = list(
                saliva_ids = config$saliva_ids,
                timestamp = event$timestamp[[1]],
                source_files = event_sources,
                previous_registration_timestamp = active_registered_at,
                previous_metadata_source_files = active_metadata_sources
              ),
              proposed_action = "reuse_registration",
              proposed_action_description = "The event is treated as the existing registration and does not create another canonical day. To use another canonical registration from this metadata event onward, set user_decision='change' and provide {\"registration\": 2} with the canonical number or {\"registration\": \"study-name\"} with the exact study_name."
            )
            active_metadata_sources <- sort(unique(c(active_metadata_sources, event_sources)))
            if (!is.na(override_target)) {
              source_registration <- which(vapply(protocol, function(item) identical(.registration_key(item), current_config_key), logical(1)))
              if (length(source_registration) != 1L || override_target == source_registration[[1]]) .carwatch_abort("Registration override target is already active or cannot resolve the source registration.", "carwatch_value_error")
              if (length(protocol[[source_registration[[1]]]]$saliva_ids) != length(protocol[[override_target]]$saliva_ids) || protocol[[source_registration[[1]]]]$study_days != protocol[[override_target]]$study_days) .carwatch_abort("Registration override requires compatible source and target schedules.", "carwatch_value_error")
              active_registration <- override_target
              active_source_registration <- source_registration[[1]]
              override_applied <- TRUE
            }
          } else if (!identical(new_key, current_config_key)) {
            active_source_registration <- NA_integer_
            active_registered_at <- event$timestamp[[1]]
            active_metadata_sources <- event_sources
          } else {
            active_metadata_sources <- sort(unique(c(active_metadata_sources, event_sources)))
          }
          current_config_key <- new_key
          position <- which(vapply(.protocol_from_logs(raw_logs, protocol_manifest), function(item) identical(.registration_key(item), current_config_key), logical(1)))
          if (length(position) && !override_applied) active_registration <- position[[1]]
          if (!is.na(active_registration)) {
            source_key <- as.character(active_registration)
            registration_sources[[source_key]] <- sort(unique(c(registration_sources[[source_key]] %||% character(), event$source_file[[1]])))
            registration_context[[paste(participant, active_registration, sep = "\r")]] <- list(registered_at = active_registered_at, source_files = active_metadata_sources)
          }
          metadata_seen <- TRUE
        } else issues[[length(issues) + 1L]] <- .new_conversion_issue("invalid_study_metadata", sprintf("Invalid 'study_metadata' payload without usable 'saliva_ids': participant=%s, source_file=%s.", shQuote(participant), shQuote(event$source_file[[1]])), participant, details = list(source_file = event$source_file[[1]], payload = event$payload[[1]]), proposed_action = "ignore_metadata_event", proposed_action_description = "The invalid metadata event is ignored; the preceding usable registration remains active.")
        next
      }
      if (!event$action[[1]] %in% c("barcode_scanned", "spontaneous_awakening", "alarm")) next
      if (event$action[[1]] == "barcode_scanned" && !metadata_seen) {
        issues[[length(issues) + 1L]] <- .new_conversion_issue("scan_before_registration_metadata", sprintf("Barcode scan occurs before usable registration metadata: participant=%s, source_file=%s.", shQuote(participant), shQuote(event$source_file[[1]])), participant, details = list(timestamp = event$timestamp[[1]], source_file = event$source_file[[1]], payload = event$payload[[1]]), proposed_action = "drop_sample", proposed_action_description = "The scan is excluded because no active sampling configuration can resolve it.")
        next
      }
      if (event$action[[1]] == "barcode_scanned") collection_started <- TRUE
      if (event$action[[1]] %in% c("spontaneous_awakening", "alarm")) {
        event_date <- as.Date(event$timestamp[[1]], tz = timezone)
        same_date_scan <- events$action == "barcode_scanned" & as.Date(events$timestamp, tz = timezone) == event_date
        expected_days <- unique(vapply(events$payload[same_date_scan], function(payload) suppressWarnings(as.integer(.payload_value(payload, "day_expected", NA_integer_))), integer(1)))
        expected_days <- expected_days[!is.na(expected_days)]
        if (length(expected_days) == 1L) {
          awakening_day_counter <- expected_days[[1]]
        } else {
          awakening_day_counter <- awakening_day_counter + 1L
        }
        day_row <- dplyr::filter(schedule, .data$registration == .env$active_registration, .data$registration_day == .env$awakening_day_counter)
        day <- if (nrow(day_row)) day_row$day[[1]] else NA_character_
      } else {
        expected_day <- suppressWarnings(as.integer(.payload_value(event$payload[[1]], "day_expected", 1L)))
        active_rows <- schedule[schedule$registration == active_registration, , drop = FALSE]
        if (is.na(expected_day) || !expected_day %in% active_rows$registration_day) {
          study_days <- max(active_rows$registration_day)
          issues[[length(issues) + 1L]] <- .new_conversion_issue("invalid_day_expected", sprintf("Barcode scan defines an invalid expected study day: participant=%s, registration=%s, day_expected=%s, study_days=%s.", shQuote(participant), active_registration, expected_day, study_days), participant, registration = active_registration, registration_day = expected_day, details = list(day_expected = expected_day, study_days = study_days, source_file = event$source_file[[1]]), proposed_action = "drop_sample", proposed_action_description = "The scan is excluded because its expected day is outside the active registration.")
          next
        }
        expected_sample <- as.character(.payload_value(event$payload[[1]], "sample_expected", ""))
        scheduled_sample_override <- .map_registration_sample(protocol, active_source_registration, active_registration, expected_sample)
        recorded_sample_override <- .map_registration_sample(protocol, active_source_registration, active_registration, as.character(.payload_value(event$payload[[1]], "sample_scanned", "")))
        expected_sample <- scheduled_sample_override
        if (!expected_sample %in% active_rows$scheduled_sample) {
          day <- .coerce_event_day(event, schedule, active_registration)
          recorded_sample <- as.character(.payload_value(event$payload[[1]], "sample_scanned", NA_character_))
          occupied <- any(vapply(events$payload[events$action == "barcode_scanned"], function(payload) identical(as.character(.payload_value(payload, "sample_expected", "")), recorded_sample), logical(1)))
          safe_target <- !is.na(day) && recorded_sample %in% active_rows$scheduled_sample && !occupied
          proposed_action <- if (safe_target) "override_expected_sample" else "drop_sample"
          description <- if (safe_target) {
            sprintf("Replace invalid expected sample %s with valid recorded sample %s, preserving the original sampling time, barcode, and recorded sample.", shQuote(expected_sample), shQuote(recorded_sample))
          } else {
            "The scan is excluded because its scheduled sample cannot be resolved to an active sample position. Set user_decision='override_expected_sample' and provide an exact registered ID in user_decision_value to reassign it manually."
          }
          issues[[length(issues) + 1L]] <- .new_conversion_issue(
            "expected_sample_not_in_active_metadata",
            sprintf("Barcode scan expected sample is absent from active registration metadata: participant=%s, sample=%s, study_name=%s, source_file=%s.", shQuote(participant), shQuote(expected_sample), shQuote(protocol[[active_registration]]$study_name), shQuote(event$source_file[[1]])),
            participant,
            registration = active_registration, registration_day = expected_day,
            day = day, sample_id = NA_character_,
            details = list(scheduled_sample = expected_sample, active_saliva_ids = protocol[[active_registration]]$saliva_ids, timestamp = event$timestamp[[1]], source_file = event$source_file[[1]]),
            proposed_action = proposed_action, proposed_action_description = description
          )
          if (!is.na(day)) records[[length(records) + 1L]] <- tibble::tibble(participant = participant, day = day, action = event$action[[1]], timestamp = event$timestamp[[1]], payload = list(event$payload[[1]]), source_file = event$source_file[[1]], registration = active_registration, registration_sources = paste(registration_sources[[as.character(active_registration)]] %||% character(), collapse = ";"), scheduled_sample_override = scheduled_sample_override, recorded_sample_override = recorded_sample_override)
          next
        }
        day <- .coerce_event_day(event, schedule, active_registration)
      }
      if (is.na(day)) next
      records[[length(records) + 1L]] <- tibble::tibble(participant = participant, day = day, action = event$action[[1]], timestamp = event$timestamp[[1]], payload = list(event$payload[[1]]), source_file = event$source_file[[1]], registration = active_registration, registration_sources = paste(registration_sources[[as.character(active_registration)]] %||% character(), collapse = ";"), scheduled_sample_override = if (event$action[[1]] == "barcode_scanned") scheduled_sample_override else NA_character_, recorded_sample_override = if (event$action[[1]] == "barcode_scanned") recorded_sample_override else NA_character_)
    }
  }
  event_records <- if (length(records)) dplyr::bind_rows(records) else tibble::tibble(participant = character(), day = character(), action = character(), timestamp = as.POSIXct(character()), payload = list(), source_file = character(), registration = integer(), registration_sources = character(), scheduled_sample_override = character(), recorded_sample_override = character())
  output <- list()
  for (participant in participants) for (day in unique(schedule$day)) {
    positions <- dplyr::filter(schedule, .data$day == .env$day)
    events <- dplyr::filter(event_records, .data$participant == .env$participant, .data$day == .env$day)
    awakening <- dplyr::filter(events, .data$action %in% c("spontaneous_awakening", "alarm"))
    awakening_time <- if (nrow(awakening)) awakening$timestamp[[1]] else as.POSIXct(NA)
    scan_dates <- sort(unique(as.Date(events$timestamp[events$action == "barcode_scanned"], tz = timezone)))
    collection_date <- if (length(scan_dates)) scan_dates[[1]] else if (nrow(events)) as.Date(events$timestamp[[1]], tz = timezone) else as.Date(NA)
    if (length(scan_dates) > 1L) {
      collection_date <- as.Date(NA)
      date_details <- lapply(scan_dates, function(scan_date) {
        date_events <- events[events$action == "barcode_scanned" & as.Date(events$timestamp, tz = timezone) == scan_date, , drop = FALSE]
        list(
          date = as.character(scan_date),
          scheduled_samples = as.list(unique(vapply(date_events$payload, function(payload) as.character(.payload_value(payload, "sample_expected", "")), character(1)))),
          source_files = as.list(sort(unique(date_events$source_file)))
        )
      })
      scheduled <- vapply(events$payload[events$action == "barcode_scanned"], function(payload) as.character(.payload_value(payload, "sample_expected", "")), character(1))
      repeated <- sort(unique(scheduled[duplicated(scheduled) | duplicated(scheduled, fromLast = TRUE)]))
      first_position <- positions[1, , drop = FALSE]
      spread_type <- if (length(repeated)) "repeated_samples_across_dates" else "unique_samples_spread_across_dates"
      issues[[length(issues) + 1L]] <- .new_conversion_issue(
        "multiple_collection_dates",
        sprintf("A canonical registration day contains barcode scans on multiple collection dates: participant=%s, day=%s, dates=%s.", shQuote(participant), shQuote(day), .python_detail_repr(date_details)),
        participant,
        registration = first_position$registration[[1]], registration_day = first_position$registration_day[[1]], day = day,
        details = list(classification = spread_type, dates = date_details, repeated_scheduled_samples = as.list(repeated)),
        proposed_action = "use_earliest_collection_date",
        proposed_action_description = "Use the earliest retained scan date as the canonical day date. Original sample timestamps remain unchanged. Use user_decision='change' with user_decision_value set to 'use_latest_collection_date' or a complete JSON date-to-day mapping for another resolution."
      )
    }
    observed_expected <- if (nrow(events)) vapply(events$payload[events$action == "barcode_scanned"], function(payload) as.character(.payload_value(payload, "sample_expected", "")), character(1)) else character()
    relative_ids <- positions$scheduled_sample[positions$schedule_type == "relative"]
    missing_relative_ids <- setdiff(relative_ids, observed_expected)
    if (!nrow(awakening) && length(missing_relative_ids)) {
      first_position <- positions[1, , drop = FALSE]
      issues[[length(issues) + 1L]] <- .new_conversion_issue(
        "missing_awakening_time",
        sprintf("A canonical study day has missing samples with relative scheduled times but no awakening time: participant=%s, day=%s, study_name=%s.", shQuote(participant), shQuote(day), shQuote(first_position$study_name[[1]])),
        participant,
        registration = first_position$registration[[1]], registration_day = first_position$registration_day[[1]], day = day,
        details = list(collection_date = collection_date, relative_sample_ids = as.list(missing_relative_ids), source_files = as.list((registration_context[[paste(participant, first_position$registration[[1]], sep = "\r")]] %||% list(source_files = character()))$source_files), timezone = timezone, input_format = "YYYY-MM-DD HH:MM"),
        proposed_action = "use_manual_diary_awakening_time",
        proposed_action_description = "Use the awakening time from the manual diary."
      )
    }
    for (j in seq_len(nrow(positions))) {
      position <- positions[j, ]
      expected_match <- vapply(seq_len(nrow(events)), function(index) {
        override <- events$scheduled_sample_override[[index]]
        value <- if (!is.na(override) && nzchar(override)) override else as.character(.payload_value(events$payload[[index]], "sample_expected", ""))
        identical(value, as.character(position$scheduled_sample))
      }, logical(1))
      scan <- events[events$action == "barcode_scanned" & expected_match, , drop = FALSE]
      duplicate_reassignment <- FALSE
      if (nrow(scan) > 1L) {
        recorded <- vapply(scan$payload, function(payload) as.character(.payload_value(payload, "sample_scanned", NA_character_)), character(1))
        correct <- which(recorded == position$scheduled_sample)
        mismatched <- which(recorded != position$scheduled_sample & !is.na(recorded))
        target <- if (length(mismatched) == 1L) recorded[[mismatched]] else NA_character_
        target_present <- if (!is.na(target)) any(vapply(events$payload[events$action == "barcode_scanned"], function(payload) identical(as.character(.payload_value(payload, "sample_expected", "")), target), logical(1))) else TRUE
        duplicate_reassignment <- nrow(scan) == 2L && length(correct) == 1L && length(mismatched) == 1L && target %in% positions$scheduled_sample && !target_present
        scan <- scan[order(scan$timestamp), , drop = FALSE]
        occurrences <- lapply(seq_len(nrow(scan)), function(index) list(
          timestamp = scan$timestamp[[index]],
          recorded_sample = .payload_value(scan$payload[[index]], "sample_scanned", NA_character_),
          source_file = scan$source_file[[index]],
          barcode = .payload_value(scan$payload[[index]], "barcode_value", NA_character_)
        ))
        reassignment_details <- if (duplicate_reassignment) list(proposed_reassignment = list(from_sample = position$scheduled_sample[[1]], to_sample = target, occurrence = occurrences[[mismatched[[1]]]])) else list()
        details <- c(list(participant = participant, day = day, scheduled_sample = position$scheduled_sample[[1]], occurrences = occurrences, retained_occurrence = occurrences[[1]], other_occurrences = occurrences[-1], supplied_fields = c("sampling_time", "barcode", "recorded_sample")), reassignment_details)
        issues[[length(issues) + 1L]] <- .new_conversion_issue(
          "duplicate_scheduled_sample_events",
          sprintf("A registration contains duplicate events for one scheduled sample: participant=%s, day=%s, sample=%s, occurrences=%s.", shQuote(participant), shQuote(day), shQuote(position$scheduled_sample[[1]]), .python_detail_repr(occurrences)),
          participant,
          registration = position$registration[[1]], registration_day = position$registration_day[[1]], day = day, sample_position = position$sample_position[[1]], sample_id = position$scheduled_sample[[1]], details = details,
          proposed_action = if (duplicate_reassignment) "reassign_to_recorded_sample" else "keep_earliest_scan",
          proposed_action_description = if (duplicate_reassignment) "Keep the correctly assigned occurrence and move the mismatched occurrence, including sampling_time and barcode, to its valid recorded_sample position." else "Use the earliest occurrence for sampling_time, barcode, and recorded_sample."
        )
      }
      payload <- if (nrow(scan) && nrow(scan) == 1L) scan$payload[[1]] else list()
      sampling_time <- if (nrow(scan) == 1L) scan$timestamp[[1]] else as.POSIXct(NA)
      potential_override <- !nrow(scan) && nrow(events) && any(!is.na(events$recorded_sample_override) & events$recorded_sample_override == position$scheduled_sample & !is.na(events$scheduled_sample_override) & events$scheduled_sample_override != position$scheduled_sample, na.rm = TRUE)
      if (!nrow(scan) && !potential_override) issues[[length(issues) + 1L]] <- .new_conversion_issue(
        "missing_scheduled_sample_event",
        sprintf("An expected scheduled sample has no barcode scan: participant=%s, day=%s, sample=%s, study_name=%s.", shQuote(participant), shQuote(day), shQuote(position$scheduled_sample[[1]]), shQuote(position$study_name[[1]])),
        participant,
        registration = position$registration[[1]], registration_day = position$registration_day[[1]], day = day, sample_position = position$sample_position[[1]], sample_id = position$scheduled_sample[[1]],
        details = {
          context <- registration_context[[paste(participant, position$registration[[1]], sep = "\r")]] %||% list(registered_at = as.POSIXct(NA), source_files = character())
          list(registered_at = context$registered_at, source_files = as.list(context$source_files), saliva_times = as.list(as.character(protocol[[position$registration[[1]]]]$saliva_times)), saliva_absolute_times = as.list(as.character(protocol[[position$registration[[1]]]]$saliva_absolute_times)))
        },
        proposed_action = "use_manual_diary_sampling_time",
        proposed_action_description = "Use the sampling time from the manual diary."
      )
      scheduled <- if (position$schedule_type == "absolute") .day_timestamp(collection_date, position$absolute_clock, timezone) else as.POSIXct(NA)
      recorded_sample <- if (nrow(scan) && !is.na(scan$recorded_sample_override[[1]]) && nzchar(scan$recorded_sample_override[[1]])) scan$recorded_sample_override[[1]] else .payload_value(payload, "sample_scanned", NA_character_)
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
      recordings <- lapply(seq_len(nrow(recorded)), function(index) list(sample_position = as.integer(recorded$sample_position[[index]]), scheduled_sample = recorded$sample[[index]], recorded_sample = recorded$recorded_sample[[index]], sampling_time = recorded$sampling_time[[index]], barcode = recorded$barcode[[index]], source_file = NA_character_))
      invalid <- which(diff(as.numeric(recorded$sampling_time)) <= 0) + 1L
      transitions <- lapply(invalid, function(index) list(from_sample_position = as.integer(recorded$sample_position[[index - 1L]]), from_sample = recorded$sample[[index - 1L]], from_time = recorded$sampling_time[[index - 1L]], to_sample_position = as.integer(recorded$sample_position[[index]]), to_sample = recorded$sample[[index]], to_time = recorded$sampling_time[[index]]))
      expected_positions <- schedule$sample_position[schedule$day == recorded$day[[1]]]
      complete <- identical(as.integer(recorded$sample_position), as.integer(expected_positions))
      schedule_context <- schedule[schedule$day == recorded$day[[1]] & schedule$sample_position == min(recorded$sample_position), , drop = FALSE]
      recording_text <- vapply(recordings, function(entry) sprintf("position %s (%s) at %s", entry$sample_position, shQuote(entry$scheduled_sample), .json_safe(entry$sampling_time)), character(1))
      transition_text <- vapply(transitions, function(entry) sprintf("position %s at %s -> position %s at %s", entry$from_sample_position, .json_safe(entry$from_time), entry$to_sample_position, .json_safe(entry$to_time)), character(1))
      note <- if (complete) "Chronological sample reassignment is available for this complete series." else sprintf("Chronological sample reassignment is unavailable because the series is incomplete; expected positions are %s.", .canonical_json(as.integer(expected_positions)))
      issues[[length(issues) + 1L]] <- .new_conversion_issue(
        "non_increasing_sampling_times",
        sprintf("Sampling times do not follow registered sample-position order: participant=%s, day=%s. Recordings: %s. Non-increasing transitions: %s. %s With errors='warn', the original scan assignments and timestamps are retained unchanged.", shQuote(recorded$participant[[1]]), shQuote(recorded$day[[1]]), paste(recording_text, collapse = "; "), paste(transition_text, collapse = "; "), note),
        recorded$participant[[1]], registration = schedule_context$registration[[1]], registration_day = schedule_context$registration_day[[1]], day = recorded$day[[1]],
        details = list(recordings = recordings, non_increasing_transitions = transitions, warning_treatment = "Keep the original scan assignments and timestamps unchanged.", change_value = "sort_samples_by_time", sort_samples_by_time_available = complete, expected_sample_positions = as.integer(expected_positions), recorded_sample_positions = as.integer(recorded$sample_position)),
        proposed_action = "drop_day", proposed_action_description = "Drop this study day because its sampling times do not follow sample-position order."
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
  if (!"details" %in% names(issue_frame)) issue_frame$details <- rep(list(list()), nrow(issue_frame))
  # Reissue the discovered anomalies through the stable report model. The
  # default `accept` cells are advisory; only rows supplied through
  # `issue_decisions` count as executable decisions.
  if (nrow(issue_frame)) for (row in seq_len(nrow(issue_frame))) {
    item <- issue_frame[row, , drop = FALSE]
    context <- schedule[schedule$day == item$day[[1]] & schedule$scheduled_sample == item$sample_id[[1]], , drop = FALSE]
    details <- item$details[[1]] %||% list()
    message <- if ("message" %in% names(item) && nzchar(item$message[[1]])) item$message[[1]] else .protocol_issue_message(item)
    description <- if ("proposed_action_description" %in% names(item) && nzchar(item$proposed_action_description[[1]])) item$proposed_action_description[[1]] else .issue_description(item$code[[1]])
    registration <- if ("registration" %in% names(item) && !is.na(item$registration[[1]])) item$registration[[1]] else if (nrow(context)) context$registration[[1]] else NA_integer_
    registration_day <- if ("registration_day" %in% names(item) && !is.na(item$registration_day[[1]])) item$registration_day[[1]] else if (nrow(context)) context$registration_day[[1]] else NA_integer_
    sample_position <- if ("sample_position" %in% names(item) && !is.na(item$sample_position[[1]])) item$sample_position[[1]] else if (nrow(context)) context$sample_position[[1]] else NA_integer_
    added <- .report_add_issue(report_state, code = item$code[[1]], message = message, participant = item$participant[[1]], day = item$day[[1]], sample_id = item$sample_id[[1]], registration = registration, registration_day = registration_day, sample_position = sample_position, details = details, proposed_action = item$proposed_action[[1]], proposed_action_description = description)
    report_state <- added$report
  }
  if (!is.null(decision_frame) && nrow(report_state$issues)) {
    long <- .apply_conversion_patches(long, report_state, schedule, manual_diary, sampling_schedule, timezone, event_records)
    if (check_compliance) long <- .append_conversion_compliance(long, participants, compliance_checker)
    results <- .from_long_results(long, participants)
    selected <- report_state$issues[report_state$issues$resolution_status == "resolved", c("participant", "day", "sample_id", "user_decision", "proposed_action"), drop = FALSE]
    if (nrow(selected)) {
      selected$effective_action <- ifelse(selected$user_decision == "accept" & selected$proposed_action %in% c("drop_participant", "drop_day", "drop_sample"), selected$proposed_action, selected$user_decision)
      remove_participants <- selected$participant[selected$effective_action == "drop_participant"]
      remove_days <- selected[selected$effective_action == "drop_day", c("participant", "day"), drop = FALSE]
      remove_samples <- selected[selected$effective_action == "drop_sample", c("participant", "day", "sample_id"), drop = FALSE]
      keep <- !long$participant %in% remove_participants
      long <- long[keep, , drop = FALSE]
      if (nrow(remove_days)) {
        day_rows <- paste(long$participant, long$day, sep = "\r") %in% paste(remove_days$participant, remove_days$day, sep = "\r")
        long <- .blank_long_rows(long, which(day_rows))
      }
      if (nrow(remove_samples)) {
        sample_rows <- long$sample != "day" & paste(long$participant, long$day, long$sample, sep = "\r") %in% paste(remove_samples$participant, remove_samples$day, remove_samples$sample_id, sep = "\r")
        long <- .blank_long_rows(long, which(sample_rows))
      }
      results <- .from_long_results(long, setdiff(participants, remove_participants))
    }
  }
  finalized_report <- .report_finalize(report_state, results, schedule, sum(!is.na(as_sample_events(results)$sampling_time)))
  unresolved <- finalized_report$issues$resolution_status == "unresolved"
  if (nrow(finalized_report$issues) && any(unresolved)) {
    if (errors == "raise") .carwatch_abort(finalized_report$issues$message[which(unresolved)[[1]]], "carwatch_schema_error")
    for (message in finalized_report$issues$message[unresolved]) rlang::warn(message, class = "carwatch_conversion_warning")
  }
  if (create_report) return(list(results = results, report = finalized_report))
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
