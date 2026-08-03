.conversion_issue_codes <- c(
  "invalid_study_metadata", "scan_before_registration_metadata",
  "ambiguous_protocol_order", "cyclic_protocol_order", "registration_order_violation",
  "manifest_registration_missing", "invalid_day_expected", "multiple_collection_dates",
  "possible_reregistration", "missing_awakening_time",
  "expected_sample_not_in_active_metadata", "duplicate_scheduled_sample_events",
  "non_increasing_sampling_times", "missing_scheduled_sample_event"
)
.conversion_decisions <- c("accept", "keep", "drop_sample", "drop_day", "drop_participant", "override_expected_sample", "change")

.empty_conversion_issues <- function() tibble::tibble(
  participant = character(), day = character(), sample_id = character(), code = character(),
  registration = integer(), registration_day = integer(), sample_position = integer(),
  issue_id = character(), message = character(), details = character(),
  proposed_action = character(), proposed_action_description = character(),
  resolution_status = character(), user_decision = character(), user_decision_value = character()
)

.json_safe <- function(value) {
  if (inherits(value, "POSIXt")) return(format(value, "%Y-%m-%dT%H:%M:%S%z"))
  if (is.list(value)) {
    if (!is.null(names(value))) return(lapply(value[order(names(value))], .json_safe))
    return(lapply(value, .json_safe))
  }
  if (length(value) == 1L && is.na(value)) return(NULL)
  value
}

.canonical_json <- function(value) jsonlite::toJSON(.json_safe(value), auto_unbox = TRUE, null = "null", digits = NA)

.stable_issue_id <- function(participant, registration = NA_integer_, registration_day = NA_integer_, day = NA_character_, sample_position = NA_integer_, sample_id = NA_character_, code, details = list()) {
  details_json <- as.character(.canonical_json(details))
  identity <- list(participant = participant %||% "__cohort__", registration = registration, registration_day = registration_day, day = day, sample_position = sample_position, sample_id = sample_id, code = code, details = details_json)
  substr(digest::digest(.canonical_json(identity), algo = "sha256", serialize = FALSE), 1L, 16L)
}

.normalize_issue_decisions <- function(issues) {
  if (!inherits(issues, "data.frame")) .carwatch_abort("`issue_decisions` must be a data frame.", "carwatch_type_error")
  frame <- tibble::as_tibble(issues)
  .require_columns(frame, c("participant", "issue_id", "user_decision"), "Issue decisions")
  if (!"code" %in% names(frame)) frame$code <- ""
  if (!"user_decision_value" %in% names(frame)) frame$user_decision_value <- ""
  for (column in c("participant", "issue_id", "user_decision", "user_decision_value", "code")) frame[[column]] <- trimws(as.character(frame[[column]] %||% ""))
  frame$user_decision <- tolower(frame$user_decision)
  if (any(!nzchar(frame$issue_id)) || anyDuplicated(frame$issue_id)) .carwatch_abort("Issue decisions require unique, non-empty `issue_id` values.", "carwatch_schema_error")
  invalid <- setdiff(unique(frame$user_decision), c("", .conversion_decisions))
  if (length(invalid)) .carwatch_abort(sprintf("Issue decisions contain unsupported values: %s.", paste(invalid, collapse = ", ")), "carwatch_value_error")
  parameterized <- frame$user_decision %in% c("override_expected_sample", "change") & !nzchar(frame$user_decision_value)
  if (any(parameterized)) .carwatch_abort("Parameterized issue decisions require `user_decision_value`.", "carwatch_value_error")
  legacy <- frame$user_decision_value %in% c("patch_sampling_time", "patch_awakening_time")
  if (any(legacy)) .carwatch_abort("Issue decisions use obsolete manual-diary action names. Regenerate the conversion report.", "carwatch_value_error")
  change <- frame$user_decision == "change"
  unsupported_change <- change & !frame$code %in% c(
    "multiple_collection_dates", "possible_reregistration",
    "non_increasing_sampling_times", "missing_awakening_time",
    "missing_scheduled_sample_event"
  )
  if (any(unsupported_change)) .carwatch_abort("change is not supported for one or more issue codes.", "carwatch_value_error")
  invalid_default <- change & frame$user_decision_value == "use_default" & frame$code != "missing_scheduled_sample_event"
  if (any(invalid_default)) .carwatch_abort("use_default is only valid for missing scheduled sample events.", "carwatch_value_error")
  invalid_sort <- change & frame$code == "non_increasing_sampling_times" & frame$user_decision_value != "sort_samples_by_time"
  if (any(invalid_sort)) .carwatch_abort("non-increasing sampling times only support `change` with `sort_samples_by_time`.", "carwatch_value_error")
  frame
}

.decision_rows <- function(report) {
  issues <- report$issues
  if (!nrow(issues)) return(issues)
  issues[issues$resolution_status == "resolved", , drop = FALSE]
}

.decision_value <- function(report, code, participant, day = NA_character_, sample_id = NA_character_) {
  rows <- .decision_rows(report)
  if (!nrow(rows)) return(list(action = "", value = ""))
  rows <- rows[rows$code == code & rows$participant == participant, , drop = FALSE]
  if (!is.na(day)) rows <- rows[is.na(rows$day) | rows$day == day, , drop = FALSE]
  if (!is.na(sample_id)) rows <- rows[is.na(rows$sample_id) | rows$sample_id == sample_id, , drop = FALSE]
  if (!nrow(rows)) return(list(action = "", value = ""))
  list(action = rows$user_decision[[1]], value = rows$user_decision_value[[1]])
}

.decision_supersedes <- function(upstream, stale) {
  if (upstream$user_decision %in% c("drop_participant")) return(upstream$participant == stale$participant)
  if (upstream$user_decision == "drop_day") return(upstream$participant == stale$participant && identical(upstream$day, stale$day))
  if (upstream$user_decision == "drop_sample") return(upstream$participant == stale$participant && identical(upstream$day, stale$day) && identical(upstream$sample_id, stale$sample_id))
  if (upstream$code == "possible_reregistration" && upstream$user_decision == "change") return(stale$participant == upstream$participant)
  if (upstream$code == "multiple_collection_dates" && upstream$user_decision == "change") return(stale$participant == upstream$participant)
  FALSE
}

.new_conversion_report <- function(raw_logs, issue_decisions = NULL) {
  decisions <- if (is.null(issue_decisions)) tibble::tibble(issue_id = character(), user_decision = character(), user_decision_value = character()) else .normalize_issue_decisions(issue_decisions)
  structure(list(
    input_event_count = nrow(raw_logs), input_participant_count = dplyr::n_distinct(raw_logs$participant), input_source_file_count = dplyr::n_distinct(raw_logs$source_file),
    decisions = decisions, encountered = character(), occurrences = integer(), issues = .empty_conversion_issues()
  ), class = "carwatch_conversion_report")
}

.report_add_issue <- function(report, code, message, participant = "__cohort__", registration = NA_integer_, registration_day = NA_integer_, day = NA_character_, sample_position = NA_integer_, sample_id = NA_character_, details = list(), proposed_action = "", proposed_action_description = "", default_user_decision = "accept") {
  base <- .stable_issue_id(participant, registration, registration_day, day, sample_position, sample_id, code, details)
  count <- if (base %in% names(report$occurrences)) report$occurrences[[base]] + 1L else 1L
  report$occurrences[[base]] <- count
  issue_id <- paste0(base, "-", count)
  selected <- report$decisions[report$decisions$issue_id == issue_id, , drop = FALSE]
  explicit <- nrow(selected) == 1L && nzchar(selected$user_decision[[1]])
  decision <- if (explicit) selected$user_decision[[1]] else ""
  if (explicit) report$encountered <- c(report$encountered, issue_id)
  status <- if (explicit) "resolved" else "unresolved"
  report$issues <- dplyr::bind_rows(report$issues, tibble::tibble(
    participant = participant, day = day, sample_id = sample_id, code = code,
    registration = as.integer(registration), registration_day = as.integer(registration_day), sample_position = as.integer(sample_position),
    issue_id = issue_id, message = gsub("\\s+", " ", trimws(message)), details = as.character(.canonical_json(details)), proposed_action = proposed_action,
    proposed_action_description = proposed_action_description, resolution_status = status,
    user_decision = if (explicit) decision else default_user_decision,
    user_decision_value = if (explicit) selected$user_decision_value[[1]] else ""
  ))
  list(report = report, decision = decision, value = if (explicit) selected$user_decision_value[[1]] else "", issue_id = issue_id)
}

.report_finalize <- function(report, results, schedule, recorded_sample_event_count = 0L) {
  stale <- setdiff(report$decisions$issue_id[report$decisions$user_decision != ""], report$encountered)
  # Scope drops legitimately make downstream issue IDs disappear. Other stale IDs
  # remain an explicit audit error rather than silently changing a conversion.
  if (length(stale)) {
    supplied <- report$decisions[match(stale, report$decisions$issue_id), , drop = FALSE]
    encountered <- report$issues[report$issues$issue_id %in% report$encountered, , drop = FALSE]
    superseded <- vapply(seq_len(nrow(supplied)), function(index) any(vapply(seq_len(nrow(encountered)), function(other) .decision_supersedes(encountered[other, , drop = FALSE], supplied[index, , drop = FALSE]), logical(1))), logical(1))
    if (any(!superseded)) .carwatch_abort(sprintf("Issue decisions contain IDs not reproduced from the current raw logs: %s.", paste(stale[!superseded], collapse = ", ")), "carwatch_value_error")
  }
  issues <- report$issues
  if (nrow(issues)) {
    priority <- match(issues$code, .conversion_issue_codes, nomatch = length(.conversion_issue_codes) + 1L)
    issues <- issues[order(issues$participant != "__cohort__", tolower(issues$participant), issues$day, issues$sample_id, priority, issues$issue_id, na.last = TRUE), , drop = FALSE]
  }
  list(summary = list(input_event_count = report$input_event_count, input_participant_count = report$input_participant_count, input_source_file_count = report$input_source_file_count, output_participant_count = nrow(results), canonical_day_count = dplyr::n_distinct(schedule$day), expected_sample_position_count = nrow(schedule), recorded_sample_event_count = as.integer(recorded_sample_event_count), issue_count = nrow(issues)), issues = issues)
}

.decision_for_scope <- function(decisions, participant, day = NA_character_, sample_id = NA_character_) {
  if (!nrow(decisions)) return("")
  rows <- decisions[decisions$participant == participant & decisions$user_decision %in% c("drop_participant", "drop_day", "drop_sample"), , drop = FALSE]
  if (any(rows$user_decision == "drop_participant")) return("drop_participant")
  if (!is.na(day) && any(rows$user_decision == "drop_day" & rows$day == day, na.rm = TRUE)) return("drop_day")
  if (!is.na(day) && !is.na(sample_id) && any(rows$user_decision == "drop_sample" & rows$day == day & rows$sample_id == sample_id, na.rm = TRUE)) return("drop_sample")
  ""
}
