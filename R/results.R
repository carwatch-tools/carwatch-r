#' Construct canonical CARWatch results
#'
#' A `carwatch_results` object is an R-native representation of the Python
#' package's participant-indexed, three-level wide table. Internally it is a
#' tibble with a `participant` column and safe column names. The `column_spec`
#' attribute maps every value column reversibly to `day`, `sample`, and
#' `variable`; opaque sample IDs are never encoded into the internal names.
#'
#' @param data Tibble containing `participant` and one column per result value.
#' @param column_spec Tibble with `name`, `day`, `sample`, and `variable`.
#' @param display_only Whether required provenance has intentionally been removed.
#' @export
new_carwatch_results <- function(data, column_spec, display_only = FALSE) {
  if (!inherits(data, "data.frame")) {
    .carwatch_abort("`data` must be a data frame.", "carwatch_type_error")
  }
  data <- tibble::as_tibble(data)
  .require_columns(data, "participant", "Study Results")
  data$participant <- .as_character_id(data$participant, "Participant IDs")
  if (anyDuplicated(data$participant)) {
    .carwatch_abort("Study Results contain duplicate participant IDs.", "carwatch_schema_error")
  }
  column_spec <- tibble::as_tibble(column_spec)
  .require_columns(column_spec, c("name", "day", "sample", "variable"), "Column specification")
  if (anyDuplicated(column_spec$name) || anyDuplicated(column_spec[c("day", "sample", "variable")])) {
    .carwatch_abort("Study Results columns must be unique.", "carwatch_schema_error")
  }
  if (!setequal(column_spec$name, setdiff(names(data), "participant"))) {
    .carwatch_abort("Column specification does not match Study Results value columns.", "carwatch_schema_error")
  }
  if (any(!grepl("^D[1-9][0-9]*$", column_spec$day))) {
    .carwatch_abort("Study Results contain invalid canonical day identifiers.", "carwatch_schema_error")
  }
  .assert_scalar_logical(display_only, "display_only")
  attr(data, "column_spec") <- column_spec
  attr(data, "carwatch_display_only") <- display_only
  class(data) <- c("carwatch_results", class(data))
  data
}

#' @export
print.carwatch_results <- function(x, ...) {
  state <- if (isTRUE(attr(x, "carwatch_display_only"))) "display-only" else "complete"
  cat(sprintf("<carwatch_results: %s; %d participants; %d fields>\n", state, nrow(x), nrow(attr(x, "column_spec"))))
  NextMethod()
}

.results_long <- function(data) {
  .require_complete_results(data)
  spec <- attr(data, "column_spec")
  rows <- lapply(seq_len(nrow(spec)), function(i) {
    key <- spec[i, ]
    tibble::tibble(
      participant = data$participant,
      day = key$day,
      sample = key$sample,
      variable = key$variable,
      value = as.list(data[[key$name]])
    )
  })
  dplyr::bind_rows(rows)
}

.day_variable_order <- c("date", "awakening_time", "awakening_type", "mismatch_summary", "registration", "study_name", "registration_day", "registration_sources", "possible_reregistration", "day_compliant", "expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count", "non_compliant_samples")
.sample_variable_order <- c("sampling_time", "barcode", "recorded_sample", "sampling_time_source", "sample_position", "day_expected", "day_scanned", "schedule_type", "expected_interval_min", "actual_interval_min", "scheduled_sampling_time", "time_deviation_min", "sample_compliant", "lab_value_available", "mismatch_corrected", "recorded_sample_in_schedule", "sampling_event_recorded")

.column_spec_order <- function(keys) {
  day_number <- suppressWarnings(as.integer(sub("^D", "", keys$day)))
  variable_order <- ifelse(keys$sample == "day", match(keys$variable, .day_variable_order), match(keys$variable, .sample_variable_order))
  variable_order[is.na(variable_order)] <- max(length(.day_variable_order), length(.sample_variable_order)) + match(keys$variable[is.na(variable_order)], sort(unique(keys$variable[is.na(variable_order)])))
  order(day_number, keys$sample != "day", keys$sample, variable_order, keys$variable, na.last = TRUE)
}

.from_long_results <- function(long, participants = NULL, display_only = FALSE) {
  .require_columns(long, c("participant", "day", "sample", "variable", "value"), "Long Study Results")
  long <- tibble::as_tibble(long)
  if (is.null(participants)) participants <- unique(long$participant)
  keys <- dplyr::distinct(long, .data$day, .data$sample, .data$variable)
  keys <- keys[.column_spec_order(keys), , drop = FALSE]
  keys$name <- paste0("v", seq_len(nrow(keys)))
  keys <- dplyr::select(keys, "name", "day", "sample", "variable")
  wide <- tibble::tibble(participant = as.character(participants))
  for (i in seq_len(nrow(keys))) {
    key <- keys[i, ]
    current <- dplyr::filter(long, .data$day == key$day, .data$sample == key$sample, .data$variable == key$variable)
    if (anyDuplicated(current$participant)) .carwatch_abort("Long Study Results contain duplicate participant/day/sample/variable values.", "carwatch_schema_error")
    positions <- match(wide$participant, current$participant)
    template <- vctrs::vec_c(!!!current$value)
    value <- if (length(template) && inherits(template, "POSIXt")) as.POSIXct(rep(NA, nrow(wide)), origin = "1970-01-01", tz = attr(template, "tzone") %||% "Europe/Berlin") else rep(NA, nrow(wide))
    if (length(template)) value[!is.na(positions)] <- template[positions[!is.na(positions)]]
    wide[[key$name]] <- value
  }
  new_carwatch_results(wide, keys, display_only = display_only)
}

#' Extract the explicit sample-level table
#'
#' @param data Complete canonical Study Results.
#' @return A tibble with one row per participant, day, and scheduled sample.
#' @export
as_sample_events <- function(data) {
  long <- .results_long(data)
  sample_data <- .values_wide(dplyr::filter(long, .data$sample != "day"), c("participant", "day", "sample"))
  days <- as_study_days(data)
  day_context <- setdiff(names(days), c("participant", "day", "date", "mismatch_summary", names(sample_data)))
  if (length(day_context)) {
    sample_data <- dplyr::left_join(sample_data, dplyr::select(days, dplyr::all_of(c("participant", "day", day_context))), by = c("participant", "day"))
  }
  if (all(c("sampling_time", "awakening_time") %in% names(sample_data))) {
    sample_data$time_min <- as.numeric(difftime(sample_data$sampling_time, sample_data$awakening_time, units = "mins"))
  }
  if (!"sampling_event_recorded" %in% names(sample_data)) {
    source_recorded <- if ("sampling_time_source" %in% names(sample_data)) sample_data$sampling_time_source == "app" else rep(FALSE, nrow(sample_data))
    source_known <- if ("sampling_time_source" %in% names(sample_data)) !is.na(sample_data$sampling_time_source) else rep(FALSE, nrow(sample_data))
    evidence <- rep(FALSE, nrow(sample_data))
    for (field in intersect(c("sampling_time", "recorded_sample", "barcode"), names(sample_data))) evidence <- evidence | !is.na(sample_data[[field]])
    sample_data$sampling_event_recorded <- ifelse(source_known, source_recorded, evidence)
  }
  if (!"sample_mismatch" %in% names(sample_data) && "recorded_sample" %in% names(sample_data)) {
    sample_data$sample_mismatch <- ifelse(is.na(sample_data$recorded_sample), NA, sample_data$recorded_sample != sample_data$sample)
  }
  if (!"sample_position" %in% names(sample_data)) sample_data$sample_position <- match(sample_data$sample, unique(sample_data$sample))
  dplyr::arrange(sample_data, .data$participant, .data$day, .data$sample_position, .data$sample)
}

#' Extract the explicit day-level table
#'
#' @param data Complete canonical Study Results.
#' @return A tibble with one row per participant and canonical day.
#' @export
as_study_days <- function(data) {
  .values_wide(dplyr::filter(.results_long(data), .data$sample == "day"), c("participant", "day")) |>
    dplyr::arrange(.data$participant, .data$day)
}

.values_wide <- function(long, keys) {
  result <- dplyr::distinct(long, dplyr::across(dplyr::all_of(keys)))
  variables <- unique(long$variable)
  for (current_variable in variables) {
    current <- dplyr::filter(long, .data$variable == .env$current_variable)
    if (anyDuplicated(current[keys])) .carwatch_abort("Study Results contain duplicate values for one participant/day/sample/variable.", "carwatch_schema_error")
    current_value <- vctrs::vec_c(!!!current$value)
    current_key <- do.call(paste, c(current[keys], sep = "\r"))
    result_key <- do.call(paste, c(result[keys], sep = "\r"))
    positions <- match(result_key, current_key)
    output <- if (inherits(current_value, "POSIXt")) as.POSIXct(rep(NA, nrow(result)), origin = "1970-01-01", tz = attr(current_value, "tzone") %||% "Europe/Berlin") else vctrs::vec_init(current_value, nrow(result))
    output[!is.na(positions)] <- current_value[positions[!is.na(positions)]]
    result[[current_variable]] <- output
  }
  result
}
