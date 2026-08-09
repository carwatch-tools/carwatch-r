#' Configure CARWatch sampling-compliance tolerances
#'
#' @param awakening_delay_tolerance_min Tolerance for the first relative sample.
#' @param sampling_delay_tolerance_min Tolerance for later relative samples.
#' @param absolute_time_tolerance_min Tolerance for fixed-time samples.
#' @param check_absolute_times Whether fixed-time samples are evaluated.
#' @export
new_sampling_compliance_checker <- function(awakening_delay_tolerance_min = 5, sampling_delay_tolerance_min = 5, absolute_time_tolerance_min = 15, check_absolute_times = TRUE) {
  values <- c(awakening_delay_tolerance_min, sampling_delay_tolerance_min, absolute_time_tolerance_min)
  if (any(!is.finite(values)) || any(values < 0)) .carwatch_abort("Sampling-compliance tolerances must be finite, non-negative numbers.", "carwatch_value_error")
  .assert_scalar_logical(check_absolute_times, "check_absolute_times")
  structure(list(awakening_delay_tolerance_min = awakening_delay_tolerance_min, sampling_delay_tolerance_min = sampling_delay_tolerance_min, absolute_time_tolerance_min = absolute_time_tolerance_min, check_absolute_times = check_absolute_times), class = "carwatch_compliance_checker")
}

.evaluate_sampling_compliance <- function(samples, checker = new_sampling_compliance_checker()) {
  required <- c("participant", "day", "scheduled_sample", "sample_position", "sampling_time", "awakening_time", "schedule_type", "expected_interval_min", "scheduled_sampling_time")
  .require_columns(samples, required, "Sampling compliance input")
  samples <- dplyr::arrange(samples, .data$participant, .data$day, .data$sample_position)
  samples$actual_interval_min <- NA_real_; samples$time_deviation_min <- NA_real_; samples$sample_compliant <- NA
  groups <- split(seq_len(nrow(samples)), interaction(samples$participant, samples$day, drop = TRUE, lex.order = TRUE))
  for (index in groups) {
    previous_relative <- as.POSIXct(NA); relative_number <- 0L
    for (i in index) {
      schedule_type <- samples$schedule_type[[i]]; sampling <- samples$sampling_time[[i]]
      if (identical(schedule_type, "relative")) relative_number <- relative_number + 1L
      if (is.na(sampling)) { if (!identical(schedule_type, "absolute") || checker$check_absolute_times) samples$sample_compliant[[i]] <- FALSE; if (identical(schedule_type, "relative")) previous_relative <- as.POSIXct(NA); next }
      if (identical(schedule_type, "relative")) {
        reference <- if (relative_number == 1L) samples$awakening_time[[i]] else previous_relative
        previous_relative <- sampling
        if (is.na(reference)) { samples$sample_compliant[[i]] <- FALSE; next }
        actual <- as.numeric(difftime(sampling, reference, units = "mins")); deviation <- actual - as.numeric(samples$expected_interval_min[[i]])
        samples$actual_interval_min[[i]] <- actual; samples$time_deviation_min[[i]] <- deviation; samples$sample_compliant[[i]] <- abs(deviation) <= if (relative_number == 1L) checker$awakening_delay_tolerance_min else checker$sampling_delay_tolerance_min
      } else if (identical(schedule_type, "absolute")) {
        if (!checker$check_absolute_times) next
        scheduled <- samples$scheduled_sampling_time[[i]]
        if (is.na(scheduled)) { samples$sample_compliant[[i]] <- FALSE; next }
        deviation <- as.numeric(difftime(sampling, scheduled, units = "mins")); samples$time_deviation_min[[i]] <- deviation; samples$sample_compliant[[i]] <- abs(deviation) <= checker$absolute_time_tolerance_min
      } else .carwatch_abort(sprintf("Unsupported sampling schedule type: %s.", schedule_type), "carwatch_schema_error")
    }
  }
  samples
}

.as_samples <- function(data) if (inherits(data, "carwatch_results")) as_sample_events(data) else tibble::as_tibble(data)

#' Identify tube and day mismatches
#' @param data Canonical results or sample events.
#' @return Rows with sampling anomalies.
#' @export
find_sampling_anomalies <- function(data) {
  samples <- .as_samples(data)
  .require_columns(samples, c("day_expected", "day_scanned"), "Sample data")
  # Canonical conversion retains the scheduled ID as `sample` and the app
  # observation as `recorded_sample`. Derive the inspection flag at this
  # boundary instead of requiring a redundant storage variable.
  if (!"sample_mismatch" %in% names(samples)) {
    .require_columns(samples, c("sample", "recorded_sample"), "Sample data")
    samples$sample_mismatch <- !is.na(samples$recorded_sample) &
      samples$recorded_sample != samples$sample
  }
  samples$day_mismatch <- !is.na(samples$day_expected) & !is.na(samples$day_scanned) & samples$day_expected != samples$day_scanned
  dplyr::filter(samples, .data$sample_mismatch %in% TRUE | .data$day_mismatch)
}

#' Identify non-compliant samples
#' @param data Canonical results or sample events.
#' @return Rows with failed compliance.
#' @export
find_non_compliant_samples <- function(data) {
  samples <- .as_samples(data)
  .require_columns(samples, "sample_compliant", "Sample data")
  dplyr::filter(samples, .data$sample_compliant %in% FALSE)
}

#' Summarize sampling compliance
#' @param data Canonical results or a sample-event tibble.
#' @param group_by Grouping column(s), or `NULL` for a cohort total.
#' @export
summarize_compliance <- function(data, group_by = "sample_position") {
  samples <- .as_samples(data); .require_columns(samples, "sample_compliant", "Sample data")
  if (is.null(group_by)) {
    samples$.all <- "all"; group_by <- ".all"
  }
  .require_columns(samples, group_by, "Sample data")
  dplyr::summarise(dplyr::group_by(samples, dplyr::across(dplyr::all_of(group_by)), .drop = FALSE), total_samples = dplyr::n(), assessed_samples = sum(!is.na(.data$sample_compliant)), compliant_samples = sum(.data$sample_compliant %in% TRUE), non_compliant_samples = sum(.data$sample_compliant %in% FALSE), unassessed_samples = sum(is.na(.data$sample_compliant)), missing_sampling_time = if ("sampling_time" %in% names(samples)) sum(is.na(.data$sampling_time)) else NA_integer_, compliance_rate = ifelse(.data$assessed_samples == 0, NA_real_, .data$compliant_samples / .data$assessed_samples), .groups = "drop")
}

.clear_result_observations <- function(data, remove, drop_entire_day) {
  spec <- attr(data, "column_spec"); samples <- as_sample_events(data)
  targets <- samples[remove, c("participant", "day", "sample"), drop = FALSE]
  result <- data
  protected <- c("sample_position", "schedule_type", "expected_interval_min", "scheduled_sampling_time")
  for (i in seq_len(nrow(spec))) {
    key <- spec[i, ]; affected <- targets$participant %in% result$participant & (key$day == targets$day)
    if (key$sample != "day") affected <- affected & key$sample %in% targets$sample
    if (drop_entire_day) affected <- result$participant %in% targets$participant & key$day %in% targets$day
    if (drop_entire_day || !key$variable %in% protected) result[[key$name]] <- replace(result[[key$name]], affected, NA)
  }
  result
}

#' Remove non-compliant observations
#' @param data Canonical results or sample events.
#' @param drop_entire_day Whether one failed sample removes its day.
#' @param drop_unassessed Whether missing assessments are removed.
#' @return Filtered data preserving canonical results when supplied.
#' @export
drop_non_compliant_samples <- function(data, drop_entire_day = TRUE, drop_unassessed = FALSE) {
  .assert_scalar_logical(drop_entire_day, "drop_entire_day"); .assert_scalar_logical(drop_unassessed, "drop_unassessed")
  samples <- .as_samples(data); .require_columns(samples, c("participant", "day", "sample_compliant"), "Sample data")
  remove <- samples$sample_compliant %in% FALSE | (drop_unassessed & is.na(samples$sample_compliant))
  if (drop_entire_day) {
    keys <- unique(samples[remove, c("participant", "day")]); remove <- interaction(samples$participant, samples$day, drop = TRUE) %in% interaction(keys$participant, keys$day, drop = TRUE)
  }
  if (!inherits(data, "carwatch_results")) return(samples[!remove, , drop = FALSE])
  .clear_result_observations(data, remove, drop_entire_day)
}
