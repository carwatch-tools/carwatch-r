#' Merge laboratory saliva values into canonical Study Results
#'
#' Raw tube identifiers remain opaque. With match_on = "sample" values are
#' matched to the recorded tube (falling back to the scheduled tube); with
#' match_on = "position" they are matched to registration-aware positions.
#'
#' @param study_results Complete canonical results.
#' @param saliva Long laboratory tibble.
#' @param correct_swaps Match physical tubes to their recorded position.
#' @param match_on "sample"/"physical_id" or "position".
#' @param missing_carwatch_data Whether unmatched laboratory rows are ignored or rejected.
#' @return Complete canonical results with laboratory and merge-provenance fields.
#' @export
merge_saliva <- function(study_results, saliva, correct_swaps = TRUE, match_on = c("sample", "position", "physical_id"), missing_carwatch_data = c("ignore", "raise")) {
  .require_complete_results(study_results)
  .assert_scalar_logical(correct_swaps, "correct_swaps")
  match_on <- match.arg(match_on); if (identical(match_on, "physical_id")) match_on <- "sample"
  missing_carwatch_data <- match.arg(missing_carwatch_data)
  saliva <- tibble::as_tibble(saliva); .require_columns(saliva, "participant", "Saliva data")
  saliva$participant <- .as_character_id(saliva$participant, "Saliva participant IDs")
  samples <- as_sample_events(study_results)
  known <- c("participant", "sample", "day", "sample_position")
  measurements <- setdiff(names(saliva), known)
  if (!length(measurements)) .carwatch_abort("Saliva data must contain at least one laboratory value column.", "carwatch_schema_error")
  if (any(measurements %in% attr(study_results, "column_spec")$variable)) .carwatch_abort(sprintf("Laboratory variables already exist in Study Results: %s.", paste(intersect(measurements, attr(study_results, "column_spec")$variable), collapse = ", ")), "carwatch_schema_error")
  if (match_on == "sample") {
    .require_columns(saliva, "sample", "Saliva data")
    saliva$sample <- .as_character_id(saliva$sample, "Saliva sample IDs")
    if (anyDuplicated(saliva[c("participant", "sample")])) .carwatch_abort("Saliva data contain duplicate participant/sample matches.", "carwatch_schema_error")
    samples$.match <- if (correct_swaps) ifelse(is.na(samples$recorded_sample) | !nzchar(samples$recorded_sample), samples$sample, samples$recorded_sample) else samples$sample
    matched <- dplyr::left_join(samples, saliva, by = c("participant", ".match" = "sample"))
    unmatched <- dplyr::anti_join(saliva, dplyr::transmute(samples, participant = .data$participant, sample = .data$.match), by = c("participant", "sample"))
    recorded_in_schedule <- samples$recorded_sample %in% samples$sample
    mismatch_corrected <- !is.na(samples$recorded_sample) & samples$recorded_sample != samples$sample & !is.na(matched[[measurements[[1]]]])
  } else {
    .require_columns(saliva, c("day", "sample_position"), "Saliva data")
    saliva$day <- .as_character_id(saliva$day, "Saliva day IDs")
    saliva$sample_position <- suppressWarnings(as.integer(saliva$sample_position))
    if (anyNA(saliva$sample_position) | any(saliva$sample_position < 1L)) .carwatch_abort("Saliva sample_position values must be positive integers.", "carwatch_schema_error")
    if (anyDuplicated(saliva[c("participant", "day", "sample_position")])) .carwatch_abort("Saliva data contain duplicate participant/day/sample_position matches.", "carwatch_schema_error")
    samples$.match_position <- samples$sample_position
    matched <- dplyr::left_join(samples, saliva, by = c("participant", "day", ".match_position" = "sample_position"))
    unmatched <- dplyr::anti_join(saliva, dplyr::transmute(samples, participant = .data$participant, day = .data$day, sample_position = .data$.match_position), by = c("participant", "day", "sample_position"))
    recorded_in_schedule <- !is.na(samples$recorded_sample) & samples$recorded_sample %in% samples$sample
    mismatch_corrected <- rep(FALSE, nrow(samples))
  }
  if (nrow(unmatched) && missing_carwatch_data == "raise") .carwatch_abort("Laboratory measurements do not match a CARWatch sampling event.", "carwatch_schema_error")
  matched$lab_value_available <- apply(!is.na(as.data.frame(matched[measurements])), 1L, all)
  matched$mismatch_corrected <- mismatch_corrected
  matched$recorded_sample_in_schedule <- recorded_in_schedule
  matched$sampling_event_recorded <- !is.na(matched$sampling_time)
  additions <- list()
  for (current_variable in c(measurements, "lab_value_available", "mismatch_corrected", "recorded_sample_in_schedule", "sampling_event_recorded")) additions[[length(additions) + 1L]] <- tibble::tibble(participant = matched$participant, day = matched$day, sample = matched$sample, variable = current_variable, value = as.list(matched[[current_variable]]))
  existing <- .results_long(study_results)
  .from_long_results(dplyr::bind_rows(existing, dplyr::bind_rows(additions)), study_results$participant)
}
