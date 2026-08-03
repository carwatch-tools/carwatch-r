#' Generate deterministic local CARWatch example data
#'
#' @param output_dir Target directory.
#' @param study_config List containing `study_name`, `study_days`, `saliva_ids`,
#'   `saliva_times`, and optional `saliva_absolute_times`.
#' @param n_participants Number of generated participants.
#' @param random_state Seed for deterministic output.
#' @param overwrite Whether an existing target directory may be replaced.
#' @param ... Reserved parity controls for non-compliance and missing events.
#' @export
generate_synthetic_study_data <- function(output_dir, study_config = NULL, n_participants = 4L, random_state = 42L, overwrite = FALSE, ...) {
  .assert_scalar_logical(overwrite, "overwrite")
  output_dir <- fs::path_abs(output_dir)
  if (fs::dir_exists(output_dir) && length(fs::dir_ls(output_dir, all = TRUE)) && !overwrite) .carwatch_abort("Synthetic-study output directory exists; set `overwrite = TRUE` to replace it.", "carwatch_file_error")
  fs::dir_create(output_dir, recurse = TRUE)
  config <- utils::modifyList(list(study_name = "Synthetic CARWatch Study", study_days = 2L, saliva_ids = c("S1", "S2", "S3", "S4"), saliva_times = c(0, 15, 15), saliva_absolute_times = "12:00"), study_config %||% list())
  set.seed(random_state)
  participants <- sprintf("VP_%02d", seq_len(n_participants))
  for (participant in participants) {
    folder <- fs::path(output_dir, "logs", participant); fs::dir_create(folder, recurse = TRUE)
    start <- as.POSIXct("2026-02-01 06:00:00", tz = "Europe/Berlin")
    metadata <- jsonlite::toJSON(config, auto_unbox = TRUE)
    for (day in seq_len(config$study_days)) {
      date <- as.Date(start + (day - 1L) * 86400)
      filename <- fs::path(folder, sprintf("carwatch_synthetic_%s_%s.csv", participant, format(date, "%Y%m%d")))
      wake <- start + (day - 1L) * 86400 + sample(0:600, 1)
      entries <- character()
      if (day == 1L) entries <- c(entries, sprintf("%.0f;local;study_metadata;%s", as.numeric(start) * 1000, metadata))
      entries <- c(entries, sprintf("%.0f;local;spontaneous_awakening;{\"id\":-1}", as.numeric(wake) * 1000))
      cursor <- wake
      for (position in seq_along(config$saliva_ids)) {
        if (position <= length(config$saliva_times)) cursor <- cursor + config$saliva_times[[position]] * 60 else cursor <- as.POSIXct(sprintf("%s %s:00", format(date, "%Y-%m-%d"), config$saliva_absolute_times[[position - length(config$saliva_times)]]), tz = "Europe/Berlin")
        sampled <- cursor + sample(-120:120, 1)
        payload <- jsonlite::toJSON(list(id = position - 1L, saliva_id = position, barcode_value = sprintf("%010d", sample.int(999999999, 1)), day_scanned = day, day_expected = day, sample_scanned = config$saliva_ids[[position]], sample_expected = config$saliva_ids[[position]]), auto_unbox = TRUE)
        entries <- c(entries, sprintf("%.0f;local;barcode_scanned;%s", as.numeric(sampled) * 1000, payload))
      }
      writeLines(entries, filename, useBytes = TRUE)
    }
  }
  readr::write_csv(tibble::tibble(participant = participants, sample = rep(config$saliva_ids[[1]], length(participants)), cortisol = round(rlnorm(length(participants), log(10), 0.2), 2)), fs::path(output_dir, "cortisol.csv"))
  output_dir
}
