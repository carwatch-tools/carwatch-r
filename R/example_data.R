#' Generate deterministic local CARWatch example data
#'
#' Generated anomalies are represented by ordinary raw-log omissions and a
#' matching manual diary/decision report. The source events themselves are
#' never patched.
#'
#' @param output_dir Target directory.
#' @param study_config List containing `study_name`, `study_days`, `saliva_ids`,
#'   `saliva_times`, and optional `saliva_absolute_times`.
#' @param n_participants Number of generated participants.
#' @param non_compliant_sample_ratio Proportion of observed samples with timing
#'   deliberately outside the default compliance tolerance.
#' @param missing_awakening_time_ratio Proportion of participant-days with a
#'   missing awakening event and first sample.
#' @param missing_sampling_time_ratio Proportion of expected samples omitted
#'   from raw logs, including omissions caused by missing awakenings.
#' @param random_state Integer seed, or `NULL` for an unseeded generation.
#' @param create_cortisol_data Whether to create position-indexed `cortisol.csv`.
#' @param overwrite Whether an existing target directory may be replaced.
#' @param validate Run advisory and submitted-decision conversion after writing.
#' @return The normalized output directory path.
#' @export
generate_synthetic_study_data <- function(output_dir, study_config = NULL, n_participants = 4L, non_compliant_sample_ratio = 0.10, missing_awakening_time_ratio = 0.01, missing_sampling_time_ratio = 0.02, random_state = 42L, create_cortisol_data = FALSE, overwrite = FALSE, validate = TRUE) {
  .assert_scalar_logical(overwrite, "overwrite"); .assert_scalar_logical(create_cortisol_data, "create_cortisol_data"); .assert_scalar_logical(validate, "validate")
  if (is.null(n_participants) || length(n_participants) != 1L || is.na(n_participants) || n_participants < 1L || n_participants != floor(n_participants)) .carwatch_abort("`n_participants` must be a positive integer.", "carwatch_value_error")
  ratios <- c(non_compliant_sample_ratio, missing_awakening_time_ratio, missing_sampling_time_ratio)
  if (any(!is.finite(ratios)) || any(ratios < 0 | ratios > 1)) .carwatch_abort("Synthetic anomaly ratios must be finite numbers between zero and one.", "carwatch_value_error")
  if (!is.null(random_state) && (length(random_state) != 1L || is.na(random_state) || random_state != floor(random_state))) .carwatch_abort("`random_state` must be one integer or NULL.", "carwatch_value_error")
  output_dir <- fs::path_abs(output_dir)
  if (fs::dir_exists(output_dir) && length(fs::dir_ls(output_dir, all = TRUE)) && !overwrite) .carwatch_abort("Synthetic-study output directory exists; set `overwrite = TRUE` to replace it.", "carwatch_file_error")
  if (fs::dir_exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE, force = TRUE)
  fs::dir_create(output_dir, recurse = TRUE)
  config <- utils::modifyList(list(study_name = "Synthetic CARWatch Study", study_days = 2L, saliva_ids = c("S1", "S2", "S3", "S4"), saliva_times = c(0, 15, 15), saliva_absolute_times = "12:00"), study_config %||% list())
  if (!is.list(config) || !length(config$saliva_ids) || config$study_days < 1L) .carwatch_abort("`study_config` requires positive `study_days` and non-empty `saliva_ids`.", "carwatch_schema_error")
  if (!is.null(random_state)) set.seed(as.integer(random_state))
  participants <- sprintf("VP_%02d", seq_len(as.integer(n_participants)))
  positions <- seq_along(config$saliva_ids); days <- seq_len(as.integer(config$study_days))
  sample_keys <- as.vector(outer(as.vector(outer(participants, days, paste, sep = "\r")), positions, paste, sep = "\r"))
  day_keys <- as.vector(outer(participants, days, paste, sep = "\r"))
  missing_awake <- sample(day_keys, round(length(day_keys) * missing_awakening_time_ratio))
  forced_missing <- if (length(missing_awake)) paste(sub("\r[0-9]+$", "", missing_awake), 1L, sep = "\r") else character()
  missing_count <- round(length(sample_keys) * missing_sampling_time_ratio)
  if (missing_count < length(forced_missing)) .carwatch_abort("`missing_sampling_time_ratio` must cover the first samples omitted for missing awakenings.", "carwatch_value_error")
  missing_samples <- unique(c(forced_missing, sample(setdiff(sample_keys, forced_missing), missing_count - length(forced_missing))))
  non_compliant <- sample(setdiff(sample_keys, missing_samples), round(length(sample_keys) * non_compliant_sample_ratio))
  diary <- list(); cortisol <- list(); metadata <- jsonlite::toJSON(config, auto_unbox = TRUE)
  start <- as.POSIXct("2026-02-01 06:00:00", tz = "Europe/Berlin")
  for (participant in participants) {
    folder <- fs::path(output_dir, "logs", participant); fs::dir_create(folder, recurse = TRUE)
    for (day in days) {
      day_key <- paste(participant, day, sep = "\r"); date <- as.Date(start + (day - 1L) * 86400)
      wake <- start + (day - 1L) * 86400 + sample(0:600, 1); has_awake <- !day_key %in% missing_awake
      entries <- if (day == 1L) sprintf("%.0f;local;study_metadata;%s", as.numeric(start) * 1000, metadata) else character()
      if (has_awake) entries <- c(entries, sprintf("%.0f;local;spontaneous_awakening;{\"id\":-1}", as.numeric(wake) * 1000))
      row <- list(participant = participant, day = paste0("D", day), date = format(date, "%Y-%m-%d"), awakening_time = if (!has_awake) format(wake, "%H:%M") else NA_character_)
      cursor <- wake
      for (position in positions) {
        if (position <= length(config$saliva_times)) cursor <- cursor + config$saliva_times[[position]] * 60 else cursor <- as.POSIXct(sprintf("%s %s:00", format(date, "%Y-%m-%d"), config$saliva_absolute_times[[position - length(config$saliva_times)]]), tz = "Europe/Berlin")
        key <- paste(participant, day, position, sep = "\r"); omitted <- key %in% missing_samples
        if (omitted) row[[paste0("sampling_time_", position)]] <- format(cursor, "%H:%M")
        if (!omitted) {
          sampled <- cursor + sample(-120:120, 1) + if (key %in% non_compliant) 600 else 0
          payload <- jsonlite::toJSON(list(id = position - 1L, saliva_id = position, barcode_value = sprintf("%010d", sample.int(999999999, 1)), day_scanned = day, day_expected = day, sample_scanned = config$saliva_ids[[position]], sample_expected = config$saliva_ids[[position]]), auto_unbox = TRUE)
          entries <- c(entries, sprintf("%.0f;local;barcode_scanned;%s", as.numeric(sampled) * 1000, payload))
        }
        if (create_cortisol_data) cortisol[[length(cortisol) + 1L]] <- tibble::tibble(participant = participant, day = paste0("D", day), sample_position = position, cortisol = round(8 + c(0, 3, 5, 2)[min(position, 4L)] + stats::rnorm(1, 0, .25), 2))
      }
      diary[[length(diary) + 1L]] <- tibble::as_tibble(row)
      entries <- entries[order(suppressWarnings(as.numeric(sub(";.*$", "", entries))), method = "radix")]
      writeLines(entries, fs::path(folder, sprintf("carwatch_synthetic_%s_%s.csv", participant, format(date, "%Y%m%d"))), useBytes = TRUE)
    }
  }
  diary <- dplyr::bind_rows(diary)
  wanted <- c("participant", "day", "date", "awakening_time", paste0("sampling_time_", positions))
  for (column in setdiff(wanted, names(diary))) diary[[column]] <- NA_character_
  diary <- diary[, wanted]
  readr::write_csv(diary, fs::path(output_dir, "manual_diary.csv"), na = "")
  folders <- stats::setNames(fs::path(output_dir, "logs", participants), participants)
  initial <- convert_raw_logs(read_raw_logs_from_participant_dirs(folders), errors = "warn", create_report = TRUE)
  decisions <- initial$report$issues
  decisions$user_decision[decisions$code %in% c("missing_awakening_time", "missing_scheduled_sample_event")] <- "accept"
  readr::write_csv(decisions, fs::path(output_dir, "issue_decisions.csv"), na = "")
  if (create_cortisol_data) readr::write_csv(dplyr::bind_rows(cortisol), fs::path(output_dir, "cortisol.csv"), na = "")
  if (validate) convert_raw_logs(read_raw_logs_from_participant_dirs(folders), errors = "raise", issue_decisions = decisions, manual_diary = read_manual_diary(fs::path(output_dir, "manual_diary.csv")))
  output_dir
}
