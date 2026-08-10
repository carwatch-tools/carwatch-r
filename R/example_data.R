.synthetic_config_value <- function(config, aliases, default = NULL) {
  supplied <- intersect(aliases, names(config))
  if (!length(supplied)) return(default)
  values <- lapply(config[supplied], identity)
  encoded <- vapply(values, function(value) as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")), character(1))
  if (length(unique(encoded)) != 1L) .carwatch_abort(sprintf("Conflicting study configuration aliases: %s.", paste(supplied, collapse = ", ")), "carwatch_value_error")
  values[[1]]
}

.decode_synthetic_qr <- function(payload) {
  if (!is.character(payload) || length(payload) != 1L || !startsWith(payload, "CARWATCH;")) .carwatch_abort("Study Manager QR payloads must begin with 'CARWATCH;'.", "carwatch_value_error")
  fields <- strsplit(payload, ";", fixed = TRUE)[[1]][-1]
  fields <- fields[nzchar(fields)]
  result <- list()
  for (field in fields) {
    separator <- regexpr(":", field, fixed = TRUE)[[1]]
    if (separator < 1L) .carwatch_abort(sprintf("Invalid Study Manager QR field: %s.", field), "carwatch_value_error")
    key <- substr(field, 1L, separator - 1L); value <- substr(field, separator + 1L, nchar(field))
    if (key %in% names(result)) .carwatch_abort(sprintf("Duplicate Study Manager QR field: %s.", key), "carwatch_value_error")
    result[[key]] <- value
  }
  for (key in intersect(c("D", "NP"), names(result))) result[[key]] <- suppressWarnings(as.integer(result[[key]]))
  for (key in intersect(c("E", "FD", "FM"), names(result))) result[[key]] <- suppressWarnings(as.integer(result[[key]]))
  if ("T" %in% names(result)) result$T <- if (nzchar(result$T)) as.numeric(strsplit(result$T, ",", fixed = TRUE)[[1]]) else numeric()
  if ("A" %in% names(result)) result$A <- if (nzchar(result$A)) strsplit(result$A, ",", fixed = TRUE)[[1]] else character()
  result
}

.synthetic_bool <- function(value, name) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(value)
  if (length(value) == 1L && !is.na(value) && as.character(value) %in% c("0", "1")) return(as.character(value) == "1")
  .carwatch_abort(sprintf("`%s` must be boolean or 0/1.", name), "carwatch_value_error")
}

.synthetic_positive_int <- function(value, name) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || number < 1L || number != floor(number)) .carwatch_abort(sprintf("`%s` must be a positive integer.", name), "carwatch_value_error")
  as.integer(number)
}

.synthetic_date <- function(value, name) {
  date <- suppressWarnings(as.Date(value))
  if (length(date) != 1L || is.na(date)) .carwatch_abort(sprintf("`%s` must be an ISO date.", name), "carwatch_value_error")
  date
}

.synthetic_registration <- function(raw, number, default_days, sample_count, sample_ids, registration_date = NULL) {
  if (!is.list(raw)) .carwatch_abort("Every synthetic registration must be a list.", "carwatch_type_error")
  study_name <- as.character(.synthetic_config_value(raw, c("study_name", "studyName", "N"), paste0("Study", number)))
  if (length(study_name) != 1L || !nzchar(trimws(study_name))) .carwatch_abort("Registration study names must be non-empty strings.", "carwatch_value_error")
  study_days <- .synthetic_positive_int(.synthetic_config_value(raw, c("study_days", "numDays", "D"), default_days), "registration study_days")
  default_date <- registration_date %||% (as.Date("2026-01-04") + 7L * (number - 1L))
  registered <- .synthetic_date(.synthetic_config_value(raw, "registration_date", default_date), "registration_date")
  collection_dates <- .synthetic_config_value(raw, "collection_dates", NULL)
  if (is.null(collection_dates)) collection_dates <- registered + seq_len(study_days)
  collection_dates <- as.Date(collection_dates)
  if (length(collection_dates) != study_days || anyNA(collection_dates)) .carwatch_abort("Registration collection_dates must contain one valid date per study day.", "carwatch_value_error")
  ids <- .synthetic_config_value(raw, "saliva_ids", sample_ids)
  ids <- trimws(as.character(ids))
  if (length(ids) != sample_count || any(!nzchar(ids)) || anyDuplicated(ids)) .carwatch_abort("Registration saliva_ids must be unique and match the configured sample count.", "carwatch_value_error")
  condition <- .synthetic_config_value(raw, "condition", NA_character_)
  list(study_name = trimws(study_name), study_days = study_days, registration_date = registered, collection_dates = collection_dates, saliva_ids = ids, condition = if (is.null(condition)) NA_character_ else as.character(condition))
}

.parse_synthetic_config <- function(study_config, n_participants) {
  config <- if (is.null(study_config)) list() else if (is.list(study_config)) study_config else if (is.character(study_config)) .decode_synthetic_qr(study_config) else .carwatch_abort("`study_config` must be a list, QR payload, or NULL.", "carwatch_type_error")
  misplaced <- intersect(names(config), c("non_compliant_sample_count", "nonCompliantSampleCount", "missing_awakening_time_count", "missingAwakeningTimeCount", "missing_sampling_time_count", "missingSamplingTimeCount", "seed"))
  if (length(misplaced)) .carwatch_abort("Synthetic generation controls are function parameters, not study_config.", "carwatch_value_error")
  study_name <- as.character(.synthetic_config_value(config, c("study_name", "studyName", "N"), "SyntheticStudy"))
  participant_prefix <- as.character(.synthetic_config_value(config, c("participant_prefix", "participantPrefix"), "VP_"))
  sample_prefix <- as.character(.synthetic_config_value(config, c("sample_prefix", "samplePrefix"), "S"))
  start_zero <- .synthetic_config_value(config, c("start_sample_from_zero", "startSampleFromZero"), FALSE)
  qr_start <- .synthetic_config_value(config, "SS", NULL)
  if (!is.null(qr_start)) {
    qr_start <- as.character(qr_start)
    match <- regexec("^(.*?)([01])$", qr_start, perl = TRUE); parts <- regmatches(qr_start, match)[[1]]
    if (length(parts) != 3L || !nzchar(parts[[2]])) .carwatch_abort("QR field `SS` must contain a sample prefix ending in 0 or 1.", "carwatch_value_error")
    sample_prefix <- parts[[2]]; start_zero <- parts[[3]] == "0"
  }
  start_zero <- .synthetic_bool(start_zero, "start_sample_from_zero")
  saliva_times <- as.numeric(.synthetic_config_value(config, c("saliva_distances", "salivaDistances", "saliva_times", "T"), c(0, 30, 15, 15)))
  saliva_absolute_times <- as.character(.synthetic_config_value(config, c("saliva_alarm_times", "salivaAlarmTimes", "saliva_absolute_times", "A"), character()))
  if (anyNA(saliva_times) || any(saliva_times < 0) || any(!grepl("^[0-2][0-9]:[0-5][0-9]$", saliva_absolute_times))) .carwatch_abort("Synthetic sampling schedules are invalid.", "carwatch_value_error")
  sample_count <- length(saliva_times) + length(saliva_absolute_times)
  if (!sample_count) .carwatch_abort("A synthetic study needs at least one sampling time.", "carwatch_value_error")
  default_ids <- paste0(sample_prefix, seq.int(if (start_zero) 0L else 1L, length.out = sample_count))
  default_days <- .synthetic_positive_int(.synthetic_config_value(config, c("study_days", "numDays", "D"), 4L), "study_days")
  registrations <- .synthetic_config_value(config, "registrations", NULL)
  if (is.null(registrations)) registrations <- list(list(study_name = "Study1", study_days = default_days, registration_date = "2026-01-04"))
  if (!is.list(registrations) || !length(registrations)) .carwatch_abort("`registrations` must contain at least one registration list.", "carwatch_value_error")
  registrations <- lapply(seq_along(registrations), function(index) .synthetic_registration(registrations[[index]], index, default_days, sample_count, default_ids))
  configured_participants <- .synthetic_config_value(config, c("num_participants", "numParticipants", "NP"), 80L)
  participant_count <- .synthetic_positive_int(n_participants %||% configured_participants, "n_participants")
  list(study_name = study_name, participant_prefix = participant_prefix, saliva_times = saliva_times, saliva_absolute_times = saliva_absolute_times, registrations = registrations, n_participants = participant_count)
}

.synthetic_cortisol_profile <- function(relative_count, absolute_count) {
  morning <- if (relative_count == 1L) 1 else if (relative_count == 4L) c(1, 1.62, 1.78, 1.42) else if (relative_count > 1L) c(seq(1, 1.75, length.out = ceiling(relative_count / 2)), seq(1.6, 1.4, length.out = floor(relative_count / 2))) else numeric()
  c(morning[seq_len(relative_count)], if (absolute_count) seq(0.72, 0.38, length.out = absolute_count) else numeric())
}

#' Generate deterministic local CARWatch example data
#'
#' Generated anomalies are represented by ordinary raw-log omissions and a
#' matching manual diary/decision report. The source events themselves are
#' never patched. Study Manager snake-case, camelCase, and QR aliases are
#' accepted, including multiple registration blocks and opaque saliva IDs.
#'
#' @param output_dir Target directory.
#' @param study_config Study Manager-style list, decoded `CARWATCH;` QR payload,
#'   or `NULL` for the four-day CAR default.
#' @param n_participants Number of generated participants. When `NULL`, use the
#'   configured value or the Study Manager default of 80.
#' @param non_compliant_sample_ratio Proportion of observed samples with timing
#'   deliberately outside the default compliance tolerance.
#' @param missing_awakening_time_ratio Proportion of participant-days with a
#'   missing awakening event and first sample.
#' @param missing_sampling_time_ratio Total proportion of expected scans omitted.
#' @param random_state Integer seed, or `NULL` for unseeded generation.
#' @param create_cortisol_data Whether to create position-indexed `cortisol.csv`.
#' @param overwrite Whether an existing target directory may be replaced.
#' @param validate Run advisory and submitted-decision conversion after writing.
#' @return The normalized output directory path.
#' @export
generate_synthetic_study_data <- function(output_dir, study_config = NULL, n_participants = NULL, non_compliant_sample_ratio = 0.10, missing_awakening_time_ratio = 0.01, missing_sampling_time_ratio = 0.02, random_state = 42L, create_cortisol_data = FALSE, overwrite = FALSE, validate = TRUE) {
  .assert_scalar_logical(overwrite, "overwrite"); .assert_scalar_logical(create_cortisol_data, "create_cortisol_data"); .assert_scalar_logical(validate, "validate")
  ratios <- c(non_compliant_sample_ratio, missing_awakening_time_ratio, missing_sampling_time_ratio)
  if (any(!is.finite(ratios)) || any(ratios < 0 | ratios > 1)) .carwatch_abort("Synthetic anomaly ratios must be finite numbers between zero and one.", "carwatch_value_error")
  if (!is.null(random_state) && (length(random_state) != 1L || is.na(random_state) || random_state != floor(random_state))) .carwatch_abort("`random_state` must be one integer or NULL.", "carwatch_value_error")
  config <- .parse_synthetic_config(study_config, n_participants)
  output_dir <- fs::path_abs(output_dir)
  if (fs::dir_exists(output_dir) && length(fs::dir_ls(output_dir, all = TRUE)) && !overwrite) .carwatch_abort("Synthetic-study output directory exists; set `overwrite = TRUE` to replace it.", "carwatch_file_error")
  if (fs::dir_exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE, force = TRUE)
  fs::dir_create(output_dir, recurse = TRUE)
  if (!is.null(random_state)) set.seed(as.integer(random_state))
  participants <- sprintf(paste0(config$participant_prefix, "%0", max(2L, nchar(config$n_participants)), "d"), seq_len(config$n_participants))
  days <- list(); canonical_day <- 0L
  for (registration in seq_along(config$registrations)) for (registration_day in seq_len(config$registrations[[registration]]$study_days)) {
    canonical_day <- canonical_day + 1L
    days[[length(days) + 1L]] <- list(registration = registration, registration_day = registration_day, day = paste0("D", canonical_day), date = config$registrations[[registration]]$collection_dates[[registration_day]])
  }
  sample_count <- length(config$saliva_times) + length(config$saliva_absolute_times)
  sample_grid <- expand.grid(participant = participants, day = vapply(days, `[[`, character(1), "day"), sample_position = seq_len(sample_count), stringsAsFactors = FALSE)
  day_grid <- expand.grid(participant = participants, day = vapply(days, `[[`, character(1), "day"), stringsAsFactors = FALSE)
  missing_day_count <- round(nrow(day_grid) * missing_awakening_time_ratio)
  missing_days <- if (missing_day_count) sample(paste(day_grid$participant, day_grid$day, sep = "\r"), missing_day_count) else character()
  forced_missing <- if (length(missing_days)) paste(missing_days, 1L, sep = "\r") else character()
  missing_count <- round(nrow(sample_grid) * missing_sampling_time_ratio)
  if (missing_count < length(forced_missing)) .carwatch_abort("`missing_sampling_time_ratio` must cover first samples omitted for missing awakenings.", "carwatch_value_error")
  sample_keys <- paste(sample_grid$participant, sample_grid$day, sample_grid$sample_position, sep = "\r")
  missing_samples <- c(forced_missing, if (missing_count > length(forced_missing)) sample(setdiff(sample_keys, forced_missing), missing_count - length(forced_missing)) else character())
  eligible <- setdiff(sample_keys, missing_samples)
  non_compliant_count <- round(nrow(sample_grid) * non_compliant_sample_ratio)
  if (non_compliant_count > length(eligible)) .carwatch_abort("Too many non-compliant samples requested after missing samples were selected.", "carwatch_value_error")
  non_compliant <- if (non_compliant_count) sample(eligible, non_compliant_count) else character()
  diary <- list(); cortisol <- list(); profile <- .synthetic_cortisol_profile(length(config$saliva_times), length(config$saliva_absolute_times))
  file_token <- gsub("[^A-Za-z0-9-]+", "-", config$study_name)
  for (participant in participants) {
    folder <- fs::path(output_dir, "logs", participant); fs::dir_create(folder, recurse = TRUE)
    for (registration in seq_along(config$registrations)) {
      current <- config$registrations[[registration]]
      metadata <- jsonlite::toJSON(list(study_name = current$study_name, study_days = current$study_days, saliva_ids = current$saliva_ids, saliva_times = config$saliva_times, saliva_absolute_times = config$saliva_absolute_times), auto_unbox = TRUE)
      timestamp <- as.POSIXct(paste(current$registration_date, "18:00:00"), tz = "Europe/Berlin")
      writeLines(sprintf("%.0f;local;study_metadata;%s", as.numeric(timestamp) * 1000, metadata), fs::path(folder, sprintf("carwatch_%s_%s_%s.csv", file_token, participant, format(current$registration_date, "%Y%m%d"))), useBytes = TRUE)
    }
    for (day_config in days) {
      current <- config$registrations[[day_config$registration]]
      key <- paste(participant, day_config$day, sep = "\r")
      date <- day_config$date
      wake <- as.POSIXct(paste(date, "06:00:00"), tz = "Europe/Berlin") + sample(0:600, 1)
      has_awake <- !key %in% missing_days
      entries <- if (has_awake) sprintf("%.0f;local;spontaneous_awakening;{\"id\":-1}", as.numeric(wake) * 1000) else character()
      row <- list(participant = participant, day = day_config$day, date = format(date, "%Y-%m-%d"), awakening_time = if (!has_awake) format(wake, "%H:%M") else NA_character_)
      cursor <- wake
      for (position in seq_len(sample_count)) {
        if (position <= length(config$saliva_times)) cursor <- cursor + config$saliva_times[[position]] * 60 else cursor <- as.POSIXct(paste(date, paste0(config$saliva_absolute_times[[position - length(config$saliva_times)]], ":00")), tz = "Europe/Berlin")
        sample_key <- paste(participant, day_config$day, position, sep = "\r")
        omitted <- sample_key %in% missing_samples
        if (omitted) row[[paste0("sampling_time_", position)]] <- format(cursor, "%H:%M")
        if (!omitted) {
          sampled <- cursor + sample(-120:120, 1) + if (sample_key %in% non_compliant) 600 else 0
          payload <- jsonlite::toJSON(list(id = position - 1L, saliva_id = position, barcode_value = sprintf("%010d", sample.int(999999999, 1)), day_scanned = day_config$registration_day, day_expected = day_config$registration_day, sample_scanned = current$saliva_ids[[position]], sample_expected = current$saliva_ids[[position]]), auto_unbox = TRUE)
          entries <- c(entries, sprintf("%.0f;local;barcode_scanned;%s", as.numeric(sampled) * 1000, payload))
          if (position <= length(config$saliva_times)) cursor <- sampled
        }
        if (create_cortisol_data) cortisol[[length(cortisol) + 1L]] <- tibble::tibble(participant = participant, day = day_config$day, sample_position = position, condition = if (!is.na(current$condition)) current$condition else if (length(config$registrations) > 1L) current$study_name else if (length(config$saliva_absolute_times)) "diurnal" else "car", cortisol = round((8 + stats::rnorm(1, 0, .25)) * profile[[position]], 2))
      }
      diary[[length(diary) + 1L]] <- tibble::as_tibble(row)
      entries <- entries[order(suppressWarnings(as.numeric(sub(";.*$", "", entries))), method = "radix")]
      writeLines(entries, fs::path(folder, sprintf("carwatch_%s_%s_%s.csv", file_token, participant, format(date, "%Y%m%d"))), useBytes = TRUE)
    }
  }
  diary <- dplyr::bind_rows(diary)
  wanted <- c("participant", "day", "date", "awakening_time", paste0("sampling_time_", seq_len(sample_count)))
  for (column in setdiff(wanted, names(diary))) diary[[column]] <- NA_character_
  readr::write_csv(diary[, wanted], fs::path(output_dir, "manual_diary.csv"), na = "")
  folders <- stats::setNames(fs::path(output_dir, "logs", participants), participants)
  initial <- suppressWarnings(convert_raw_logs(read_raw_logs_from_participant_dirs(folders), errors = "warn", create_report = TRUE))
  decisions <- initial$report$issues
  decisions$user_decision[decisions$code %in% c("missing_awakening_time", "missing_scheduled_sample_event")] <- "accept"
  readr::write_csv(decisions, fs::path(output_dir, "issue_decisions.csv"), na = "")
  if (create_cortisol_data) readr::write_csv(dplyr::bind_rows(cortisol), fs::path(output_dir, "cortisol.csv"), na = "")
  readme <- c(paste0("# ", config$study_name), "", sprintf("Participants: %d", config$n_participants), sprintf("Canonical days: %d", length(days)), sprintf("Registrations: %s", paste(vapply(config$registrations, `[[`, character(1), "study_name"), collapse = ", ")), "", "Use read_raw_logs_from_participant_dirs(), convert_raw_logs(), read_conversion_report(), and read_manual_diary() to reconstruct this study.")
  writeLines(readme, fs::path(output_dir, "README.md"), useBytes = TRUE)
  if (validate) convert_raw_logs(read_raw_logs_from_participant_dirs(folders), errors = "raise", issue_decisions = decisions, manual_diary = read_manual_diary(fs::path(output_dir, "manual_diary.csv")))
  output_dir
}
