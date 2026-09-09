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

.synthetic_nonnegative_int <- function(value, name) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || number < 0L || number != floor(number)) .carwatch_abort(sprintf("`%s` must be a non-negative integer.", name), "carwatch_value_error")
  as.integer(number)
}

.synthetic_relative_schedule <- function(value) {
  if (!is.numeric(value)) .carwatch_abort("`saliva_distances` must be a numeric vector of non-negative integer minutes.", "carwatch_type_error")
  if (anyNA(value) || any(value < 0) || any(value != floor(value))) .carwatch_abort("`saliva_distances` must contain non-negative integer minutes.", "carwatch_value_error")
  as.integer(value)
}

.synthetic_absolute_schedule <- function(value) {
  if (is.null(value)) return(character())
  if (!is.atomic(value) || is.logical(value)) .carwatch_abort("`saliva_alarm_times` must contain HH:MM strings or HHMM integers.", "carwatch_type_error")
  output <- vapply(value, function(item) {
    if (is.numeric(item)) {
      if (is.na(item) || item != floor(item)) .carwatch_abort("`saliva_alarm_times` must contain HH:MM strings or HHMM integers.", "carwatch_type_error")
      item <- sprintf("%04d", as.integer(item))
    }
    item <- as.character(item)
    compact <- gsub(":", "", item, fixed = TRUE)
    if (!grepl("^[0-9]{4}$", compact)) .carwatch_abort(sprintf("Invalid fixed saliva alarm time: %s.", item), "carwatch_value_error")
    hour <- as.integer(substr(compact, 1L, 2L)); minute <- as.integer(substr(compact, 3L, 4L))
    if (hour > 23L || minute > 59L) .carwatch_abort(sprintf("Invalid fixed saliva alarm time: %s.", item), "carwatch_value_error")
    sprintf("%02d:%02d", hour, minute)
  }, character(1))
  unname(output)
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
  saliva_times <- .synthetic_relative_schedule(.synthetic_config_value(config, c("saliva_distances", "salivaDistances", "saliva_times", "T"), c(0, 30, 15, 15)))
  saliva_absolute_times <- .synthetic_absolute_schedule(.synthetic_config_value(config, c("saliva_alarm_times", "salivaAlarmTimes", "saliva_absolute_times", "A"), character()))
  sample_count <- length(saliva_times) + length(saliva_absolute_times)
  if (!sample_count) .carwatch_abort("A synthetic study needs at least one sampling time.", "carwatch_value_error")
  default_ids <- paste0(sample_prefix, seq.int(if (start_zero) 0L else 1L, length.out = sample_count))
  default_days <- .synthetic_positive_int(.synthetic_config_value(config, c("study_days", "numDays", "D"), 4L), "study_days")
  registrations <- .synthetic_config_value(config, "registrations", NULL)
  if (is.null(registrations)) registrations <- list(list(study_name = "Study1", study_days = default_days, registration_date = "2026-01-04"))
  if (!is.list(registrations) || !length(registrations)) .carwatch_abort("`registrations` must contain at least one registration list.", "carwatch_value_error")
  registrations <- lapply(seq_along(registrations), function(index) .synthetic_registration(registrations[[index]], index, default_days, sample_count, default_ids))
  registration_names <- vapply(registrations, `[[`, character(1), "study_name")
  if (anyDuplicated(registration_names)) .carwatch_abort("Registration study_name values must be unique within a synthetic study.", "carwatch_value_error")
  configured_participants <- .synthetic_config_value(config, c("num_participants", "numParticipants", "NP"), 80L)
  participant_count <- .synthetic_positive_int(n_participants %||% configured_participants, "n_participants")
  has_evening_sample <- .synthetic_bool(.synthetic_config_value(config, c("has_evening_sample", "hasEveningSample", "E"), length(saliva_absolute_times) > 0L), "has_evening_sample")
  check_duplicates <- .synthetic_bool(.synthetic_config_value(config, c("check_duplicates", "checkDuplicates", "FD"), FALSE), "check_duplicates")
  enable_manual_scan <- .synthetic_bool(.synthetic_config_value(config, c("enable_manual_scan", "enableManualScan", "FM"), FALSE), "enable_manual_scan")
  filename_token <- as.character(.synthetic_config_value(config, "filename_token", study_name))
  if (length(filename_token) != 1L || !nzchar(trimws(filename_token))) .carwatch_abort("`filename_token` must be a non-empty string.", "carwatch_value_error")
  list(study_name = study_name, filename_token = filename_token, participant_prefix = participant_prefix, saliva_times = saliva_times, saliva_absolute_times = saliva_absolute_times, has_evening_sample = has_evening_sample, check_duplicates = check_duplicates, enable_manual_scan = enable_manual_scan, registrations = registrations, n_participants = participant_count)
}

.synthetic_cortisol_profile <- function(relative_count, absolute_count) {
  morning <- if (relative_count == 1L) 1 else if (relative_count == 4L) c(1, 1.62, 1.78, 1.42) else if (relative_count > 1L) c(seq(1, 1.75, length.out = ceiling(relative_count / 2)), seq(1.6, 1.4, length.out = floor(relative_count / 2))) else numeric()
  c(morning[seq_len(relative_count)], if (absolute_count) seq(0.72, 0.38, length.out = absolute_count) else numeric())
}

.synthetic_seed <- function(random_state, offset = 0L) {
  if (is.null(random_state)) return(NULL)
  as.integer((as.double(random_state) + as.double(offset)) %% 2147483647)
}

.synthetic_sample_keys <- function(participants, days, sample_count) {
  unlist(lapply(participants, function(participant) {
    unlist(lapply(days, function(day_config) {
      paste(participant, day_config$day, seq_len(sample_count), sep = "\r")
    }), use.names = FALSE)
  }), use.names = FALSE)
}

.synthetic_day_keys <- function(participants, days) {
  unlist(lapply(participants, function(participant) {
    vapply(days, function(day_config) paste(participant, day_config$day, sep = "\r"), character(1))
  }), use.names = FALSE)
}

.synthetic_select <- function(values, size, seed) {
  if (!size) return(character())
  if (is.null(seed)) return(sample(values, size, replace = FALSE))
  withr::with_seed(seed, sample(values, size, replace = FALSE))
}

.synthetic_signed_deviation <- function(non_compliant, absolute, relative_interval = NULL, first_relative = FALSE) {
  if (absolute) {
    bounds <- if (non_compliant) c(17, 35) else c(0, 14)
    magnitude <- stats::runif(1, bounds[[1]], bounds[[2]])
    return(if (stats::runif(1) >= 0.5) magnitude else -magnitude)
  }
  if (first_relative) {
    bounds <- if (non_compliant) c(7, 25) else c(0, 4)
    return(stats::runif(1, bounds[[1]], bounds[[2]]))
  }
  if (is.null(relative_interval)) .carwatch_abort("Relative sample deviations require their interval.", "carwatch_runtime_error")
  bounds <- if (non_compliant) c(7, min(25, relative_interval - 1)) else c(0, 4)
  magnitude <- stats::runif(1, bounds[[1]], bounds[[2]])
  if (stats::runif(1) >= 0.5) magnitude else -magnitude
}

.synthetic_barcode <- function(participant, registration, position) {
  hexadecimal <- substr(digest::digest(paste(participant, registration, position, sep = "|"), algo = "sha256", serialize = FALSE), 1L, 8L)
  digits <- strsplit(toupper(hexadecimal), "", fixed = TRUE)[[1]]
  values <- match(digits, c(as.character(0:9), LETTERS[1:6])) - 1
  number <- sum(values * 16 ^ rev(seq_along(values) - 1L))
  sprintf("%010.0f", number)
}

.write_synthetic_cortisol <- function(path, config, participants, days, profile, random_state) {
  seed <- .synthetic_seed(random_state, 30L)
  generate <- function() {
    baselines <- stats::setNames(stats::rlnorm(length(participants), log(12), 0.28), participants)
  rows <- list()
  for (day_config in days) {
    registration <- config$registrations[[day_config$registration]]
    condition <- if (!is.na(registration$condition)) registration$condition else if (length(config$registrations) > 1L) registration$study_name else if (length(config$saliva_absolute_times)) "diurnal" else "car"
    day_multiplier <- stats::rlnorm(1, 0, 0.14)
    condition_multiplier <- if (tolower(condition) == "challenge") 1.12 else 1
    for (participant in participants) for (position in seq_along(profile)) {
      value <- baselines[[participant]] * day_multiplier * profile[[position]] * condition_multiplier * stats::rlnorm(1, 0, 0.11)
      rows[[length(rows) + 1L]] <- tibble::tibble(participant = participant, day = day_config$day, sample_position = position, cortisol = round(max(value, 0.2), 3), condition = condition)
    }
  }
    readr::write_csv(dplyr::bind_rows(rows), path, na = "")
  }
  if (is.null(seed)) generate() else withr::with_seed(seed, generate())
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
#' @param non_compliant_sample_ratio Proportion of expected samples with timing
#'   deliberately outside the default compliance tolerance. Relative and fixed
#'   clock-time samples use their respective tolerance-aware deviation ranges.
#' @param missing_awakening_time_ratio Proportion of participant-days with a
#'   missing awakening event and first sample.
#' @param missing_sampling_time_ratio Total proportion of expected scans omitted.
#' @param random_state Integer seed, or `NULL` for unseeded generation. Separate
#'   deterministic streams are used for timings, anomaly selection, and cortisol.
#' @param create_cortisol_data Whether to create position-indexed `cortisol.csv`.
#' @param overwrite Whether an existing target directory may be replaced.
#' @param validate Run advisory and submitted-decision conversion after writing.
#' @return The normalized output directory path.
#' @examples
#' output <- tempfile("carwatch-study-")
#' generate_synthetic_study_data(
#'   output,
#'   study_config = list(study_days = 1, saliva_distances = c(0, 30)),
#'   n_participants = 1,
#'   non_compliant_sample_ratio = 0,
#'   missing_awakening_time_ratio = 0,
#'   missing_sampling_time_ratio = 0,
#'   validate = FALSE
#' )
#' unlink(output, recursive = TRUE)
#' @export
generate_synthetic_study_data <- function(output_dir, study_config = NULL, n_participants = NULL, non_compliant_sample_ratio = 0.10, missing_awakening_time_ratio = 0.01, missing_sampling_time_ratio = 0.02, random_state = 42L, create_cortisol_data = FALSE, overwrite = FALSE, validate = TRUE) {
  .assert_scalar_logical(overwrite, "overwrite"); .assert_scalar_logical(create_cortisol_data, "create_cortisol_data"); .assert_scalar_logical(validate, "validate")
  ratios <- c(non_compliant_sample_ratio, missing_awakening_time_ratio, missing_sampling_time_ratio)
  if (any(!is.finite(ratios)) || any(ratios < 0 | ratios > 1)) .carwatch_abort("Synthetic anomaly ratios must be finite numbers between zero and one.", "carwatch_value_error")
  if (!is.null(random_state)) random_state <- .synthetic_nonnegative_int(random_state, "random_state")
  if (is.null(random_state)) random_state <- sample.int(2147483647L, 1L) - 1L
  config <- .parse_synthetic_config(study_config, n_participants)
  output_dir <- fs::path_abs(output_dir)
  if (fs::dir_exists(output_dir) && length(fs::dir_ls(output_dir, all = TRUE)) && !overwrite) .carwatch_abort("Synthetic-study output directory exists; set `overwrite = TRUE` to replace it.", "carwatch_file_error")
  if (fs::dir_exists(output_dir) && overwrite) unlink(output_dir, recursive = TRUE, force = TRUE)
  fs::dir_create(output_dir, recurse = TRUE)
  participants <- sprintf(paste0(config$participant_prefix, "%0", max(2L, nchar(config$n_participants)), "d"), seq_len(config$n_participants))
  days <- list(); canonical_day <- 0L
  for (registration in seq_along(config$registrations)) for (registration_day in seq_len(config$registrations[[registration]]$study_days)) {
    canonical_day <- canonical_day + 1L
    days[[length(days) + 1L]] <- list(registration = registration, registration_day = registration_day, day = paste0("D", canonical_day), date = config$registrations[[registration]]$collection_dates[[registration_day]])
  }
  sample_count <- length(config$saliva_times) + length(config$saliva_absolute_times)
  sample_keys <- .synthetic_sample_keys(participants, days, sample_count)
  day_keys <- .synthetic_day_keys(participants, days)
  missing_day_count <- round(length(day_keys) * missing_awakening_time_ratio)
  if (missing_day_count && !length(config$saliva_times)) .carwatch_abort("`missing_awakening_time_ratio` requires at least one relative saliva distance.", "carwatch_value_error")
  missing_days <- .synthetic_select(day_keys, missing_day_count, .synthetic_seed(random_state, 20L))
  forced_missing <- if (length(missing_days)) paste(missing_days, 1L, sep = "\r") else character()
  missing_count <- round(length(sample_keys) * missing_sampling_time_ratio)
  if (missing_count < length(forced_missing)) .carwatch_abort("`missing_sampling_time_ratio` must cover first samples omitted for missing awakenings.", "carwatch_value_error")
  additional_missing <- .synthetic_select(setdiff(sample_keys, forced_missing), missing_count - length(forced_missing), .synthetic_seed(random_state, 21L))
  missing_samples <- c(forced_missing, additional_missing)
  non_compliant_count <- round(length(sample_keys) * non_compliant_sample_ratio)
  non_compliant <- .synthetic_select(sample_keys, non_compliant_count, .synthetic_seed(random_state, 10L))
  diary <- list(); profile <- .synthetic_cortisol_profile(length(config$saliva_times), length(config$saliva_absolute_times))
  timing_seed <- .synthetic_seed(random_state)
  timing <- function() {
  file_token <- gsub("[^A-Za-z0-9-]+", "-", config$filename_token)
  for (participant in participants) {
    folder <- fs::path(output_dir, "logs", participant); fs::dir_create(folder, recurse = TRUE)
    for (registration in seq_along(config$registrations)) {
      current <- config$registrations[[registration]]
      metadata <- jsonlite::toJSON(list(study_name = current$study_name, study_days = current$study_days, saliva_ids = current$saliva_ids, saliva_times = config$saliva_times, saliva_absolute_times = config$saliva_absolute_times, check_duplicates = config$check_duplicates, has_evening_salivette = config$has_evening_sample, enable_manual_scan = config$enable_manual_scan), auto_unbox = TRUE)
      timestamp <- as.POSIXct(paste(current$registration_date, "18:00:00"), tz = "Europe/Berlin")
      writeLines(sprintf("%.0f;local;study_metadata;%s", as.numeric(timestamp) * 1000, metadata), fs::path(folder, sprintf("carwatch_%s_%s_%s.csv", file_token, participant, format(current$registration_date, "%Y%m%d"))), useBytes = TRUE)
    }
    for (day_config in days) {
      current <- config$registrations[[day_config$registration]]
      key <- paste(participant, day_config$day, sep = "\r")
      date <- day_config$date
      wake <- as.POSIXct(paste(date, "00:00:00"), tz = "Europe/Berlin") + sample(330:509, 1) * 60 + sample(0:59, 1)
      has_awake <- !key %in% missing_days
      awakening_action <- if (stats::runif(1) < 0.35) "alarm_stop" else "spontaneous_awakening"
      entries <- if (has_awake) sprintf("%.0f;local;%s;{\"id\":-1}", as.numeric(wake) * 1000, awakening_action) else character()
      row <- list(participant = participant, day = day_config$day, date = format(date, "%Y-%m-%d"), awakening_time = format(wake, "%H:%M"))
      cursor <- wake
      for (position in seq_len(sample_count)) {
        sample_key <- paste(participant, day_config$day, position, sep = "\r")
        is_relative <- position <= length(config$saliva_times)
        if (is_relative) {
          interval <- config$saliva_times[[position]]
          target <- cursor + interval * 60
          sampled <- target + 60 * .synthetic_signed_deviation(sample_key %in% non_compliant, absolute = FALSE, relative_interval = interval, first_relative = position == 1L)
          cursor <- sampled
        } else {
          target <- as.POSIXct(paste(date, paste0(config$saliva_absolute_times[[position - length(config$saliva_times)]], ":00")), tz = "Europe/Berlin")
          sampled <- target + 60 * .synthetic_signed_deviation(sample_key %in% non_compliant, absolute = TRUE)
        }
        omitted <- sample_key %in% missing_samples
        row[[paste0("sampling_time_", position)]] <- format(sampled, "%H:%M")
        if (!omitted) {
          payload <- jsonlite::toJSON(list(id = position - 1L, saliva_id = position, barcode_value = .synthetic_barcode(participant, day_config$registration, position), day_scanned = day_config$registration_day, day_expected = day_config$registration_day, sample_scanned = current$saliva_ids[[position]], sample_expected = current$saliva_ids[[position]]), auto_unbox = TRUE)
          entries <- c(entries, sprintf("%.0f;local;barcode_scanned;%s", as.numeric(sampled) * 1000, payload))
        }
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
  if (create_cortisol_data) .write_synthetic_cortisol(fs::path(output_dir, "cortisol.csv"), config, participants, days, profile, random_state)
  readme <- c(paste0("# ", config$study_name), "", sprintf("Participants: %d", config$n_participants), sprintf("Canonical days: %d", length(days)), sprintf("Registrations: %s", paste(vapply(config$registrations, `[[`, character(1), "study_name"), collapse = ", ")), "", "Use read_raw_logs_from_participant_dirs(), convert_raw_logs(), read_conversion_report(), and read_manual_diary() to reconstruct this study.")
  writeLines(readme, fs::path(output_dir, "README.md"), useBytes = TRUE)
  if (validate) {
    final <- convert_raw_logs(read_raw_logs_from_participant_dirs(folders), errors = "raise", issue_decisions = decisions, manual_diary = read_manual_diary(fs::path(output_dir, "manual_diary.csv")))
    samples <- as_sample_events(final)
    actual_non_compliant <- sum(samples$sample_compliant %in% FALSE)
    if (actual_non_compliant != non_compliant_count) .carwatch_abort(sprintf("Final non-compliance count differs from the deterministic target: expected %d, found %d.", non_compliant_count, actual_non_compliant), "carwatch_runtime_error")
    if (sum(samples$sampling_time_source == "manual_diary", na.rm = TRUE) != missing_count) .carwatch_abort("Manual diary patch count does not match missing raw samples.", "carwatch_runtime_error")
  }
    output_dir
  }
  if (is.null(timing_seed)) timing() else withr::with_seed(timing_seed, timing())
}
