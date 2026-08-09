#' Read a BioPsyKit-style saliva CSV
#'
#' @param path CSV containing exactly `participant`, `sample`, and one biomarker.
#' @param saliva_type Biomarker column name.
#' @return A tibble.
#' @export
read_saliva <- function(path, saliva_type = "cortisol") {
  path <- .assert_file(path, "csv")
  if (!is.character(saliva_type) || length(saliva_type) != 1L || !nzchar(trimws(saliva_type))) .carwatch_abort("`saliva_type` must be a non-empty string.", "carwatch_value_error")
  saliva_type <- trimws(saliva_type)
  data <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = character(), show_col_types = FALSE)
  if (!nrow(data)) .carwatch_abort("Saliva file does not contain any measurements.", "carwatch_schema_error")
  expected <- c("participant", "sample", saliva_type)
  if (!setequal(names(data), expected)) .carwatch_abort(sprintf("Saliva data must contain exactly: %s.", paste(expected, collapse = ", ")), "carwatch_schema_error")
  data <- dplyr::select(data, dplyr::all_of(expected))
  data$participant <- .as_character_id(data$participant, "Saliva participant IDs")
  data$sample <- .as_character_id(data$sample, "Saliva sample IDs")
  values <- suppressWarnings(as.numeric(data[[saliva_type]]))
  invalid <- data[[saliva_type]] != "" & is.na(values)
  if (any(invalid)) .carwatch_abort(sprintf("Column `%s` contains non-numeric measurements.", saliva_type), "carwatch_schema_error")
  data[[saliva_type]] <- values
  if (anyDuplicated(data[c("participant", "sample")])) .carwatch_abort("Saliva data contain duplicate participant/sample pairs.", "carwatch_schema_error")
  data
}

#' Read a manual measurement diary
#'
#' @param path Wide CSV diary.
#' @return A tibble keyed by participant and canonical day.
#' @export
read_manual_diary <- function(path) {
  path <- .assert_file(path, "csv")
  data <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = character(), show_col_types = FALSE)
  required <- c("participant", "day", "date", "awakening_time")
  sampling <- grep("^sampling_time_[1-9][0-9]*$", names(data), value = TRUE)
  if (!setequal(names(data), c(required, sampling))) .carwatch_abort("Manual diary must use participant, day, date, awakening_time, and optional contiguous sampling_time_ columns.", "carwatch_schema_error")
  positions <- as.integer(sub("^sampling_time_", "", sampling))
  if (length(positions) && !identical(sort(positions), seq_len(max(positions)))) .carwatch_abort("Manual diary sampling-time columns must be contiguous and start at 1.", "carwatch_schema_error")
  data$participant <- .as_character_id(data$participant, "Manual diary participant IDs")
  data$day <- .as_character_id(data$day, "Manual diary day IDs")
  if (anyDuplicated(data[c("participant", "day")])) .carwatch_abort("Manual diary contains duplicate participant/day rows.", "carwatch_schema_error")
  if (any(!grepl("^D[1-9][0-9]*$", data$day))) .carwatch_abort("Manual diary days must use canonical identifiers such as D1.", "carwatch_schema_error")
  if (any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", data$date))) .carwatch_abort("Manual diary dates must use YYYY-MM-DD.", "carwatch_schema_error")
  time_columns <- c("awakening_time", sampling)
  for (column in time_columns) {
    non_empty <- data[[column]] != ""
    if (any(non_empty & !grepl("^\\d{2}:\\d{2}$", data[[column]]))) .carwatch_abort(sprintf("Manual diary column `%s` must use HH:MM.", column), "carwatch_schema_error")
    data[[column]][!non_empty] <- NA_character_
    data[[column]] <- ifelse(is.na(data[[column]]), NA_character_, paste(data$date, data[[column]]))
  }
  data
}

#' Read CARWatch raw log files
#'
#' @param path CSV file, ZIP archive, directory, or vector of paths.
#' @param tz IANA timezone for Unix timestamps.
#' @param errors Invalid-payload handling: `"error"`, `"warn"`, or `"ignore"`.
#' @return A tibble of immutable raw events.
#' @export
read_raw_logs <- function(path, tz = "Europe/Berlin", errors = c("raise", "warn", "ignore", "error")) {
  errors <- match.arg(errors); if (identical(errors, "raise")) errors <- "error"
  paths <- fs::path_abs(path)
  if (!length(paths) || any(!fs::file_exists(paths) & !fs::dir_exists(paths))) .carwatch_abort("Every raw-log path must exist.", "carwatch_file_error")
  files <- unlist(lapply(paths, function(current) if (fs::dir_exists(current)) fs::dir_ls(current, recurse = FALSE, type = "file") else current), use.names = FALSE)
  files <- files[!grepl("/(\\.|~)", files)]
  rows <- unlist(lapply(files, .read_raw_log_path, tz = tz, errors = errors), recursive = FALSE)
  if (!length(rows)) return(tibble::tibble(participant = character(), date = as.Date(character()), timestamp = as.POSIXct(character()), timestamp_ms = numeric(), action = character(), payload = list(), source_file = character()))
  data <- dplyr::bind_rows(rows) |>
    dplyr::arrange(.data$participant, .data$date, .data$timestamp, .data$source_file)
  data
}

#' Read raw logs from one explicitly mapped folder per participant
#' @param participant_dirs Named paths, one folder per participant.
#' @param tz IANA study timezone.
#' @param errors Invalid-payload handling.
#' @param create_report Whether to return the source audit.
#' @return Raw events, or a raw-events/source-audit list.
#' @export
read_raw_logs_from_participant_dirs <- function(participant_dirs, tz = "Europe/Berlin", errors = c("raise", "warn", "ignore", "error"), create_report = FALSE) {
  errors <- match.arg(errors); if (identical(errors, "raise")) errors <- "error"; .assert_scalar_logical(create_report, "create_report")
  if (!is.list(participant_dirs) && !is.atomic(participant_dirs)) .carwatch_abort("`participant_dirs` must be a named vector or list.", "carwatch_type_error")
  if (!length(participant_dirs) || is.null(names(participant_dirs)) || any(!nzchar(trimws(names(participant_dirs))))) .carwatch_abort("`participant_dirs` must contain named, non-empty participant identifiers.", "carwatch_type_error")
  identifiers <- trimws(names(participant_dirs))
  if (anyDuplicated(tolower(identifiers))) .carwatch_abort("Participant identifiers must be unique case-insensitively.", "carwatch_value_error")
  selected <- list(); audit <- list()
  for (participant in sort(identifiers, method = "radix")) {
    folder <- fs::path_abs(participant_dirs[[match(participant, identifiers)]])
    if (!fs::dir_exists(folder)) .carwatch_abort(sprintf("Participant folder does not exist: participant=%s, folder=%s.", participant, folder), "carwatch_file_error")
    files <- sort(fs::dir_ls(folder, recurse = TRUE, type = "file", regexp = "\\.(csv|zip)$"), method = "radix")
    files <- files[!vapply(fs::path_rel(files, start = folder), .is_hidden_path, logical(1))]
    csvs <- files[tolower(fs::path_ext(files)) == "csv"]
    zips <- files[tolower(fs::path_ext(files)) == "zip"]
    extracted <- lapply(csvs, function(source) .selected_raw_source(participant, folder, source))
    extracted <- Filter(Negate(is.null), extracted)
    for (source in csvs) if (is.null(.selected_raw_source(participant, folder, source))) audit[[length(audit) + 1L]] <- .raw_source_audit(participant, folder, source, reason = .source_exclusion_reason(participant, fs::path_file(source)))
    if (length(extracted)) {
      choice <- .deduplicate_selected_sources(extracted, participant, folder, "selected_extracted_csv")
      selected <- c(selected, choice$selected); audit <- c(audit, choice$audit)
      for (source in zips) audit[[length(audit) + 1L]] <- .raw_source_audit(participant, folder, source, reason = "zip_not_needed")
    } else {
      members <- unlist(lapply(zips, function(source) {
        listed <- utils::unzip(source, list = TRUE)$Name
        listed <- listed[grepl("\\.csv$", listed, ignore.case = TRUE) & !vapply(listed, .is_hidden_path, logical(1))]
        lapply(sort(listed, method = "radix"), function(member) .selected_raw_source(participant, folder, source, member))
      }), recursive = FALSE)
      members <- Filter(Negate(is.null), members)
      for (source in zips) {
        listed <- utils::unzip(source, list = TRUE)$Name
        for (member in listed[grepl("\\.csv$", listed, ignore.case = TRUE) & !vapply(listed, .is_hidden_path, logical(1))]) if (is.null(.selected_raw_source(participant, folder, source, member))) audit[[length(audit) + 1L]] <- .raw_source_audit(participant, folder, source, archive_member = member, reason = .source_exclusion_reason(participant, fs::path_file(member)))
      }
      choice <- .deduplicate_selected_sources(members, participant, folder, "selected_zip_fallback")
      selected <- c(selected, choice$selected); audit <- c(audit, choice$audit)
    }
    if (!length(Filter(function(x) identical(x$participant, participant), selected))) .carwatch_abort(sprintf("No matching combined CARWatch export found for participant `%s`.", participant), "carwatch_file_error")
  }
  rows <- unlist(lapply(selected, function(source) .read_raw_log_path(source$source, tz, errors, source$archive_member)), recursive = FALSE)
  logs <- if (length(rows)) dplyr::bind_rows(rows) |> dplyr::arrange(.data$participant, .data$date, .data$timestamp, .data$source_file) else tibble::tibble(participant = character(), date = as.Date(character()), timestamp = as.POSIXct(character()), timestamp_ms = numeric(), action = character(), payload = list(), source_file = character())
  if (!create_report) return(logs)
  report <- dplyr::bind_rows(audit)
  if (!nrow(report)) report <- tibble::tibble(participant = character(), participant_folder = character(), source = character(), archive_member = character(), logical_source_file = character(), raw_event_count = integer(), status = character(), reason = character())
  counts <- dplyr::count(logs, .data$source_file, name = "raw_event_count")
  report$raw_event_count <- counts$raw_event_count[match(ifelse(is.na(report$archive_member), fs::path_file(report$source), paste0(fs::path_file(report$source), "!", report$archive_member)), counts$source_file)]
  report$raw_event_count[report$status != "selected"] <- NA_integer_
  report <- dplyr::arrange(report, .data$participant, .data$status, .data$source, .data$archive_member)
  list(raw_logs = logs, source_audit = report)
}

.is_hidden_path <- function(path) any(startsWith(strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]], ".") | strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]] == "__MACOSX")

.raw_filename_metadata <- function(source_file) {
  file_name <- fs::path_file(sub("^.*!", "", source_file))
  stem <- sub("(?i)^carwatch_", "", sub("(?i)\\.csv$", "", file_name, perl = TRUE), perl = TRUE)
  tokens <- strsplit(stem, "_", fixed = TRUE)[[1]]
  has_date <- length(tokens) && grepl("^[0-9]{8}$", tokens[[length(tokens)]])
  content <- if (has_date) tokens[-length(tokens)] else tokens
  participant <- if (length(content) >= 2L) paste(content[-1L], collapse = "_") else if (length(content)) content[[1]] else NA_character_
  list(participant = participant, date = if (has_date) tokens[[length(tokens)]] else NA_character_)
}

.source_exclusion_reason <- function(participant, logical_source_file) {
  detected <- .raw_filename_metadata(logical_source_file)$participant
  if (is.na(detected) || !nzchar(detected) || !grepl("_", sub("(?i)^carwatch_", "", logical_source_file, perl = TRUE))) "missing_participant_identifier" else if (!identical(tolower(detected), tolower(participant))) "participant_mismatch" else "excluded"
}

.selected_raw_source <- function(participant, folder, source, archive_member = NA_character_) {
  logical <- fs::path_file(ifelse(is.na(archive_member), source, archive_member))
  if (.source_exclusion_reason(participant, logical) != "excluded") return(NULL)
  list(participant = participant, folder = folder, source = source, archive_member = archive_member, logical_source_file = logical)
}

.raw_source_audit <- function(participant, folder, source, archive_member = NA_character_, logical_source_file = NA_character_, status = "excluded", reason) tibble::tibble(participant = participant, participant_folder = folder, source = source, archive_member = archive_member, logical_source_file = logical_source_file, raw_event_count = NA_integer_, status = status, reason = reason)

.deduplicate_selected_sources <- function(sources, participant, folder, selected_reason) {
  selected <- list(); audit <- list(); seen <- character()
  sources <- sources[order(tolower(vapply(sources, `[[`, character(1), "logical_source_file")), tolower(vapply(sources, `[[`, character(1), "source")), tolower(vapply(sources, `[[`, character(1), "archive_member")), na.last = TRUE)]
  for (source in sources) {
    duplicate <- tolower(source$logical_source_file) %in% seen
    if (!duplicate) { seen <- c(seen, tolower(source$logical_source_file)); selected[[length(selected) + 1L]] <- source }
    audit[[length(audit) + 1L]] <- .raw_source_audit(participant, folder, source$source, source$archive_member, source$logical_source_file, if (duplicate) "excluded" else "selected", if (duplicate) "duplicate_logical_source" else selected_reason)
  }
  list(selected = selected, audit = audit)
}

.read_raw_log_path <- function(path, tz, errors, archive_member = NA_character_) {
  extension <- tolower(fs::path_ext(path))
  if (!is.na(archive_member)) {
    con <- unz(path, archive_member, open = "rt"); on.exit(close(con), add = TRUE)
    return(.parse_raw_log_text(readLines(con, warn = FALSE), paste0(fs::path_file(path), "!", archive_member), tz, errors))
  }
  if (extension == "zip") {
    members <- utils::unzip(path, list = TRUE)$Name
    members <- members[grepl("\\.csv$", members, ignore.case = TRUE) & !grepl("(^|/)\\.", members)]
    return(unlist(lapply(members, function(member) {
      con <- unz(path, member, open = "rt")
      on.exit(close(con), add = TRUE)
      .parse_raw_log_text(readLines(con, warn = FALSE), paste0(fs::path_file(path), "!", member), tz, errors)
    }), recursive = FALSE))
  }
  if (extension != "csv") .carwatch_abort(sprintf("Raw log source must be CSV or ZIP: %s", path), "carwatch_value_error")
  .parse_raw_log_text(readLines(path, warn = FALSE), fs::path_file(path), tz, errors)
}

.parse_raw_log_text <- function(lines, source_file, tz, errors) {
  if (!length(lines)) return(list())
  metadata <- .raw_filename_metadata(source_file)
  tokens <- strsplit(fs::path_file(sub("^.*!", "", source_file)), "_", fixed = TRUE)[[1]]
  date_token <- metadata$date
  participant <- metadata$participant
  if (!nzchar(date_token) || is.na(participant) || !nzchar(participant)) .carwatch_abort(sprintf("Could not determine participant and date from raw-log filename: %s", source_file), "carwatch_schema_error")
  date <- as.Date(date_token, "%Y%m%d")
  starts <- grep("^[0-9]+;", lines)
  if (!length(starts)) .carwatch_abort(sprintf("No log entries found in %s.", source_file), "carwatch_parse_error")
  ends <- c(starts[-1L] - 1L, length(lines))
  entries <- vapply(seq_along(starts), function(i) paste(lines[starts[[i]]:ends[[i]]], collapse = "\n"), character(1))
  parsed <- lapply(entries, function(entry) {
    fields <- strsplit(trimws(entry), ";", fixed = TRUE)[[1]]
    if (length(fields) < 3L) .carwatch_abort(sprintf("Invalid raw-log entry in %s.", source_file), "carwatch_parse_error")
    timestamp_ms <- suppressWarnings(as.numeric(fields[[1]]))
    if (length(fields) >= 4L) { action <- fields[[3]]; payload_text <- paste(fields[-c(1, 2, 3)], collapse = ";") } else { action <- fields[[2]]; payload_text <- paste(fields[-c(1, 2)], collapse = ";") }
    payload <- tryCatch(jsonlite::fromJSON(payload_text, simplifyVector = FALSE), error = function(error) error)
    if (inherits(payload, "error")) {
      if (errors == "error") .carwatch_abort(sprintf("Invalid JSON payload in %s: %s", source_file, conditionMessage(payload)), "carwatch_parse_error")
      if (errors == "warn") rlang::warn(sprintf("Invalid JSON payload retained in %s.", source_file), class = "carwatch_parse_warning")
      payload <- list(`_invalid_json` = payload_text)
    }
    timestamp <- as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = tz)
    tibble::tibble(participant = participant, date = date, timestamp = timestamp, timestamp_ms = timestamp_ms, action = action, payload = list(payload), source_file = source_file)
  })
  timestamps <- vapply(parsed, function(row) row$timestamp_ms[[1]], numeric(1))
  if (length(timestamps) > 1L && any(diff(timestamps) < 0) && errors != "ignore") {
    rlang::warn(sprintf("Timestamps are not monotonically increasing in %s; events are retained and will be sorted during conversion.", source_file), class = "carwatch_parse_warning")
  }
  parsed
}

#' Compactly summarize a source audit
#' @param source_audit Source audit returned by `read_raw_logs_from_participant_dirs()`.
#' @export
summarize_source_audit <- function(source_audit) {
  .require_columns(source_audit, c("participant", "status", "raw_event_count"), "Source audit")
  selected <- source_audit$status == "selected"
  tibble::tibble(raw_log_import = c(sum(selected), sum(source_audit$raw_event_count[selected], na.rm = TRUE), dplyr::n_distinct(source_audit$participant)), .name_repair = "minimal") |>
    stats::setNames("raw_log_import")
}
