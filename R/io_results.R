.result_timestamp_variables <- c("date", "awakening_time", "sampling_time", "scheduled_sampling_time")
.result_integer_variables <- c("registration", "registration_day", "sample_position", "day_expected", "day_scanned", "expected_sample_count", "recorded_sample_count", "assessed_sample_count", "compliant_sample_count")
.result_double_variables <- c("expected_interval_min", "actual_interval_min", "time_deviation_min")
.result_logical_variables <- c("possible_reregistration", "day_compliant", "sample_compliant", "sample_mismatch")

.read_result_cells <- function(path) {
  raw <- readr::read_csv(path, col_names = FALSE, col_types = readr::cols(.default = readr::col_character()), na = character(), name_repair = "minimal", show_col_types = FALSE)
  if (nrow(raw) < 5L || ncol(raw) < 2L) {
    .carwatch_abort("Saved Study Results require three header rows and participant data.", "carwatch_schema_error")
  }
  as.matrix(raw)
}

.parse_result_column <- function(values, variable, tz) {
  values[values == ""] <- NA_character_
  if (variable %in% .result_timestamp_variables) {
    result <- rep(as.POSIXct(NA), length(values))
    non_missing <- !is.na(values)
    if (any(non_missing)) result[non_missing] <- .parse_local_time(values[non_missing], tz, variable, require_midnight = identical(variable, "date"))
    return(result)
  }
  if (variable %in% .result_integer_variables) {
    numeric <- suppressWarnings(as.numeric(values))
    if (any(!is.na(values) & (is.na(numeric) | numeric != floor(numeric)))) {
      .carwatch_abort(sprintf("Invalid integer values for `%s`.", variable), "carwatch_schema_error")
    }
    return(as.integer(numeric))
  }
  if (variable %in% .result_double_variables) {
    numeric <- suppressWarnings(as.numeric(values))
    if (any(!is.na(values) & is.na(numeric))) .carwatch_abort(sprintf("Invalid numeric values for `%s`.", variable), "carwatch_schema_error")
    return(numeric)
  }
  if (variable %in% .result_logical_variables) {
    normalized <- tolower(values)
    if (any(!is.na(normalized) & !normalized %in% c("true", "false"))) .carwatch_abort(sprintf("Invalid boolean values for `%s`.", variable), "carwatch_schema_error")
    return(ifelse(is.na(normalized), NA, normalized == "true"))
  }
  non_missing <- values[!is.na(values)]
  normalized <- tolower(non_missing)
  if (length(non_missing) && all(normalized %in% c("true", "false"))) return(ifelse(is.na(values), NA, tolower(values) == "true"))
  numeric <- suppressWarnings(as.numeric(values))
  if (length(non_missing) && all(!is.na(numeric[!is.na(values)]))) return(numeric)
  values
}

#' Read canonical CARWatch Study Results
#'
#' Reads the three-header CSV produced by either `carwatch-python` or
#' `write_study_results()`. The returned object preserves opaque identifiers,
#' time zones, and column placement.
#'
#' @param path Path to a three-header Study Results CSV.
#' @param tz IANA study timezone.
#' @param simple Return a display-only subset.
#' @return A `carwatch_results` object.
#' @export
read_study_results <- function(path, tz = "Europe/Berlin", simple = FALSE) {
  .assert_scalar_logical(simple, "simple")
  path <- .assert_file(path, "csv")
  cells <- .read_result_cells(path)
  if (cells[1, 1] != "day" || cells[2, 1] != "sample" || cells[3, 1] != "variable" || cells[4, 1] != "participant") {
    .carwatch_abort("Saved Study Results require header rows named 'day', 'sample', 'variable', and 'participant'.", "carwatch_schema_error")
  }
  spec <- tibble::tibble(
    name = paste0("v", seq_len(ncol(cells) - 1L)),
    day = cells[1, -1],
    sample = cells[2, -1],
    variable = cells[3, -1]
  )
  if (any(spec$day == "" | spec$sample == "" | spec$variable == "")) .carwatch_abort("Study Results headers must not be empty.", "carwatch_schema_error")
  values <- tibble::as_tibble(as.data.frame(cells[-(1:4), , drop = FALSE], stringsAsFactors = FALSE), .name_repair = "minimal")
  names(values) <- c("participant", spec$name)
  values$participant <- .as_character_id(values$participant, "Participant IDs")
  if (anyDuplicated(values$participant)) .carwatch_abort("Saved Study Results contain duplicate participant IDs.", "carwatch_schema_error")
  for (i in seq_len(nrow(spec))) values[[spec$name[[i]]]] <- .parse_result_column(values[[spec$name[[i]]]], spec$variable[[i]], tz)
  result <- new_carwatch_results(values, spec)
  if (!simple) return(result)
  day_variables <- c("date", "awakening_time", "awakening_type", "mismatch_summary", "day_compliant")
  sample_variables <- c("sampling_time", "recorded_sample", "sample_position", "sample_compliant")
  known <- unique(c(.result_timestamp_variables, .result_integer_variables, .result_double_variables, .result_logical_variables, "registration_sources", "study_name", "awakening_type", "mismatch_summary", "barcode", "sampling_time_source", "recorded_sample", "schedule_type", "non_compliant_samples"))
  keep <- (spec$sample == "day" & spec$variable %in% day_variables) | (spec$sample != "day" & spec$variable %in% sample_variables) | !spec$variable %in% known
  new_carwatch_results(dplyr::select(result, dplyr::all_of(c("participant", spec$name[keep]))), spec[keep, ], display_only = TRUE)
}

#' Write canonical CARWatch Study Results
#'
#' @param data Complete `carwatch_results` object.
#' @param path Destination CSV path.
#' @export
write_study_results <- function(data, path) {
  .require_complete_results(data)
  path <- fs::path_abs(path)
  if (tolower(fs::path_ext(path)) != "csv") .carwatch_abort("Study Results must be saved as a CSV file.", "carwatch_value_error")
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  spec <- attr(data, "column_spec")
  rendered <- as.data.frame(data, stringsAsFactors = FALSE)
  for (name in spec$name) {
    value <- rendered[[name]]
    if (inherits(value, "POSIXt")) value <- format(value, "%Y-%m-%d %H:%M:%S%z", tz = attr(value, "tzone") %||% "Europe/Berlin")
    rendered[[name]] <- ifelse(is.na(value), "", as.character(value))
  }
  rendered$participant <- .as_character_id(rendered$participant, "Participant IDs")
  header <- rbind(c("day", spec$day), c("sample", spec$sample), c("variable", spec$variable), c("participant", rep("", nrow(spec))))
  output <- rbind(header, as.matrix(rendered[, c("participant", spec$name), drop = FALSE]))
  readr::write_csv(as.data.frame(output, stringsAsFactors = FALSE), path, col_names = FALSE, na = "")
  invisible(path)
}

#' Read a flat CARWatch Study Manager export
#'
#' @param path Path to a Study Manager export.
#' @param tz IANA study timezone.
#' @return Complete `carwatch_results`.
#' @export
read_study_manager_export <- function(path, tz = "Europe/Berlin") {
  path <- .assert_file(path, "csv")
  raw <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = character(), show_col_types = FALSE)
  if (!nrow(raw)) .carwatch_abort("Study Manager export does not contain any participants.", "carwatch_schema_error")
  lowered <- tolower(names(raw))
  if (anyDuplicated(lowered)) .carwatch_abort("Study Manager export contains columns that differ only by case.", "carwatch_schema_error")
  participant_name <- names(raw)[lowered == "participant id"]
  if (length(participant_name) != 1L) .carwatch_abort("Study Manager export requires a 'Participant ID' column.", "carwatch_schema_error")
  participant <- .as_character_id(raw[[participant_name]], "Participant IDs")
  if (anyDuplicated(participant)) .carwatch_abort("Study Manager export contains duplicate participant IDs.", "carwatch_schema_error")
  date_columns <- grep("^date_d[0-9]+$", names(raw), ignore.case = TRUE, value = TRUE)
  if (!length(date_columns)) .carwatch_abort("Study Manager export does not contain any `date_D*` columns.", "carwatch_schema_error")
  days <- sort(unique(as.integer(sub("(?i)^date_d", "", date_columns, perl = TRUE))))
  sample_columns <- grep("^(sampling_time|sample_barcode|sample_scanned)_d[0-9]+_.+$", names(raw), ignore.case = TRUE, value = TRUE)
  if (!length(sample_columns)) .carwatch_abort("Study Manager export does not contain any sample columns.", "carwatch_schema_error")
  parse_sample <- function(name) {
    match <- regexec("(?i)^(sampling_time|sample_barcode|sample_scanned)_d([0-9]+)_(.+)$", name, perl = TRUE)
    values <- regmatches(name, match)[[1]]
    list(field = tolower(values[[2]]), day = as.integer(values[[3]]), sample = values[[4]])
  }
  layout <- lapply(sample_columns, parse_sample)
  sample_days <- unique(vapply(layout, `[[`, integer(1), "day"))
  missing_dates <- setdiff(sample_days, days)
  if (length(missing_dates)) .carwatch_abort(sprintf("Study Manager export contains samples without matching dates for days: %s.", paste(missing_dates, collapse = ", ")), "carwatch_schema_error")
  long <- list()
  column <- function(name) names(raw)[match(tolower(name), lowered)]
  value <- function(row, name) { found <- column(name); if (is.na(found)) NA_character_ else { item <- trimws(raw[[found]][[row]]); if (identical(item, "")) NA_character_ else item } }
  date_time <- function(date, time, name) {
    if (is.na(date) || is.na(time)) return(as.POSIXct(NA))
    .parse_local_time(paste(date, time), tz, name)
  }
  date_value <- function(text) if (is.na(text)) as.POSIXct(NA) else .parse_local_time(paste(text, "00:00:00"), tz, "study date", require_midnight = TRUE)
  for (row in seq_len(nrow(raw))) for (number in days) {
    day <- paste0("D", number); date <- value(row, paste0("date_d", number)); parsed_date <- date_value(date)
    add <- function(sample, variable, item) long[[length(long) + 1L]] <<- tibble::tibble(participant = participant[[row]], day = day, sample = sample, variable = variable, value = list(item))
    add("day", "date", parsed_date)
    add("day", "awakening_time", date_time(date, value(row, paste0("awakening_time_d", number, "_app")), "awakening time"))
    awakening_type <- value(row, paste0("awakening_type_d", number)); if (!is.na(awakening_type)) awakening_type <- switch(awakening_type, "self-report" = "spontaneous_awakening", "manual" = "manual_diary", awakening_type)
    add("day", "awakening_type", awakening_type)
    add("day", "mismatch_summary", value(row, paste0("sample_mismatches_d", number)))
    samples <- unique(vapply(Filter(function(item) item$day == number, layout), `[[`, character(1), "sample"))
    samples <- samples[.natural_order(samples)]
    for (position in seq_along(samples)) {
      sample <- samples[[position]]
      add(sample, "sampling_time", date_time(date, value(row, paste0("sampling_time_d", number, "_", sample)), "sampling time"))
      add(sample, "barcode", value(row, paste0("sample_barcode_d", number, "_", sample)))
      add(sample, "recorded_sample", value(row, paste0("sample_scanned_d", number, "_", sample)))
      add(sample, "sample_position", as.integer(position))
    }
  }
  .from_long_results(dplyr::bind_rows(long), participant)
}
