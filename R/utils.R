#' Package namespace declarations
#'
#' @importFrom rlang :=
#' @importFrom stats rlnorm
#' @importFrom utils head tail
#' @keywords internal
"_PACKAGE"

.carwatch_abort <- function(message, class = "carwatch_error") {
  rlang::abort(message, class = c(class, "carwatch_error"))
}

utils::globalVariables(c(".data", ".env"))

`%||%` <- function(x, y) if (is.null(x)) y else x

.timezone_or_default <- function(value, default = "Europe/Berlin") {
  zone <- attr(value, "tzone")
  if (is.null(zone) || !length(zone) || is.na(zone[[1]]) || !nzchar(zone[[1]])) default else zone[[1]]
}

.assert_scalar_logical <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .carwatch_abort(sprintf("`%s` must be TRUE or FALSE.", name), "carwatch_type_error")
  }
}

.assert_choice <- function(value, choices, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% choices) {
    .carwatch_abort(
      sprintf("`%s` must be one of: %s.", name, paste(sprintf("'%s'", choices), collapse = ", ")),
      "carwatch_value_error"
    )
  }
}

.assert_file <- function(path, extension = NULL) {
  path <- fs::path_abs(path)
  if (!fs::file_exists(path)) {
    .carwatch_abort(sprintf("File does not exist: %s", path), "carwatch_file_error")
  }
  if (!is.null(extension) && tolower(fs::path_ext(path)) != tolower(extension)) {
    .carwatch_abort(sprintf("Expected a .%s file: %s", extension, path), "carwatch_value_error")
  }
  path
}

.as_character_id <- function(x, name) {
  value <- trimws(as.character(x))
  if (anyNA(value) || any(value == "")) {
    .carwatch_abort(sprintf("%s must not contain missing or empty values.", name), "carwatch_schema_error")
  }
  value
}

.parse_local_time <- function(x, tz, name = "timestamp", require_midnight = FALSE) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz)) {
    .carwatch_abort("`tz` must be one IANA time-zone name.", "carwatch_type_error")
  }
  parsed <- tryCatch(
    clock::date_time_parse(
      as.character(x),
      zone = tz,
      format = "%Y-%m-%d %H:%M:%S",
      nonexistent = "error",
      ambiguous = "error"
    ),
    error = function(error) .carwatch_abort(
      sprintf("Invalid, ambiguous, or nonexistent %s in timezone %s: %s", name, tz, conditionMessage(error)),
      "carwatch_schema_error"
    )
  )
  if (require_midnight && any(!is.na(parsed) & format(parsed, "%H:%M:%S", tz = tz) != "00:00:00")) {
    .carwatch_abort(sprintf("%s values must be local midnight.", name), "carwatch_schema_error")
  }
  result <- as.POSIXct(parsed)
  attr(result, "tzone") <- tz
  result
}

.natural_order <- function(x) {
  key <- vapply(as.character(x), function(value) {
    tokens <- strsplit(tolower(value), "(?<=[^0-9])(?=[0-9])|(?<=[0-9])(?=[^0-9])", perl = TRUE)[[1]]
    paste(vapply(tokens, function(token) {
      if (grepl("^[0-9]+$", token)) sprintf("%020d", suppressWarnings(as.numeric(token))) else token
    }, character(1)), collapse = "")
  }, character(1))
  order(key, as.character(x), method = "radix", na.last = TRUE)
}

.is_false <- function(x) is.logical(x) && !is.na(x) && !x

.require_columns <- function(data, columns, context) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    .carwatch_abort(
      sprintf("%s is missing required columns: %s.", context, paste(sprintf("'%s'", missing), collapse = ", ")),
      "carwatch_schema_error"
    )
  }
}

.require_complete_results <- function(data) {
  if (!inherits(data, "carwatch_results")) {
    .carwatch_abort("Expected a `carwatch_results` object.", "carwatch_type_error")
  }
  if (isTRUE(attr(data, "carwatch_display_only"))) {
    .carwatch_abort(
      "Display-only results cannot be used for this operation. Reload with `simple = FALSE`.",
      "carwatch_schema_error"
    )
  }
  data
}
