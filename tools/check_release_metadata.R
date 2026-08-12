args <- commandArgs(trailingOnly = TRUE)
tag <- if (length(args) > 0L && nzchar(args[[1L]])) args[[1L]] else NULL

description <- read.dcf("DESCRIPTION")
version <- unname(description[1L, "Version"])

if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version)) {
  stop(
    "DESCRIPTION must contain a release version such as 1.0.0; found ",
    sQuote(version),
    call. = FALSE
  )
}

news <- readLines("NEWS.md", warn = FALSE)
expected_news_heading <- paste("# carwatch", version)
if (!length(news) || !identical(news[[1L]], expected_news_heading)) {
  stop(
    "NEWS.md must start with ", sQuote(expected_news_heading),
    call. = FALSE
  )
}

citation <- readLines("CITATION.cff", warn = FALSE)
citation_version <- sub(
  "^version:[[:space:]]*['\"]?([^'\"]+)['\"]?[[:space:]]*$",
  "\\1",
  grep("^version:", citation, value = TRUE)[[1L]]
)
if (!identical(citation_version, version)) {
  stop(
    "CITATION.cff version ", sQuote(citation_version),
    " does not match DESCRIPTION version ", sQuote(version),
    call. = FALSE
  )
}

if (!is.null(tag)) {
  expected_tag <- paste0("v", version)
  if (!identical(tag, expected_tag)) {
    stop(
      "Release tag must be ", sQuote(expected_tag),
      " for DESCRIPTION version ", sQuote(version),
      "; found ", sQuote(tag),
      call. = FALSE
    )
  }
}

message("Release metadata are consistent for carwatch ", version, ".")
