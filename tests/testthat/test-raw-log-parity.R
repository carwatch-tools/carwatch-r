.raw_log_path <- function(lines) {
  root <- tempfile("carwatch-log-"); dir.create(root)
  path <- file.path(root, "carwatch_demo_01_20250515.csv")
  writeLines(lines, path)
  path
}

test_that("conversion issue identity excludes wording and proposed action", {
  reports <- list(carwatch:::.new_conversion_report(tibble::tibble(participant = character(), source_file = character())), carwatch:::.new_conversion_report(tibble::tibble(participant = character(), source_file = character())))
  first <- carwatch:::.report_add_issue(reports[[1]], code = "missing_scheduled_sample_event", participant = "01", registration = 1L, day = "D1", sample_position = 1L, sample_id = "tube-x", message = "First message.", details = list(source = "same"), proposed_action = "first_action")
  second <- carwatch:::.report_add_issue(reports[[2]], code = "missing_scheduled_sample_event", participant = "01", registration = 1L, day = "D1", sample_position = 1L, sample_id = "tube-x", message = "Second message.", details = list(source = "same"), proposed_action = "second_action")
  expect_identical(first$issue_id, second$issue_id)
  expect_identical(carwatch:::.report_add_issue(second$report, code = "example", message = "First line.\nSecond line.", proposed_action = "keep")$report$issues$message[[2]], "First line. Second line.")
})

test_that("raw-log reader accepts current, multiline, and legacy exports", {
  current <- read_raw_logs(.raw_log_path(c(
    '1747282410799;local;spontaneous_awakening;{"id":0}',
    '1747282435999;local;barcode_scanned;{"id":0,"saliva_id":100,"barcode_value":"0010101","day_scanned":1,"day_expected":1,"sample_scanned":"B1","sample_expected":"B1"}'
  )))
  expect_identical(current$participant, c("01", "01"))
  expect_identical(current$payload[[2]]$barcode_value, "0010101")
  multiline <- read_raw_logs(.raw_log_path(c('1776429093567;local;spontaneous_awakening;{', '  "id" : -1', '}')))
  expect_equal(multiline$payload[[1]]$id, -1)
  legacy <- read_raw_logs(.raw_log_path('1747282410799;spontaneous_awakening;{"id":0}'))
  expect_identical(legacy$action, "spontaneous_awakening")
})

test_that("raw-log reader exposes invalid JSON modes and ignores hidden ZIP members", {
  path <- .raw_log_path('1747282410799;local;spontaneous_awakening;{invalid}')
  expect_error(read_raw_logs(path), "Invalid JSON")
  expect_warning(warned <- read_raw_logs(path, errors = "warn"), "Invalid JSON")
  expect_identical(warned$payload[[1]]$`_invalid_json`, "{invalid}")
  root <- tempfile("carwatch-zip-"); dir.create(root)
  visible <- file.path(root, "carwatch_demo_01_20250515.csv")
  hidden <- file.path(root, ".hidden.csv")
  writeLines('1747282410799;local;spontaneous_awakening;{"id":0}', visible)
  writeLines('1747282410799;local;spontaneous_awakening;{"id":99}', hidden)
  archive <- file.path(root, "logs.zip")
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(root); utils::zip(archive, c(basename(visible), basename(hidden)), flags = "-q")
  loaded <- read_raw_logs(archive)
  expect_equal(loaded$payload[[1]]$id, 0)
})
