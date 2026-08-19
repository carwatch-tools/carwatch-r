test_that("interactive sampling timeline creates a Shiny app", {
  skip_if_not_installed("shiny")
  samples <- tibble::tibble(
    participant = c("p1", "p1", "p2"), day = c("D1", "D2", "D1"), sample = "S1", sample_position = 1L,
    sampling_time = as.POSIXct(c("2025-05-15 06:00:00", "2025-05-16 06:00:00", "2025-05-15 07:00:00"), tz = "Europe/Berlin"),
    scheduled_sampling_time = as.POSIXct(c("2025-05-15 06:00:00", "2025-05-16 06:00:00", "2025-05-15 07:00:00"), tz = "Europe/Berlin"),
    sample_compliant = TRUE, sampling_time_source = "app"
  )
  app <- interactive_sampling_timeline(samples, launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
  expect_error(interactive_sampling_timeline(samples[0, ], launch = FALSE), "No CARWatch samples")
})

test_that("conversion report editor exposes issue-constrained decisions", {
  skip_if_not_installed("shiny"); skip_if_not_installed("DT")
  issues <- tibble::tibble(
    participant = "p1", day = "D1", sample_id = "S1", code = "missing_scheduled_sample_event",
    issue_id = "issue-1", message = "missing", proposed_action = "use_manual_diary_sampling_time",
    proposed_action_description = "Use diary", resolution_status = "unresolved",
    user_decision = "accept", user_decision_value = ""
  )
  app <- conversion_report_editor(issues, launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  expect_no_error(app$serverFuncSource()(session$input, session$output, session))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(issues_rows_selected = 1L)
    session$setInputs(decision = "change", decision_value = "use_default", apply = 1L)
    expect_identical(current()$user_decision[[1]], "change")
    expect_identical(current()$user_decision_value[[1]], "use_default")
  })
  expect_true("change" %in% carwatch:::.editor_decisions("missing_scheduled_sample_event"))
  expect_true("" %in% carwatch:::.editor_decisions("missing_scheduled_sample_event"))
  expect_false("change" %in% carwatch:::.editor_decisions("invalid_study_metadata"))
  expect_true("override_expected_sample" %in% carwatch:::.editor_decisions("expected_sample_not_in_active_metadata"))
  table <- carwatch:::.editor_issue_datatable(issues, names(issues))
  expect_true("KeyTable" %in% unlist(table$x$extensions))
  expect_match(as.character(table$x$callback), "key-focus\\.dt")
  path <- tempfile(fileext = ".csv")
  write_conversion_report(issues, path)
  expect_equal(read_conversion_report(path)$issue_id, "issue-1")
  expect_error(conversion_report_editor(issues[0, ], launch = FALSE), "no issues")
})

test_that("conversion report editor refreshes unresolved issues and retains history", {
  skip_if_not_installed("shiny"); skip_if_not_installed("DT")
  root <- tempfile("carwatch-editor-")
  generate_synthetic_study_data(
    root,
    n_participants = 1,
    missing_awakening_time_ratio = 0.25,
    missing_sampling_time_ratio = 0.25,
    random_state = 42,
    overwrite = TRUE,
    validate = FALSE
  )
  folders <- list.dirs(file.path(root, "logs"), recursive = FALSE, full.names = TRUE)
  raw_logs <- read_raw_logs_from_participant_dirs(stats::setNames(folders, basename(folders)))
  raw_logs_before <- raw_logs
  advisory <- suppressWarnings(convert_raw_logs(raw_logs, errors = "warn", create_report = TRUE))
  diary <- read_manual_diary(file.path(root, "manual_diary.csv"))
  app <- conversion_report_editor(
    advisory$report,
    raw_logs = raw_logs,
    manual_diary = diary,
    launch = FALSE
  )

  shiny::testServer(app$serverFuncSource(), {
    expect_gt(nrow(current()), 0L)
    expect_equal(nrow(history()), nrow(advisory$report$issues))
    session$setInputs(refresh = 1L)
    session$flushReact()
    expect_equal(nrow(current()), 0L)
    expect_equal(nrow(history()), nrow(advisory$report$issues))
    expect_identical(refresh_status(), "No unresolved issues remain.")
  })
  expect_identical(raw_logs, raw_logs_before)
})

test_that("conversion report editor defers unavailable diary decisions", {
  root <- tempfile("carwatch-editor-deferred-")
  generate_synthetic_study_data(
    root,
    n_participants = 1,
    missing_awakening_time_ratio = 0.25,
    missing_sampling_time_ratio = 0.25,
    random_state = 42,
    overwrite = TRUE,
    validate = FALSE
  )
  folders <- list.dirs(file.path(root, "logs"), recursive = FALSE, full.names = TRUE)
  raw_logs <- read_raw_logs_from_participant_dirs(stats::setNames(folders, basename(folders)))
  advisory <- suppressWarnings(convert_raw_logs(raw_logs, errors = "warn", create_report = TRUE))
  issues <- advisory$report$issues
  diary <- read_manual_diary(file.path(root, "manual_diary.csv"))
  deferred_issue <- issues[1, , drop = FALSE]
  diary_column <- if (deferred_issue$code[[1]] == "missing_awakening_time") {
    "awakening_time"
  } else {
    paste0("sampling_time_", deferred_issue$sample_position[[1]])
  }
  diary_row <- diary$participant == deferred_issue$participant[[1]] & diary$day == deferred_issue$day[[1]]
  diary[[diary_column]][diary_row] <- NA_character_

  refreshed <- carwatch:::.refresh_editor_issues(
    raw_logs,
    issues,
    protocol_manifest = NULL,
    sampling_schedule = NULL,
    manual_diary = diary,
    check_compliance = TRUE,
    compliance_checker = new_sampling_compliance_checker()
  )

  deferred_id <- deferred_issue$issue_id[[1]]
  expect_named(refreshed$deferred_errors, deferred_id)
  expect_equal(refreshed$remaining$issue_id, deferred_id)
  expect_identical(refreshed$remaining$user_decision[[1]], "")
  expect_identical(
    refreshed$history$user_decision[match(deferred_id, refreshed$history$issue_id)],
    ""
  )
  expect_false(any(setdiff(issues$issue_id, deferred_id) %in% refreshed$remaining$issue_id))
})

test_that("conversion report editor preserves explicit unresolved decisions", {
  previous <- tibble::tibble(
    participant = "p1", day = "D1", sample_id = "S1", code = "missing_scheduled_sample_event",
    issue_id = "issue-1", message = "missing", proposed_action = "use_manual_diary_sampling_time",
    proposed_action_description = "Use diary", resolution_status = "unresolved",
    user_decision = "", user_decision_value = ""
  )
  refreshed <- previous
  refreshed$user_decision <- "accept"

  preserved <- carwatch:::.preserve_editor_unresolved_decisions(previous, refreshed)

  expect_identical(preserved$user_decision[[1]], "")
  expect_identical(preserved$user_decision_value[[1]], "")
})

test_that("conversion report editor reports unexpected refresh errors", {
  skip_if_not_installed("shiny"); skip_if_not_installed("DT")
  issues <- tibble::tibble(
    participant = "p1", day = "D1", sample_id = "S1", code = "duplicate_scheduled_sample_events",
    issue_id = "issue-1", message = "duplicate", proposed_action = "keep_earliest_scan",
    proposed_action_description = "Keep earliest", resolution_status = "unresolved",
    user_decision = "accept", user_decision_value = ""
  )
  invalid_raw_logs <- tibble::tibble(timestamp = as.POSIXct("2025-01-01", tz = "Europe/Berlin"))
  app <- conversion_report_editor(issues, raw_logs = invalid_raw_logs, launch = FALSE)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(refresh = 1L)
    session$flushReact()
    expect_match(refresh_status(), "^Refresh failed:")
    expect_equal(current()$issue_id, "issue-1")
    expect_equal(history()$issue_id, "issue-1")
  })
})
