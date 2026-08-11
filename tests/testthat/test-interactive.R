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
  expect_false("change" %in% carwatch:::.editor_decisions("invalid_study_metadata"))
  expect_true("override_expected_sample" %in% carwatch:::.editor_decisions("expected_sample_not_in_active_metadata"))
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
