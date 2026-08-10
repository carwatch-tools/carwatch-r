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
