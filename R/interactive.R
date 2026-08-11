.require_optional_packages <- function(packages, feature) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) .carwatch_abort(sprintf("%s requires optional packages: %s. Install them with install.packages(c(%s)).", feature, paste(missing, collapse = ", "), paste(shQuote(missing), collapse = ", ")), "carwatch_dependency_error")
}

.editor_decisions <- function(code) {
  decisions <- c("accept", "keep", "drop_sample", "drop_day", "drop_participant")
  if (identical(code, "expected_sample_not_in_active_metadata")) decisions <- c(decisions, "override_expected_sample")
  if (code %in% c("multiple_collection_dates", "possible_reregistration", "non_increasing_sampling_times", "missing_awakening_time", "missing_scheduled_sample_event")) decisions <- c(decisions, "change")
  decisions
}

.editor_issue_frame <- function(report) {
  if (is.list(report) && !inherits(report, "data.frame") && "issues" %in% names(report)) report <- report$issues
  frame <- .normalize_issue_decisions(report)
  .require_columns(frame, c("code", "message", "proposed_action", "proposed_action_description"), "Conversion report")
  frame
}

.editor_unresolved_issues <- function(issues) {
  if (!"resolution_status" %in% names(issues)) return(issues)
  issues[issues$resolution_status == "unresolved", , drop = FALSE]
}

.merge_editor_history <- function(history, refreshed) {
  merged <- dplyr::bind_rows(.normalize_issue_decisions(history), .normalize_issue_decisions(refreshed))
  merged <- merged[!duplicated(merged$issue_id, fromLast = TRUE), , drop = FALSE]
  .normalize_issue_decisions(merged)
}

#' Write an editable conversion report
#' @param report A conversion report list or its issue table.
#' @param path Destination CSV path.
#' @return `path`, invisibly.
#' @export
write_conversion_report <- function(report, path) {
  frame <- .editor_issue_frame(report)
  path <- fs::path_abs(path)
  if (tolower(fs::path_ext(path)) != "csv") .carwatch_abort("Conversion reports must be saved as CSV files.", "carwatch_value_error")
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  readr::write_csv(frame, path, na = "")
  invisible(path)
}

#' Open an interactive participant-day sampling timeline
#'
#' This R-native replacement for the Python notebook widget uses Shiny. The
#' app owns participant/day selectors and redraws [plot_sampling_timeline()].
#'
#' @param data Complete canonical results or a sample-event tibble.
#' @param show_expected Whether registered target times are drawn.
#' @param launch Run the app immediately. Set `FALSE` to return a `shiny.appobj`
#'   for testing, embedding, or deployment.
#' @param host Host passed to [shiny::runApp()].
#' @param port Optional port passed to [shiny::runApp()].
#' @return A Shiny app object when `launch = FALSE`; otherwise the result of
#'   [shiny::runApp()].
#' @export
interactive_sampling_timeline <- function(data, show_expected = TRUE, launch = interactive(), host = "127.0.0.1", port = NULL) {
  .require_optional_packages("shiny", "Interactive sampling timelines")
  .assert_scalar_logical(show_expected, "show_expected"); .assert_scalar_logical(launch, "launch")
  samples <- .plot_samples(data)
  .require_columns(samples, c("participant", "day"), "Timeline data")
  if (!nrow(samples)) .carwatch_abort("No CARWatch samples are available for an interactive timeline.", "carwatch_value_error")
  participants <- sort(unique(as.character(samples$participant)), method = "radix")
  initial_days <- sort(unique(as.character(samples$day[samples$participant == participants[[1]]])), method = "radix")
  ui <- shiny::fluidPage(
    shiny::titlePanel("CARWatch sampling timeline"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("participant", "Participant", choices = participants, selected = participants[[1]])),
      shiny::column(4, shiny::selectInput("day", "Study day", choices = initial_days, selected = initial_days[[1]]))
    ),
    shiny::plotOutput("timeline", height = "520px")
  )
  server <- function(input, output, session) {
    shiny::observeEvent(input$participant, {
      choices <- sort(unique(as.character(samples$day[samples$participant == input$participant])), method = "radix")
      shiny::updateSelectInput(session, "day", choices = choices, selected = choices[[1]])
    }, ignoreInit = TRUE)
    output$timeline <- shiny::renderPlot({
      shiny::req(input$participant, input$day)
      print(plot_sampling_timeline(samples, input$participant, input$day, show_expected = show_expected))
    })
  }
  app <- shiny::shinyApp(ui, server)
  if (!launch) return(app)
  shiny::runApp(app, host = host, port = port)
}

#' Edit conversion decisions interactively
#'
#' The table is shown on the left and issue-specific decision controls on the
#' right. The report remains immutable until the selected row passes the same
#' validation used by [convert_raw_logs()]. When `raw_logs` is supplied,
#' `Refresh remaining issues` applies the cumulative decisions to the original
#' events with `errors = "warn"` and replaces the visible table with unresolved
#' issues from the new conversion. Resolved upstream decisions remain in the
#' history returned by `Done`.
#'
#' @param report A conversion report list or its editable issue table.
#' @param launch Run the Shiny gadget immediately. Set `FALSE` to return a
#'   `shiny.appobj` for tests, embedding, or deployment.
#' @param host Host passed to [shiny::runApp()].
#' @param port Optional port passed to [shiny::runApp()].
#' @param raw_logs Optional immutable raw events. Required to enable refreshing
#'   the conversion after decisions change.
#' @param protocol_manifest,sampling_schedule,manual_diary,check_compliance,compliance_checker
#'   Conversion settings forwarded unchanged by `Refresh remaining issues`.
#' @return A validated issue tibble, `NULL` after cancellation, or a Shiny app
#'   object when `launch = FALSE`.
#' @export
conversion_report_editor <- function(report, launch = interactive(), host = "127.0.0.1", port = NULL, raw_logs = NULL, protocol_manifest = NULL, sampling_schedule = NULL, manual_diary = NULL, check_compliance = TRUE, compliance_checker = new_sampling_compliance_checker()) {
  .require_optional_packages(c("shiny", "DT"), "The conversion-report editor")
  .assert_scalar_logical(launch, "launch"); .assert_scalar_logical(check_compliance, "check_compliance")
  issues <- .editor_issue_frame(report)
  if (!nrow(issues)) .carwatch_abort("The conversion report contains no issues to edit.", "carwatch_value_error")
  display_columns <- intersect(c("participant", "day", "sample_id", "code", "message", "proposed_action", "resolution_status", "user_decision", "user_decision_value"), names(issues))
  refresh_button <- shiny::actionButton("refresh", "Refresh remaining issues", icon = shiny::icon("refresh"), class = "btn-primary")
  if (is.null(raw_logs)) refresh_button$attribs$disabled <- "disabled"
  ui <- shiny::fluidPage(
    shiny::titlePanel("CARWatch conversion decisions"),
    shiny::fluidRow(
      shiny::column(12, refresh_button, shiny::span(shiny::textOutput("refresh_status", inline = TRUE), style = "margin-left: 1rem;"))
    ),
    shiny::br(),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::helpText("Select one issue, apply a supported decision, then refresh the remaining issue queue."),
        shiny::selectInput("decision", "Decision", choices = character()),
        shiny::textAreaInput("decision_value", "Decision value", rows = 4, placeholder = "Required for change and override_expected_sample"),
        shiny::actionButton("apply", "Apply decision"),
        shiny::hr(),
        shiny::actionButton("done", "Done"),
        shiny::actionButton("cancel", "Cancel")
      ),
      shiny::mainPanel(DT::DTOutput("issues")),
      position = "right"
    )
  )
  server <- function(input, output, session) {
    unresolved <- .editor_unresolved_issues(issues)
    history <- shiny::reactiveVal(issues)
    current <- shiny::reactiveVal(unresolved)
    refresh_status <- shiny::reactiveVal(sprintf("%d unresolved issue%s.", nrow(unresolved), if (nrow(unresolved) == 1L) "" else "s"))
    output$refresh_status <- shiny::renderText(refresh_status())
    output$issues <- DT::renderDT(DT::datatable(current()[display_columns], selection = "single", rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE)))
    shiny::observeEvent(input$issues_rows_selected, {
      row <- input$issues_rows_selected
      if (!length(row)) return()
      frame <- current(); code <- frame$code[[row]]
      choices <- .editor_decisions(code)
      selected <- frame$user_decision[[row]]
      if (!selected %in% choices) selected <- choices[[1]]
      shiny::updateSelectInput(session, "decision", choices = choices, selected = selected)
      shiny::updateTextAreaInput(session, "decision_value", value = frame$user_decision_value[[row]])
    })
    shiny::observeEvent(input$apply, {
      row <- input$issues_rows_selected
      if (length(row) != 1L) { shiny::showNotification("Select one issue first.", type = "error"); return() }
      frame <- current(); frame$user_decision[[row]] <- input$decision %||% ""; frame$user_decision_value[[row]] <- input$decision_value %||% ""
      validated <- tryCatch(.normalize_issue_decisions(frame), error = identity)
      if (inherits(validated, "error")) { shiny::showNotification(conditionMessage(validated), type = "error", duration = NULL); return() }
      current(validated); history(.merge_editor_history(history(), validated)); shiny::showNotification("Decision applied.", type = "message")
    })
    shiny::observeEvent(input$refresh, {
      if (is.null(raw_logs)) {
        shiny::showNotification("Refresh requires `raw_logs`.", type = "error", duration = NULL)
        return()
      }
      decisions <- tryCatch(.normalize_issue_decisions(history()), error = identity)
      if (inherits(decisions, "error")) { shiny::showNotification(conditionMessage(decisions), type = "error", duration = NULL); return() }
      refreshed <- tryCatch(
        suppressWarnings(convert_raw_logs(
          raw_logs,
          protocol_manifest = protocol_manifest,
          errors = "warn",
          create_report = TRUE,
          issue_decisions = decisions,
          sampling_schedule = sampling_schedule,
          manual_diary = manual_diary,
          check_compliance = check_compliance,
          compliance_checker = compliance_checker
        )),
        error = identity
      )
      if (inherits(refreshed, "error")) {
        refresh_status(paste("Refresh failed:", conditionMessage(refreshed)))
        shiny::showNotification(conditionMessage(refreshed), type = "error", duration = NULL)
        return()
      }
      refreshed_issues <- .editor_issue_frame(refreshed$report)
      history(.merge_editor_history(history(), refreshed_issues))
      remaining <- .editor_unresolved_issues(refreshed_issues)
      current(remaining)
      refresh_status(if (nrow(remaining)) sprintf("%d unresolved issue%s remain.", nrow(remaining), if (nrow(remaining) == 1L) "" else "s") else "No unresolved issues remain.")
      if (nrow(remaining)) {
        session$onFlushed(function() DT::selectRows(DT::dataTableProxy("issues", session = session), 1L), once = TRUE)
      }
    })
    shiny::observeEvent(input$done, {
      validated <- tryCatch(.normalize_issue_decisions(history()), error = identity)
      if (inherits(validated, "error")) { shiny::showNotification(conditionMessage(validated), type = "error", duration = NULL); return() }
      shiny::stopApp(validated)
    })
    shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
  }
  app <- shiny::shinyApp(ui, server)
  if (!launch) return(app)
  shiny::runApp(app, host = host, port = port)
}
