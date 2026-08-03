.plot_samples <- function(data) .as_samples(data)

#' Plot an individual sampling timeline
#' @param data Canonical results or sample events.
#' @param participant Participant identifier.
#' @param day Canonical day identifier.
#' @param show_expected Whether expected sampling times are shown.
#' @return A ggplot object.
#' @export
plot_sampling_timeline <- function(data, participant, day, show_expected = TRUE) {
  samples <- dplyr::filter(.plot_samples(data), .data$participant == participant, .data$day == day)
  if (!nrow(samples)) .carwatch_abort("Participant/day combination is absent from Study Results.", "carwatch_value_error")
  plot <- ggplot2::ggplot(samples, ggplot2::aes(x = .data$sample_position, y = .data$sampling_time, colour = .data$sample_compliant)) + ggplot2::geom_point(size = 3) + ggplot2::labs(x = "Sample position", y = "Sampling time", colour = "Compliant")
  if (show_expected && "scheduled_sampling_time" %in% names(samples)) plot <- plot + ggplot2::geom_point(ggplot2::aes(y = .data$scheduled_sampling_time), inherit.aes = TRUE, shape = 1, colour = "black")
  plot
}

#' Plot cohort sampling compliance
#' @param data Canonical results or sample events.
#' @param by Summary grouping variable.
#' @param view Plot form.
#' @return A ggplot object.
#' @export
plot_compliance_overview <- function(data, by = "sample_position", view = c("proportion", "heatmap")) {
  view <- match.arg(view); summary <- summarize_compliance(data, by)
  if (view == "proportion") ggplot2::ggplot(summary, ggplot2::aes(x = .data[[by]], y = .data$compliance_rate)) + ggplot2::geom_col() + ggplot2::labs(y = "Compliance rate", x = by) else ggplot2::ggplot(summary, ggplot2::aes(x = .data[[by]], y = 1, fill = .data$compliance_rate)) + ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c(na.value = "grey80")
}

#' Plot timing deviations
#' @param data Canonical results or sample events.
#' @param by Grouping variable.
#' @return A ggplot object.
#' @export
plot_timing_deviation <- function(data, by = "sample_position") {
  samples <- .plot_samples(data); .require_columns(samples, c(by, "time_deviation_min"), "Sample data")
  ggplot2::ggplot(samples, ggplot2::aes(x = .data[[by]], y = .data$time_deviation_min)) + ggplot2::geom_hline(yintercept = 0, colour = "grey50") + ggplot2::geom_boxplot() + ggplot2::labs(x = by, y = "Timing deviation (min)")
}

#' Plot individual or grouped saliva curves
#' @param data Canonical results or sample events.
#' @param value Saliva value column.
#' @param participant Optional participant filter.
#' @param day Optional day filter.
#' @param group_by Reserved grouping option.
#' @param ci Reserved confidence level.
#' @param n_boot Reserved bootstrap count.
#' @param seed Reserved random seed.
#' @param show_individual Whether individual curves are drawn.
#' @return A ggplot object.
#' @export
plot_saliva_curve <- function(data, value = "cortisol", participant = NULL, day = NULL, group_by = NULL, ci = 95, n_boot = 1000, seed = 0, show_individual = TRUE) {
  samples <- .plot_samples(data); .require_columns(samples, c("sample_position", value), "Saliva data")
  if (!is.null(participant)) samples <- dplyr::filter(samples, .data$participant == participant)
  if (!is.null(day)) samples <- dplyr::filter(samples, .data$day == day)
  plot <- ggplot2::ggplot(samples, ggplot2::aes(x = .data$sample_position, y = .data[[value]], group = interaction(.data$participant, .data$day)))
  if (show_individual) plot <- plot + ggplot2::geom_line(alpha = 0.35) + ggplot2::geom_point(alpha = 0.35)
  plot + ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", colour = "black", linewidth = 1) + ggplot2::labs(x = "Sample position", y = value)
}
