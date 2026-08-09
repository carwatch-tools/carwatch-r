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
  .assert_scalar_logical(show_expected, "show_expected")
  .require_columns(samples, c("sample", "sample_position", "sampling_time", "sample_compliant", "sampling_time_source"), "Timeline data")
  recorded <- !is.na(samples$sampling_time)
  invalid_sources <- recorded & (is.na(samples$sampling_time_source) | !samples$sampling_time_source %in% c("app", "manual_diary", "schedule"))
  if (any(invalid_sources)) .carwatch_abort("Recorded samples require a supported `sampling_time_source`: app, manual_diary, or schedule.", "carwatch_schema_error")
  samples$sample_label <- factor(samples$sample, levels = rev(samples$sample[order(samples$sample_position)]))
  plot <- ggplot2::ggplot(samples, ggplot2::aes(x = .data$sampling_time, y = .data$sample_label, colour = .data$sample_compliant, shape = .data$sampling_time_source)) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::scale_shape_manual(values = c(app = 16, manual_diary = 17, schedule = 15), na.translate = FALSE) +
    ggplot2::labs(title = sprintf("Sampling timeline: %s / %s", participant, day), x = "Sampling time", y = "Scheduled sample", colour = "Compliant", shape = "Sampling-time source")
  if (show_expected && "scheduled_sampling_time" %in% names(samples)) plot <- plot + ggplot2::geom_point(ggplot2::aes(x = .data$scheduled_sampling_time), inherit.aes = FALSE, y = samples$sample_label, shape = 1, colour = "black", na.rm = TRUE)
  if ("awakening_time" %in% names(samples) && any(!is.na(samples$awakening_time))) plot <- plot + ggplot2::geom_vline(xintercept = as.numeric(samples$awakening_time[which(!is.na(samples$awakening_time))[[1]]]), linetype = 2, colour = "grey40")
  plot
}

#' Plot cohort sampling compliance
#' @param data Canonical results or sample events.
#' @param by Summary grouping variable.
#' @param view Plot form.
#' @return A ggplot object.
#' @export
plot_compliance_overview <- function(data, by = "sample_position", view = c("proportion", "heatmap")) {
  view <- match.arg(view)
  samples <- .plot_samples(data)
  .require_columns(samples, c("participant", "day", by, "sample_compliant"), "Compliance plot data")
  if (view == "proportion") {
    samples$.status <- factor(ifelse(is.na(samples$sample_compliant), "unassessed", ifelse(samples$sample_compliant, "compliant", "non-compliant")), levels = c("compliant", "non-compliant", "unassessed"))
    counts <- dplyr::count(samples, .data[[by]], .data$.status, .drop = FALSE, name = "count")
    counts <- dplyr::group_by(counts, .data[[by]])
    counts <- dplyr::mutate(counts, proportion = .data$count / sum(.data$count))
    counts <- dplyr::ungroup(counts)
    return(ggplot2::ggplot(counts, ggplot2::aes(x = .data[[by]], y = .data$proportion, fill = .data$.status)) + ggplot2::geom_col() + ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::labs(y = "Proportion", x = by, fill = "Status"))
  }
  daily <- dplyr::summarise(dplyr::group_by(samples, .data$participant, .data$day), compliance = if (all(is.na(.data$sample_compliant))) NA_real_ else mean(.data$sample_compliant %in% TRUE), .groups = "drop")
  ggplot2::ggplot(daily, ggplot2::aes(x = .data$day, y = .data$participant, fill = .data$compliance)) + ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c(limits = c(0, 1), na.value = "grey80") + ggplot2::labs(title = "Day-level sampling compliance", x = "Day", y = "Participant", fill = "Compliance")
}

#' Plot timing deviations
#' @param data Canonical results or sample events.
#' @param by Grouping variable.
#' @return A ggplot object.
#' @export
plot_timing_deviation <- function(data, by = "sample_position") {
  samples <- .plot_samples(data); .require_columns(samples, c(by, "time_deviation_min"), "Sample data")
  samples <- dplyr::filter(samples, !is.na(.data$time_deviation_min))
  if (!nrow(samples)) .carwatch_abort("No recorded timing deviations are available.", "carwatch_value_error")
  ggplot2::ggplot(samples, ggplot2::aes(x = factor(.data[[by]]), y = .data$time_deviation_min)) + ggplot2::geom_hline(yintercept = 0, colour = "grey50") + ggplot2::geom_boxplot() + ggplot2::labs(x = by, y = "Deviation from registered time [min]")
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
  samples <- .plot_samples(data); .require_columns(samples, c("participant", "day", "time_min", value), "Saliva data")
  .assert_scalar_logical(show_individual, "show_individual")
  if (length(ci) != 1L || is.na(ci) || ci <= 0 || ci >= 100) .carwatch_abort("`ci` must be between 0 and 100.", "carwatch_value_error")
  if (length(n_boot) != 1L || is.na(n_boot) || n_boot < 1L) .carwatch_abort("`n_boot` must be a positive integer.", "carwatch_value_error")
  if (!is.null(participant)) samples <- dplyr::filter(samples, .data$participant == participant)
  if (!is.null(day)) samples <- dplyr::filter(samples, .data$day == day)
  if (!nrow(samples)) .carwatch_abort("No saliva samples match the requested filters.", "carwatch_value_error")
  if (!is.null(group_by)) .require_columns(samples, group_by, "Saliva data")
  samples$.curve <- interaction(samples$participant, samples$day, drop = TRUE, lex.order = TRUE)
  mapping <- if (is.null(group_by)) ggplot2::aes(x = .data$time_min, y = .data[[value]], group = .data$.curve) else ggplot2::aes(x = .data$time_min, y = .data[[value]], group = .data$.curve, colour = .data[[group_by]])
  plot <- ggplot2::ggplot(samples, mapping)
  if (show_individual) plot <- plot + ggplot2::geom_line(alpha = 0.35) + ggplot2::geom_point(alpha = 0.35)
  summary_mapping <- if (is.null(group_by)) ggplot2::aes(group = 1) else ggplot2::aes(group = .data[[group_by]], colour = .data[[group_by]])
  plot + ggplot2::stat_summary(summary_mapping, fun = mean, geom = "line", linewidth = 1) + ggplot2::stat_summary(summary_mapping, fun.data = ggplot2::mean_se, geom = "errorbar", width = 0) + ggplot2::labs(x = "Minutes since awakening", y = tools::toTitleCase(gsub("_", " ", value)), colour = group_by)
}
