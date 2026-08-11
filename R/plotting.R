.plot_samples <- function(data) .as_samples(data)

.compliance_colours <- c(
  "compliant" = "#009E73",
  "non-compliant" = "#D55E00",
  "unassessed" = "#999999"
)
.compliance_gradient <- c("#D55E00", "#F0E442", "#009E73")
.timeline_app_target_colour <- "#5DADE2"
.timeline_source_shapes <- c(app = 21, manual_diary = 22, schedule = 24)
.timeline_source_labels <- c(app = "App", manual_diary = "Manual diary", schedule = "Schedule")
.timeline_awakening_labels <- c(
  alarm = "Alarm",
  spontaneous_awakening = "Spontaneous awakening",
  manual_diary = "Manual diary",
  manual_override = "Manual override"
)

.compliance_status <- function(value) {
  factor(
    ifelse(is.na(value), "unassessed", ifelse(value, "compliant", "non-compliant")),
    levels = names(.compliance_colours)
  )
}

.timeline_na_time <- function(n, timezone) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = timezone)
}

.timeline_protocol_targets <- function(samples) {
  timezone <- .timezone_or_default(samples$sampling_time)
  targets <- .timeline_na_time(nrow(samples), timezone)
  awakening <- if ("awakening_time" %in% names(samples)) samples$awakening_time else .timeline_na_time(nrow(samples), timezone)
  observed_awakening <- awakening[!is.na(awakening)]
  current_relative <- if (length(observed_awakening)) observed_awakening[[1]] else .timeline_na_time(1L, timezone)
  for (index in seq_len(nrow(samples))) {
    schedule_type <- if ("schedule_type" %in% names(samples)) samples$schedule_type[[index]] else NA_character_
    absolute <- if ("scheduled_sampling_time" %in% names(samples)) samples$scheduled_sampling_time[[index]] else .timeline_na_time(1L, timezone)
    interval <- if ("expected_interval_min" %in% names(samples)) suppressWarnings(as.numeric(samples$expected_interval_min[[index]])) else NA_real_
    if (!is.na(schedule_type) && schedule_type == "absolute" && !is.na(absolute)) {
      targets[[index]] <- absolute
    } else if (!is.na(schedule_type) && schedule_type == "relative" && !is.na(current_relative) && !is.na(interval)) {
      current_relative <- current_relative + interval * 60
      targets[[index]] <- current_relative
    }
  }
  targets
}

.timeline_app_targets <- function(samples) {
  timezone <- .timezone_or_default(samples$sampling_time)
  targets <- .timeline_na_time(nrow(samples), timezone)
  awakening <- if ("awakening_time" %in% names(samples)) samples$awakening_time else .timeline_na_time(nrow(samples), timezone)
  observed_awakening <- awakening[!is.na(awakening)]
  previous_relative <- if (length(observed_awakening)) observed_awakening[[1]] else .timeline_na_time(1L, timezone)
  for (index in seq_len(nrow(samples))) {
    schedule_type <- if ("schedule_type" %in% names(samples)) samples$schedule_type[[index]] else NA_character_
    absolute <- if ("scheduled_sampling_time" %in% names(samples)) samples$scheduled_sampling_time[[index]] else .timeline_na_time(1L, timezone)
    interval <- if ("expected_interval_min" %in% names(samples)) suppressWarnings(as.numeric(samples$expected_interval_min[[index]])) else NA_real_
    if (!is.na(schedule_type) && schedule_type == "absolute" && !is.na(absolute)) {
      targets[[index]] <- absolute
      next
    }
    if (is.na(schedule_type) || schedule_type != "relative") next
    if (!is.na(previous_relative) && !is.na(interval)) targets[[index]] <- previous_relative + interval * 60
    sampling_time <- samples$sampling_time[[index]]
    previous_relative <- if (!is.na(sampling_time)) sampling_time else .timeline_na_time(1L, timezone)
  }
  targets
}

.format_timing_deviation <- function(value) {
  ifelse(abs(value) < 1, sprintf("%+.0f s", value * 60), sprintf("%+.1f min", value))
}

.format_awakening_type <- function(value) {
  if (value %in% names(.timeline_awakening_labels)) return(unname(.timeline_awakening_labels[[value]]))
  tools::toTitleCase(gsub("_", " ", value, fixed = TRUE))
}

.timeline_collection_date <- function(samples) {
  for (field in c("date", "awakening_time", "sampling_time", ".protocol_target_time", ".app_target_time")) {
    if (!field %in% names(samples)) next
    values <- samples[[field]][!is.na(samples[[field]])]
    if (length(values)) return(as.Date(values[[1]], tz = .timezone_or_default(values)))
  }
  NULL
}

#' Plot an individual sampling timeline
#'
#' Actual sampling events are coloured by compliance and shaped by timestamp
#' provenance. When `show_expected = TRUE`, hollow grey circles show the static
#' protocol targets, hollow blue diamonds show targets updated from preceding
#' recorded relative samples, and arrows show the signed deviation from the
#' app-updated target. Missing relative scans break the adaptive target chain.
#' Missing events and scheduled/recorded sample mismatches remain visible.
#'
#' @param data Canonical results or sample events.
#' @param participant Participant identifier.
#' @param day Canonical day identifier.
#' @param show_expected Whether expected sampling times are shown.
#' @return A ggplot object.
#' @examples
#' fixture <- system.file("extdata", "parity", "v1.0.0", package = "carwatch")
#' results <- read_study_results(file.path(fixture, "results.csv"))
#' plot_sampling_timeline(results, participant = "VP01", day = "D1")
#' @export
plot_sampling_timeline <- function(data, participant, day, show_expected = TRUE) {
  samples <- dplyr::filter(.plot_samples(data), .data$participant == .env$participant, .data$day == .env$day)
  if (!nrow(samples)) .carwatch_abort("Participant/day combination is absent from Study Results.", "carwatch_value_error")
  .assert_scalar_logical(show_expected, "show_expected")
  .require_columns(samples, c("sample", "sample_position", "sampling_time"), "Timeline data")
  positions <- suppressWarnings(as.integer(samples$sample_position))
  if (any(is.na(positions) | positions < 1L | positions != samples$sample_position) || anyDuplicated(positions)) {
    .carwatch_abort("Timeline data require unique positive integer `sample_position` values.", "carwatch_schema_error")
  }
  samples$sample_position <- positions
  samples <- dplyr::arrange(samples, .data$sample_position)
  if (!"scheduled_sample" %in% names(samples)) samples$scheduled_sample <- samples$sample
  if (!"sample_compliant" %in% names(samples)) samples$sample_compliant <- NA
  recorded <- !is.na(samples$sampling_time)
  if (any(recorded) && !"sampling_time_source" %in% names(samples)) {
    .carwatch_abort("Timeline plots require `sampling_time_source` for every recorded sampling time.", "carwatch_schema_error")
  }
  if (!"sampling_time_source" %in% names(samples)) samples$sampling_time_source <- NA_character_
  invalid_sources <- recorded & (is.na(samples$sampling_time_source) | !samples$sampling_time_source %in% c("app", "manual_diary", "schedule"))
  if (any(invalid_sources)) .carwatch_abort("Recorded samples require a supported `sampling_time_source`: app, manual_diary, or schedule.", "carwatch_schema_error")
  samples$.status <- .compliance_status(samples$sample_compliant)
  samples$sampling_time_source <- factor(samples$sampling_time_source, levels = names(.timeline_source_shapes))
  samples$.protocol_target_time <- .timeline_protocol_targets(samples)
  samples$.app_target_time <- .timeline_app_targets(samples)
  samples$.sample_label <- paste0(samples$sample_position, ": ", samples$scheduled_sample)
  if ("recorded_sample" %in% names(samples)) {
    mismatch <- !is.na(samples$recorded_sample) & as.character(samples$recorded_sample) != as.character(samples$scheduled_sample)
    samples$.sample_label[mismatch] <- paste0(samples$.sample_label[mismatch], " (scanned: ", samples$recorded_sample[mismatch], ")")
  } else {
    mismatch <- rep(FALSE, nrow(samples))
  }
  if ("sample_mismatch" %in% names(samples)) mismatch <- samples$sample_mismatch %in% TRUE

  collection_date <- .timeline_collection_date(samples)
  title <- sprintf("Sampling timeline: %s / %s", participant, day)
  if (!is.null(collection_date)) title <- sprintf("%s (%s)", title, format(collection_date, "%Y-%m-%d"))
  legend_rows <- tibble::tibble(
    .legend_time = .timeline_na_time(3L, .timezone_or_default(samples$sampling_time)),
    .legend_position = NA_integer_,
    .status = factor(names(.compliance_colours), levels = names(.compliance_colours)),
    sampling_time_source = factor(names(.timeline_source_shapes), levels = names(.timeline_source_shapes))
  )
  plot <- ggplot2::ggplot(samples, ggplot2::aes(y = .data$sample_position)) +
    ggplot2::geom_point(
      data = dplyr::filter(samples, !is.na(.data$sampling_time)),
      ggplot2::aes(x = .data$sampling_time, fill = .data$.status, shape = .data$sampling_time_source),
      size = 3.5, stroke = 0.6, colour = "black", na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = legend_rows,
      ggplot2::aes(x = .data$.legend_time, y = .data$.legend_position, fill = .data$.status, shape = .data$sampling_time_source),
      size = 3.5, stroke = 0.6, colour = "black", alpha = 0, na.rm = TRUE,
      inherit.aes = FALSE, show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = .compliance_colours,
      labels = c(compliant = "Compliant", `non-compliant` = "Non-compliant", unassessed = "Unassessed"),
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = .timeline_source_shapes, labels = .timeline_source_labels,
      limits = names(.timeline_source_shapes), drop = FALSE, na.translate = FALSE
    ) +
    ggplot2::scale_y_reverse(breaks = samples$sample_position, labels = samples$.sample_label, expand = ggplot2::expansion(add = c(0.45, 0.35))) +
    ggplot2::scale_x_datetime(date_labels = "%H:%M", timezone = .timezone_or_default(samples$sampling_time)) +
    ggplot2::labs(title = title, x = "Local collection time", y = "Scheduled sample", fill = "Status", shape = "Sampling-time source") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2, override.aes = list(shape = 21, colour = "black", size = 3.5, alpha = 1)),
      shape = ggplot2::guide_legend(order = 3, override.aes = list(fill = "grey75", colour = "grey20", size = 3.5, alpha = 1))
    ) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "right")

  if (show_expected) {
    protocol <- dplyr::filter(samples, !is.na(.data$.protocol_target_time))
    updated <- dplyr::filter(samples, !is.na(.data$.app_target_time) & (is.na(.data$.protocol_target_time) | .data$.app_target_time != .data$.protocol_target_time))
    connectors <- dplyr::filter(samples, !is.na(.data$sampling_time) & !is.na(.data$.app_target_time))
    connectors$.deviation_min <- as.numeric(difftime(connectors$sampling_time, connectors$.app_target_time, units = "mins"))
    connectors <- dplyr::filter(connectors, abs(.data$.deviation_min) > sqrt(.Machine$double.eps))
    if (nrow(connectors)) {
      connectors$.midpoint <- as.POSIXct(
        (as.numeric(connectors$.app_target_time) + as.numeric(connectors$sampling_time)) / 2,
        origin = "1970-01-01", tz = .timezone_or_default(samples$sampling_time)
      )
      connectors$.deviation_label <- .format_timing_deviation(connectors$.deviation_min)
      for (status in levels(samples$.status)) {
        current <- dplyr::filter(connectors, .data$.status == .env$status)
        if (!nrow(current)) next
        plot <- plot +
          ggplot2::geom_segment(
            data = current,
            ggplot2::aes(x = .data$.app_target_time, xend = .data$sampling_time, y = .data$sample_position, yend = .data$sample_position),
            colour = unname(.compliance_colours[[status]]), linewidth = 0.65,
            arrow = grid::arrow(type = "open", length = grid::unit(0.10, "inches")),
            inherit.aes = FALSE
          ) +
          ggplot2::geom_label(
            data = current,
            ggplot2::aes(x = .data$.midpoint, y = .data$sample_position, label = .data$.deviation_label),
            colour = unname(.compliance_colours[[status]]), fill = "white", linewidth = 0,
            size = 3.2, vjust = 1.6, inherit.aes = FALSE
          )
      }
    }
    if (nrow(protocol)) {
      plot <- plot + ggplot2::geom_point(
        data = protocol, ggplot2::aes(x = .data$.protocol_target_time, y = .data$sample_position, colour = "Protocol target time"),
        shape = 21, fill = "white", size = 3.5, stroke = 1.1, inherit.aes = FALSE
      )
    }
    if (nrow(updated)) {
      plot <- plot + ggplot2::geom_point(
        data = updated, ggplot2::aes(x = .data$.app_target_time, y = .data$sample_position, colour = "App-updated target time"),
        shape = 23, fill = "white", size = 3.3, stroke = 1.0, inherit.aes = FALSE
      )
    }
    missing <- dplyr::filter(samples, is.na(.data$sampling_time) & !is.na(.data$.protocol_target_time))
    if (nrow(missing)) {
      plot <- plot + ggplot2::geom_point(
        data = missing, ggplot2::aes(x = .data$.protocol_target_time, y = .data$sample_position),
        shape = 4, colour = unname(.compliance_colours[["non-compliant"]]), size = 3.6,
        stroke = 1.2, inherit.aes = FALSE
      )
    }
    target_values <- c("Protocol target time" = "#595959", "App-updated target time" = .timeline_app_target_colour)
    plot <- plot + ggplot2::scale_colour_manual(
      values = target_values, breaks = names(target_values), name = NULL,
      guide = ggplot2::guide_legend(order = 1)
    )
  }
  if (any(mismatch & recorded)) {
    plot <- plot + ggplot2::geom_point(
      data = samples[mismatch & recorded, , drop = FALSE], ggplot2::aes(x = .data$sampling_time, y = .data$sample_position),
      shape = 4, colour = "black", size = 4.2, stroke = 1.1, inherit.aes = FALSE
    )
  }
  if ("awakening_time" %in% names(samples) && any(!is.na(samples$awakening_time))) {
    awakening <- samples$awakening_time[which(!is.na(samples$awakening_time))[[1]]]
    if (!"awakening_type" %in% names(samples)) .carwatch_abort("Timeline plots require `awakening_type` when an awakening time is displayed.", "carwatch_schema_error")
    awakening_types <- unique(as.character(samples$awakening_type[!is.na(samples$awakening_type)]))
    if (length(awakening_types) != 1L) .carwatch_abort("Timeline plots require exactly one non-missing `awakening_type` for a displayed awakening time.", "carwatch_schema_error")
    awakening_label <- paste0("Awakening: ", .format_awakening_type(awakening_types[[1]]))
    plot <- plot +
      ggplot2::geom_vline(
        data = tibble::tibble(awakening_time = awakening),
        ggplot2::aes(xintercept = .data$awakening_time),
        linetype = 2, colour = "#264653", linewidth = 0.7, inherit.aes = FALSE
      ) +
      ggplot2::annotate("label", x = awakening, y = min(samples$sample_position) - 0.28, label = awakening_label, hjust = 0, vjust = 0.5, colour = "#264653", fill = "white", linewidth = 0, size = 3.2)
  }
  plot
}

#' Plot cohort sampling compliance
#' @param data Canonical results or sample events.
#' @param by Summary grouping variable.
#' @param view Plot form.
#' @return A ggplot object.
#' @examples
#' fixture <- system.file("extdata", "parity", "v1.0.0", package = "carwatch")
#' results <- read_study_results(file.path(fixture, "results.csv"))
#' plot_compliance_overview(results)
#' @export
plot_compliance_overview <- function(data, by = "sample_position", view = c("proportion", "heatmap")) {
  view <- match.arg(view)
  samples <- .plot_samples(data)
  .require_columns(samples, c("participant", "day", by, "sample_compliant"), "Compliance plot data")
  if (view == "proportion") {
    samples$.status <- .compliance_status(samples$sample_compliant)
    counts <- dplyr::count(samples, .data[[by]], .data$.status, .drop = FALSE, name = "count")
    counts <- dplyr::group_by(counts, .data[[by]])
    counts <- dplyr::mutate(counts, proportion = .data$count / sum(.data$count))
    counts <- dplyr::ungroup(counts)
    return(ggplot2::ggplot(counts, ggplot2::aes(x = .data[[by]], y = .data$proportion, fill = .data$.status)) + ggplot2::geom_col() + ggplot2::scale_fill_manual(values = .compliance_colours, drop = FALSE) + ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::labs(y = "Proportion", x = by, fill = "Status"))
  }
  daily <- dplyr::summarise(dplyr::group_by(samples, .data$participant, .data$day), compliance = if (all(is.na(.data$sample_compliant))) NA_real_ else mean(.data$sample_compliant %in% TRUE), .groups = "drop")
  ggplot2::ggplot(daily, ggplot2::aes(x = .data$day, y = .data$participant, fill = .data$compliance)) + ggplot2::geom_tile() + ggplot2::scale_fill_gradientn(colours = .compliance_gradient, values = c(0, 0.5, 1), limits = c(0, 1), na.value = unname(.compliance_colours[["unassessed"]])) + ggplot2::labs(title = "Day-level sampling compliance", x = "Day", y = "Participant", fill = "Compliance")
}

.as_plot_numeric <- function(value) {
  if (is.factor(value)) value <- as.character(value)
  suppressWarnings(as.numeric(value))
}

.assert_plot_field <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    .carwatch_abort(sprintf("`%s` must be a non-empty string.", name), "carwatch_type_error")
  }
}

.normalize_plot_groups <- function(group_by) {
  if (is.null(group_by)) return(character())
  if (!is.character(group_by) || !length(group_by) || anyNA(group_by) || any(!nzchar(group_by))) {
    .carwatch_abort("`group_by` must contain one or more non-empty column names, or be NULL.", "carwatch_type_error")
  }
  if (anyDuplicated(group_by)) .carwatch_abort("`group_by` must not contain duplicate columns.", "carwatch_value_error")
  group_by
}

.saliva_group_labels <- function(samples, groups) {
  if (!length(groups)) return(rep("All recordings", nrow(samples)))
  values <- lapply(groups, function(field) {
    value <- as.character(samples[[field]])
    value[is.na(value)] <- "NA"
    paste0(field, "=", value)
  })
  do.call(paste, c(values, sep = " | "))
}

.bootstrap_mean_ci <- function(values, ci, n_boot, seed) {
  if (length(values) < 2L) return(c(NA_real_, NA_real_))
  if (length(unique(values)) == 1L) return(rep(values[[1]], 2L))
  calculate <- function() {
    result <- boot::boot(values, statistic = function(data, index) mean(data[index]), R = n_boot)
    interval <- suppressWarnings(tryCatch(boot::boot.ci(result, conf = ci / 100, type = "bca"), error = function(error) NULL))
    bounds <- if (!is.null(interval$bca) && ncol(interval$bca) >= 5L) as.numeric(interval$bca[1L, 4:5]) else c(NA_real_, NA_real_)
    if (any(!is.finite(bounds))) {
      alpha <- (100 - ci) / 200
      bounds <- as.numeric(stats::quantile(result$t, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE))
    }
    round(bounds, 6L)
  }
  if (is.null(seed)) return(calculate())
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv) else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  calculate()
}

.summarize_saliva_curve <- function(samples, value, groups, ci, n_boot, seed) {
  labels <- unique(samples$.group)
  keys <- unique(samples[c(".group", "sample_position")])
  keys$.group_order <- match(keys$.group, labels)
  keys <- dplyr::arrange(keys, .data$.group_order, .data$sample_position)
  records <- lapply(seq_len(nrow(keys)), function(index) {
    key <- keys[index, , drop = FALSE]
    selected <- samples$.group == key$.group[[1]] & samples$sample_position == key$sample_position[[1]]
    measurements <- samples[[value]][selected]
    interval <- if (is.null(ci)) c(NA_real_, NA_real_) else .bootstrap_mean_ci(measurements, ci, n_boot, seed)
    tibble::tibble(
      sample_position = key$sample_position[[1]],
      time_min = mean(samples$time_min[selected]),
      .mean = mean(measurements),
      .ci_low = interval[[1]],
      .ci_high = interval[[2]],
      .group = key$.group[[1]]
    )
  })
  summary <- dplyr::bind_rows(records)
  summary$.group <- factor(summary$.group, levels = labels)
  summary
}

#' Plot signed sampling-time deviations
#'
#' Negative values indicate early collection and positive values late
#' collection. Boxes summarize each group while jittered points retain every
#' recorded observation.
#'
#' @param data Canonical results or sample events.
#' @param by Grouping variable. Defaults to `sample_position`.
#' @return A ggplot object.
#' @examples
#' fixture <- system.file("extdata", "parity", "v1.0.0", package = "carwatch")
#' results <- read_study_results(file.path(fixture, "results.csv"))
#' plot_timing_deviation(results)
#' @export
plot_timing_deviation <- function(data, by = "sample_position") {
  .assert_plot_field(by, "by")
  samples <- .plot_samples(data)
  .require_columns(samples, c(by, "time_deviation_min"), "Sample data")
  samples$time_deviation_min <- .as_plot_numeric(samples$time_deviation_min)
  samples <- dplyr::filter(samples, !is.na(.data$time_deviation_min))
  if (!nrow(samples)) .carwatch_abort("No recorded timing deviations are available.", "carwatch_value_error")
  group_values <- as.character(samples[[by]])
  samples$.deviation_group <- factor(group_values, levels = unique(group_values))
  ggplot2::ggplot(samples, ggplot2::aes(x = .data$.deviation_group, y = .data$time_deviation_min)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#404040", linewidth = 0.55, linetype = 2) +
    ggplot2::geom_boxplot(fill = "#B8D8D8", width = 0.5, outlier.shape = NA, colour = "grey40") +
    ggplot2::geom_jitter(width = 0.16, height = 0, colour = "#264653", alpha = 0.68, size = 1.8) +
    ggplot2::labs(
      title = "Sampling-time deviation",
      x = tools::toTitleCase(gsub("_", " ", by, fixed = TRUE)),
      y = "Deviation from registered time [min]"
    )
}

#' Plot CARWatch-aligned saliva response curves
#'
#' Actual `time_min` values define the horizontal axis. Faint lines show
#' participant-day curves. Thick lines connect group means calculated
#' separately at each registered `sample_position`; shaded ribbons show BCa
#' bootstrap confidence intervals when at least two measurements contribute.
#'
#' @param data Canonical results or sample events.
#' @param value Non-empty name of the saliva measurement column.
#' @param participant Optional exact participant filter.
#' @param day Optional exact canonical-day filter.
#' @param group_by Optional character vector defining separate aggregate curves.
#' @param ci Bootstrap confidence level between 0 and 100, or `NULL` for no band.
#' @param n_boot Positive number of bootstrap resamples.
#' @param seed Integer random seed, or `NULL` to use the current R RNG state.
#' @param show_individual Whether individual participant-day curves are drawn.
#' @return A ggplot object.
#' @examples
#' fixture <- system.file("extdata", "parity", "v1.0.0", package = "carwatch")
#' results <- read_study_results(file.path(fixture, "results.csv"))
#' saliva <- read_saliva(file.path(fixture, "saliva.csv"))
#' merged <- merge_saliva(results, saliva)
#' plot_saliva_curve(merged, ci = NULL)
#' @export
plot_saliva_curve <- function(data, value = "cortisol", participant = NULL, day = NULL, group_by = NULL, ci = 95, n_boot = 1000, seed = 0, show_individual = TRUE) {
  .assert_plot_field(value, "value")
  .assert_scalar_logical(show_individual, "show_individual")
  groups <- .normalize_plot_groups(group_by)
  if (!is.null(ci) && (!is.numeric(ci) || length(ci) != 1L || is.na(ci) || !is.finite(ci) || ci <= 0 || ci >= 100)) {
    .carwatch_abort("`ci` must be a number strictly between 0 and 100, or NULL.", "carwatch_value_error")
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) || !is.finite(n_boot) || n_boot < 1 || n_boot != as.integer(n_boot)) {
    .carwatch_abort("`n_boot` must be a positive integer.", "carwatch_value_error")
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) || seed != as.integer(seed))) {
    .carwatch_abort("`seed` must be an integer or NULL.", "carwatch_type_error")
  }
  n_boot <- as.integer(n_boot)
  if (!is.null(seed)) seed <- as.integer(seed)

  samples <- .plot_samples(data)
  .require_columns(samples, c("participant", "day", "sample_position", "time_min", value, groups), "Saliva data")
  if (!is.null(participant)) samples <- dplyr::filter(samples, .data$participant == .env$participant)
  if (!is.null(day)) samples <- dplyr::filter(samples, .data$day == .env$day)
  numeric_positions <- .as_plot_numeric(samples$sample_position)
  positions <- suppressWarnings(as.integer(numeric_positions))
  invalid_positions <- is.na(positions) | positions < 1L | positions != numeric_positions
  if (any(invalid_positions)) .carwatch_abort("`sample_position` must contain positive integers.", "carwatch_schema_error")
  samples$sample_position <- positions
  samples$time_min <- .as_plot_numeric(samples$time_min)
  samples[[value]] <- .as_plot_numeric(samples[[value]])
  samples <- dplyr::filter(samples, !is.na(.data$time_min) & !is.na(.data[[value]]))
  if (!nrow(samples)) .carwatch_abort("No saliva measurements with an actual sampling time remain.", "carwatch_value_error")

  curve_fields <- c("participant", "day", groups)
  curve_values <- lapply(curve_fields, function(field) as.character(samples[[field]]))
  samples$.curve <- do.call(paste, c(curve_values, sep = " / "))
  duplicate <- duplicated(samples[c(".curve", "sample_position")]) | duplicated(samples[c(".curve", "sample_position")], fromLast = TRUE)
  if (any(duplicate)) .carwatch_abort("Each saliva curve requires one row per sample position.", "carwatch_schema_error")
  samples$.group <- .saliva_group_labels(samples, groups)
  samples <- dplyr::arrange(samples, .data$.curve, .data$sample_position)
  summary <- .summarize_saliva_curve(samples, value, groups, ci, n_boot, seed)

  palette <- c("#0173B2", "#DE8F05", "#029E73", "#D55E00", "#CC78BC", "#CA9161", "#FBafe4", "#949494", "#ECE133", "#56B4E9")
  group_levels <- levels(summary$.group)
  colours <- stats::setNames(rep(palette, length.out = length(group_levels)), group_levels)
  plot <- ggplot2::ggplot(samples)
  if (show_individual) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x = .data$time_min, y = .data[[value]], group = .data$.curve), colour = "grey65", alpha = 0.38, linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes(x = .data$time_min, y = .data[[value]], group = .data$.curve), colour = "grey65", alpha = 0.38, size = 1.2)
  }
  if (!is.null(ci) && any(stats::complete.cases(summary[c(".ci_low", ".ci_high")]))) {
    plot <- plot + ggplot2::geom_ribbon(
      data = dplyr::filter(summary, !is.na(.data$.ci_low) & !is.na(.data$.ci_high)),
      ggplot2::aes(x = .data$time_min, ymin = .data$.ci_low, ymax = .data$.ci_high, fill = .data$.group, group = .data$.group),
      alpha = 0.18, colour = NA, inherit.aes = FALSE
    )
  }
  plot +
    ggplot2::geom_line(
      data = summary,
      ggplot2::aes(x = .data$time_min, y = .data$.mean, colour = .data$.group, group = .data$.group),
      linewidth = 1.15, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = summary,
      ggplot2::aes(x = .data$time_min, y = .data$.mean, colour = .data$.group),
      size = 2.5, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = colours, name = "Group", drop = FALSE) +
    ggplot2::scale_fill_manual(values = colours, guide = "none", drop = FALSE) +
    ggplot2::labs(title = "Saliva response curve", x = "Minutes since awakening", y = tools::toTitleCase(gsub("_", " ", value, fixed = TRUE)))
}
