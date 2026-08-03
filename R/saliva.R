.saliva_data <- function(data, saliva_type, sample_col = NULL, group_cols = NULL, remove_s0 = FALSE) {
  if (!inherits(data, "data.frame")) .carwatch_abort("Saliva data must be a data frame.", "carwatch_type_error")
  .require_columns(data, saliva_type, "Saliva data")
  if (inherits(data, "carwatch_results")) data <- as_sample_events(data)
  data <- tibble::as_tibble(data)
  if (is.null(sample_col)) sample_col <- intersect(c("sample_position", "scheduled_sample", "sample"), names(data))[[1]]
  if (is.null(sample_col) || !sample_col %in% names(data)) .carwatch_abort("Saliva data require `sample`, `scheduled_sample`, or `sample_position`.", "carwatch_schema_error")
  if (is.null(group_cols)) group_cols <- setdiff(names(data), c(saliva_type, sample_col, "time", "time_min", "sampling_time", "recorded_sample", "barcode", "sample_compliant", "sample_mismatch", "day_expected", "day_scanned", "schedule_type", "sampling_time_source", "expected_interval_min", "actual_interval_min", "scheduled_sampling_time", "time_deviation_min"))
  .require_columns(data, group_cols, "Saliva data")
  data[[saliva_type]] <- suppressWarnings(as.numeric(data[[saliva_type]]))
  if (remove_s0) data <- dplyr::filter(data, !as.character(.data[[sample_col]]) %in% c("0", "S0"))
  data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(c(group_cols, sample_col))))
  list(data = data, sample_col = sample_col, group_cols = group_cols)
}

.saliva_times <- function(data, sample_times) {
  if (is.null(sample_times)) {
    column <- intersect(c("time_min", "time"), names(data))
    if (!length(column)) .carwatch_abort("Sampling times must be supplied or available as `time_min`/`time`.", "carwatch_schema_error")
    return(as.numeric(data[[column[[1]]]]))
  }
  if (is.character(sample_times) && length(sample_times) == 1L) {
    .require_columns(data, sample_times, "Saliva data")
    return(as.numeric(data[[sample_times]]))
  }
  as.numeric(sample_times)
}

.saliva_group_apply <- function(prepared, fun) {
  data <- prepared$data
  groups <- prepared$group_cols
  if (!length(groups)) return(fun(data))
  dplyr::group_modify(dplyr::group_by(data, dplyr::across(dplyr::all_of(groups)), .drop = FALSE), ~fun(.x)) |>
    dplyr::ungroup()
}

.multi_analyte <- function(data, saliva_type, fun) {
  if (length(saliva_type) == 1L) return(fun(saliva_type))
  stats::setNames(lapply(saliva_type, fun), saliva_type)
}

#' Compute the maximum measured value in each saliva curve
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param remove_s0 Whether to remove baseline sample S0.
#' @return Group-level maximum values.
#' @export
max_value <- function(data, saliva_type = "cortisol", remove_s0 = FALSE) {
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, remove_s0 = remove_s0)
    .saliva_group_apply(prepared, function(x) tibble::tibble(!!paste0(analyte, "_max_val") := if (all(is.na(x[[analyte]]))) NA_real_ else max(x[[analyte]], na.rm = TRUE)))
  })
}

#' Return the initial value in each saliva curve
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param remove_s0 Whether to remove baseline sample S0.
#' @return Group-level initial values.
#' @export
initial_value <- function(data, saliva_type = "cortisol", remove_s0 = FALSE) {
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, remove_s0 = remove_s0)
    .saliva_group_apply(prepared, function(x) tibble::tibble(!!paste0(analyte, "_ini_val") := x[[analyte]][[1]]))
  })
}

#' Compute maximum increase from the initial sample
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param remove_s0 Whether to remove baseline sample S0.
#' @param percent Whether to return the percentage increase.
#' @return Group-level maximum increase.
#' @export
max_increase <- function(data, saliva_type = "cortisol", remove_s0 = FALSE, percent = FALSE) {
  .assert_scalar_logical(percent, "percent")
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, remove_s0 = remove_s0)
    .saliva_group_apply(prepared, function(x) {
      values <- x[[analyte]]; initial <- values[[1]]
      increase <- if (length(values) < 2L || is.na(initial) || all(is.na(values[-1]))) NA_real_ else max(values[-1], na.rm = TRUE) - initial
      if (percent && !is.na(increase)) increase <- 100 * increase / abs(initial)
      tibble::tibble(!!paste0(analyte, if (percent) "_max_inc_percent" else "_max_inc") := increase)
    })
  })
}

#' Compute area under the curve with respect to ground and increase
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param remove_s0 Whether to remove baseline sample S0.
#' @param compute_auc_post Whether to add post-baseline AUCi.
#' @param sample_times Sampling-time vector or column name.
#' @return Group-level AUC values.
#' @export
auc <- function(data, saliva_type = "cortisol", remove_s0 = FALSE, compute_auc_post = FALSE, sample_times = NULL) {
  .assert_scalar_logical(compute_auc_post, "compute_auc_post")
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, remove_s0 = remove_s0)
    .saliva_group_apply(prepared, function(x) {
      values <- x[[analyte]]; times <- .saliva_times(x, sample_times)
      valid <- length(values) >= 2L && !anyNA(values) && !anyNA(times) && all(diff(times) > 0)
      if (!valid) return(tibble::tibble(!!paste0(analyte, "_auc_g") := NA_real_, !!paste0(analyte, "_auc_i") := NA_real_, !!!if (compute_auc_post) list(!!paste0(analyte, "_auc_i_post") := NA_real_)))
      auc_g <- sum(diff(times) * (head(values, -1) + tail(values, -1)) / 2)
      delta <- values - values[[1]]
      auc_i <- sum(diff(times) * (head(delta, -1) + tail(delta, -1)) / 2)
      result <- list(); result[[paste0(analyte, "_auc_g")]] <- auc_g; result[[paste0(analyte, "_auc_i")]] <- auc_i
      if (compute_auc_post) { keep <- times >= 0; post <- if (sum(keep) >= 2) { v <- values[keep] - values[which(keep)[[1]]]; sum(diff(times[keep]) * (head(v, -1) + tail(v, -1)) / 2) } else NA_real_; result[[paste0(analyte, "_auc_i_post")]] <- post }
      tibble::as_tibble(result)
    })
  })
}

#' Compute the slope between two saliva samples
#' @param data Saliva data or canonical results.
#' @param sample_labels Two sample labels.
#' @param sample_idx Two one-based sample indices.
#' @param saliva_type Measurement column name(s).
#' @param sample_times Sampling-time vector or column name.
#' @return Group-level slope values.
#' @export
slope <- function(data, sample_labels = NULL, sample_idx = NULL, saliva_type = "cortisol", sample_times = NULL) {
  if (is.null(sample_labels) == is.null(sample_idx)) .carwatch_abort("Supply exactly one of `sample_labels` or `sample_idx`.", "carwatch_value_error")
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte)
    .saliva_group_apply(prepared, function(x) {
      indices <- if (!is.null(sample_idx)) as.integer(sample_idx) else match(as.character(sample_labels), as.character(x[[prepared$sample_col]]))
      if (length(indices) != 2L || anyNA(indices) || any(indices < 1L) || any(indices > nrow(x))) .carwatch_abort("Requested slope samples are not present.", "carwatch_value_error")
      times <- .saliva_times(x, sample_times)[indices]; values <- x[[analyte]][indices]
      output <- if (anyNA(times) || anyNA(values) || times[[2]] <= times[[1]]) NA_real_ else (values[[2]] - values[[1]]) / (times[[2]] - times[[1]])
      labels <- if (!is.null(sample_labels)) sample_labels else indices
      tibble::tibble(!!paste0(analyte, "_slope", labels[[1]], labels[[2]]) := output)
    })
  })
}

#' Compute the standard CARWatch saliva response feature set
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param group_levels Grouping column names.
#' @param sample_level Sample-position column name.
#' @param sample_times Sampling-time vector or column name.
#' @param slope_pairs Sample pairs for slopes.
#' @param remove_s0 Whether to remove baseline sample S0.
#' @return Group-level response features.
#' @export
compute_features <- function(data, saliva_type = "cortisol", group_levels = NULL, sample_level = NULL, sample_times = NULL, slope_pairs = NULL, remove_s0 = FALSE) {
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, sample_col = sample_level, group_cols = group_levels, remove_s0 = remove_s0)
    .saliva_group_apply(prepared, function(x) {
      values <- x[[analyte]]; times <- .saliva_times(x, sample_times)
      valid <- length(values) >= 2L && !anyNA(values) && !anyNA(times) && all(diff(times) > 0)
      initial <- values[[1]]; maximum <- if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
      response <- list()
      response[[paste0(analyte, "_auc_g")]] <- NA_real_
      response[[paste0(analyte, "_auc_i")]] <- NA_real_
      response[[paste0(analyte, "_ini_val")]] <- initial
      response[[paste0(analyte, "_max_val")]] <- maximum
      response[[paste0(analyte, "_max_inc")]] <- if (length(values) < 2L || is.na(initial)) NA_real_ else max(values[-1], na.rm = TRUE) - initial
      if (valid) { response[[paste0(analyte, "_auc_g")]] <- sum(diff(times) * (head(values, -1) + tail(values, -1)) / 2); delta <- values - initial; response[[paste0(analyte, "_auc_i")]] <- sum(diff(times) * (head(delta, -1) + tail(delta, -1)) / 2) }
      pairs <- slope_pairs %||% list(c(1L, length(values)))
      for (pair in pairs) { idx <- if (is.numeric(pair)) as.integer(pair) else match(as.character(pair), as.character(x[[prepared$sample_col]])); name <- paste0(analyte, "_slope", pair[[1]], pair[[2]]); response[[name]] <- if (length(idx) != 2L || anyNA(idx) || !valid) NA_real_ else (values[idx[[2]]] - values[idx[[1]]]) / (times[idx[[2]]] - times[idx[[1]]]) }
      tibble::as_tibble(response)
    })
  })
}

#' Compute CARWatch saliva features using sample position and actual time
#' @param data Canonical results or sample events.
#' @param saliva_type Measurement column name.
#' @param slope_pairs Sample pairs for slopes.
#' @return Per participant-day response features.
#' @export
compute_features_from_carwatch <- function(data, saliva_type = "cortisol", slope_pairs = NULL) {
  if (inherits(data, "carwatch_results")) data <- as_sample_events(data)
  .require_columns(data, c("participant", "day", "sample_position", "time_min"), "CARWatch saliva data")
  compute_features(data, saliva_type = saliva_type, group_levels = c("participant", "day"), sample_level = "sample_position", sample_times = "time_min", slope_pairs = slope_pairs)
}

#' Summarize descriptive saliva features
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param group_cols Grouping columns.
#' @param keep_index Retained for API compatibility.
#' @return Descriptive saliva features.
#' @export
standard_features <- function(data, saliva_type = "cortisol", group_cols = NULL, keep_index = TRUE) {
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, group_cols = group_cols)
    .saliva_group_apply(prepared, function(x) { values <- x[[analyte]]; observed <- values[!is.na(values)]; centered <- observed - mean(observed); tibble::tibble(!!paste0(analyte, "_argmax") := if (length(observed)) x[[prepared$sample_col]][which.max(values)] else NA, !!paste0(analyte, "_mean") := if (length(observed)) mean(observed) else NA_real_, !!paste0(analyte, "_std") := if (length(observed) > 1L) stats::sd(observed) else NA_real_, !!paste0(analyte, "_skew") := if (length(observed) > 2L) mean(centered^3) / stats::sd(observed)^3 else NA_real_, !!paste0(analyte, "_kurt") := if (length(observed) > 3L) mean(centered^4) / stats::sd(observed)^4 - 3 else NA_real_) })
  })
}

#' Compute mean and standard error by sample
#' @param data Saliva data or canonical results.
#' @param saliva_type Measurement column name(s).
#' @param group_cols Grouping columns.
#' @param remove_s0 Whether to remove baseline sample S0.
#' @return Mean and standard error by sample.
#' @export
mean_se <- function(data, saliva_type = "cortisol", group_cols = NULL, remove_s0 = FALSE) {
  .multi_analyte(data, saliva_type, function(analyte) {
    prepared <- .saliva_data(data, analyte, group_cols = group_cols, remove_s0 = remove_s0)
    groups <- c(prepared$group_cols, prepared$sample_col)
    dplyr::summarise(dplyr::group_by(prepared$data, dplyr::across(dplyr::all_of(groups)), .drop = FALSE), mean = mean(.data[[analyte]], na.rm = TRUE), se = stats::sd(.data[[analyte]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[analyte]]))), .groups = "drop")
  })
}
