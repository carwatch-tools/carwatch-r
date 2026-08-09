.merge_results <- function(days = "D1", samples = c("B1", "B2", "B3", "B4"), recorded = samples, missing = character()) {
  rows <- list()
  for (day in days) for (position in seq_along(samples)) {
    sample <- samples[[position]]
    omitted <- paste(day, sample, sep = "\r") %in% missing
    sampling_time <- if (omitted) as.POSIXct(NA) else as.POSIXct(sprintf("2025-05-%02d 06:%02d:00", 14L + match(day, days), position), tz = "Europe/Berlin")
    rows[[length(rows) + 1L]] <- tibble::tibble(
      participant = "02", day = day, sample = sample,
      variable = c("sampling_time", "recorded_sample", "sample_position"),
      value = list(sampling_time, if (omitted) NA_character_ else recorded[[position]], as.integer(position))
    )
  }
  carwatch:::.from_long_results(dplyr::bind_rows(rows))
}

test_that("canonical Study Results reject duplicate participants", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "day,D1",
    "sample,day",
    "variable,date",
    "participant,",
    "01,2025-05-15 00:00:00+02:00",
    "01,2025-05-16 00:00:00+02:00"
  ), path)
  expect_error(read_study_results(path), "duplicate participant")
})

test_that("physical matching treats recorded tubes as opaque identifiers", {
  results <- .merge_results(samples = c("B1", "B2"), recorded = c("external-tube", "B2"))
  saliva <- tibble::tibble(participant = "02", sample = c("external-tube", "B2"), cortisol = c(9, 2))
  merged <- merge_saliva(results, saliva)
  samples <- as_sample_events(merged)
  expect_equal(samples$cortisol, c(9, 2))
  expect_true(samples$mismatch_corrected[[1]])
  expect_false("recorded_sample_in_schedule" %in% names(samples))
})

test_that("positional matching falls back only for unknown recorded positions", {
  results <- .merge_results(samples = c("B1", "B2"), recorded = c("unknown", "B2"))
  saliva <- tibble::tibble(participant = "02", day = "D1", sample_position = 1:2, cortisol = c(1, 2))
  expect_warning(merged <- merge_saliva(results, saliva, match_on = "position"), "absent from their registration")
  samples <- as_sample_events(merged)
  expect_equal(samples$cortisol, c(1, 2))
  expect_identical(samples$recorded_sample_in_schedule, c(FALSE, TRUE))
})

test_that("positional matching supports reused scheduled IDs across days", {
  results <- .merge_results(days = c("D1", "D2"), samples = c("S1", "S2"))
  saliva <- tibble::tibble(participant = "02", day = rep(c("D1", "D2"), each = 2), sample_position = rep(1:2, 2))
  saliva$cortisol <- c(1, 2, 3, 4)
  merged <- merge_saliva(results, saliva, match_on = "position")
  expect_equal(as_sample_events(merged)$cortisol, c(1, 2, 3, 4))
})

test_that("required merge keys remain unique despite metadata", {
  results <- .merge_results(samples = c("B1", "B2"))
  saliva <- tibble::tibble(
    participant = "02", day = "D1", sample_position = c(1L, 1L),
    condition = c("control", "stress"), cortisol = c(1, 2)
  )
  expect_error(merge_saliva(results, saliva, match_on = "position"), "duplicate participant/day/sample_position")
})

test_that("R metadata columns reproduce additional pandas index levels", {
  results <- .merge_results(days = c("D1", "D2"), samples = c("B1", "B2"))
  saliva <- tibble::tibble(participant = "02", day = rep(c("D1", "D2"), each = 2), sample_position = rep(1:2, 2))
  saliva$cortisol <- c(1, 2, 3, 4)
  saliva$condition <- c("challenge", "challenge", "challenge", "challenge")
  saliva$assay_batch <- c("a", "b", "a", "a")
  merged <- merge_saliva(results, saliva, match_on = "position")
  days <- as_study_days(merged)
  samples <- as_sample_events(merged)
  expect_identical(days$condition, c("challenge", "challenge"))
  expect_false("assay_batch" %in% names(days))
  expect_identical(samples$assay_batch, c("a", "b", "a", "a"))
})

test_that("unmatched laboratory rows cannot create protocol positions", {
  results <- .merge_results(samples = c("B1", "B2"))
  saliva <- tibble::tibble(participant = "02", sample = c("B1", "B2", "B3"), cortisol = c(1, 2, 3))
  expect_error(merge_saliva(results, saliva), "no CARWatch protocol position")
})

test_that("missing app events are controlled independently from unmatched rows", {
  results <- .merge_results(samples = c("B1", "B2"), missing = "D1\rB2")
  saliva <- tibble::tibble(participant = "02", sample = c("B1", "B2"), cortisol = c(1, 2))
  ignored <- merge_saliva(results, saliva)
  expect_false(as_sample_events(ignored)$sampling_event_recorded[[2]])
  expect_error(merge_saliva(results, saliva, missing_carwatch_data = "raise"), "scheduled_sample=B2")
})

test_that("saliva merge leaves inputs unchanged and permits another analyte", {
  results <- .merge_results(samples = c("B1", "B2"))
  saliva <- tibble::tibble(participant = "02", sample = c("B1", "B2"), cortisol = c(1, 2))
  original_results <- results
  original_saliva <- saliva
  cortisol <- merge_saliva(results, saliva)
  amylase_input <- saliva
  names(amylase_input)[names(amylase_input) == "cortisol"] <- "alpha_amylase"
  amylase <- merge_saliva(cortisol, amylase_input)
  expect_equal(results, original_results)
  expect_equal(saliva, original_saliva)
  expect_equal(as_sample_events(amylase)$alpha_amylase, c(1, 2))
  expect_error(merge_saliva(cortisol, saliva), "already exist")
})

.metric_data <- function(include_s0 = FALSE) {
  samples <- if (include_s0) c("S0", "S1", "S2", "S3", "S4") else c("S1", "S2", "S3", "S4")
  times <- if (include_s0) c(-10, 0, 10, 20, 30) else c(0, 10, 20, 30)
  values <- if (include_s0) c(5, 1, 3, 2, 4, 10, 12, 8, 6, 4) else c(1, 3, 2, 4, 10, 12, 8, 6)
  tibble::tibble(
    participant = rep(c("VP01", "VP02"), each = length(samples)), day = "D1",
    sample = rep(samples, 2), cortisol = values, amylase = values * 10,
    time_min = rep(times, 2)
  )
}

test_that("saliva AUC and response features match the reference examples", {
  pruessner <- tibble::tibble(
    participant = rep(c("P1", "P2"), each = 5), sample = rep(paste0("S", 1:5), 2),
    cortisol = rep(c(3.5, 7, 14, 7, 10), 2),
    time_min = c(1:5, 0, 10, 15, 30, 45)
  )
  result <- auc(pruessner)
  expect_equal(result$cortisol_auc_g, c(34.75, 390))
  expect_equal(result$cortisol_auc_i, c(20.75, 232.5))
  features <- compute_features(.metric_data())
  expect_equal(features$cortisol_auc_g, c(75, 280))
  expect_equal(features$cortisol_ini_val, c(1, 10))
  expect_equal(features$cortisol_max_val, c(4, 12))
  expect_equal(features$cortisol_max_inc, c(3, 2))
})

test_that("saliva metrics distinguish incomplete from invalid time series", {
  incomplete <- .metric_data()
  incomplete$cortisol[[2]] <- NA_real_
  expect_true(is.na(auc(incomplete)$cortisol_auc_g[[1]]))
  invalid <- .metric_data()
  invalid$time_min[1:4] <- c(0, 10, 10, 30)
  expect_error(auc(invalid), "strictly increasing")
  expect_error(compute_features(invalid), "strictly increasing")
  invalid_measurement <- .metric_data()
  invalid_measurement$cortisol <- as.character(invalid_measurement$cortisol)
  invalid_measurement$cortisol[[1]] <- "invalid"
  expect_error(compute_features(invalid_measurement), "numeric")
})

test_that("baseline removal and multi-analyte features retain curve groups", {
  data <- .metric_data(include_s0 = TRUE)
  expect_equal(initial_value(data)$cortisol_ini_val, c(5, 10))
  expect_equal(initial_value(data, remove_s0 = TRUE)$cortisol_ini_val, c(1, 12))
  multiple <- auc(data, saliva_type = c("cortisol", "amylase"))
  expect_named(multiple, c("cortisol", "amylase"))
  expect_equal(multiple$amylase$amylase_auc_g, multiple$cortisol$cortisol_auc_g * 10)
})

test_that("CARWatch feature adapter uses position, actual time, and extra groups", {
  data <- .metric_data()
  names(data)[names(data) == "sample"] <- "scheduled_sample"
  data$sample_position <- rep(1:4, 2)
  data$condition <- rep(c("control", "intervention"), each = 4)
  result <- compute_features_from_carwatch(data)
  expect_named(result, c("participant", "day", "condition", "cortisol_auc_g", "cortisol_auc_i", "cortisol_ini_val", "cortisol_max_val", "cortisol_max_inc", "cortisol_slope14"))
  expect_equal(result$cortisol_auc_g, c(75, 280))
  duplicate <- data
  duplicate$sample_position[[2]] <- 1L
  expect_error(compute_features_from_carwatch(duplicate), "duplicate sample rows")
  invalid <- data
  invalid$sample_position[[1]] <- 0L
  expect_error(compute_features_from_carwatch(invalid), "positive integers")
})

test_that("saliva table utilities provide reversible R-native forms", {
  features <- compute_features(.metric_data())
  long <- saliva_feature_wide_to_long(features, "cortisol")
  expect_true(all(c("participant", "day", "saliva_feature", "cortisol") %in% names(long)))
  times <- sample_times_datetime_to_minute(tibble::tibble(S1 = "06:00:00", S2 = "06:15:00", S3 = "06:45:00"))
  expect_equal(unlist(times, use.names = FALSE), c(0, 15, 45))
})

.compliance_data <- function() tibble::tibble(
  participant = c(rep("vp01", 7), rep("vp02", 4)),
  day = c(rep("D1", 4), rep("D2", 3), rep("D1", 4)),
  sample = c(paste0("S", 1:4), paste0("S", 1:3), paste0("S", 1:4)),
  sample_position = c(1:4, 1:3, 1:4),
  sampling_time = as.POSIXct("2025-01-01 06:00:00", tz = "Europe/Berlin") + seq_len(11) * 60,
  sample_compliant = c(TRUE, FALSE, NA, TRUE, TRUE, NA, TRUE, rep(TRUE, 4)),
  cortisol = seq_len(11)
)

test_that("compliance removal preserves participant-day scope", {
  data <- .compliance_data()
  days <- drop_non_compliant_samples(data)
  expect_false(any(days$participant == "vp01" & days$day == "D1"))
  expect_true(any(days$participant == "vp02" & days$day == "D1"))
  samples <- drop_non_compliant_samples(data, drop_entire_day = FALSE, drop_unassessed = TRUE)
  expect_false(any(is.na(samples$sample_compliant)))
  expect_false(any(samples$sample_compliant %in% FALSE))
  invalid <- data
  invalid$sample_compliant <- as.character(invalid$sample_compliant)
  expect_error(drop_non_compliant_samples(invalid), "TRUE, FALSE")
})

test_that("wide compliance clearing retains schedule structure", {
  data <- .compliance_data()[1:4, ]
  long <- dplyr::bind_rows(lapply(seq_len(nrow(data)), function(row) tibble::tibble(
    participant = data$participant[[row]], day = data$day[[row]], sample = data$sample[[row]],
    variable = c("sampling_time", "sample_position", "sample_compliant", "cortisol"),
    value = list(data$sampling_time[[row]], data$sample_position[[row]], data$sample_compliant[[row]], data$cortisol[[row]])
  )))
  results <- carwatch:::.from_long_results(long)
  sample_only <- as_sample_events(drop_non_compliant_samples(results, drop_entire_day = FALSE))
  failed <- sample_only$sample == "S2"
  expect_true(is.na(sample_only$cortisol[failed]))
  expect_identical(sample_only$sample_position[failed], 2L)
  whole_day <- as_sample_events(drop_non_compliant_samples(results))
  expect_true(all(is.na(whole_day$cortisol)))
})

test_that("static plots use actual time, status, and source provenance", {
  data <- .compliance_data()[1:4, ]
  data$awakening_time <- as.POSIXct("2025-01-01 06:00:00", tz = "Europe/Berlin")
  data$scheduled_sampling_time <- data$awakening_time + c(0, 15, 30, 45) * 60
  data$sampling_time_source <- c("app", "manual_diary", NA, "schedule")
  data$sampling_time[[3]] <- as.POSIXct(NA)
  data$time_deviation_min <- c(1, 2, NA, -1)
  data$time_min <- c(1, 17, NA, 44)
  data$condition <- "control"
  data$cortisol <- c(3, 8, NA, 4)
  timeline <- plot_sampling_timeline(data, "vp01", "D1")
  expect_s3_class(timeline, "ggplot")
  expect_identical(timeline$labels$y, "Scheduled sample")
  expect_s3_class(plot_compliance_overview(data), "ggplot")
  expect_s3_class(plot_compliance_overview(data, view = "heatmap"), "ggplot")
  expect_s3_class(plot_timing_deviation(data), "ggplot")
  curve <- plot_saliva_curve(data, group_by = "condition", n_boot = 20)
  expect_identical(curve$labels$x, "Minutes since awakening")
  expect_error(plot_timing_deviation(dplyr::mutate(data, time_deviation_min = NA_real_)), "No recorded timing deviations")
})
