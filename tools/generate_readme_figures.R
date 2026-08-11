arguments <- commandArgs(trailingOnly = FALSE)
script_argument <- arguments[grepl("^--file=", arguments)]
if (!length(script_argument)) {
  stop("Run this script with Rscript tools/generate_readme_figures.R")
}

script_path <- normalizePath(sub("^--file=", "", script_argument[[1]]))
package_root <- normalizePath(file.path(dirname(script_path), ".."))
output_dir <- file.path(package_root, "man", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload with install.packages('pkgload').")
}

pkgload::load_all(package_root, reset = TRUE, quiet = TRUE)

study_dir <- file.path(tempdir(), "carwatch-readme-figures")
on.exit(unlink(study_dir, recursive = TRUE, force = TRUE), add = TRUE)

study_config <- list(
  study_name = "README example",
  study_days = 4L,
  saliva_distances = c(0, 30, 15, 15),
  saliva_alarm_times = c("12:00", "17:00")
)

generate_synthetic_study_data(
  study_dir,
  study_config = study_config,
  n_participants = 40L,
  non_compliant_sample_ratio = 0.15,
  missing_awakening_time_ratio = 0.01,
  missing_sampling_time_ratio = 0.03,
  random_state = 42L,
  create_cortisol_data = TRUE,
  overwrite = TRUE,
  validate = FALSE
)

participant_folders <- list.dirs(
  file.path(study_dir, "logs"),
  recursive = FALSE,
  full.names = TRUE
)
participant_dirs <- stats::setNames(
  participant_folders,
  basename(participant_folders)
)

raw_logs <- read_raw_logs_from_participant_dirs(participant_dirs)
decisions <- read_conversion_report(file.path(study_dir, "issue_decisions.csv"))
manual_diary <- read_manual_diary(file.path(study_dir, "manual_diary.csv"))

study_results <- convert_raw_logs(
  raw_logs,
  errors = "raise",
  issue_decisions = decisions,
  manual_diary = manual_diary
)

cortisol <- readr::read_csv(
  file.path(study_dir, "cortisol.csv"),
  show_col_types = FALSE
)
merged_results <- merge_saliva(
  study_results,
  cortisol,
  match_on = "position",
  metadata_cols = "condition"
)

sample_events <- as_sample_events(study_results)
timeline_candidates <- sample_events |>
  dplyr::group_by(.data$participant, .data$day) |>
  dplyr::summarise(
    has_non_compliant = any(.data$sample_compliant %in% FALSE),
    has_manual_time = any(.data$sampling_time_source == "manual_diary", na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(score = 2L * .data$has_manual_time + .data$has_non_compliant) |>
  dplyr::arrange(dplyr::desc(.data$score), .data$participant, .data$day)

timeline_participant <- timeline_candidates$participant[[1]]
timeline_day <- timeline_candidates$day[[1]]

figures <- list(
  sampling_timeline = list(
    plot = plot_sampling_timeline(
      study_results,
      participant = timeline_participant,
      day = timeline_day
    ),
    width = 11,
    height = 6.5
  ),
  compliance_overview = list(
    plot = plot_compliance_overview(study_results),
    width = 8,
    height = 5
  ),
  timing_deviation = list(
    plot = plot_timing_deviation(study_results),
    width = 8,
    height = 5
  ),
  saliva_curve = list(
    plot = plot_saliva_curve(
      merged_results,
      value = "cortisol",
      group_by = "condition",
      n_boot = 1000L,
      seed = 42L
    ),
    width = 9,
    height = 5.5
  )
)

for (name in names(figures)) {
  figure <- figures[[name]]
  path <- file.path(output_dir, paste0(name, ".png"))
  ggplot2::ggsave(
    filename = path,
    plot = figure$plot,
    width = figure$width,
    height = figure$height,
    units = "in",
    dpi = 160,
    bg = "white"
  )
  message("Wrote ", path)
}
