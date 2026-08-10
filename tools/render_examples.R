example_files <- c(
  "examples/01-log-processing.Rmd",
  "examples/02-saliva-analysis.Rmd",
  "examples/03-plotting.Rmd",
  "examples/04-synthetic-study-interactive-log-processing.Rmd",
  "examples/gallery/01-research-workflow/01-load-carwatch-logs.Rmd",
  "examples/gallery/01-research-workflow/02-resolve-conversion-issues-in-spreadsheet.Rmd",
  "examples/gallery/01-research-workflow/03-resolve-conversion-issues-interactively.Rmd",
  "examples/gallery/01-research-workflow/04-assess-and-filter-sampling-compliance.Rmd",
  "examples/gallery/01-research-workflow/05-inspect-individual-sampling-timing.Rmd",
  "examples/gallery/01-research-workflow/06-merge-saliva-measurements.Rmd",
  "examples/gallery/01-research-workflow/07-compute-cortisol-features.Rmd",
  "examples/gallery/02-protocol-variants/01-reconstruct-multi-registration-protocol.Rmd",
  "examples/gallery/03-example-data/01-generate-local-example-data.Rmd"
)

missing_files <- example_files[!file.exists(example_files)]
if (length(missing_files)) {
  stop("Missing R Markdown examples: ", paste(missing_files, collapse = ", "))
}

output_root <- file.path(tempdir(), "carwatch-r-rendered-examples")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

for (input in example_files) {
  relative_dir <- dirname(sub("^examples/", "", input))
  output_dir <- file.path(output_root, relative_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  message("Rendering ", input)
  rmarkdown::render(
    input = input,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}

message("Rendered ", length(example_files), " examples to ", output_root)
