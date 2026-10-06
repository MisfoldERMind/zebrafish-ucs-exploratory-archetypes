# ============================================================
# 003_Figure1_Distributions.R
#
# Objective 1
# Distribution of behavioural variables in the
# LAPCOM control population
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

expected_folders <- c(
  "01_Data"
)

missing_folders <- expected_folders[
  !dir.exists(
    file.path(
      base_dir,
      expected_folders
    )
  )
]

if (length(missing_folders) > 0) {
  stop(
    "The script does not appear to be running from the project root.\n",
    "Expected project folder(s) not found:\n",
    paste(
      missing_folders,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running the script:\n",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 3. Load the LAPCOM control population
# ------------------------------------------------------------

control_file <- file.path(
  "01_Data",
  "Processed",
  "control.rds"
)

if (!file.exists(control_file)) {
  stop(
    "The control dataset was not found:\n",
    control_file,
    "\nRun 001_load_lapcom_data.R before this script."
  )
}

control <- readRDS(
  control_file
)

if (!is.data.frame(control)) {
  stop(
    "The object stored in control.rds is not a data frame."
  )
}

# ------------------------------------------------------------
# 4. Integrity checks for the control dataset
# ------------------------------------------------------------

vars <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

required_columns <- c(
  "Author",
  "ID",
  "Treatment",
  vars
)

missing_columns <- setdiff(
  required_columns,
  names(control)
)

unexpected_columns <- setdiff(
  names(control),
  required_columns
)

if (length(missing_columns) > 0) {
  stop(
    "The following columns are missing from control.rds:\n",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

if (length(unexpected_columns) > 0) {
  stop(
    "Unexpected columns were found in control.rds:\n",
    paste(
      unexpected_columns,
      collapse = ", "
    ),
    "\nRegenerate control.rds with 001_load_lapcom_data.R."
  )
}

expected_control_n <- 89L

if (nrow(control) != expected_control_n) {
  stop(
    "control.rds must contain exactly ",
    expected_control_n,
    " control fish; found ",
    nrow(control),
    "."
  )
}

if (anyNA(control[required_columns])) {
  missing_by_column <- vapply(
    control[required_columns],
    function(x) sum(is.na(x)),
    integer(1)
  )

  missing_by_column <- missing_by_column[
    missing_by_column > 0
  ]

  stop(
    "Missing values were found in control.rds:\n",
    paste(
      paste0(
        names(missing_by_column),
        " = ",
        missing_by_column
      ),
      collapse = "\n"
    )
  )
}

blank_identifier <- vapply(
  control[c("Author", "ID", "Treatment")],
  function(x) any(trimws(as.character(x)) == ""),
  logical(1)
)

if (any(blank_identifier)) {
  stop(
    "Blank values were found in the following identifier columns:\n",
    paste(
      names(blank_identifier)[blank_identifier],
      collapse = ", "
    )
  )
}

if (!all(as.character(control$Treatment) == "Control")) {
  stop(
    "control.rds contains observations whose Treatment is not 'Control'."
  )
}

expected_cohort_n <- 4L
observed_cohort_n <- dplyr::n_distinct(control$Author)

if (observed_cohort_n != expected_cohort_n) {
  stop(
    "control.rds must contain exactly ",
    expected_cohort_n,
    " cohorts in Author; found ",
    observed_cohort_n,
    "."
  )
}

duplicate_key <- duplicated(
  control[c("Author", "ID")]
)

if (any(duplicate_key)) {
  duplicate_rows <- which(duplicate_key)

  stop(
    "Duplicated Author + ID keys were found at row(s): ",
    paste(
      duplicate_rows,
      collapse = ", "
    )
  )
}

non_numeric_vars <- vars[
  !vapply(
    control[vars],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_vars) > 0) {
  stop(
    "The following behavioural variables are not numeric:\n",
    paste(
      non_numeric_vars,
      collapse = ", "
    )
  )
}

non_finite_vars <- vars[
  vapply(
    control[vars],
    function(x) any(!is.finite(x)),
    logical(1)
  )
]

if (length(non_finite_vars) > 0) {
  stop(
    "The following behavioural variables contain non-finite values:\n",
    paste(
      non_finite_vars,
      collapse = ", "
    )
  )
}

negative_vars <- vars[
  vapply(
    control[vars],
    function(x) any(x < 0),
    logical(1)
  )
]

if (length(negative_vars) > 0) {
  stop(
    "The following behavioural variables contain negative values:\n",
    paste(
      negative_vars,
      collapse = ", "
    )
  )
}

cohort_counts <- control %>%
  dplyr::count(
    Author,
    name = "n"
  ) %>%
  dplyr::arrange(
    Author
  )

# ------------------------------------------------------------
# 5. Prepare raw behavioural data for plotting
# ------------------------------------------------------------

variable_labels <- c(
  distance = "Distance travelled (m)",
  crossing = "Line crossings (count)",
  top_entry = "Top entries (count)",
  top_time = "Time in top (s)"
)

plot_data <- control %>%
  dplyr::select(
    dplyr::all_of(vars)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Variable = factor(
      Variable,
      levels = vars
    )
  )

missing_data_summary <- plot_data %>%
  dplyr::group_by(
    Variable
  ) %>%
  dplyr::summarise(
    Total_n = dplyr::n(),
    Available_n = sum(!is.na(Value)),
    Missing_n = sum(is.na(Value)),
    .groups = "drop"
  )

if (
  nrow(plot_data) != expected_control_n * length(vars) ||
  nrow(missing_data_summary) != length(vars) ||
  !all(missing_data_summary$Total_n == expected_control_n) ||
  !all(missing_data_summary$Available_n == expected_control_n) ||
  any(missing_data_summary$Missing_n != 0L)
) {
  stop(
    "The plotting dataset does not match the expected audit conditions."
  )
}

# ------------------------------------------------------------
# 6. Audit output
# ------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 1 — BEHAVIOURAL VARIABLE DISTRIBUTIONS\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nNumber of control fish: ",
  nrow(control),
  "\n",
  sep = ""
)

cat(
  "Number of cohorts: ",
  observed_cohort_n,
  "\n",
  sep = ""
)

cat(
  "\nControl fish by cohort:\n"
)

print(
  cohort_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nMissing-data summary:\n"
)

print(
  missing_data_summary,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# 7. Generate Figure 1
# ------------------------------------------------------------

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(
    x = Value
  )
) +
  ggplot2::geom_histogram(
    bins = 20,
    colour = "black",
    fill = "grey75",
    na.rm = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ Variable,
    scales = "free",
    labeller = ggplot2::labeller(
      Variable = ggplot2::as_labeller(
        variable_labels
      )
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Frequency"
  ) +
  ggplot2::theme_classic(
    base_size = 13
  ) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      face = "bold"
    ),
    axis.text = ggplot2::element_text(
      colour = "black"
    )
  )

print(
  p
)

# ------------------------------------------------------------
# 8. Export Figure 1
# ------------------------------------------------------------

output_dir <- file.path(
  "03_Results",
  "Figures"
)

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}

if (!dir.exists(output_dir)) {
  stop(
    "The figure output directory could not be created:\n",
    output_dir
  )
}

pdf_output <- file.path(
  output_dir,
  "Figure1_Distributions.pdf"
)

tiff_output <- file.path(
  output_dir,
  "Figure1_Distributions.tiff"
)

ggplot2::ggsave(
  filename = pdf_output,
  plot = p,
  width = 9,
  height = 7,
  units = "in"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = p,
  width = 9,
  height = 7,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

output_files <- c(
  pdf_output,
  tiff_output
)

output_info <- file.info(
  output_files
)

if (
  any(!file.exists(output_files)) ||
  any(is.na(output_info$size)) ||
  any(output_info$size <= 0)
) {
  stop(
    "At least one Figure 1 output file was not created correctly."
  )
}

cat(
  "\nFigure saved as:\n",
  normalizePath(
    pdf_output,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  normalizePath(
    tiff_output,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nAll integrity checks passed.\n"
)

cat(
  "============================================================\n"
)
