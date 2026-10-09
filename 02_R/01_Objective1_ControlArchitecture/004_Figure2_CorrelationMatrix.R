# ============================================================
# 004_Figure2_CorrelationMatrix.R
#
# Objective 1
# Spearman correlation matrix in the
# LAPCOM control population
#
# Behavioural variables are robustly normalised within each
# cohort (Author), using only that cohort's control fish.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- paste0(
  "C:/Users/USER/Documents/",
  "LAPCOM_UCS_Resilience"
)

if (!dir.exists(base_dir)) {
  stop(
    "Project directory not found:\n",
    base_dir
  )
}

setwd(base_dir)

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "corrplot"
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
library(corrplot)

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
# 5. Robust normalisation within each control cohort
# ------------------------------------------------------------

mad_scale_constant <- 1.4826
normalisation_tolerance <- 1e-8

cohort_ids <- sort(
  unique(
    as.character(control$Author)
  )
)

normalisation_parameters <- dplyr::bind_rows(
  lapply(
    cohort_ids,
    function(cohort_id) {
      cohort_rows <- as.character(control$Author) == cohort_id

      dplyr::bind_rows(
        lapply(
          vars,
          function(variable_name) {
            values <- control[[variable_name]][cohort_rows]
            centre <- stats::median(values)
            mad_raw <- stats::mad(
              values,
              center = centre,
              constant = 1
            )

            data.frame(
              Author = cohort_id,
              Variable = variable_name,
              n = length(values),
              Median = centre,
              MAD_raw = mad_raw,
              MAD_scaled = mad_scale_constant * mad_raw,
              stringsAsFactors = FALSE
            )
          }
        )
      )
    }
  )
)

expected_parameter_n <- expected_cohort_n * length(vars)

if (nrow(normalisation_parameters) != expected_parameter_n) {
  stop(
    "Expected ",
    expected_parameter_n,
    " cohort-by-variable normalisation parameter sets; found ",
    nrow(normalisation_parameters),
    "."
  )
}

invalid_parameters <- normalisation_parameters %>%
  dplyr::filter(
    !is.finite(Median) |
      !is.finite(MAD_raw) |
      !is.finite(MAD_scaled) |
      MAD_raw <= 0 |
      MAD_scaled <= 0
  )

if (nrow(invalid_parameters) > 0) {
  print(
    invalid_parameters,
    n = Inf,
    width = Inf
  )

  stop(
    "At least one cohort-by-variable parameter set has a zero, ",
    "missing or non-finite MAD. Robust normalisation cannot continue."
  )
}

control_normalised <- control
normalised_vars <- paste0(
  vars,
  "_rz"
)

for (normalised_name in normalised_vars) {
  control_normalised[[normalised_name]] <- NA_real_
}

for (parameter_row in seq_len(nrow(normalisation_parameters))) {
  cohort_id <- normalisation_parameters$Author[parameter_row]
  variable_name <- normalisation_parameters$Variable[parameter_row]
  normalised_name <- paste0(variable_name, "_rz")
  cohort_rows <- as.character(control$Author) == cohort_id

  control_normalised[[normalised_name]][cohort_rows] <- (
    control[[variable_name]][cohort_rows] -
      normalisation_parameters$Median[parameter_row]
  ) / normalisation_parameters$MAD_scaled[parameter_row]
}

if (
  !identical(
    control[c("Author", "ID", "Treatment")],
    control_normalised[c("Author", "ID", "Treatment")]
  )
) {
  stop(
    "Individual identity or row order changed during normalisation."
  )
}

if (
  any(
    !is.finite(
      as.matrix(
        control_normalised[normalised_vars]
      )
    )
  )
) {
  stop(
    "Normalisation generated at least one missing or non-finite value."
  )
}

normalisation_audit <- dplyr::bind_rows(
  lapply(
    cohort_ids,
    function(cohort_id) {
      cohort_rows <- as.character(control_normalised$Author) == cohort_id

      dplyr::bind_rows(
        lapply(
          vars,
          function(variable_name) {
            normalised_name <- paste0(variable_name, "_rz")
            values <- control_normalised[[normalised_name]][cohort_rows]
            centre <- stats::median(values)
            mad_scaled <- stats::mad(
              values,
              center = centre,
              constant = mad_scale_constant
            )

            data.frame(
              Author = cohort_id,
              Variable = variable_name,
              n = length(values),
              Median_rz = centre,
              MAD_scaled_rz = mad_scaled,
              stringsAsFactors = FALSE
            )
          }
        )
      )
    }
  )
)

if (
  nrow(normalisation_audit) != expected_parameter_n ||
  any(abs(normalisation_audit$Median_rz) > normalisation_tolerance) ||
  any(
    abs(
      normalisation_audit$MAD_scaled_rz - 1
    ) > normalisation_tolerance
  )
) {
  print(
    normalisation_audit,
    n = Inf,
    width = Inf
  )

  stop(
    "The cohort-wise robust-normalisation audit failed."
  )
}

# ------------------------------------------------------------
# 6. Prepare and verify the correlation input
# ------------------------------------------------------------

datos_cor <- control_normalised %>%
  dplyr::select(
    dplyr::all_of(normalised_vars)
  )

names(datos_cor) <- vars

variable_audit <- data.frame(
  Variable = vars,
  Available_n = vapply(
    datos_cor,
    function(x) sum(is.finite(x)),
    integer(1)
  ),
  Missing_or_nonfinite_n = vapply(
    datos_cor,
    function(x) sum(!is.finite(x)),
    integer(1)
  ),
  Unique_values = vapply(
    datos_cor,
    function(x) length(unique(x)),
    integer(1)
  ),
  stringsAsFactors = FALSE
)

if (
  nrow(datos_cor) != expected_control_n ||
  any(variable_audit$Available_n != expected_control_n) ||
  any(variable_audit$Missing_or_nonfinite_n != 0L) ||
  any(variable_audit$Unique_values < 2L)
) {
  stop(
    "The normalised correlation input does not match the expected ",
    "audit conditions."
  )
}

# ------------------------------------------------------------
# 7. Calculate pairwise Spearman correlations
# ------------------------------------------------------------

calculate_spearman_matrices <- function(data) {
  data <- as.data.frame(data)
  number_of_variables <- ncol(data)
  variable_names <- names(data)

  correlation_matrix <- matrix(
    NA_real_,
    nrow = number_of_variables,
    ncol = number_of_variables,
    dimnames = list(variable_names, variable_names)
  )

  p_value_matrix <- matrix(
    NA_real_,
    nrow = number_of_variables,
    ncol = number_of_variables,
    dimnames = list(variable_names, variable_names)
  )

  sample_size_matrix <- matrix(
    NA_integer_,
    nrow = number_of_variables,
    ncol = number_of_variables,
    dimnames = list(variable_names, variable_names)
  )

  for (i in seq_len(number_of_variables)) {
    x_i <- data[[i]]
    correlation_matrix[i, i] <- 1
    p_value_matrix[i, i] <- 0
    sample_size_matrix[i, i] <- sum(is.finite(x_i))
  }

  for (i in seq_len(number_of_variables - 1L)) {
    for (j in seq.int(i + 1L, number_of_variables)) {
      x_i <- data[[i]]
      x_j <- data[[j]]
      complete_pair <- is.finite(x_i) & is.finite(x_j)
      pair_n <- sum(complete_pair)

      if (pair_n != expected_control_n) {
        stop(
          "The correlation between ",
          variable_names[i],
          " and ",
          variable_names[j],
          " did not use all ",
          expected_control_n,
          " control fish."
        )
      }

      x_pair <- x_i[complete_pair]
      y_pair <- x_j[complete_pair]

      if (
        length(unique(x_pair)) < 2L ||
        length(unique(y_pair)) < 2L
      ) {
        stop(
          "A variable had no variability for the pair ",
          variable_names[i],
          " and ",
          variable_names[j],
          "."
        )
      }

      correlation_test <- suppressWarnings(
        stats::cor.test(
          x = x_pair,
          y = y_pair,
          method = "spearman",
          exact = FALSE
        )
      )

      rho_value <- unname(correlation_test$estimate)
      p_value <- correlation_test$p.value

      correlation_matrix[i, j] <- rho_value
      correlation_matrix[j, i] <- rho_value
      p_value_matrix[i, j] <- p_value
      p_value_matrix[j, i] <- p_value
      sample_size_matrix[i, j] <- pair_n
      sample_size_matrix[j, i] <- pair_n
    }
  }

  list(
    correlation = correlation_matrix,
    p_value = p_value_matrix,
    sample_size = sample_size_matrix
  )
}

spearman_results <- calculate_spearman_matrices(
  datos_cor
)

cor_matrix <- spearman_results$correlation
p_matrix <- spearman_results$p_value
n_matrix <- spearman_results$sample_size

if (
  any(!is.finite(cor_matrix)) ||
  any(!is.finite(p_matrix)) ||
  any(n_matrix != expected_control_n) ||
  !isTRUE(
    all.equal(
      cor_matrix,
      t(cor_matrix),
      tolerance = normalisation_tolerance
    )
  ) ||
  any(abs(diag(cor_matrix) - 1) > normalisation_tolerance)
) {
  stop(
    "The Spearman correlation matrices failed their integrity checks."
  )
}

raw_cor_matrix <- stats::cor(
  control[vars],
  method = "spearman"
)

maximum_raw_normalised_difference <- max(
  abs(
    raw_cor_matrix - cor_matrix
  )
)

# ------------------------------------------------------------
# 8. Apply descriptive labels and print the audit
# ------------------------------------------------------------

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Top entries",
  top_time = "Time in top"
)

display_names <- unname(
  variable_labels[vars]
)

dimnames(cor_matrix) <- list(display_names, display_names)
dimnames(p_matrix) <- list(display_names, display_names)
dimnames(n_matrix) <- list(display_names, display_names)

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 2 — SPEARMAN CORRELATION MATRIX\n"
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
  "\nRobust-normalisation parameters by control cohort:\n"
)

print(
  as.data.frame(
    normalisation_parameters
  ),
  row.names = FALSE
)

cat(
  "\nRobust-normalisation audit by control cohort:\n"
)

print(
  as.data.frame(
    normalisation_audit
  ),
  row.names = FALSE
)

cat(
  "\nCorrelation input audit:\n"
)

print(
  variable_audit,
  row.names = FALSE
)

cat(
  "\nSpearman correlation matrix based on cohort-normalised values:\n"
)

print(
  round(
    cor_matrix,
    3
  )
)

cat(
  "\nNominal (unadjusted) P-value matrix:\n"
)

print(
  format.pval(
    p_matrix,
    digits = 4,
    eps = 2.2e-16
  )
)

cat(
  "\nPairwise sample-size matrix:\n"
)

print(
  n_matrix
)

cat(
  "\nMaximum absolute difference between raw-pooled and ",
  "cohort-normalised Spearman rho: ",
  format(
    maximum_raw_normalised_difference,
    digits = 6
  ),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 9. Plotting function
# ------------------------------------------------------------

draw_correlation_matrix <- function() {
  corrplot::corrplot(
    cor_matrix,
    method = "color",
    type = "upper",
    order = "hclust",
    addCoef.col = "black",
    number.cex = 0.8,
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.9,
    p.mat = p_matrix,
    sig.level = 0.05,
    insig = "blank",
    diag = FALSE
  )
}

# ------------------------------------------------------------
# 10. Export Figure 2
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
  "Figure2_CorrelationMatrix.pdf"
)

tiff_output <- file.path(
  output_dir,
  "Figure2_CorrelationMatrix.tiff"
)

grDevices::pdf(
  file = pdf_output,
  width = 7,
  height = 7
)

draw_correlation_matrix()
grDevices::dev.off()

grDevices::tiff(
  filename = tiff_output,
  width = 7,
  height = 7,
  units = "in",
  res = 600,
  compression = "lzw"
)

draw_correlation_matrix()
grDevices::dev.off()

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
    "At least one Figure 2 output file was not created correctly."
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
