# ============================================================
# 005_Figure3_PCA.R
#
# Objective 1
# PCA of the normative behavioural architecture in the
# LAPCOM control population
#
# The four behavioural variables are robustly normalised within
# each cohort (Author), using only that cohort's control fish.
# After normalisation, all 89 controls are analysed together.
# Author is retained only for individual traceability.
# ============================================================

# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

base_dir <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

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
  "tibble",
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
library(tibble)
library(ggplot2)

# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

control_file <- file.path(
  "01_Data",
  "Processed",
  "control.rds"
)

figures_dir <- file.path(
  "03_Results",
  "Figures"
)

objects_dir <- file.path(
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  "03_Results",
  "Tables"
)

for (
  directory_i in c(
    figures_dir,
    objects_dir,
    tables_dir
  )
) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

if (!file.exists(control_file)) {
  stop(
    "The control dataset was not found:\n",
    control_file,
    "\nRun 001_load_lapcom_data.R before this script."
  )
}

# ------------------------------------------------------------
# 4. Load the LAPCOM control population
# ------------------------------------------------------------

control <- readRDS(
  control_file
)

if (!is.data.frame(control)) {
  stop(
    "The object stored in control.rds is not a data frame."
  )
}

# ------------------------------------------------------------
# 5. Integrity checks for the control dataset
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
expected_cohort_n <- 4L

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

observed_cohort_n <- dplyr::n_distinct(
  control$Author
)

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
  stop(
    "Duplicated Author + ID keys were found at row(s): ",
    paste(
      which(duplicate_key),
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

if (sum(cohort_counts$n) != expected_control_n) {
  stop(
    "The cohort counts do not sum to 89 control fish."
  )
}

data_audit <- data.frame(
  Variable = vars,
  Total_n = nrow(control),
  Available_n = vapply(
    control[vars],
    function(x) sum(is.finite(x)),
    integer(1)
  ),
  Missing_or_nonfinite_n = vapply(
    control[vars],
    function(x) sum(!is.finite(x)),
    integer(1)
  ),
  Unique_values = vapply(
    control[vars],
    function(x) length(unique(x)),
    integer(1)
  ),
  stringsAsFactors = FALSE
)

if (
  any(data_audit$Available_n != expected_control_n) ||
  any(data_audit$Missing_or_nonfinite_n != 0L) ||
  any(data_audit$Unique_values < 2L)
) {
  stop(
    "The raw behavioural variables failed their integrity checks."
  )
}

# ------------------------------------------------------------
# 6. Robust normalisation within each control cohort
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
    as.data.frame(invalid_parameters),
    row.names = FALSE
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
  normalised_name <- paste0(
    variable_name,
    "_rz"
  )
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
            normalised_name <- paste0(
              variable_name,
              "_rz"
            )
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
    as.data.frame(normalisation_audit),
    row.names = FALSE
  )

  stop(
    "The cohort-wise robust-normalisation audit failed."
  )
}

# ------------------------------------------------------------
# 7. Prepare and verify the 89 x 4 PCA input matrix
# ------------------------------------------------------------

X_robust <- control_normalised %>%
  dplyr::select(
    dplyr::all_of(normalised_vars)
  )

# Use the original behavioural names in the PCA output. Values
# remain the cohort-normalised robust z-scores.

names(X_robust) <- vars

pca_input_audit <- data.frame(
  Variable = vars,
  Available_n = vapply(
    X_robust,
    function(x) sum(is.finite(x)),
    integer(1)
  ),
  Missing_or_nonfinite_n = vapply(
    X_robust,
    function(x) sum(!is.finite(x)),
    integer(1)
  ),
  Unique_values = vapply(
    X_robust,
    function(x) length(unique(x)),
    integer(1)
  ),
  stringsAsFactors = FALSE
)

if (
  nrow(X_robust) != expected_control_n ||
  ncol(X_robust) != length(vars) ||
  !identical(names(X_robust), vars) ||
  any(pca_input_audit$Available_n != expected_control_n) ||
  any(pca_input_audit$Missing_or_nonfinite_n != 0L) ||
  any(pca_input_audit$Unique_values < 2L)
) {
  stop(
    "The normalised PCA input is not the expected 89 x 4 complete matrix."
  )
}

# ------------------------------------------------------------
# 8. Principal component analysis
# ------------------------------------------------------------

# The variables are already robustly scaled within cohort.
# Mean centring is performed as required for conventional PCA.
# No additional SD scaling is performed.

pca <- stats::prcomp(
  x = X_robust,
  center = TRUE,
  scale. = FALSE,
  retx = TRUE
)

expected_pca_centre <- colMeans(
  X_robust
)

if (
  !isTRUE(
    all.equal(
      unname(pca$center),
      unname(expected_pca_centre),
      tolerance = normalisation_tolerance
    )
  ) ||
  !identical(pca$scale, FALSE)
) {
  stop(
    "The PCA did not preserve the required settings: ",
    "center = TRUE and scale. = FALSE."
  )
}

if (
  nrow(pca$x) != expected_control_n ||
  ncol(pca$x) != length(vars) ||
  any(!is.finite(pca$x)) ||
  any(!is.finite(pca$rotation))
) {
  stop(
    "The PCA model failed its dimensional or finite-value checks."
  )
}

# ------------------------------------------------------------
# 9. PCA variance
# ------------------------------------------------------------

pca_eigenvalues <- pca$sdev^2

pca_variance <- tibble::tibble(
  Component = paste0(
    "PC",
    seq_along(pca_eigenvalues)
  ),
  Eigenvalue = pca_eigenvalues,
  Percentage_of_variance =
    100 * pca_eigenvalues / sum(pca_eigenvalues),
  Cumulative_percentage = cumsum(
    Percentage_of_variance
  )
)

if (
  nrow(pca_variance) != length(vars) ||
  any(!is.finite(as.matrix(pca_variance[-1]))) ||
  abs(
    sum(pca_variance$Percentage_of_variance) - 100
  ) > normalisation_tolerance ||
  abs(
    tail(pca_variance$Cumulative_percentage, 1) - 100
  ) > normalisation_tolerance
) {
  stop(
    "The PCA variance table failed its integrity checks."
  )
}

percent1 <- round(
  pca_variance$Percentage_of_variance[1],
  1
)

percent2 <- round(
  pca_variance$Percentage_of_variance[2],
  1
)

# ------------------------------------------------------------
# 10. PCA individual coordinates with author traceability
# ------------------------------------------------------------

coord <- dplyr::bind_cols(
  control_normalised %>%
    dplyr::select(
      Author,
      ID,
      Treatment
    ),
  as.data.frame(pca$x)
) %>%
  tibble::as_tibble()

traceability_columns <- c(
  "Author",
  "ID",
  "Treatment"
)

traceability_preserved <- all(
  vapply(
    traceability_columns,
    function(column_name) {
      identical(
        as.character(coord[[column_name]]),
        as.character(control_normalised[[column_name]])
      )
    },
    logical(1)
  )
)

if (
  nrow(coord) != expected_control_n ||
  dplyr::n_distinct(coord$Author) != expected_cohort_n ||
  !traceability_preserved
) {
  stop(
    "Author + ID traceability or row order was not preserved in PCA scores."
  )
}

# ------------------------------------------------------------
# 11. PCA variable loadings
# ------------------------------------------------------------

pca_loadings <- as.data.frame(
  pca$rotation
) %>%
  tibble::rownames_to_column(
    var = "Variable"
  ) %>%
  tibble::as_tibble()

if (
  nrow(pca_loadings) != length(vars) ||
  !identical(pca_loadings$Variable, vars)
) {
  stop(
    "The PCA loading table failed its integrity checks."
  )
}

# ------------------------------------------------------------
# 12. Generate Figure 3 without a confidence ellipse
# ------------------------------------------------------------

p <- ggplot2::ggplot(
  coord,
  ggplot2::aes(
    x = PC1,
    y = PC2
  )
) +
  ggplot2::geom_point(
    size = 2.5,
    alpha = 0.80,
    colour = "#1F77B4"
  ) +
  ggplot2::labs(
    x = paste0(
      "PC1 (",
      percent1,
      "%)"
    ),
    y = paste0(
      "PC2 (",
      percent2,
      "%)"
    )
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_classic(
    base_size = 14
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.title = ggplot2::element_text(
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
# 13. Define export paths
# ------------------------------------------------------------

pdf_output <- file.path(
  figures_dir,
  "Figure3_PCA.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Figure3_PCA.tiff"
)

normalisation_parameters_csv <- file.path(
  tables_dir,
  "Objective1_RobustNormalisationParameters.csv"
)

normalisation_audit_csv <- file.path(
  tables_dir,
  "Objective1_RobustNormalisationAudit.csv"
)

pca_variance_csv <- file.path(
  tables_dir,
  "Figure3_PCA_Variance.csv"
)

pca_loadings_csv <- file.path(
  tables_dir,
  "Figure3_PCA_Loadings.csv"
)

pca_coordinates_csv <- file.path(
  tables_dir,
  "Figure3_PCA_IndividualCoordinates.csv"
)

normalisation_parameters_rds <- file.path(
  objects_dir,
  "objective1_robust_normalisation_parameters.rds"
)

normalisation_audit_rds <- file.path(
  objects_dir,
  "objective1_robust_normalisation_audit.rds"
)

control_normalised_rds <- file.path(
  objects_dir,
  "objective1_control_robust_z.rds"
)

pca_model_rds <- file.path(
  objects_dir,
  "objective1_pca_model_robust.rds"
)

pca_scores_rds <- file.path(
  objects_dir,
  "objective1_pca_scores_robust.rds"
)

pca_loadings_rds <- file.path(
  objects_dir,
  "objective1_pca_loadings_robust.rds"
)

pca_variance_rds <- file.path(
  objects_dir,
  "objective1_pca_variance_robust.rds"
)

# ------------------------------------------------------------
# 14. Export Figure 3, tables and R objects
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = pdf_output,
  plot = p,
  width = 7,
  height = 6,
  units = "in"
)

ggplot2::ggsave(
  filename = tiff_output,
  plot = p,
  width = 7,
  height = 6,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

utils::write.csv(
  normalisation_parameters,
  file = normalisation_parameters_csv,
  row.names = FALSE
)

utils::write.csv(
  normalisation_audit,
  file = normalisation_audit_csv,
  row.names = FALSE
)

utils::write.csv(
  pca_variance,
  file = pca_variance_csv,
  row.names = FALSE
)

utils::write.csv(
  pca_loadings,
  file = pca_loadings_csv,
  row.names = FALSE
)

utils::write.csv(
  coord,
  file = pca_coordinates_csv,
  row.names = FALSE
)

saveRDS(
  normalisation_parameters,
  normalisation_parameters_rds
)

saveRDS(
  normalisation_audit,
  normalisation_audit_rds
)

saveRDS(
  control_normalised,
  control_normalised_rds
)

saveRDS(
  pca,
  pca_model_rds
)

saveRDS(
  coord,
  pca_scores_rds
)

saveRDS(
  pca_loadings,
  pca_loadings_rds
)

saveRDS(
  pca_variance,
  pca_variance_rds
)

all_output_files <- c(
  pdf_output,
  tiff_output,
  normalisation_parameters_csv,
  normalisation_audit_csv,
  pca_variance_csv,
  pca_loadings_csv,
  pca_coordinates_csv,
  normalisation_parameters_rds,
  normalisation_audit_rds,
  control_normalised_rds,
  pca_model_rds,
  pca_scores_rds,
  pca_loadings_rds,
  pca_variance_rds
)

output_file_info <- file.info(
  all_output_files
)

if (
  any(!file.exists(all_output_files)) ||
  any(is.na(output_file_info$size)) ||
  any(output_file_info$size <= 0)
) {
  stop(
    "At least one PCA output file was not created correctly."
  )
}

# ------------------------------------------------------------
# 15. Final audit
# ------------------------------------------------------------

cat(
  "\n============================================================\n"
)

cat(
  "FIGURE 3 — COHORT-NORMALISED PCA OF LAPCOM CONTROLS\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nNumber of control fish: ",
  nrow(control_normalised),
  "\n",
  sep = ""
)

cat(
  "Number of cohorts retained for traceability: ",
  observed_cohort_n,
  "\n",
  sep = ""
)

cat(
  "PCA input dimensions: ",
  nrow(X_robust),
  " rows x ",
  ncol(X_robust),
  " variables\n",
  sep = ""
)

cat(
  "PCA settings: center = TRUE; scale. = FALSE\n"
)

cat(
  "Confidence ellipse: not plotted\n"
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
  "\nInput-data audit:\n"
)

print(
  data_audit,
  row.names = FALSE
)

cat(
  "\nRobust-normalisation parameters by control cohort:\n"
)

print(
  as.data.frame(normalisation_parameters),
  row.names = FALSE
)

cat(
  "\nRobust-normalisation audit by control cohort:\n"
)

print(
  as.data.frame(normalisation_audit),
  row.names = FALSE
)

cat(
  "\nPCA input audit:\n"
)

print(
  pca_input_audit,
  row.names = FALSE
)

cat(
  "\nPCA variance:\n"
)

print(
  as.data.frame(pca_variance),
  row.names = FALSE
)

cat(
  "\nPCA loadings:\n"
)

print(
  as.data.frame(pca_loadings),
  row.names = FALSE
)

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
