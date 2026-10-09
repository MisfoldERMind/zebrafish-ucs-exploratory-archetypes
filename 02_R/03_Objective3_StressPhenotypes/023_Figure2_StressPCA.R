# ============================================================
# 023_Figure2_StressPCA_CORREGIDO.R
#
# Objective 3
# Internal behavioural architecture of the UCS-exposed population
# Figure 2: de novo PCA of the Stress population
#
# Scientific question:
#   What internal behavioural structure emerges within the
#   UCS-exposed population?
#
# Analytical role:
#   Describe the dominant axes of internal variation within the
#   89 UCS-exposed zebrafish before de novo Louvain community
#   detection.
#
# Required upstream script:
#   020_StressArchitecture_Setup_CORREGIDO_v3.R
#
# Definitive analytical scale:
#   Author-specific Control-referenced robust coordinates inherited
#   unchanged from corrected Objectives 1 and 2:
#
#     rz = (x - Median_control_within_Author) /
#          (1.4826 * MAD_raw_control_within_Author)
#
# PCA specification:
#   - population: Stress only;
#   - variables: distance_rz, crossing_rz, top_entry_rz, top_time_rz;
#   - centring: TRUE, around the Stress arithmetic mean;
#   - additional scaling: FALSE.
#
# This matches the corrected PCA logic used for the Control
# reference population: mean-centring without additional scaling.
# Centring changes the PCA origin but does not redefine the inherited
# robust units or alter Euclidean pairwise distances.
#
# Traceability:
#   Author + ID
#
# IMPORTANT:
#   - No global normalisation.
#   - No Stress-specific normalisation.
#   - No SD re-standardisation.
#   - No scale().
#   - PCA is descriptive and does not define communities.
#   - Louvain must later operate on the full four-dimensional
#     inherited robust space, not on PC1-PC2 alone.
#   - No resilient-like / susceptible-like labels are assigned here.
# ============================================================

script_version <- paste(
  "023_Figure2_StressPCA_CORREGIDO",
  "| 2026-08-07",
  "| Author-specific robust-coordinate and PCA audit"
)

cat(
  "\nRunning: ",
  script_version,
  "\n",
  sep = ""
)


# ------------------------------------------------------------
# 1. Project directory
# ------------------------------------------------------------

project_root <- "C:/Users/USER/Documents/LAPCOM_UCS_Resilience"

if (!dir.exists(project_root)) {
  stop(
    "Project directory not found:\n",
    project_root
  )
}

setwd(project_root)


# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "ggplot2",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before running this script:\n",
    paste(missing_packages, collapse = ", ")
  )
}

library(dplyr)
library(tibble)
library(ggplot2)
library(openxlsx)


# ------------------------------------------------------------
# 3. Fixed analytical configuration
# ------------------------------------------------------------

expected_stress_n <- 89L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"

expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

mad_scale_constant <- 1.4826

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- c(
  "distance_rz",
  "crossing_rz",
  "top_entry_rz",
  "top_time_rz"
)

variable_labels <- c(
  distance_rz = "Distance travelled",
  crossing_rz = "Line crossings",
  top_entry_rz = "Upper-zone entries",
  top_time_rz = "Upper-zone time"
)

legacy_variables <- c(
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z"
)

numeric_tolerance <- 1e-10
orthogonality_tolerance <- 1e-10
ellipse_level <- 0.95
stress_colour <- "#63B8A6"


# ------------------------------------------------------------
# 4. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(
  project_root,
  "01_Data",
  "Processed"
)

objects_dir <- file.path(
  project_root,
  "03_Results",
  "Objects"
)

tables_dir <- file.path(
  project_root,
  "03_Results",
  "Tables"
)

figures_dir <- file.path(
  project_root,
  "03_Results",
  "Figures"
)

for (directory_i in c(
  objects_dir,
  tables_dir,
  figures_dir
)) {
  if (!dir.exists(directory_i)) {
    dir.create(
      directory_i,
      recursive = TRUE
    )
  }
}

stress_input <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

setup_input <- file.path(
  objects_dir,
  "objective3_stress_architecture_setup.rds"
)

required_input_files <- c(
  stress_input,
  setup_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive Objective 3 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun 020_StressArchitecture_Setup_CORREGIDO_v3.R first."
  )
}

pdf_output <- file.path(
  figures_dir,
  "Objective3_Figure2_StressPCA.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Objective3_Figure2_StressPCA.tiff"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective3_Stress_PCA.xlsx"
)

variance_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_PCA_ExplainedVariance.csv"
)

loadings_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_PCA_Loadings.csv"
)

scores_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_PCA_Scores.csv"
)

model_output <- file.path(
  objects_dir,
  "objective3_stress_pca_model.rds"
)

scores_output <- file.path(
  objects_dir,
  "objective3_stress_pca_scores.rds"
)

results_output <- file.path(
  objects_dir,
  "objective3_stress_pca_results.rds"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_stress_pca_sessionInfo.txt"
)


# ------------------------------------------------------------
# 5. Load definitive Objective 3 inputs
# ------------------------------------------------------------

stress <- readRDS(
  stress_input
)

setup_object <- readRDS(
  setup_input
)

if (!is.data.frame(stress)) {
  stop(
    basename(stress_input),
    " does not contain a data frame."
  )
}

if (!is.list(setup_object)) {
  stop(
    basename(setup_input),
    " does not contain the expected Objective 3 setup list."
  )
}


# ------------------------------------------------------------
# 6. Audit the analytical Stress population
# ------------------------------------------------------------

required_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_columns <- setdiff(
  required_columns,
  names(stress)
)

if (length(missing_columns) > 0L) {
  stop(
    "The definitive Objective 3 dataset is missing required columns:\n",
    paste(missing_columns, collapse = ", ")
  )
}

stress <- stress %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    ID = trimws(
      as.character(ID)
    ),
    Traceability_ID = trimws(
      as.character(Traceability_ID)
    ),
    Treatment = factor(
      as.character(Treatment),
      levels = "Stress"
    )
  ) %>%
  dplyr::arrange(
    Author,
    ID
  )

expected_traceability_id <- paste(
  stress$Author,
  stress$ID,
  sep = " || "
)

if (
  nrow(stress) != expected_stress_n ||
  anyNA(
    stress[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  any(stress$Author == "") ||
  any(stress$ID == "") ||
  any(stress$Traceability_ID == "") ||
  anyDuplicated(stress$Traceability_ID) > 0L ||
  !identical(
    stress$Traceability_ID,
    expected_traceability_id
  ) ||
  !all(
    as.character(stress$Treatment) == "Stress"
  )
) {
  stop(
    "The Objective 3 Stress dataset fails the Author + ID / ",
    "Stress-only population audit."
  )
}

population_authors <- sort(
  unique(
    stress$Author
  )
)

if (
  length(population_authors) != expected_number_of_authors
) {
  stop(
    "The Stress dataset does not retain the expected four Authors."
  )
}

legacy_variables_present <- intersect(
  legacy_variables,
  names(stress)
)

if (length(legacy_variables_present) > 0L) {
  stop(
    "Legacy mean/SD-standardised variables were detected in the ",
    "definitive Objective 3 dataset:\n",
    paste(legacy_variables_present, collapse = ", ")
  )
}

analytical_variables <- c(
  behavioural_variables,
  robust_variables
)

non_numeric_variables <- analytical_variables[
  !vapply(
    stress[analytical_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following analytical variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

analytical_matrix <- as.matrix(
  stress[
    analytical_variables
  ]
)

storage.mode(
  analytical_matrix
) <- "double"

if (any(!is.finite(analytical_matrix))) {
  stop(
    "Missing or non-finite values were detected in the raw or ",
    "robust behavioural variables."
  )
}

stress_matrix <- as.matrix(
  stress[
    robust_variables
  ]
)

storage.mode(
  stress_matrix
) <- "double"

rownames(
  stress_matrix
) <- stress$Traceability_ID

colnames(
  stress_matrix
) <- robust_variables

if (
  !identical(
    dim(stress_matrix),
    c(
      expected_stress_n,
      length(robust_variables)
    )
  )
) {
  stop(
    "The Stress behavioural matrix must have dimensions 89 x 4."
  )
}

variable_standard_deviations <- apply(
  stress_matrix,
  2L,
  stats::sd
)

if (
  any(
    !is.finite(
      variable_standard_deviations
    )
  ) ||
  any(
    variable_standard_deviations <= 0
  )
) {
  stop(
    "At least one behavioural variable has zero or invalid variance."
  )
}


# ------------------------------------------------------------
# 7. Audit dependence on corrected Script 020
# ------------------------------------------------------------

required_setup_elements <- c(
  "Scientific_question",
  "Operational_question",
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Reference_space",
  "Normalisation",
  "Normalisation_formula",
  "Behavioural_variables",
  "Robust_variables",
  "Normalisation_parameters",
  "Author_group_variable_normalisation_validation",
  "Stress_matrix_rank",
  "Downstream_constraint"
)

missing_setup_elements <- setdiff(
  required_setup_elements,
  names(setup_object)
)

if (length(missing_setup_elements) > 0L) {
  stop(
    "The corrected Objective 3 setup object is missing:\n",
    paste(missing_setup_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(
      setup_object[["Analytical_population"]]
    ),
    "Stress only"
  )
) {
  stop(
    "The setup object does not define Stress as the sole analytical population."
  )
}

if (
  as.integer(
    setup_object[["Stress_n"]]
  ) != expected_stress_n
) {
  stop(
    "The setup object does not contain the expected Stress n = 89."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Traceability"]]
    ),
    expected_traceability
  )
) {
  stop(
    "The setup object does not preserve Author + ID traceability."
  )
}

if (
  !identical(
    sort(
      as.character(
        setup_object[["Authors"]]
      )
    ),
    population_authors
  )
) {
  stop(
    "The Authors in the Stress dataset do not match corrected Script 020."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Behavioural_variables"]]
    ),
    behavioural_variables
  ) ||
  !identical(
    as.character(
      setup_object[["Robust_variables"]]
    ),
    robust_variables
  )
) {
  stop(
    "The behavioural variables differ from the corrected Objective 3 setup."
  )
}

if (
  !identical(
    as.character(
      setup_object[["Normalisation"]]
    ),
    expected_normalisation
  )
) {
  stop(
    "The inherited normalisation does not match the corrected ",
    "Author-specific robust coordinate system."
  )
}


# ------------------------------------------------------------
# 8. Validate the 16 Author x variable reference parameters
# ------------------------------------------------------------

normalisation_parameters <-
  setup_object[["Normalisation_parameters"]]

if (!is.data.frame(normalisation_parameters)) {
  stop(
    "setup_object$Normalisation_parameters is not a data frame."
  )
}

required_parameter_columns <- c(
  "Author",
  "Variable",
  "n",
  "Median",
  "MAD_raw",
  "MAD_scaled"
)

missing_parameter_columns <- setdiff(
  required_parameter_columns,
  names(normalisation_parameters)
)

if (length(missing_parameter_columns) > 0L) {
  stop(
    "The inherited Author-specific parameter table is missing:\n",
    paste(missing_parameter_columns, collapse = ", ")
  )
}

normalisation_parameters <- normalisation_parameters %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    Variable = as.character(
      Variable
    )
  ) %>%
  dplyr::arrange(
    Author,
    match(
      Variable,
      behavioural_variables
    )
  )

expected_parameter_rows <-
  expected_number_of_authors *
  length(behavioural_variables)

if (
  nrow(normalisation_parameters) != expected_parameter_rows ||
  any(
    duplicated(
      normalisation_parameters[
        c(
          "Author",
          "Variable"
        )
      ]
    )
  ) ||
  !setequal(
    normalisation_parameters$Author,
    population_authors
  ) ||
  !setequal(
    normalisation_parameters$Variable,
    behavioural_variables
  )
) {
  stop(
    "The corrected setup must contain exactly 16 unique ",
    "Author x variable reference parameter sets."
  )
}

parameter_numeric_columns <- c(
  "n",
  "Median",
  "MAD_raw",
  "MAD_scaled"
)

if (
  any(
    !vapply(
      normalisation_parameters[
        parameter_numeric_columns
      ],
      is.numeric,
      logical(1)
    )
  )
) {
  stop(
    "At least one Author-specific reference parameter column is not numeric."
  )
}

parameter_matrix <- as.matrix(
  normalisation_parameters[
    parameter_numeric_columns
  ]
)

storage.mode(
  parameter_matrix
) <- "double"

if (
  any(!is.finite(parameter_matrix)) ||
  any(normalisation_parameters$MAD_raw <= 0) ||
  any(normalisation_parameters$MAD_scaled <= 0)
) {
  stop(
    "At least one Author-specific reference parameter is invalid."
  )
}

if (
  any(
    abs(
      normalisation_parameters$MAD_scaled -
        mad_scale_constant *
        normalisation_parameters$MAD_raw
    ) > numeric_tolerance
  )
) {
  stop(
    "MAD_scaled does not reproduce 1.4826 x MAD_raw for at least one ",
    "Author x variable parameter set."
  )
}


# ------------------------------------------------------------
# 9. Audit inherited Author x Group x Variable validation
# ------------------------------------------------------------

normalisation_validation <-
  setup_object[[
    "Author_group_variable_normalisation_validation"
  ]]

if (!is.data.frame(normalisation_validation)) {
  stop(
    "The inherited Author x Group x Variable validation is not a data frame."
  )
}

required_validation_columns <- c(
  "Author",
  "Group",
  "Variable",
  "Robust_variable",
  "n",
  "Maximum_absolute_recalculation_difference"
)

missing_validation_columns <- setdiff(
  required_validation_columns,
  names(normalisation_validation)
)

if (length(missing_validation_columns) > 0L) {
  stop(
    "The inherited normalisation validation is missing:\n",
    paste(missing_validation_columns, collapse = ", ")
  )
}

normalisation_validation <- normalisation_validation %>%
  dplyr::mutate(
    Author = trimws(
      as.character(Author)
    ),
    Group = as.character(
      Group
    ),
    Variable = as.character(
      Variable
    ),
    Robust_variable = as.character(
      Robust_variable
    )
  )

expected_validation_rows <-
  expected_number_of_authors *
  2L *
  length(behavioural_variables)

if (
  nrow(normalisation_validation) != expected_validation_rows ||
  any(
    duplicated(
      normalisation_validation[
        c(
          "Author",
          "Group",
          "Variable"
        )
      ]
    )
  ) ||
  !setequal(
    normalisation_validation$Author,
    population_authors
  ) ||
  !setequal(
    normalisation_validation$Group,
    c(
      "Control",
      "Stress"
    )
  ) ||
  !setequal(
    normalisation_validation$Variable,
    behavioural_variables
  )
) {
  stop(
    "The inherited corrected normalisation validation must contain ",
    "exactly 32 Author x Group x Variable rows."
  )
}

recalculation_differences <-
  normalisation_validation[[
    "Maximum_absolute_recalculation_difference"
  ]]

if (
  !is.numeric(
    recalculation_differences
  ) ||
  any(
    !is.finite(
      recalculation_differences
    )
  ) ||
  any(
    recalculation_differences >
      numeric_tolerance
  )
) {
  stop(
    "The corrected setup does not confirm exact Author-specific ",
    "reproduction of the inherited robust coordinates."
  )
}


# ------------------------------------------------------------
# 10. Reconstruct Stress robust scores using each fish's Author
# ------------------------------------------------------------

normalisation_recalculation_rows <- vector(
  mode = "list",
  length =
    expected_number_of_authors *
    length(behavioural_variables)
)

audit_index <- 0L

for (author_i in population_authors) {
  author_rows <- stress$Author == author_i

  if (!any(author_rows)) {
    stop(
      "No Stress observations found for Author ",
      author_i,
      "."
    )
  }

  for (variable_i in behavioural_variables) {
    robust_variable_i <- paste0(
      variable_i,
      "_rz"
    )

    parameter_row <- normalisation_parameters[
      normalisation_parameters$Author == author_i &
        normalisation_parameters$Variable == variable_i,
      ,
      drop = FALSE
    ]

    if (nrow(parameter_row) != 1L) {
      stop(
        "Exactly one reference parameter row is required for Author ",
        author_i,
        " and variable ",
        variable_i,
        "."
      )
    }

    expected_values <- (
      stress[[variable_i]][author_rows] -
        parameter_row$Median[[1]]
    ) / parameter_row$MAD_scaled[[1]]

    observed_values <-
      stress[[robust_variable_i]][author_rows]

    maximum_difference <- max(
      abs(
        observed_values -
          expected_values
      )
    )

    audit_index <- audit_index + 1L

    normalisation_recalculation_rows[[
      audit_index
    ]] <- tibble::tibble(
      Author = author_i,
      Variable = variable_i,
      Robust_variable = robust_variable_i,
      n = sum(author_rows),
      Maximum_absolute_recalculation_difference =
        maximum_difference
    )
  }
}

normalisation_recalculation_audit <-
  dplyr::bind_rows(
    normalisation_recalculation_rows
  ) %>%
  dplyr::arrange(
    Author,
    match(
      Variable,
      behavioural_variables
    )
  )

if (
  nrow(normalisation_recalculation_audit) !=
    expected_number_of_authors *
    length(behavioural_variables) ||
  any(
    normalisation_recalculation_audit[[
      "Maximum_absolute_recalculation_difference"
    ]] > numeric_tolerance
  )
) {
  stop(
    "At least one Stress robust coordinate does not reproduce the ",
    "Author-specific Control-referenced transformation."
  )
}


# ------------------------------------------------------------
# 11. Audit the centred Stress matrix rank
# ------------------------------------------------------------

stress_column_means <- colMeans(
  stress_matrix
)

centered_stress_matrix <- sweep(
  stress_matrix,
  2L,
  stress_column_means,
  FUN = "-"
)

stress_matrix_rank <- qr(
  centered_stress_matrix
)$rank

if (
  stress_matrix_rank !=
    length(robust_variables)
) {
  stop(
    "The centred Stress behavioural matrix does not have full rank: ",
    stress_matrix_rank,
    " of ",
    length(robust_variables),
    "."
  )
}

if (
  as.integer(
    setup_object[["Stress_matrix_rank"]]
  ) != stress_matrix_rank
) {
  stop(
    "The centred Stress matrix rank differs from the corrected ",
    "Script 020 setup audit."
  )
}


# ------------------------------------------------------------
# 12. Fit the de novo Stress PCA without additional scaling
# ------------------------------------------------------------

pca_stress <- stats::prcomp(
  stress_matrix,
  center = TRUE,
  scale. = FALSE,
  retx = TRUE
)

if (!inherits(
  pca_stress,
  "prcomp"
)) {
  stop(
    "The PCA model was not created successfully."
  )
}

if (
  isFALSE(
    pca_stress$center
  ) ||
  !isFALSE(
    pca_stress$scale
  )
) {
  stop(
    "The PCA specification is incorrect: centring must be TRUE and ",
    "additional scaling must be FALSE."
  )
}

if (
  !isTRUE(
    all.equal(
      as.numeric(
        pca_stress$center
      ),
      as.numeric(
        stress_column_means
      ),
      tolerance = numeric_tolerance,
      check.attributes = FALSE
    )
  )
) {
  stop(
    "The PCA centring vector does not reproduce the arithmetic ",
    "Stress means."
  )
}

if (
  !identical(
    dim(
      pca_stress$x
    ),
    c(
      expected_stress_n,
      4L
    )
  ) ||
  !identical(
    dim(
      pca_stress$rotation
    ),
    c(
      4L,
      4L
    )
  )
) {
  stop(
    "The PCA score or rotation matrix has unexpected dimensions."
  )
}

if (
  any(
    !is.finite(
      pca_stress$x
    )
  ) ||
  any(
    !is.finite(
      pca_stress$rotation
    )
  ) ||
  any(
    !is.finite(
      pca_stress$sdev
    )
  )
) {
  stop(
    "The PCA model contains missing or non-finite values."
  )
}


# ------------------------------------------------------------
# 13. Apply deterministic component orientation
# ------------------------------------------------------------

orientation_multipliers <- rep(
  1,
  ncol(
    pca_stress$rotation
  )
)

names(
  orientation_multipliers
) <- colnames(
  pca_stress$rotation
)

for (
  component_i in seq_len(
    ncol(
      pca_stress$rotation
    )
  )
) {
  anchor_index <- which.max(
    abs(
      pca_stress$rotation[
        ,
        component_i
      ]
    )
  )

  if (
    pca_stress$rotation[
      anchor_index,
      component_i
    ] < 0
  ) {
    orientation_multipliers[
      component_i
    ] <- -1
  }
}

pca_stress$rotation <- sweep(
  pca_stress$rotation,
  2L,
  orientation_multipliers,
  FUN = "*"
)

pca_stress$x <- sweep(
  pca_stress$x,
  2L,
  orientation_multipliers,
  FUN = "*"
)


# ------------------------------------------------------------
# 14. Audit the PCA decomposition
# ------------------------------------------------------------

component_names <- paste0(
  "PC",
  seq_len(
    ncol(
      stress_matrix
    )
  )
)

colnames(
  pca_stress$x
) <- component_names

colnames(
  pca_stress$rotation
) <- component_names

rownames(
  pca_stress$rotation
) <- robust_variables

maximum_reconstruction_error <- max(
  abs(
    centered_stress_matrix -
      pca_stress$x %*%
      t(
        pca_stress$rotation
      )
  )
)

if (
  !is.finite(
    maximum_reconstruction_error
  ) ||
  maximum_reconstruction_error >
    numeric_tolerance
) {
  stop(
    "The PCA decomposition does not reconstruct the centred Stress ",
    "matrix within tolerance. Maximum error = ",
    format(
      maximum_reconstruction_error,
      scientific = TRUE
    )
  )
}

rotation_crossproduct <- crossprod(
  pca_stress$rotation
)

maximum_orthogonality_error <- max(
  abs(
    rotation_crossproduct -
      diag(
        ncol(
          pca_stress$rotation
        )
      )
  )
)

if (
  !is.finite(
    maximum_orthogonality_error
  ) ||
  maximum_orthogonality_error >
    orthogonality_tolerance
) {
  stop(
    "The PCA rotation matrix is not orthonormal within tolerance. ",
    "Maximum error = ",
    format(
      maximum_orthogonality_error,
      scientific = TRUE
    )
  )
}

score_covariance <- stats::cov(
  pca_stress$x
)

score_covariance_off_diagonal <-
  score_covariance

diag(
  score_covariance_off_diagonal
) <- 0

maximum_score_covariance <- max(
  abs(
    score_covariance_off_diagonal
  )
)

if (
  !is.finite(
    maximum_score_covariance
  ) ||
  maximum_score_covariance >
    1e-10
) {
  stop(
    "The PCA component scores are not mutually uncorrelated within ",
    "numerical tolerance. Maximum covariance = ",
    format(
      maximum_score_covariance,
      scientific = TRUE
    )
  )
}


# ------------------------------------------------------------
# 15. Derive PCA tables
# ------------------------------------------------------------

eigenvalues <- pca_stress$sdev^2

percentage_of_variance <-
  100 *
  eigenvalues /
  sum(
    eigenvalues
  )

cumulative_variance <- cumsum(
  percentage_of_variance
)

if (
  any(
    !is.finite(
      percentage_of_variance
    )
  ) ||
  abs(
    sum(
      percentage_of_variance
    ) -
      100
  ) >
    1e-10
) {
  stop(
    "The PCA explained-variance percentages are invalid."
  )
}

pca_variance <- tibble::tibble(
  Component =
    component_names,
  Eigenvalue =
    as.numeric(
      eigenvalues
    ),
  Percentage_of_variance =
    as.numeric(
      percentage_of_variance
    ),
  Cumulative_variance_percent =
    as.numeric(
      cumulative_variance
    )
)

pca_loadings <- tibble::tibble(
  Variable =
    robust_variables,
  Variable_label =
    unname(
      variable_labels[
        robust_variables
      ]
    )
) %>%
  dplyr::bind_cols(
    tibble::as_tibble(
      pca_stress$rotation,
      .name_repair = "minimal"
    )
  )

pca_contributions <- tibble::tibble(
  Variable =
    robust_variables,
  Variable_label =
    unname(
      variable_labels[
        robust_variables
      ]
    )
) %>%
  dplyr::bind_cols(
    tibble::as_tibble(
      100 *
        pca_stress$rotation^2,
      .name_repair = "minimal"
    )
  )

contribution_sums <- colSums(
  pca_contributions[
    ,
    component_names,
    drop = FALSE
  ]
)

if (
  any(
    abs(
      contribution_sums -
        100
    ) >
      1e-10
  )
) {
  stop(
    "The variable contributions do not sum to 100% for every PC."
  )
}

variable_pc_correlations <- stats::cor(
  stress_matrix,
  pca_stress$x
)

pca_variable_correlations <- tibble::tibble(
  Variable =
    robust_variables,
  Variable_label =
    unname(
      variable_labels[
        robust_variables
      ]
    )
) %>%
  dplyr::bind_cols(
    tibble::as_tibble(
      variable_pc_correlations,
      .name_repair = "minimal"
    )
  )

pca_scores <- tibble::tibble(
  Author =
    stress$Author,
  ID =
    stress$ID,
  Traceability_ID =
    stress$Traceability_ID,
  Treatment =
    as.character(
      stress$Treatment
    )
) %>%
  dplyr::bind_cols(
    tibble::as_tibble(
      pca_stress$x,
      .name_repair = "minimal"
    )
  )

if (
  nrow(pca_scores) != expected_stress_n ||
  anyNA(
    pca_scores[
      c(
        "Author",
        "ID",
        "Traceability_ID",
        "Treatment"
      )
    ]
  ) ||
  anyDuplicated(
    pca_scores$Traceability_ID
  ) > 0L ||
  !identical(
    pca_scores$Traceability_ID,
    stress$Traceability_ID
  ) ||
  !all(
    pca_scores$Treatment == "Stress"
  )
) {
  stop(
    "The PCA score table fails the Author + ID traceability audit."
  )
}

pca_score_matrix <- as.matrix(
  pca_scores[
    component_names
  ]
)

storage.mode(
  pca_score_matrix
) <- "double"

if (
  any(
    !is.finite(
      pca_score_matrix
    )
  )
) {
  stop(
    "The PCA score table contains non-finite values."
  )
}


# ------------------------------------------------------------
# 16. Construct the 95% normal-theory PC1-PC2 data ellipse
# ------------------------------------------------------------

pc12_scores <- as.matrix(
  pca_scores[
    c(
      "PC1",
      "PC2"
    )
  ]
)

storage.mode(
  pc12_scores
) <- "double"

pc12_centroid <- colMeans(
  pc12_scores
)

pc12_covariance <- stats::cov(
  pc12_scores
)

pc12_eigendecomposition <- eigen(
  pc12_covariance,
  symmetric = TRUE
)

if (
  any(
    !is.finite(
      pc12_covariance
    )
  ) ||
  any(
    pc12_eigendecomposition$values <= 0
  )
) {
  stop(
    "The PC1-PC2 covariance matrix is not positive definite."
  )
}

ellipse_angles <- seq(
  0,
  2 * pi,
  length.out = 361L
)

ellipse_unit_circle <- rbind(
  cos(
    ellipse_angles
  ),
  sin(
    ellipse_angles
  )
)

ellipse_transform <-
  pc12_eigendecomposition$vectors %*%
  diag(
    sqrt(
      pc12_eigendecomposition$values
    ),
    nrow = 2L,
    ncol = 2L
  )

ellipse_radius <- sqrt(
  stats::qchisq(
    ellipse_level,
    df = 2L
  )
)

ellipse_offsets <-
  ellipse_radius *
  ellipse_transform %*%
  ellipse_unit_circle

ellipse_coordinates <- t(
  sweep(
    ellipse_offsets,
    1L,
    pc12_centroid,
    FUN = "+"
  )
)

ellipse_data <- tibble::tibble(
  Ellipse_order =
    seq_len(
      nrow(
        ellipse_coordinates
      )
    ),
  PC1 =
    ellipse_coordinates[
      ,
      1L
    ],
  PC2 =
    ellipse_coordinates[
      ,
      2L
    ]
)

pc12_mahalanobis_squared <- stats::mahalanobis(
  pc12_scores,
  center = pc12_centroid,
  cov = pc12_covariance
)

ellipse_threshold <- stats::qchisq(
  ellipse_level,
  df = 2L
)

ellipse_inside <-
  pc12_mahalanobis_squared <=
  ellipse_threshold

ellipse_inside_n <- sum(
  ellipse_inside
)

ellipse_inside_percent <-
  100 *
  mean(
    ellipse_inside
  )

ellipse_diagnostics <- tibble::tibble(
  Level =
    ellipse_level,
  PC1_centroid =
    unname(
      pc12_centroid[
        1L
      ]
    ),
  PC2_centroid =
    unname(
      pc12_centroid[
        2L
      ]
    ),
  Covariance_PC1 =
    pc12_covariance[
      1L,
      1L
    ],
  Covariance_PC2 =
    pc12_covariance[
      2L,
      2L
    ],
  Covariance_PC1_PC2 =
    pc12_covariance[
      1L,
      2L
    ],
  Individuals_inside =
    ellipse_inside_n,
  Total_individuals =
    expected_stress_n,
  Percent_inside =
    ellipse_inside_percent
)


# ------------------------------------------------------------
# 17. Create Figure 2
# ------------------------------------------------------------

pc1_percent <-
  percentage_of_variance[
    1L
  ]

pc2_percent <-
  percentage_of_variance[
    2L
  ]

pc12_percent <-
  pc1_percent +
  pc2_percent

pca_plot <- ggplot2::ggplot() +
  ggplot2::geom_polygon(
    data =
      ellipse_data,
    mapping =
      ggplot2::aes(
        x = PC1,
        y = PC2
      ),
    fill =
      stress_colour,
    alpha =
      0.16,
    colour =
      NA
  ) +
  ggplot2::geom_path(
    data =
      ellipse_data,
    mapping =
      ggplot2::aes(
        x = PC1,
        y = PC2
      ),
    colour =
      stress_colour,
    linewidth =
      0.95
  ) +
  ggplot2::geom_point(
    data =
      pca_scores,
    mapping =
      ggplot2::aes(
        x = PC1,
        y = PC2
      ),
    shape =
      21,
    size =
      2.6,
    stroke =
      0.35,
    fill =
      stress_colour,
    colour =
      "black",
    alpha =
      0.82
  ) +
  ggplot2::labs(
    x = paste0(
      "PC1 (",
      format(
        round(
          pc1_percent,
          1
        ),
        nsmall = 1
      ),
      "%)"
    ),
    y = paste0(
      "PC2 (",
      format(
        round(
          pc2_percent,
          1
        ),
        nsmall = 1
      ),
      "%)"
    )
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_classic(
    base_size = 12,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    panel.border =
      ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.6
      ),
    axis.title =
      ggplot2::element_text(
        colour = "black"
      ),
    axis.text =
      ggplot2::element_text(
        colour = "black"
      ),
    plot.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
    panel.background =
      ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
    plot.margin =
      ggplot2::margin(
        8,
        10,
        8,
        8
      )
  )


# ------------------------------------------------------------
# 18. Prepare export metadata
# ------------------------------------------------------------

metadata_export <- data.frame(
  Field = c(
    "Objective",
    "Scientific question",
    "Operational question",
    "Analytical population",
    "Stress n",
    "Traceability",
    "Authors/cohorts",
    "Reference space",
    "Normalisation",
    "PCA fitted de novo within Stress",
    "PCA centred",
    "PCA additionally scaled",
    "Control-method consistency",
    "PC1 variance percent",
    "PC2 variance percent",
    "PC1-PC2 cumulative variance percent",
    "Ellipse type",
    "Ellipse level",
    "Interpretive constraint"
  ),
  Value = c(
    "Objective 3",
    as.character(
      setup_object[["Scientific_question"]]
    ),
    as.character(
      setup_object[["Operational_question"]]
    ),
    "Stress only",
    as.character(
      expected_stress_n
    ),
    expected_traceability,
    paste(
      population_authors,
      collapse = ", "
    ),
    as.character(
      setup_object[["Reference_space"]]
    ),
    expected_normalisation,
    "Yes",
    "Yes; arithmetic Stress mean",
    "No; inherited robust units preserved",
    paste(
      "Same PCA specification used for the Control architecture:",
      "mean-centring with no additional scaling"
    ),
    as.character(
      pc1_percent
    ),
    as.character(
      pc2_percent
    ),
    as.character(
      pc12_percent
    ),
    "95% normal-theory data ellipse; not a confidence region",
    as.character(
      ellipse_level
    ),
    paste(
      "PCA describes dominant internal variation before clustering;",
      "it does not define discrete post-stress phenotypes"
    )
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ------------------------------------------------------------
# 19. Save tables and reusable PCA objects
# ------------------------------------------------------------

openxlsx::write.xlsx(
  x = list(
    Explained_variance =
      as.data.frame(
        pca_variance
      ),
    Loadings =
      as.data.frame(
        pca_loadings
      ),
    Contributions_percent =
      as.data.frame(
        pca_contributions
      ),
    Variable_PC_correlations =
      as.data.frame(
        pca_variable_correlations
      ),
    Scores =
      as.data.frame(
        pca_scores
      ),
    Author_recalculation_audit =
      as.data.frame(
        normalisation_recalculation_audit
      ),
    Ellipse_diagnostics =
      as.data.frame(
        ellipse_diagnostics
      ),
    Metadata =
      metadata_export
  ),
  file =
    xlsx_output,
  overwrite =
    TRUE
)

utils::write.csv(
  pca_variance,
  variance_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  pca_loadings,
  loadings_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  pca_scores,
  scores_csv_output,
  row.names = FALSE,
  na = ""
)

pca_results <- list(
  Objective =
    "Objective 3",
  Scientific_question =
    setup_object[["Scientific_question"]],
  Operational_question =
    setup_object[["Operational_question"]],
  Analytical_population =
    "Stress only",
  Stress_n =
    expected_stress_n,
  Authors =
    population_authors,
  Traceability =
    expected_traceability,
  Reference_space =
    setup_object[["Reference_space"]],
  Normalisation =
    setup_object[["Normalisation"]],
  Normalisation_formula =
    setup_object[["Normalisation_formula"]],
  Behavioural_variables =
    behavioural_variables,
  Robust_variables =
    robust_variables,
  Variable_labels =
    variable_labels,
  Author_specific_normalisation_recalculation_audit =
    normalisation_recalculation_audit,
  Stress_matrix_rank =
    stress_matrix_rank,
  PCA_scope =
    "De novo within the Stress population",
  PCA_center =
    TRUE,
  PCA_center_values =
    pca_stress$center,
  PCA_scale =
    FALSE,
  PCA_method_consistency =
    "Mean-centred, no additional scaling, matching corrected Control PCA logic",
  PCA_model =
    pca_stress,
  PCA_variance =
    pca_variance,
  PCA_loadings =
    pca_loadings,
  PCA_contributions_percent =
    pca_contributions,
  Variable_PC_correlations =
    pca_variable_correlations,
  PCA_scores =
    pca_scores,
  Orientation_multipliers =
    orientation_multipliers,
  Maximum_reconstruction_error =
    maximum_reconstruction_error,
  Maximum_orthogonality_error =
    maximum_orthogonality_error,
  Maximum_score_covariance =
    maximum_score_covariance,
  PC1_PC2_cumulative_variance_percent =
    pc12_percent,
  Ellipse_level =
    ellipse_level,
  Ellipse_coordinates =
    ellipse_data,
  Ellipse_diagnostics =
    ellipse_diagnostics,
  Source_stress_file =
    stress_input,
  Source_setup_file =
    setup_input,
  Downstream_constraint = paste(
    "Louvain must operate on the complete four-dimensional",
    "inherited robust Stress coordinates, not on PC1-PC2 alone"
  ),
  Interpretive_constraint = paste(
    "PCA describes dominant internal variation within Stress before",
    "community detection and does not establish discrete resilient-like",
    "or susceptible-like phenotypes"
  )
)

saveRDS(
  pca_stress,
  model_output
)

saveRDS(
  pca_scores,
  scores_output
)

saveRDS(
  pca_results,
  results_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)


# ------------------------------------------------------------
# 20. Save Figure 2
# ------------------------------------------------------------

if (!capabilities("cairo")) {
  stop(
    "Cairo graphics support is required to export the PDF with the ",
    "Arial font. This R installation reports capabilities('cairo') = FALSE."
  )
}

ggplot2::ggsave(
  filename =
    pdf_output,
  plot =
    pca_plot,
  device =
    grDevices::cairo_pdf,
  width =
    7,
  height =
    6,
  units =
    "in",
  bg =
    "white"
)

ggplot2::ggsave(
  filename =
    tiff_output,
  plot =
    pca_plot,
  device =
    "tiff",
  width =
    7,
  height =
    6,
  units =
    "in",
  dpi =
    600,
  compression =
    "lzw",
  bg =
    "white"
)


# ------------------------------------------------------------
# 21. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  pdf_output,
  tiff_output,
  xlsx_output,
  variance_csv_output,
  loadings_csv_output,
  scores_csv_output,
  model_output,
  scores_output,
  results_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(
    expected_output_files
  )
]

if (
  length(
    missing_output_files
  ) > 0L
) {
  stop(
    "The following expected outputs were not created:\n",
    paste(
      missing_output_files,
      collapse = "\n"
    )
  )
}

output_file_information <- file.info(
  expected_output_files
)

if (
  any(
    is.na(
      output_file_information$size
    )
  ) ||
  any(
    output_file_information$size <= 0
  )
) {
  stop(
    "At least one expected output file is empty or inaccessible."
  )
}


# ------------------------------------------------------------
# 22. Final console audit
# ------------------------------------------------------------

pc1_loading_order <- order(
  abs(
    pca_stress$rotation[
      ,
      "PC1"
    ]
  ),
  decreasing = TRUE
)

pc2_loading_order <- order(
  abs(
    pca_stress$rotation[
      ,
      "PC2"
    ]
  ),
  decreasing = TRUE
)

cat(
  "\n============================================================\n"
)

cat(
  "OBJECTIVE 3 - CORRECTED STRESS PCA\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nAnalytical population: Stress only (n = ",
  expected_stress_n,
  ")\n",
  sep = ""
)

cat(
  "Traceability: ",
  expected_traceability,
  "\n",
  sep = ""
)

cat(
  "Authors/cohorts retained: ",
  paste(
    population_authors,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Analytical scale: Author-specific Control-referenced robust ",
  "coordinates inherited unchanged from corrected Objectives 1 and 2.\n",
  sep = ""
)

cat(
  "Normalisation: ",
  expected_normalisation,
  "\n",
  sep = ""
)

cat(
  "\nAuthor-specific robust-score recalculation audit:\n"
)

print(
  normalisation_recalculation_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nMaximum robust-score recalculation difference: ",
  format(
    max(
      normalisation_recalculation_audit[[
        "Maximum_absolute_recalculation_difference"
      ]]
    ),
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nPCA specification: fitted de novo within Stress, centred = TRUE, ",
  "additional scaling = FALSE.\n",
  sep = ""
)

cat(
  "This is the same PCA specification used for the corrected Control ",
  "architecture: arithmetic mean-centring with no additional scaling.\n",
  sep = ""
)

cat(
  "Centred Stress behavioural-matrix rank: ",
  stress_matrix_rank,
  " of ",
  length(
    robust_variables
  ),
  "\n",
  sep = ""
)

cat(
  "Maximum PCA reconstruction error: ",
  format(
    maximum_reconstruction_error,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Maximum rotation orthogonality error: ",
  format(
    maximum_orthogonality_error,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Maximum off-diagonal covariance between PC scores: ",
  format(
    maximum_score_covariance,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nExplained variance:\n"
)

print(
  pca_variance,
  n = Inf,
  width = Inf
)

cat(
  "\nPCA loadings:\n"
)

print(
  pca_loadings,
  n = Inf,
  width = Inf
)

cat(
  "\nVariable contributions (%):\n"
)

print(
  pca_contributions,
  n = Inf,
  width = Inf
)

cat(
  "\nVariable-PC correlations:\n"
)

print(
  pca_variable_correlations,
  n = Inf,
  width = Inf
)

cat(
  "\nPC1-PC2 cumulative explained variance: ",
  sprintf(
    "%.2f",
    pc12_percent
  ),
  "%.\n",
  sep = ""
)

cat(
  "Individuals inside the 95% normal-theory PC1-PC2 data ellipse: ",
  ellipse_inside_n,
  " of ",
  expected_stress_n,
  " (",
  sprintf(
    "%.1f",
    ellipse_inside_percent
  ),
  "%).\n",
  sep = ""
)

cat(
  "\nLargest absolute PC1 loading: ",
  unname(
    variable_labels[
      rownames(
        pca_stress$rotation
      )[
        pc1_loading_order[
          1L
        ]
      ]
    ]
  ),
  " (loading = ",
  sprintf(
    "%.3f",
    pca_stress$rotation[
      pc1_loading_order[
        1L
      ],
      "PC1"
    ]
  ),
  ").\n",
  sep = ""
)

cat(
  "Largest absolute PC2 loading: ",
  unname(
    variable_labels[
      rownames(
        pca_stress$rotation
      )[
        pc2_loading_order[
          1L
        ]
      ]
    ]
  ),
  " (loading = ",
  sprintf(
    "%.3f",
    pca_stress$rotation[
      pc2_loading_order[
        1L
      ],
      "PC2"
    ]
  ),
  ").\n",
  sep = ""
)

cat(
  "\nInterpretive constraint: this PCA summarises dominant internal ",
  "variation within Stress. It does not, by itself, demonstrate ",
  "discrete resilient-like or susceptible-like phenotypes.\n",
  sep = ""
)

cat(
  "The PC1-PC2 display must not replace the complete four-dimensional ",
  "robust behavioural space for de novo Louvain community detection.\n",
  sep = ""
)

cat(
  "\nOutputs created:\n"
)

for (
  output_file in expected_output_files
) {
  cat(
    normalizePath(
      output_file,
      winslash = "/",
      mustWork = FALSE
    ),
    "\n",
    sep = ""
  )
}

cat(
  "\n============================================================\n"
)
