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

project_root <- normalizePath(
  Sys.getenv(
    "LAPCOM_UCS_PROJECT_ROOT",
    unset = getwd()
  ),
  winslash = "/",
  mustWork = TRUE
)

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
