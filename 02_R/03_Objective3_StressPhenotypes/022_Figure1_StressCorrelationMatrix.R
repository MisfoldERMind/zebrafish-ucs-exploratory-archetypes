# ============================================================
# 022_Figure1_StressCorrelationMatrix_CORREGIDO.R
#
# Objective 3
# Internal behavioural architecture of the UCS-exposed population
# Figure 1: correlation matrix of the Stress population
#
# Scientific question:
#   What internal behavioural structure emerges within the
#   UCS-exposed population?
#
# Purpose:
#   Quantify and visualise the pre-clustering dependence structure
#   among the four behavioural variables in the 89 stress-exposed
#   fish.
#
# Required upstream script:
#   020_StressArchitecture_Setup_CORREGIDO_v3.R
#
# Definitive analytical scale:
#   Author-specific Control-referenced robust coordinates inherited
#   unchanged from Objectives 1 and 2:
#
#     rz = (x - Median_control_within_Author) /
#          (1.4826 * MAD_raw_control_within_Author)
#
# Traceability:
#   Author + ID
#
# Correlations:
#   Spearman rank correlations.
#   Holm correction across the six unique pairwise tests.
#
# IMPORTANT:
#   - No global normalisation.
#   - No Stress-specific normalisation.
#   - No scale().
#   - No community detection.
#   - No resilient-like / susceptible-like labels.
# ============================================================

script_version <- paste(
  "022_Figure1_StressCorrelationMatrix_CORREGIDO",
  "| 2026-08-07",
  "| Author-specific robust-coordinate audit"
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

project_root <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

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
  "corrplot",
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
library(corrplot)
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
numeric_tolerance <- 1e-10
significance_level <- 0.05

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
  "Objective3_Figure1_StressCorrelationMatrix.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Objective3_Figure1_StressCorrelationMatrix.tiff"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective3_Stress_CorrelationMatrix.xlsx"
)

pairwise_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_Correlation_PairwiseResults.csv"
)

object_output <- file.path(
  objects_dir,
  "objective3_stress_correlation_matrix.rds"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_stress_correlation_matrix_sessionInfo.txt"
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

unique_value_counts <- vapply(
  seq_len(
    ncol(stress_matrix)
  ),
  function(j) {
    length(
      unique(
        stress_matrix[, j]
      )
    )
  },
  integer(1)
)

names(
  unique_value_counts
) <- robust_variables

if (
  any(
    unique_value_counts < 2L
  )
) {
  stop(
    "At least one behavioural variable has fewer than two unique ",
    "values and cannot be correlated."
  )
}

variable_standard_deviations <- apply(
  stress_matrix,
  2,
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
    "Author x variable reference parameter set."
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
# 11. Compute Spearman correlations and pairwise tests
# ------------------------------------------------------------

cor_matrix_stress <- stats::cor(
  stress_matrix,
  method = "spearman",
  use = "everything"
)

pair_indices <- utils::combn(
  seq_along(
    robust_variables
  ),
  2L
)

pairwise_results_list <- vector(
  "list",
  ncol(pair_indices)
)

for (
  pair_i in seq_len(
    ncol(pair_indices)
  )
) {
  variable_1_index <-
    pair_indices[1L, pair_i]

  variable_2_index <-
    pair_indices[2L, pair_i]

  variable_1 <-
    robust_variables[variable_1_index]

  variable_2 <-
    robust_variables[variable_2_index]

  test_result <- suppressWarnings(
    stats::cor.test(
      stress_matrix[
        ,
        variable_1_index
      ],
      stress_matrix[
        ,
        variable_2_index
      ],
      method = "spearman",
      exact = FALSE
    )
  )

  pairwise_results_list[[
    pair_i
  ]] <- tibble::tibble(
    Variable_1 = variable_1,
    Variable_1_label =
      unname(
        variable_labels[
          variable_1
        ]
      ),
    Variable_2 = variable_2,
    Variable_2_label =
      unname(
        variable_labels[
          variable_2
        ]
      ),
    n = expected_stress_n,
    Spearman_rho =
      as.numeric(
        unname(
          test_result$estimate
        )
      ),
    P_raw =
      as.numeric(
        test_result$p.value
      )
  )
}

pairwise_results <- dplyr::bind_rows(
  pairwise_results_list
)

pairwise_results <- pairwise_results %>%
  dplyr::mutate(
    P_Holm = stats::p.adjust(
      P_raw,
      method = "holm"
    ),
    Significant_Holm =
      P_Holm <
      significance_level
  )

p_matrix_raw <- matrix(
  0,
  nrow = length(robust_variables),
  ncol = length(robust_variables),
  dimnames = list(
    robust_variables,
    robust_variables
  )
)

p_matrix_holm <- matrix(
  0,
  nrow = length(robust_variables),
  ncol = length(robust_variables),
  dimnames = list(
    robust_variables,
    robust_variables
  )
)

for (
  pair_i in seq_len(
    nrow(pairwise_results)
  )
) {
  variable_1_index <- match(
    pairwise_results$Variable_1[
      pair_i
    ],
    robust_variables
  )

  variable_2_index <- match(
    pairwise_results$Variable_2[
      pair_i
    ],
    robust_variables
  )

  p_matrix_raw[
    variable_1_index,
    variable_2_index
  ] <- pairwise_results$P_raw[
    pair_i
  ]

  p_matrix_raw[
    variable_2_index,
    variable_1_index
  ] <- pairwise_results$P_raw[
    pair_i
  ]

  p_matrix_holm[
    variable_1_index,
    variable_2_index
  ] <- pairwise_results$P_Holm[
    pair_i
  ]

  p_matrix_holm[
    variable_2_index,
    variable_1_index
  ] <- pairwise_results$P_Holm[
    pair_i
  ]
}


# ------------------------------------------------------------
# 12. Validate correlation and p-value results
# ------------------------------------------------------------

validate_symmetric_matrix <- function(
    matrix_object,
    expected_diagonal,
    lower_limit,
    upper_limit,
    object_name
) {
  matrix_object <- as.matrix(
    matrix_object
  )

  if (
    !identical(
      dim(matrix_object),
      c(
        4L,
        4L
      )
    )
  ) {
    stop(
      object_name,
      " does not have dimensions 4 x 4."
    )
  }

  if (
    !identical(
      rownames(matrix_object),
      robust_variables
    ) ||
    !identical(
      colnames(matrix_object),
      robust_variables
    )
  ) {
    stop(
      object_name,
      " has incorrect variable names or order."
    )
  }

  if (
    any(
      !is.finite(
        matrix_object
      )
    )
  ) {
    stop(
      object_name,
      " contains non-finite values."
    )
  }

  if (
    max(
      abs(
        matrix_object -
          t(matrix_object)
      )
    ) >
      numeric_tolerance
  ) {
    stop(
      object_name,
      " is not symmetric within tolerance."
    )
  }

  if (
    max(
      abs(
        diag(matrix_object) -
          expected_diagonal
      )
    ) >
      numeric_tolerance
  ) {
    stop(
      object_name,
      " has an incorrect diagonal."
    )
  }

  if (
    any(
      matrix_object <
        lower_limit -
        numeric_tolerance
    ) ||
    any(
      matrix_object >
        upper_limit +
        numeric_tolerance
    )
  ) {
    stop(
      object_name,
      " contains values outside the permitted range."
    )
  }

  invisible(
    TRUE
  )
}

validate_symmetric_matrix(
  cor_matrix_stress,
  expected_diagonal = 1,
  lower_limit = -1,
  upper_limit = 1,
  object_name =
    "The Stress Spearman correlation matrix"
)

validate_symmetric_matrix(
  p_matrix_raw,
  expected_diagonal = 0,
  lower_limit = 0,
  upper_limit = 1,
  object_name =
    "The raw p-value matrix"
)

validate_symmetric_matrix(
  p_matrix_holm,
  expected_diagonal = 0,
  lower_limit = 0,
  upper_limit = 1,
  object_name =
    "The Holm-adjusted p-value matrix"
)

if (
  nrow(pairwise_results) != 6L ||
  any(
    pairwise_results$n !=
      expected_stress_n
  ) ||
  any(
    !is.finite(
      pairwise_results$Spearman_rho
    )
  ) ||
  any(
    !is.finite(
      pairwise_results$P_raw
    )
  ) ||
  any(
    !is.finite(
      pairwise_results$P_Holm
    )
  )
) {
  stop(
    "The pairwise correlation-results table is incomplete or invalid."
  )
}

matrix_correlations_for_pairs <- vapply(
  seq_len(
    nrow(pairwise_results)
  ),
  function(pair_i) {
    cor_matrix_stress[
      pairwise_results$Variable_1[
        pair_i
      ],
      pairwise_results$Variable_2[
        pair_i
      ]
    ]
  },
  numeric(1)
)

maximum_rho_recalculation_difference <- max(
  abs(
    pairwise_results$Spearman_rho -
      matrix_correlations_for_pairs
  )
)

if (
  maximum_rho_recalculation_difference >
    numeric_tolerance
) {
  stop(
    "The pairwise Spearman estimates do not reproduce the ",
    "correlation matrix within tolerance."
  )
}


# ------------------------------------------------------------
# 13. Prepare export tables
# ------------------------------------------------------------

matrix_to_export_table <- function(
    matrix_object
) {
  matrix_object <- as.matrix(
    matrix_object
  )

  output <- data.frame(
    Robust_variable =
      rownames(matrix_object),
    Variable =
      unname(
        variable_labels[
          rownames(matrix_object)
        ]
      ),
    as.data.frame(
      matrix_object,
      check.names = FALSE
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(output)[-(1:2)] <- unname(
    variable_labels[
      colnames(matrix_object)
    ]
  )

  rownames(output) <- NULL

  output
}

correlation_export <-
  matrix_to_export_table(
    cor_matrix_stress
  )

p_raw_export <-
  matrix_to_export_table(
    p_matrix_raw
  )

p_holm_export <-
  matrix_to_export_table(
    p_matrix_holm
  )

metadata_export <- data.frame(
  Field = c(
    "Objective",
    "Analytical population",
    "Sample size",
    "Traceability",
    "Authors/cohorts",
    "Reference space",
    "Normalisation",
    "Correlation method",
    "Pairwise tests",
    "Multiplicity correction",
    "Significance threshold",
    "Interpretive status"
  ),
  Value = c(
    "Objective 3",
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
    "Spearman rank correlation",
    "Two-sided; asymptotic p values (exact = FALSE)",
    "Holm correction across six unique variable pairs",
    as.character(
      significance_level
    ),
    paste(
      "Pre-clustering description; does not identify, define, or validate",
      "resilient-like or susceptible-like phenotypes"
    )
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ------------------------------------------------------------
# 14. Save tables and reusable analysis object
# ------------------------------------------------------------

openxlsx::write.xlsx(
  x = list(
    Spearman_rho =
      correlation_export,
    P_raw =
      p_raw_export,
    P_Holm =
      p_holm_export,
    Pairwise_results =
      as.data.frame(
        pairwise_results
      ),
    Author_recalculation_audit =
      as.data.frame(
        normalisation_recalculation_audit
      ),
    Metadata =
      metadata_export
  ),
  file = xlsx_output,
  overwrite = TRUE
)

utils::write.csv(
  pairwise_results,
  pairwise_csv_output,
  row.names = FALSE,
  na = ""
)

correlation_object <- list(
  Objective = "Objective 3",
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
  Correlation_method =
    "Spearman",
  Exact_p_values =
    FALSE,
  Multiple_testing_method =
    "Holm",
  Significance_level =
    significance_level,
  Author_specific_normalisation_recalculation_audit =
    normalisation_recalculation_audit,
  Correlation_matrix =
    cor_matrix_stress,
  P_raw_matrix =
    p_matrix_raw,
  P_Holm_matrix =
    p_matrix_holm,
  Pairwise_results =
    pairwise_results,
  Unique_value_counts =
    unique_value_counts,
  Variable_standard_deviations =
    variable_standard_deviations,
  Maximum_rho_recalculation_difference =
    maximum_rho_recalculation_difference,
  Source_stress_file =
    stress_input,
  Source_setup_file =
    setup_input
)

saveRDS(
  correlation_object,
  object_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)


# ------------------------------------------------------------
# 15. Create the correlation-matrix figure
# ------------------------------------------------------------

plot_correlation_matrix <-
  cor_matrix_stress

plot_p_matrix <-
  p_matrix_holm

plot_labels <- unname(
  variable_labels[
    robust_variables
  ]
)

dimnames(
  plot_correlation_matrix
) <- list(
  plot_labels,
  plot_labels
)

dimnames(
  plot_p_matrix
) <- list(
  plot_labels,
  plot_labels
)

correlation_palette <- grDevices::colorRampPalette(
  c(
    "#2166AC",
    "#F7F7F7",
    "#B2182B"
  )
)(200)

draw_correlation_figure <- function() {
  corrplot::corrplot(
    corr =
      plot_correlation_matrix,
    method =
      "color",
    type =
      "upper",
    order =
      "hclust",
    hclust.method =
      "complete",
    col =
      correlation_palette,
    col.lim =
      c(
        -1,
        1
      ),
    diag =
      FALSE,
    addCoef.col =
      "black",
    number.cex =
      0.95,
    tl.col =
      "black",
    tl.srt =
      45,
    tl.cex =
      1.0,
    p.mat =
      plot_p_matrix,
    sig.level =
      significance_level,
    insig =
      "blank",
    cl.pos =
      "r",
    cl.cex =
      0.9,
    mar =
      c(
        0,
        0,
        0,
        0
      )
  )
}

grDevices::pdf(
  pdf_output,
  width = 7,
  height = 7,
  useDingbats = FALSE
)

tryCatch(
  draw_correlation_figure(),
  finally =
    grDevices::dev.off()
)

grDevices::tiff(
  tiff_output,
  width = 7,
  height = 7,
  units = "in",
  res = 600,
  compression = "lzw",
  bg = "white"
)

tryCatch(
  draw_correlation_figure(),
  finally =
    grDevices::dev.off()
)


# ------------------------------------------------------------
# 16. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  pdf_output,
  tiff_output,
  xlsx_output,
  pairwise_csv_output,
  object_output,
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
# 17. Final console audit
# ------------------------------------------------------------

strongest_pair_index <- which.max(
  abs(
    pairwise_results$Spearman_rho
  )
)

weakest_pair_index <- which.min(
  abs(
    pairwise_results$Spearman_rho
  )
)

number_significant_holm <- sum(
  pairwise_results$Significant_Holm
)

cat(
  "\n============================================================\n"
)

cat(
  "OBJECTIVE 3 - CORRECTED STRESS CORRELATION MATRIX\n"
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
  "Correlation method: Spearman; Holm correction across six ",
  "unique pairwise tests.\n",
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
  "Maximum difference between matrix and pairwise rho estimates: ",
  format(
    maximum_rho_recalculation_difference,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "\nSpearman correlation matrix:\n"
)

print(
  round(
    cor_matrix_stress,
    3
  )
)

cat(
  "\nRaw p-value matrix:\n"
)

print(
  format.pval(
    p_matrix_raw,
    digits = 4,
    eps = 1e-16
  )
)

cat(
  "\nHolm-adjusted p-value matrix:\n"
)

print(
  format.pval(
    p_matrix_holm,
    digits = 4,
    eps = 1e-16
  )
)

cat(
  "\nUnique pairwise results:\n"
)

print(
  pairwise_results,
  n = Inf,
  width = Inf
)

cat(
  "\nStrongest association: ",
  pairwise_results$Variable_1_label[
    strongest_pair_index
  ],
  " with ",
  pairwise_results$Variable_2_label[
    strongest_pair_index
  ],
  " (rho = ",
  sprintf(
    "%.3f",
    pairwise_results$Spearman_rho[
      strongest_pair_index
    ]
  ),
  ").\n",
  sep = ""
)

cat(
  "Weakest association: ",
  pairwise_results$Variable_1_label[
    weakest_pair_index
  ],
  " with ",
  pairwise_results$Variable_2_label[
    weakest_pair_index
  ],
  " (rho = ",
  sprintf(
    "%.3f",
    pairwise_results$Spearman_rho[
      weakest_pair_index
    ]
  ),
  ").\n",
  sep = ""
)

cat(
  "Holm-significant associations: ",
  number_significant_holm,
  " of 6.\n",
  sep = ""
)

cat(
  "\nInterpretive constraint: this figure describes the shared ",
  "pre-clustering dependence structure within Stress and does not ",
  "demonstrate discrete post-stress behavioural phenotypes.\n",
  sep = ""
)

cat(
  "\nDownstream constraint: PCA and Louvain must use the same four ",
  "*_rz coordinates without additional scaling or Stress-specific ",
  "normalisation.\n",
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
