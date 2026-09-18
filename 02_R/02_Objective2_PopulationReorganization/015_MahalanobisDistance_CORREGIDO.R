# ============================================================
# 015_MahalanobisDistance.R
#
# Objective 2
# Mahalanobis distance to the normative control atlas
#
# Purpose:
#   Quantify how far each fish lies from the multivariate
#   organisation of the unstressed reference population while
#   accounting for covariance among the four behavioural variables.
#
# Definitive analytical space:
#   Four-dimensional control-referenced robust z-score space
#   produced by 011_Project_Control_Stress.R.
#
# Primary comparison:
#   - Control fish: leave-one-out Mahalanobis distance, so that
#     each control fish is evaluated against an atlas estimated
#     from the remaining controls.
#   - Stress fish: Mahalanobis distance to the complete control
#     atlas.
#
# Statistical inference:
#   A label-permutation test operates on the definitive, fixed,
#   robustly normalised coordinates. In every permutation it
#   reconstructs the pooled Control atlas, the leave-one-out Control
#   distances, and the Stress distances. This is the primary
#   inferential analysis because the distances are derived from
#   a control-defined reference and are therefore not ordinary
#   independent raw observations.
#
# Important:
#   stats::mahalanobis() returns squared Mahalanobis distances.
#   This script stores both the squared quantity and its square
#   root; all figures and group summaries use the actual
#   Mahalanobis distance.
# ============================================================

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
  "tidyr",
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
library(tidyr)
library(tibble)
library(ggplot2)
library(openxlsx)

# ------------------------------------------------------------
# 3. Analysis settings
# ------------------------------------------------------------

permutation_seed <- 1234L
n_permutations <- 9999L
permutation_progress_interval <- 250L

if (
  length(n_permutations) != 1L ||
  !is.finite(n_permutations) ||
  n_permutations < 999L ||
  n_permutations != as.integer(n_permutations)
) {
  stop(
    "n_permutations must be a single integer of at least 999."
  )
}

required_treatments <- c(
  "Control",
  "Stress"
)

behavioural_variables <- c(
  "distance",
  "crossing",
  "top_entry",
  "top_time"
)

robust_variables <- paste0(
  behavioural_variables,
  "_rz"
)

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
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

population_file <- file.path(
  processed_dir,
  "population_control_stress_robust_z.rds"
)

centroid_displacement_file <- file.path(
  objects_dir,
  "objective2_centroid_displacement_4d_robust.rds"
)

required_input_files <- c(
  population_file,
  centroid_displacement_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following required input files were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun Scripts 011 and 012 before this script."
  )
}

individual_output <- file.path(
  objects_dir,
  "objective2_mahalanobis_individual_distances_robust.rds"
)

results_output <- file.path(
  objects_dir,
  "objective2_mahalanobis_normative_atlas_results_robust.rds"
)

permutation_output <- file.path(
  objects_dir,
  "objective2_mahalanobis_permutation_distribution_robust.rds"
)

workbook_output <- file.path(
  tables_dir,
  "Objective2_Mahalanobis_Distance_to_Normative_Atlas.xlsx"
)

figure_pdf_output <- file.path(
  figures_dir,
  "Objective2_Mahalanobis_Distance_to_Normative_Atlas.pdf"
)

figure_tiff_output <- file.path(
  figures_dir,
  "Objective2_Mahalanobis_Distance_to_Normative_Atlas.tiff"
)

figure_png_output <- file.path(
  figures_dir,
  "Objective2_Mahalanobis_Distance_to_Normative_Atlas.png"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_mahalanobis_distance_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load and validate the definitive corrected population
# ------------------------------------------------------------

population <- readRDS(
  population_file
)

centroid_displacement_object <- readRDS(
  centroid_displacement_file
)

if (!is.data.frame(population)) {
  stop(
    basename(population_file),
    " does not contain a data frame."
  )
}

if (!is.list(centroid_displacement_object)) {
  stop(
    basename(centroid_displacement_file),
    " does not contain the expected result list."
  )
}

treatment_variable <- "Treatment"
author_variable <- "Author"

required_columns <- c(
  author_variable,
  treatment_variable,
  robust_variables
)

missing_columns <- setdiff(
  required_columns,
  names(population)
)

if (length(missing_columns) > 0L) {
  stop(
    "The corrected Objective 2 population object is missing:\n",
    paste(missing_columns, collapse = ", ")
  )
}

non_numeric_variables <- robust_variables[
  !vapply(
    population[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0L) {
  stop(
    "The following robust behavioural variables are not numeric:\n",
    paste(non_numeric_variables, collapse = ", ")
  )
}

treatment_values <- unique(
  as.character(
    population[[treatment_variable]]
  )
)

if (!setequal(treatment_values, required_treatments)) {
  stop(
    "The corrected population object must contain exactly the ",
    "Control and Stress treatment levels."
  )
}

population <- population %>%
  dplyr::mutate(
    Author = factor(
      as.character(.data[[author_variable]])
    ),
    Treatment = factor(
      as.character(.data[[treatment_variable]]),
      levels = required_treatments
    )
  )

if (
  anyNA(population$Author) ||
  nlevels(population$Author) != 4L
) {
  stop(
    "Author must contain four non-missing cohort levels."
  )
}

group_counts <- population %>%
  dplyr::count(
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(Treatment)

expected_group_counts <- tibble::tibble(
  Treatment = factor(
    required_treatments,
    levels = required_treatments
  ),
  n = c(89L, 89L)
)

if (!identical(group_counts, expected_group_counts)) {
  stop(
    "Unexpected group counts were detected.\nObserved:\n",
    paste(
      capture.output(
        print(
          group_counts,
          n = Inf,
          width = Inf
        )
      ),
      collapse = "\n"
    ),
    "\nExpected: Control = 89 and Stress = 89."
  )
}

behaviour_matrix <- as.matrix(
  population[robust_variables]
)

storage.mode(behaviour_matrix) <- "double"

if (
  nrow(behaviour_matrix) != 178L ||
  ncol(behaviour_matrix) != 4L
) {
  stop(
    "The corrected behavioural matrix must have dimensions 178 x 4."
  )
}

if (any(!is.finite(behaviour_matrix))) {
  stop(
    "Missing or non-finite values were detected in the robust ",
    "behavioural matrix."
  )
}

control_rows <- population$Treatment == "Control"
stress_rows <- population$Treatment == "Stress"

control_matrix <- behaviour_matrix[
  control_rows,
  ,
  drop = FALSE
]

stress_matrix <- behaviour_matrix[
  stress_rows,
  ,
  drop = FALSE
]

colnames(control_matrix) <- robust_variables
colnames(stress_matrix) <- robust_variables

# Reconfirm the already-completed control-reference robust
# transformation within each Author cohort. This is an audit only:
# the coordinates are not recalculated or modified in this script.

audit_tolerance <- 1e-10

author_levels <- levels(
  population$Author
)

control_reference_audit <- dplyr::bind_rows(
  lapply(
    author_levels,
    function(author_i) {

      author_control_matrix <- as.matrix(
        population[
          population$Author == author_i &
            population$Treatment == "Control",
          robust_variables,
          drop = FALSE
        ]
      )

      if (nrow(author_control_matrix) == 0L) {
        stop(
          "No Control fish were found for Author ",
          author_i,
          "."
        )
      }

      tibble::tibble(
        Author = author_i,
        Robust_variable = robust_variables,
        Control_median = apply(
          author_control_matrix,
          2,
          stats::median
        ),
        Control_scaled_MAD = apply(
          author_control_matrix,
          2,
          stats::mad,
          constant = 1.4826
        )
      )
    }
  )
)

if (
  nrow(control_reference_audit) !=
    length(author_levels) *
      length(robust_variables)
) {
  stop(
    "The Author-specific robust-normalisation audit is incomplete."
  )
}

if (
  any(
    abs(control_reference_audit$Control_median) >
      audit_tolerance
  )
) {
  stop(
    "At least one Author-specific Control median is not zero ",
    "within tolerance."
  )
}

if (
  any(
    abs(
      control_reference_audit$Control_scaled_MAD -
        1
    ) >
      audit_tolerance
  )
) {
  stop(
    "At least one Author-specific Control scaled MAD is not one ",
    "within tolerance."
  )
}

# ------------------------------------------------------------
# 6. Helper functions
# ------------------------------------------------------------

check_covariance_matrix <- function(
    covariance_matrix,
    context_label
) {

  if (!is.matrix(covariance_matrix)) {
    stop(
      context_label,
      ": covariance object is not a matrix."
    )
  }

  if (
    nrow(covariance_matrix) !=
      length(robust_variables) ||
    ncol(covariance_matrix) !=
      length(robust_variables)
  ) {
    stop(
      context_label,
      ": covariance matrix has incorrect dimensions."
    )
  }

  if (any(!is.finite(covariance_matrix))) {
    stop(
      context_label,
      ": covariance matrix contains non-finite values."
    )
  }

  symmetry_difference <- max(
    abs(
      covariance_matrix -
        t(covariance_matrix)
    )
  )

  if (symmetry_difference > 1e-10) {
    stop(
      context_label,
      ": covariance matrix is not symmetric."
    )
  }

  eigenvalues <- eigen(
    covariance_matrix,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  eigen_tolerance <- max(
    eigenvalues
  ) * sqrt(
    .Machine$double.eps
  )

  if (
    any(!is.finite(eigenvalues)) ||
    min(eigenvalues) <= eigen_tolerance
  ) {
    stop(
      context_label,
      ": covariance matrix is singular or not positive definite."
    )
  }

  invisible(eigenvalues)
}

calculate_reference_distances <- function(
    reference_matrix,
    comparison_matrix
) {

  reference_matrix <- as.matrix(
    reference_matrix
  )

  comparison_matrix <- as.matrix(
    comparison_matrix
  )

  storage.mode(reference_matrix) <- "double"
  storage.mode(comparison_matrix) <- "double"

  n_reference <- nrow(
    reference_matrix
  )

  p_dimensions <- ncol(
    reference_matrix
  )

  if (n_reference <= p_dimensions + 2L) {
    stop(
      "The reference sample is too small for leave-one-out ",
      "Mahalanobis estimation."
    )
  }

  if (ncol(comparison_matrix) != p_dimensions) {
    stop(
      "Reference and comparison matrices have different numbers ",
      "of variables."
    )
  }

  reference_center <- colMeans(
    reference_matrix
  )

  reference_covariance <- stats::cov(
    reference_matrix
  )

  covariance_eigenvalues <- check_covariance_matrix(
    covariance_matrix = reference_covariance,
    context_label = "Mahalanobis reference"
  )

  reference_squared_in_sample <- stats::mahalanobis(
    x = reference_matrix,
    center = reference_center,
    cov = reference_covariance,
    inverted = FALSE
  )

  # Exact deleted-observation transformation. It converts each
  # in-sample squared Mahalanobis distance into the distance obtained
  # when that reference observation is evaluated against the mean
  # and covariance estimated from the remaining n - 1 observations.

  loo_denominator <- (
    (n_reference - 1) *
      (
        (n_reference - 1)^2 -
          n_reference *
            reference_squared_in_sample
      )
  )

  if (
    any(!is.finite(loo_denominator)) ||
    any(loo_denominator <= 0)
  ) {
    stop(
      "The leave-one-out Mahalanobis transformation produced an ",
      "invalid denominator."
    )
  }

  reference_squared_loo <- (
    n_reference^2 *
      (n_reference - 2) *
      reference_squared_in_sample
  ) /
    loo_denominator

  comparison_squared <- stats::mahalanobis(
    x = comparison_matrix,
    center = reference_center,
    cov = reference_covariance,
    inverted = FALSE
  )

  if (
    any(!is.finite(reference_squared_loo)) ||
    any(!is.finite(comparison_squared)) ||
    any(reference_squared_loo < -1e-12) ||
    any(comparison_squared < -1e-12)
  ) {
    stop(
      "Invalid squared Mahalanobis distances were produced."
    )
  }

  reference_squared_loo <- pmax(
    reference_squared_loo,
    0
  )

  comparison_squared <- pmax(
    comparison_squared,
    0
  )

  list(
    Reference_center = reference_center,
    Reference_covariance = reference_covariance,
    Covariance_eigenvalues = covariance_eigenvalues,
    Reference_squared_in_sample =
      reference_squared_in_sample,
    Reference_squared_leave_one_out =
      reference_squared_loo,
    Comparison_squared = comparison_squared,
    Reference_distance_leave_one_out =
      sqrt(reference_squared_loo),
    Comparison_distance =
      sqrt(comparison_squared)
  )
}

calculate_rank_biserial <- function(
    reference_distances,
    comparison_distances
) {

  n_reference <- length(
    reference_distances
  )

  n_comparison <- length(
    comparison_distances
  )

  combined_values <- c(
    reference_distances,
    comparison_distances
  )

  combined_ranks <- rank(
    combined_values,
    ties.method = "average"
  )

  reference_rank_sum <- sum(
    combined_ranks[
      seq_len(n_reference)
    ]
  )

  U_reference <- (
    reference_rank_sum -
      n_reference *
        (n_reference + 1) / 2
  )

  rank_biserial_comparison_minus_reference <- (
    1 -
      2 *
        U_reference /
        (
          n_reference *
            n_comparison
        )
  )

  c(
    U_reference = U_reference,
    Rank_biserial_comparison_minus_reference =
      rank_biserial_comparison_minus_reference
  )
}

# ------------------------------------------------------------
# 7. Estimate the observed normative atlas and distances
# ------------------------------------------------------------

observed_reference <- calculate_reference_distances(
  reference_matrix = control_matrix,
  comparison_matrix = stress_matrix
)

control_squared_loo <- observed_reference[[
  "Reference_squared_leave_one_out"
]]

stress_squared <- observed_reference[[
  "Comparison_squared"
]]

control_distance_loo <- observed_reference[[
  "Reference_distance_leave_one_out"
]]

stress_distance <- observed_reference[[
  "Comparison_distance"
]]

control_squared_in_sample <- observed_reference[[
  "Reference_squared_in_sample"
]]

# Directly verify the exact leave-one-out transformation for every
# control fish using explicit refitting.

control_squared_loo_direct <- vapply(
  seq_len(nrow(control_matrix)),
  function(i) {

    control_reference_i <- control_matrix[
      -i,
      ,
      drop = FALSE
    ]

    center_i <- colMeans(
      control_reference_i
    )

    covariance_i <- stats::cov(
      control_reference_i
    )

    check_covariance_matrix(
      covariance_matrix = covariance_i,
      context_label = paste0(
        "Direct leave-one-out control ",
        i
      )
    )

    as.numeric(
      stats::mahalanobis(
        x = matrix(
          control_matrix[i, ],
          nrow = 1
        ),
        center = center_i,
        cov = covariance_i
      )
    )
  },
  numeric(1)
)

loo_maximum_absolute_difference <- max(
  abs(
    control_squared_loo -
      control_squared_loo_direct
  )
)

if (
  !is.finite(
    loo_maximum_absolute_difference
  ) ||
  loo_maximum_absolute_difference > 1e-8
) {
  stop(
    "The fast leave-one-out Mahalanobis calculation does not ",
    "reproduce explicit refitting."
  )
}

# ------------------------------------------------------------
# 8. Construct the individual-level analysis table
# ------------------------------------------------------------

individual_distances <- population %>%
  dplyr::mutate(
    Mahalanobis_squared_in_sample_control =
      NA_real_,
    Mahalanobis_squared =
      NA_real_,
    Mahalanobis_distance =
      NA_real_,
    Distance_reference =
      NA_character_
  )

individual_distances[
  control_rows,
  "Mahalanobis_squared_in_sample_control"
] <- control_squared_in_sample

individual_distances[
  control_rows,
  "Mahalanobis_squared"
] <- control_squared_loo

individual_distances[
  stress_rows,
  "Mahalanobis_squared"
] <- stress_squared

individual_distances[
  control_rows,
  "Mahalanobis_distance"
] <- control_distance_loo

individual_distances[
  stress_rows,
  "Mahalanobis_distance"
] <- stress_distance

individual_distances[
  control_rows,
  "Distance_reference"
] <- "Leave-one-out control atlas"

individual_distances[
  stress_rows,
  "Distance_reference"
] <- "Complete control atlas"

if (
  any(
    !is.finite(
      individual_distances[[
        "Mahalanobis_squared"
      ]]
    )
  ) ||
  any(
    !is.finite(
      individual_distances[[
        "Mahalanobis_distance"
      ]]
    )
  )
) {
  stop(
    "The final individual Mahalanobis distances contain ",
    "non-finite values."
  )
}

if (
  any(
    abs(
      individual_distances[[
        "Mahalanobis_distance"
      ]]^2 -
        individual_distances[[
          "Mahalanobis_squared"
        ]]
    ) > 1e-10
  )
) {
  stop(
    "The reported Mahalanobis distance is not the square root of ",
    "the stored squared distance."
  )
}

# ------------------------------------------------------------
# 9. Descriptive statistics
# ------------------------------------------------------------

summary_table <- individual_distances %>%
  dplyr::group_by(
    Treatment
  ) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean = mean(
      Mahalanobis_distance
    ),
    SD = stats::sd(
      Mahalanobis_distance
    ),
    Median = stats::median(
      Mahalanobis_distance
    ),
    Q1 = as.numeric(
      stats::quantile(
        Mahalanobis_distance,
        probs = 0.25,
        names = FALSE,
        type = 7
      )
    ),
    Q3 = as.numeric(
      stats::quantile(
        Mahalanobis_distance,
        probs = 0.75,
        names = FALSE,
        type = 7
      )
    ),
    IQR = stats::IQR(
      Mahalanobis_distance,
      type = 7
    ),
    Minimum = min(
      Mahalanobis_distance
    ),
    Maximum = max(
      Mahalanobis_distance
    ),
    Mean_squared_distance = mean(
      Mahalanobis_squared
    ),
    Median_squared_distance = stats::median(
      Mahalanobis_squared
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Treatment
  )

# ------------------------------------------------------------
# 10. Observed rank separation and secondary Wilcoxon test
# ------------------------------------------------------------

observed_rank_statistics <- calculate_rank_biserial(
  reference_distances = control_distance_loo,
  comparison_distances = stress_distance
)
