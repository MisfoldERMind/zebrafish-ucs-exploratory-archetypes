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

observed_U_control <- unname(
  observed_rank_statistics[
    "U_reference"
  ]
)

observed_rank_biserial <- unname(
  observed_rank_statistics[
    "Rank_biserial_comparison_minus_reference"
  ]
)

wilcoxon_secondary <- stats::wilcox.test(
  x = control_distance_loo,
  y = stress_distance,
  alternative = "two.sided",
  exact = FALSE,
  correct = FALSE,
  conf.int = FALSE
)

wilcoxon_secondary_table <- tibble::tibble(
  Comparison = "Stress versus Control",
  Outcome = "Mahalanobis distance to normative control atlas",
  Control_distance_method = "Leave-one-out control atlas",
  Stress_distance_method = "Complete control atlas",
  Test = "Wilcoxon rank-sum test",
  Role = paste0(
    "Secondary descriptive test; primary inference uses ",
    "reference-reconstructing label permutations"
  ),
  W_or_U_for_Control = as.numeric(
    wilcoxon_secondary$statistic
  ),
  Two_sided_p = wilcoxon_secondary$p.value,
  Rank_biserial_Stress_minus_Control =
    observed_rank_biserial
)

if (
  !isTRUE(
    all.equal(
      as.numeric(
        wilcoxon_secondary$statistic
      ),
      observed_U_control,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The manually reconstructed Mann-Whitney statistic does not ",
    "match wilcox.test()."
  )
}

# ------------------------------------------------------------
# 11. Primary reference-reconstructing permutation test
# ------------------------------------------------------------

set.seed(
  permutation_seed
)

n_total <- nrow(
  behaviour_matrix
)

n_control <- nrow(
  control_matrix
)

all_fish_indices <- seq_len(
  n_total
)

permutation_rank_biserial <- numeric(
  n_permutations
)

permutation_median_difference <- numeric(
  n_permutations
)

permutation_mean_difference <- numeric(
  n_permutations
)

permutation_start_time <- proc.time()[["elapsed"]]

cat(
  "\nStarting ",
  n_permutations,
  " global label permutations on the fixed robustly normalised ",
  "coordinates.\n",
  sep = ""
)

flush.console()

for (b in seq_len(n_permutations)) {

  # This is the original global label-permutation scheme. No
  # restriction by Author and no re-normalisation are introduced.

  permuted_control_indices <- sample.int(
    n = n_total,
    size = n_control,
    replace = FALSE
  )

  is_permuted_control <- logical(
    n_total
  )

  is_permuted_control[
    permuted_control_indices
  ] <- TRUE

  permuted_stress_indices <- all_fish_indices[
    !is_permuted_control
  ]

  permuted_reference <- calculate_reference_distances(
    reference_matrix = behaviour_matrix[
      permuted_control_indices,
      ,
      drop = FALSE
    ],
    comparison_matrix = behaviour_matrix[
      permuted_stress_indices,
      ,
      drop = FALSE
    ]
  )

  permuted_control_distance <- permuted_reference[[
    "Reference_distance_leave_one_out"
  ]]

  permuted_stress_distance <- permuted_reference[[
    "Comparison_distance"
  ]]

  permuted_rank_statistics <- calculate_rank_biserial(
    reference_distances = permuted_control_distance,
    comparison_distances = permuted_stress_distance
  )

  permutation_rank_biserial[b] <- unname(
    permuted_rank_statistics[
      "Rank_biserial_comparison_minus_reference"
    ]
  )

  permutation_median_difference[b] <- (
    stats::median(
      permuted_stress_distance
    ) -
      stats::median(
        permuted_control_distance
      )
  )

  permutation_mean_difference[b] <- (
    mean(
      permuted_stress_distance
    ) -
      mean(
        permuted_control_distance
      )
  )

  if (
    b %% permutation_progress_interval == 0L ||
    b == n_permutations
  ) {

    elapsed_seconds <- (
      proc.time()[["elapsed"]] -
        permutation_start_time
    )

    estimated_total_seconds <- (
      elapsed_seconds /
        b *
        n_permutations
    )

    remaining_seconds <- max(
      estimated_total_seconds -
        elapsed_seconds,
      0
    )

    cat(
      sprintf(
        paste0(
          "Permutation progress: %d/%d (%.1f%%) | ",
          "elapsed %.1f min | estimated remaining %.1f min\n"
        ),
        b,
        n_permutations,
        100 * b / n_permutations,
        elapsed_seconds / 60,
        remaining_seconds / 60
      )
    )

    flush.console()
  }
}

if (
  any(
    !is.finite(
      permutation_rank_biserial
    )
  ) ||
  any(
    !is.finite(
      permutation_median_difference
    )
  ) ||
  any(
    !is.finite(
      permutation_mean_difference
    )
  )
) {
  stop(
    "The permutation procedure produced non-finite statistics."
  )
}

observed_median_difference <- (
  stats::median(
    stress_distance
  ) -
    stats::median(
      control_distance_loo
    )
)

observed_mean_difference <- (
  mean(
    stress_distance
  ) -
    mean(
      control_distance_loo
    )
)

permutation_p_two_sided <- (
  1 +
    sum(
      abs(
        permutation_rank_biserial
      ) >=
        abs(
          observed_rank_biserial
        )
    )
) /
  (
    n_permutations +
      1
  )

permutation_p_stress_greater <- (
  1 +
    sum(
      permutation_rank_biserial >=
        observed_rank_biserial
    )
) /
  (
    n_permutations +
      1
  )

permutation_test_table <- tibble::tibble(
  Comparison = "Stress versus Control",
  Null_procedure = paste0(
    "Treatment labels permuted globally across the fixed robustly ",
    "normalised coordinates; pooled Control atlas, leave-one-out ",
    "Control distances, and Stress distances reconstructed in every ",
    "permutation"
  ),
  Primary_statistic =
    "Rank-biserial correlation, Stress minus Control",
  Observed_rank_biserial =
    observed_rank_biserial,
  Observed_median_difference_Stress_minus_Control =
    observed_median_difference,
  Observed_mean_difference_Stress_minus_Control =
    observed_mean_difference,
  Two_sided_permutation_p =
    permutation_p_two_sided,
  One_sided_p_Stress_greater =
    permutation_p_stress_greater,
  Permutations = n_permutations,
  Seed = permutation_seed
)

# ------------------------------------------------------------
# 12. Normative atlas diagnostics
# ------------------------------------------------------------

reference_center <- observed_reference[[
  "Reference_center"
]]

reference_covariance <- observed_reference[[
  "Reference_covariance"
]]

reference_correlation <- stats::cov2cor(
  reference_covariance
)

covariance_eigenvalues <- observed_reference[[
  "Covariance_eigenvalues"
]]

covariance_condition_number <- kappa(
  reference_covariance,
  exact = TRUE
)

covariance_rank <- qr(
  reference_covariance
)$rank

reference_center_table <- tibble::tibble(
  Variable = behavioural_variables,
  Behaviour = unname(
    variable_labels[
      behavioural_variables
    ]
  ),
  Robust_variable = robust_variables,
  Control_atlas_mean = as.numeric(
    reference_center[
      robust_variables
    ]
  )
)

covariance_table <- as.data.frame(
  reference_covariance,
  check.names = FALSE
) %>%
  tibble::rownames_to_column(
    var = "Variable"
  ) %>%
  tibble::as_tibble()

correlation_table <- as.data.frame(
  reference_correlation,
  check.names = FALSE
) %>%
  tibble::rownames_to_column(
    var = "Variable"
  ) %>%
  tibble::as_tibble()

covariance_diagnostics <- tibble::tibble(
  Dimensions = length(
    robust_variables
  ),
  Control_n = nrow(
    control_matrix
  ),
  Covariance_rank = covariance_rank,
  Minimum_eigenvalue = min(
    covariance_eigenvalues
  ),
  Maximum_eigenvalue = max(
    covariance_eigenvalues
  ),
  Condition_number = covariance_condition_number,
  Determinant = determinant(
    reference_covariance,
    logarithm = FALSE
  )$modulus %>%
    as.numeric(),
  Positive_definite = all(
    covariance_eigenvalues > 0
  ),
  Maximum_LOO_formula_error =
    loo_maximum_absolute_difference
)

if (
  covariance_rank !=
    length(robust_variables)
) {
  stop(
    "The control covariance matrix is not full rank."
  )
}

# ------------------------------------------------------------
# 13. Consistency with the 4D centroid-displacement analysis
# ------------------------------------------------------------

required_centroid_field <- "Euclidean_centroid_displacement_4D"

if (
  !required_centroid_field %in%
    names(
      centroid_displacement_object
    )
) {
  stop(
    "The Script 012 object does not contain ",
    required_centroid_field,
    "."
  )
}

stored_centroid_displacement <- as.numeric(
  centroid_displacement_object[[required_centroid_field]]
)

if (
  length(stored_centroid_displacement) != 1L ||
  !is.finite(stored_centroid_displacement)
) {
  stop(
    "The centroid displacement stored by Script 012 is invalid."
  )
}

recalculated_centroid_displacement <- sqrt(
  sum(
    (
      colMeans(stress_matrix) -
        colMeans(control_matrix)
    )^2
  )
)

if (
  !isTRUE(
    all.equal(
      recalculated_centroid_displacement,
      stored_centroid_displacement,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The current robustly normalised population does not reproduce ",
    "the 4D centroid displacement stored by Script 012."
  )
}

# ------------------------------------------------------------
# 14. Figure
# ------------------------------------------------------------

figure_data <- individual_distances %>%
  dplyr::select(
    Treatment,
    Mahalanobis_distance
  )

mahalanobis_plot <- ggplot2::ggplot(
  figure_data,
  ggplot2::aes(
    x = Treatment,
    y = Mahalanobis_distance
  )
) +
  ggplot2::geom_violin(
    width = 0.78,
    trim = FALSE,
    fill = "grey90",
    colour = "grey35",
    linewidth = 0.55
  ) +
  ggplot2::geom_boxplot(
    width = 0.22,
    outlier.shape = NA,
    fill = "white",
    colour = "black",
    linewidth = 0.55
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_jitter(
      width = 0.09,
      height = 0,
      seed = 2026L
    ),
    size = 1.65,
    alpha = 0.65,
    colour = "black"
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Mahalanobis distance to normative control atlas"
  ) +
  ggplot2::theme_classic(
    base_size = 12,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    axis.text = ggplot2::element_text(
      colour = "black"
    ),
    axis.title = ggplot2::element_text(
      colour = "black"
    )
  )

print(
  mahalanobis_plot
)

# ------------------------------------------------------------
# 15. Save definitive corrected outputs
# ------------------------------------------------------------

permutation_distribution <- tibble::tibble(
  Replicate = seq_len(
    n_permutations
  ),
  Rank_biserial_Stress_minus_Control =
    permutation_rank_biserial,
  Median_difference_Stress_minus_Control =
    permutation_median_difference,
  Mean_difference_Stress_minus_Control =
    permutation_mean_difference
)

analysis_summary <- tibble::tibble(
  Objective = "Objective 2",
  Comparison = "Stress versus Control",
  Design = "Two independent populations",
  Space = "Four-dimensional control-referenced robust behavioural space",
  Distance = "Mahalanobis distance",
  Control_distance_method = "Leave-one-out control atlas",
  Stress_distance_method = "Complete control atlas",
  Control_n = nrow(
    control_matrix
  ),
  Stress_n = nrow(
    stress_matrix
  ),
  Observed_centroid_displacement_4D =
    stored_centroid_displacement,
  Observed_rank_biserial_Stress_minus_Control =
    observed_rank_biserial,
  Primary_two_sided_permutation_p =
    permutation_p_two_sided,
  Secondary_Wilcoxon_p =
    wilcoxon_secondary$p.value,
  Permutations = n_permutations
)

results_object <- list(
  Objective = "Objective 2",
  Comparison_design = "Independent Control and Stress populations",
  Input_population_file = population_file,
  Input_centroid_displacement_file =
    centroid_displacement_file,
  Coordinate_system =
    "Control-referenced robust behavioural space",
  Behavioural_variables =
    behavioural_variables,
  Robust_variables =
    robust_variables,
  Distance_definition =
    "Square root of classical Mahalanobis squared distance",
  Control_distance_method =
    "Leave-one-out control atlas",
  Stress_distance_method =
    "Complete control atlas",
  Reference_center =
    reference_center,
  Reference_covariance =
    reference_covariance,
  Reference_correlation =
    reference_correlation,
  Covariance_eigenvalues =
    covariance_eigenvalues,
  Covariance_diagnostics =
    covariance_diagnostics,
  Author_reference_audit =
    control_reference_audit,
  Individual_distances =
    individual_distances,
  Descriptive_summary =
    summary_table,
  Primary_permutation_test =
    permutation_test_table,
  Secondary_Wilcoxon_test =
    wilcoxon_secondary_table,
  Permutation_distribution =
    permutation_distribution,
  Analysis_summary =
    analysis_summary
)

saveRDS(
  individual_distances,
  individual_output
)

saveRDS(
  results_object,
  results_output
)

saveRDS(
  permutation_distribution,
  permutation_output
)

workbook_sheets <- list(
  Analysis_summary = analysis_summary,
  Descriptive_statistics = summary_table,
  Primary_permutation_test = permutation_test_table,
  Secondary_Wilcoxon = wilcoxon_secondary_table,
  Control_atlas_centre = reference_center_table,
  Control_covariance = covariance_table,
  Control_correlation = correlation_table,
  Covariance_diagnostics = covariance_diagnostics,
  Author_reference_audit = control_reference_audit,
  Individual_distances = individual_distances
)

workbook <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE
)

for (sheet_i in names(workbook_sheets)) {

  sheet_data_i <- workbook_sheets[[sheet_i]]

  if (!is.data.frame(sheet_data_i)) {
    stop(
      "Workbook sheet ",
      sheet_i,
      " is not a data frame."
    )
  }

  openxlsx::addWorksheet(
    workbook,
    sheet_i
  )

  openxlsx::writeData(
    workbook,
    sheet = sheet_i,
    x = sheet_data_i
  )

  n_columns_i <- ncol(
    sheet_data_i
  )

  if (n_columns_i > 0L) {

    openxlsx::addStyle(
      workbook,
      sheet = sheet_i,
      style = header_style,
      rows = 1,
      cols = seq_len(
        n_columns_i
      ),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::setColWidths(
      workbook,
      sheet = sheet_i,
      cols = seq_len(
        n_columns_i
      ),
      widths = "auto"
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet = sheet_i,
    firstRow = TRUE
  )
}

openxlsx::saveWorkbook(
  workbook,
  workbook_output,
  overwrite = TRUE
)

ggplot2::ggsave(
  filename = figure_pdf_output,
  plot = mahalanobis_plot,
  width = 5.2,
  height = 6.2,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_tiff_output,
  plot = mahalanobis_plot,
  width = 5.2,
  height = 6.2,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = figure_png_output,
  plot = mahalanobis_plot,
  width = 5.2,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "white"
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 16. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  individual_output,
  results_output,
  permutation_output,
  workbook_output,
  figure_pdf_output,
  figure_tiff_output,
  figure_png_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected output files were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 17. Identify obsolete outputs from the former workflow
# ------------------------------------------------------------

legacy_files <- c(
  file.path(
    processed_dir,
    "population_control_stress_z.rds"
  ),
  file.path(
    figures_dir,
    "Figure11_Mahalanobis.pdf"
  ),
  file.path(
    figures_dir,
    "Figure11_Mahalanobis.tiff"
  ),
  file.path(
    tables_dir,
    "Table4_Mahalanobis.xlsx"
  )
)

existing_legacy_files <- legacy_files[
  file.exists(legacy_files)
]

if (length(existing_legacy_files) > 0L) {
  warning(
    "Legacy Mahalanobis outputs from the former mean/SD workflow ",
    "were detected. They were not used or overwritten:\n",
    paste(existing_legacy_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 18. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - MAHALANOBIS DISTANCE TO NORMATIVE ATLAS\n")
cat("============================================================\n")

cat(
  "\nIndependent populations:\n"
)

print(
  group_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nAuthor-specific control-reference audit ",
  "(expected median = 0 and scaled MAD = 1):\n",
  sep = ""
)

print(
  control_reference_audit,
  n = Inf,
  width = Inf
)

cat(
  "\nControl normative-atlas centre:\n"
)

print(
  reference_center_table,
  n = Inf,
  width = Inf
)

cat(
  "\nCovariance diagnostics:\n"
)

print(
  covariance_diagnostics,
  n = Inf,
  width = Inf
)

cat(
  "\nDescriptive Mahalanobis distances:\n"
)

print(
  summary_table,
  n = Inf,
  width = Inf
)

cat(
  "\nPrimary reference-reconstructing permutation test:\n"
)

print(
  permutation_test_table,
  n = Inf,
  width = Inf
)

cat(
  "\nSecondary Wilcoxon rank-sum result:\n"
)

print(
  wilcoxon_secondary_table,
  n = Inf,
  width = Inf
)

cat(
  "\nMethodological note: the Author-specific robust normalisation ",
  "was completed previously in Script 011 and remained fixed. ",
  "Control distances were calculated by leave-one-out estimation, ",
  "whereas Stress distances were calculated against the complete ",
  "pooled Control atlas. The primary global label-permutation test ",
  "reconstructed the pooled atlas and both distance sets after every ",
  "permutation.\n",
  sep = ""
)

cat(
  "\nstats::mahalanobis() returns squared distances; the reported ",
  "and plotted outcome is their square root.\n",
  sep = ""
)

cat(
  "\nDefinitive corrected outputs saved as:\n",
  normalizePath(
    individual_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    results_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    permutation_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    workbook_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    figure_pdf_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    figure_tiff_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    figure_png_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    session_info_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)

cat("\n============================================================\n")
