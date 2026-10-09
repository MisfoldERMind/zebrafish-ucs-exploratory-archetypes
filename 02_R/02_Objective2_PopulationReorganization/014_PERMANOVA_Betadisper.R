# ============================================================
# 014_PERMANOVA_Betadisper.R
#
# Objective 2
# Permutational assessment of population-level behavioural
# reorganisation after UCS
#
# This script compares the independent Control and Stress
# populations in the definitive four-dimensional,
# control-referenced robust behavioural space.
#
# Analyses:
#   1. Euclidean distance matrix across the four robust variables.
#   2. Author-adjusted PERMANOVA for Treatment, with Author
#      included as an adjustment term and 9,999 permutations
#      restricted within Author.
#   3. PERMDISP using distances to the group spatial median,
#      with 9,999 residual permutations restricted within Author.
#   4. Descriptive summaries of within-group multivariate
#      dispersion.
#
# Input produced by:
#   011_Project_Control_Stress.R
#
# Consistency check against:
#   012_Centroid_Displacement_4D.R
#
# Important:
#   PERMANOVA and PERMDISP answer different questions.
#   PERMANOVA evaluates the Author-adjusted treatment-associated
#   difference in multivariate location within the selected
#   distance geometry.
#   PERMDISP evaluates whether within-group multivariate
#   dispersion differs between treatments.
#   Author is included as an adjustment term in the PERMANOVA
#   model and is also used as the non-movable permutation block.
#   These operations address
#   different aspects of the cohort structure and are not redundant.
# ============================================================

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
  "vegan",
  "permute"
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
library(vegan)
library(permute)

# ------------------------------------------------------------
# 3. Analysis settings
# ------------------------------------------------------------

n_permutations <- 9999L
permanova_seed <- 1234L
permdisp_seed <- 1235L

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

expected_authors <- as.character(
  seq_len(4L)
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

for (directory_i in c(objects_dir, tables_dir)) {
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

distance_output <- file.path(
  objects_dir,
  "objective2_euclidean_distance_control_stress_4d_robust.rds"
)

betadisper_output <- file.path(
  objects_dir,
  "objective2_betadisper_model_4d_robust.rds"
)

results_output <- file.path(
  objects_dir,
  "objective2_permanova_betadisper_4d_robust.rds"
)

permanova_csv_output <- file.path(
  tables_dir,
  "Objective2_PERMANOVA_4D_RobustZ.csv"
)

permdisp_csv_output <- file.path(
  tables_dir,
  "Objective2_PERMDISP_Permutation_4D_RobustZ.csv"
)

permdisp_anova_csv_output <- file.path(
  tables_dir,
  paste0(
    "Objective2_PERMDISP_Unblocked_ParametricANOVA_",
    "Secondary_4D_RobustZ.csv"
  )
)

cohort_counts_audit_csv_output <- file.path(
  tables_dir,
  "Objective2_PERMANOVA_Author_Treatment_Counts_Audit.csv"
)

author_reference_audit_csv_output <- file.path(
  tables_dir,
  "Objective2_PERMANOVA_Author_Reference_Audit.csv"
)

dispersion_descriptives_csv_output <- file.path(
  tables_dir,
  "Objective2_Multivariate_Dispersion_Descriptives_4D_RobustZ.csv"
)

dispersion_contrast_csv_output <- file.path(
  tables_dir,
  "Objective2_Multivariate_Dispersion_Contrast_4D_RobustZ.csv"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_permanova_betadisper_sessionInfo.txt"
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
id_variable <- "ID"

required_columns <- c(
  author_variable,
  id_variable,
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
      as.character(.data[[author_variable]]),
      levels = expected_authors
    ),
    ID = as.character(
      .data[[id_variable]]
    ),
    Treatment = factor(
      as.character(.data[[treatment_variable]]),
      levels = required_treatments
    )
  )

if (anyNA(population$Author)) {
  stop(
    "Author must contain exactly the four expected cohort labels: ",
    paste(expected_authors, collapse = ", "),
    "."
  )
}

if (
  anyNA(population$ID) ||
  any(!nzchar(trimws(population$ID)))
) {
  stop(
    "ID contains missing or empty values."
  )
}

duplicate_author_id <- population %>%
  dplyr::count(
    Author,
    ID,
    name = "n"
  ) %>%
  dplyr::filter(n != 1L)

if (nrow(duplicate_author_id) > 0L) {
  stop(
    "Author + ID does not uniquely identify every fish."
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

cohort_group_counts <- population %>%
  dplyr::count(
    Author,
    Treatment,
    name = "n",
    .drop = FALSE
  ) %>%
  dplyr::arrange(
    Author,
    Treatment
  )

expected_cohort_group_counts <- tibble::tibble(
  Author = factor(
    rep(expected_authors, each = 2L),
    levels = expected_authors
  ),
  Treatment = factor(
    rep(required_treatments, times = 4L),
    levels = required_treatments
  ),
  n = c(
    24L, 24L,
    11L, 12L,
    18L, 17L,
    36L, 36L
  )
)

if (!identical(cohort_group_counts, expected_cohort_group_counts)) {
  stop(
    "Unexpected Author x Treatment counts were detected.\nObserved:\n",
    paste(
      capture.output(
        print(
          cohort_group_counts,
          n = Inf,
          width = Inf
        )
      ),
      collapse = "\n"
    )
  )
}

behaviour_matrix <- as.matrix(
  population[robust_variables]
)

storage.mode(behaviour_matrix) <- "double"

if (
  nrow(behaviour_matrix) != sum(group_counts$n) ||
  ncol(behaviour_matrix) != length(robust_variables)
) {
  stop(
    "The robust behavioural matrix has incorrect dimensions."
  )
}

if (any(!is.finite(behaviour_matrix))) {
  stop(
    "Missing or non-finite values were detected in the robust ",
    "behavioural matrix."
  )
}

# Reconfirm the control-reference robust transformation separately
# within every Author cohort. A pooled check is insufficient because
# the definitive transformation in Script 011 used 16 distinct
# Author x variable reference parameter sets.

audit_tolerance <- 1e-10

control_reference_audit <- dplyr::bind_rows(
  lapply(
    expected_authors,
    function(author_i) {
      author_control_matrix <- as.matrix(
        population[
          population$Author == author_i &
            population$Treatment == "Control",
          robust_variables,
          drop = FALSE
        ]
      )

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

if (nrow(control_reference_audit) != 16L) {
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
    "At least one Author-specific corrected Control median is not ",
    "zero within tolerance."
  )
}

if (
  any(
    abs(control_reference_audit$Control_scaled_MAD - 1) >
      audit_tolerance
  )
) {
  stop(
    "At least one Author-specific corrected Control scaled MAD is ",
    "not one within tolerance."
  )
}

metadata <- tibble::tibble(
  Author = population$Author,
  Treatment = population$Treatment
)

# ------------------------------------------------------------
# 6. Construct and validate the Euclidean distance matrix
# ------------------------------------------------------------

distance_matrix <- stats::dist(
  behaviour_matrix,
  method = "euclidean",
  diag = FALSE,
  upper = FALSE
)

distance_values <- as.numeric(
  distance_matrix
)

if (
  length(distance_values) !=
  nrow(behaviour_matrix) *
    (nrow(behaviour_matrix) - 1) / 2
) {
  stop(
    "The Euclidean distance object has an incorrect length."
  )
}

if (
  any(!is.finite(distance_values)) ||
  any(distance_values < 0)
) {
  stop(
    "The Euclidean distance matrix contains invalid values."
  )
}

if (!identical(attr(distance_matrix, "method"), "euclidean")) {
  stop(
    "The distance object is not labelled as Euclidean."
  )
}

if (attr(distance_matrix, "Size") != nrow(population)) {
  stop(
    "The Euclidean distance object does not contain all fish."
  )
}

# ------------------------------------------------------------
# 7. Reproduce the observed centroid displacement from Script 012
# ------------------------------------------------------------

control_centroid <- colMeans(
  behaviour_matrix[
    population$Treatment == "Control",
    ,
    drop = FALSE
  ]
)

stress_centroid <- colMeans(
  behaviour_matrix[
    population$Treatment == "Stress",
    ,
    drop = FALSE
  ]
)

observed_centroid_displacement <- sqrt(
  sum(
    (
      stress_centroid -
        control_centroid
    )^2
  )
)

required_displacement_field <- "Euclidean_centroid_displacement_4D"

if (
  !required_displacement_field %in%
  names(centroid_displacement_object)
) {
  stop(
    "The Script 012 result object does not contain ",
    required_displacement_field,
    "."
  )
}

stored_centroid_displacement <- as.numeric(
  centroid_displacement_object[[required_displacement_field]]
)

if (
  length(stored_centroid_displacement) != 1L ||
  !is.finite(stored_centroid_displacement)
) {
  stop(
    "The centroid displacement stored by Script 012 is invalid."
  )
}

if (
  !isTRUE(
    all.equal(
      observed_centroid_displacement,
      stored_centroid_displacement,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The population object does not reproduce the centroid ",
    "displacement calculated in Script 012."
  )
}

# ------------------------------------------------------------
# 8. Author-adjusted PERMANOVA: Treatment effect
# ------------------------------------------------------------

permanova_permutation_design <- permute::how(
  within = permute::Within(
    type = "free"
  ),
  blocks = metadata$Author,
  nperm = n_permutations,
  complete = FALSE,
  observed = FALSE
)

if (
  !identical(
    as.character(
      permute::getStrata(
        permanova_permutation_design,
        which = "blocks",
        drop = FALSE
      )
    ),
    as.character(metadata$Author)
  )
) {
  stop(
    "The PERMANOVA permutation design does not preserve Author ",
    "as a non-movable block."
  )
}

set.seed(
  permanova_seed
)

# The explicit Author term is used for compatibility across vegan
# versions. With by = "margin", the Treatment row is tested after
# adjustment for Author. The permutation design independently
# preserves exchangeability by restricting permutations within
# Author blocks.

permanova_model <- vegan::adonis2(
  formula = distance_matrix ~
    Author +
    Treatment,
  data = metadata,
  permutations = permanova_permutation_design,
  by = "margin",
  parallel = 1
)

permanova_table <- as.data.frame(
  permanova_model,
  check.names = FALSE
) %>%
  tibble::rownames_to_column(
    var = "Term"
  ) %>%
  tibble::as_tibble()

required_permanova_columns <- c(
  "Term",
  "Df",
  "SumOfSqs",
  "R2",
  "F",
  "Pr(>F)"
)

missing_permanova_columns <- setdiff(
  required_permanova_columns,
  names(permanova_table)
)

if (length(missing_permanova_columns) > 0L) {
  stop(
    "The PERMANOVA table is missing:\n",
    paste(missing_permanova_columns, collapse = ", ")
  )
}

permanova_treatment_row <- permanova_table %>%
  dplyr::filter(
    Term == "Treatment"
  )

if (nrow(permanova_treatment_row) != 1L) {
  stop(
    "The PERMANOVA table does not contain exactly one Treatment row."
  )
}

permanova_F <- permanova_treatment_row$F
permanova_R2 <- permanova_treatment_row$R2
permanova_p <- permanova_treatment_row[["Pr(>F)"]]

if (
  any(
    !is.finite(
      c(
        permanova_F,
        permanova_R2,
        permanova_p
      )
    )
  )
) {
  stop(
    "The principal PERMANOVA statistics are non-finite."
  )
}

if (
  permanova_R2 < 0 ||
  permanova_R2 > 1 ||
  permanova_p < 0 ||
  permanova_p > 1
) {
  stop(
    "The PERMANOVA effect size or p-value is outside its valid range."
  )
}

# ------------------------------------------------------------
# 9. PERMDISP: homogeneity of multivariate dispersion
# ------------------------------------------------------------

# type = "median" uses the group spatial median and is the
# robust default in vegan. bias.adjust = FALSE preserves the
# original Euclidean distance scale. Because both groups have
# n = 89, no unequal-sample-size correction is required.

dispersion_model <- vegan::betadisper(
  d = distance_matrix,
  group = metadata$Treatment,
  type = "median",
  bias.adjust = FALSE,
  sqrt.dist = FALSE,
  add = FALSE
)

if (
  length(dispersion_model$distances) !=
  nrow(population)
) {
  stop(
    "The PERMDISP model does not contain one distance per fish."
  )
}

if (
  any(!is.finite(dispersion_model$distances)) ||
  any(dispersion_model$distances < 0)
) {
  stop(
    "The distances to group spatial medians are invalid."
  )
}

dispersion_eigenvalues <- vegan::eigenvals(
  dispersion_model
)

substantive_negative_eigenvalues <- dispersion_eigenvalues[
  dispersion_eigenvalues < -1e-10
]

if (length(substantive_negative_eigenvalues) > 0L) {
  stop(
    "Substantive negative eigenvalues were detected despite using ",
    "Euclidean distances."
  )
}

# Classical ANOVA is retained as a secondary diagnostic.
# The permutation result below is the primary inferential result.

dispersion_anova <- stats::anova(
  dispersion_model
)

dispersion_anova_table <- as.data.frame(
  dispersion_anova,
  check.names = FALSE
) %>%
  tibble::rownames_to_column(
    var = "Term"
  ) %>%
  tibble::as_tibble()

permdisp_permutation_design <- permute::how(
  within = permute::Within(
    type = "free"
  ),
  blocks = metadata$Author,
  nperm = n_permutations,
  complete = FALSE,
  observed = FALSE
)

if (
  !identical(
    as.character(
      permute::getStrata(
        permdisp_permutation_design,
        which = "blocks",
        drop = FALSE
      )
    ),
    as.character(metadata$Author)
  )
) {
  stop(
    "The PERMDISP permutation design does not preserve Author ",
    "as a non-movable block."
  )
}

set.seed(
  permdisp_seed
)

dispersion_permutation_test <- vegan::permutest(
  dispersion_model,
  pairwise = FALSE,
  permutations = permdisp_permutation_design,
  parallel = 1
)

if (
  is.null(dispersion_permutation_test$tab) ||
  !is.data.frame(dispersion_permutation_test$tab)
) {
  stop(
    "The PERMDISP permutation result does not contain the expected ",
    "ANOVA table."
  )
}

permdisp_table <- dispersion_permutation_test$tab %>%
  as.data.frame(
    check.names = FALSE
  ) %>%
  tibble::rownames_to_column(
    var = "Term"
  ) %>%
  tibble::as_tibble()

required_permdisp_columns <- c(
  "Term",
  "Df",
  "Sum Sq",
  "Mean Sq",
  "F",
  "N.Perm",
  "Pr(>F)"
)

missing_permdisp_columns <- setdiff(
  required_permdisp_columns,
  names(permdisp_table)
)

if (length(missing_permdisp_columns) > 0L) {
  stop(
    "The PERMDISP permutation table is missing:\n",
    paste(missing_permdisp_columns, collapse = ", ")
  )
}

permdisp_group_row <- permdisp_table %>%
  dplyr::filter(
    Term == "Groups"
  )

if (nrow(permdisp_group_row) != 1L) {
  stop(
    "The PERMDISP table does not contain exactly one Groups row."
  )
}

permdisp_F <- permdisp_group_row$F
permdisp_NPerm <- permdisp_group_row$N.Perm
permdisp_p <- permdisp_group_row[["Pr(>F)"]]

if (
  any(
    !is.finite(
      c(
        permdisp_F,
        permdisp_NPerm,
        permdisp_p
      )
    )
  )
) {
  stop(
    "The principal PERMDISP statistics are non-finite."
  )
}

if (permdisp_NPerm != n_permutations) {
  stop(
    "PERMDISP did not use the requested number of permutations."
  )
}

if (
  permdisp_p < 0 ||
  permdisp_p > 1
) {
  stop(
    "The PERMDISP p-value is outside its valid range."
  )
}

# ------------------------------------------------------------
# 10. Descriptive multivariate dispersion
# ------------------------------------------------------------

dispersion_individual <- tibble::tibble(
  Author = metadata$Author,
  Treatment = metadata$Treatment,
  Distance_to_group_spatial_median =
    as.numeric(
      dispersion_model$distances
    )
)

dispersion_descriptives <- dispersion_individual %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean_distance = mean(
      Distance_to_group_spatial_median
    ),
    SD = stats::sd(
      Distance_to_group_spatial_median
    ),
    Median_distance = stats::median(
      Distance_to_group_spatial_median
    ),
    Q1 = as.numeric(
      stats::quantile(
        Distance_to_group_spatial_median,
        probs = 0.25,
        names = FALSE,
        type = 7
      )
    ),
    Q3 = as.numeric(
      stats::quantile(
        Distance_to_group_spatial_median,
        probs = 0.75,
        names = FALSE,
        type = 7
      )
    ),
    IQR = stats::IQR(
      Distance_to_group_spatial_median,
      type = 7
    ),
    Minimum = min(
      Distance_to_group_spatial_median
    ),
    Maximum = max(
      Distance_to_group_spatial_median
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Treatment)

if (
  nrow(dispersion_descriptives) != 2L ||
  !identical(
    as.character(
      dispersion_descriptives$Treatment
    ),
    required_treatments
  )
) {
  stop(
    "The dispersion descriptive table is incomplete or incorrectly ",
    "ordered."
  )
}

control_mean_dispersion <- dispersion_descriptives$
  Mean_distance[
    dispersion_descriptives$Treatment == "Control"
  ]

stress_mean_dispersion <- dispersion_descriptives$
  Mean_distance[
    dispersion_descriptives$Treatment == "Stress"
  ]

control_median_dispersion <- dispersion_descriptives$
  Median_distance[
    dispersion_descriptives$Treatment == "Control"
  ]

stress_median_dispersion <- dispersion_descriptives$
  Median_distance[
    dispersion_descriptives$Treatment == "Stress"
  ]

dispersion_contrast <- tibble::tibble(
  Comparison = "Stress minus Control",
  Distance_definition = "Euclidean distance to group spatial median",
  Control_mean_distance = control_mean_dispersion,
  Stress_mean_distance = stress_mean_dispersion,
  Mean_difference = (
    stress_mean_dispersion -
      control_mean_dispersion
  ),
  Stress_to_Control_mean_ratio = (
    stress_mean_dispersion /
      control_mean_dispersion
  ),
  Control_median_distance = control_median_dispersion,
  Stress_median_distance = stress_median_dispersion,
  Median_difference = (
    stress_median_dispersion -
      control_median_dispersion
  )
)

# ------------------------------------------------------------
# 11. Integrated interpretation flag
# ------------------------------------------------------------

alpha_interpretation <- 0.05

interpretation_flag <- dplyr::case_when(
  permanova_p < alpha_interpretation &
    permdisp_p >= alpha_interpretation ~
    paste0(
      "Author-adjusted treatment-associated multivariate location ",
      "difference ",
      "without evidence of unequal multivariate dispersion."
    ),

  permanova_p < alpha_interpretation &
    permdisp_p < alpha_interpretation ~
    paste0(
      "Author-adjusted treatment-associated multivariate difference ",
      "accompanied ",
      "by unequal multivariate dispersion; PERMANOVA must be ",
      "interpreted jointly with PERMDISP."
    ),

  permanova_p >= alpha_interpretation &
    permdisp_p < alpha_interpretation ~
    paste0(
      "No PERMANOVA evidence of an Author-adjusted treatment-associated ",
      "location ",
      "difference, but multivariate dispersion differs between groups."
    ),

  TRUE ~
    paste0(
      "No permutation evidence of either an Author-adjusted ",
      "treatment-associated multivariate location difference or ",
      "unequal multivariate dispersion at alpha = 0.05."
    )
)

analysis_summary <- tibble::tibble(
  Comparison = "Control versus Stress",
  Design = "Independent fish within four Author cohorts",
  Coordinate_system = "Control-referenced robust behavioural space",
  Distance_metric = "Euclidean",
  Dimensions = length(robust_variables),
  Authors = length(expected_authors),
  Control_n = group_counts$n[
    group_counts$Treatment == "Control"
  ],
  Stress_n = group_counts$n[
    group_counts$Treatment == "Stress"
  ],
  Observed_centroid_displacement_4D =
    observed_centroid_displacement,
  PERMANOVA_model =
    "Author + Treatment; marginal Treatment test",
  PERMANOVA_permutation_scheme =
    "Free permutations restricted within Author blocks",
  PERMANOVA_F = permanova_F,
  PERMANOVA_R2 = permanova_R2,
  PERMANOVA_p = permanova_p,
  PERMDISP_reference = "Group spatial median",
  PERMDISP_permutation_scheme =
    "Residual permutations restricted within Author blocks",
  PERMDISP_F = permdisp_F,
  PERMDISP_NPerm = permdisp_NPerm,
  PERMDISP_p = permdisp_p,
  Permutations = n_permutations,
  Interpretation_flag = interpretation_flag
)

# ------------------------------------------------------------
# 12. Save definitive corrected outputs
# ------------------------------------------------------------

results_object <- list(
  Objective = "Objective 2",
  Comparison_design =
    "Independent Control and Stress fish within four Author cohorts",
  Input_population_file = population_file,
  Input_centroid_displacement_file = centroid_displacement_file,
  Coordinate_system = "Control-referenced robust behavioural space",
  Behavioural_variables = behavioural_variables,
  Robust_variables = robust_variables,
  Distance_metric = "Euclidean",
  Group_counts = group_counts,
  Author_by_Treatment_counts = cohort_group_counts,
  Author_reference_audit = control_reference_audit,
  Observed_centroid_displacement_4D =
    observed_centroid_displacement,
  PERMANOVA = list(
    Model = permanova_model,
    Table = permanova_table,
    Formula = "distance_matrix ~ Author + Treatment",
    Adjustment_variable = "Author",
    Test_type = "Marginal Treatment test (by = 'margin')",
    Permutations = n_permutations,
    Seed = permanova_seed,
    Permutation_scheme =
      "Free permutations restricted within Author blocks",
    Permutation_design = permanova_permutation_design
  ),
  PERMDISP = list(
    Model = dispersion_model,
    Unblocked_parametric_ANOVA_secondary = dispersion_anova,
    Unblocked_parametric_ANOVA_table_secondary =
      dispersion_anova_table,
    Permutation_test =
      dispersion_permutation_test,
    Permutation_table = permdisp_table,
    Centre_type = "Spatial median",
    Bias_adjustment = FALSE,
    Permutations = n_permutations,
    Seed = permdisp_seed,
    Permutation_scheme =
      "Residual permutations restricted within Author blocks",
    Permutation_design = permdisp_permutation_design,
    Eigenvalues = dispersion_eigenvalues
  ),
  Dispersion_individual = dispersion_individual,
  Dispersion_descriptives = dispersion_descriptives,
  Dispersion_contrast = dispersion_contrast,
  Analysis_summary = analysis_summary
)

saveRDS(
  distance_matrix,
  distance_output
)

saveRDS(
  dispersion_model,
  betadisper_output
)

saveRDS(
  results_object,
  results_output
)

utils::write.csv(
  permanova_table,
  permanova_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  permdisp_table,
  permdisp_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  cohort_group_counts,
  cohort_counts_audit_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  control_reference_audit,
  author_reference_audit_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dispersion_anova_table,
  permdisp_anova_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dispersion_descriptives,
  dispersion_descriptives_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  dispersion_contrast,
  dispersion_contrast_csv_output,
  row.names = FALSE,
  na = ""
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 13. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  distance_output,
  betadisper_output,
  results_output,
  permanova_csv_output,
  permdisp_csv_output,
  cohort_counts_audit_csv_output,
  author_reference_audit_csv_output,
  permdisp_anova_csv_output,
  dispersion_descriptives_csv_output,
  dispersion_contrast_csv_output,
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
# 14. Identify obsolete outputs from the former workflow
# ------------------------------------------------------------

legacy_population_file <- file.path(
  processed_dir,
  "population_control_stress_z.rds"
)

legacy_files <- c(
  legacy_population_file,
  file.path(
    project_root,
    "03_Results",
    "Statistics",
    "PERMANOVA_Control_Stress.csv"
  ),
  file.path(
    project_root,
    "03_Results",
    "Statistics",
    "Betadisper_Control_Stress.csv"
  ),
  file.path(
    tables_dir,
    "Objective2_PERMDISP_ParametricANOVA_4D_RobustZ.csv"
  )
)

existing_legacy_files <- legacy_files[
  file.exists(legacy_files)
]

if (length(existing_legacy_files) > 0L) {
  warning(
    "Legacy files from former Objective 2 workflows were detected. ",
    "They were not used or overwritten:\n",
    paste(existing_legacy_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 15. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - PERMANOVA AND MULTIVARIATE DISPERSION\n")
cat("============================================================\n")

cat(
  "\nIndependent fish by Treatment:\n"
)

print(
  group_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nAuthor x Treatment permutation blocks:\n"
)

print(
  cohort_group_counts,
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
  "\nObserved 4D centroid displacement reproduced from Script 012: ",
  format(
    observed_centroid_displacement,
    digits = 10
  ),
  "\n",
  sep = ""
)

cat(
  "\nAuthor-adjusted PERMANOVA for Treatment:\n",
  "Model: Author + Treatment; Treatment tested marginally after ",
  "adjustment for Author.\n",
  "Permutation scheme: free permutations restricted within Author ",
  "blocks (9,999).\n",
  sep = ""
)

print(
  permanova_table,
  n = Inf,
  width = Inf
)

cat(
  "\nPERMDISP permutation test ",
  "(distances to group spatial medians):\n",
  "Permutation scheme: residual permutations restricted within ",
  "Author blocks (9,999).\n",
  sep = ""
)

print(
  permdisp_table,
  n = Inf,
  width = Inf
)

cat(
  "\nDescriptive multivariate dispersion:\n"
)

print(
  dispersion_descriptives,
  n = Inf,
  width = Inf
)

cat(
  "\nStress-minus-Control dispersion contrast:\n"
)

print(
  dispersion_contrast,
  n = Inf,
  width = Inf
)

cat(
  "\nIntegrated interpretation:\n",
  interpretation_flag,
  "\n",
  sep = ""
)

cat(
  "\nAnalytical constraint: PERMANOVA and PERMDISP must be ",
  "interpreted jointly because distance-based location tests can ",
  "be affected by differences in within-group dispersion.\n",
  sep = ""
)

cat(
  "\nDefinitive corrected outputs saved as:\n",
  normalizePath(
    distance_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    betadisper_output,
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
    permanova_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    permdisp_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    cohort_counts_audit_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    author_reference_audit_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    permdisp_anova_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    dispersion_descriptives_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    dispersion_contrast_csv_output,
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
