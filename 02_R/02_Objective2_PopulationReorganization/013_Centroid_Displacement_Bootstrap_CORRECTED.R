# ============================================================
# 013_Centroid_Displacement_Bootstrap.R
#
# Objective 2
# Bootstrap uncertainty for the observed 4D centroid displacement
# between the independent Control and Stress populations
#
# This script:
#   1. loads the definitive corrected objects produced by Scripts
#      011 and 012;
#   2. verifies the observed 4D displacement independently;
#   3. resamples fish independently within every Author x Treatment
#      stratum, preserving the original cohort sizes;
#   4. re-estimates the control-reference median and scaled MAD
#      separately for every Author within every bootstrap replicate;
#   5. reapplies each replicate-specific Author control reference to
#      Control and Stress fish from that same Author;
#   6. explicitly records and redraws the rare bootstrap attempts in
#      which an Author-specific resampled Control MAD is zero;
#   7. calculates percentile confidence intervals for the total
#      4D displacement and its four directional components.
#
# The bootstrap interval quantifies sampling uncertainty. It is not
# a null-hypothesis test of equality between populations.
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
  "tidyr",
  "tibble"
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

# ------------------------------------------------------------
# 3. Analysis settings
# ------------------------------------------------------------

bootstrap_seed <- 1234L
B <- 5000L
confidence_level <- 0.95

if (
  length(B) != 1L ||
  !is.finite(B) ||
  B < 1000L ||
  B != as.integer(B)
) {
  stop(
    "B must be a single integer of at least 1,000."
  )
}

if (
  length(confidence_level) != 1L ||
  !is.finite(confidence_level) ||
  confidence_level <= 0 ||
  confidence_level >= 1
) {
  stop(
    "confidence_level must lie strictly between 0 and 1."
  )
}

alpha <- 1 - confidence_level
ci_probabilities <- c(
  alpha / 2,
  1 - alpha / 2
)

# A finite safeguard for the conditional redraw procedure. This does
# not alter B: B remains the required number of valid replicates.
maximum_bootstrap_attempts <- as.integer(
  10L * B
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

raw_population_file <- file.path(
  processed_dir,
  "LAPCOM_raw.rds"
)

corrected_population_file <- file.path(
  processed_dir,
  "population_control_stress_robust_z.rds"
)

observed_displacement_file <- file.path(
  objects_dir,
  "objective2_centroid_displacement_4d_robust.rds"
)

required_input_files <- c(
  raw_population_file,
  corrected_population_file,
  observed_displacement_file
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

bootstrap_object_output <- file.path(
  objects_dir,
  "objective2_centroid_displacement_bootstrap_4d_robust.rds"
)

bootstrap_distance_output <- file.path(
  objects_dir,
  "objective2_bootstrap_centroid_distance_4d_robust.rds"
)

bootstrap_summary_output <- file.path(
  tables_dir,
  "Objective2_Centroid_Displacement_Bootstrap_Summary_4D_RobustZ.csv"
)

bootstrap_components_output <- file.path(
  tables_dir,
  "Objective2_Centroid_Displacement_Bootstrap_Components_4D_RobustZ.csv"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_centroid_displacement_bootstrap_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load source objects
# ------------------------------------------------------------

population_raw <- readRDS(
  raw_population_file
)

population_corrected <- readRDS(
  corrected_population_file
)

observed_displacement_object <- readRDS(
  observed_displacement_file
)

if (!is.data.frame(population_raw)) {
  stop(
    basename(raw_population_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(population_corrected)) {
  stop(
    basename(corrected_population_file),
    " does not contain a data frame."
  )
}

if (!is.list(observed_displacement_object)) {
  stop(
    basename(observed_displacement_file),
    " does not contain the expected result list."
  )
}

# ------------------------------------------------------------
# 6. Define and validate the analytical structure
# ------------------------------------------------------------

treatment_variable <- "Treatment"
author_variable <- "Author"
id_variable <- "ID"

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

required_treatments <- c(
  "Control",
  "Stress"
)

required_raw_columns <- c(
  author_variable,
  id_variable,
  treatment_variable,
  behavioural_variables
)

required_corrected_columns <- c(
  author_variable,
  id_variable,
  treatment_variable,
  behavioural_variables,
  robust_variables
)

missing_raw_columns <- setdiff(
  required_raw_columns,
  names(population_raw)
)

if (length(missing_raw_columns) > 0L) {
  stop(
    "LAPCOM_raw.rds is missing the following columns:\n",
    paste(missing_raw_columns, collapse = ", ")
  )
}

missing_corrected_columns <- setdiff(
  required_corrected_columns,
  names(population_corrected)
)

if (length(missing_corrected_columns) > 0L) {
  stop(
    "The corrected population object is missing the following columns:\n",
    paste(missing_corrected_columns, collapse = ", ")
  )
}

non_numeric_raw_variables <- behavioural_variables[
  !vapply(
    population_raw[behavioural_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_raw_variables) > 0L) {
  stop(
    "The following raw behavioural variables are not numeric:\n",
    paste(non_numeric_raw_variables, collapse = ", ")
  )
}

non_numeric_corrected_variables <- robust_variables[
  !vapply(
    population_corrected[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_corrected_variables) > 0L) {
  stop(
    "The following corrected behavioural variables are not numeric:\n",
    paste(non_numeric_corrected_variables, collapse = ", ")
  )
}

raw_behaviour_matrix <- as.matrix(
  population_raw[behavioural_variables]
)

corrected_behaviour_matrix <- as.matrix(
  population_corrected[robust_variables]
)

storage.mode(raw_behaviour_matrix) <- "double"
storage.mode(corrected_behaviour_matrix) <- "double"

if (any(!is.finite(raw_behaviour_matrix))) {
  stop(
    "Missing or non-finite values were detected in the raw ",
    "behavioural variables."
  )
}

if (any(!is.finite(corrected_behaviour_matrix))) {
  stop(
    "Missing or non-finite values were detected in the corrected ",
    "behavioural variables."
  )
}

for (data_name in c("population_raw", "population_corrected")) {
  data_i <- get(data_name)

  if (
    any(is.na(data_i[[author_variable]])) ||
    any(trimws(as.character(data_i[[author_variable]])) == "") ||
    any(is.na(data_i[[id_variable]])) ||
    any(trimws(as.character(data_i[[id_variable]])) == "")
  ) {
    stop(
      data_name,
      " contains missing or blank Author/ID values."
    )
  }
}

raw_treatment_values <- unique(
  as.character(
    population_raw[[treatment_variable]]
  )
)

corrected_treatment_values <- unique(
  as.character(
    population_corrected[[treatment_variable]]
  )
)

if (!setequal(raw_treatment_values, required_treatments)) {
  stop(
    "LAPCOM_raw.rds does not contain exactly the Control and Stress ",
    "treatment levels."
  )
}

if (!setequal(corrected_treatment_values, required_treatments)) {
  stop(
    "The corrected population object does not contain exactly the ",
    "Control and Stress treatment levels."
  )
}

population_raw <- population_raw %>%
  dplyr::mutate(
    Author = as.character(.data[[author_variable]]),
    ID = as.character(.data[[id_variable]]),
    Treatment = factor(
      as.character(.data[[treatment_variable]]),
      levels = required_treatments
    )
  )

population_corrected <- population_corrected %>%
  dplyr::mutate(
    Author = as.character(.data[[author_variable]]),
    ID = as.character(.data[[id_variable]]),
    Treatment = factor(
      as.character(.data[[treatment_variable]]),
      levels = required_treatments
    )
  )

raw_group_counts <- population_raw %>%
  dplyr::count(
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(Treatment)

corrected_group_counts <- population_corrected %>%
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

if (!identical(raw_group_counts, expected_group_counts)) {
  stop(
    "Unexpected group counts were detected in LAPCOM_raw.rds."
  )
}

if (!identical(corrected_group_counts, expected_group_counts)) {
  stop(
    "Unexpected group counts were detected in the corrected ",
    "population object."
  )
}

expected_authors <- c(
  "1",
  "2",
  "3",
  "4"
)

if (
  !setequal(unique(population_raw$Author), expected_authors) ||
  !setequal(unique(population_corrected$Author), expected_authors)
) {
  stop(
    "Both input objects must contain exactly Authors 1, 2, 3, and 4."
  )
}

raw_cohort_group_counts <- population_raw %>%
  dplyr::count(
    Author,
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(
    match(Author, expected_authors),
    Treatment
  )

corrected_cohort_group_counts <- population_corrected %>%
  dplyr::count(
    Author,
    Treatment,
    name = "n"
  ) %>%
  dplyr::arrange(
    match(Author, expected_authors),
    Treatment
  )

expected_cohort_group_counts <- tibble::tibble(
  Author = rep(
    expected_authors,
    each = length(required_treatments)
  ),
  Treatment = factor(
    rep(
      required_treatments,
      times = length(expected_authors)
    ),
    levels = required_treatments
  ),
  n = c(
    24L, 24L,
    11L, 12L,
    18L, 17L,
    36L, 36L
  )
)

if (!identical(raw_cohort_group_counts, expected_cohort_group_counts)) {
  stop(
    "Unexpected Author x Treatment counts were detected in LAPCOM_raw.rds."
  )
}

if (!identical(
  corrected_cohort_group_counts,
  expected_cohort_group_counts
)) {
  stop(
    "Unexpected Author x Treatment counts were detected in the corrected ",
    "population object."
  )
}

raw_keys <- paste(
  population_raw$Author,
  population_raw$ID,
  sep = "\r"
)

corrected_keys <- paste(
  population_corrected$Author,
  population_corrected$ID,
  sep = "\r"
)

if (anyDuplicated(raw_keys) > 0L) {
  stop(
    "Duplicated Author + ID keys were found in LAPCOM_raw.rds."
  )
}

if (anyDuplicated(corrected_keys) > 0L) {
  stop(
    "Duplicated Author + ID keys were found in the corrected population object."
  )
}

population_raw_ordered <- population_raw %>%
  dplyr::arrange(
    Author,
    ID
  )

population_corrected_ordered <- population_corrected %>%
  dplyr::arrange(
    Author,
    ID
  )

identity_and_raw_columns <- c(
  author_variable,
  id_variable,
  treatment_variable,
  behavioural_variables
)

if (
  !isTRUE(
    all.equal(
      as.data.frame(
        population_raw_ordered[identity_and_raw_columns]
      ),
      as.data.frame(
        population_corrected_ordered[identity_and_raw_columns]
      ),
      tolerance = 0,
      check.attributes = FALSE
    )
  )
) {
  stop(
    "The raw and corrected input objects do not contain identical fish and ",
    "raw measurements when matched by Author + ID."
  )
}

control_raw <- population_raw %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::select(
    dplyr::all_of(
      c(
        author_variable,
        id_variable,
        behavioural_variables
      )
    )
  )

stress_raw <- population_raw %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::select(
    dplyr::all_of(
      c(
        author_variable,
        id_variable,
        behavioural_variables
      )
    )
  )

control_corrected <- population_corrected %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::select(
    dplyr::all_of(robust_variables)
  )

stress_corrected <- population_corrected %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::select(
    dplyr::all_of(robust_variables)
  )

# ------------------------------------------------------------
# 7. Verify the observed displacement independently
# ------------------------------------------------------------

observed_control_centroid <- colMeans(
  as.matrix(control_corrected)
)

observed_stress_centroid <- colMeans(
  as.matrix(stress_corrected)
)

observed_displacement_vector <- (
  observed_stress_centroid -
    observed_control_centroid
)

observed_distance_direct <- sqrt(
  sum(
    observed_displacement_vector^2
  )
)

required_observed_fields <- c(
  "Euclidean_centroid_displacement_4D",
  "Displacement_vector"
)

missing_observed_fields <- setdiff(
  required_observed_fields,
  names(observed_displacement_object)
)

if (length(missing_observed_fields) > 0L) {
  stop(
    "The Script 012 displacement object is missing:\n",
    paste(missing_observed_fields, collapse = ", ")
  )
}

observed_distance_script012 <- as.numeric(
  observed_displacement_object$
    Euclidean_centroid_displacement_4D
)

observed_vector_script012 <- as.numeric(
  observed_displacement_object$
    Displacement_vector[
      robust_variables
    ]
)

if (
  length(observed_distance_script012) != 1L ||
  !is.finite(observed_distance_script012)
) {
  stop(
    "The observed distance stored by Script 012 is invalid."
  )
}

if (
  length(observed_vector_script012) !=
  length(robust_variables) ||
  any(!is.finite(observed_vector_script012))
) {
  stop(
    "The displacement vector stored by Script 012 is invalid."
  )
}

if (
  !isTRUE(
    all.equal(
      observed_distance_direct,
      observed_distance_script012,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The observed distance cannot be reproduced from the corrected ",
    "population object."
  )
}

if (
  !isTRUE(
    all.equal(
      unname(observed_displacement_vector),
      unname(observed_vector_script012),
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The observed displacement vector cannot be reproduced from ",
    "the corrected population object."
  )
}

names(observed_displacement_vector) <- robust_variables

# ------------------------------------------------------------
# 8. Bootstrap helper: full control-reference workflow
# ------------------------------------------------------------

calculate_bootstrap_displacement <- function(
    control_boot,
    stress_boot,
    variable_names,
    author_values
) {

  control_standardised_by_author <- vector(
    "list",
    length(author_values)
  )

  stress_standardised_by_author <- vector(
    "list",
    length(author_values)
  )

  for (author_index in seq_along(author_values)) {

    author_i <- author_values[author_index]

    control_matrix_i <- as.matrix(
      control_boot[
        as.character(control_boot$Author) == author_i,
        variable_names,
        drop = FALSE
      ]
    )

    stress_matrix_i <- as.matrix(
      stress_boot[
        as.character(stress_boot$Author) == author_i,
        variable_names,
        drop = FALSE
      ]
    )

    storage.mode(control_matrix_i) <- "double"
    storage.mode(stress_matrix_i) <- "double"

    if (
      nrow(control_matrix_i) == 0L ||
      nrow(stress_matrix_i) == 0L
    ) {
      return(
        c(
          Distance = NA_real_,
          stats::setNames(
            rep(
              NA_real_,
              length(variable_names)
            ),
            paste0(
              "Delta_",
              variable_names
            )
          )
        )
      )
    }

    control_medians_i <- apply(
      control_matrix_i,
      2,
      stats::median
    )

    control_mad_raw_i <- vapply(
      seq_along(variable_names),
      function(j) {
        stats::mad(
          control_matrix_i[, j],
          center = control_medians_i[j],
          constant = 1
        )
      },
      numeric(1)
    )

    control_mad_scaled_i <- (
      1.4826 *
        control_mad_raw_i
    )

    if (
      any(!is.finite(control_medians_i)) ||
      any(!is.finite(control_mad_scaled_i)) ||
      any(control_mad_scaled_i <= 0)
    ) {
      return(
        c(
          Distance = NA_real_,
          stats::setNames(
            rep(
              NA_real_,
              length(variable_names)
            ),
            paste0(
              "Delta_",
              variable_names
            )
          )
        )
      )
    }

    control_standardised_i <- sweep(
      control_matrix_i,
      2,
      control_medians_i,
      FUN = "-"
    )

    control_standardised_i <- sweep(
      control_standardised_i,
      2,
      control_mad_scaled_i,
      FUN = "/"
    )

    stress_standardised_i <- sweep(
      stress_matrix_i,
      2,
      control_medians_i,
      FUN = "-"
    )

    stress_standardised_i <- sweep(
      stress_standardised_i,
      2,
      control_mad_scaled_i,
      FUN = "/"
    )

    control_standardised_by_author[[author_index]] <-
      control_standardised_i

    stress_standardised_by_author[[author_index]] <-
      stress_standardised_i
  }

  control_standardised <- do.call(
    rbind,
    control_standardised_by_author
  )

  stress_standardised <- do.call(
    rbind,
    stress_standardised_by_author
  )

  control_centroid_boot <- colMeans(
    control_standardised
  )

  stress_centroid_boot <- colMeans(
    stress_standardised
  )

  displacement_boot <- (
    stress_centroid_boot -
      control_centroid_boot
  )

  distance_boot <- sqrt(
    sum(
      displacement_boot^2
    )
  )

  c(
    Distance = unname(distance_boot),
    stats::setNames(
      unname(displacement_boot),
      paste0(
        "Delta_",
        variable_names
      )
    )
  )
}

# The complete Author-specific procedure, evaluated on the original
# observations without resampling, must reproduce Script 012 exactly.
full_sample_workflow <- calculate_bootstrap_displacement(
  control_boot = control_raw,
  stress_boot = stress_raw,
  variable_names = behavioural_variables,
  author_values = expected_authors
)

full_sample_distance <- as.numeric(
  full_sample_workflow["Distance"]
)

full_sample_vector <- as.numeric(
  full_sample_workflow[
    paste0(
      "Delta_",
      behavioural_variables
    )
  ]
)

if (
  length(full_sample_distance) != 1L ||
  !is.finite(full_sample_distance) ||
  any(!is.finite(full_sample_vector))
) {
  stop(
    "The full Author-specific control-reference workflow produced ",
    "non-finite estimates before bootstrapping."
  )
}

if (
  !isTRUE(
    all.equal(
      full_sample_distance,
      observed_distance_direct,
      tolerance = 1e-12
    )
  ) ||
  !isTRUE(
    all.equal(
      unname(full_sample_vector),
      unname(observed_displacement_vector),
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The Author-specific full-sample workflow does not reproduce the ",
    "observed displacement from Script 012."
  )
}

# ------------------------------------------------------------
# 9. Run the Author x Treatment-stratified non-parametric bootstrap
# ------------------------------------------------------------

set.seed(
  bootstrap_seed
)

bootstrap_result_names <- c(
  "Distance",
  paste0(
    "Delta_",
    behavioural_variables
  )
)

bootstrap_results <- matrix(
  NA_real_,
  nrow = B,
  ncol = length(bootstrap_result_names),
  dimnames = list(
    NULL,
    bootstrap_result_names
  )
)

control_raw_by_author <- split(
  control_raw,
  factor(
    control_raw$Author,
    levels = expected_authors
  ),
  drop = TRUE
)

stress_raw_by_author <- split(
  stress_raw,
  factor(
    stress_raw$Author,
    levels = expected_authors
  ),
  drop = TRUE
)

if (
  !identical(names(control_raw_by_author), expected_authors) ||
  !identical(names(stress_raw_by_author), expected_authors)
) {
  stop(
    "The bootstrap strata could not be constructed for all four Authors."
  )
}

resample_author_strata <- function(strata_list) {
  sampled_strata <- lapply(
    strata_list,
    function(stratum_i) {
      stratum_n <- nrow(stratum_i)

      stratum_i[
        sample.int(
          n = stratum_n,
          size = stratum_n,
          replace = TRUE
        ),
        ,
        drop = FALSE
      ]
    }
  )

  result <- dplyr::bind_rows(
    sampled_strata
  )

  rownames(result) <- NULL
  result
}

valid_bootstrap_replicates <- 0L
total_bootstrap_attempts <- 0L
invalid_bootstrap_attempts <- 0L

while (
  valid_bootstrap_replicates < B &&
  total_bootstrap_attempts < maximum_bootstrap_attempts
) {

  total_bootstrap_attempts <- total_bootstrap_attempts + 1L

  control_boot <- resample_author_strata(
    control_raw_by_author
  )

  stress_boot <- resample_author_strata(
    stress_raw_by_author
  )

  bootstrap_result_i <- calculate_bootstrap_displacement(
    control_boot = control_boot,
    stress_boot = stress_boot,
    variable_names = behavioural_variables,
    author_values = expected_authors
  )

  bootstrap_result_i <- bootstrap_result_i[
    bootstrap_result_names
  ]

  if (
    length(bootstrap_result_i) ==
      length(bootstrap_result_names) &&
    all(is.finite(bootstrap_result_i)) &&
    bootstrap_result_i["Distance"] >= 0
  ) {
    valid_bootstrap_replicates <-
      valid_bootstrap_replicates + 1L

    bootstrap_results[
      valid_bootstrap_replicates,
    ] <- bootstrap_result_i
  } else {
    invalid_bootstrap_attempts <-
      invalid_bootstrap_attempts + 1L
  }
}

if (valid_bootstrap_replicates != B) {
  stop(
    "Only ",
    valid_bootstrap_replicates,
    " valid bootstrap replicates were obtained after ",
    total_bootstrap_attempts,
    " attempts. The frequency of zero Author-specific resampled ",
    "Control MADs is too high for this conditional bootstrap."
  )
}

invalid_bootstrap_attempt_fraction <-
  invalid_bootstrap_attempts /
  total_bootstrap_attempts

if (
  any(!stats::complete.cases(bootstrap_results)) ||
  any(!is.finite(bootstrap_results))
) {
  stop(
    "The retained bootstrap matrix contains incomplete or non-finite ",
    "estimates."
  )
}

boot_distance <- bootstrap_results[, "Distance"]

if (
  length(boot_distance) != B ||
  any(!is.finite(boot_distance)) ||
  any(boot_distance < 0)
) {
  stop(
    "The bootstrap distance distribution is invalid."
  )
}

# ------------------------------------------------------------
# 10. Summarise the total 4D displacement
# ------------------------------------------------------------

distance_ci <- stats::quantile(
  boot_distance,
  probs = ci_probabilities,
  names = FALSE,
  type = 7
)

centroid_bootstrap_summary <- tibble::tibble(
  Comparison = "Stress minus Control",
  Space = "Four-dimensional control-referenced robust behavioural space",
  Bootstrap_scheme = paste0(
    "Independent resampling within Author x Treatment strata with ",
    "replicate-specific re-estimation of the Control median and ",
    "scaled MAD within each Author; attempts with a zero or ",
    "non-finite Author-specific Control MAD were recorded and redrawn"
  ),
  Interval_method = paste0(
    100 * confidence_level,
    "% conditional percentile bootstrap interval"
  ),
  Observed_distance = observed_distance_direct,
  Bootstrap_mean = mean(boot_distance),
  Bootstrap_median = stats::median(boot_distance),
  Bootstrap_bias = mean(boot_distance) - observed_distance_direct,
  Bootstrap_SE = stats::sd(boot_distance),
  CI_low = distance_ci[1],
  CI_high = distance_ci[2],
  Minimum = min(boot_distance),
  Maximum = max(boot_distance),
  B = B,
  Total_attempts = total_bootstrap_attempts,
  Invalid_attempts = invalid_bootstrap_attempts,
  Invalid_attempt_fraction = invalid_bootstrap_attempt_fraction,
  Seed = bootstrap_seed
)

# ------------------------------------------------------------
# 11. Summarise directional components
# ------------------------------------------------------------

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

component_summary_list <- lapply(
  seq_along(behavioural_variables),
  function(j) {

    variable_i <- behavioural_variables[j]
    boot_column_i <- paste0(
      "Delta_",
      variable_i
    )

    component_distribution_i <- bootstrap_results[
      ,
      boot_column_i
    ]

    component_ci_i <- stats::quantile(
      component_distribution_i,
      probs = ci_probabilities,
      names = FALSE,
      type = 7
    )

    tibble::tibble(
      Variable = variable_i,
      Behaviour = unname(
        variable_labels[variable_i]
      ),
      Robust_variable = robust_variables[j],
      Direction = "Stress minus Control",
      Observed_component = as.numeric(
        observed_displacement_vector[
          robust_variables[j]
        ]
      ),
      Bootstrap_mean = mean(
        component_distribution_i
      ),
      Bootstrap_median = stats::median(
        component_distribution_i
      ),
      Bootstrap_bias = (
        mean(component_distribution_i) -
          observed_displacement_vector[
            robust_variables[j]
          ]
      ),
      Bootstrap_SE = stats::sd(
        component_distribution_i
      ),
      CI_low = component_ci_i[1],
      CI_high = component_ci_i[2],
      Proportion_below_zero = mean(
        component_distribution_i < 0
      ),
      Proportion_above_zero = mean(
        component_distribution_i > 0
      ),
      B = B
    )
  }
)

centroid_bootstrap_components <- dplyr::bind_rows(
  component_summary_list
)

# ------------------------------------------------------------
# 12. Internal bootstrap checks
# ------------------------------------------------------------

if (
  centroid_bootstrap_summary$CI_low >
  centroid_bootstrap_summary$Observed_distance ||
  centroid_bootstrap_summary$CI_high <
  centroid_bootstrap_summary$Observed_distance
) {
  warning(
    "The percentile interval does not contain the observed estimate. ",
    "This is possible for a biased or asymmetric bootstrap ",
    "distribution and should be reported rather than silently altered."
  )
}

if (
  any(
    centroid_bootstrap_components$CI_low >
      centroid_bootstrap_components$CI_high
  )
) {
  stop(
    "At least one component confidence interval is invalid."
  )
}

if (
  any(
    centroid_bootstrap_components$
      Proportion_below_zero +
      centroid_bootstrap_components$
        Proportion_above_zero >
      1 + 1e-12
  )
) {
  stop(
    "The directional bootstrap proportions are invalid."
  )
}

# ------------------------------------------------------------
# 13. Save definitive corrected outputs
# ------------------------------------------------------------

bootstrap_distribution <- tibble::as_tibble(
  bootstrap_results
) %>%
  dplyr::mutate(
    Replicate = dplyr::row_number(),
    .before = 1
  )

bootstrap_object <- list(
  Objective = "Objective 2",
  Comparison_design = "Independent Control and Stress populations",
  Comparison = "Stress minus Control",
  Input_raw_population_file = raw_population_file,
  Input_corrected_population_file = corrected_population_file,
  Input_observed_displacement_file = observed_displacement_file,
  Coordinate_system = "Control-referenced robust behavioural space",
  Normalisation_formula = paste0(
    "(x - median_control_Author) / ",
    "(1.4826 * MAD_raw_control_Author)"
  ),
  Bootstrap_scheme = paste0(
    "Independent resampling within Author x Treatment strata; ",
    "the Control reference is re-estimated separately within each ",
    "Author in every replicate; attempts with a zero or non-finite ",
    "Author-specific Control MAD are recorded and redrawn"
  ),
  Degenerate_resample_policy = paste0(
    "Conditional redraw without scale regularisation; every invalid ",
    "attempt is counted and excluded before the next complete ",
    "Author x Treatment-stratified attempt"
  ),
  Interval_method = paste0(
    100 * confidence_level,
    "% conditional percentile bootstrap interval"
  ),
  Behavioural_variables = behavioural_variables,
  Robust_variables = robust_variables,
  Authors = expected_authors,
  Group_counts = raw_group_counts,
  Author_treatment_counts = raw_cohort_group_counts,
  Full_sample_author_workflow_reproduced = TRUE,
  Observed_distance = observed_distance_direct,
  Observed_displacement_vector = observed_displacement_vector,
  Bootstrap_summary = centroid_bootstrap_summary,
  Component_summary = centroid_bootstrap_components,
  Bootstrap_distribution = bootstrap_distribution,
  B = B,
  Total_bootstrap_attempts = total_bootstrap_attempts,
  Invalid_bootstrap_attempts = invalid_bootstrap_attempts,
  Invalid_bootstrap_attempt_fraction =
    invalid_bootstrap_attempt_fraction,
  Seed = bootstrap_seed,
  Confidence_level = confidence_level
)

saveRDS(
  bootstrap_object,
  bootstrap_object_output
)

saveRDS(
  boot_distance,
  bootstrap_distance_output
)

utils::write.csv(
  centroid_bootstrap_summary,
  bootstrap_summary_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  centroid_bootstrap_components,
  bootstrap_components_output,
  row.names = FALSE,
  na = ""
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 14. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  bootstrap_object_output,
  bootstrap_distance_output,
  bootstrap_summary_output,
  bootstrap_components_output,
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
# 15. Identify obsolete output from the former workflow
# ------------------------------------------------------------

legacy_files <- c(
  file.path(
    objects_dir,
    "bootstrap_centroid_distance_4d.rds"
  ),
  file.path(
    project_root,
    "03_Results",
    "Statistics",
    "Centroid_Displacement_Bootstrap.csv"
  )
)

existing_legacy_files <- legacy_files[
  file.exists(legacy_files)
]

if (length(existing_legacy_files) > 0L) {
  warning(
    "Legacy bootstrap outputs from the former mean/SD workflow ",
    "were detected. They were not used or overwritten:\n",
    paste(existing_legacy_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 16. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - BOOTSTRAP OF 4D CENTROID DISPLACEMENT\n")
cat("============================================================\n")

cat(
  "\nIndependent populations:\n"
)

print(
  raw_group_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nPopulation sizes preserved within every Author x Treatment stratum:\n"
)

print(
  raw_cohort_group_counts,
  n = Inf,
  width = Inf
)

cat(
  "\nObserved displacement reproduced from Script 012:\n"
)

cat(
  "Euclidean distance in 4D = ",
  format(
    observed_distance_direct,
    digits = 10
  ),
  "\n",
  sep = ""
)

cat(
  "\nBootstrap design:\n",
  "Valid replicates (B) = ",
  B,
  "\nTotal resampling attempts = ",
  total_bootstrap_attempts,
  "\nRejected attempts with zero/non-finite Author-specific ",
  "Control MAD = ",
  invalid_bootstrap_attempts,
  "\nRejected-attempt fraction = ",
  format(
    invalid_bootstrap_attempt_fraction,
    digits = 6
  ),
  "\nSeed = ",
  bootstrap_seed,
  "\nIndependent resampling within Author x Treatment strata.\n",
  "Control-reference median and MAD re-estimated separately for ",
  "each Author in every replicate.\n",
  "The same replicate-specific Author reference is applied to that ",
  "Author's Control and Stress fish.\n",
  "Invalid attempts are recorded and completely redrawn; no zero MAD ",
  "is regularised or replaced by an arbitrary value.\n",
  sep = ""
)

cat(
  "\nBootstrap summary for the total 4D displacement:\n"
)

print(
  centroid_bootstrap_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nBootstrap summary for the directional components:\n"
)

print(
  centroid_bootstrap_components,
  n = Inf,
  width = Inf
)

cat(
  "\nInterpretive constraint: the percentile bootstrap interval ",
  "is conditional on positive Author-specific resampled Control MADs ",
  "and quantifies sampling uncertainty around the non-negative distance. ",
  "It must not be treated as a null-hypothesis test merely because ",
  "its lower bound is greater than zero.\n",
  sep = ""
)

cat(
  "\nDefinitive corrected outputs saved as:\n",
  normalizePath(
    bootstrap_object_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    bootstrap_distance_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    bootstrap_summary_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    bootstrap_components_output,
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
