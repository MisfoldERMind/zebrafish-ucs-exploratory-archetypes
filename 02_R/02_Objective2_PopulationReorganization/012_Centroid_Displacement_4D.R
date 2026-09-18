# ============================================================
# 012_Centroid_Displacement_4D.R
#
# Objective 2
# Observed displacement of the Stress population centroid
# relative to the Control population centroid in the definitive
# four-dimensional, control-referenced robust behavioural space
#
# Input produced by:
#   011_Project_Control_Stress.R
#
# Robust variables:
#   distance_rz
#   crossing_rz
#   top_entry_rz
#   top_time_rz
#
# Direction convention:
#   displacement = Stress centroid - Control centroid
#
# The population centroid is defined as the arithmetic mean
# vector, which is the Euclidean centroid of the observations.
# Statistical uncertainty is evaluated separately in Script 013.
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
# 3. Input and output paths
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

if (!file.exists(population_file)) {
  stop(
    "The definitive Objective 2 population object was not found:\n",
    population_file,
    "\nRun 011_Project_Control_Stress.R before this script."
  )
}

centroids_output <- file.path(
  objects_dir,
  "objective2_centroids_control_stress_4d_robust.rds"
)

displacement_output <- file.path(
  objects_dir,
  "objective2_centroid_displacement_4d_robust.rds"
)

centroids_csv_output <- file.path(
  tables_dir,
  "Objective2_Centroids_Control_Stress_4D_RobustZ.csv"
)

components_csv_output <- file.path(
  tables_dir,
  "Objective2_Centroid_Displacement_Components_4D_RobustZ.csv"
)

summary_csv_output <- file.path(
  tables_dir,
  "Objective2_Centroid_Displacement_Summary_4D_RobustZ.csv"
)

session_info_output <- file.path(
  objects_dir,
  "objective2_centroid_displacement_4d_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Load and validate the corrected population object
# ------------------------------------------------------------

population <- readRDS(population_file)

if (!is.data.frame(population)) {
  stop(
    basename(population_file),
    " does not contain a data frame."
  )
}

treatment_variable <- "Treatment"

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

required_columns <- c(
  treatment_variable,
  robust_variables
)

missing_columns <- setdiff(
  required_columns,
  names(population)
)

if (length(missing_columns) > 0L) {
  stop(
    "The corrected population object is missing the following columns:\n",
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

robust_matrix <- as.matrix(
  population[robust_variables]
)

storage.mode(robust_matrix) <- "double"

if (any(!is.finite(robust_matrix))) {
  stop(
    "Missing or non-finite values were detected in the robust ",
    "behavioural variables."
  )
}

treatment_values <- unique(
  as.character(
    population[[treatment_variable]]
  )
)

required_treatments <- c(
  "Control",
  "Stress"
)

missing_treatments <- setdiff(
  required_treatments,
  treatment_values
)

if (length(missing_treatments) > 0L) {
  stop(
    "The following required treatment labels were not found:\n",
    paste(missing_treatments, collapse = ", ")
  )
}

unexpected_treatments <- setdiff(
  treatment_values,
  required_treatments
)

if (length(unexpected_treatments) > 0L) {
  stop(
    "Unexpected treatment levels were detected in the definitive ",
    "Objective 2 population object:\n",
    paste(unexpected_treatments, collapse = ", ")
  )
}

population <- population %>%
  dplyr::mutate(
    Treatment = factor(
      as.character(.data[[treatment_variable]]),
      levels = required_treatments
    )
  )

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

# ------------------------------------------------------------
# 5. Calculate the arithmetic centroid of each population
# ------------------------------------------------------------

centroids_wide <- population %>%
  dplyr::group_by(Treatment) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(robust_variables),
      mean
    ),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Treatment)

if (
  nrow(centroids_wide) != 2L ||
  !identical(
    as.character(centroids_wide$Treatment),
    required_treatments
  )
) {
  stop(
    "The centroid table does not contain exactly one ordered row ",
    "for Control and one for Stress."
  )
}

centroid_numeric_matrix <- as.matrix(
  centroids_wide[robust_variables]
)

storage.mode(centroid_numeric_matrix) <- "double"

if (any(!is.finite(centroid_numeric_matrix))) {
  stop(
    "One or more centroid coordinates are non-finite."
  )
}

control_centroid <- unname(
  centroid_numeric_matrix[
    centroids_wide$Treatment == "Control",
    ,
    drop = TRUE
  ]
)

stress_centroid <- unname(
  centroid_numeric_matrix[
    centroids_wide$Treatment == "Stress",
    ,
    drop = TRUE
  ]
)

names(control_centroid) <- robust_variables
names(stress_centroid) <- robust_variables

if (
  length(control_centroid) != length(robust_variables) ||
  length(stress_centroid) != length(robust_variables)
) {
  stop(
    "The centroid vectors do not have four coordinates."
  )
}

# ------------------------------------------------------------
# 6. Calculate the observed 4D displacement
# ------------------------------------------------------------

displacement_vector <- stress_centroid - control_centroid

squared_components <- displacement_vector^2

squared_distance_4d <- sum(
  squared_components
)

centroid_distance_4d <- sqrt(
  squared_distance_4d
)

if (
  !is.finite(centroid_distance_4d) ||
  centroid_distance_4d < 0
) {
  stop(
    "The 4D Euclidean centroid displacement is invalid."
  )
}

if (centroid_distance_4d > 0) {
  unit_direction_vector <- (
    displacement_vector /
      centroid_distance_4d
  )

  contribution_percent <- (
    squared_components /
      squared_distance_4d
  ) * 100
} else {
  unit_direction_vector <- rep(
    NA_real_,
    length(robust_variables)
  )

  contribution_percent <- rep(
    NA_real_,
    length(robust_variables)
  )
}

names(unit_direction_vector) <- robust_variables
names(contribution_percent) <- robust_variables

# ------------------------------------------------------------
# 7. Construct definitive result tables
# ------------------------------------------------------------

variable_labels <- c(
  distance = "Distance travelled",
  crossing = "Line crossings",
  top_entry = "Upper-zone entries",
  top_time = "Upper-zone time"
)

centroids_long <- centroids_wide %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(robust_variables),
    names_to = "Robust_variable",
    values_to = "Centroid_coordinate"
  ) %>%
  dplyr::mutate(
    Variable = sub(
      "_rz$",
      "",
      Robust_variable
    ),
    Behaviour = unname(
      variable_labels[Variable]
    )
  ) %>%
  dplyr::select(
    Treatment,
    Variable,
    Behaviour,
    Robust_variable,
    Centroid_coordinate
  )

displacement_components <- tibble::tibble(
  Variable = behavioural_variables,
  Behaviour = unname(
    variable_labels[behavioural_variables]
  ),
  Robust_variable = robust_variables,
  Control_centroid = as.numeric(
    control_centroid[robust_variables]
  ),
  Stress_centroid = as.numeric(
    stress_centroid[robust_variables]
  ),
  Displacement_Stress_minus_Control = as.numeric(
    displacement_vector[robust_variables]
  ),
  Absolute_displacement = abs(
    as.numeric(
      displacement_vector[robust_variables]
    )
  ),
  Squared_component = as.numeric(
    squared_components[robust_variables]
  ),
  Contribution_percent_to_squared_distance = as.numeric(
    contribution_percent[robust_variables]
  ),
  Unit_direction_coordinate = as.numeric(
    unit_direction_vector[robust_variables]
  )
)

displacement_summary <- tibble::tibble(
  Comparison = "Stress minus Control",
  Space = "Four-dimensional control-referenced robust behavioural space",
  Dimensions = length(robust_variables),
  Control_n = group_counts$n[
    group_counts$Treatment == "Control"
  ],
  Stress_n = group_counts$n[
    group_counts$Treatment == "Stress"
  ],
  Squared_Euclidean_displacement = squared_distance_4d,
  Euclidean_centroid_displacement_4D = centroid_distance_4d
)

# ------------------------------------------------------------
# 8. Internal numerical checks
# ------------------------------------------------------------

reconstructed_distance <- sqrt(
  sum(
    displacement_components$Squared_component
  )
)

if (
  !isTRUE(
    all.equal(
      reconstructed_distance,
      centroid_distance_4d,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The Euclidean displacement could not be reconstructed from ",
    "its four squared components."
  )
}

if (centroid_distance_4d > 0) {
  contribution_sum <- sum(
    displacement_components$
      Contribution_percent_to_squared_distance
  )

  if (
    !isTRUE(
      all.equal(
        contribution_sum,
        100,
        tolerance = 1e-10
      )
    )
  ) {
    stop(
      "The variable contributions to squared displacement do not ",
      "sum to 100%."
    )
  }

  unit_vector_norm <- sqrt(
    sum(
      displacement_components$
        Unit_direction_coordinate^2
    )
  )

  if (
    !isTRUE(
      all.equal(
        unit_vector_norm,
        1,
        tolerance = 1e-12
      )
    )
  ) {
    stop(
      "The displacement direction vector does not have unit norm."
    )
  }
}

# Direct reconstruction from group means in the source data

control_means_direct <- colMeans(
  robust_matrix[
    population$Treatment == "Control",
    ,
    drop = FALSE
  ]
)

stress_means_direct <- colMeans(
  robust_matrix[
    population$Treatment == "Stress",
    ,
    drop = FALSE
  ]
)

direct_distance <- sqrt(
  sum(
    (
      stress_means_direct -
        control_means_direct
    )^2
  )
)

if (
  !isTRUE(
    all.equal(
      direct_distance,
      centroid_distance_4d,
      tolerance = 1e-12
    )
  )
) {
  stop(
    "The centroid displacement does not match the direct ",
    "calculation from the source matrix."
  )
}

# ------------------------------------------------------------
# 9. Save definitive corrected objects
# ------------------------------------------------------------

centroid_object <- list(
  Objective = "Objective 2",
  Comparison_design = "Independent Control and Stress populations",
  Input_file = population_file,
  Coordinate_system = "Control-referenced robust z-score space",
  Centroid_definition = "Arithmetic mean vector",
  Behavioural_variables = behavioural_variables,
  Robust_variables = robust_variables,
  Group_counts = group_counts,
  Centroids_wide = centroids_wide,
  Centroids_long = centroids_long
)

displacement_object <- list(
  Objective = "Objective 2",
  Comparison = "Stress minus Control",
  Distance_metric = "Euclidean distance",
  Dimensions = robust_variables,
  Control_centroid = control_centroid,
  Stress_centroid = stress_centroid,
  Displacement_vector = displacement_vector,
  Unit_direction_vector = unit_direction_vector,
  Squared_components = squared_components,
  Contribution_percent_to_squared_distance = contribution_percent,
  Squared_Euclidean_displacement = squared_distance_4d,
  Euclidean_centroid_displacement_4D = centroid_distance_4d,
  Components_table = displacement_components,
  Summary_table = displacement_summary
)

saveRDS(
  centroid_object,
  centroids_output
)

saveRDS(
  displacement_object,
  displacement_output
)

utils::write.csv(
  centroids_long,
  centroids_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  displacement_components,
  components_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  displacement_summary,
  summary_csv_output,
  row.names = FALSE,
  na = ""
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 10. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  centroids_output,
  displacement_output,
  centroids_csv_output,
  components_csv_output,
  summary_csv_output,
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
# 11. Identify obsolete output from the former mean/SD workflow
# ------------------------------------------------------------

legacy_output <- file.path(
  objects_dir,
  "centroids_control_stress_4d.rds"
)

if (file.exists(legacy_output)) {
  warning(
    "A legacy centroid object from the former mean/SD workflow ",
    "was detected and was not used or overwritten:\n",
    legacy_output
  )
}

# ------------------------------------------------------------
# 12. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 2 - OBSERVED CENTROID DISPLACEMENT IN 4D\n")
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
  "\nCentroid coordinates in the control-referenced robust space:\n"
)

print(
  centroids_wide,
  n = Inf,
  width = Inf
)

cat(
  "\nDisplacement components (Stress minus Control):\n"
)

print(
  displacement_components,
  n = Inf,
  width = Inf
)

cat(
  "\nObserved Euclidean centroid displacement in 4D: ",
  format(
    centroid_distance_4d,
    digits = 10
  ),
  "\n",
  sep = ""
)

cat(
  "Squared Euclidean displacement: ",
  format(
    squared_distance_4d,
    digits = 10
  ),
  "\n",
  sep = ""
)

cat(
  "\nThis script estimates the observed displacement only. ",
  "Bootstrap uncertainty and inferential evaluation are performed ",
  "in Script 013.\n",
  sep = ""
)

cat(
  "\nDefinitive corrected outputs saved as:\n",
  normalizePath(
    centroids_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    displacement_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    centroids_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    components_csv_output,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n",
  normalizePath(
    summary_csv_output,
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