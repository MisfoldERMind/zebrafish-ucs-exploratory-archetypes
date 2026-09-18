# ============================================================
# 028_Swaminathan_Classification_CORREGIDO_v4.R
#
# Objective 4
# Swaminathan-like dynamic thresholding
# Multivariate behavioural index based on PC1-PC4
#
# PURPOSE
# Apply the previously defined Swaminathan-like operational
# classification to the corrected LAPCOM dataset.
#
# DEFINITIVE ANALYTICAL FOUNDATION
# - Control n = 89; Stress n = 89.
# - Traceability = Author + ID.
# - Behavioural coordinates are inherited unchanged from the
#   corrected Objectives 1–3:
#
#     distance_rz
#     crossing_rz
#     top_entry_rz
#     top_time_rz
#
# - These coordinates were robustly normalised within Author
#   using the corresponding Control median and scaled raw MAD:
#
#     rz = (x - median_Control_within_Author) /
#          (1.4826 * MAD_raw_Control_within_Author)
#
# - No mean/SD z-score is calculated here.
# - No global normalisation is calculated here.
# - No Stress-specific normalisation is calculated here.
# - The PCA is NOT refitted. The definitive Control-derived PCA
#   from Objective 1 is inherited unchanged.
#
# METHOD PRESERVED FROM THE PREVIOUS OBJECTIVE 4 SCRIPT
# - Construct a directional Behavioural_Index from PC1-PC4.
# - Direction is defined from the Stress centroid toward the
#   Control centroid in the inherited PCA space.
# - Candidate thresholds are evaluated using Cohen's d within
#   Stress and Fisher's exact test on Control/Stress occupancy
#   above and below each threshold.
# - Cohen's d magnitude and Fisher evidence are min-max
#   normalised and summed.
# - The highest Combined_score defines the operational threshold.
# - Stress fish above the threshold are classified Resilient;
#   Stress fish at/below the threshold are Susceptible.
#
# IMPORTANT
# This script corrects the analytical dependencies and traceability
# of the previous script. It does not introduce a new classifier.
# ============================================================

script_version <- paste(
  "028_Swaminathan_Classification_CORREGIDO",
  "| 2026-08-09",
  "| corrected robust-coordinate dependencies"
)

cat("\nRunning: ", script_version, "\n", sep = "")

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
  stop("Project directory not found:\n", project_root)
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

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(openxlsx)
})

# ------------------------------------------------------------
# 3. Fixed analytical configuration
# ------------------------------------------------------------

set.seed(1234)

expected_control_n <- 89L
expected_stress_n <- 89L
expected_total_n <- 178L
expected_number_of_authors <- 4L

required_treatments <- c(
  "Control",
  "Stress"
)

expected_traceability <- "Author + ID"

expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"

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

pc_variables <- paste0(
  "PC",
  1:4
)

legacy_variables <- c(
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z",
  "distance_author_z",
  "crossing_author_z",
  "top_entry_author_z",
  "top_time_author_z"
)

numeric_tolerance <- 1e-10
projection_tolerance <- 1e-8

minimum_stress_group_size <- 2L

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

stress_objective3_file <- file.path(
  processed_dir,
  "objective3_stress_robust_z_control_reference.rds"
)

pca_model_file <- file.path(
  objects_dir,
  "objective1_pca_model_robust.rds"
)

pca_scores_file <- file.path(
  objects_dir,
  "objective1_pca_scores_robust.rds"
)

pca_variance_file <- file.path(
  objects_dir,
  "objective1_pca_variance_robust.rds"
)

required_input_files <- c(
  population_file,
  stress_objective3_file,
  pca_model_file,
  pca_scores_file,
  pca_variance_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive corrected inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\nRun the corrected Objectives 1–3 workflow first."
  )
}

classification_object_output <- file.path(
  objects_dir,
  "objective4_swaminathan_classification.rds"
)

threshold_candidates_output <- file.path(
  objects_dir,
  "objective4_swaminathan_threshold_candidates.rds"
)

optimal_threshold_output <- file.path(
  objects_dir,
  "objective4_swaminathan_optimal_threshold.rds"
)

pca_model_output <- file.path(
  objects_dir,
  "objective4_swaminathan_pca_model.rds"
)

index_weights_output <- file.path(
  objects_dir,
  "objective4_swaminathan_index_weights.rds"
)

parameters_output <- file.path(
  objects_dir,
  "objective4_swaminathan_parameters.rds"
)

xlsx_output <- file.path(
  tables_dir,
  "Objective4_Swaminathan_Classification.xlsx"
)

pdf_output <- file.path(
  figures_dir,
  "Objective4_Swaminathan_Threshold.pdf"
)

tiff_output <- file.path(
  figures_dir,
  "Objective4_Swaminathan_Threshold.tiff"
)

session_info_output <- file.path(
  objects_dir,
  "objective4_swaminathan_sessionInfo.txt"
)

# ------------------------------------------------------------
# 5. Load definitive corrected inputs
# ------------------------------------------------------------

population <- readRDS(
  population_file
)

stress_objective3 <- readRDS(
  stress_objective3_file
)

pca_control <- readRDS(
  pca_model_file
)

objective1_pca_scores <- readRDS(
  pca_scores_file
)

objective1_pca_variance <- readRDS(
  pca_variance_file
)

if (!is.data.frame(population)) {
  stop(
    basename(population_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(stress_objective3)) {
  stop(
    basename(stress_objective3_file),
    " does not contain a data frame."
  )
}

if (!inherits(pca_control, "prcomp")) {
  stop(
    basename(pca_model_file),
    " does not contain the definitive prcomp object."
  )
}

if (!is.data.frame(objective1_pca_scores)) {
  stop(
    basename(pca_scores_file),
    " does not contain a data frame."
  )
}

if (!is.data.frame(objective1_pca_variance)) {
  stop(
    basename(pca_variance_file),
    " does not contain a data frame."
  )
}

# ------------------------------------------------------------
# 6. Audit corrected population and traceability
# ------------------------------------------------------------

required_population_columns <- c(
  "Author",
  "ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_population_columns <- setdiff(
  required_population_columns,
  names(population)
)

if (length(missing_population_columns) > 0L) {
  stop(
    "The corrected Control/Stress population is missing:\n",
    paste(missing_population_columns, collapse = ", ")
  )
}

required_stress_columns <- c(
  "Author",
  "ID",
  "Treatment",
  robust_variables
)

missing_stress_columns <- setdiff(
  required_stress_columns,
  names(stress_objective3)
)

if (length(missing_stress_columns) > 0L) {
  stop(
    "The definitive Objective 3 Stress object is missing:\n",
    paste(missing_stress_columns, collapse = ", ")
  )
}

make_traceability_key <- function(author, id) {
  paste(
    trimws(as.character(author)),
    trimws(as.character(id)),
    sep = " || "
  )
}

population <- population %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = factor(
      as.character(Treatment),
      levels = required_treatments
    ),
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

stress_objective3 <- stress_objective3 %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Treatment = as.character(Treatment),
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

if (
  nrow(population) != expected_total_n ||
  sum(population$Treatment == "Control") != expected_control_n ||
  sum(population$Treatment == "Stress") != expected_stress_n
) {
  stop(
    "The corrected population must contain 89 Control and 89 Stress fish."
  )
}

if (
  nrow(stress_objective3) != expected_stress_n ||
  !all(stress_objective3$Treatment == "Stress")
) {
  stop(
    "The definitive Objective 3 object must contain exactly 89 Stress fish."
  )
}

for (data_name_i in c(
  "population",
  "stress_objective3"
)) {

  data_i <- get(data_name_i)

  if (
    anyNA(data_i$Author) ||
    anyNA(data_i$ID) ||
    anyNA(data_i$Traceability_ID) ||
    any(data_i$Author == "") ||
    any(data_i$ID == "") ||
    any(data_i$Traceability_ID == "")
  ) {
    stop(
      "Missing or empty Author + ID traceability values in ",
      data_name_i,
      "."
    )
  }

  if (
    anyDuplicated(
      data_i[c("Author", "ID")]
    ) > 0L ||
    anyDuplicated(
      data_i$Traceability_ID
    ) > 0L
  ) {
    stop(
      "Duplicated Author + ID traceability keys in ",
      data_name_i,
      "."
    )
  }
}

author_levels <- sort(
  unique(
    population$Author
  )
)

if (
  length(author_levels) != expected_number_of_authors ||
  !identical(
    author_levels,
    c("1", "2", "3", "4")
  )
) {
  stop(
    "The corrected population must contain Authors 1, 2, 3, and 4."
  )
}

author_treatment_counts <- population %>%
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

if (
  nrow(author_treatment_counts) !=
    expected_number_of_authors *
      length(required_treatments) ||
  sum(author_treatment_counts$n) != expected_total_n
) {
  stop(
    "The Author-by-Treatment count audit failed."
  )
}

# ------------------------------------------------------------
# 7. Verify exact continuity with the definitive Objective 3 Stress data
# ------------------------------------------------------------

population_stress <- population %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

stress_objective3 <- stress_objective3 %>%
  dplyr::arrange(
    Traceability_ID
  )

if (
  !identical(
    population_stress$Traceability_ID,
    stress_objective3$Traceability_ID
  )
) {
  stop(
    "Stress Author + ID keys in the corrected population do not match ",
    "the definitive Objective 3 cohort."
  )
}

stress_provenance_difference <- vapply(
  robust_variables,
  function(variable_i) {
    max(
      abs(
        population_stress[[variable_i]] -
          stress_objective3[[variable_i]]
      )
    )
  },
  numeric(1)
)

if (
  any(
    !is.finite(
      stress_provenance_difference
    )
  ) ||
  any(
    stress_provenance_difference >
      numeric_tolerance
  )
) {
  stop(
    "The corrected population does not reproduce the definitive ",
    "Objective 3 Stress robust coordinates."
  )
}

# ------------------------------------------------------------
# 8. Audit the inherited robust behavioural coordinates
# ------------------------------------------------------------

non_numeric_robust_variables <- robust_variables[
  !vapply(
    population[robust_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_robust_variables) > 0L) {
  stop(
    "The following definitive robust variables are not numeric:\n",
    paste(non_numeric_robust_variables, collapse = ", ")
  )
}

robust_matrix <- as.matrix(
  population[
    robust_variables
  ]
)

storage.mode(robust_matrix) <- "double"

if (any(!is.finite(robust_matrix))) {
  stop(
    "Missing or non-finite values detected in the definitive robust variables."
  )
}

control_reference_audit <- dplyr::bind_rows(
  lapply(
    author_levels,
    function(author_i) {

      control_i <- population[
        population$Author == author_i &
          population$Treatment == "Control",
        robust_variables,
        drop = FALSE
      ]

      control_matrix_i <- as.matrix(
        control_i
      )

      storage.mode(control_matrix_i) <- "double"

      tibble::tibble(
        Author = author_i,
        Robust_variable = robust_variables,
        Control_median = apply(
          control_matrix_i,
          2,
          stats::median
        ),
        Control_scaled_MAD = apply(
          control_matrix_i,
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
    expected_number_of_authors *
      length(robust_variables) ||
  any(
    abs(
      control_reference_audit$Control_median
    ) > numeric_tolerance
  ) ||
  any(
    abs(
      control_reference_audit$Control_scaled_MAD -
        1
    ) > numeric_tolerance
  )
) {
  stop(
    "The inherited Author-specific Control robust-reference audit failed."
  )
}

legacy_variables_present <- intersect(
  legacy_variables,
  names(population)
)

# They may exist in an upstream object, but are never used here.
# Their presence is reported only for transparency.

# ------------------------------------------------------------
# 9. Audit and apply the definitive Objective 1 PCA
# ------------------------------------------------------------

required_score_columns <- c(
  "Author",
  "ID",
  pc_variables
)

missing_score_columns <- setdiff(
  required_score_columns,
  names(objective1_pca_scores)
)

if (length(missing_score_columns) > 0L) {
  stop(
    "The definitive Objective 1 PCA-score object is missing:\n",
    paste(missing_score_columns, collapse = ", ")
  )
}

required_variance_columns <- c(
  "Component",
  "Percentage_of_variance"
)

missing_variance_columns <- setdiff(
  required_variance_columns,
  names(objective1_pca_variance)
)

if (length(missing_variance_columns) > 0L) {
  stop(
    "The definitive Objective 1 PCA-variance object is missing:\n",
    paste(missing_variance_columns, collapse = ", ")
  )
}

objective1_pca_scores <- objective1_pca_scores %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = make_traceability_key(
      Author,
      ID
    )
  )

if (
  nrow(objective1_pca_scores) != expected_control_n ||
  anyDuplicated(
    objective1_pca_scores$Traceability_ID
  ) > 0L
) {
  stop(
    "The definitive Objective 1 PCA-score object failed its traceability audit."
  )
}

control_population <- population %>%
  dplyr::filter(
    Treatment == "Control"
  )

if (
  !setequal(
    control_population$Traceability_ID,
    objective1_pca_scores$Traceability_ID
  )
) {
  stop(
    "The corrected Control population and Objective 1 PCA-score keys differ."
  )
}

# The definitive Objective 1 PCA was fitted using the robust
# values, but the PCA input columns were deliberately renamed
# to the original behavioural variable names before prcomp().
# Therefore predict.prcomp() requires these original names.
# Values remain exactly the inherited robust coordinates.

pca_projection_input <- population[
  robust_variables
]

names(pca_projection_input) <- behavioural_variables

# In a prcomp object, the original input-variable names are stored
# in the ROW names of the rotation matrix; the COLUMN names are the
# principal components (PC1-PC4).

expected_pca_input_names <- rownames(
  pca_control$rotation
)

if (
  is.null(expected_pca_input_names) ||
  !identical(
    names(pca_projection_input),
    expected_pca_input_names
  )
) {
  stop(
    "The inherited Objective 1 PCA expects input variables: ",
    paste(expected_pca_input_names, collapse = ", "),
    "; projection input contains: ",
    paste(names(pca_projection_input), collapse = ", "),
    "."
  )
}

all_scores_matrix <- stats::predict(
  pca_control,
  newdata = pca_projection_input
)

if (
  !is.matrix(all_scores_matrix) ||
  nrow(all_scores_matrix) != expected_total_n ||
  ncol(all_scores_matrix) < length(pc_variables)
) {
  stop(
    "The inherited Objective 1 PCA did not return the expected PC1-PC4 scores."
  )
}

scores_all <- as.data.frame(
  all_scores_matrix[
    ,
    pc_variables,
    drop = FALSE
  ]
)

if (
  any(
    !is.finite(
      as.matrix(
        scores_all
      )
    )
  )
) {
  stop(
    "Non-finite PC scores were obtained from the inherited Objective 1 PCA."
  )
}

score_data <- dplyr::bind_cols(
  population,
  scores_all
)

predicted_control_scores <- score_data %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::select(
    Traceability_ID,
    dplyr::all_of(pc_variables)
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

stored_control_scores <- objective1_pca_scores %>%
  dplyr::select(
    Traceability_ID,
    dplyr::all_of(pc_variables)
  ) %>%
  dplyr::arrange(
    Traceability_ID
  )

if (
  !identical(
    predicted_control_scores$Traceability_ID,
    stored_control_scores$Traceability_ID
  )
) {
  stop(
    "The inherited PCA score audit could not be aligned by Author + ID."
  )
}

pca_projection_difference <- max(
  abs(
    as.matrix(
      predicted_control_scores[
        pc_variables
      ]
    ) -
      as.matrix(
        stored_control_scores[
          pc_variables
        ]
      )
  )
)

if (
  !is.finite(
    pca_projection_difference
  ) ||
  pca_projection_difference >
    projection_tolerance
) {
  stop(
    "Projection through the inherited Objective 1 PCA does not reproduce ",
    "the stored Control PC1-PC4 scores."
  )
}

variance_explained <- objective1_pca_variance %>%
  dplyr::filter(
    Component %in% pc_variables
  ) %>%
  dplyr::arrange(
    match(
      Component,
      pc_variables
    )
  )

if (
  nrow(variance_explained) != length(pc_variables) ||
  !identical(
    as.character(
      variance_explained$Component
    ),
    pc_variables
  )
) {
  stop(
    "The Objective 1 PCA variance table does not contain PC1-PC4 correctly."
  )
}

# ------------------------------------------------------------
# 10. Construct the directional Behavioural Index
# ------------------------------------------------------------

control_pc_centroid <- score_data %>%
  dplyr::filter(
    Treatment == "Control"
  ) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(pc_variables),
      mean
    )
  ) %>%
  unlist(
    use.names = FALSE
  )

stress_pc_centroid <- score_data %>%
  dplyr::filter(
    Treatment == "Stress"
  ) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(pc_variables),
      mean
    )
  ) %>%
  unlist(
    use.names = FALSE
  )

index_weights <- (
  control_pc_centroid -
    stress_pc_centroid
)

index_weight_norm <- sqrt(
  sum(
    index_weights^2
  )
)

if (
  length(index_weight_norm) != 1L ||
  !is.finite(index_weight_norm) ||
  index_weight_norm <= 0
) {
  stop(
    "The Control-to-Stress centroid direction cannot define a valid index."
  )
}

index_weights <- index_weights /
  index_weight_norm

names(index_weights) <- pc_variables

score_data$Behavioural_Index <- as.numeric(
  as.matrix(
    score_data[
      pc_variables
    ]
  ) %*%
    index_weights
)

if (
  any(
    !is.finite(
      score_data$Behavioural_Index
    )