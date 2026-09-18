# ============================================================
# 024_Stress_Louvain_CORREGIDO.R
#
# Objective 3
# De novo Louvain community detection within the UCS-exposed
# population
#
# Scientific question:
#   What internal behavioural structure emerges within
#   the UCS-exposed population?
#
# Operational question:
#   What structure does the displaced UCS-exposed population adopt
#   internally within the Control-referenced behavioural space?
#
# Analytical role:
#   Identify a candidate internal community structure de novo using
#   only the 89 Stress fish. Control contributes only the robust
#   coordinate system inherited from Objectives 1 and 2.
#
# Required upstream scripts:
#   020_StressArchitecture_Setup_CORREGIDO_v3.R
#   023_Figure2_StressPCA_CORREGIDO_v2.R
#
# Definitive graph construction:
#   - full four-dimensional Control-referenced robust space;
#   - Euclidean distance;
#   - similarity = 1 / (1 + distance);
#   - weighted, undirected union k-nearest-neighbour graph;
#   - Louvain community detection;
#   - k evaluated from 5 to 80;
#   - fixed seed for reproducibility.
#
# Candidate-k selection:
#   The selected k is the medoid partition within the longest
#   consecutive k plateau showing the same number of communities
#   and high adjacent-partition agreement. The selected solution
#   remains a candidate until stability is assessed in Script 025.
#
# Community numbering:
#   Louvain labels are arbitrary. Communities are renumbered from
#   lower to higher mean Stress PC1 solely to obtain deterministic
#   labels. No resilient-like or susceptible-like labels are assigned
#   in this script.
# ============================================================

script_version <- "024_Stress_Louvain_CORREGIDO | 2026-08-07 | Author+ID and corrected upstream audit"
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
  "igraph",
  "cluster",
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
  library(igraph)
  library(cluster)
  library(openxlsx)
})

# ------------------------------------------------------------
# 3. Input and output paths
# ------------------------------------------------------------

processed_dir <- file.path(project_root, "01_Data", "Processed")
objects_dir <- file.path(project_root, "03_Results", "Objects")
tables_dir <- file.path(project_root, "03_Results", "Tables")

if (!dir.exists(processed_dir)) {
  stop("Processed-data directory not found:\n", processed_dir)
}

for (directory_i in c(objects_dir, tables_dir)) {
  if (!dir.exists(directory_i)) {
    dir.create(directory_i, recursive = TRUE)
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

pca_results_input <- file.path(
  objects_dir,
  "objective3_stress_pca_results.rds"
)

required_input_files <- c(
  stress_input,
  setup_input,
  pca_results_input
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "The following definitive Objective 3 inputs were not found:\n",
    paste(missing_input_files, collapse = "\n"),
    "\n\nRun 020_StressArchitecture_Setup_CORREGIDO_v3.R and ",
    "023_Figure2_StressPCA_CORREGIDO_v2.R first."
  )
}

stress_output <- file.path(
  processed_dir,
  "stress_louvain_clusters.rds"
)

graph_output <- file.path(
  objects_dir,
  "stress_knn_graph.rds"
)

partition_output <- file.path(
  objects_dir,
  "stress_louvain_partition.rds"
)

optimisation_results_output <- file.path(
  objects_dir,
  "objective3_louvain_k_optimisation_results.rds"
)

memberships_output <- file.path(
  objects_dir,
  "objective3_louvain_memberships_by_k.rds"
)

selected_configuration_output <- file.path(
  objects_dir,
  "objective3_selected_louvain_configuration.rds"
)

selected_membership_output <- file.path(
  objects_dir,
  "objective3_selected_louvain_membership.rds"
)

results_object_output <- file.path(
  objects_dir,
  "objective3_stress_louvain_results.rds"
)

workbook_output <- file.path(
  tables_dir,
  "Objective3_Stress_Louvain.xlsx"
)

optimisation_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_Louvain_kOptimisation.csv"
)

plateaus_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_Louvain_StablePlateaus.csv"
)

membership_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_Louvain_SelectedMembership.csv"
)

community_sizes_csv_output <- file.path(
  tables_dir,
  "Objective3_Stress_Louvain_CommunitySizes.csv"
)

session_info_output <- file.path(
  objects_dir,
  "objective3_stress_louvain_sessionInfo.txt"
)

# ------------------------------------------------------------
# 4. Load definitive upstream objects
# ------------------------------------------------------------

stress <- readRDS(stress_input)
setup_object <- readRDS(setup_input)
pca_results <- readRDS(pca_results_input)

if (!is.data.frame(stress)) {
  stop(basename(stress_input), " does not contain a data frame.")
}

if (!is.list(setup_object)) {
  stop(basename(setup_input), " does not contain the expected list.")
}

if (!is.list(pca_results)) {
  stop(basename(pca_results_input), " does not contain the expected list.")
}

# ------------------------------------------------------------
# 5. Define variables and analytical constants
# ------------------------------------------------------------

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

expected_stress_n <- 89L
expected_number_of_variables <- 4L
expected_number_of_authors <- 4L

expected_traceability <- "Author + ID"
expected_normalisation <-
  "Within Author: (x - median) / (1.4826 x raw MAD)"
mad_scale_constant <- 1.4826

k_min <- 5L
k_max_requested <- 80L
louvain_seed <- 1234L

minimum_community_size <- 1L
primary_ari_threshold <- 0.95
secondary_ari_threshold <- 0.90
minimum_plateau_length <- 3L

numeric_tolerance <- 1e-12
ari_tolerance <- 1e-12

normalisation_description <-
  "Within Author: (x - median_control) / (1.4826 * MAD_raw_control)"

distance_description <- "Euclidean"
similarity_description <- "1 / (1 + distance)"
graph_description <- "Weighted, undirected union kNN"

# ------------------------------------------------------------
# 6. Audit the definitive Stress dataset
# ------------------------------------------------------------

required_stress_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  behavioural_variables,
  robust_variables
)

missing_stress_columns <- setdiff(
  required_stress_columns,
  names(stress)
)

if (length(missing_stress_columns) > 0L) {
  stop(
    "The definitive Stress dataset is missing required columns:\n",
    paste(missing_stress_columns, collapse = ", ")
  )
}

stress <- stress %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
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
  !all(as.character(stress$Treatment) == "Stress")
) {
  stop(
    "The Objective 3 Stress dataset fails the Author + ID / ",
    "Stress-only population audit."
  )
}

population_authors <- sort(
  unique(stress$Author)
)

if (
  length(population_authors) != expected_number_of_authors
) {
  stop(
    "The Stress dataset does not retain the expected four Authors."
  )
}

legacy_variables <- c(
  "distance_z",
  "crossing_z",
  "top_entry_z",
  "top_time_z"
)

legacy_variables_present <- intersect(
  legacy_variables,
  names(stress)
)

if (length(legacy_variables_present) > 0L) {
  stop(
    "Legacy mean/SD-standardised variables were detected:\n",
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

storage.mode(analytical_matrix) <- "double"

if (any(!is.finite(analytical_matrix))) {
  stop(
    "Missing or non-finite values were detected in the raw or ",
    "robust Stress behavioural variables."
  )
}

stress_matrix <- as.matrix(
  stress[
    robust_variables
  ]
)

storage.mode(stress_matrix) <- "double"
rownames(stress_matrix) <- stress$Traceability_ID
colnames(stress_matrix) <- robust_variables

if (
  !identical(
    dim(stress_matrix),
    c(
      expected_stress_n,
      expected_number_of_variables
    )
  )
) {
  stop(
    "The Stress behavioural matrix must have dimensions 89 x 4."
  )
}

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
  stress_matrix_rank != expected_number_of_variables
) {
  stop(
    "The centred Stress behavioural matrix does not have full rank."
  )
}

number_duplicated_profiles <- sum(
  duplicated(
    as.data.frame(stress_matrix)
  )
)

if (number_duplicated_profiles != 0L) {
  stop(
    "Exact duplicated four-variable Stress profiles were detected. ",
    "The definitive setup reported none."
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
  "Clustering_scope",
  "Reference_space",
  "Normalisation",
  "Normalisation_formula",
  "Behavioural_variables",
  "Robust_variables",
  "Normalisation_parameters",
  "Author_group_variable_normalisation_validation",
  "Stress_matrix_rank",
  "Duplicated_robust_profiles",
  "Downstream_constraint"
)

missing_setup_elements <- setdiff(
  required_setup_elements,
  names(setup_object)
)

if (length(missing_setup_elements) > 0L) {
  stop(
    "The corrected Objective 3 setup object is missing required elements:\n",
    paste(missing_setup_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(setup_object[["Analytical_population"]]),
    "Stress only"
  )
) {
  stop(
    "The setup object does not define Stress as the sole population."
  )
}

if (
  as.integer(setup_object[["Stress_n"]]) != expected_stress_n
) {
  stop(
    "The setup object does not contain the expected Stress n = 89."
  )
}

if (
  !identical(
    as.character(setup_object[["Traceability"]]),
    expected_traceability
  )
) {
  stop(
    "The setup object does not preserve Author + ID traceability."
  )
}

if (
  !identical(
    sort(as.character(setup_object[["Authors"]])),
    population_authors
  )
) {
  stop(
    "The Authors in the Stress dataset do not match corrected Script 020."
  )
}

if (
  !identical(
    as.character(setup_object[["Behavioural_variables"]]),
    behavioural_variables
  ) ||
  !identical(
    as.character(setup_object[["Robust_variables"]]),
    robust_variables
  )
) {
  stop(
    "The behavioural variables differ from the corrected Objective 3 setup."
  )
}

if (
  !identical(
    as.character(setup_object[["Normalisation"]]),
    expected_normalisation
  )
) {
  stop(
    "The inherited normalisation does not match the corrected ",
    "Author-specific robust coordinate system."
  )
}

if (
  as.integer(setup_object[["Stress_matrix_rank"]]) !=
    stress_matrix_rank
) {
  stop(
    "The centred Stress matrix rank differs from corrected Script 020."
  )
}

if (
  as.integer(setup_object[["Duplicated_robust_profiles"]]) != 0L
) {
  stop(
    "The corrected setup reports duplicated Stress profiles."
  )
}

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
    Author = trimws(as.character(Author)),
    Variable = as.character(Variable)
  ) %>%
  dplyr::arrange(
    Author,
    match(Variable, behavioural_variables)
  )

expected_parameter_rows <-
  expected_number_of_authors *
  length(behavioural_variables)

if (
  nrow(normalisation_parameters) != expected_parameter_rows ||
  any(
    duplicated(
      normalisation_parameters[
        c("Author", "Variable")
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

storage.mode(parameter_matrix) <- "double"

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
    Author = trimws(as.character(Author)),
    Group = as.character(Group),
    Variable = as.character(Variable),
    Robust_variable = as.character(Robust_variable)
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
        c("Author", "Group", "Variable")
      ]
    )
  ) ||
  !setequal(
    normalisation_validation$Author,
    population_authors
  ) ||
  !setequal(
    normalisation_validation$Group,
    c("Control", "Stress")
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
  !is.numeric(recalculation_differences) ||
  any(!is.finite(recalculation_differences)) ||
  any(recalculation_differences > numeric_tolerance)
) {
  stop(
    "The corrected setup does not confirm exact Author-specific ",
    "reproduction of the inherited robust coordinates."
  )
}

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
    match(Variable, behavioural_variables)
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
# 8. Audit dependence on corrected Script 023
# ------------------------------------------------------------

required_pca_elements <- c(
  "Analytical_population",
  "Stress_n",
  "Authors",
  "Traceability",
  "Robust_variables",
  "PCA_scope",
  "PCA_center",
  "PCA_scale",
  "PCA_loadings",
  "PCA_scores",
  "Stress_matrix_rank"
)

missing_pca_elements <- setdiff(
  required_pca_elements,
  names(pca_results)
)

if (length(missing_pca_elements) > 0L) {
  stop(
    "The corrected Objective 3 PCA object is missing required elements:\n",
    paste(missing_pca_elements, collapse = ", ")
  )
}

if (
  !identical(
    as.character(pca_results[["Analytical_population"]]),
    "Stress only"
  ) ||
  as.integer(pca_results[["Stress_n"]]) != expected_stress_n
) {
  stop(
    "The PCA object does not describe the definitive Stress population."
  )
}

if (
  !identical(
    as.character(pca_results[["Traceability"]]),
    expected_traceability
  ) ||
  !identical(
    sort(as.character(pca_results[["Authors"]])),
    population_authors
  )
) {
  stop(
    "The corrected PCA object does not preserve the definitive ",
    "Author + ID provenance."
  )
}

if (
  !identical(
    as.character(pca_results[["Robust_variables"]]),
    robust_variables
  )
) {
  stop(
    "The PCA variables differ from the definitive robust variables."
  )
}

if (
  !isTRUE(pca_results[["PCA_center"]]) ||
  !identical(pca_results[["PCA_scale"]], FALSE)
) {
  stop(
    "The PCA object must be centred within Stress and must not apply ",
    "additional SD scaling."
  )
}

if (
  as.integer(pca_results[["Stress_matrix_rank"]]) !=
    stress_matrix_rank
) {