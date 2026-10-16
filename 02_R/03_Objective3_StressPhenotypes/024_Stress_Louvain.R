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

project_root <- Sys.getenv("LAPCOM_UCS_PROJECT_ROOT", unset = getwd())

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
  stop(
    "The PCA object and corrected setup disagree on Stress matrix rank."
  )
}

pca_scores <- pca_results[["PCA_scores"]]
pca_loadings <- pca_results[["PCA_loadings"]]

if (!is.data.frame(pca_scores)) {
  stop(
    "PCA_scores is not a data frame."
  )
}

if (!is.data.frame(pca_loadings)) {
  stop(
    "PCA_loadings is not a data frame."
  )
}

required_pca_score_columns <- c(
  "Author",
  "ID",
  "Traceability_ID",
  "Treatment",
  "PC1",
  "PC2"
)

missing_pca_score_columns <- setdiff(
  required_pca_score_columns,
  names(pca_scores)
)

if (length(missing_pca_score_columns) > 0L) {
  stop(
    "The corrected PCA-score table is missing required columns:\n",
    paste(missing_pca_score_columns, collapse = ", ")
  )
}

required_pca_loading_columns <- c(
  "Variable",
  "PC1"
)

missing_pca_loading_columns <- setdiff(
  required_pca_loading_columns,
  names(pca_loadings)
)

if (length(missing_pca_loading_columns) > 0L) {
  stop(
    "The PCA-loading table is missing required columns:\n",
    paste(missing_pca_loading_columns, collapse = ", ")
  )
}

pca_scores <- pca_scores %>%
  dplyr::mutate(
    Author = trimws(as.character(Author)),
    ID = trimws(as.character(ID)),
    Traceability_ID = trimws(as.character(Traceability_ID)),
    Treatment = as.character(Treatment)
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
  anyDuplicated(pca_scores$Traceability_ID) > 0L ||
  !setequal(
    pca_scores$Traceability_ID,
    stress$Traceability_ID
  ) ||
  !all(pca_scores$Treatment == "Stress")
) {
  stop(
    "The PCA-score traceability keys do not match the definitive ",
    "Stress population."
  )
}

pca_scores <- pca_scores[
  match(
    stress$Traceability_ID,
    pca_scores$Traceability_ID
  ),
  ,
  drop = FALSE
]

if (
  !identical(
    pca_scores$Traceability_ID,
    stress$Traceability_ID
  ) ||
  !identical(
    pca_scores$Author,
    stress$Author
  ) ||
  !identical(
    pca_scores$ID,
    stress$ID
  )
) {
  stop(
    "The PCA scores could not be aligned exactly by Author + ID."
  )
}

if (
  any(!is.finite(pca_scores$PC1)) ||
  any(!is.finite(pca_scores$PC2))
) {
  stop(
    "The aligned PCA scores contain non-finite values."
  )
}

pc1_loadings <- pca_loadings$PC1[
  match(
    robust_variables,
    pca_loadings$Variable
  )
]

if (
  length(pc1_loadings) != length(robust_variables) ||
  anyNA(pc1_loadings) ||
  any(!is.finite(pc1_loadings))
) {
  stop(
    "PC1 loadings could not be aligned with the robust variables."
  )
}

if (!all(pc1_loadings > 0)) {
  stop(
    "PC1 is not oriented from lower to higher overall exploration. ",
    "Run the corrected Script 023 before this script."
  )
}

# ------------------------------------------------------------
# 9. Construct the Euclidean distance matrix
# ------------------------------------------------------------

distance_object <- stats::dist(
  stress_matrix,
  method = "euclidean",
  upper = FALSE,
  diag = FALSE
)

distance_matrix <- as.matrix(distance_object)
diag(distance_matrix) <- 0

if (
  !identical(
    dim(distance_matrix),
    c(expected_stress_n, expected_stress_n)
  )
) {
  stop("The Euclidean distance matrix must have dimensions 89 x 89.")
}

if (
  any(!is.finite(distance_matrix)) ||
  any(distance_matrix < 0)
) {
  stop("The Euclidean distance matrix contains invalid values.")
}

maximum_distance_asymmetry <- max(
  abs(distance_matrix - t(distance_matrix))
)

if (maximum_distance_asymmetry > numeric_tolerance) {
  stop("The Euclidean distance matrix is not symmetric.")
}

if (any(abs(diag(distance_matrix)) > numeric_tolerance)) {
  stop("The Euclidean distance matrix has a non-zero diagonal.")
}

positive_distances <- distance_matrix[
  upper.tri(distance_matrix)
]

if (any(positive_distances <= 0)) {
  stop(
    "At least one pair of Stress fish has zero Euclidean distance. ",
    "Exact duplicated profiles were not expected."
  )
}

# ------------------------------------------------------------
# 10. Define graph and cluster-quality functions
# ------------------------------------------------------------

adjusted_rand_index <- function(
  membership_1,
  membership_2
) {
  membership_1 <- as.character(membership_1)
  membership_2 <- as.character(membership_2)

  if (
    length(membership_1) != length(membership_2) ||
    length(membership_1) < 2L ||
    anyNA(membership_1) ||
    anyNA(membership_2)
  ) {
    stop("Invalid memberships supplied to adjusted_rand_index().")
  }

  contingency_table <- table(
    membership_1,
    membership_2
  )

  choose_two <- function(x) {
    x * (x - 1) / 2
  }

  observed_index <- sum(
    choose_two(contingency_table)
  )

  row_index <- sum(
    choose_two(rowSums(contingency_table))
  )

  column_index <- sum(
    choose_two(colSums(contingency_table))
  )

  total_pairs <- choose_two(
    length(membership_1)
  )

  expected_index <-
    row_index * column_index / total_pairs

  maximum_index <-
    0.5 * (row_index + column_index)

  denominator <- maximum_index - expected_index

  if (abs(denominator) <= .Machine$double.eps^0.5) {
    same_partition <- identical(
      outer(
        membership_1,
        membership_1,
        FUN = "=="
      ),
      outer(
        membership_2,
        membership_2,
        FUN = "=="
      )
    )

    return(
      if (same_partition) 1 else 0
    )
  }

  ari <- (
    observed_index - expected_index
  ) / denominator

  if (!is.finite(ari)) {
    stop("Adjusted Rand index is not finite.")
  }

  as.numeric(ari)
}

build_weighted_union_knn <- function(
  distance_matrix,
  ids,
  k_value
) {
  number_of_fish <- nrow(distance_matrix)

  if (
    ncol(distance_matrix) != number_of_fish ||
    length(ids) != number_of_fish
  ) {
    stop("Invalid inputs supplied to build_weighted_union_knn().")
  }

  if (
    length(k_value) != 1L ||
    is.na(k_value) ||
    k_value < 1L ||
    k_value >= number_of_fish
  ) {
    stop("k must be a single integer between 1 and n - 1.")
  }

  k_value <- as.integer(k_value)

  neighbour_indices <- lapply(
    seq_len(number_of_fish),
    function(i) {
      candidates <- seq_len(number_of_fish)
      candidates <- candidates[candidates != i]

      ordered_candidates <- candidates[
        order(
          distance_matrix[i, candidates],
          candidates
        )
      ]

      ordered_candidates[seq_len(k_value)]
    }
  )

  directed_from <- rep(
    seq_len(number_of_fish),
    each = k_value
  )

  directed_to <- unlist(
    neighbour_indices,
    use.names = FALSE
  )

  lower_index <- pmin(directed_from, directed_to)
  upper_index <- pmax(directed_from, directed_to)

  edge_key <- paste(
    lower_index,
    upper_index,
    sep = "__"
  )

  keep_edge <- !duplicated(edge_key)

  lower_index <- lower_index[keep_edge]
  upper_index <- upper_index[keep_edge]

  edge_distance <- distance_matrix[
    cbind(lower_index, upper_index)
  ]

  edge_weight <- 1 / (1 + edge_distance)

  edge_table <- data.frame(
    from = ids[lower_index],
    to = ids[upper_index],
    distance = as.numeric(edge_distance),
    weight = as.numeric(edge_weight),
    stringsAsFactors = FALSE
  )

  if (
    anyNA(edge_table) ||
    any(!is.finite(edge_table$distance)) ||
    any(!is.finite(edge_table$weight)) ||
    any(edge_table$distance <= 0) ||
    any(edge_table$weight <= 0)
  ) {
    stop("Invalid weighted kNN edges were generated.")
  }

  vertices <- data.frame(
    name = ids,
    Original_row = seq_len(number_of_fish),
    stringsAsFactors = FALSE
  )

  graph <- igraph::graph_from_data_frame(
    d = edge_table,
    directed = FALSE,
    vertices = vertices
  )

  if (igraph::vcount(graph) != number_of_fish) {
    stop("The kNN graph does not contain one vertex per Stress fish.")
  }

  if (
    igraph::any_loop(graph) ||
    igraph::any_multiple(graph)
  ) {
    stop("The weighted union kNN graph contains loops or multiple edges.")
  }

  if (
    any(!is.finite(igraph::E(graph)$weight)) ||
    any(igraph::E(graph)$weight <= 0)
  ) {
    stop("The weighted union kNN graph contains invalid weights.")
  }

  list(
    Graph = graph,
    Edge_table = edge_table
  )
}

calculate_internal_metrics <- function(
  data_matrix,
  membership_vector,
  distance_object
) {
  membership_vector <- as.integer(
    factor(membership_vector)
  )

  number_of_fish <- nrow(data_matrix)
  cluster_levels <- sort(unique(membership_vector))
  number_of_clusters <- length(cluster_levels)

  if (
    length(membership_vector) != number_of_fish ||
    number_of_clusters < 1L
  ) {
    stop("Invalid membership supplied to calculate_internal_metrics().")
  }

  cluster_sizes <- as.integer(
    table(
      factor(
        membership_vector,
        levels = cluster_levels
      )
    )
  )

  names(cluster_sizes) <- as.character(cluster_levels)

  if (number_of_clusters == 1L) {
    return(
      list(
        Calinski_Harabasz = NA_real_,
        Silhouette = NA_real_,
        Davies_Bouldin = NA_real_,
        Minimum_cluster_size = min(cluster_sizes),
        Maximum_cluster_size = max(cluster_sizes),
        Cluster_size_CV = 0
      )
    )
  }

  overall_centroid <- colMeans(data_matrix)

  within_sum_squares <- 0
  between_sum_squares <- 0

  centroids <- matrix(
    NA_real_,
    nrow = number_of_clusters,
    ncol = ncol(data_matrix)
  )

  within_cluster_scatter <- numeric(number_of_clusters)

  for (cluster_i in seq_along(cluster_levels)) {
    cluster_label <- cluster_levels[cluster_i]
    cluster_index <- membership_vector == cluster_label
    cluster_matrix <- data_matrix[
      cluster_index,
      ,
      drop = FALSE
    ]

    cluster_centroid <- colMeans(cluster_matrix)
    centroids[cluster_i, ] <- cluster_centroid

    squared_distances_to_centroid <- rowSums(
      sweep(
        cluster_matrix,
        2L,
        cluster_centroid,
        FUN = "-"
      )^2
    )

    within_sum_squares <- within_sum_squares +
      sum(squared_distances_to_centroid)

    between_sum_squares <- between_sum_squares +
      nrow(cluster_matrix) *
      sum(
        (cluster_centroid - overall_centroid)^2
      )

    within_cluster_scatter[cluster_i] <- mean(
      sqrt(squared_distances_to_centroid)
    )
  }

  if (
    number_of_fish > number_of_clusters &&
    within_sum_squares > 0
  ) {
    calinski_harabasz <-
      (
        between_sum_squares /
          (number_of_clusters - 1)
      ) /
      (
        within_sum_squares /
          (number_of_fish - number_of_clusters)
      )
  } else {
    calinski_harabasz <- NA_real_
  }

  silhouette_object <- cluster::silhouette(
    membership_vector,
    distance_object
  )

  mean_silhouette <- mean(
    silhouette_object[, "sil_width"]
  )

  centroid_distances <- as.matrix(
    stats::dist(centroids)
  )

  db_ratio_matrix <- outer(
    within_cluster_scatter,
    within_cluster_scatter,
    FUN = "+"
  ) / centroid_distances

  diag(db_ratio_matrix) <- -Inf

  if (
    any(
      !is.finite(
        db_ratio_matrix[
          row(db_ratio_matrix) != col(db_ratio_matrix)
        ]
      )
    )
  ) {
    davies_bouldin <- Inf
  } else {
    davies_bouldin <- mean(
      apply(
        db_ratio_matrix,
        1L,
        max
      )
    )
  }

  cluster_size_cv <- if (
    length(cluster_sizes) > 1L &&
    mean(cluster_sizes) > 0
  ) {
    stats::sd(cluster_sizes) / mean(cluster_sizes)
  } else {
    0
  }

  list(
    Calinski_Harabasz = as.numeric(calinski_harabasz),
    Silhouette = as.numeric(mean_silhouette),
    Davies_Bouldin = as.numeric(davies_bouldin),
    Minimum_cluster_size = min(cluster_sizes),
    Maximum_cluster_size = max(cluster_sizes),
    Cluster_size_CV = as.numeric(cluster_size_cv)
  )
}

fit_louvain_for_k <- function(
  k_value,
  distance_matrix,
  distance_object,
  data_matrix,
  ids,
  seed
) {
  graph_object <- build_weighted_union_knn(
    distance_matrix = distance_matrix,
    ids = ids,
    k_value = k_value
  )

  graph <- graph_object$Graph

  set.seed(seed)

  partition <- igraph::cluster_louvain(
    graph,
    weights = igraph::E(graph)$weight
  )

  membership_vector <- igraph::membership(partition)

  if (is.null(names(membership_vector))) {
    names(membership_vector) <- igraph::V(graph)$name
  }

  membership_vector <- membership_vector[ids]

  if (
    length(membership_vector) != length(ids) ||
    anyNA(membership_vector)
  ) {
    stop("Louvain membership could not be aligned with Stress IDs.")
  }

  number_of_clusters <- length(
    unique(membership_vector)
  )

  internal_metrics <- calculate_internal_metrics(
    data_matrix = data_matrix,
    membership_vector = membership_vector,
    distance_object = distance_object
  )

  weighted_modularity <- igraph::modularity(
    graph,
    membership = membership_vector[
      igraph::V(graph)$name
    ],
    weights = igraph::E(graph)$weight
  )

  if (!is.finite(weighted_modularity)) {
    stop("Weighted modularity is not finite.")
  }

  result_row <- tibble::tibble(
    k = as.integer(k_value),
    Connected = igraph::is_connected(graph),
    Components = igraph::components(graph)$no,
    Vertices = igraph::vcount(graph),
    Edges = igraph::ecount(graph),
    Graph_density = igraph::edge_density(
      graph,
      loops = FALSE
    ),
    Mean_degree = mean(
      igraph::degree(graph)
    ),
    Mean_strength = mean(
      igraph::strength(
        graph,
        weights = igraph::E(graph)$weight
      )
    ),
    Number_of_clusters = as.integer(number_of_clusters),
    Minimum_cluster_size = as.integer(
      internal_metrics$Minimum_cluster_size
    ),
    Maximum_cluster_size = as.integer(
      internal_metrics$Maximum_cluster_size
    ),
    Cluster_size_CV = internal_metrics$Cluster_size_CV,
    Weighted_modularity = as.numeric(weighted_modularity),
    Calinski_Harabasz =
      internal_metrics$Calinski_Harabasz,
    Silhouette = internal_metrics$Silhouette,
    Davies_Bouldin = internal_metrics$Davies_Bouldin
  )

  list(
    Result = result_row,
    Membership = as.integer(membership_vector),
    Graph = graph,
    Partition = partition,
    Edge_table = graph_object$Edge_table
  )
}

construct_plateaus <- function(
  optimisation_table,
  threshold,
  minimum_size,
  minimum_length
) {
  table_local <- optimisation_table %>%
    dplyr::arrange(k) %>%
    dplyr::mutate(
      Eligible_for_plateau =
        Connected &
        Number_of_clusters >= 2L &
        Minimum_cluster_size >= minimum_size
    )

  plateau_id <- rep(NA_integer_, nrow(table_local))
  current_plateau <- 0L

  for (i in seq_len(nrow(table_local))) {
    if (!isTRUE(table_local$Eligible_for_plateau[i])) {
      next
    }

    starts_new_plateau <- TRUE

    if (
      i > 1L &&
      isTRUE(table_local$Eligible_for_plateau[i - 1L]) &&
      table_local$k[i] == table_local$k[i - 1L] + 1L &&
      table_local$Number_of_clusters[i] ==
        table_local$Number_of_clusters[i - 1L] &&
      is.finite(table_local$Consecutive_ARI_previous[i]) &&
      table_local$Consecutive_ARI_previous[i] >= threshold
    ) {
      starts_new_plateau <- FALSE
    }

    if (starts_new_plateau) {
      current_plateau <- current_plateau + 1L
    }

    plateau_id[i] <- current_plateau
  }

  table_local$Plateau_ID <- plateau_id

  plateau_table <- table_local %>%
    dplyr::filter(!is.na(Plateau_ID)) %>%
    dplyr::group_by(Plateau_ID) %>%
    dplyr::summarise(
      Start_k = min(k),
      End_k = max(k),
      Number_of_k = dplyr::n(),
      Number_of_clusters = dplyr::first(Number_of_clusters),
      Minimum_cluster_size_over_plateau =
        min(Minimum_cluster_size),
      Mean_adjacent_ARI = if (dplyr::n() > 1L) {
        mean(
          Consecutive_ARI_previous[-1L],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      Minimum_adjacent_ARI = if (dplyr::n() > 1L) {
        min(
          Consecutive_ARI_previous[-1L],
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      Mean_Silhouette = mean(
        Silhouette,
        na.rm = TRUE
      ),
      Mean_Calinski_Harabasz = mean(
        Calinski_Harabasz,
        na.rm = TRUE
      ),
      Mean_Davies_Bouldin = mean(
        Davies_Bouldin,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ARI_threshold = threshold,
      Eligible_plateau =
        Number_of_k >= minimum_length
    )

  list(
    Optimisation_table = table_local,
    Plateau_table = plateau_table
  )
}

# ------------------------------------------------------------
# 11. Evaluate k = 5 to 80
# ------------------------------------------------------------

k_max <- min(
  k_max_requested,
  expected_stress_n - 2L
)

k_values <- seq.int(
  from = k_min,
  to = k_max,
  by = 1L
)

if (
  length(k_values) < minimum_plateau_length ||
  any(k_values >= expected_stress_n)
) {
  stop("The requested k range is not valid for n = 89.")
}

results_list <- vector(
  "list",
  length(k_values)
)

memberships_by_k <- vector(
  "list",
  length(k_values)
)

names(memberships_by_k) <- as.character(k_values)

for (k_index in seq_along(k_values)) {
  k_value <- k_values[k_index]

  fitted_k <- fit_louvain_for_k(
    k_value = k_value,
    distance_matrix = distance_matrix,
    distance_object = distance_object,
    data_matrix = stress_matrix,
    ids = stress$Traceability_ID,
    seed = louvain_seed
  )

  results_list[[k_index]] <- fitted_k$Result

  membership_k <- fitted_k$Membership
  names(membership_k) <- stress$Traceability_ID

  memberships_by_k[[as.character(k_value)]] <-
    membership_k
}

optimisation_results <- dplyr::bind_rows(
  results_list
) %>%
  dplyr::arrange(k)

if (
  nrow(optimisation_results) != length(k_values) ||
  anyDuplicated(optimisation_results$k) > 0L ||
  !identical(optimisation_results$k, k_values)
) {
  stop("The k-optimisation table is incomplete or incorrectly ordered.")
}

if (
  any(optimisation_results$Vertices != expected_stress_n) ||
  any(optimisation_results$Components < 1L) ||
  any(optimisation_results$Edges <= 0L) ||
  any(!is.finite(optimisation_results$Weighted_modularity))
) {
  stop("At least one evaluated k produced an invalid graph or partition.")
}

# ------------------------------------------------------------
# 12. Calculate adjacent-partition stability across k
# ------------------------------------------------------------

consecutive_ari_previous <- rep(
  NA_real_,
  nrow(optimisation_results)
)

for (i in 2:nrow(optimisation_results)) {
  previous_k <- optimisation_results$k[i - 1L]
  current_k <- optimisation_results$k[i]

  previous_membership <-
    memberships_by_k[[as.character(previous_k)]]

  current_membership <-
    memberships_by_k[[as.character(current_k)]]

  consecutive_ari_previous[i] <-
    adjusted_rand_index(
      previous_membership,
      current_membership
    )
}

optimisation_results$Consecutive_ARI_previous <-
  consecutive_ari_previous

if (
  any(
    !is.finite(
      optimisation_results$Consecutive_ARI_previous[-1L]
    )
  )
) {
  stop("At least one consecutive-k ARI could not be calculated.")
}

# ------------------------------------------------------------
# 13. Identify a reproducible candidate plateau
# ------------------------------------------------------------

selection_thresholds <- c(
  primary_ari_threshold,
  secondary_ari_threshold
)

selected_plateau_result <- NULL
selected_ari_threshold <- NA_real_
selection_method <- NA_character_

for (threshold_i in selection_thresholds) {
  plateau_result_i <- construct_plateaus(
    optimisation_table = optimisation_results,
    threshold = threshold_i,
    minimum_size = minimum_community_size,
    minimum_length = minimum_plateau_length
  )

  eligible_plateaus_i <- plateau_result_i$Plateau_table %>%
    dplyr::filter(Eligible_plateau)

  if (nrow(eligible_plateaus_i) > 0L) {
    selected_plateau_result <- plateau_result_i
    selected_ari_threshold <- threshold_i
    selection_method <- paste0(
      "Longest consecutive same-cluster plateau with adjacent ARI >= ",
      sprintf("%.2f", threshold_i),
      "; selected k is the plateau-medoid partition"
    )
    break
  }
}

if (is.null(selected_plateau_result)) {
  selected_plateau_result <- construct_plateaus(
    optimisation_table = optimisation_results,
    threshold = -Inf,
    minimum_size = minimum_community_size,
    minimum_length = minimum_plateau_length
  )

  eligible_fallback_plateaus <-
    selected_plateau_result$Plateau_table %>%
    dplyr::filter(Eligible_plateau)

  if (nrow(eligible_fallback_plateaus) == 0L) {
    stop(
      "No connected consecutive k plateau of at least ",
      minimum_plateau_length,
      " values containing at least two non-empty communities was ",
      "identified. Review the k-optimisation outputs ",
      "before defining a candidate partition."
    )
  }

  selected_ari_threshold <- NA_real_
  selection_method <- paste(
    "Fallback: longest connected same-cluster-count plateau;",
    "selected k is the plateau-medoid partition"
  )

  warning(
    "No plateau met adjacent ARI >= ",
    secondary_ari_threshold,
    ". A transparent same-cluster-count fallback was used. ",
    "Script 025 must determine whether the candidate solution is stable."
  )
}

optimisation_results <-
  selected_plateau_result$Optimisation_table

plateau_table <-
  selected_plateau_result$Plateau_table

eligible_plateaus <- plateau_table %>%
  dplyr::filter(Eligible_plateau) %>%
  dplyr::arrange(
    dplyr::desc(Number_of_k),
    dplyr::desc(Mean_adjacent_ARI),
    dplyr::desc(Mean_Silhouette),
    Start_k
  )

selected_plateau <- eligible_plateaus[
  1L,
  ,
  drop = FALSE
]

selected_plateau_id <- selected_plateau$Plateau_ID[[1L]]

selected_plateau_k <- optimisation_results$k[
  !is.na(optimisation_results$Plateau_ID) &
    optimisation_results$Plateau_ID == selected_plateau_id
]

if (
  length(selected_plateau_k) <
    minimum_plateau_length
) {
  stop("The selected plateau is shorter than the required minimum.")
}

# ------------------------------------------------------------
# 14. Select the medoid partition within the plateau
# ------------------------------------------------------------

plateau_memberships <- lapply(
  selected_plateau_k,
  function(k_value) {
    memberships_by_k[[as.character(k_value)]]
  }
)

names(plateau_memberships) <- as.character(
  selected_plateau_k
)

number_plateau_k <- length(selected_plateau_k)

plateau_ari_matrix <- matrix(
  1,
  nrow = number_plateau_k,
  ncol = number_plateau_k,
  dimnames = list(
    as.character(selected_plateau_k),
    as.character(selected_plateau_k)
  )
)

if (number_plateau_k > 1L) {
  for (i in seq_len(number_plateau_k - 1L)) {
    for (j in (i + 1L):number_plateau_k) {
      ari_ij <- adjusted_rand_index(
        plateau_memberships[[i]],
        plateau_memberships[[j]]
      )

      plateau_ari_matrix[i, j] <- ari_ij
      plateau_ari_matrix[j, i] <- ari_ij
    }
  }
}

mean_ari_to_plateau <- rowMeans(
  plateau_ari_matrix
)

plateau_midpoint <- stats::median(
  selected_plateau_k
)

selection_candidates <- tibble::tibble(
  k = selected_plateau_k,
  Mean_ARI_to_plateau = as.numeric(
    mean_ari_to_plateau[
      as.character(selected_plateau_k)
    ]
  ),
  Distance_to_plateau_midpoint = abs(
    selected_plateau_k - plateau_midpoint
  )
) %>%
  dplyr::left_join(
    optimisation_results %>%
      dplyr::select(
        k,
        Silhouette,
        Calinski_Harabasz,
        Davies_Bouldin
      ),
    by = "k"
  ) %>%
  dplyr::arrange(
    dplyr::desc(Mean_ARI_to_plateau),
    Distance_to_plateau_midpoint,
    dplyr::desc(Silhouette),
    k
  )

selected_k <- as.integer(
  selection_candidates$k[[1L]]
)

if (
  length(selected_k) != 1L ||
  is.na(selected_k) ||
  !selected_k %in% selected_plateau_k
) {
  stop("A valid candidate k could not be selected.")
}

# ------------------------------------------------------------
# 15. Rebuild and audit the selected graph and partition
# ------------------------------------------------------------

selected_fit <- fit_louvain_for_k(
  k_value = selected_k,
  distance_matrix = distance_matrix,
  distance_object = distance_object,
  data_matrix = stress_matrix,
  ids = stress$Traceability_ID,
  seed = louvain_seed
)

selected_graph <- selected_fit$Graph
selected_partition <- selected_fit$Partition
selected_membership_raw <- selected_fit$Membership

names(selected_membership_raw) <- stress$Traceability_ID

stored_selected_membership <-
  memberships_by_k[[as.character(selected_k)]]

selected_rebuild_ari <- adjusted_rand_index(
  selected_membership_raw,
  stored_selected_membership
)

if (
  !is.finite(selected_rebuild_ari) ||
  abs(selected_rebuild_ari - 1) > ari_tolerance
) {
  stop(
    "Rebuilding the selected k graph did not reproduce the stored ",
    "candidate partition."
  )
}

if (!igraph::is_connected(selected_graph)) {
  stop("The selected weighted union kNN graph is not connected.")
}

if (
  igraph::vcount(selected_graph) != expected_stress_n ||
  !identical(
    as.character(igraph::V(selected_graph)$name),
    stress$Traceability_ID
  )
) {
  stop("The selected graph vertices do not match the Stress IDs.")
}

selected_number_of_communities <- length(
  unique(selected_membership_raw)
)

if (selected_number_of_communities < 2L) {
  stop("The selected candidate partition contains fewer than two communities.")
}

selected_community_sizes_raw <- table(
  selected_membership_raw
)

if (min(selected_community_sizes_raw) < 1L) {
  stop("The selected candidate partition contains an empty community.")
}

# ------------------------------------------------------------
# 16. Renumber communities deterministically by mean Stress PC1
# ------------------------------------------------------------

community_ordering_table <- tibble::tibble(
  Traceability_ID = stress$Traceability_ID,
  Raw_community = as.integer(
    selected_membership_raw
  ),
  PC1 = as.numeric(pca_scores$PC1)
) %>%
  dplyr::group_by(Raw_community) %>%
  dplyr::summarise(
    n = dplyr::n(),
    Mean_PC1 = mean(PC1),
    Median_PC1 = stats::median(PC1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(
    Mean_PC1,
    Median_PC1,
    Raw_community
  ) %>%
  dplyr::mutate(
    Stress_Cluster_integer = dplyr::row_number(),
    Stress_Community = paste0(
      "Community_",
      Stress_Cluster_integer
    )
  )

raw_to_ordered_cluster <- stats::setNames(
  community_ordering_table$Stress_Cluster_integer,
  as.character(
    community_ordering_table$Raw_community
  )
)

ordered_membership <- as.integer(
  raw_to_ordered_cluster[
    as.character(selected_membership_raw)
  ]
)

if (
  length(ordered_membership) != expected_stress_n ||
  anyNA(ordered_membership)
) {
  stop("The selected communities could not be renumbered deterministically.")
}

stress_louvain <- stress %>%
  dplyr::mutate(
    PC1 = as.numeric(pca_scores$PC1),
    PC2 = as.numeric(pca_scores$PC2),
    Stress_Cluster = factor(
      ordered_membership,
      levels = seq_len(selected_number_of_communities)
    ),
    Stress_Community = factor(
      paste0("Community_", ordered_membership),
      levels = paste0(
        "Community_",
        seq_len(selected_number_of_communities)
      )
    )
  )

if (
  anyNA(stress_louvain$Stress_Cluster) ||
  anyNA(stress_louvain$Stress_Community)
) {
  stop("At least one Stress fish lacks a selected community assignment.")
}

igraph::V(selected_graph)$Stress_Cluster <- as.character(
  stress_louvain$Stress_Cluster[
    match(
      igraph::V(selected_graph)$name,
      stress_louvain$Traceability_ID
    )
  ]
)

igraph::V(selected_graph)$Stress_Community <- as.character(
  stress_louvain$Stress_Community[
    match(
      igraph::V(selected_graph)$name,
      stress_louvain$Traceability_ID
    )
  ]
)

# ------------------------------------------------------------
# 17. Create definitive candidate summaries
# ------------------------------------------------------------

selected_result <- optimisation_results %>%
  dplyr::filter(k == selected_k)

if (nrow(selected_result) != 1L) {
  stop("The selected k does not have exactly one optimisation row.")
}

# Build the community-size table directly from the integer membership.
# This avoids counting combinations of two redundant factor variables
# and guarantees exactly one row per observed community.
expected_cluster_sequence <- seq_len(
  selected_number_of_communities
)

community_count_vector <- tabulate(
  ordered_membership,
  nbins = selected_number_of_communities
)

community_sizes <- tibble::tibble(
  Stress_Cluster_integer = expected_cluster_sequence,
  Stress_Cluster = factor(
    expected_cluster_sequence,
    levels = expected_cluster_sequence
  ),
  Stress_Community = factor(
    paste0("Community_", expected_cluster_sequence),
    levels = paste0(
      "Community_",
      expected_cluster_sequence
    )
  ),
  n = as.integer(community_count_vector),
  Percentage = 100 *
    as.integer(community_count_vector) /
    expected_stress_n
) %>%
  dplyr::left_join(
    community_ordering_table %>%
      dplyr::select(
        Stress_Cluster_integer,
        Mean_PC1,
        Median_PC1
      ),
    by = "Stress_Cluster_integer"
  ) %>%
  dplyr::arrange(
    Stress_Cluster_integer
  )

if (
  length(community_count_vector) !=
    selected_number_of_communities ||
  any(community_count_vector <= 0L) ||
  sum(community_count_vector) != expected_stress_n ||
  nrow(community_sizes) != selected_number_of_communities ||
  !identical(
    community_sizes$Stress_Cluster_integer,
    expected_cluster_sequence
  ) ||
  anyNA(community_sizes$Stress_Cluster) ||
  anyNA(community_sizes$Stress_Community) ||
  anyNA(community_sizes$Mean_PC1) ||
  anyNA(community_sizes$Median_PC1)
) {
  stop("The selected community-size table is inconsistent.")
}

selected_membership_table <- stress_louvain %>%
  dplyr::transmute(
    Author,
    ID,
    Traceability_ID,
    Treatment,
    k = selected_k,
    Stress_Cluster,
    Stress_Community,
    PC1,
    PC2
  )

selected_configuration <- list(
  Objective = "Objective 3",
  Scientific_question = setup_object$Scientific_question,
  Operational_question = setup_object$Operational_question,
  Analytical_population = "Stress only",
  Stress_n = expected_stress_n,
  Authors = population_authors,
  Traceability = expected_traceability,
  Clustering_scope = "De novo within the Stress population",
  Reference_space = setup_object[["Reference_space"]],
  Normalisation = setup_object[["Normalisation"]],
  Normalisation_formula = setup_object[["Normalisation_formula"]],
  Behavioural_variables = behavioural_variables,
  Robust_variables = robust_variables,
  Distance = distance_description,
  Similarity = similarity_description,
  Graph = graph_description,
  k_range = range(k_values),
  Minimum_community_size = minimum_community_size,
  Louvain_seed = louvain_seed,
  Selection_method = selection_method,
  Selected_ARI_threshold = selected_ari_threshold,
  Selected_plateau_ID = selected_plateau_id,
  Selected_plateau_start_k =
    selected_plateau$Start_k[[1L]],
  Selected_plateau_end_k =
    selected_plateau$End_k[[1L]],
  Selected_plateau_length =
    selected_plateau$Number_of_k[[1L]],
  Selected_plateau_number_of_communities =
    selected_plateau$Number_of_clusters[[1L]],
  Selected_k = selected_k,
  Selected_number_of_communities =
    selected_number_of_communities,
  Selected_weighted_modularity =
    selected_result$Weighted_modularity[[1L]],
  Selected_Calinski_Harabasz =
    selected_result$Calinski_Harabasz[[1L]],
  Selected_Silhouette =
    selected_result$Silhouette[[1L]],
  Selected_Davies_Bouldin =
    selected_result$Davies_Bouldin[[1L]],
  Selected_rebuild_ARI = selected_rebuild_ari,
  Community_numbering = paste(
    "Ascending mean Stress PC1; numbering is deterministic and",
    "does not constitute biological labelling"
  ),
  Status = paste(
    "Candidate de novo partition; reproducibility must be evaluated",
    "in 025_Stress_Louvain_Stability.R before interpretation"
  )
)

graph_summary <- tibble::tibble(
  Number_of_fish = expected_stress_n,
  Number_of_behavioural_variables =
    expected_number_of_variables,
  Normalisation = normalisation_description,
  Distance = distance_description,
  Similarity = similarity_description,
  Graph_type = graph_description,
  k = selected_k,
  Vertices = igraph::vcount(selected_graph),
  Edges = igraph::ecount(selected_graph),
  Connected = igraph::is_connected(selected_graph),
  Components = igraph::components(selected_graph)$no,
  Graph_density = igraph::edge_density(
    selected_graph,
    loops = FALSE
  ),
  Mean_degree = mean(
    igraph::degree(selected_graph)
  ),
  Mean_strength = mean(
    igraph::strength(
      selected_graph,
      weights = igraph::E(selected_graph)$weight
    )
  ),
  Number_of_communities =
    selected_number_of_communities,
  Minimum_community_size = min(community_sizes$n),
  Maximum_community_size = max(community_sizes$n),
  Weighted_modularity =
    selected_result$Weighted_modularity[[1L]],
  Louvain_seed = louvain_seed
)

results_object <- list(
  Objective = "Objective 3",
  Scientific_question = setup_object$Scientific_question,
  Operational_question = setup_object$Operational_question,
  Analytical_population = "Stress only",
  Stress_n = expected_stress_n,
  Authors = population_authors,
  Traceability = expected_traceability,
  Reference_space = setup_object[["Reference_space"]],
  Normalisation = setup_object[["Normalisation"]],
  Normalisation_formula = setup_object[["Normalisation_formula"]],
  Robust_variables = robust_variables,
  Author_specific_normalisation_recalculation_audit =
    normalisation_recalculation_audit,
  Distance_matrix = distance_matrix,
  k_values = k_values,
  k_optimisation_results = optimisation_results,
  Memberships_by_k = memberships_by_k,
  Plateau_table = plateau_table,
  Selected_plateau = selected_plateau,
  Plateau_pairwise_ARI = plateau_ari_matrix,
  Plateau_selection_candidates = selection_candidates,
  Selected_configuration = selected_configuration,
  Selected_graph_summary = graph_summary,
  Selected_membership = selected_membership_table,
  Community_sizes = community_sizes,
  Community_ordering = community_ordering_table,
  Selected_graph = selected_graph,
  Selected_partition = selected_partition,
  Source_stress_file = stress_input,
  Source_setup_file = setup_input,
  Source_pca_results_file = pca_results_input,
  Interpretive_constraint = paste(
    "The selected communities are a candidate de novo internal",
    "partition of Stress. Script 025 must establish reproducibility",
    "before resilient-like or susceptible-like interpretation."
  )
)

# ------------------------------------------------------------
# 18. Export tables and R objects
# ------------------------------------------------------------

openxlsx::write.xlsx(
  x = list(
    k_optimisation = optimisation_results,
    stable_plateaus = plateau_table,
    plateau_candidates = selection_candidates,
    selected_configuration = data.frame(
      Field = names(selected_configuration),
      Value = vapply(
        selected_configuration,
        function(x) {
          paste(as.character(x), collapse = "; ")
        },
        character(1)
      ),
      stringsAsFactors = FALSE
    ),
    graph_summary = graph_summary,
    author_recalculation_audit =
      normalisation_recalculation_audit,
    selected_membership = selected_membership_table,
    community_sizes = community_sizes,
    community_ordering = community_ordering_table
  ),
  file = workbook_output,
  overwrite = TRUE
)

utils::write.csv(
  optimisation_results,
  optimisation_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  plateau_table,
  plateaus_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  selected_membership_table,
  membership_csv_output,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  community_sizes,
  community_sizes_csv_output,
  row.names = FALSE,
  na = ""
)

saveRDS(
  stress_louvain,
  stress_output
)

saveRDS(
  selected_graph,
  graph_output
)

saveRDS(
  selected_partition,
  partition_output
)

saveRDS(
  optimisation_results,
  optimisation_results_output
)

saveRDS(
  memberships_by_k,
  memberships_output
)

saveRDS(
  selected_configuration,
  selected_configuration_output
)

saveRDS(
  selected_membership_table,
  selected_membership_output
)

saveRDS(
  results_object,
  results_object_output
)

capture.output(
  utils::sessionInfo(),
  file = session_info_output
)

# ------------------------------------------------------------
# 19. Confirm output creation
# ------------------------------------------------------------

expected_output_files <- c(
  stress_output,
  graph_output,
  partition_output,
  optimisation_results_output,
  memberships_output,
  selected_configuration_output,
  selected_membership_output,
  results_object_output,
  workbook_output,
  optimisation_csv_output,
  plateaus_csv_output,
  membership_csv_output,
  community_sizes_csv_output,
  session_info_output
)

missing_output_files <- expected_output_files[
  !file.exists(expected_output_files)
]

if (length(missing_output_files) > 0L) {
  stop(
    "The following expected outputs were not created:\n",
    paste(missing_output_files, collapse = "\n")
  )
}

output_file_information <- file.info(
  expected_output_files
)

if (
  any(is.na(output_file_information$size)) ||
  any(output_file_information$size <= 0)
) {
  stop("At least one expected output file is empty or inaccessible.")
}

# ------------------------------------------------------------
# 20. Final console audit
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("OBJECTIVE 3 - DE NOVO STRESS LOUVAIN ARCHITECTURE\n")
cat("============================================================\n")

cat(
  "\nScientific question:\n",
  setup_object$Scientific_question,
  "\n",
  sep = ""
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
  paste(population_authors, collapse = ", "),
  "\n",
  sep = ""
)

cat(
  "Analytical space: four Author-specific Control-referenced robust ",
  "variables inherited unchanged from corrected Objectives 1 and 2.\n",
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
  "Community detection: de novo weighted Louvain in the full ",
  "four-dimensional space, not in PC1-PC2.\n",
  sep = ""
)

cat(
  "\nEvaluated k range: ",
  min(k_values),
  " to ",
  max(k_values),
  " (",
  length(k_values),
  " values).\n",
  sep = ""
)

cat(
  "Graph: ",
  graph_description,
  "; distance = ",
  distance_description,
  "; similarity = ",
  similarity_description,
  ".\n",
  sep = ""
)

cat(
  "Louvain seed: ",
  louvain_seed,
  ".\n",
  sep = ""
)

cat(
  "\nSelection method:\n",
  selection_method,
  "\n",
  sep = ""
)

cat("\nSelected plateau:\n")
print(
  selected_plateau,
  row.names = FALSE
)

cat("\nCandidate-k medoid table:\n")
print(
  selection_candidates,
  n = Inf,
  width = Inf
)

cat(
  "\nSelected k: ",
  selected_k,
  "\n",
  sep = ""
)

cat(
  "Selected graph vertices: ",
  igraph::vcount(selected_graph),
  "\n",
  sep = ""
)

cat(
  "Selected graph edges: ",
  igraph::ecount(selected_graph),
  "\n",
  sep = ""
)

cat(
  "Selected graph connected: ",
  igraph::is_connected(selected_graph),
  "\n",
  sep = ""
)

cat(
  "Candidate communities: ",
  selected_number_of_communities,
  "\n",
  sep = ""
)

cat(
  "Weighted modularity: ",
  sprintf(
    "%.4f",
    selected_result$Weighted_modularity[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Calinski-Harabasz: ",
  sprintf(
    "%.4f",
    selected_result$Calinski_Harabasz[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Mean silhouette: ",
  sprintf(
    "%.4f",
    selected_result$Silhouette[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Davies-Bouldin: ",
  sprintf(
    "%.4f",
    selected_result$Davies_Bouldin[[1L]]
  ),
  "\n",
  sep = ""
)

cat(
  "Selected-partition rebuild ARI: ",
  sprintf(
    "%.6f",
    selected_rebuild_ari
  ),
  "\n",
  sep = ""
)

cat("\nCandidate community sizes and PC1 ordering:\n")
print(
  community_sizes,
  n = Inf,
  width = Inf
)

cat(
  "\nInterpretive constraint: this script identifies a candidate ",
  "de novo internal partition of Stress. It does not yet establish ",
  "that the communities are reproducible and does not assign ",
  "resilient-like or susceptible-like labels. Those steps require ",
  "Script 025 and subsequent community characterisation.\n",
  sep = ""
)

cat("\nOutputs saved as:\n")
cat(
  paste0(
    "- ",
    normalizePath(
      expected_output_files,
      winslash = "/",
      mustWork = FALSE
    ),
    collapse = "\n"
  ),
  "\n",
  sep = ""
)

cat("============================================================\n")
